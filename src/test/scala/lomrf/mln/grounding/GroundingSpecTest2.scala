/*
 * o                        o     o   o         o
 * |             o          |     |\ /|         | /
 * |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 * |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 * O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *             |
 *          o--o
 * o--o              o               o--o       o    o
 * |   |             |               |    o     |    |
 * O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 * |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 * o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 * Logical Markov Random Fields.
 *
 * Copyright (C) 2012  Anastasios Skarlatidis.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.mln.grounding

import auxlib.log.Logging
import gnu.trove.set.hash.TIntHashSet
import lomrf.logic.{AtomSignature, KBParser, _}
import lomrf.mln.model.{DomainSchema, DomainSpace, MLN}
import lomrf.tests.ECExampleDomain1._
import lomrf.util.{FunctionMapper, AtomIdentityFunction, AtomEvidenceDB, ConstantsSet}
import org.scalatest.{FunSpec, Matchers}
import scala.collection.breakOut
import scala.{collection => sc}


class GroundingSpecTest2 extends FunSpec with Matchers with Logging {


  private val parser = new KBParser(predicateSchema, functionsSchema)

  private val schema = DomainSchema(predicateSchema, functionsSchema, queryAtoms, cwa, owa)

  private val atomIdentifier = DomainSpace(schema,constants)

  // Manually create sample evidence
  private val atomStateDB: Map[AtomSignature, AtomEvidenceDB] = {

    var result = Map[AtomSignature, AtomEvidenceDB]()

    // All predicates with open-world assumption have unknown state values --- i.e., HoldsAt/2, InitiatedAt/2 and TerminatedAt/2
    for (signature <- owa)
      result += signature -> AtomEvidenceDB.OWA(atomIdentifier.identities(signature))

    // Add all positive instantiations of predicate Next/2 (for time points 1 to 100)
    val nextSignature = AtomSignature("Next", 2)
    val nextIDF = atomIdentifier.identities(nextSignature)
    val nextPositives = new TIntHashSet()
    (1 until LAST_TIME_POINT).map(t => nextIDF.encode(Seq((t + 1).toString, t.toString))).foreach(nextPositives.add)
    result += (nextSignature -> AtomEvidenceDB.CWA(nextPositives, nextIDF))

    // Assume true All instantiations of predicate Happens/2 having its first argument equals with 'walking' and
    // the rest instantiations are false.
    val happensSignature = AtomSignature("Happens", 2)
    val happensIDF = atomIdentifier.identities(happensSignature)
    val happensPositives = new TIntHashSet()
    (1 to LAST_TIME_POINT).map(t => happensIDF.encode(Seq("Walking", t.toString))).foreach(happensPositives.add)
    result += (happensSignature -> AtomEvidenceDB.CWA(happensPositives, happensIDF))

    result
  }


  //val formulaStr = "HoldsAt(f,t) ^ !TerminatedAt(f,t) ^ Next(t,tNext) => HoldsAt(f,tNext)."
  //val formulaStr = "HoldsAt(f,t) ^ !TerminatedAt(f,t) => HoldsAt(f, next(t))."
  val formulaStr = "HoldsAt(f,t) ^ !TerminatedAt(f,t) => HoldsAt(f, t + 1 )."

  val formula = parser.parseFormula(formulaStr)


  describe("Formula '" + formulaStr + "'") {



    val clauses = formula.toCNF(constants)

    it("produces a single clause") {
      clauses.size should be(1)
    }

    val clause = clauses.head

    /*it("contains three variables") {
      clause.variables.size should be(3)
    }*/

    val mln =  new MLN(
          schema,
          formulas = Set(formula),
          dynamicAtoms,
          dynamicFunctions,
          constants,
          functionMappers,
          probabilisticAtoms = Set.empty[AtomSignature],
          tristateAtoms = Set.empty[AtomSignature],
          atomStateDB
        )


    val orderedLiterals =
      clause
        .literals
        .toArray
        .sortBy(l => l)(ClauseLiteralsOrdering(mln))

    info("original sequence of literals : " + clause.literals.map(_.toString).mkString(" v ") + "\n" +
      "ordered sequence of literals  : " + orderedLiterals.map(_.toString).mkString(" v "))


    // --- Lets build the flat-indexes

    // Vector of ( Term -> Domain Name), Map of (Domain Name -> its corresponding constants set),
    //   Map of (function signature -> (function return domain name, Seq of function's term domains))
    type FlatDomainDescription = (
      Vector[(Term, String)],
        sc.Map[String, ConstantsSet],
        sc.Map[AtomSignature, (String, collection.Seq[String], FunctionMapper)]
      )


    def mkFlatTermDomains(literals: Iterable[Literal],
                          predicateSchema: sc.Map[AtomSignature, sc.Seq[String]],
                          functionSchema: sc.Map[AtomSignature, (String, collection.Seq[String])],
                          staticDomain2ConstantSets: sc.Map[String, ConstantsSet]): FlatDomainDescription = {

      val queue = collection.mutable.Queue[(Term, String)]()

      var memory = Set[Term]()

      // Vector of ( Term -> Domain Name)
      var result = Vector[(Term, String)]()

      //Map of (Domain Name -> its corresponding constants set)
      var dynConstants = Map[String, ConstantsSet]()

      var dynFunctionSchema = Map[AtomSignature, (String, Vector[String], FunctionMapper)]()

      for ((literal, literalIdx) <- literals.zipWithIndex) {

        // todo: add term index
        queue ++= literal.sentence.terms.zip(predicateSchema(literal.sentence.signature))

        while (queue.nonEmpty) {
          val (candidate, domain) = queue.dequeue()

          if (!candidate.isFunction && !memory.contains(candidate)) {
            memory += candidate
            result :+= (candidate, domain)

          }
          else candidate match {
            case f: TermFunction if functionSchema.contains(f.signature) =>
              queue ++= f.terms.zip(functionSchema(f.signature)._2)

            case f: TermFunction if dynamicFunctions.contains(f.signature) =>
              println("dynamicFunction with args: "+f.terms.mkString(", "))
              //val unarySets = matchedTerms(f.terms, _.isConstant).map(c => ConstantsSet(c.symbol))
              //unarySets.foreach(println)

              val term2Domains: Vector[(Term, String)] = (
                for((term, termIdx) <- f.terms.zipWithIndex) yield term match {
                  case v: Variable => (v, v.domain)

                  case c: Constant =>
                    //todo add function position in literals terms
                    val dynDomainName = "_D"+literalIdx+":"+termIdx

                    // Update dynConstants
                    dynConstants += (dynDomainName -> ConstantsSet(c.symbol))

                    (c, dynDomainName)

                  case f: TermFunction =>
                    fatal("Recursive function definitions are not supported.")
                })(breakOut)

              queue ++= term2Domains

              // todo: add function position in literals terms
              /*dynFunctionSchema +=
                AtomSignature(f.symbol+"@"+literalIdx, f.arity) ->
                  (f.domain, term2Domains.map(_._2), FunctionMapper(dynamicFunctions(f.signature)))*/

              // this is not correct, but I am using it temporally for testing:
              dynFunctionSchema +=
                f.signature -> (f.domain, term2Domains.map(_._2), FunctionMapper(dynamicFunctions(f.signature)))

            case _ => //do nothing
          }
        }
      }

      val foo = functionSchema.map{
        case (signature, (resultingDomain, termDomains)) =>
          signature ->  (resultingDomain, termDomains, functionMappers(signature))
      }

      (result, staticDomain2ConstantSets ++ dynConstants, foo ++ dynFunctionSchema)
    }

    // theta:
    //  - Assume that we want to perform a theta substitution to a clause, in order to produce a possible grounding.
    //  - The theta array will hold the current domain index for each variable in the clause. The domain index represents
    //  the current constant value that will substitute to the corresponding variable. Additionally, for efficiency and
    //  technical reasons, the theta will contain the corresponding domain index of a each constant in the clause. This
    //  feature is required in the grounding process (not discussed here, see function: ???).
    //
    // For example:
    //  - Lets say that we want to represent the theta of the clause: !Foo(x, y) v !Bar(x, A) v Q(y, f(z))
    //  - the theta will be: theta = <idx(x), idx(y), A, idx(z)>
    //
    // theta.size:
    //  - Equal with the number of distinct variables and constants
    //
    val theta = new Array[Int](clause.variables.size + clause.constants.size)
    val thetaDomains = new Array[String](clause.variables.size + clause.constants.size)

    val (flatTerms, domain2ConstantSets, functionSchema) =
      mkFlatTermDomains(orderedLiterals, mln.schema.predicateSchema, mln.schema.functionSchema, mln.constants)
    var term2Pos = Map[Term, Int]()


    for( ((term, domain: String), index) <- flatTerms.zipWithIndex) {

      term2Pos += (term -> index)

      thetaDomains(index) = domain

      term match {
        case c: Constant =>
          theta(index) =
            if (domain.startsWith("_D")) 0
            else domain2ConstantSets(domain)(c.symbol)

        case _ =>
          theta(index) = domain2ConstantSets(domain).size - 1
      }
    }

    //println("TERMS: " + flatTerms.mkString(", "))

    println("DOMAINS: "+domain2ConstantSets.mkString(", "))

    println("FUNCTIONS: "+functionSchema.mkString(", "))

    println("TERMS: " + flatTerms.map(e => e +"["+term2Pos(e._1)+"]").mkString(", "))

    println("THETA: " + theta.mkString(", "))


    def mkL2T(): Array[Array[Int]] = {
      val literal2Theta = new Array[Array[Int]](orderedLiterals.size + clause.functions.size)


      var func2Pos = Map[TermFunction, Int]() // gives the position of a function in the literal2Theta
      val collectedFunctions = collection.mutable.Stack[TermFunction]() // to collect the functions to parse

      // offset: to begin from the auxiliary position of functions
      val functionIdxOffset = orderedLiterals.size - 1
      var functionIdx = functionIdxOffset

      def mkEntries(terms: Vector[Term], size: Int): Array[Int] ={
        val entries = new Array[Int](size)

        //val atomSchema = mln.predicateSchema(atom.signature)

        for((term, tidx) <- terms.zipWithIndex) term match {
          case f: TermFunction =>

            func2Pos.get(f) match{
              case Some(position) => entries(tidx) = -position
              case _ =>
                collectedFunctions.push(f)
                functionIdx += 1
                entries(tidx) = -functionIdx
                func2Pos += (f -> functionIdx)

            }
          case _ =>
            entries(tidx) = term2Pos(term)
        }

        entries
      }


      // Step 1: parse all literals
      for( (literal, index) <- orderedLiterals.zipWithIndex; atom = literal.sentence )
        literal2Theta(index) = mkEntries(atom.terms, atom.arity)


      println("collectedFunctions={"+collectedFunctions.mkString(", ")+"}")

      // Step 2: parse all functions
      while(collectedFunctions.nonEmpty){
        val currentFunction = collectedFunctions.pop()
        literal2Theta(functionIdx) = mkEntries(currentFunction.terms, currentFunction.arity)
        functionIdx += 1
      }


      literal2Theta
    }

    val l2t = mkL2T()

    for((l2tEntry, index) <- l2t.zipWithIndex)
      println("theta_I["+index+"] = <"+l2tEntry.mkString(",")+">\n")


    type ThetaEncoder = (Array[Int] => Int)


    // Try to compute the theta-substitution
    // Manually change the time variable
    theta(1) = theta(1) - 1

    // step 1: get AtomIdentityFunctions (code is from ClauseGrounderImplNew.apply())
    val orderedIdentityFunctions = orderedLiterals.map(literal => mln.space.identities(literal.sentence.signature))


    // step 2: Creation of substitution functions for each literal
    def createThetaEncoder(literal: Literal, idf: AtomIdentityFunction, entryIds: Array[Int]): ThetaEncoder = {


      def functionEncoder(function: TermFunction, entryIds: Array[Int]): Int = {

        val (resultDomain, termDomains, mapper) = functionSchema(function.signature)

        val constants =
          for( ((term, termDomain), index) <- function.terms.zip(termDomains).zipWithIndex )
            yield term match{
              case f: TermFunction =>
                val i = math.abs(entryIds(index))
                domain2ConstantSets(termDomain)(functionEncoder(f, l2t(i)))
              case _ =>
                println("entryIds("+index+") = "+entryIds(index))
                println("theta(entryIds("+index+")) = "+theta(entryIds(index)))
                domain2ConstantSets(termDomain).apply(theta(entryIds(index)))
            }

        println("--- FUNCTION: "+function.toText)
        println("----> resultDomain: "+resultDomain)
        println("----> termDomains : "+termDomains.mkString(", "))
        println("----> mapper      : "+mapper)
        println("----> CONSTANTS: "+ constants.mkString(", "))
        domain2ConstantSets(resultDomain)(mapper(constants))

      }


      if(literal.sentence.functions.isEmpty)
        (theta: Array[Int]) => idf.encode(entryIds, theta)
      else {
        (theta: Array[Int]) => {
          val constantIds = new Array[Int](literal.sentence.terms.size)

          for((term, idx) <- literal.sentence.terms.zipWithIndex) {
            constantIds(idx) = term match {
              case f: TermFunction =>
                val i = math.abs(entryIds(idx))
                println("l2t("+i+") -> "+l2t(i).mkString(", "))
                functionEncoder(f, l2t(i))
              case _ => theta(entryIds(idx))
            }
          }

          idf.encode2(constantIds)
        }
      }


    }




    // step 3: substitute (through encoding) and check the result (through decoding)

    val thetaDomainsStr =
      theta
        .zip(thetaDomains)
        .map{case (thetaIdx, domainName) => domain2ConstantSets(domainName)(thetaIdx)}
        .mkString(", ")

    val thetaDomainSet =
      theta
        .zip(thetaDomains)
        .map{case (thetaIdx, domainName) => domain2ConstantSets(domainName)(thetaIdx)}
        .toSet

    for{
      i <- 0 until orderedLiterals.length
      literal = orderedLiterals(i)
      idf = orderedIdentityFunctions(i)
      entryIds = l2t(i)
      encoder = createThetaEncoder(literal, idf, entryIds)} {


      if (literal.sentence.functions.isEmpty){
        println("Encoding literal: "+literal.toText+" --- theta = {"+thetaDomainsStr+"}")
        val encoded = idf.encode(entryIds, theta)
        println("\t encoded: "+encoded)

        val encoded2 = encoder(theta)
        assert(encoded2 == encoded)

        idf.decode(encoded2) match{
          case Some(decoded) =>
            println("\t decoded: "+decoded+"\n\n")
            assert(decoded.forall(thetaDomainSet.contains))
          case None =>
            error("decoding has failed!")
        }

      }
      else {
        println("Houston we got a problem, we have at least one function!")
        println("entryIds := "+entryIds.mkString(", "))
        println("Literal  := "+literal.toText)

        val encoded2 = encoder(theta)

        idf.decode(encoded2) match {
          case Some(decoded) =>
            println("\t decoded: "+decoded+"\n\n")

          case None =>
            error("decoding has failed!")
        }

      }



    }





  }


}
