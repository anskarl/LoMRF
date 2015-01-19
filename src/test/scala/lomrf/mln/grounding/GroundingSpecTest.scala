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
import lomrf.logic._
import lomrf.logic.{Literal, AtomSignature, KBParser}
import lomrf.mln.model.{AtomIdentifier, MLN}
import lomrf.util.{ConstantsSet, Utilities, AtomEvidenceDB}
import org.scalatest.{FunSpec, Matchers}
import lomrf.tests.ECExampleDomain1._

import scala.collection.mutable

/**
 * @author Anastasios Skarlatidis
 */
class GroundingSpecTest extends FunSpec with Matchers with Logging {


  private val parser = new KBParser(predicateSchema, functionsSchema)

  private val atomIdentifier = AtomIdentifier(predicateSchema, constants, queryAtoms, hiddenAtoms)

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
  val formulaStr = "HoldsAt(f,t) ^ !TerminatedAt(f,t) ^ !TerminatedAt(f,t1) ^ Next(t,tNext) => HoldsAt(f,tNext)."

  describe("Formula '" + formulaStr + "'") {
    val formula = parser.parseFormula(formulaStr)
    val clauses = formula.toCNF(constants)

    it("produces a single clause") {
      clauses.size should be(1)
    }

    val clause = clauses.head

    it("contains three variables") {
      clause.variables.size should be(4)
    }

    val mln = new MLN(
      formulas = Set(formula),
      predicateSchema,
      functionsSchema,
      dynamicAtoms,
      dynamicFunctions,
      constants,
      functionMappers,
      queryAtoms,
      cwa,
      owa,
      probabilisticAtoms = Set.empty[AtomSignature],
      tristateAtoms = Set.empty[AtomSignature],
      atomStateDB,
      atomIdentifier
    )


    val orderedLiterals =
      clause
        .literals
        .toArray
        .sortBy(l => l)(ClauseLiteralsOrdering(mln))

    info("original sequence of literals : " + clause.literals.map(_.toString).mkString(" v ") + "\n" +
      "ordered sequence of literals  : " + orderedLiterals.map(_.toString).mkString(" v "))

    val orderedIdentityFunctions = orderedLiterals.map(literal => mln.identityFunctions(literal.sentence.signature))

    // extract the sequence of unique variables
    val uniqOrderedVars = uniqueOrderedVariablesIn(orderedLiterals)



    info(uniqOrderedVars.mkString(", "))

    // ordered unique variables to corresponding domain sizes
    val varDomains = uniqOrderedVars.map(v => constants(v.domain).size).toArray
    info("Domain sizes of the ordered unique variables: " + varDomains.mkString(", "))


    val var2Idx: Map[Variable, Int] = uniqOrderedVars.zipWithIndex.toMap

    val litLastVarIdx = orderedLiterals.map(literal => var2Idx(variablesIn(literal.sentence.terms).last))

    val varIdx2ConstantSet: Map[Int, ConstantsSet] = var2Idx.map{ case (v: Variable, idx: Int) => idx -> mln.constants(v.domain)}.toMap

    val matchTerms = matchedTerms(orderedLiterals, !_.isFunction)




    println(matchTerms.mkString(", "))



    /*private def substituteTerm(theta: collection.Map[Variable, String])(term: Term): String = term match {
        case c: Constant => c.symbol
        case v: Variable => theta(v)
        case f: TermFunction =>
          mln.functionMappers.get(f.signature) match {
            case Some(m) => m(f.terms.map(a => substituteTerm(theta)(a)))
            case None => fatal("Cannot apply substitution using theta: " + theta + " in function " + f.signature)
          }
      }*/

    def substituteTerm(sentence: AtomicFormula)(theta: Array[Int])(index: Int): Int ={
      sentence.terms(index) match {
        case f: TermFunction =>
          val mapper = mln
            .functionMappers
            .getOrElse(f.signature,
              fatal("Cannot apply substitution using indexed theta: " + theta.mkString(", ") + " in function " + f.signature))


        case _ => theta(index)
      }

      -1
    }


    //
    // Cartesian products
    //
    /*def cartesianProducts(source: Array[Int]): Set[Array[Int]] = {
      var products =  Set[Array[Int]]()

      // cartesian factors: current domain indexes for each unique ordered variable
      val indexes = util.Arrays.copyOf(source.map(_ - 1), source.length)
      val factors = util.Arrays.copyOf(indexes, indexes.length)

      val MAX = source.product


      val LAST_VARIABLE_INDEX = factors.length - 1
      var stop_inner = false
      var variable_idx = LAST_VARIABLE_INDEX
      var product_counter = 0

      val startTime = System.currentTimeMillis()
      while (variable_idx >= 0 && (product_counter < MAX)) {

        products += factors.clone()
        product_counter += 1

        stop_inner = false
        variable_idx = LAST_VARIABLE_INDEX

        while (!stop_inner && variable_idx >= 0) {
          //iterations += 1
          if (factors(variable_idx) > 0) {
            factors(variable_idx) -= 1
            stop_inner = true

            val pos = variable_idx + 1

            if (pos <= LAST_VARIABLE_INDEX)
              System.arraycopy(indexes, pos, factors, pos, LAST_VARIABLE_INDEX - pos + 1)
          }
          else variable_idx -= 1
        }



      }
      val endTime = System.currentTimeMillis()
      info("Total number of groundings: " + product_counter + " of " + MAX)
      info("Total number of stored groundings: " + products.size)
      info("Total time:" + Utilities.msecTimeToText(endTime - startTime))

      products
    }*/

    /*val LT_IDX = 3
    val tautology = (factors: Array[Int]) => factors(0) == factors(LT_IDX)

    def cartesianProductsT(source: Array[Int]): Set[Array[Int]] = {
      var products =  Set[Array[Int]]()
      var groundings = List[Array[Int]]()



      // cartesian factors: current domain indexes for each unique ordered variable
      val indexes = util.Arrays.copyOf(source.map(_ - 1), source.length)
      val factors = util.Arrays.copyOf(indexes, indexes.length)

      val MAX_PRODUCT = source.product


      val LAST_VARIABLE_INDEX = factors.length - 1
      var stop_inner = false
      var variable_idx = LAST_VARIABLE_INDEX
      var product_counter = 0
      //var iterations = 0
      //var outerIterations = 0
      //var copies = 0

      // Last tautological index
      /* val LT_IDX = 3
      val tautology = () => factors(0) == factors(LT_IDX)*/

      val startTime = System.currentTimeMillis()
      while (variable_idx >= 0 && (product_counter < MAX_PRODUCT)) {


        //println(counter + " : "+factors.mkString(", "))
        //outerIterations += 1

        if(!tautology(factors)) {
          variable_idx = LAST_VARIABLE_INDEX
          products += factors.clone()
          product_counter += 1
        }
        else
          variable_idx = LT_IDX

        //reset stop flag
        stop_inner = false

        while (!stop_inner && variable_idx >= 0) {
          //iterations += 1
          if (factors(variable_idx) > 0) {
            factors(variable_idx) -= 1
            stop_inner = true

            val pos = variable_idx + 1

            if (pos <= LAST_VARIABLE_INDEX)
              System.arraycopy(indexes, pos, factors, pos, LAST_VARIABLE_INDEX - pos + 1)
          }
          else variable_idx -= 1
        }



      }
      val endTime = System.currentTimeMillis()
      info("Total number of groundings: " + product_counter + " of " + MAX_PRODUCT)
      info("Total number of stored groundings: " + products.size)
      /*info("Total number of iterations: " + iterations)
      info("Total number of outer iterations: " + outerIterations)
      info("Total number of inner iterations: " + (iterations - outerIterations))*/
      info("Total time:" + Utilities.msecTimeToText(endTime - startTime))
      //info("Total copies:" + copies)

      products
    }*/


    //
    // TEST PRODUCTS
    //

    /*val products = cartesianProducts(varDomains)
    val productsT = cartesianProductsT(varDomains)

    val productsTStr = productsT.map(_.mkString(":"))
    val productsStr = products.map(_.mkString(":"))

    println("productsStr.size:="+productsStr.size)
    println("productsTStr.size:="+productsTStr.size)

    println("\nMISSING:")
    val missing = productsStr -- productsTStr
    val missingStr = missing.toArray.sorted

    missingStr.foreach(x => println("\t"+x))

    // ERROR 1
    val error1 = missing.filter(x => !tautology(x.split(":").map(_.toInt)))
    println("\nWRONG MISSING ENTRIES: "+error1.size)
    error1.foreach(x => println("\t"+x))

    // ERROR 2
    val error2 = productsTStr.filter(x => tautology(x.split(":").map(_.toInt)))
    println("\nWRONG COLLECTED ENTRIES: "+error2.size)
    error2.foreach(x => println("\t"+x))*/


    // Note: the given domains should always be above zero
    /*val domainList = List(
      Array(10, 5, 2),
      Array(10, 1, 2),
      Array(1, 1, 10),
      Array(1, 10),
      Array(5, 10),
      Array(10),
      Array(1),
      Array(1000, 1000, 2, 1000),
      Array(1000, 1000, 2, 2, 2, 1, 1, 2)
    )

    require(domainList.forall(_.forall(_ > 0)))

    for (domain <- domainList; (l, iteration) <- domain.permutations.zipWithIndex) {
      val elements = l.map(_ - 1)
      val expectedIterations = l.product // this is the correct number of products
      describe("Cartesian product of domains [" + elements.mkString(", ") + "]") {

        val result = cartesianProducts(l)
        info("iteration: " + iteration + "\n" +
          "\telements = [" + elements.map(_.toString).reduceLeft(_ + ", " + _) + "]\n" +
          "\texpected = " + expectedIterations + "\n" +
          "\tproduced = " + result)

        it("produces " + expectedIterations + " distinct Cartesian products") {
          assert(expectedIterations == result)
        }
      }
    }*/

  }


}
