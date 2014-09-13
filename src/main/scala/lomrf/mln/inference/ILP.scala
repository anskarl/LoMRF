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

package lomrf.mln.inference

import lomrf.mln.model.mrf._
import lomrf.util._
import oscar.linprog.modeling._
import oscar.algebra._
import java.io.PrintStream
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.TIntObjectHashMap
import scala.collection.mutable
import gnu.trove.procedure.TIntObjectProcedure

/**
 * This is an implementation of an approximate MAP inference algorithm for MLNs using Integer Linear Programming.
 * The original implementation of the algorithm can be found in: [[http://alchemy.cs.washington.edu/code/]].
 * Details about the ILP algorithm can be found in the following publication:
 *
 * <ul>
 * <li> Tuyen N. Huynh and Raymond J. Mooney. (2011) Max-Margin Weight Learning for Markov Logic Networks.
 * In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery in Databases
 * (ECML-PKDD 2011), Vol. 2, pp. 81-96, September 2011.
 * The paper can be found in [[http://www.cs.utexas.edu/users/ai-lab/?HuynhTuyen]]
 * </ul>
 *
 * @param mrf The ground Markov network
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class ILP(mrf: MRF) extends LPModel(LPSolverLib.lp_solve) with Logging {
  implicit val mln = mrf.mln

  def infer(out: PrintStream = System.out) {

    // Hash map containing pairs of unique literal values to LD variables
    var cacheLiterals = Map[Int, LPFloatVar]()
    var cacheClauses = Map[Int, LPFloatVar]()

    /**
     * A collection of expressions of the equation that we aim to maximize.
     * Each expression has the following form:
     *
     * {{{ weight * LD variable}}}
     */
    var expressions = List[LinearExpression]()

    var constraintsIterator = mrf.constraints.iterator() // iterator on the ground clauses

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()

      var constraints: List[LinearExpression] =  Nil
      // fetch the current constraint, i.e., current weighted ground clause or clique
      val constraint = constraintsIterator.value()

      debug {
        "Ground Clause: " + constraint.weight + " " +
          constraint
            .literals
            .map (l => decodeLiteral(l).getOrElse(sys.error("Cannot decode literal: " + l)))
            .reduceLeft(_ + " v " + _)
      }

      // STEP 1: Introduce variables for each ground atom and create possible constraints
      for(key <- constraint.literals) {
        val k = math.abs(key) // key is negative if the literal is negative

        if(!cacheLiterals.contains(k))
          cacheLiterals += ( (k, LPFloatVar("y" + k, 0, 1)) )

        if( (constraint.weight > 0 || constraint.weight.isInfinite || constraint.weight.isNaN ||
          constraint.weight == mrf.weightHard) && key > 0)
            constraints ::= cacheLiterals(k)
        else if( (constraint.weight > 0 || constraint.weight.isInfinite || constraint.weight.isNaN ||
          constraint.weight == mrf.weightHard) && key < 0)
            constraints ::= (1 - cacheLiterals(k))
        else if(constraint.weight < 0 && key < 0)
            constraints ::= cacheLiterals(k)
        else
            constraints ::= (1 - cacheLiterals(k))
      }

      debug("Possible Constraints: [" + constraints.mkString(", ") + "]")

      val cid = math.abs(constraint.id)

      // Step 2: Create expressions for objective function (only for soft constraints)
      if(!constraint.weight.isInfinite && !constraint.weight.isNaN && constraint.weight != mrf.weightHard) {

        if(constraint.isUnit && constraint.weight > 0 && constraint.literals(0) > 0)
          expressions ::= ( constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else if(constraint.isUnit && constraint.weight > 0 && constraint.literals(0) < 0)
          expressions ::= ( -constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else if(constraint.isUnit && constraint.weight < 0 && constraint.literals(0) > 0)
          expressions ::= ( constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else if(constraint.isUnit && constraint.weight < 0 && constraint.literals(0) < 0)
          expressions ::= ( -constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else {
          if(!cacheClauses.contains(cid)) cacheClauses += ( (cid, LPFloatVar("z" + cid, 0, 1)) )

          expressions ::= ( math.abs(constraint.weight) * cacheClauses(cid) )
        }

      }

      debug("Expressions: [" + expressions.mkString(", ") + "]")

      // Step 3: Add constraints to the solver
      if(constraint.weight.isInfinite || constraint.weight.isNaN || constraint.weight == mrf.weightHard) {
        add(sum(constraints) >= 1)
        if(isDebugEnabled) debug(constraints.mkString(" + ") + " >= 1")
      }
      else if(constraint.weight > 0 && !constraint.isUnit) {
        add(sum(constraints) >= cacheClauses(cid))
        if(isDebugEnabled) debug(constraints.mkString(" + ") + " >= " + cacheClauses(cid).name)
      }
      else if(constraint.weight < 0 && !constraint.isUnit) {
        for(c <- constraints) {
          add(c >= cacheClauses(cid))
          if(isDebugEnabled) debug(c + " >= " + cacheClauses(cid).name)
        }
      }
    }

    info(
      "\nGround Atoms: "+ mrf.numberOfAtoms +
      "\nAtom Variables: " + cacheLiterals.size + " + Clauses Variables: " + cacheClauses.size +
              " = " + (cacheLiterals.size + cacheClauses.size))


    maximize(sum(expressions))
    start()
    release()

    info(
      "\n=========================== Solution ===========================" +
      "\nAre constraints satisfied: " + checkConstraints() +
      "\nSolution status: " + status.toString +
      "\nObjective = "+ objectiveValue.get)


    // ?????
    /*if(isDebugEnabled) { // before rounding up
      for( (k, v) <- cacheLiterals )
        info(v.name + " = " + v.value.get)
      for( (k, v) <- cacheClauses )
        debug(v.name + " = " + v.value.get)
    }*/

    val solution = new mutable.HashMap[Int, Double]()
    for( (k, v) <- cacheLiterals )
      solution += ( (k, v.value.get) )

    info("Non-Integral Solutions: " + solution.count(p => p._2 != 0.0 && p._2 != 1.0))

    if(solution.exists(p => p._2 != 0.0 && p._2 != 1.0)) {
      val clauses = new TIntObjectHashMap[Constraint]()
      clauses.putAll(mrf.constraints)
      roundup(solution, clauses)
    }

    var w = 0.0
    var wNAll = 0
    constraintsIterator = mrf.constraints.iterator()
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      if(constraint.weight.isInfinite || constraint.weight.isNaN || constraint.weight == mrf.weightHard) w += mrf.weightHard
      else if(constraint.weight > 0) w += constraint.weight
      else wNAll += 1
    }
    info("#Clauses with negative weights: "+wNAll+"/"+mrf.constraints.size())
    info("Likelihood UBound: e^"+ w)

    for( (k, v) <- solution)
      out.println(decodeLiteral(k).get + " " + v.toInt)

  }

  def roundup(solution: mutable.HashMap[Int, Double], groundClauses: TIntObjectMap[Constraint]) {
      var F = List[(Double, Int, Double)]()

      val constraints = new TIntObjectHashMap[Constraint]()
      val unSatConstraints = new TIntObjectHashMap[Constraint]()

      constraints.putAll(groundClauses)
      for( (k, v) <- solution ) {
        constraints.retainEntries(new TIntObjectProcedure[Constraint] {
          override def execute(a: Int, b: Constraint): Boolean = {
            !((v == 1.0 && b.literals.contains(k)) || (v == 0.0 && b.literals.contains(-k)))
          }
        })
        if(v != 0.0 && v != 1.0) F ::= ( (0.0, k, v) )
      }

      info("Fractional Solutions: "+F.size)
      unSatConstraints.putAll(constraints)
      //info("#UnSAT Clauses: " + unSatConstraints.size()) //?????

      for (i <- 0 until F.size) {
        val k = F(i)._2
        val v = F(i)._3

        var delta = 0.0

        val constraintsIterator = unSatConstraints.iterator()

        while (constraintsIterator.hasNext) {
          constraintsIterator.advance()
          val constraint = constraintsIterator.value()
          if (constraint.literals.contains(k))
            delta += constraint.weight
          if (constraint.literals.contains(-k))
            delta -= constraint.weight
        }

        solution(k) = if(delta > 0) 1.0 else 0.0

        val y = solution(k)
        unSatConstraints.retainEntries(new TIntObjectProcedure[Constraint] {
          override def execute(a: Int, b: Constraint): Boolean = {
            !((y == 1.0 && b.literals.contains(k)) || (y == 0.0 && b.literals.contains(-k)))
          }
        })
        info("#UnSAT Clauses: " + unSatConstraints.size())
        info("SOLUTION: " + y)
      }

    var wNUnSat = 0
    var constraintsIterator = unSatConstraints.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()

      val key = constraintsIterator.key()
      val constraint = constraintsIterator.value()
      groundClauses.remove(key)

      if(constraint.weight < 0) wNUnSat += 1
    }


    var w = 0.0
    constraintsIterator = groundClauses.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()


      if(constraint.weight.isInfinite || constraint.weight.isNaN || constraint.weight == mrf.weightHard)
        w += mrf.weightHard
      else
        w += constraint.weight
    }

    info(
      "\n#SAT Clauses: " + groundClauses.size()+
      "\n#UnSAT clauses with negative weights: "+wNUnSat+"/"+unSatConstraints.size()+
      "\nLikelihood of the solution is: e^" + w)
  }

  /*def writeResults(out: PrintStream = System.out) {
    import lomrf.util.decodeAtom

    implicit val mln = mrf.mln

    val iterator = mrf.atoms.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()
      if (atomID >= mln.queryStartID && atomID <= mln.queryEndID) {
        val groundAtom = iterator.value()
        val state = if(groundAtom.getState) 1 else 0
        if(showAll) {
          decodeAtom(iterator.key()) match {
            case Some(txtAtom) => out.println(txtAtom + " " + state)
            case _ => error("failed to decode id:" + atomID)
          }
        }
        else {
          if(state == 1) decodeAtom(iterator.key()) match {
            case Some(txtAtom) => out.println(txtAtom + " " + state)
            case _ => error("failed to decode id:" + atomID)
          }
        }
      }
    }
  }*/

}
