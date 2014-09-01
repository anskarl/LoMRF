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

import lomrf.util._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import oscar.linprog.modeling._
import oscar.algebra._
import java.io.PrintStream
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.TIntObjectHashMap
import scala.collection.mutable
import gnu.trove.procedure.{TIntObjectProcedure, TObjectProcedure, TIntProcedure}

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
 * @author Vagelis Michelioudakis
 */
final class ILP(mrf: MRF) extends LPModel(LPSolverLib.lp_solve) with Logging {

  def infer(out: PrintStream = System.out) {

    // Hash map containing pairs of unique literal values to LD variables
    val cacheLiterals = new HashMap[Int, LPFloatVar]()
    val cacheClauses = new HashMap[Int, LPFloatVar]()

    // Array containing expressions of the equation we want to maximize, of the form weight * LD variable
    val expressions = ArrayBuffer[LinearExpression]()
    val constraints =  ArrayBuffer[LinearExpression]()

    val constraintsIterator = mrf.constraints.iterator() // iterator on the ground clauses

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()

      if(isDebugEnabled) { // print ground clause for debug
        val clause = constraint.literals.map {
          l =>
            decodeLiteral(l)(mrf.mln) match {
              case Some(litTXT) => litTXT
              case None => sys.error("Cannot decode literal: " + l)
            }
        }.reduceLeft(_ + " v " + _)
        debug("Ground Clause: " + constraint.weight + " " + clause)
      }

      // STEP 1: Introduce variables for each ground atom and create possible constraints
      for(key <- constraint.literals) {
        val k = math.abs(key) // key is negative if the literal is negative

        if(!cacheLiterals.contains(k))
          cacheLiterals += ( (k, LPFloatVar("y" + k, 0, 1)) )

        if( (constraint.weight > 0 || constraint.weight.isInfinite || constraint.weight.isNaN ||
          constraint.weight == mrf.weightHard) && key > 0)
            constraints += (cacheLiterals(k))
        else if( (constraint.weight > 0 || constraint.weight.isInfinite || constraint.weight.isNaN ||
          constraint.weight == mrf.weightHard) && key < 0)
            constraints += (1 - cacheLiterals(k))
        else if(constraint.weight < 0 && key < 0)
            constraints += (cacheLiterals(k))
        else
            constraints += (1 - cacheLiterals(k))
      }
      if(isDebugEnabled)
        debug("Possible Constraints: [" + constraints.mkString(", ") + "]")

      val cid = math.abs(constraint.id)

      // Step 2: Create expressions for objective function (only for soft constraints)
      if(!constraint.weight.isInfinite && !constraint.weight.isNaN && constraint.weight != mrf.weightHard) {

        if(constraint.isUnit && constraint.weight > 0 && constraint.literals(0) > 0)
          expressions += ( constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else if(constraint.isUnit && constraint.weight > 0 && constraint.literals(0) < 0)
          expressions += ( -constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else if(constraint.isUnit && constraint.weight < 0 && constraint.literals(0) > 0)
          expressions += ( constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else if(constraint.isUnit && constraint.weight < 0 && constraint.literals(0) < 0)
          expressions += ( -constraint.weight * cacheLiterals(math.abs(constraint.literals(0))) )
        else {
          if(!cacheClauses.contains(cid))
            cacheClauses += ( (cid, LPFloatVar("z" + cid, 0, 1)) )
          expressions += ( math.abs(constraint.weight) * cacheClauses(cid) )
        }

      }
      if(isDebugEnabled)
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

      constraints.clear()
    }

    info("Ground Atoms: "+ mrf.numberOfAtoms)
    info("Atom Variables: " + cacheLiterals.size + " + Clauses Variables: " + cacheClauses.size + " = "
      + (cacheLiterals.size + cacheClauses.size))

    maximize(sum(expressions))
    start()
    release()

    info("=========================== Solution ===========================")
    info("Are constraints satisfied: " + checkConstraints())
    info("Solution status: " + status.toString)
    info("Objective = "+ objectiveValue.get)

    if(isDebugEnabled) { // before rounding up
      for( (k, v) <- cacheLiterals )
        debug(v.name + " = " + v.value.get)
      for( (k, v) <- cacheClauses )
        debug(v.name + " = " + v.value.get)
    }

    val solution = new HashMap[Int, Double]()
    for( (k, v) <- cacheLiterals )
      solution += ( (k, v.value.get) )

    for( (k, v) <- solution) {
      debug(decodeLiteral(k)(mrf.mln).get + " " + v)
    }


    roundup(solution, mrf.constraints)

    for( (k, v) <- solution) {
      out.println(decodeLiteral(k)(mrf.mln).get + " " + v.toInt)
    }
//    for( (k, v) <- cacheLiterals) {
//     if(v.value.get <= 0.0 || v.value.get >= 1.0) // warning rounding to int may cause problems (new hashmap)
//        out.println(decodeLiteral(k)(mrf.mln).get + " " + v.value.get.toInt)
//      else
//        out.println(decodeLiteral(k)(mrf.mln).get + " " + v.value.get)
//    }

  }

  def roundup(solution: HashMap[Int, Double], groundClauses: TIntObjectMap[Constraint]) {

      val F = new HashMap[Int, Double]()
      val unSatConstraints = new TIntObjectHashMap[Constraint]

      for( (k, v) <- solution ) {
        val constraintsIterator = groundClauses.iterator() // iterator on the ground clauses
        while (constraintsIterator.hasNext) {
          constraintsIterator.advance()
          val key = constraintsIterator.key()
          val constraint = constraintsIterator.value()

          if( !(v == 1.0 && constraint.literals.contains(k)) && !(v == 0.0 && constraint.literals.contains(-k)) )
            unSatConstraints.put(key, constraint) // problematic
        }
        if(v != 0.0 && v != 1.0) F += ( (k, v) )
      }

      for( (k, v) <- F ) {
        var wP, wN = 0.0
        val constraintsIterator = unSatConstraints.iterator() // iterator on the ground clauses
        while(constraintsIterator.hasNext) {
          constraintsIterator.advance()
          val constraint = constraintsIterator.value()
          if(constraint.literals.contains(k))
            wP += constraint.weight
          if(constraint.literals.contains(-k))
            wN += constraint.weight
        }
        if(wP > wN) solution(k) = 1.0 else solution(k) = 0.0

        val y = solution(k)
        unSatConstraints.retainEntries(new TIntObjectProcedure[Constraint] {
          override def execute(a: Int, b: Constraint): Boolean = {
            !(y == 1.0 && b.literals.contains(k)) && !(y == 0.0 && b.literals.contains(-k))
          }
        })
      }

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
