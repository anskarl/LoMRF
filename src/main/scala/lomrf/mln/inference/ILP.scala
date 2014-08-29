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

  def infer() {

    var gcount = 0

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

      info("STEP 1")
     //write ground clause for debug
      val cl = constraint.literals.map {
        l =>
          decodeLiteral(l)(mrf.mln) match {
            case Some(litTXT) => litTXT
            case None => sys.error("Cannot decode literal: " + l)
          }
      }.reduceLeft(_ + " v " + _)
      info("Ground Clause: " + constraint.weight + " " + cl)

      // check ground clauses and perform the ILP transformation
      for(key <- constraint.literals) {
        val k = math.abs(key) // keys may be negative
        info("Key: " + key)
        if(!cacheLiterals.contains(k)) {
          cacheLiterals += ((k, LPFloatVar("y" + k, 0, 1)))
          gcount += 1
        }
//        if(constraint.weight < 0) {
//          if(key < 0) // TODO WARNING: for negative  weights the opposite constraints MUST be included!!!
//            constraints += (cacheLiterals(k))
//          else
//            constraints += (1 - cacheLiterals(k))
//        }
//        else {
          if (key > 0) // TODO WARNING: for negative  weights the opposite constraints MUST be included!!!
            constraints += (cacheLiterals(k))
          else
            constraints += (1 - cacheLiterals(k))
        //}
      }
      info("Constraints: [" + constraints.mkString(", ") + "]")
      //info("Literals Cache: [" + cacheLiterals.mkString(", ") + "]")

      info("STEP 2")
      val cid = math.abs(constraint.id)
      // store expressions for maximization equation
      if(!constraint.weight.isInfinite && !constraint.weight.isNaN && constraint.weight != mrf.weightHard) {
        if (constraint.isUnit)
          expressions += ( math.abs(constraint.weight) * cacheLiterals(math.abs(constraint.literals(0))) )
        else {
          if(!cacheClauses.contains(cid)) // does not change anything, no duplicate ground clauses
            cacheClauses += ((cid, LPFloatVar("z" + cid, 0, 1))) // keys may be negative
          expressions += (math.abs(constraint.weight) * cacheClauses(cid)) // id can be negative??
        }
      }
      info("Expressions: [" + expressions.mkString(", ") + "]")
      //info("Clauses Cache: " + cacheClauses.mkString(", "))

      info("STEP 3")
      if(constraint.weight.isInfinite || constraint.weight.isNaN || constraint.weight == mrf.weightHard) {
        add(sum(constraints) >= 1)
        info(constraints.mkString(" + ") + " >= 1")
      }
      else if(constraint.weight > 0 && !constraint.isUnit) { // TODO if weight is 0 ??? which is the case
        add(sum(constraints) >= cacheClauses(cid))
        info(constraints.mkString(" + ") + " >= " + cacheClauses(cid).name)
      }
      else if(constraint.weight < 0 && !constraint.isUnit) {
        for(constr <- constraints) {
          add(constr >= cacheClauses(cid))
          info(constr + " >= " + cacheClauses(cid).name)
        }
      }

      constraints.clear()
      //cacheClauses.clear()
    }

    maximize(sum(expressions))
    start()
    release()

    println("Literal Vars: "+cacheLiterals.size + ", Clauses Vars: "+cacheClauses.size +", Total: "+(cacheLiterals.size + cacheClauses.size))
    println("Ground Atoms: "+ mrf.numberOfAtoms + ", Counted: "+ gcount)

    println("===========================")
    println("objective: "+ objectiveValue)
    for((k,v) <- cacheLiterals)
      println(v.name + "= " + v.value)
    for((k,v) <- cacheClauses)
      println(v.name + "= " + v.value)

    for( (k, v) <- cacheLiterals) {
      println(decodeLiteral(k)(mrf.mln).get + " " + v.value.get)
    }

  }

/*  def writeResults(out: PrintStream = System.out) {
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
