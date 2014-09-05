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
import lomrf.logic.AtomSignature

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

    var constraintsIterator = mrf.constraints.iterator() // iterator on the ground clauses

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
        info(v.name + " = " + v.value.get)
      for( (k, v) <- cacheClauses )
        debug(v.name + " = " + v.value.get)
    }

    val solution = new HashMap[Int, Double]()
    for( (k, v) <- cacheLiterals )
      solution += ( (k, v.value.get) )

//    for( (k, v) <- solution) {
//      info(decodeLiteral(k)(mrf.mln).get + " " + v)
//    }

    info("Non-Integral Solutions: " + solution.count(p => p._2 != 0.0 && p._2 != 1.0))

    if(solution.exists(p => p._2 != 0.0 && p._2 != 1.0)) {
      val clauses = new TIntObjectHashMap[Constraint]()
      clauses.putAll(mrf.constraints)
      roundup(solution, clauses)
    }

    var w = 0.0
    var wNAll = 0
    constraintsIterator = mrf.constraints.iterator() // iterator on the ground clauses
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      if(constraint.weight.isInfinite || constraint.weight.isNaN || constraint.weight == mrf.weightHard) w += mrf.weightHard
      else if(constraint.weight > 0) w += constraint.weight
      else wNAll += 1
    }
    info("#Clauses with negative weights: "+wNAll+"/"+mrf.constraints.size())
    info("Likelihood UBound: e^"+ w)

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

      //val idf = mrf.mln.identityFunctions(AtomSignature("HoldsAt", 2))
      //val al_seq = Array("HoldsAt(Meet_ID6_ID2,957)", "HoldsAt(Meet_ID6_ID2,956)", "HoldsAt(Meet_ID6_ID2,955)", "HoldsAt(Meet_ID6_ID2,954)", "HoldsAt(Meet_ID6_ID2,953)", "HoldsAt(Meet_ID6_ID2,952)", "HoldsAt(Meet_ID6_ID2,951)", "HoldsAt(Meet_ID6_ID2,950)", "HoldsAt(Meet_ID6_ID2,949)", "HoldsAt(Meet_ID6_ID2,948)", "HoldsAt(Meet_ID6_ID2,947)", "HoldsAt(Meet_ID6_ID2,946)", "HoldsAt(Meet_ID6_ID2,945)", "HoldsAt(Meet_ID6_ID2,944)", "HoldsAt(Meet_ID6_ID2,943)", "HoldsAt(Meet_ID6_ID2,942)", "HoldsAt(Meet_ID6_ID2,941)", "HoldsAt(Meet_ID6_ID2,940)", "HoldsAt(Meet_ID6_ID2,939)", "HoldsAt(Meet_ID6_ID2,938)", "HoldsAt(Meet_ID6_ID2,937)", "HoldsAt(Meet_ID6_ID2,936)", "HoldsAt(Meet_ID6_ID2,935)", "HoldsAt(Meet_ID6_ID2,934)", "HoldsAt(Meet_ID6_ID2,933)", "HoldsAt(Meet_ID6_ID2,932)", "HoldsAt(Meet_ID6_ID2,931)", "HoldsAt(Meet_ID6_ID2,930)", "HoldsAt(Meet_ID6_ID2,929)", "HoldsAt(Meet_ID6_ID2,928)", "HoldsAt(Meet_ID6_ID2,927)", "HoldsAt(Meet_ID6_ID2,926)", "HoldsAt(Meet_ID6_ID2,925)", "HoldsAt(Meet_ID6_ID2,924)", "HoldsAt(Meet_ID6_ID2,923)", "HoldsAt(Meet_ID6_ID2,922)", "HoldsAt(Meet_ID6_ID2,921)", "HoldsAt(Meet_ID6_ID2,920)", "HoldsAt(Meet_ID6_ID2,919)", "HoldsAt(Meet_ID6_ID2,918)", "HoldsAt(Meet_ID6_ID2,917)", "HoldsAt(Meet_ID6_ID2,916)", "HoldsAt(Meet_ID6_ID2,915)", "HoldsAt(Meet_ID6_ID2,914)", "HoldsAt(Meet_ID6_ID2,913)", "HoldsAt(Meet_ID6_ID2,912)", "HoldsAt(Meet_ID6_ID2,911)", "HoldsAt(Meet_ID6_ID2,910)", "HoldsAt(Meet_ID6_ID2,909)", "HoldsAt(Meet_ID6_ID2,908)", "HoldsAt(Meet_ID6_ID2,907)", "HoldsAt(Meet_ID6_ID2,906)", "HoldsAt(Meet_ID6_ID2,905)", "HoldsAt(Meet_ID6_ID2,904)", "HoldsAt(Meet_ID6_ID2,903)", "HoldsAt(Meet_ID6_ID2,902)", "HoldsAt(Meet_ID6_ID2,901)", "HoldsAt(Meet_ID6_ID2,900)", "HoldsAt(Meet_ID6_ID2,899)", "HoldsAt(Meet_ID6_ID2,898)", "HoldsAt(Meet_ID6_ID2,897)", "HoldsAt(Meet_ID6_ID2,896)", "HoldsAt(Meet_ID6_ID2,895)", "HoldsAt(Meet_ID6_ID2,894)", "HoldsAt(Meet_ID6_ID2,893)", "HoldsAt(Meet_ID6_ID2,892)", "HoldsAt(Meet_ID6_ID2,891)", "HoldsAt(Meet_ID6_ID2,890)", "HoldsAt(Meet_ID6_ID2,889)", "HoldsAt(Meet_ID6_ID2,888)", "HoldsAt(Meet_ID6_ID2,887)", "HoldsAt(Meet_ID6_ID2,886)", "HoldsAt(Meet_ID6_ID2,885)", "HoldsAt(Meet_ID6_ID2,884)", "HoldsAt(Meet_ID6_ID2,883)", "HoldsAt(Meet_ID6_ID2,882)", "HoldsAt(Meet_ID6_ID2,881)", "HoldsAt(Meet_ID6_ID2,880)", "HoldsAt(Meet_ID6_ID2,879)", "HoldsAt(Meet_ID6_ID2,878)", "HoldsAt(Meet_ID6_ID2,877)", "HoldsAt(Meet_ID6_ID2,876)", "HoldsAt(Meet_ID6_ID2,875)", "HoldsAt(Meet_ID6_ID2,874)", "HoldsAt(Meet_ID6_ID2,873)", "HoldsAt(Meet_ID6_ID2,872)", "HoldsAt(Meet_ID6_ID2,871)", "HoldsAt(Meet_ID6_ID2,870)", "HoldsAt(Meet_ID6_ID2,869)", "HoldsAt(Meet_ID6_ID2,868)", "HoldsAt(Meet_ID6_ID2,867)", "HoldsAt(Meet_ID6_ID2,866)", "HoldsAt(Meet_ID6_ID2,865)", "HoldsAt(Meet_ID6_ID2,864)", "HoldsAt(Meet_ID6_ID2,863)", "HoldsAt(Meet_ID6_ID2,862)", "HoldsAt(Meet_ID6_ID2,861)", "HoldsAt(Meet_ID6_ID2,860)", "HoldsAt(Meet_ID6_ID2,859)", "HoldsAt(Meet_ID6_ID2,858)", "HoldsAt(Meet_ID6_ID2,857)", "HoldsAt(Meet_ID6_ID2,856)", "HoldsAt(Meet_ID6_ID2,855)", "HoldsAt(Meet_ID6_ID2,854)", "HoldsAt(Meet_ID6_ID2,853)", "HoldsAt(Meet_ID6_ID2,852)", "HoldsAt(Meet_ID6_ID2,851)", "HoldsAt(Meet_ID6_ID2,850)", "HoldsAt(Meet_ID6_ID2,849)", "HoldsAt(Meet_ID6_ID2,848)", "HoldsAt(Meet_ID6_ID2,847)", "HoldsAt(Meet_ID6_ID2,846)", "HoldsAt(Meet_ID6_ID2,845)", "HoldsAt(Meet_ID6_ID2,844)", "HoldsAt(Meet_ID6_ID2,843)", "HoldsAt(Meet_ID6_ID2,842)", "HoldsAt(Meet_ID6_ID2,841)", "HoldsAt(Meet_ID6_ID2,840)", "HoldsAt(Meet_ID6_ID2,839)", "HoldsAt(Meet_ID6_ID2,838)", "HoldsAt(Meet_ID6_ID2,837)", "HoldsAt(Meet_ID6_ID2,836)", "HoldsAt(Meet_ID6_ID2,835)", "HoldsAt(Meet_ID6_ID2,834)", "HoldsAt(Meet_ID6_ID2,833)", "HoldsAt(Meet_ID6_ID2,832)", "HoldsAt(Meet_ID6_ID2,831)", "HoldsAt(Meet_ID6_ID2,830)", "HoldsAt(Meet_ID6_ID2,829)", "HoldsAt(Meet_ID6_ID2,828)", "HoldsAt(Meet_ID6_ID2,827)", "HoldsAt(Meet_ID6_ID2,826)", "HoldsAt(Meet_ID6_ID2,825)", "HoldsAt(Meet_ID6_ID2,824)", "HoldsAt(Meet_ID6_ID2,823)", "HoldsAt(Meet_ID6_ID2,822)", "HoldsAt(Meet_ID6_ID2,821)", "HoldsAt(Meet_ID6_ID2,820)", "HoldsAt(Meet_ID6_ID2,819)", "HoldsAt(Meet_ID6_ID2,818)", "HoldsAt(Meet_ID6_ID2,817)", "HoldsAt(Meet_ID6_ID2,816)", "HoldsAt(Meet_ID6_ID2,815)", "HoldsAt(Meet_ID6_ID2,814)", "HoldsAt(Meet_ID6_ID2,813)", "HoldsAt(Meet_ID6_ID2,812)", "HoldsAt(Meet_ID6_ID2,811)", "HoldsAt(Meet_ID6_ID2,810)", "HoldsAt(Meet_ID6_ID2,809)", "HoldsAt(Meet_ID6_ID2,808)", "HoldsAt(Meet_ID6_ID2,807)", "HoldsAt(Meet_ID6_ID2,806)", "HoldsAt(Meet_ID6_ID2,805)", "HoldsAt(Meet_ID6_ID2,804)", "HoldsAt(Meet_ID6_ID2,803)", "HoldsAt(Meet_ID6_ID2,802)", "HoldsAt(Meet_ID6_ID2,801)", "HoldsAt(Meet_ID6_ID2,800)", "HoldsAt(Meet_ID6_ID2,799)", "HoldsAt(Meet_ID6_ID2,798)", "HoldsAt(Meet_ID6_ID2,797)", "HoldsAt(Meet_ID6_ID2,796)", "HoldsAt(Meet_ID6_ID2,795)", "HoldsAt(Meet_ID6_ID2,794)", "HoldsAt(Meet_ID6_ID2,793)", "HoldsAt(Meet_ID6_ID2,792)", "HoldsAt(Meet_ID6_ID2,791)", "HoldsAt(Meet_ID6_ID2,790)", "HoldsAt(Meet_ID6_ID2,789)", "HoldsAt(Meet_ID6_ID2,788)", "HoldsAt(Meet_ID6_ID2,787)", "HoldsAt(Meet_ID6_ID2,786)", "HoldsAt(Meet_ID6_ID2,785)", "HoldsAt(Meet_ID6_ID2,784)", "HoldsAt(Meet_ID6_ID2,783)", "HoldsAt(Meet_ID6_ID2,782)", "HoldsAt(Meet_ID6_ID2,781)", "HoldsAt(Meet_ID6_ID2,780)", "HoldsAt(Meet_ID6_ID2,779)", "HoldsAt(Meet_ID6_ID2,778)", "HoldsAt(Meet_ID6_ID2,777)", "HoldsAt(Meet_ID6_ID2,776)", "HoldsAt(Meet_ID6_ID2,775)", "HoldsAt(Meet_ID6_ID2,774)", "HoldsAt(Meet_ID6_ID2,773)", "HoldsAt(Meet_ID6_ID2,772)", "HoldsAt(Meet_ID6_ID2,771)", "HoldsAt(Meet_ID6_ID2,770)", "HoldsAt(Meet_ID6_ID2,769)", "HoldsAt(Meet_ID6_ID2,768)", "HoldsAt(Meet_ID6_ID2,767)", "HoldsAt(Meet_ID6_ID2,766)", "HoldsAt(Meet_ID6_ID2,765)", "HoldsAt(Meet_ID6_ID2,764)", "HoldsAt(Meet_ID6_ID2,763)", "HoldsAt(Meet_ID6_ID2,762)", "HoldsAt(Meet_ID6_ID2,761)", "HoldsAt(Meet_ID6_ID2,760)", "HoldsAt(Meet_ID6_ID2,759)", "HoldsAt(Meet_ID6_ID2,758)", "HoldsAt(Meet_ID6_ID2,757)", "HoldsAt(Meet_ID6_ID2,756)", "HoldsAt(Meet_ID6_ID2,755)", "HoldsAt(Meet_ID6_ID2,754)", "HoldsAt(Meet_ID6_ID2,753)", "HoldsAt(Meet_ID6_ID2,752)", "HoldsAt(Meet_ID6_ID2,751)", "HoldsAt(Meet_ID6_ID2,750)", "HoldsAt(Meet_ID6_ID2,749)", "HoldsAt(Meet_ID6_ID2,748)", "HoldsAt(Meet_ID6_ID2,747)", "HoldsAt(Meet_ID6_ID2,746)", "HoldsAt(Meet_ID6_ID2,745)", "HoldsAt(Meet_ID6_ID2,744)", "HoldsAt(Meet_ID6_ID2,743)", "HoldsAt(Meet_ID6_ID2,742)", "HoldsAt(Meet_ID6_ID2,741)", "HoldsAt(Meet_ID6_ID2,740)", "HoldsAt(Meet_ID6_ID2,739)", "HoldsAt(Meet_ID6_ID2,738)", "HoldsAt(Meet_ID6_ID2,737)", "HoldsAt(Meet_ID6_ID2,736)", "HoldsAt(Meet_ID6_ID2,735)", "HoldsAt(Meet_ID6_ID2,734)", "HoldsAt(Meet_ID6_ID2,733)", "HoldsAt(Meet_ID6_ID2,732)", "HoldsAt(Meet_ID6_ID2,731)", "HoldsAt(Meet_ID6_ID2,730)", "HoldsAt(Meet_ID6_ID2,729)", "HoldsAt(Meet_ID6_ID2,728)", "HoldsAt(Meet_ID6_ID2,727)", "HoldsAt(Meet_ID6_ID2,726)", "HoldsAt(Meet_ID6_ID2,725)", "HoldsAt(Meet_ID6_ID2,724)", "HoldsAt(Meet_ID6_ID2,723)", "HoldsAt(Meet_ID6_ID2,722)", "HoldsAt(Meet_ID6_ID2,721)", "HoldsAt(Meet_ID6_ID2,720)", "HoldsAt(Meet_ID6_ID2,719)", "HoldsAt(Meet_ID6_ID2,718)", "HoldsAt(Meet_ID6_ID2,717)", "HoldsAt(Meet_ID6_ID2,716)", "HoldsAt(Meet_ID6_ID2,715)", "HoldsAt(Meet_ID6_ID2,714)", "HoldsAt(Meet_ID6_ID2,713)", "HoldsAt(Meet_ID6_ID2,712)", "HoldsAt(Meet_ID6_ID2,711)", "HoldsAt(Meet_ID6_ID2,710)", "HoldsAt(Meet_ID6_ID2,709)", "HoldsAt(Meet_ID6_ID2,708)", "HoldsAt(Meet_ID6_ID2,707)", "HoldsAt(Meet_ID6_ID2,706)", "HoldsAt(Meet_ID6_ID2,705)", "HoldsAt(Meet_ID6_ID2,704)", "HoldsAt(Meet_ID6_ID2,703)", "HoldsAt(Meet_ID6_ID2,702)", "HoldsAt(Meet_ID6_ID2,701)", "HoldsAt(Meet_ID6_ID2,700)", "HoldsAt(Meet_ID6_ID2,699)", "HoldsAt(Meet_ID6_ID2,698)", "HoldsAt(Meet_ID6_ID2,697)", "HoldsAt(Meet_ID6_ID2,696)", "HoldsAt(Meet_ID6_ID2,695)", "HoldsAt(Meet_ID6_ID2,694)", "HoldsAt(Meet_ID6_ID2,693)", "HoldsAt(Meet_ID6_ID2,692)", "HoldsAt(Meet_ID6_ID2,691)", "HoldsAt(Meet_ID6_ID2,690)", "HoldsAt(Meet_ID6_ID2,689)", "HoldsAt(Meet_ID6_ID2,688)", "HoldsAt(Meet_ID6_ID2,687)", "HoldsAt(Meet_ID6_ID2,686)", "HoldsAt(Meet_ID6_ID2,685)", "HoldsAt(Meet_ID6_ID2,684)", "HoldsAt(Meet_ID6_ID2,683)", "HoldsAt(Meet_ID6_ID2,682)", "HoldsAt(Meet_ID6_ID2,681)", "HoldsAt(Meet_ID6_ID2,680)", "HoldsAt(Meet_ID6_ID2,679)", "HoldsAt(Meet_ID6_ID2,678)", "HoldsAt(Meet_ID6_ID2,677)", "HoldsAt(Meet_ID6_ID2,676)", "HoldsAt(Meet_ID6_ID2,675)", "HoldsAt(Meet_ID6_ID2,674)", "HoldsAt(Meet_ID6_ID2,673)", "HoldsAt(Meet_ID6_ID2,672)", "HoldsAt(Meet_ID6_ID2,671)", "HoldsAt(Meet_ID6_ID2,670)", "HoldsAt(Meet_ID6_ID2,669)", "HoldsAt(Meet_ID6_ID2,668)", "HoldsAt(Meet_ID6_ID2,667)", "HoldsAt(Meet_ID6_ID2,666)", "HoldsAt(Meet_ID6_ID2,665)", "HoldsAt(Meet_ID6_ID2,664)", "HoldsAt(Meet_ID6_ID2,663)", "HoldsAt(Meet_ID6_ID2,662)", "HoldsAt(Meet_ID6_ID2,661)", "HoldsAt(Meet_ID6_ID2,660)", "HoldsAt(Meet_ID6_ID2,659)", "HoldsAt(Meet_ID6_ID2,658)", "HoldsAt(Meet_ID6_ID2,657)", "HoldsAt(Meet_ID6_ID2,656)", "HoldsAt(Meet_ID6_ID2,655)", "HoldsAt(Meet_ID6_ID2,654)", "HoldsAt(Meet_ID6_ID2,653)", "HoldsAt(Meet_ID6_ID2,652)", "HoldsAt(Meet_ID6_ID2,651)", "HoldsAt(Meet_ID6_ID2,650)", "HoldsAt(Meet_ID6_ID2,649)", "HoldsAt(Meet_ID6_ID2,648)", "HoldsAt(Meet_ID6_ID2,647)", "HoldsAt(Meet_ID6_ID2,646)", "HoldsAt(Meet_ID6_ID2,645)", "HoldsAt(Meet_ID6_ID2,644)", "HoldsAt(Meet_ID6_ID2,643)", "HoldsAt(Meet_ID6_ID2,642)", "HoldsAt(Meet_ID6_ID2,641)", "HoldsAt(Meet_ID6_ID2,640)", "HoldsAt(Meet_ID6_ID2,639)", "HoldsAt(Meet_ID6_ID2,638)", "HoldsAt(Meet_ID6_ID2,637)", "HoldsAt(Meet_ID6_ID2,636)", "HoldsAt(Meet_ID6_ID2,635)", "HoldsAt(Meet_ID6_ID2,634)", "HoldsAt(Meet_ID6_ID2,633)", "HoldsAt(Meet_ID6_ID2,632)", "HoldsAt(Meet_ID6_ID2,631)", "HoldsAt(Meet_ID6_ID2,630)", "HoldsAt(Meet_ID6_ID2,629)", "HoldsAt(Meet_ID6_ID2,628)", "HoldsAt(Meet_ID6_ID2,627)", "HoldsAt(Meet_ID6_ID2,626)", "HoldsAt(Meet_ID6_ID2,625)", "HoldsAt(Meet_ID6_ID2,624)", "HoldsAt(Meet_ID6_ID2,623)", "HoldsAt(Meet_ID6_ID2,622)", "HoldsAt(Meet_ID6_ID2,621)", "HoldsAt(Meet_ID6_ID2,620)", "HoldsAt(Meet_ID6_ID2,619)", "HoldsAt(Meet_ID6_ID2,618)", "HoldsAt(Meet_ID6_ID2,617)", "HoldsAt(Meet_ID6_ID2,616)", "HoldsAt(Meet_ID6_ID2,615)", "HoldsAt(Meet_ID6_ID2,614)", "HoldsAt(Meet_ID6_ID2,613)", "HoldsAt(Meet_ID6_ID2,612)", "HoldsAt(Meet_ID6_ID2,611)", "HoldsAt(Meet_ID6_ID2,610)", "HoldsAt(Meet_ID6_ID2,609)", "HoldsAt(Meet_ID6_ID2,608)", "HoldsAt(Meet_ID6_ID2,607)", "HoldsAt(Meet_ID6_ID2,606)", "HoldsAt(Meet_ID6_ID2,605)", "HoldsAt(Meet_ID6_ID2,334)", "HoldsAt(Meet_ID6_ID2,333)", "HoldsAt(Meet_ID6_ID2,332)", "HoldsAt(Meet_ID6_ID2,331)", "HoldsAt(Meet_ID6_ID2,330)", "HoldsAt(Meet_ID6_ID2,329)", "HoldsAt(Meet_ID6_ID2,328)", "HoldsAt(Meet_ID6_ID2,327)", "HoldsAt(Meet_ID6_ID2,326)", "HoldsAt(Meet_ID6_ID2,325)", "HoldsAt(Meet_ID6_ID2,324)", "HoldsAt(Meet_ID6_ID2,323)", "HoldsAt(Meet_ID6_ID2,322)", "HoldsAt(Meet_ID6_ID2,321)", "HoldsAt(Meet_ID6_ID2,320)", "HoldsAt(Meet_ID6_ID2,319)", "HoldsAt(Meet_ID6_ID2,318)", "HoldsAt(Meet_ID6_ID2,317)", "HoldsAt(Meet_ID6_ID2,316)", "HoldsAt(Meet_ID6_ID2,315)", "HoldsAt(Meet_ID6_ID2,314)", "HoldsAt(Meet_ID6_ID2,313)", "HoldsAt(Meet_ID6_ID2,312)", "HoldsAt(Meet_ID6_ID2,311)", "HoldsAt(Meet_ID6_ID2,310)", "HoldsAt(Meet_ID6_ID2,309)", "HoldsAt(Meet_ID6_ID2,308)", "HoldsAt(Meet_ID6_ID2,307)", "HoldsAt(Meet_ID6_ID2,306)", "HoldsAt(Meet_ID6_ID2,305)", "HoldsAt(Meet_ID6_ID2,304)", "HoldsAt(Meet_ID6_ID2,303)", "HoldsAt(Meet_ID6_ID2,302)", "HoldsAt(Meet_ID6_ID2,301)", "HoldsAt(Meet_ID6_ID2,300)", "HoldsAt(Meet_ID6_ID2,299)", "HoldsAt(Meet_ID6_ID2,298)", "HoldsAt(Meet_ID6_ID2,297)", "HoldsAt(Meet_ID6_ID2,296)", "HoldsAt(Meet_ID6_ID2,295)", "HoldsAt(Meet_ID6_ID2,294)", "HoldsAt(Meet_ID6_ID2,293)", "HoldsAt(Meet_ID6_ID2,292)", "HoldsAt(Meet_ID6_ID2,291)", "HoldsAt(Meet_ID6_ID2,290)", "HoldsAt(Meet_ID6_ID2,289)", "HoldsAt(Meet_ID6_ID2,288)", "HoldsAt(Meet_ID6_ID2,287)", "HoldsAt(Meet_ID6_ID2,286)", "HoldsAt(Meet_ID6_ID2,285)", "HoldsAt(Meet_ID6_ID2,284)", "HoldsAt(Meet_ID6_ID2,283)", "HoldsAt(Meet_ID6_ID2,282)", "HoldsAt(Meet_ID6_ID2,281)", "HoldsAt(Meet_ID6_ID2,280)", "HoldsAt(Meet_ID6_ID2,279)", "HoldsAt(Meet_ID6_ID2,278)", "HoldsAt(Meet_ID6_ID2,277)", "HoldsAt(Meet_ID6_ID2,276)", "HoldsAt(Meet_ID6_ID2,275)", "HoldsAt(Meet_ID6_ID2,274)", "HoldsAt(Meet_ID6_ID2,273)", "HoldsAt(Meet_ID6_ID2,272)", "HoldsAt(Meet_ID6_ID2,271)", "HoldsAt(Meet_ID6_ID2,270)", "HoldsAt(Meet_ID6_ID2,269)", "HoldsAt(Meet_ID6_ID2,268)", "HoldsAt(Meet_ID6_ID2,267)", "HoldsAt(Meet_ID6_ID2,266)", "HoldsAt(Meet_ID6_ID2,265)", "HoldsAt(Meet_ID6_ID2,264)", "HoldsAt(Meet_ID6_ID2,263)", "HoldsAt(Meet_ID6_ID2,262)", "HoldsAt(Meet_ID6_ID2,261)", "HoldsAt(Meet_ID6_ID2,260)", "HoldsAt(Meet_ID6_ID2,259)", "HoldsAt(Meet_ID6_ID2,258)", "HoldsAt(Meet_ID6_ID2,257)", "HoldsAt(Meet_ID6_ID2,256)", "HoldsAt(Meet_ID6_ID2,255)", "HoldsAt(Meet_ID6_ID2,254)", "HoldsAt(Meet_ID6_ID2,253)", "HoldsAt(Meet_ID6_ID2,252)", "HoldsAt(Meet_ID6_ID2,251)", "HoldsAt(Meet_ID6_ID2,250)", "HoldsAt(Meet_ID6_ID2,249)", "HoldsAt(Meet_ID6_ID2,248)", "HoldsAt(Meet_ID6_ID2,247)", "HoldsAt(Meet_ID6_ID2,246)", "HoldsAt(Meet_ID6_ID2,245)", "HoldsAt(Meet_ID6_ID2,244)", "HoldsAt(Meet_ID6_ID2,243)", "HoldsAt(Meet_ID6_ID2,242)", "HoldsAt(Meet_ID6_ID2,241)", "HoldsAt(Meet_ID6_ID2,240)", "HoldsAt(Meet_ID6_ID2,239)", "HoldsAt(Meet_ID6_ID2,238)", "HoldsAt(Meet_ID6_ID2,237)", "HoldsAt(Meet_ID6_ID2,236)", "HoldsAt(Meet_ID6_ID2,235)", "HoldsAt(Meet_ID6_ID2,234)", "HoldsAt(Meet_ID6_ID2,233)", "HoldsAt(Meet_ID6_ID2,232)", "HoldsAt(Meet_ID6_ID2,231)", "HoldsAt(Meet_ID6_ID2,230)", "HoldsAt(Meet_ID6_ID2,229)", "HoldsAt(Meet_ID6_ID2,228)", "HoldsAt(Meet_ID6_ID2,227)", "HoldsAt(Meet_ID6_ID2,226)", "HoldsAt(Meet_ID6_ID2,225)", "HoldsAt(Meet_ID6_ID2,224)", "HoldsAt(Meet_ID6_ID2,223)", "HoldsAt(Meet_ID6_ID2,222)", "HoldsAt(Meet_ID6_ID2,221)", "HoldsAt(Meet_ID6_ID2,220)", "HoldsAt(Meet_ID6_ID2,219)", "HoldsAt(Meet_ID6_ID2,218)", "HoldsAt(Meet_ID6_ID2,217)", "HoldsAt(Meet_ID6_ID2,216)", "HoldsAt(Meet_ID6_ID2,215)", "HoldsAt(Meet_ID6_ID2,214)", "HoldsAt(Meet_ID6_ID2,213)", "HoldsAt(Meet_ID6_ID2,212)", "HoldsAt(Meet_ID6_ID2,211)", "HoldsAt(Meet_ID6_ID2,210)", "HoldsAt(Meet_ID6_ID2,209)", "HoldsAt(Meet_ID6_ID2,208)", "HoldsAt(Meet_ID6_ID2,207)", "HoldsAt(Meet_ID6_ID2,206)", "HoldsAt(Meet_ID6_ID2,205)", "HoldsAt(Meet_ID6_ID2,204)", "HoldsAt(Meet_ID6_ID2,203)", "HoldsAt(Meet_ID6_ID2,202)", "HoldsAt(Meet_ID6_ID2,201)", "HoldsAt(Meet_ID6_ID2,200)", "HoldsAt(Meet_ID6_ID2,199)", "HoldsAt(Meet_ID6_ID2,198)", "HoldsAt(Meet_ID6_ID2,197)", "HoldsAt(Meet_ID6_ID2,196)", "HoldsAt(Meet_ID6_ID2,195)", "HoldsAt(Meet_ID6_ID2,194)", "HoldsAt(Meet_ID6_ID2,193)", "HoldsAt(Meet_ID6_ID2,192)", "HoldsAt(Meet_ID6_ID2,191)", "HoldsAt(Meet_ID6_ID2,190)", "HoldsAt(Meet_ID6_ID2,189)", "HoldsAt(Meet_ID6_ID2,188)", "HoldsAt(Meet_ID6_ID2,187)", "HoldsAt(Meet_ID6_ID2,186)", "HoldsAt(Meet_ID6_ID2,185)", "HoldsAt(Meet_ID6_ID2,184)", "HoldsAt(Meet_ID6_ID2,183)", "HoldsAt(Meet_ID6_ID2,182)", "HoldsAt(Meet_ID6_ID2,181)", "HoldsAt(Meet_ID6_ID2,180)", "HoldsAt(Meet_ID6_ID2,179)", "HoldsAt(Meet_ID6_ID2,178)", "HoldsAt(Meet_ID6_ID2,177)", "HoldsAt(Meet_ID6_ID2,176)", "HoldsAt(Meet_ID6_ID2,175)", "HoldsAt(Meet_ID6_ID2,174)", "HoldsAt(Meet_ID6_ID2,173)", "HoldsAt(Meet_ID6_ID2,172)", "HoldsAt(Meet_ID6_ID2,171)", "HoldsAt(Meet_ID6_ID2,170)", "HoldsAt(Meet_ID6_ID2,169)", "HoldsAt(Meet_ID6_ID2,168)", "HoldsAt(Meet_ID6_ID2,167)", "HoldsAt(Meet_ID6_ID2,166)", "HoldsAt(Meet_ID6_ID2,165)", "HoldsAt(Meet_ID6_ID2,164)", "HoldsAt(Meet_ID6_ID2,163)", "HoldsAt(Meet_ID6_ID2,162)", "HoldsAt(Meet_ID6_ID2,161)", "HoldsAt(Meet_ID6_ID2,160)", "HoldsAt(Meet_ID6_ID2,159)", "HoldsAt(Meet_ID6_ID2,158)", "HoldsAt(Meet_ID6_ID2,157)", "HoldsAt(Meet_ID6_ID2,156)", "HoldsAt(Meet_ID6_ID2,155)", "HoldsAt(Meet_ID6_ID2,154)", "HoldsAt(Meet_ID6_ID2,153)", "HoldsAt(Meet_ID6_ID2,152)", "HoldsAt(Meet_ID6_ID2,151)", "HoldsAt(Meet_ID6_ID2,150)", "HoldsAt(Meet_ID6_ID2,149)", "HoldsAt(Meet_ID6_ID2,148)", "HoldsAt(Meet_ID6_ID2,147)", "HoldsAt(Meet_ID6_ID2,146)", "HoldsAt(Meet_ID6_ID2,145)", "HoldsAt(Meet_ID6_ID2,144)", "HoldsAt(Meet_ID6_ID2,143)", "HoldsAt(Meet_ID6_ID2,142)", "HoldsAt(Meet_ID6_ID2,141)", "HoldsAt(Meet_ID6_ID2,140)", "HoldsAt(Meet_ID6_ID2,139)", "HoldsAt(Meet_ID6_ID2,138)", "HoldsAt(Meet_ID6_ID2,137)", "HoldsAt(Meet_ID6_ID2,136)", "HoldsAt(Meet_ID6_ID2,135)", "HoldsAt(Meet_ID6_ID2,134)", "HoldsAt(Meet_ID6_ID2,133)", "HoldsAt(Meet_ID6_ID2,132)", "HoldsAt(Meet_ID6_ID2,131)", "HoldsAt(Meet_ID6_ID2,130)", "HoldsAt(Meet_ID6_ID2,129)", "HoldsAt(Meet_ID6_ID2,128)", "HoldsAt(Meet_ID6_ID2,127)", "HoldsAt(Meet_ID6_ID2,126)", "HoldsAt(Meet_ID6_ID2,125)", "HoldsAt(Meet_ID6_ID2,124)", "HoldsAt(Meet_ID6_ID2,123)", "HoldsAt(Meet_ID6_ID2,122)", "HoldsAt(Meet_ID6_ID2,121)", "HoldsAt(Meet_ID6_ID2,120)", "HoldsAt(Meet_ID6_ID2,119)", "HoldsAt(Meet_ID6_ID2,118)", "HoldsAt(Meet_ID6_ID2,117)", "HoldsAt(Meet_ID6_ID2,116)", "HoldsAt(Meet_ID6_ID2,115)", "HoldsAt(Meet_ID6_ID2,114)", "HoldsAt(Meet_ID6_ID2,113)", "HoldsAt(Meet_ID6_ID2,112)", "HoldsAt(Meet_ID6_ID2,111)", "HoldsAt(Meet_ID6_ID2,110)", "HoldsAt(Meet_ID6_ID2,109)", "HoldsAt(Meet_ID6_ID2,108)", "HoldsAt(Meet_ID6_ID2,107)", "HoldsAt(Meet_ID6_ID2,106)", "HoldsAt(Meet_ID6_ID2,105)", "HoldsAt(Meet_ID6_ID2,104)", "HoldsAt(Meet_ID6_ID2,103)", "HoldsAt(Meet_ID6_ID2,102)", "HoldsAt(Meet_ID6_ID2,101)", "HoldsAt(Meet_ID6_ID2,100)", "HoldsAt(Meet_ID6_ID2,99)", "HoldsAt(Meet_ID6_ID2,98)", "HoldsAt(Meet_ID6_ID2,97)", "HoldsAt(Meet_ID6_ID2,96)", "HoldsAt(Meet_ID6_ID2,95)", "HoldsAt(Meet_ID6_ID2,94)", "HoldsAt(Meet_ID6_ID2,93)", "HoldsAt(Meet_ID6_ID2,92)", "HoldsAt(Meet_ID6_ID2,91)", "HoldsAt(Meet_ID6_ID2,90)", "HoldsAt(Meet_ID6_ID2,89)", "HoldsAt(Meet_ID6_ID2,88)", "HoldsAt(Meet_ID6_ID2,87)", "HoldsAt(Meet_ID6_ID2,86)", "HoldsAt(Meet_ID6_ID2,85)", "HoldsAt(Meet_ID6_ID2,84)", "HoldsAt(Meet_ID6_ID2,83)", "HoldsAt(Meet_ID6_ID2,82)", "HoldsAt(Meet_ID6_ID2,81)", "HoldsAt(Meet_ID6_ID2,80)", "HoldsAt(Meet_ID6_ID2,79)", "HoldsAt(Meet_ID6_ID2,78)", "HoldsAt(Meet_ID6_ID2,77)", "HoldsAt(Meet_ID6_ID2,76)", "HoldsAt(Meet_ID6_ID2,75)", "HoldsAt(Meet_ID6_ID2,74)", "HoldsAt(Meet_ID6_ID2,73)", "HoldsAt(Meet_ID6_ID2,72)", "HoldsAt(Meet_ID6_ID2,71)", "HoldsAt(Meet_ID6_ID2,70)", "HoldsAt(Meet_ID6_ID2,69)", "HoldsAt(Meet_ID6_ID2,68)", "HoldsAt(Meet_ID6_ID2,67)", "HoldsAt(Meet_ID6_ID2,66)", "HoldsAt(Meet_ID6_ID2,65)", "HoldsAt(Meet_ID6_ID2,64)", "HoldsAt(Meet_ID6_ID2,63)", "HoldsAt(Meet_ID6_ID2,62)", "HoldsAt(Meet_ID6_ID2,61)", "HoldsAt(Meet_ID6_ID2,60)", "HoldsAt(Meet_ID6_ID2,59)", "HoldsAt(Meet_ID6_ID2,58)", "HoldsAt(Meet_ID6_ID2,57)", "HoldsAt(Meet_ID6_ID2,56)", "HoldsAt(Meet_ID6_ID2,55)", "HoldsAt(Meet_ID6_ID2,54)", "HoldsAt(Meet_ID6_ID2,53)", "HoldsAt(Meet_ID6_ID2,52)", "HoldsAt(Meet_ID6_ID2,51)", "HoldsAt(Meet_ID6_ID2,50)", "HoldsAt(Meet_ID6_ID2,49)", "HoldsAt(Meet_ID6_ID2,48)", "HoldsAt(Meet_ID6_ID2,47)", "HoldsAt(Meet_ID6_ID2,46)", "HoldsAt(Meet_ID6_ID2,45)", "HoldsAt(Meet_ID6_ID2,44)", "HoldsAt(Meet_ID6_ID2,43)", "HoldsAt(Meet_ID6_ID2,42)", "HoldsAt(Meet_ID6_ID2,41)", "HoldsAt(Meet_ID6_ID2,40)", "HoldsAt(Meet_ID6_ID2,39)", "HoldsAt(Meet_ID6_ID2,38)", "HoldsAt(Meet_ID6_ID2,37)", "HoldsAt(Meet_ID6_ID2,36)", "HoldsAt(Meet_ID6_ID2,35)", "HoldsAt(Meet_ID6_ID2,34)", "HoldsAt(Meet_ID6_ID2,33)", "HoldsAt(Meet_ID6_ID2,32)", "HoldsAt(Meet_ID6_ID2,31)", "HoldsAt(Meet_ID6_ID2,30)", "HoldsAt(Meet_ID6_ID2,29)", "HoldsAt(Meet_ID6_ID2,28)", "HoldsAt(Meet_ID6_ID2,27)", "HoldsAt(Meet_ID6_ID2,26)", "HoldsAt(Meet_ID6_ID2,25)", "HoldsAt(Meet_ID6_ID2,24)", "HoldsAt(Meet_ID6_ID2,23)", "HoldsAt(Meet_ID6_ID2,22)", "HoldsAt(Meet_ID6_ID2,21)", "HoldsAt(Meet_ID6_ID2,20)", "HoldsAt(Meet_ID6_ID2,19)", "HoldsAt(Meet_ID6_ID2,18)", "HoldsAt(Meet_ID6_ID2,17)", "HoldsAt(Meet_ID6_ID2,16)", "HoldsAt(Meet_ID6_ID2,15)", "HoldsAt(Meet_ID6_ID2,14)", "HoldsAt(Meet_ID6_ID2,13)", "HoldsAt(Meet_ID6_ID2,12)", "HoldsAt(Meet_ID6_ID2,11)", "HoldsAt(Meet_ID6_ID2,10)", "HoldsAt(Meet_ID6_ID2,9)", "HoldsAt(Meet_ID6_ID2,8)", "HoldsAt(Meet_ID6_ID2,7)", "HoldsAt(Meet_ID6_ID2,6)", "HoldsAt(Meet_ID6_ID2,5)", "HoldsAt(Meet_ID6_ID2,4)", "HoldsAt(Meet_ID6_ID2,3)", "HoldsAt(Meet_ID6_ID2,2)", "HoldsAt(Meet_ID6_ID2,1)", "HoldsAt(Meet_ID2_ID6,957)", "HoldsAt(Meet_ID2_ID6,956)", "HoldsAt(Meet_ID2_ID6,955)", "HoldsAt(Meet_ID2_ID6,954)", "HoldsAt(Meet_ID2_ID6,953)", "HoldsAt(Meet_ID2_ID6,952)", "HoldsAt(Meet_ID2_ID6,951)", "HoldsAt(Meet_ID2_ID6,950)", "HoldsAt(Meet_ID2_ID6,949)", "HoldsAt(Meet_ID2_ID6,948)", "HoldsAt(Meet_ID2_ID6,947)", "HoldsAt(Meet_ID2_ID6,946)", "HoldsAt(Meet_ID2_ID6,945)", "HoldsAt(Meet_ID2_ID6,944)", "HoldsAt(Meet_ID2_ID6,943)", "HoldsAt(Meet_ID2_ID6,942)", "HoldsAt(Meet_ID2_ID6,941)", "HoldsAt(Meet_ID2_ID6,940)", "HoldsAt(Meet_ID2_ID6,939)", "HoldsAt(Meet_ID2_ID6,938)", "HoldsAt(Meet_ID2_ID6,937)", "HoldsAt(Meet_ID2_ID6,936)", "HoldsAt(Meet_ID2_ID6,935)", "HoldsAt(Meet_ID2_ID6,934)", "HoldsAt(Meet_ID2_ID6,933)", "HoldsAt(Meet_ID2_ID6,932)", "HoldsAt(Meet_ID2_ID6,931)", "HoldsAt(Meet_ID2_ID6,930)", "HoldsAt(Meet_ID2_ID6,929)", "HoldsAt(Meet_ID2_ID6,928)", "HoldsAt(Meet_ID2_ID6,927)", "HoldsAt(Meet_ID2_ID6,926)", "HoldsAt(Meet_ID2_ID6,925)", "HoldsAt(Meet_ID2_ID6,924)", "HoldsAt(Meet_ID2_ID6,923)", "HoldsAt(Meet_ID2_ID6,922)", "HoldsAt(Meet_ID2_ID6,921)", "HoldsAt(Meet_ID2_ID6,920)", "HoldsAt(Meet_ID2_ID6,919)", "HoldsAt(Meet_ID2_ID6,918)", "HoldsAt(Meet_ID2_ID6,917)", "HoldsAt(Meet_ID2_ID6,916)", "HoldsAt(Meet_ID2_ID6,915)", "HoldsAt(Meet_ID2_ID6,914)", "HoldsAt(Meet_ID2_ID6,913)", "HoldsAt(Meet_ID2_ID6,912)", "HoldsAt(Meet_ID2_ID6,911)", "HoldsAt(Meet_ID2_ID6,910)", "HoldsAt(Meet_ID2_ID6,909)", "HoldsAt(Meet_ID2_ID6,908)", "HoldsAt(Meet_ID2_ID6,907)", "HoldsAt(Meet_ID2_ID6,906)", "HoldsAt(Meet_ID2_ID6,905)", "HoldsAt(Meet_ID2_ID6,904)", "HoldsAt(Meet_ID2_ID6,903)", "HoldsAt(Meet_ID2_ID6,902)", "HoldsAt(Meet_ID2_ID6,901)", "HoldsAt(Meet_ID2_ID6,900)", "HoldsAt(Meet_ID2_ID6,899)", "HoldsAt(Meet_ID2_ID6,898)", "HoldsAt(Meet_ID2_ID6,897)", "HoldsAt(Meet_ID2_ID6,896)", "HoldsAt(Meet_ID2_ID6,895)", "HoldsAt(Meet_ID2_ID6,894)", "HoldsAt(Meet_ID2_ID6,893)", "HoldsAt(Meet_ID2_ID6,892)", "HoldsAt(Meet_ID2_ID6,891)", "HoldsAt(Meet_ID2_ID6,890)", "HoldsAt(Meet_ID2_ID6,889)", "HoldsAt(Meet_ID2_ID6,888)", "HoldsAt(Meet_ID2_ID6,887)", "HoldsAt(Meet_ID2_ID6,886)", "HoldsAt(Meet_ID2_ID6,885)", "HoldsAt(Meet_ID2_ID6,884)", "HoldsAt(Meet_ID2_ID6,883)", "HoldsAt(Meet_ID2_ID6,882)", "HoldsAt(Meet_ID2_ID6,881)", "HoldsAt(Meet_ID2_ID6,880)", "HoldsAt(Meet_ID2_ID6,879)", "HoldsAt(Meet_ID2_ID6,878)", "HoldsAt(Meet_ID2_ID6,877)", "HoldsAt(Meet_ID2_ID6,876)", "HoldsAt(Meet_ID2_ID6,875)", "HoldsAt(Meet_ID2_ID6,874)", "HoldsAt(Meet_ID2_ID6,873)", "HoldsAt(Meet_ID2_ID6,872)", "HoldsAt(Meet_ID2_ID6,871)", "HoldsAt(Meet_ID2_ID6,870)", "HoldsAt(Meet_ID2_ID6,869)", "HoldsAt(Meet_ID2_ID6,868)", "HoldsAt(Meet_ID2_ID6,867)", "HoldsAt(Meet_ID2_ID6,866)", "HoldsAt(Meet_ID2_ID6,865)", "HoldsAt(Meet_ID2_ID6,864)", "HoldsAt(Meet_ID2_ID6,863)", "HoldsAt(Meet_ID2_ID6,862)", "HoldsAt(Meet_ID2_ID6,861)", "HoldsAt(Meet_ID2_ID6,860)", "HoldsAt(Meet_ID2_ID6,859)", "HoldsAt(Meet_ID2_ID6,858)", "HoldsAt(Meet_ID2_ID6,857)", "HoldsAt(Meet_ID2_ID6,856)", "HoldsAt(Meet_ID2_ID6,855)", "HoldsAt(Meet_ID2_ID6,854)", "HoldsAt(Meet_ID2_ID6,853)", "HoldsAt(Meet_ID2_ID6,852)", "HoldsAt(Meet_ID2_ID6,851)", "HoldsAt(Meet_ID2_ID6,850)", "HoldsAt(Meet_ID2_ID6,849)", "HoldsAt(Meet_ID2_ID6,848)", "HoldsAt(Meet_ID2_ID6,847)", "HoldsAt(Meet_ID2_ID6,846)", "HoldsAt(Meet_ID2_ID6,845)", "HoldsAt(Meet_ID2_ID6,844)", "HoldsAt(Meet_ID2_ID6,843)", "HoldsAt(Meet_ID2_ID6,842)", "HoldsAt(Meet_ID2_ID6,841)", "HoldsAt(Meet_ID2_ID6,840)", "HoldsAt(Meet_ID2_ID6,839)", "HoldsAt(Meet_ID2_ID6,838)", "HoldsAt(Meet_ID2_ID6,837)", "HoldsAt(Meet_ID2_ID6,836)", "HoldsAt(Meet_ID2_ID6,835)", "HoldsAt(Meet_ID2_ID6,834)", "HoldsAt(Meet_ID2_ID6,833)", "HoldsAt(Meet_ID2_ID6,832)", "HoldsAt(Meet_ID2_ID6,831)", "HoldsAt(Meet_ID2_ID6,830)", "HoldsAt(Meet_ID2_ID6,829)", "HoldsAt(Meet_ID2_ID6,828)", "HoldsAt(Meet_ID2_ID6,827)", "HoldsAt(Meet_ID2_ID6,826)", "HoldsAt(Meet_ID2_ID6,825)", "HoldsAt(Meet_ID2_ID6,824)", "HoldsAt(Meet_ID2_ID6,823)", "HoldsAt(Meet_ID2_ID6,822)", "HoldsAt(Meet_ID2_ID6,821)", "HoldsAt(Meet_ID2_ID6,820)", "HoldsAt(Meet_ID2_ID6,819)", "HoldsAt(Meet_ID2_ID6,818)", "HoldsAt(Meet_ID2_ID6,817)", "HoldsAt(Meet_ID2_ID6,816)", "HoldsAt(Meet_ID2_ID6,815)", "HoldsAt(Meet_ID2_ID6,814)", "HoldsAt(Meet_ID2_ID6,813)", "HoldsAt(Meet_ID2_ID6,812)", "HoldsAt(Meet_ID2_ID6,811)", "HoldsAt(Meet_ID2_ID6,810)", "HoldsAt(Meet_ID2_ID6,809)", "HoldsAt(Meet_ID2_ID6,808)", "HoldsAt(Meet_ID2_ID6,807)", "HoldsAt(Meet_ID2_ID6,806)", "HoldsAt(Meet_ID2_ID6,805)", "HoldsAt(Meet_ID2_ID6,804)", "HoldsAt(Meet_ID2_ID6,803)", "HoldsAt(Meet_ID2_ID6,802)", "HoldsAt(Meet_ID2_ID6,801)", "HoldsAt(Meet_ID2_ID6,800)", "HoldsAt(Meet_ID2_ID6,799)", "HoldsAt(Meet_ID2_ID6,798)", "HoldsAt(Meet_ID2_ID6,797)", "HoldsAt(Meet_ID2_ID6,796)", "HoldsAt(Meet_ID2_ID6,795)", "HoldsAt(Meet_ID2_ID6,794)", "HoldsAt(Meet_ID2_ID6,793)", "HoldsAt(Meet_ID2_ID6,792)", "HoldsAt(Meet_ID2_ID6,791)", "HoldsAt(Meet_ID2_ID6,790)", "HoldsAt(Meet_ID2_ID6,789)", "HoldsAt(Meet_ID2_ID6,788)", "HoldsAt(Meet_ID2_ID6,787)", "HoldsAt(Meet_ID2_ID6,786)", "HoldsAt(Meet_ID2_ID6,785)", "HoldsAt(Meet_ID2_ID6,784)", "HoldsAt(Meet_ID2_ID6,783)", "HoldsAt(Meet_ID2_ID6,782)", "HoldsAt(Meet_ID2_ID6,781)", "HoldsAt(Meet_ID2_ID6,780)", "HoldsAt(Meet_ID2_ID6,779)", "HoldsAt(Meet_ID2_ID6,778)", "HoldsAt(Meet_ID2_ID6,777)", "HoldsAt(Meet_ID2_ID6,776)", "HoldsAt(Meet_ID2_ID6,775)", "HoldsAt(Meet_ID2_ID6,774)", "HoldsAt(Meet_ID2_ID6,773)", "HoldsAt(Meet_ID2_ID6,772)", "HoldsAt(Meet_ID2_ID6,771)", "HoldsAt(Meet_ID2_ID6,770)", "HoldsAt(Meet_ID2_ID6,769)", "HoldsAt(Meet_ID2_ID6,768)", "HoldsAt(Meet_ID2_ID6,767)", "HoldsAt(Meet_ID2_ID6,766)", "HoldsAt(Meet_ID2_ID6,765)", "HoldsAt(Meet_ID2_ID6,764)", "HoldsAt(Meet_ID2_ID6,763)", "HoldsAt(Meet_ID2_ID6,762)", "HoldsAt(Meet_ID2_ID6,761)", "HoldsAt(Meet_ID2_ID6,760)", "HoldsAt(Meet_ID2_ID6,759)", "HoldsAt(Meet_ID2_ID6,758)", "HoldsAt(Meet_ID2_ID6,757)", "HoldsAt(Meet_ID2_ID6,756)", "HoldsAt(Meet_ID2_ID6,755)", "HoldsAt(Meet_ID2_ID6,754)", "HoldsAt(Meet_ID2_ID6,753)", "HoldsAt(Meet_ID2_ID6,752)", "HoldsAt(Meet_ID2_ID6,751)", "HoldsAt(Meet_ID2_ID6,750)", "HoldsAt(Meet_ID2_ID6,749)", "HoldsAt(Meet_ID2_ID6,748)", "HoldsAt(Meet_ID2_ID6,747)", "HoldsAt(Meet_ID2_ID6,746)", "HoldsAt(Meet_ID2_ID6,745)", "HoldsAt(Meet_ID2_ID6,744)", "HoldsAt(Meet_ID2_ID6,743)", "HoldsAt(Meet_ID2_ID6,742)", "HoldsAt(Meet_ID2_ID6,741)", "HoldsAt(Meet_ID2_ID6,740)", "HoldsAt(Meet_ID2_ID6,739)", "HoldsAt(Meet_ID2_ID6,738)", "HoldsAt(Meet_ID2_ID6,737)", "HoldsAt(Meet_ID2_ID6,736)", "HoldsAt(Meet_ID2_ID6,735)", "HoldsAt(Meet_ID2_ID6,734)", "HoldsAt(Meet_ID2_ID6,733)", "HoldsAt(Meet_ID2_ID6,732)", "HoldsAt(Meet_ID2_ID6,731)", "HoldsAt(Meet_ID2_ID6,730)", "HoldsAt(Meet_ID2_ID6,729)", "HoldsAt(Meet_ID2_ID6,728)", "HoldsAt(Meet_ID2_ID6,727)", "HoldsAt(Meet_ID2_ID6,726)", "HoldsAt(Meet_ID2_ID6,725)", "HoldsAt(Meet_ID2_ID6,724)", "HoldsAt(Meet_ID2_ID6,723)", "HoldsAt(Meet_ID2_ID6,722)", "HoldsAt(Meet_ID2_ID6,721)", "HoldsAt(Meet_ID2_ID6,720)", "HoldsAt(Meet_ID2_ID6,719)", "HoldsAt(Meet_ID2_ID6,718)", "HoldsAt(Meet_ID2_ID6,717)", "HoldsAt(Meet_ID2_ID6,716)", "HoldsAt(Meet_ID2_ID6,715)", "HoldsAt(Meet_ID2_ID6,714)", "HoldsAt(Meet_ID2_ID6,713)", "HoldsAt(Meet_ID2_ID6,712)", "HoldsAt(Meet_ID2_ID6,711)", "HoldsAt(Meet_ID2_ID6,710)", "HoldsAt(Meet_ID2_ID6,709)", "HoldsAt(Meet_ID2_ID6,708)", "HoldsAt(Meet_ID2_ID6,707)", "HoldsAt(Meet_ID2_ID6,706)", "HoldsAt(Meet_ID2_ID6,705)", "HoldsAt(Meet_ID2_ID6,704)", "HoldsAt(Meet_ID2_ID6,703)", "HoldsAt(Meet_ID2_ID6,702)", "HoldsAt(Meet_ID2_ID6,701)", "HoldsAt(Meet_ID2_ID6,700)", "HoldsAt(Meet_ID2_ID6,699)", "HoldsAt(Meet_ID2_ID6,698)", "HoldsAt(Meet_ID2_ID6,697)", "HoldsAt(Meet_ID2_ID6,696)", "HoldsAt(Meet_ID2_ID6,695)", "HoldsAt(Meet_ID2_ID6,694)", "HoldsAt(Meet_ID2_ID6,693)", "HoldsAt(Meet_ID2_ID6,692)", "HoldsAt(Meet_ID2_ID6,691)", "HoldsAt(Meet_ID2_ID6,690)", "HoldsAt(Meet_ID2_ID6,689)", "HoldsAt(Meet_ID2_ID6,688)", "HoldsAt(Meet_ID2_ID6,687)", "HoldsAt(Meet_ID2_ID6,686)", "HoldsAt(Meet_ID2_ID6,685)", "HoldsAt(Meet_ID2_ID6,684)", "HoldsAt(Meet_ID2_ID6,683)", "HoldsAt(Meet_ID2_ID6,682)", "HoldsAt(Meet_ID2_ID6,681)", "HoldsAt(Meet_ID2_ID6,680)", "HoldsAt(Meet_ID2_ID6,679)", "HoldsAt(Meet_ID2_ID6,678)", "HoldsAt(Meet_ID2_ID6,677)", "HoldsAt(Meet_ID2_ID6,676)", "HoldsAt(Meet_ID2_ID6,675)", "HoldsAt(Meet_ID2_ID6,674)", "HoldsAt(Meet_ID2_ID6,673)", "HoldsAt(Meet_ID2_ID6,672)", "HoldsAt(Meet_ID2_ID6,671)", "HoldsAt(Meet_ID2_ID6,670)", "HoldsAt(Meet_ID2_ID6,669)", "HoldsAt(Meet_ID2_ID6,668)", "HoldsAt(Meet_ID2_ID6,667)", "HoldsAt(Meet_ID2_ID6,666)", "HoldsAt(Meet_ID2_ID6,665)", "HoldsAt(Meet_ID2_ID6,664)", "HoldsAt(Meet_ID2_ID6,663)", "HoldsAt(Meet_ID2_ID6,662)", "HoldsAt(Meet_ID2_ID6,661)", "HoldsAt(Meet_ID2_ID6,660)", "HoldsAt(Meet_ID2_ID6,659)", "HoldsAt(Meet_ID2_ID6,658)", "HoldsAt(Meet_ID2_ID6,657)", "HoldsAt(Meet_ID2_ID6,656)", "HoldsAt(Meet_ID2_ID6,655)", "HoldsAt(Meet_ID2_ID6,654)", "HoldsAt(Meet_ID2_ID6,653)", "HoldsAt(Meet_ID2_ID6,652)", "HoldsAt(Meet_ID2_ID6,651)", "HoldsAt(Meet_ID2_ID6,650)", "HoldsAt(Meet_ID2_ID6,649)", "HoldsAt(Meet_ID2_ID6,648)", "HoldsAt(Meet_ID2_ID6,647)", "HoldsAt(Meet_ID2_ID6,646)", "HoldsAt(Meet_ID2_ID6,645)", "HoldsAt(Meet_ID2_ID6,644)", "HoldsAt(Meet_ID2_ID6,643)", "HoldsAt(Meet_ID2_ID6,642)", "HoldsAt(Meet_ID2_ID6,641)", "HoldsAt(Meet_ID2_ID6,640)", "HoldsAt(Meet_ID2_ID6,639)", "HoldsAt(Meet_ID2_ID6,638)", "HoldsAt(Meet_ID2_ID6,637)", "HoldsAt(Meet_ID2_ID6,636)", "HoldsAt(Meet_ID2_ID6,635)", "HoldsAt(Meet_ID2_ID6,634)", "HoldsAt(Meet_ID2_ID6,633)", "HoldsAt(Meet_ID2_ID6,632)", "HoldsAt(Meet_ID2_ID6,631)", "HoldsAt(Meet_ID2_ID6,630)", "HoldsAt(Meet_ID2_ID6,629)", "HoldsAt(Meet_ID2_ID6,628)", "HoldsAt(Meet_ID2_ID6,627)", "HoldsAt(Meet_ID2_ID6,626)", "HoldsAt(Meet_ID2_ID6,625)", "HoldsAt(Meet_ID2_ID6,624)", "HoldsAt(Meet_ID2_ID6,623)", "HoldsAt(Meet_ID2_ID6,622)", "HoldsAt(Meet_ID2_ID6,621)", "HoldsAt(Meet_ID2_ID6,620)", "HoldsAt(Meet_ID2_ID6,619)", "HoldsAt(Meet_ID2_ID6,618)", "HoldsAt(Meet_ID2_ID6,617)", "HoldsAt(Meet_ID2_ID6,616)", "HoldsAt(Meet_ID2_ID6,615)", "HoldsAt(Meet_ID2_ID6,614)", "HoldsAt(Meet_ID2_ID6,613)", "HoldsAt(Meet_ID2_ID6,612)", "HoldsAt(Meet_ID2_ID6,611)", "HoldsAt(Meet_ID2_ID6,610)", "HoldsAt(Meet_ID2_ID6,609)", "HoldsAt(Meet_ID2_ID6,608)", "HoldsAt(Meet_ID2_ID6,607)", "HoldsAt(Meet_ID2_ID6,606)", "HoldsAt(Meet_ID2_ID6,605)", "HoldsAt(Meet_ID2_ID6,510)", "HoldsAt(Meet_ID2_ID6,509)", "HoldsAt(Meet_ID2_ID6,508)", "HoldsAt(Meet_ID2_ID6,507)", "HoldsAt(Meet_ID2_ID6,506)", "HoldsAt(Meet_ID2_ID6,505)", "HoldsAt(Meet_ID2_ID6,504)", "HoldsAt(Meet_ID2_ID6,503)", "HoldsAt(Meet_ID2_ID6,502)", "HoldsAt(Meet_ID2_ID6,501)", "HoldsAt(Meet_ID2_ID6,500)", "HoldsAt(Meet_ID2_ID6,499)", "HoldsAt(Meet_ID2_ID6,498)", "HoldsAt(Meet_ID2_ID6,334)", "HoldsAt(Meet_ID2_ID6,333)", "HoldsAt(Meet_ID2_ID6,332)", "HoldsAt(Meet_ID2_ID6,331)", "HoldsAt(Meet_ID2_ID6,330)", "HoldsAt(Meet_ID2_ID6,329)", "HoldsAt(Meet_ID2_ID6,328)", "HoldsAt(Meet_ID2_ID6,327)", "HoldsAt(Meet_ID2_ID6,326)", "HoldsAt(Meet_ID2_ID6,325)", "HoldsAt(Meet_ID2_ID6,324)", "HoldsAt(Meet_ID2_ID6,323)", "HoldsAt(Meet_ID2_ID6,322)", "HoldsAt(Meet_ID2_ID6,321)", "HoldsAt(Meet_ID2_ID6,320)", "HoldsAt(Meet_ID2_ID6,319)", "HoldsAt(Meet_ID2_ID6,318)", "HoldsAt(Meet_ID2_ID6,317)", "HoldsAt(Meet_ID2_ID6,316)", "HoldsAt(Meet_ID2_ID6,315)", "HoldsAt(Meet_ID2_ID6,314)", "HoldsAt(Meet_ID2_ID6,313)", "HoldsAt(Meet_ID2_ID6,312)", "HoldsAt(Meet_ID2_ID6,311)", "HoldsAt(Meet_ID2_ID6,310)", "HoldsAt(Meet_ID2_ID6,309)", "HoldsAt(Meet_ID2_ID6,308)", "HoldsAt(Meet_ID2_ID6,307)", "HoldsAt(Meet_ID2_ID6,306)", "HoldsAt(Meet_ID2_ID6,305)", "HoldsAt(Meet_ID2_ID6,304)", "HoldsAt(Meet_ID2_ID6,303)", "HoldsAt(Meet_ID2_ID6,302)", "HoldsAt(Meet_ID2_ID6,301)", "HoldsAt(Meet_ID2_ID6,300)", "HoldsAt(Meet_ID2_ID6,299)", "HoldsAt(Meet_ID2_ID6,298)", "HoldsAt(Meet_ID2_ID6,297)", "HoldsAt(Meet_ID2_ID6,296)", "HoldsAt(Meet_ID2_ID6,295)", "HoldsAt(Meet_ID2_ID6,294)", "HoldsAt(Meet_ID2_ID6,293)", "HoldsAt(Meet_ID2_ID6,292)", "HoldsAt(Meet_ID2_ID6,291)", "HoldsAt(Meet_ID2_ID6,290)", "HoldsAt(Meet_ID2_ID6,289)", "HoldsAt(Meet_ID2_ID6,288)", "HoldsAt(Meet_ID2_ID6,287)", "HoldsAt(Meet_ID2_ID6,286)", "HoldsAt(Meet_ID2_ID6,285)", "HoldsAt(Meet_ID2_ID6,284)", "HoldsAt(Meet_ID2_ID6,283)", "HoldsAt(Meet_ID2_ID6,282)", "HoldsAt(Meet_ID2_ID6,281)", "HoldsAt(Meet_ID2_ID6,280)", "HoldsAt(Meet_ID2_ID6,279)", "HoldsAt(Meet_ID2_ID6,278)", "HoldsAt(Meet_ID2_ID6,277)", "HoldsAt(Meet_ID2_ID6,276)", "HoldsAt(Meet_ID2_ID6,275)", "HoldsAt(Meet_ID2_ID6,274)", "HoldsAt(Meet_ID2_ID6,273)", "HoldsAt(Meet_ID2_ID6,272)", "HoldsAt(Meet_ID2_ID6,271)", "HoldsAt(Meet_ID2_ID6,270)", "HoldsAt(Meet_ID2_ID6,269)", "HoldsAt(Meet_ID2_ID6,268)", "HoldsAt(Meet_ID2_ID6,267)", "HoldsAt(Meet_ID2_ID6,266)", "HoldsAt(Meet_ID2_ID6,265)", "HoldsAt(Meet_ID2_ID6,264)", "HoldsAt(Meet_ID2_ID6,263)", "HoldsAt(Meet_ID2_ID6,262)", "HoldsAt(Meet_ID2_ID6,261)", "HoldsAt(Meet_ID2_ID6,260)", "HoldsAt(Meet_ID2_ID6,259)", "HoldsAt(Meet_ID2_ID6,258)", "HoldsAt(Meet_ID2_ID6,257)", "HoldsAt(Meet_ID2_ID6,256)", "HoldsAt(Meet_ID2_ID6,255)", "HoldsAt(Meet_ID2_ID6,254)", "HoldsAt(Meet_ID2_ID6,253)", "HoldsAt(Meet_ID2_ID6,252)", "HoldsAt(Meet_ID2_ID6,251)", "HoldsAt(Meet_ID2_ID6,250)", "HoldsAt(Meet_ID2_ID6,249)", "HoldsAt(Meet_ID2_ID6,248)", "HoldsAt(Meet_ID2_ID6,247)", "HoldsAt(Meet_ID2_ID6,246)", "HoldsAt(Meet_ID2_ID6,245)", "HoldsAt(Meet_ID2_ID6,244)", "HoldsAt(Meet_ID2_ID6,243)", "HoldsAt(Meet_ID2_ID6,242)", "HoldsAt(Meet_ID2_ID6,241)", "HoldsAt(Meet_ID2_ID6,240)", "HoldsAt(Meet_ID2_ID6,239)", "HoldsAt(Meet_ID2_ID6,238)", "HoldsAt(Meet_ID2_ID6,237)", "HoldsAt(Meet_ID2_ID6,236)", "HoldsAt(Meet_ID2_ID6,235)", "HoldsAt(Meet_ID2_ID6,234)", "HoldsAt(Meet_ID2_ID6,233)", "HoldsAt(Meet_ID2_ID6,232)", "HoldsAt(Meet_ID2_ID6,231)", "HoldsAt(Meet_ID2_ID6,230)", "HoldsAt(Meet_ID2_ID6,229)", "HoldsAt(Meet_ID2_ID6,228)", "HoldsAt(Meet_ID2_ID6,227)", "HoldsAt(Meet_ID2_ID6,226)", "HoldsAt(Meet_ID2_ID6,225)", "HoldsAt(Meet_ID2_ID6,224)", "HoldsAt(Meet_ID2_ID6,223)", "HoldsAt(Meet_ID2_ID6,222)", "HoldsAt(Meet_ID2_ID6,221)", "HoldsAt(Meet_ID2_ID6,220)", "HoldsAt(Meet_ID2_ID6,219)", "HoldsAt(Meet_ID2_ID6,218)", "HoldsAt(Meet_ID2_ID6,217)", "HoldsAt(Meet_ID2_ID6,216)", "HoldsAt(Meet_ID2_ID6,215)", "HoldsAt(Meet_ID2_ID6,214)", "HoldsAt(Meet_ID2_ID6,213)", "HoldsAt(Meet_ID2_ID6,212)", "HoldsAt(Meet_ID2_ID6,211)", "HoldsAt(Meet_ID2_ID6,210)", "HoldsAt(Meet_ID2_ID6,209)", "HoldsAt(Meet_ID2_ID6,208)", "HoldsAt(Meet_ID2_ID6,207)", "HoldsAt(Meet_ID2_ID6,206)", "HoldsAt(Meet_ID2_ID6,205)", "HoldsAt(Meet_ID2_ID6,204)", "HoldsAt(Meet_ID2_ID6,203)", "HoldsAt(Meet_ID2_ID6,202)", "HoldsAt(Meet_ID2_ID6,201)", "HoldsAt(Meet_ID2_ID6,200)", "HoldsAt(Meet_ID2_ID6,199)", "HoldsAt(Meet_ID2_ID6,198)", "HoldsAt(Meet_ID2_ID6,197)", "HoldsAt(Meet_ID2_ID6,196)", "HoldsAt(Meet_ID2_ID6,195)", "HoldsAt(Meet_ID2_ID6,194)", "HoldsAt(Meet_ID2_ID6,193)", "HoldsAt(Meet_ID2_ID6,192)", "HoldsAt(Meet_ID2_ID6,191)", "HoldsAt(Meet_ID2_ID6,190)", "HoldsAt(Meet_ID2_ID6,189)", "HoldsAt(Meet_ID2_ID6,188)", "HoldsAt(Meet_ID2_ID6,187)", "HoldsAt(Meet_ID2_ID6,186)", "HoldsAt(Meet_ID2_ID6,185)", "HoldsAt(Meet_ID2_ID6,184)", "HoldsAt(Meet_ID2_ID6,183)", "HoldsAt(Meet_ID2_ID6,182)", "HoldsAt(Meet_ID2_ID6,181)", "HoldsAt(Meet_ID2_ID6,180)", "HoldsAt(Meet_ID2_ID6,179)", "HoldsAt(Meet_ID2_ID6,178)", "HoldsAt(Meet_ID2_ID6,177)", "HoldsAt(Meet_ID2_ID6,176)", "HoldsAt(Meet_ID2_ID6,175)", "HoldsAt(Meet_ID2_ID6,174)", "HoldsAt(Meet_ID2_ID6,173)", "HoldsAt(Meet_ID2_ID6,172)", "HoldsAt(Meet_ID2_ID6,171)", "HoldsAt(Meet_ID2_ID6,170)", "HoldsAt(Meet_ID2_ID6,169)", "HoldsAt(Meet_ID2_ID6,168)", "HoldsAt(Meet_ID2_ID6,167)", "HoldsAt(Meet_ID2_ID6,166)", "HoldsAt(Meet_ID2_ID6,165)", "HoldsAt(Meet_ID2_ID6,164)", "HoldsAt(Meet_ID2_ID6,163)", "HoldsAt(Meet_ID2_ID6,162)", "HoldsAt(Meet_ID2_ID6,161)", "HoldsAt(Meet_ID2_ID6,160)", "HoldsAt(Meet_ID2_ID6,159)", "HoldsAt(Meet_ID2_ID6,158)", "HoldsAt(Meet_ID2_ID6,157)", "HoldsAt(Meet_ID2_ID6,156)", "HoldsAt(Meet_ID2_ID6,155)", "HoldsAt(Meet_ID2_ID6,154)", "HoldsAt(Meet_ID2_ID6,153)", "HoldsAt(Meet_ID2_ID6,152)", "HoldsAt(Meet_ID2_ID6,151)", "HoldsAt(Meet_ID2_ID6,150)", "HoldsAt(Meet_ID2_ID6,149)", "HoldsAt(Meet_ID2_ID6,148)", "HoldsAt(Meet_ID2_ID6,147)", "HoldsAt(Meet_ID2_ID6,146)", "HoldsAt(Meet_ID2_ID6,145)", "HoldsAt(Meet_ID2_ID6,144)", "HoldsAt(Meet_ID2_ID6,143)", "HoldsAt(Meet_ID2_ID6,142)", "HoldsAt(Meet_ID2_ID6,141)", "HoldsAt(Meet_ID2_ID6,140)", "HoldsAt(Meet_ID2_ID6,139)", "HoldsAt(Meet_ID2_ID6,138)", "HoldsAt(Meet_ID2_ID6,137)", "HoldsAt(Meet_ID2_ID6,136)", "HoldsAt(Meet_ID2_ID6,135)", "HoldsAt(Meet_ID2_ID6,134)", "HoldsAt(Meet_ID2_ID6,133)", "HoldsAt(Meet_ID2_ID6,132)", "HoldsAt(Meet_ID2_ID6,131)", "HoldsAt(Meet_ID2_ID6,130)", "HoldsAt(Meet_ID2_ID6,129)", "HoldsAt(Meet_ID2_ID6,128)", "HoldsAt(Meet_ID2_ID6,127)", "HoldsAt(Meet_ID2_ID6,126)", "HoldsAt(Meet_ID2_ID6,125)", "HoldsAt(Meet_ID2_ID6,124)", "HoldsAt(Meet_ID2_ID6,123)", "HoldsAt(Meet_ID2_ID6,122)", "HoldsAt(Meet_ID2_ID6,121)", "HoldsAt(Meet_ID2_ID6,120)", "HoldsAt(Meet_ID2_ID6,119)", "HoldsAt(Meet_ID2_ID6,118)", "HoldsAt(Meet_ID2_ID6,117)", "HoldsAt(Meet_ID2_ID6,116)", "HoldsAt(Meet_ID2_ID6,115)", "HoldsAt(Meet_ID2_ID6,114)", "HoldsAt(Meet_ID2_ID6,113)", "HoldsAt(Meet_ID2_ID6,112)", "HoldsAt(Meet_ID2_ID6,111)", "HoldsAt(Meet_ID2_ID6,110)", "HoldsAt(Meet_ID2_ID6,109)", "HoldsAt(Meet_ID2_ID6,108)", "HoldsAt(Meet_ID2_ID6,107)", "HoldsAt(Meet_ID2_ID6,106)", "HoldsAt(Meet_ID2_ID6,105)", "HoldsAt(Meet_ID2_ID6,104)", "HoldsAt(Meet_ID2_ID6,103)", "HoldsAt(Meet_ID2_ID6,102)", "HoldsAt(Meet_ID2_ID6,101)", "HoldsAt(Meet_ID2_ID6,100)", "HoldsAt(Meet_ID2_ID6,99)", "HoldsAt(Meet_ID2_ID6,98)", "HoldsAt(Meet_ID2_ID6,97)", "HoldsAt(Meet_ID2_ID6,96)", "HoldsAt(Meet_ID2_ID6,95)", "HoldsAt(Meet_ID2_ID6,94)", "HoldsAt(Meet_ID2_ID6,93)", "HoldsAt(Meet_ID2_ID6,92)", "HoldsAt(Meet_ID2_ID6,91)", "HoldsAt(Meet_ID2_ID6,90)", "HoldsAt(Meet_ID2_ID6,89)", "HoldsAt(Meet_ID2_ID6,88)", "HoldsAt(Meet_ID2_ID6,87)", "HoldsAt(Meet_ID2_ID6,86)", "HoldsAt(Meet_ID2_ID6,85)", "HoldsAt(Meet_ID2_ID6,84)", "HoldsAt(Meet_ID2_ID6,83)", "HoldsAt(Meet_ID2_ID6,82)", "HoldsAt(Meet_ID2_ID6,81)", "HoldsAt(Meet_ID2_ID6,80)", "HoldsAt(Meet_ID2_ID6,79)", "HoldsAt(Meet_ID2_ID6,78)", "HoldsAt(Meet_ID2_ID6,77)", "HoldsAt(Meet_ID2_ID6,76)", "HoldsAt(Meet_ID2_ID6,75)", "HoldsAt(Meet_ID2_ID6,74)", "HoldsAt(Meet_ID2_ID6,73)", "HoldsAt(Meet_ID2_ID6,72)", "HoldsAt(Meet_ID2_ID6,71)", "HoldsAt(Meet_ID2_ID6,70)", "HoldsAt(Meet_ID2_ID6,69)", "HoldsAt(Meet_ID2_ID6,68)", "HoldsAt(Meet_ID2_ID6,67)", "HoldsAt(Meet_ID2_ID6,66)", "HoldsAt(Meet_ID2_ID6,65)", "HoldsAt(Meet_ID2_ID6,64)", "HoldsAt(Meet_ID2_ID6,63)", "HoldsAt(Meet_ID2_ID6,62)", "HoldsAt(Meet_ID2_ID6,61)", "HoldsAt(Meet_ID2_ID6,60)", "HoldsAt(Meet_ID2_ID6,59)", "HoldsAt(Meet_ID2_ID6,58)", "HoldsAt(Meet_ID2_ID6,57)", "HoldsAt(Meet_ID2_ID6,56)", "HoldsAt(Meet_ID2_ID6,55)", "HoldsAt(Meet_ID2_ID6,54)", "HoldsAt(Meet_ID2_ID6,53)", "HoldsAt(Meet_ID2_ID6,52)", "HoldsAt(Meet_ID2_ID6,51)", "HoldsAt(Meet_ID2_ID6,50)", "HoldsAt(Meet_ID2_ID6,49)", "HoldsAt(Meet_ID2_ID6,48)", "HoldsAt(Meet_ID2_ID6,47)", "HoldsAt(Meet_ID2_ID6,46)", "HoldsAt(Meet_ID2_ID6,45)", "HoldsAt(Meet_ID2_ID6,44)", "HoldsAt(Meet_ID2_ID6,43)", "HoldsAt(Meet_ID2_ID6,42)", "HoldsAt(Meet_ID2_ID6,41)", "HoldsAt(Meet_ID2_ID6,40)", "HoldsAt(Meet_ID2_ID6,39)", "HoldsAt(Meet_ID2_ID6,38)", "HoldsAt(Meet_ID2_ID6,37)", "HoldsAt(Meet_ID2_ID6,36)", "HoldsAt(Meet_ID2_ID6,35)", "HoldsAt(Meet_ID2_ID6,34)", "HoldsAt(Meet_ID2_ID6,33)", "HoldsAt(Meet_ID2_ID6,32)", "HoldsAt(Meet_ID2_ID6,31)", "HoldsAt(Meet_ID2_ID6,30)", "HoldsAt(Meet_ID2_ID6,29)", "HoldsAt(Meet_ID2_ID6,28)", "HoldsAt(Meet_ID2_ID6,27)", "HoldsAt(Meet_ID2_ID6,26)", "HoldsAt(Meet_ID2_ID6,25)", "HoldsAt(Meet_ID2_ID6,24)", "HoldsAt(Meet_ID2_ID6,23)", "HoldsAt(Meet_ID2_ID6,22)", "HoldsAt(Meet_ID2_ID6,21)", "HoldsAt(Meet_ID2_ID6,20)", "HoldsAt(Meet_ID2_ID6,19)", "HoldsAt(Meet_ID2_ID6,18)", "HoldsAt(Meet_ID2_ID6,17)", "HoldsAt(Meet_ID2_ID6,16)", "HoldsAt(Meet_ID2_ID6,15)", "HoldsAt(Meet_ID2_ID6,14)", "HoldsAt(Meet_ID2_ID6,13)", "HoldsAt(Meet_ID2_ID6,12)", "HoldsAt(Meet_ID2_ID6,11)", "HoldsAt(Meet_ID2_ID6,10)", "HoldsAt(Meet_ID2_ID6,9)", "HoldsAt(Meet_ID2_ID6,8)", "HoldsAt(Meet_ID2_ID6,7)", "HoldsAt(Meet_ID2_ID6,6)", "HoldsAt(Meet_ID2_ID6,5)", "HoldsAt(Meet_ID2_ID6,4)", "HoldsAt(Meet_ID2_ID6,3)", "HoldsAt(Meet_ID2_ID6,2)", "HoldsAt(Meet_ID2_ID6,1)")
      var F = ArrayBuffer[(Double, Int, Double)]()
      //var F = new HashMap[Int, (Double, Double)]()
      val constraints = new TIntObjectHashMap[Constraint]()
      val unSatConstraints = new TIntObjectHashMap[Constraint]()

      constraints.putAll(groundClauses)
      for( (k, v) <- solution ) {
        constraints.retainEntries(new TIntObjectProcedure[Constraint] {
          override def execute(a: Int, b: Constraint): Boolean = {
            !((v == 1.0 && b.literals.contains(k)) || (v == 0.0 && b.literals.contains(-k)))
          }
        })
        if(v != 0.0 && v != 1.0) F += ( (0.0, k, v) )
      }

      info("Fractional Solutions: "+F.size)
      unSatConstraints.putAll(constraints)
      info("#UnSAT Clauses: " + unSatConstraints.size())

    //while(!F.isEmpty) {

      for (i <- 0 until F.size) {
        val k = F(i)._2
        val v = F(i)._3
        //info("========== STEP ==========")
        //val const = al_seq(i).replace("HoldsAt(","").replace(")","").split(",")
        //info("HoldsAt("+const.deep.mkString(", ")+")")
        //val k = idf.encode(const)
        //val res = decodeLiteral(k)(mrf.mln)
        //info("CHECKING: "+res)
        //info("REMAINING UnSAT CLAUSES: " + unSatConstraints.size())
        var delta = 0.0

        val constraintsIterator = unSatConstraints.iterator() // iterator on the ground clauses
        while (constraintsIterator.hasNext) {
          constraintsIterator.advance()
          val constraint = constraintsIterator.value()
          if (constraint.literals.contains(k))
            delta += constraint.weight
          if (constraint.literals.contains(-k))
            delta -= constraint.weight
        }
        if (delta > 0) solution(k) = 1.0 else solution(k) = 0.0
        //info("DELTA:" + delta)
        //F.remove(i)
        //F.append((delta, k, v))
        val y = solution(k)
        unSatConstraints.retainEntries(new TIntObjectProcedure[Constraint] {
          override def execute(a: Int, b: Constraint): Boolean = {
            !((y == 1.0 && b.literals.contains(k)) || (y == 0.0 && b.literals.contains(-k)))
          }
        })
        info("#UnSAT Clauses: " + unSatConstraints.size())
        info("SOLUTION: " + y)
      }
      //F = F.sortWith(_._1 > _._1)
      //info("F: "+F.mkString(", "))
      //val k = F(0)._2
      //val delta = F(0)._1
      //F.remove(0)
      //if (delta > 0) solution(k) = 1.0 else solution(k) = 0.0

//      val constraintsIterator = unSatConstraints.iterator() // iterator on the ground clauses
//      while (constraintsIterator.hasNext) {
//        constraintsIterator.advance()
//        val constraint = constraintsIterator.value()
//        val clause = constraint.literals.map {
//          l =>
//            decodeLiteral(l)(mrf.mln) match {
//              case Some(litTXT) => litTXT
//              case None => sys.error("Cannot decode literal: " + l)
//            }
//        }.reduceLeft(_ + " v " + _)
//        info("UnSAT Ground Clause: " + constraint.weight + " " + clause)
//      }

//      val y = solution(k)
//      unSatConstraints.retainEntries(new TIntObjectProcedure[Constraint] {
//        override def execute(a: Int, b: Constraint): Boolean = {
//          !((y == 1.0 && b.literals.contains(k)) || (y == 0.0 && b.literals.contains(-k)))
//        }
//      })
//      info("#UnSAT Clauses: " + unSatConstraints.size())
//      info("SOLUTION: " + y)

    //}
    var wNUnSat = 0
    var constraintsIterator = unSatConstraints.iterator() // iterator on the ground clauses
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val key = constraintsIterator.key()
      val constraint = constraintsIterator.value()
      groundClauses.remove(key)
      if(constraint.weight < 0) wNUnSat += 1
    }
    info("#SAT Clauses: " + groundClauses.size())
    var w = 0.0
    constraintsIterator = groundClauses.iterator() // iterator on the ground clauses
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      if(constraint.weight.isInfinite || constraint.weight.isNaN || constraint.weight == mrf.weightHard)
        w += mrf.weightHard
      else
        w += constraint.weight
    }
    info("#UnSAT clauses with negative weights: "+wNUnSat+"/"+unSatConstraints.size())
    info("Likelihood of the solution is: e^" + w)
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
