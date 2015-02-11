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

import java.util.concurrent.CountDownLatch

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import auxlib.log.Logging
import gnu.trove.map.TIntIntMap
import gnu.trove.map.hash.{TIntIntHashMap, TIntObjectHashMap}
import lomrf.mln.model.MLN
import lomrf.mln.model.mrf.{GroundAtom, Constraint, MRF}
import lomrf.util.Utilities
import lomrf.{DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY}

import scala.concurrent.Await
import scala.language.postfixOps
import scalaxy.loops._


/**
 * This is a high-performance parallel algorithm for ground MRF construction.
 *
 * <p> The implementation uses the high-performance Actor Akka framework ( [[http://akka.io]] ), in order to distribute the grounding
 * process over the available processors/threads. Additionally, for memory and speed efficiency local processes take advantage
 * of the Trove library, that provides high speed regular and primitive collections ( [[http://trove.starlight-systems.com]] ).
 * </p>
 *
 * <p> General architecture and features:
 * <ul>
 * <li>The algorithm distributes the FOL clauses into the available system processors.
 * The grounding process is performed in parallel and supports the following features:
 * <ul>
 * <li>Eliminates all tautological clauses</li>
 * <li>Support for FOL functions</li>
 * <li>Grounds the minimal required Markov network. Like many Knowledge Base Model Construction methods,
 * all nodes not associated (directly or indirectly through intermediate nodes) with the query variables
 * are eliminated.</li>
 * <li>Optionally, negative clauses can be transformed into positive clauses. For example, the ground clause
 * {{{-3 A v B v C}}}
 * will be transformed into the following ground clauses:
 * {{{
 *     1 !A
 *     1 !B
 *     1 !C
 * }}}
 * </li>
 * <li>Optionally, negated unit clauses can be eliminated by inverting their weights. For example, the ground unit clause
 * {{{ 3 !A }}}
 * will be transformed into the following ground clause:
 * {{{ -3 A }}}
 * </li>
 * </ul>
 * </li>
 * <li>The produced cliques (i.e. ground clauses clauses) are distributed across the available processors and
 * cliques with the same nodes (literals) are merged into a single clique (ground clause).</li>
 * <li>All produced nodes (ground atoms) are distributed across the available processors.</li>
 * </ul>
 * </p>
 *
 * @param mln the input MLN to ground
 * @param noNegWeights transform negative weighted clauses into (possibly several) positive weighted clauses (default is false, since the inference algorithms support negative weights).
 * @param eliminateNegatedUnit eliminate negated unit clauses by transforming them into negative positive unit clauses.
 *
 * @author Anastasios Skarlatidis
 */
final class MRFBuilder(val mln: MLN, noNegWeights: Boolean = false, eliminateNegatedUnit: Boolean = false) extends Logging {

  private val mcSatParam = 1

  private val system = ActorSystem.create("MRFBuilder")


  private implicit val timeout = Timeout.intToTimeout(5000)


  def buildNetwork: MRF = {
    val latch = new CountDownLatch(1)

    val startTime = System.currentTimeMillis()
    val masterActor = system.actorOf(Props(new GroundingMaster(mln, latch, noNegWeights, eliminateNegatedUnit)), name = "master")
    latch.await()
    val endTime = System.currentTimeMillis()
    val result = Await.result((masterActor ? REQUEST_RESULTS).mapTo[Result], timeout.duration)


    system.shutdown()


    var weightHard = 10.0
    for (clause <- mln.clauses; if !clause.isHard && clause.variables.nonEmpty) {
      val productOfVarDomains = clause.variables.iterator.map(v => mln.constants(v.domain).size).reduceLeftOption(_ * _)
      val productOfFuncDomains = clause.functions.iterator.map(f => mln.constants(f.domain).size).reduceLeftOption(_ * _)

      weightHard += (math.abs(clause.weight) * productOfVarDomains.getOrElse(0) * productOfFuncDomains.getOrElse(1))
    }
    info("Hard weight value is set to: " + weightHard)

    // Conversion to flat version
    val numConstraints = if (result.cliques ne null) result.cliques.map(fs => if (fs ne null) fs.size() else 0).sum else 0
    var numAtoms = if (result.cliques ne null) result.atom2Cliques.map(as => if (as ne null) as.size() else 0).sum else 0
    if (numAtoms == 0) numAtoms = if (result.queryAtomIDs ne null) result.queryAtomIDs.map(qas => if (qas ne null) qas.size() else 0).sum else 0

    if (numAtoms == 0) fatal("The ground MRF is empty.")


    val constraints = new TIntObjectHashMap[Constraint](
      if (numConstraints == 0) DEFAULT_CAPACITY else numConstraints, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

    val atoms = new TIntObjectHashMap[GroundAtom](
      if (numAtoms == 0) DEFAULT_CAPACITY else numAtoms)

    val mergedDependencyMap = new TIntObjectHashMap[TIntIntMap](
      if (numConstraints == 0) DEFAULT_CAPACITY else numConstraints, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)
    result.dependencyMap.foreach(mergedDependencyMap.putAll)

    for (qas <- result.queryAtomIDs) {
      val iterator = qas.iterator()
      while (iterator.hasNext) {
        val atomID = iterator.next()
        atoms.putIfAbsent(atomID, new GroundAtom(atomID, weightHard))
      }
    }

    for (segmentIdx <- (0 until result.cliques.length).optimized) {
      if (result.cliques(segmentIdx) ne null) {
        val clausesIterator = result.cliques(segmentIdx).iterator()
        while (clausesIterator.hasNext) {
          clausesIterator.advance()
          val clique = clausesIterator.value()
          require(!clique.weight.isNaN, "Found a clause with weight == NaN (possible bug?).")

          if (clique.weight.isInfinite)
            constraints.put(clausesIterator.key(), new Constraint(weightHard, clique.variables, true, 1.0, clausesIterator.key()))
          else if (clique.weight != 0)
            constraints.put(clausesIterator.key(), new Constraint(clique.weight, clique.variables, false,
              1 - math.exp(-math.abs(clique.weight) * mcSatParam), clausesIterator.key()))

          // println(constraint.weight+" "+constraint.literals.mkString(" "))

        }

        val atomsIterator = result.atom2Cliques(segmentIdx).iterator()

        while (atomsIterator.hasNext) {
          atomsIterator.advance()
          val atomId = atomsIterator.key()
          atoms.putIfAbsent(atomId, new GroundAtom(atomId, weightHard))
        }
      }

    }

    info("Grounding completed:" +
      "\n\tTotal grounding time: " + Utilities.msecTimeToText(endTime - startTime)
      + "\n\tTotal ground clauses: " + constraints.size()
      + "\n\tTotal ground atoms: " + atoms.size())


    //---------------------------------------------
    // Todo: add this as a unit test
    // This code tests if the Clique IDs are continuous
    /*val keys = constraints.keys()
    var fail = false
    util.Arrays.sort(keys)
    for((key,idx) <- keys.zipWithIndex) {
      if(key != idx){
        println(key+" != "+idx)
        fail = true
      }
    }
    if(fail) sys.exit() // */
    //---------------------------------------------

    MRF(mln, constraints, atoms, weightHard, mln.queryStartID, mln.queryEndID, mergedDependencyMap)
  }
}

