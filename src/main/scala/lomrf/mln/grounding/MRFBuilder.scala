/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *
 *
 */

package lomrf.mln.grounding

import java.util.concurrent.CountDownLatch

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import lomrf.util.collection.trove.TroveConversions._
import com.typesafe.scalalogging.LazyLogging
import gnu.trove.map.TIntFloatMap
import gnu.trove.map.hash.TIntObjectHashMap
import lomrf.mln.model.MLN
import lomrf.mln.model.mrf.{ Constraint, GroundAtom, MRF }
import lomrf.util.time._
import lomrf.util.logging.Implicits._
import lomrf.{ DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY }

import scala.concurrent.{ Await, ExecutionContextExecutor, Future }
import scala.concurrent.duration._
import scala.language.postfixOps
import spire.syntax.cfor._

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
  */
final class MRFBuilder(
    val mln: MLN,
    noNegWeights: Boolean = false,
    eliminateNegatedUnit: Boolean = false,
    createDependencyMap: Boolean = false) extends LazyLogging {

  import messages._

  private val mcSatParam = 1

  private val system = ActorSystem.create("MRFBuilder")
  private implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  private implicit val timeout: Timeout = Timeout(21474800 seconds) // todo should fix this

  def buildNetwork: MRF = {
    val latch = new CountDownLatch(1)

    val startTime = System.currentTimeMillis()
    val masterActor = system.actorOf(Props(new GroundingMaster(mln, latch, noNegWeights, eliminateNegatedUnit, createDependencyMap)), name = "master")
    latch.await()
    val endTime = System.currentTimeMillis()
    val resultF: Future[Result] = (masterActor ? REQUEST_RESULTS).mapTo[Result]
    val result = Await.result(resultF, Duration.Inf)

    system.terminate().foreach { _ =>
      logger.info("Actor system was shut down")
    }

    var weightHard = 10.0
    for (clause <- mln.clauses; if !clause.isHard && clause.variables.nonEmpty) {
      val productOfVarDomains = clause.variables.iterator.map(v => mln.evidence.constants(v.domain).size).reduceLeftOption(_ * _)
      val productOfFuncDomains = clause.functions.iterator.map(f => mln.evidence.constants(f.domain).size).reduceLeftOption(_ * _)

      weightHard += (math.abs(clause.weight) * productOfVarDomains.getOrElse(0) * productOfFuncDomains.getOrElse(1))
    }
    logger.info("Hard weight value is set to: " + weightHard)

    // Conversion to flat version
    val numConstraints = if (result.cliques ne null) result.cliques.partitions.map(fs => if (fs ne null) fs.size() else 0).sum else 0
    var numAtoms = if (result.cliques ne null) result.atom2Cliques.partitions.map(as => if (as ne null) as.size() else 0).sum else 0
    if (numAtoms == 0) numAtoms = if (result.queryAtomIDs ne null) result.queryAtomIDs.partitions.map(qas => if (qas ne null) qas.size() else 0).sum else 0

    if (numAtoms == 0) logger.fatal("The ground MRF is empty.")

    val constraints = new TIntObjectHashMap[Constraint](
      if (numConstraints == 0) DEFAULT_CAPACITY else numConstraints, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

    val atoms = new TIntObjectHashMap[GroundAtom](
      if (numAtoms == 0) DEFAULT_CAPACITY else numAtoms)

    val mergedDependencyMapOpt =
      if (createDependencyMap) {
        val mergedDependencyMap =
          new TIntObjectHashMap[TIntFloatMap](
            if (numConstraints == 0) DEFAULT_CAPACITY else numConstraints, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

        // Update the frequencies in the DependencyMap:
        // When a clause has negative a negative weight and the 'noNegWeights' is True:
        //    - adjust the frequency value by dividing with the size of the corresponding FOL clause.

        val dependencyMapPartitions = result.dependencyMap.getOrElse(logger.fatal("DependencyMap is not computed."))

        if (noNegWeights) for {
          partition <- dependencyMapPartitions.partitions.par
          (_, frequencies) <- partition.iterator()
        } {

          val iterator = frequencies.iterator()
          while (iterator.hasNext) {
            iterator.advance()
            if (iterator.value() < 0)
              iterator.setValue(iterator.value() / mln.clauses(iterator.key()).size)
          }
        }

        dependencyMapPartitions.partitions.foreach(mergedDependencyMap.putAll)

        Some(mergedDependencyMap)
      } else None

    for (qas <- result.queryAtomIDs.partitions) {
      val iterator = qas.iterator()
      while (iterator.hasNext) {
        val atomID = iterator.next()
        atoms.putIfAbsent(atomID, new GroundAtom(atomID, weightHard))
      }
    }

    cfor(0) (_ < result.cliques.size, _ + 1) { segmentIdx: Int =>
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

    logger.info("Grounding completed:" +
      "\n\tTotal grounding time: " + msecTimeToText(endTime - startTime)
      + "\n\tTotal ground clauses: " + constraints.size()
      + "\n\tTotal ground atoms: " + atoms.size())

    MRF(mln, constraints, atoms, weightHard, mergedDependencyMapOpt)
  }
}

