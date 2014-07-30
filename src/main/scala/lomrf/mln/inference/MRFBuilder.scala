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

import scala.concurrent.Await
import collection.breakOut
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import java.util
import java.util.concurrent.CountDownLatch
import lomrf.logic._
import lomrf.mln.model.MLN
import lomrf.util.{Utilities, Logging, AtomIdentityFunction, Cartesian}
import lomrf.{DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY, processors}
import scala.Some
import akka.actor._
import akka.pattern._
import akka.util.Timeout


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
 *
 * @author Anastasios Skarlatidis
 */
final class MRFBuilder(val mln: MLN, noNegWeights: Boolean = false, unitWeights: Boolean = false) extends Logging {

  private val mcSatParam = 1

  private val system = ActorSystem.create("MRFBuilder")


  private implicit val timeout = Timeout.intToTimeout(5000)


  def buildNetwork: MRF = {
    val latch = new CountDownLatch(1)

    val startTime = System.currentTimeMillis()
    val masterActor = system.actorOf(Props(new Master(mln, latch)), name = "master")
    latch.await()
    val endTime = System.currentTimeMillis()
    val result = Await.result((masterActor ? REQUEST_RESULTS).mapTo[Result], timeout.duration)


    system.shutdown()


    var weightHard = 10.0
    for (clause <- mln.clauses; if !clause.isHard && !clause.variables.isEmpty) {
      weightHard += (math.abs(clause.weight) * clause.variables.map(v => mln.constants(v.domain).size).reduceLeft(_ * _))
    }
    weightHard = math.ceil(weightHard)
    info("Hard weight value is set to: " + weightHard)

    // Conversion to flat version
    val numConstraints = if (result.cliques ne null) result.cliques.map(fs => if (fs ne null) fs.size() else 0).sum else 0
    var numAtoms = if (result.cliques ne null) result.atom2Cliques.map(as => if (as ne null) as.size() else 0).sum else 0
    if (numAtoms == 0) numAtoms = if (result.queryAtomIDs ne null) result.queryAtomIDs.map(qas => if (qas ne null) qas.size() else 0).sum else 0

    if (numAtoms == 0) fatal("The ground MRF is empty.")


    val constraints = new TIntObjectHashMap[Constraint](if (numConstraints == 0) DEFAULT_CAPACITY else numConstraints, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)
    val atoms = new TIntObjectHashMap[GroundAtom](if (numAtoms == 0) DEFAULT_CAPACITY else numAtoms)

    for (qas <- result.queryAtomIDs) {
      val iterator = qas.iterator()
      while (iterator.hasNext) {
        val atomID = iterator.next()
        atoms.putIfAbsent(atomID, new GroundAtom(atomID, weightHard))
      }
    }

    for (segmentIdx <- 0 until result.cliques.length) {
      if (result.cliques(segmentIdx) ne null) {
        val clausesIterator = result.cliques(segmentIdx).iterator()
        while (clausesIterator.hasNext) {
          clausesIterator.advance()
          val clique = clausesIterator.value()
          require(!clique.weight.isNaN, "Found a clause with weight == NaN (possible bug?).")

          if(unitWeights) {
            if (clique.weight.isInfinite)
              constraints.put(clausesIterator.key(), new Constraint(1, clique.variables, true, 1.0, clausesIterator.key()))
            else if (clique.weight != 0)
              constraints.put(clausesIterator.key(),
                new Constraint(if(clique.weight<0) -1 else 1, clique.variables, false,
                  1 - math.exp(-math.abs(clique.weight) * mcSatParam), clausesIterator.key()))
          }
          else {
            if (clique.weight.isInfinite)
              constraints.put(clausesIterator.key(), new Constraint(weightHard, clique.variables, true, 1.0, clausesIterator.key()))
            else if (clique.weight != 0)
              constraints.put(clausesIterator.key(), new Constraint(clique.weight, clique.variables, false,
                1 - math.exp(-math.abs(clique.weight) * mcSatParam), clausesIterator.key()))
          }

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
    //Test if the Clique IDs are continuous
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

    //return
    MRF(mln, constraints, atoms, weightHard, mln.queryStartID, mln.queryEndID)
  }


  private final class Master(mln: MLN, latch: CountDownLatch) extends Actor with Logging {

    private val _variables2Cliques = new Array[TIntObjectMap[TIntHashSet]](processors)
    private val _cliques = new Array[TIntObjectMap[CliqueEntry]](processors)
    private val _queryAtomIDs = new Array[TIntSet](processors)
    private val atomsDB = new Array[TIntSet](processors)

    //private val numberOfFOLClauses = mln.clauses.size

    private var clauseCounter = -mln.queryAtoms.size
    //0
    private var cliqueBatchesCounter = 0
    private var atomBatchesCounter = 0
    private var atomIDBatchesCounter = 0
    private var clausesBatchSize = 0
    private var completed = false

    private var workerIdx = 0
    private var cliqueStartID = 0
    private var groundingIterations = 1
    private var remainingClauses: Set[Clause] = mln.clauses
    private var atomSignatures: Set[AtomSignature] = mln.queryAtoms.toSet


    private val atomRegisters: Array[ActorRef] = (
      for (i <- 0 until processors)
        yield context.actorOf(Props(new AtomRegister(i, this.self)), name = "atom_register-" + i)
      )(breakOut)

    private val cliqueRegisters: Array[ActorRef] = (
      for (i <- 0 until processors)
        yield context.actorOf(Props(new CliqueRegister(i, this.self, atomRegisters)), name = "clique_register-" + i)
      )(breakOut)

    private val clauseGroundingWorkers: Array[ActorRef] = {
      val n = mln.queryAtoms.size + mln.clauses.size
      (for (i <- 0 until (if (n <= processors) n else processors))
        yield context.actorOf(Props(new GroundingWorker(cliqueRegisters)), name = "grounding_worker-" + i)
      )(breakOut)
    }

    override def preStart() {
      info("Number of processors: " + lomrf.processors
        + "\n\tTotal atom registry workers to use: " + atomRegisters.length
        + "\n\tTotal clique registry workers to use: " + cliqueRegisters.length
        + "\n\tTotal grounding workers to use: " + clauseGroundingWorkers.length)

      // To make sure that all ground query predicates will be stored in the network,
      // insert all ground query predicates as zero weighted unit clauses
      for (signature <- mln.queryAtoms) {
        val terms =
          mln.schema(signature).view.zipWithIndex.map {
            case (argType: String, idx: Int) => Variable("v" + idx, argType, idx)
          }.toList
        remainingClauses += Clause(0, AtomicFormula(signature.symbol, terms))
      }

      performGrounding()
    }

    private def performGrounding() {
      var remaining = Set[Clause]()
      var counter = 0
      clauseCounter = 0
      for (clause <- remainingClauses) {
        if (clause.literals.exists(literal => atomSignatures.contains(literal.sentence.signature))) {
          clauseGroundingWorkers(workerIdx) ! Ground(clause, atomSignatures, atomsDB)
          workerIdx = if (workerIdx == clauseGroundingWorkers.length - 1) 0 else workerIdx + 1
          counter += 1
        }
        else remaining = remaining + clause

      }

      debug("(MASTER) Grounding iteration " + groundingIterations + " --- total " + counter + " clause(s) selected to ground.")
      groundingIterations += 1


      remainingClauses = remaining
      clausesBatchSize = counter

      if (counter == 0) {
        // Start Phase 2 :
        cliqueRegisters.foreach(_ ! GRND_Completed)
      }
    }

    def receive = {

      case ClauseGroundingCompleted(clause, collectedSignatures) =>
        clauseCounter += 1
        atomSignatures = collectedSignatures ++ atomSignatures

        if (clauseCounter == clausesBatchSize)
          atomRegisters.foreach(_ ! GRND_Iteration_Completed)

      case CollectedAtomIDs(index, atomIDs) =>
        atomIDBatchesCounter += 1
        atomsDB(index) = atomIDs
        if (atomIDBatchesCounter == atomRegisters.length) {
          atomIDBatchesCounter = 0
          if (remainingClauses.isEmpty) {
            // Start Phase 2 :
            cliqueRegisters.foreach(_ ! GRND_Completed)
          }
          else {
            //continue with the remaining clauses
            debug(
              "(MASTER) Continue with the remaining clauses..." +
                "\n(MASTER) AtomSignatures to focus grounding: " + atomSignatures.map(_.toString).reduceLeft(_ + ", " + _) +
                "\n(MASTER) Remaining clauses to ground: " + remainingClauses.size + " {" +
                remainingClauses.view.zipWithIndex.foldLeft("\n")((rest, entry) => rest + "\n" + entry._2 + " --- " + entry._1) +
                "\n"
            )
            performGrounding()
          }
        }

      case NumberOfCliques(index, size) =>
        sender ! StartID(cliqueStartID)
        sender ! PoisonPill
        cliqueStartID += size

      case CollectedCliques(index, cliques) =>
        _cliques(index) = cliques
        cliqueBatchesCounter += 1
        if (cliqueBatchesCounter == processors) atomRegisters.foreach(_ ! PoisonPill)

      case AtomsBatch(index, registry, queryAtoms) =>
        _variables2Cliques(index) = registry
        _queryAtomIDs(index) = queryAtoms
        atomBatchesCounter += 1

        if (atomBatchesCounter == processors) killAll()


      case REQUEST_RESULTS => if (completed) sender ! Result(_cliques, _variables2Cliques, _queryAtomIDs) else sender ! None
      case msg => fatal("Master --- Received an unknown message '" + msg + "' from " + sender)
    }

    private def killAll() {
      cliqueRegisters.foreach(_ ! PoisonPill)
      atomRegisters.foreach(_ ! PoisonPill)
      clauseGroundingWorkers.foreach(_ ! PoisonPill)
      completed = true
      latch.countDown()
    }

  }


  private final class AtomRegister(val index: Int, master: ActorRef) extends Actor {

    private lazy val atomID2CliqueID = new TIntObjectHashMap[TIntHashSet]()
    private val queryAtomIDs = new TIntHashSet()
    private val atomIDs = new TIntHashSet()
    private var buffer = new TIntArrayList()

    def receive = {

      case QueryVariable(atomID) => queryAtomIDs.add(atomID)
      case atomID: Int =>
        //println("AtomRegister[" + index + "].received: " + atomID)
        assert(atomID != 0, "atomID cannot be equal to zero.")

        if (!atomIDs.contains(atomID)) buffer.add(atomID)

      case GRND_Iteration_Completed =>
        atomIDs.addAll(buffer)
        sender ! CollectedAtomIDs(index, atomIDs)
        buffer = new TIntArrayList()

      case Register(atomID, cliqueID) =>
        assert(atomID != 0, "atomID cannot be equal to zero.")

        val cliqueSet = atomID2CliqueID.get(atomID)
        if (cliqueSet == null) {
          val set = new TIntHashSet()
          set.add(cliqueID)
          atomID2CliqueID.put(atomID, set)
        }
        else {
          cliqueSet.add(cliqueID)
        }

      case msg => fatal("AtomRegister[" + index + "] --- Received an unknown message: " + msg)
    }

    /**
     * When the AtomRegistryWorker is stopped, it sent the results to the master actor.
     */
    override def postStop() {
      master ! AtomsBatch(index, atomID2CliqueID, queryAtomIDs)
    }
  }

  private final class CliqueRegister(val index: Int, master: ActorRef, atomRegisters: Array[ActorRef]) extends Actor with Logging {

    private var hashCode2CliqueIDs = new TIntObjectHashMap[TIntArrayList]()
    private var cliques = new TIntObjectHashMap[CliqueEntry](DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

    private val numOfAtomBatches = atomRegisters.length

    private var cliqueID = 0

    //private var statReceived = 0
    //private var statMerged = 0

    def receive = {
      case ce: CliqueEntry =>
        //println("CliqueRegister[" + index + "].received: " + ce)
        if (ce.weight == 0 && ce.variables.length == 1) {
          atomRegisters(ce.variables(0) % numOfAtomBatches) ! QueryVariable(ce.variables(0))
        } else if (ce.weight != 0) storeClique(ce)
      case GRND_Completed => sender ! NumberOfCliques(index, cliques.size())
      case StartID(offset: Int) =>
        hashCode2CliqueIDs = null //not needed anymore (allow GC to delete it)

        val resultingCliques = new TIntObjectHashMap[CliqueEntry](cliques.size() + 1, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

        val iterator = cliques.iterator()
        var currentClique: CliqueEntry = null
        var currentCliqueID = 0

        while (iterator.hasNext) {
          iterator.advance()
          currentCliqueID = iterator.key() + offset
          currentClique = iterator.value()
          resultingCliques.put(currentCliqueID, currentClique)
          for (variable <- currentClique.variables) {
            val atomID = math.abs(variable)
            atomRegisters(atomID % numOfAtomBatches) ! Register(atomID, currentCliqueID)
          }
        }

        cliques = null //not needed anymore (allow GC to delete it)

        master ! CollectedCliques(index, resultingCliques)

      case msg => fatal("CliqueRegister[" + index + "] --- Received an unknown message '" + msg + "' from " + sender)
    }


    private def storeClique(cliqueEntry: CliqueEntry) {
      //statReceived += 1

      @inline def fetchClique(fid: Int): CliqueEntry = cliques.get(fid)

      @inline def put(fid: Int, clique: CliqueEntry) = cliques.put(fid, clique)

      val storedCliqueIDs = hashCode2CliqueIDs.get(cliqueEntry.hashKey)
      // (1) check for a stored clique with the same variables
      if (storedCliqueIDs ne null) {

        val iterator = storedCliqueIDs.iterator()
        var merged = false
        while (iterator.hasNext && !merged) {
          val storedId = iterator.next()
          val storedClique = fetchClique(storedId)
          if (util.Arrays.equals(storedClique.variables, cliqueEntry.variables)) {
            if (storedClique.weight != Double.PositiveInfinity) {
              // merge these cliques
              if (cliqueEntry.weight == Double.PositiveInfinity) {
                // When the stored constraint (from a previous run/iteration) is soft and
                // the new one is hard; then the resulting constraint will be hard.
                storedClique.weight = Double.PositiveInfinity
              }
              else {
                // When both stored and new constraints are soft, then merge these constraints
                storedClique.weight += cliqueEntry.weight
              }
            }
            // Otherwise, the stored constrain is hard, do not change anything and
            // thus ignore the current constraint.

            //state that a merging operation is performed.
            merged = true
            //statMerged += 1
          }
        } // while

        if (!merged) {
          // The constraint is not merged, thus we simply store it.
          put(cliqueID, cliqueEntry)
          storedCliqueIDs.add(cliqueID)
          registerVariables(cliqueEntry.variables)
          cliqueID += 1
        }
      }
      else {
        // (2) Otherwise store this clique
        if (cliqueEntry.weight != 0) {
          put(cliqueID, cliqueEntry)
          val newEntries = new TIntArrayList()
          newEntries.add(cliqueID)
          hashCode2CliqueIDs.put(cliqueEntry.hashKey, newEntries)
          cliqueID += 1
        }
        registerVariables(cliqueEntry.variables)

      }
    } // store(...)

    @inline private def registerVariables(variables: Array[Int]) {
      for (i <- 0 until variables.length) {
        val atomID = math.abs(variables(i))
        atomRegisters(atomID % numOfAtomBatches) ! atomID
      }
    }

  }


  private final class GroundingWorker(cliqueRegisters: Array[ActorRef]) extends Actor with Logging {
    //private var statTime = 0L

    def receive = {
      case Ground(clause, atomSignatures, atomsDB) =>
        val grounder = new ClauseGrounderImpl(clause, cliqueRegisters, atomSignatures, atomsDB)
        grounder.computeGroundings()
        sender ! ClauseGroundingCompleted(clause, grounder.collectedSignatures)

      case msg => fatal("GroundingWorker --- Received an unknown message '" + msg + "' from " + sender)
    }

    private final class ClauseGrounderImpl(val clause: Clause,
                                           cliqueRegisters: Array[ActorRef],
                                           atomSignatures: Set[AtomSignature],
                                           atomsDB: Array[TIntSet]) {

      import AtomIdentityFunction.IDENTITY_NOT_EXIST

      require(!clause.weight.isNaN, "Found a clause with not a valid weight value (NaN).")

      private val cliqueBatches = cliqueRegisters.length
      private val atomsDBBatches = atomsDB.length

      private var sat = 0

      private val variableDomains: Map[Variable, Iterable[String]] = {
        if (clause.isGround) Map.empty[Variable, Iterable[String]]
        else (for (v <- clause.variables) yield v -> mln.constants(v.domain))(breakOut)
      }

      private val groundIterator =
        try {
          Cartesian.CartesianIteratorMap(variableDomains)
        } catch {
          case ex: NoSuchElementException => fatal("Failed to initialise CartesianIterator for clause: " + clause.toString + " --- domain = " + variableDomains)
        }


      private val identities: Map[AtomSignature, AtomIdentityFunction] =
        (for (literal <- clause.literals if !mln.isDynamicAtom(literal.sentence.signature))
          yield literal.sentence.signature -> mln.identityFunctions(literal.sentence.signature))(breakOut)


      /**
       * <p> To improve the grounding speed, we change the order of clause literals according to their type
       *  (i.e. dynamic or regular predicates) and a score function.
       * </p>
       *
       *  <ul>
       *    <li> When both literals contain dynamic sentences (e.q. equals, lessThan, etc.), then
       *    the literal with the lowest number of Variables is placed first</li>
       *    <li> When only one literal contains a dynamic sentence, then there are two sub-cases:
       *    (1) if the other literal contains a sentence with unknown groundings, then the dynamic one
       *    is placed first. (2) Otherwise, the literal with the lowest number of Variables is placed first.</li>
       *    <li>Finally, when both literals are regular (i.e. not dynamic), then the literal with the
       *    lowest score is placed first:
       *    <br/>
       *    '''score = (number of unsatisfied - number of unknown)/(number of all groundings)'''
       *    <br/>
       *    In other words, this score value represents the fraction of tuples (i.e. constants replacing
       *    variables in the clause)  that will remain after the literal is grounded. This heuristic score function
       *    is based in the following paper:
       *    <br/>
       *    <br/>
       *    ''Shavlik, J. and Natarajan, S. Speeding Up Inference in Markov Logic Networks by pre-processing to
       *    Reduce the Size of the Resulting Grounded Network. In Proceedings of the 21th International
       *    Joint Conference on Artificial Intelligence (IJCAI), 2009.''
       *    </li>
       *  </ul>
       *
       *
       */
      private val orderedLiterals =
        clause.literals.view.map(lit =>
          (lit, identities.getOrElse(lit.sentence.signature, null))).toArray.sortBy(entry => entry._1)(new Ordering[Literal] {

          def compare(x: Literal, y: Literal) = {
            val xDB = mln.atomStateDB.getOrElse(x.sentence.signature, null)
            val yDB = mln.atomStateDB.getOrElse(y.sentence.signature, null)

            val scoreX =
              if (x.sentence.isDynamic) Double.NaN
              else {
                val satX = if (x.isNegative) xDB.numberOfFalse else xDB.numberOfTrue
                val unsatX = xDB.length - satX
                (unsatX + xDB.numberOfUnknown) / xDB.length.toDouble
              }

            val scoreY =
              if (y.sentence.isDynamic) Double.NaN
              else {
                val satY = if (y.isNegative) yDB.numberOfFalse else yDB.numberOfTrue
                val unsatY = yDB.length - satY
                (unsatY + yDB.numberOfUnknown) / yDB.length.toDouble
              }

            (scoreX, scoreY) match {
              case (Double.NaN, Double.NaN) =>
                val nVarX = x.sentence.variables.size
                val nVarY = y.sentence.variables.size
                nVarX.compare(nVarY)
              case (Double.NaN, _) =>
                if(yDB.numberOfUnknown > 0) -1
                else{
                  val nVarX = x.sentence.variables.size
                  val nVarY = y.sentence.variables.size
                  nVarX.compare(nVarY)
                }
              case (_, Double.NaN) =>
                if(xDB.numberOfUnknown > 0) 1
                else{
                  val nVarX = x.sentence.variables.size
                  val nVarY = y.sentence.variables.size
                  nVarX.compare(nVarY)
                }
              case _ =>
                // regular literals
                if (scoreX < scoreY) -1
                else if (scoreX > scoreY) 1
                else 0
            }
          }
        }
        )

      //private val owaLiterals = orderedLiterals.view.map(_._1).filter(literal => mln.isOWA(literal.sentence.signature))
      private val owaLiterals = orderedLiterals.view.map(_._1).filter(literal => mln.isTriState(literal.sentence.signature))

      // Collect dynamic atoms
      private lazy val dynamicAtoms: Map[Int, (List[String] => Boolean)] =
        (for (i <- 0 until orderedLiterals.length; sentence = orderedLiterals(i)._1.sentence; if sentence.isDynamic)
        yield i -> mln.dynamicAtoms(sentence.signature))(breakOut)

      //private val length = clause.literals.count(l => mln.isOWA(l.sentence.signature))
      private val length = clause.literals.count(l => mln.isTriState(l.sentence.signature))

      lazy val collectedSignatures = clause.literals.map(_.sentence.signature) -- atomSignatures

      private var groundingsCounter = 0

      def getVariableDomains = variableDomains

      def computeGroundings(): Int = {

        debug("The ordering of literals in clause: " + clause + "\n\t" +
          "changed to: " + orderedLiterals.map(_.toString()).reduceLeft(_ + " v " + _))

        sat = 0 //reset number of satisfied ground clauses

        var idCounter = 0

        // an array of integer literals, indicating the current ground clause's literals
        val currentVariables = new Array[Int](length)


        def performGrounding(theta: Map[Variable, String] = Map.empty[Variable, String]) {
          // partial function for substituting terms w.r.t the given theta
          val substitution = substituteTerm(theta) _
          var idx = 0 //literal position index in the currentVariables array
          var flagDrop = false //utility flag to indicate whether to keep or not the current ground clause
          val literalsIterator = orderedLiterals.iterator // literals iterator, that gives first all evidence literals

          while (!flagDrop && literalsIterator.hasNext) {
            val (literal, idf) = literalsIterator.next()
            // When the literal is a dynamic atom, then invoke its truth state dynamically
            if (literal.sentence.isDynamic) {
              if (literal.isPositive == dynamicAtoms(idx)(literal.sentence.terms.map(substitution))) flagDrop = true
            }
            else {
              // Otherwise, invoke its state from the evidence
              val atomID = idf.encode(literal.sentence, substitution)

              if (atomID == IDENTITY_NOT_EXIST) {
                // Due to closed-world assumption in the evidence atoms or in the function mappings,
                // the identity of the atom cannot be determined and in that case the current clause grounding
                // will be omitted from the MRF
                flagDrop = true
              } else {
                // Otherwise, the atomID has a valid id number and the following pattern matching procedure
                // investigates whether the current literal satisfies the ground clause. If it does, the clause
                // is omitted from the MRF, since it is always satisfied from that literal.
                val state = mln.atomStateDB(literal.sentence.signature).get(atomID).value
                if ((literal.isNegative && (state == FALSE.value)) || (literal.isPositive && (state == TRUE.value))) {
                  // the clause is always satisfied from that literal
                  sat += 1
                  flagDrop = true //we don't need to keep that ground clause
                }
                else if (state == UNKNOWN.value) {
                  // The state of the literal is unknown, thus the literal will be stored to the currentVariables
                  currentVariables(idx) = atomID
                  idx += 1
                }
              }
            }
          } //end:  while (literalsIterator.hasNext && !flagDrop)

          if (!flagDrop) {
            // So far the ground clause is produced, but we have to
            // examine whether we will keep it or not. If the
            // ground clause contains any literal that is included in the
            // atomsDB, then it will be stored (and later will be send to clique registers),
            // otherwise it will not be stored and omitted.

            var canSend = false //utility flag

            var owaIdx = 0
            val cliqueVariables = new Array[Int](idx)

            for (i <- 0 until idx) {
              //val currentLiteral = iterator.next()
              val currentAtomID = currentVariables(i)
              cliqueVariables(i) = if (owaLiterals(owaIdx).isPositive) currentAtomID else -currentAtomID

              // Examine whether the current literal is included to the atomsDB. If it isn't,
              // the current clause will be omitted from the MRF
              val atomsDBSegment = atomsDB(currentAtomID % atomsDBBatches)
              if (!canSend && (atomsDBSegment ne null)) canSend = atomsDBSegment.contains(currentAtomID)
              else if (atomsDBSegment eq null) canSend = true // this case happens only for Query literals

              owaIdx += 1
            }

            if (canSend) {
              // Finally, the current ground clause will be included in the MRF.
              // However, if the weight of the clause is a negative number, then
              // the ground clause will be negated and broke up into several
              // unit ground clauses with positive weight literals.

              if (noNegWeights && clause.weight < 0) {
                if (cliqueVariables.length == 1) {
                  // If the clause is unit and its weight value is negative
                  // negate this clause (e.g. the clause "-w A" will be converted into w !A)
                  cliqueVariables(0) = -cliqueVariables(0)
                  store(idCounter, -clause.weight, cliqueVariables)
                  idCounter += 1
                } else {
                  val posWeight = -clause.weight / cliqueVariables.length
                  cliqueVariables.foreach {
                    groundLiteral =>
                      store(idCounter, posWeight, Array(-groundLiteral))
                      idCounter += 1
                  }
                }
              }
              else {
                // store as it is
                if (cliqueVariables.length > 1) util.Arrays.sort(cliqueVariables)
                store(idCounter, clause.weight, cliqueVariables)

//                var www = clause.weight
//                if (cliqueVariables.length > 1) util.Arrays.sort(cliqueVariables)
//                else if(cliqueVariables.length == 1 && cliqueVariables(0) < 0){
//                  cliqueVariables(0) = -cliqueVariables(0)
//                  www = -www
//                }
//
//                store(idCounter, www, cliqueVariables)

                idCounter += 1
              }
            } // end: if (canSend)
          }
        }

        if (clause.isGround) performGrounding()
        else while (groundIterator.hasNext) performGrounding(theta = groundIterator.next())

        debug("Clause: " + clause.toString + " --- produced " + groundingsCounter + " groundings.")
        //return
        groundingsCounter
      }

      def numOfGroundings = groundingsCounter

      @inline
      private def substituteTerm(theta: collection.Map[Variable, String])(term: Term): String = term match {
        case c: Constant => c.symbol
        case v: Variable => theta(v)
        case f: Function =>
          mln.functionMappers.get(f.signature) match {
            case Some(m) => m(f.args.map(a => substituteTerm(theta)(a)))
            case None => fatal("Cannot apply substitution using theta: " + theta + " in function " + f.signature)
          }
      }

      @inline
      private def store(id: Int, weight: Double, variables: Array[Int]) {
        var hashKey = util.Arrays.hashCode(variables)
        if (hashKey == 0) hashKey += 1 //required for trove collections, since zero represents the key-not-found value

        cliqueRegisters(math.abs(hashKey % cliqueBatches)) ! CliqueEntry(hashKey, weight, variables)
        groundingsCounter += 1
      }


    }

  }

  // ----------------------------------------
  // Messages
  // ----------------------------------------


  /**
   * Message for requesting the final ground MRF from the Master actor.
   */
  private case object REQUEST_RESULTS

  private case object GRND_Iteration_Completed

  private case object GRND_Completed

  private case class Result(cliques: Array[TIntObjectMap[CliqueEntry]], atom2Cliques: Array[TIntObjectMap[TIntHashSet]], queryAtomIDs: Array[TIntSet])

  private case class ClauseGroundingCompleted(clause: Clause, collectedSignatures: Set[AtomSignature])

  // Master -> GroundingWorker
  private case class Ground(clause: Clause, atomSignatures: Set[AtomSignature], atomsDB: Array[TIntSet])

  // GroundingWorker -> CliqueRegister
  private case class CliqueEntry(hashKey: Int, var weight: Double, variables: Array[Int]) {


    override def hashCode() = hashKey

    override def equals(obj: Any): Boolean = obj match {
      case other: CliqueEntry =>
        other.hashKey == hashKey && other.weight == weight && util.Arrays.equals(other.variables, variables)
      case _ => false
    }
  }


  // Master -> AtomRegister
  private case class CollectedAtomIDs(atomRegisterIdx: Int, atomIDs: TIntSet)

  // AtomRegister -> Master
  private case class AtomsBatch(index: Int, registry: TIntObjectHashMap[TIntHashSet], queryAtomIDs: TIntSet)

  // CliqueRegister -> AtomRegister
  private case class Register(atomID: Int, cliqueID: Int)

  // CliqueRegister -> Master
  private case class CollectedCliques(index: Int, cliques: TIntObjectMap[CliqueEntry])

  private case class StartID(id: Int)

  private case class NumberOfCliques(index: Int, size: Int)

  private case class QueryVariable(atomID: Int)

}

