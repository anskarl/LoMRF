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

package lomrf.app

import lomrf.logic._
import lomrf.logic.AtomSignatureOps._
import lomrf.mln.learning.structure.ModeParser
import lomrf.mln.learning.supervision.graphs._
import lomrf.mln.learning.supervision.metric._
import lomrf.mln.model.{ AtomEvidenceDB, Evidence, KB, MLN }
import lomrf.util.NaturalComparator
import lomrf.util.evaluation.{ Evaluate, Metrics }
import lomrf.util.time._
import lomrf.util.logging.Implicits._
import scala.io.Source
import java.io.{ File, FileOutputStream, PrintStream }
import lomrf.app.ConnectorType._
import lomrf.app.GraphSolverType._
import lomrf.app.DistanceType._
import scala.util.{ Failure, Success }

/**
  * Command line tool for supervision completion
  */
object SemiSupervisionCLI extends CLIApp {

  // The path to the input MLN file
  private var _mlnFileName: Option[String] = None

  // Input training file(s) (path)
  private var _trainingFileNames: Option[Array[String]] = None

  // Input annotation file(s) (path)
  private var _annotationFileNames: Option[Array[String]] = None

  // Results file (path)
  private var _resultsFileName: Option[String] = None

  // The path to the input mode declaration file
  private var _modesFileName: Option[String] = None

  // The set of non evidence atoms (in the form of AtomName/Arity)
  private var _nonEvidenceAtoms = Set.empty[AtomSignature]

  // By default output the negatives
  private var _outputNegatives: Boolean = true

  // By default run using harmonic graph cut
  private var _solver: GraphSolverType = HGC

  // By default run using atomic distance
  private var _distance: DistanceType = Atomic

  // By default run using a kNN connector
  private var _connector: ConnectorType = kNN

  // Epsilon threshold for the eNN graph
  private var _epsilon = 0.75

  // K value for the kNN graph
  private var _k = 2

  opt("i", "input", "<kb file>", "Markov Logic file defining the predicate and function schema.", {
    v: String => _mlnFileName = Some(v)
  })

  opt("t", "training", "<training file | folder>", "Training database file(s)", {
    v: String =>
      val file = new java.io.File(v)
      if (file.isDirectory) _trainingFileNames =
        Some(file.listFiles.filter(file => file.getName.matches(".*[.]db")).map(file => file.getPath))
      else _trainingFileNames = Some(v.split(','))
  })

  opt("a", "annotation", "<annotation file | folder>", "Annotation database file(s)", {
    v: String =>
      val file = new java.io.File(v)
      if (file.isDirectory) _annotationFileNames =
        Some(file.listFiles.filter(file => file.getName.matches(".*[.]db")).map(file => file.getPath))
      else _annotationFileNames = Some(v.split(','))
  })

  opt("r", "results", "<results file>", "Results result file", {
    v: String => _resultsFileName = Some(v)
  })

  opt("ne", "non-evidence atoms", "<string>", "Comma separated non-evidence atoms. "
    + "Each atom must be defined using its identity (i.e. Name/arity). "
    + "For example the identity of NonEvidenceAtom(arg1,arg2) is NonEvidenceAtom/2", {
    _nonEvidenceAtoms ++= _.split(',').map(s => s.signature.getOrElse(logger.fatal(s"Cannot parse the arity of atom signature: $s")))
  })

  opt("m", "modes", "<mode file>", "Mode declarations file.", {
    v: String => _modesFileName = Some(v)
  })

  opt("s", "solver", "<nn | hgc>", "Specify a solver for completion (default is hgc).", {
    v: String =>
      v.trim.toLowerCase match {
        case "nn"  => _solver = NN
        case "hgc" => _solver = HGC
        case _     => logger.fatal(s"Unknown solver of type '$v'.")
      }
  })

  opt("d", "distance", "<binary | atomic | evidence>", "Specify a distance over atoms (default is atomic).", {
    v: String =>
      v.trim.toLowerCase match {
        case "binary"   => _distance = Binary
        case "atomic"   => _distance = Atomic
        case "evidence" => _distance = Structure
        case _          => logger.fatal(s"Unknown distance of type '$v'.")
      }
  })

  opt("c", "connector", "<kNN | eNN | Full>", "Specify a connection heuristic for the graph (default is kNN).", {
    v: String =>
      v.trim.toLowerCase match {
        case "knn"  => _connector = kNN
        case "enn"  => _connector = eNN
        case "full" => _connector = Full
        case _      => logger.fatal(s"Unknown connector of type '$v'.")
      }
  })

  intOpt("k", "kappa", "Kappa parameter for the kNN connector (default is " + _k + ")", {
    v: Int => if (v < 1) logger.fatal("k value must be any integer greater than zero, but you gave: " + v) else _k = v
  })

  doubleOpt("e", "epsilon", "Epsilon parameter for eNN connector (default is " + _epsilon + ").", {
    v: Double => if (v < 0 || v > 1) logger.fatal("Epsilon value must be any number greater or equal to zero and less or equal to one, but you gave: " + v) else _epsilon = v
  })

  flagOpt("skip-negatives", "skip-negatives", "Do not output negative labels into the resulted files.", {
    _outputNegatives = false
  })

  flagOpt("v", "version", "Print LoMRF version.", sys.exit(0))

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  private def completer(): Unit = {

    val strMLNFileName = _mlnFileName.getOrElse(logger.fatal("Please specify an input MLN file."))

    val strTrainingFileNames = _trainingFileNames
      .map(_.sortWith(NaturalComparator.compareBool))
      .getOrElse(logger.fatal("Please specify input training file(s)."))

    // Annotation files are optional
    val strAnnotationFileNames = _annotationFileNames
      .map(files => files.sortWith(NaturalComparator.compareBool))
      .getOrElse(Array.empty)

    val resultsStream = _resultsFileName match {
      case Some(fileName) =>
        if (fileName == strMLNFileName)
          logger.fatal(s"Output file '${fileName}' cannot be the same with input MLN '${strMLNFileName}' file")

        new PrintStream(new FileOutputStream(fileName), true)
      case None => System.out
    }

    val strModeFileName = _modesFileName.getOrElse(logger.fatal("Please specify an input mode declaration file."))

    // Parse all mode declarations from file
    val modes = ModeParser.parseFrom(new File(strModeFileName))
    logger.info("Modes Declarations: \n" + modes.map { case (signature, mode) => "\t" + signature + " -> " + mode }.mkString("\n"))

    logger.info("Parameters:"
      + "\n\t(ne) Non-evidence predicate(s): " + _nonEvidenceAtoms.map(_.toString).mkString(", ")
      + "\n\t(distance) Distance metric for atomic formula: " + _distance
      + "\n\t(connector) Graph connection heuristic: " + _connector
      + "\n\t(kappa) k parameter for the kNN connector: " + _k
      + "\n\t(epsilon) Epsilon parameter for the eNN connector: " + _epsilon
      + "\n\t(negatives) Output negative labels: " + _outputNegatives)

    // Init all statistics values to zero
    var actualPositive, actualNegative, positiveFound, negativeFound = 0
    var supervisionGraphs = Map.empty[AtomSignature, SupervisionGraph]
    var stats = Evaluate.empty

    // Create a knowledge base and convert all functions
    val (kb, constants) = KB.fromFile(strMLNFileName, convertFunctions = true)

    val connector =
      if (_connector == kNN) kNNConnector(_k)
      else if (_connector == eNN) eNNConnector(_epsilon)
      else FullConnector

    val distance: Metric[_ <: AtomicFormula] =
      if (_distance == Binary) BinaryMetric(HungarianMatcher)
      else if (_distance == Atomic) AtomMetric(HungarianMatcher)
      else StructureMetric(modes, HungarianMatcher)

    val start = System.currentTimeMillis

    for (step <- strTrainingFileNames.indices) {

      logger.info(s"Step ${step + 1} / ${strTrainingFileNames.length}. Processing chunk ${strTrainingFileNames(step)}")

      val currentTrainingFile = new File(strTrainingFileNames(step))

      // Do not force CWA in order to complete UNKNOWN query atoms
      val trainingEvidence = Evidence.fromFiles(
        kb,
        constants,
        _nonEvidenceAtoms,
        Set.empty[AtomSignature],
        kb.predicateSchema.keySet -- _nonEvidenceAtoms,
        List(currentTrainingFile),
        convertFunctions = true,
        forceCWAForAll   = false
      )

      // Partition the training data into annotation and evidence databases
      var (annotationDB, atomStateDB) = trainingEvidence.db.partition(e => _nonEvidenceAtoms.contains(e._1))

      // Define all non evidence atoms as unknown in the evidence database
      for (signature <- annotationDB.keysIterator)
        atomStateDB += (signature -> AtomEvidenceDB.allUnknown(trainingEvidence.db(signature).identity))

      // Count positive and negative labels found
      _nonEvidenceAtoms.foreach { querySignature =>
        positiveFound += annotationDB(querySignature).numberOfTrue
        negativeFound += annotationDB(querySignature).numberOfFalse
      }

      val evidence = new Evidence(trainingEvidence.constants, atomStateDB, trainingEvidence.functionMappers)
      val mln = MLN(kb.schema, evidence, _nonEvidenceAtoms, Vector.empty[Clause])

      // Create or update supervision graphs for each given non evidence atom
      _nonEvidenceAtoms.foreach { querySignature =>
        supervisionGraphs.get(querySignature) match {
          case Some(graph) => supervisionGraphs += querySignature ->
            (graph ++ (mln, annotationDB, modes))
          case None => supervisionGraphs += querySignature ->
            SupervisionGraph(mln, modes, annotationDB, querySignature, connector, distance)
        }
      }

      // Run supervision completion for all given non evidence atoms and collect the results
      val (completedEvidenceAtoms, completedEvidenceSet) = supervisionGraphs.values
        .map(graph => if (_solver == NN) graph.completeSupervisionNN else graph.completeSupervisionGraphCut)
        .foldLeft(Set.empty[EvidenceAtom] -> Set.empty[Evidence]) {
          case ((atoms, evidenceSet), tuple) => (atoms ++ tuple._1, evidenceSet + tuple._2)
        }

      logger.info(msecTimeToTextUntilNow("Supervision completion time until now: ", start))

      /*
       * OK, lets store the resulted completed supervision
       */
      val resultsFolder = new File(s"${currentTrainingFile.getParentFile.getParent}/$connector.${_distance}.${_solver}")
      resultsFolder.mkdirs

      val completedBatch = new PrintStream(resultsFolder.getCanonicalPath + "/" + currentTrainingFile.getName)

      val inputStream = Source.fromFile(currentTrainingFile)
      inputStream.getLines.filter { line =>
        _nonEvidenceAtoms.map(_.symbol).forall(!line.contains(_)) && line.nonEmpty
      }.foreach(completedBatch.println)
      inputStream.close()

      completedBatch.println("\n// Completed supervision")

      completedEvidenceSet.map(_.db).foreach { evidenceDB =>
        evidenceDB.foreach {
          case (signature, atomDB) =>

            completedBatch.println(s"// Positives for $signature")
            val atomIDF = atomDB.identity

            /*
             * We cannot decode the ids using the mln because the indices are allocated
             * in a different way, so there would be a problem.
             *
             * Note: Keep only positive. Negatives may be too many. We have CWA during learning.
             */
            atomIDF.indices.flatMap { id =>
              atomIDF.decode(id) match {
                case Success(terms) if atomDB(id) == TRUE =>
                  Some(s"${signature.symbol}(${terms.mkString(",")})")
                case Failure(exception) => throw exception
                case _                  => None
              }
            }.sortWith(NaturalComparator.compareBool).foreach(completedBatch.println)

            if (_outputNegatives) {
              completedBatch.println(s"// Negatives for $signature")

              atomIDF.indices.flatMap { id =>
                atomIDF.decode(id) match {
                  case Success(terms) if atomDB(id) == FALSE =>
                    Some(s"!${signature.symbol}(${terms.mkString(",")})")
                  case Failure(exception) => throw exception
                  case _                  => None
                }
              }.sortWith(NaturalComparator.compareBool).foreach(completedBatch.println)
            }
        }
      }

      completedBatch.close()

      // In case annotation files are given, output statistics
      if (strAnnotationFileNames.nonEmpty) {

        val currentAnnotationFile = new File(strAnnotationFileNames(step))

        if (resultsStream != System.out) resultsStream.println(
          s"Step ${step + 1} / ${strTrainingFileNames.length}:\n" +
            s"Chunk ${currentTrainingFile.getName}\n" +
            s"Annotation ${currentAnnotationFile.getName}"
        )

        // At this point force CWA because annotation cannot have UNKNOWN atoms.
        val fullAnnotationDB = Evidence.fromFiles(
          kb,
          trainingEvidence.constants,
          _nonEvidenceAtoms,
          Set.empty[AtomSignature],
          kb.predicateSchema.keySet -- _nonEvidenceAtoms,
          List(currentAnnotationFile),
          convertFunctions = false,
          forceCWAForAll   = true
        )

        _nonEvidenceAtoms.foreach { querySignature =>
          actualPositive += fullAnnotationDB.db(querySignature).numberOfTrue
          actualNegative += fullAnnotationDB.db(querySignature).numberOfFalse
        }

        stats = Evaluate(completedEvidenceAtoms.toSeq.par, fullAnnotationDB.db, Some(stats))
        if (stats != Evaluate.empty) Metrics.report(stats, resultsStream)
        resultsStream.println(s"Positives: $positiveFound/$actualPositive Negatives: $negativeFound/$actualNegative\n")
      }

    }

    if (resultsStream != System.out) {
      resultsStream.println(msecTimeToTextUntilNow("Total supervision completion time: ", start))
      resultsStream.close()
    }
    logger.info(msecTimeToTextUntilNow("Total supervision completion time: ", start))
  }

  // Main:
  if (args.length == 0) println(usage)
  else if (parse(args)) completer()
}
