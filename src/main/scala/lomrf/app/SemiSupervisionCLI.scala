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
import lomrf.mln.learning.supervision.graphs.SupervisionGraph
import lomrf.mln.learning.supervision.graphs.SupervisionGraph._
import lomrf.mln.learning.supervision.metric.HungarianMatcher
import lomrf.mln.model.{AtomEvidenceDB, Evidence, KB, MLN}
import lomrf.util.NaturalComparator
import lomrf.util.evaluation.{Evaluate, Metrics}
import lomrf.util.time._
import lomrf.util.logging.Implicits._
import scala.io.Source
import java.io.{File, FileOutputStream, PrintStream}
import scala.util.{Failure, Success}

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

  // The set of domains to group by
  private var _groupByDomains: Option[Set[String]] = None

  // The set of numerical domains
  private var _numericalDomains: Option[Set[String]] = None

  // By default run using a kNN connector
  private var _kNNConnector = true

  // Epsilon threshold for the eNN graph
  private var _epsilon = 0.75

  // K value for the kNN graph
  private var _k = 2

  // Cache labels for online supervision completion
  private var _cacheLabels = false

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

  opt("gD", "group-domains", "<string>", "Comma separated domains to group the data.", {
    v: String => _groupByDomains = Some(v.split(',').toSet)
  })

  opt("nD", "numerical-domains", "<string>", "Comma separated domains to be considered as numerical.", {
    v: String => _numericalDomains = Some(v.split(',').toSet)
  })

  opt("c", "connector", "<kNN | eNN>", "Specify a connection heuristic for the graph (default is kNN).", {
    v: String => v.trim.toLowerCase match {
      case "knn" => _kNNConnector = true
      case "enn" => _kNNConnector = false
      case _ => logger.fatal(s"Unknown connector of type '$v'.")
    }
  })

  intOpt("k", "kappa", "Kappa parameter for the kNN connector (default is "+ _k +")", {
    v: Int => if (v < 1) logger.fatal("k value must be any integer greater than zero, but you gave: " + v) else _k = v
  })

  doubleOpt("e", "epsilon", "Epsilon parameter for eNN connector (default is " + _epsilon + ").", {
    v: Double => if (v < 0 || v > 1) logger.fatal("Epsilon value must be any number greater or equal to zero and less or equal to one, but you gave: " + v) else _epsilon = v
  })

  flagOpt("cache", "cache-labels", "Cache labels for online supervision completion.", {
    _cacheLabels = true
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
      case Some(fileName) => new PrintStream(new FileOutputStream(fileName), true)
      case None => System.out
    }

    val strModeFileName = _modesFileName.getOrElse(logger.fatal("Please specify an input mode declaration file."))

    // Parse all mode declarations from file
    val modes = ModeParser.parseFrom(new File(strModeFileName))
    logger.info("Modes Declarations: \n" + modes.map { case (signature, mode) => "\t" + signature + " -> " + mode }.mkString("\n"))

    logger.info("Parameters:"
      + "\n\t(ne) Non-evidence predicate(s): " + _nonEvidenceAtoms.map(_.toString).mkString(", ")
      + "\n\t(gD) GroupBy domains: " + _groupByDomains.getOrElse(Set("None")).mkString(", ")
      + "\n\t(nD) Numerical domains: " + _numericalDomains.getOrElse(Set("None")).mkString(", ")
      + "\n\t(connector) Graph connection heuristic: " + ( if (_kNNConnector) "kNN" else "eNN" )
      + "\n\t(kappa) k parameter for the kNN connector: " + _k
      + "\n\t(epsilon) Epsilon parameter for the eNN connector: " + _epsilon
      + "\n\t(cache) Cache labels for online supervision completion: " + _cacheLabels
    )

    // Init all statistics values to zero
    var actualPositive, actualNegative, positiveFound, negativeFound = 0
    var supervisionGraphs = Map.empty[AtomSignature, SupervisionGraph]
    var stats = Evaluate.empty

    // Create a knowledge base and convert all functions
    val (kb, constants) = KB.fromFile(strMLNFileName, convertFunctions = true)

    val start = System.currentTimeMillis
    for (step <- strTrainingFileNames.indices) {

      logger.info(s"Step ${step + 1} / ${strTrainingFileNames.length}. Processing chunk ${strTrainingFileNames(step)}")

      val currentTrainingFile = new File(strTrainingFileNames(step))

      // Do not force CWA in order to complete UNKNOWN query atoms
      val trainingEvidence =
        Evidence.fromFiles(kb, constants,
          _nonEvidenceAtoms,
          Set.empty[AtomSignature],
          kb.predicateSchema.keySet -- _nonEvidenceAtoms,
          List(currentTrainingFile),
          convertFunctions = true,
          forceCWAForAll = false)

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

      val evidence = Evidence(trainingEvidence.constants, atomStateDB, trainingEvidence.functionMappers)
      val mln = MLN(kb.schema, evidence, _nonEvidenceAtoms, Vector.empty[Clause])

      // Create or update supervision graphs for each given non evidence atom
      _nonEvidenceAtoms.foreach { querySignature =>
        (_kNNConnector, _cacheLabels, supervisionGraphs.get(querySignature).isEmpty) match {
          case (true, _, true) | (true, false, false) =>
            supervisionGraphs +=
              querySignature -> kNNGraph(_k, mln, modes, annotationDB, querySignature, HungarianMatcher, _groupByDomains, _numericalDomains)

          case (false, _, true) | (false, false, false) =>
            supervisionGraphs +=
              querySignature -> eNNGraph(_epsilon, mln, modes, annotationDB, querySignature, HungarianMatcher, _groupByDomains, _numericalDomains)

          case (_, true, false) =>
            supervisionGraphs += querySignature -> (supervisionGraphs(querySignature) ++ (mln, annotationDB, modes))
        }
      }

      // Run supervision completion for all given non evidence atoms and collect the results
      val (completedEvidenceAtoms, completedEvidenceSet) =
        supervisionGraphs.values.map(_.completeSupervision).foldLeft(Set.empty[EvidenceAtom] -> Set.empty[Evidence]) {
          case ((atoms, evidenceSet), tuple) => (atoms ++ tuple._1, evidenceSet + tuple._2)
        }

      logger.info(msecTimeToTextUntilNow("Supervision completion time until now: ", start))

      /*
       * OK, lets store the resulted completed supervision
       */
      val resultsFolder = new File(
        currentTrainingFile.getParentFile.getParent + "/" +
          (if (_kNNConnector) "kNN." + _k else "eNN." + _epsilon) + ".batch." +
          (if (_cacheLabels) "cache" else "no.cache")
      )
      resultsFolder.mkdirs()

      val completedBatch = new PrintStream(
        new File(resultsFolder.getCanonicalPath + "/" + currentTrainingFile.getName)
      )

      val inputStream = Source.fromFile(currentTrainingFile)
      inputStream.getLines.filter { line =>
        _nonEvidenceAtoms.map(_.symbol).forall(!line.contains(_)) && line.nonEmpty
      }.foreach(completedBatch.println(_))
      inputStream.close()

      completedBatch.println("\n// Completed supervision")

      completedEvidenceSet.map(_.db).foreach { evidenceDB =>
        evidenceDB.foreach { case (signature, atomDB) =>

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
              case _ => None
            }
          }.sortWith(NaturalComparator.compareBool).foreach(completedBatch.println(_))

          // TODO negatives are too many, should be removed
          completedBatch.println(s"// Negatives for $signature")

          atomIDF.indices.flatMap { id =>
            atomIDF.decode(id) match {
              case Success(terms) if atomDB(id) == FALSE =>
                Some(s"!${signature.symbol}(${terms.mkString(",")})")
              case Failure(exception) => throw exception
              case _ => None
            }
          }.sortWith(NaturalComparator.compareBool).foreach(completedBatch.println(_))

        }
      }

      completedBatch.close()

      // In case annotation files are given, output statistics
      if (strAnnotationFileNames.nonEmpty) {

        val currentAnnotationFile = new File(strAnnotationFileNames(step))

        if (resultsStream != System.out)
          resultsStream.println(s"Step ${step + 1} / ${strTrainingFileNames.length}:\n" +
            s"Chunk ${currentTrainingFile.getName}\nAnnotation ${currentAnnotationFile.getName}")

        // At this point force CWA because annotation cannot have UNKNOWN atoms.
        val fullAnnotationDB =
          Evidence.fromFiles(kb, trainingEvidence.constants,
            _nonEvidenceAtoms,
            Set.empty[AtomSignature],
            kb.predicateSchema.keySet -- _nonEvidenceAtoms,
            List(currentAnnotationFile),
            convertFunctions = false,
            forceCWAForAll = true)

        _nonEvidenceAtoms.foreach { querySignature =>
          actualPositive += fullAnnotationDB.db(querySignature).numberOfTrue
          actualNegative += fullAnnotationDB.db(querySignature).numberOfFalse
        }

        stats = Evaluate(completedEvidenceAtoms.toSeq.par, fullAnnotationDB.db, Some(stats))
        if (stats != Evaluate.empty) Metrics.report(stats, resultsStream)
        resultsStream.println(s"Positive: $positiveFound/$actualPositive Negative: $negativeFound/$actualNegative\n")
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
