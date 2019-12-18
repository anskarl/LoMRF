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
import lomrf.mln.learning.supervision.graph._
import lomrf.mln.learning.supervision.metric._
import lomrf.mln.model.{ AtomEvidenceDB, Evidence, KB, MLN }
import lomrf.util.NaturalComparator
import lomrf.util.evaluation.{ Evaluate, Metrics }
import lomrf.util.time._
import lomrf.util.logging.Implicits._
import scala.io.Source
import java.io.{ File, FileOutputStream, PrintStream }
import lomrf.app.ConnectorStrategy._
import lomrf.app.GraphSolverType._
import lomrf.logic.compile.NormalForm
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

  // By default do not compress results
  private var _compressResults: Boolean = false

  // By default run using harmonic graph cut
  private var _solver: GraphSolverType = GraphSolverType.HFC_SPLICE

  // By default run using atomic distance
  private var _distance: DistanceType = DistanceType.Atomic

  // By default run using a kNN connector
  private var _connector: ConnectorStrategy = ConnectorStrategy.kNN

  // By default run using a simple cache filter
  private var _cacheFilter: CacheFilter = CacheFilter.Simple

  // By default do no cluster identical unlabeled nodes
  private var _clusterUnlabeled: Boolean = false

  // By default do not perform instance and feature selection
  private var _selection: Boolean = false

  // Epsilon threshold for the eNN graph
  private var _epsilon = 0.75

  // K value for the kNN graph
  private var _k = 2

  // Number of trees for mass metric
  private var _trees = 100

  // Alpha parameter for LGC solver
  private var _alpha = 0.01

  // Maximum density for instance selection
  private var _maxDensity = 1.0

  // Memory for streaming synopsis
  private var _memory = 2

  // Minimum node size
  private var _minNodeSize = 2

  // Minimum node occurrence
  private var _minOccSize = 1

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

  opt("s", "solver", "<nn | ext.nn | lp.[splice,tlp] | hgc.[splice,tlp] | lgc.[splice,tlp]>",
    "Specify a solver for completion (default is hgc).", {
      v: String =>
        v.trim.toLowerCase match {
          case "nn"         => _solver = NN
          case "ext.nn"     => _solver = EXT_NN
          case "lp.splice"  => _solver = LP_SPLICE
          case "hfc.splice" => _solver = HFC_SPLICE
          case "lgc.splice" => _solver = LGC_SPLICE
          case "lp.tlp"     => _solver = LP_TLP
          case "hfc.tlp"    => _solver = HFC_TLP
          case "lgc.tlp"    => _solver = LGC_TLP
          case _            => logger.fatal(s"Unknown solver of type '$v'.")
        }
    })

  opt("d", "distance", "<binary | atomic | evidence | mass | hybrid.tree>",
    "Specify a distance over atoms (default is atomic).", {
      v: String =>
        v.trim.toLowerCase match {
          case "binary"   => _distance = DistanceType.Binary
          case "atomic"   => _distance = DistanceType.Atomic
          case "evidence" => _distance = DistanceType.Evidence
          case "mass"     => _distance = DistanceType.Mass
          case "hybrid"   => _distance = DistanceType.Hybrid
          case _          => logger.fatal(s"Unknown distance of type '$v'.")
        }
    }
  )

  opt("c", "connector", "<aNN | aNN.labeled | aNN.temporal | kNN | kNN.labeled | kNN.temporal | eNN | eNN.labeled | eNN.temporal | full>",
    "Specify a connection strategy for the graph (default is kNN).", {
      v: String =>
        v.trim.toLowerCase match {
          case "ann"          => _connector = aNN
          case "ann.labeled"  => _connector = aNNLabeled
          case "ann.temporal" => _connector = aNNTemporal
          case "knn"          => _connector = kNN
          case "knn.labeled"  => _connector = kNNLabeled
          case "knn.temporal" => _connector = kNNTemporal
          case "enn"          => _connector = eNN
          case "enn.labeled"  => _connector = eNNLabeled
          case "enn.temporal" => _connector = eNNTemporal
          case "full"         => _connector = Full
          case _              => logger.fatal(s"Unknown connector of type '$v'.")
        }
    }
  )

  opt("cf", "cache-filter", "<simple | hoeffding>",
    "Specify a cache filter for contradicting examples (default is simple).", {
      v: String =>
        v.trim.toLowerCase match {
          case "simple"    => _cacheFilter = CacheFilter.Simple
          case "hoeffding" => _cacheFilter = CacheFilter.Hoeffding
          case _           => logger.fatal(s"Unknown distance of type '$v'.")
        }
    }
  )

  intOpt("k", "kappa", "Kappa parameter for the kNN connector (default is " + _k + ")", {
    v: Int => if (v < 1) logger.fatal("k value must be any integer greater than zero, but you gave: " + v) else _k = v
  })

  doubleOpt("e", "epsilon", "Epsilon parameter for eNN connector (default is " + _epsilon + ").", {
    v: Double =>
      if (v < 0 || v > 1) logger.fatal("Epsilon value must be any number greater or equal to zero and less or equal to one, but you gave: " + v)
      else _epsilon = v
  })

  doubleOpt("p", "alpha", "Alpha parameter for local and global consistency (default is " + _alpha + ").", {
    v: Double =>
      if (v < 0 || v > 1) logger.fatal("Alpha value must be any number greater or equal to zero and less or equal to one, but you gave: " + v)
      else _alpha = v
  })

  doubleOpt("md", "max-density", "Maximum density parameter for instance selection (default is " + _maxDensity + ").", {
    v: Double =>
      if (v < 0 || v > 1) logger.fatal("Alpha value must be any number greater or equal to zero and less or equal to one, but you gave: " + v)
      else _maxDensity = v
  })

  flagOpt("sn", "skip-negatives", "Do not output negative labels into the resulted files.", {
    _outputNegatives = false
  })

  flagOpt("cr", "compressed-results", "Output all results in a single file.", {
    _compressResults = true
  })

  flagOpt("cu", "cluster-unlabeled", "Perform clustering on identical unlabeled nodes.", {
    _clusterUnlabeled = true
  })

  flagOpt("fs", "feature-selection", "Perform instance and feature selection.", {
    _selection = true
  })

  intOpt("trees", "isolation-trees", "Number of isolation trees (default is " + _trees + ")", {
    v: Int =>
      if (v < 1) logger.fatal("Number of isolation trees value must be any integer greater than one, but you gave: " + v)
      else _trees = v
  })

  intOpt("ms", "min-node-size", "Minimum node size (default is " + _minNodeSize + ")", {
    v: Int =>
      if (v < 1) logger.fatal("Minimum node size value must be any integer greater than one, but you gave: " + v)
      else _minNodeSize = v
  })

  intOpt("mo", "min-node-occur", "Minimum node occurrence (default is " + _minOccSize + ")", {
    v: Int =>
      if (v < 1) logger.fatal("Minimum node occurrence value must be any integer greater than one, but you gave: " + v)
      else _minOccSize = v
  })

  intOpt("mem", "memory", "Memory parameter (default is " + _memory + ")", {
    v: Int =>
      if (v < 1) logger.fatal("memory value must be any integer greater than one, but you gave: " + v)
      else _memory = v
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
      + "\n\t(solver) Graph solver for completion: " + _solver
      + "\n\t(distance) Distance metric for atomic formula: " + _distance
      + "\n\t(connector) Graph connection heuristic: " + _connector
      + "\n\t(cache-filter) Cache filter: " + _cacheFilter
      + "\n\t(kappa) k parameter for the kNN connector: " + _k
      + "\n\t(epsilon) Epsilon parameter for the eNN connector: " + _epsilon
      + "\n\t(negatives) Output negative labels: " + _outputNegatives
      + "\n\t(compress-results) Output all results in a single file: " + _compressResults
      + "\n\t(cluster-unlabeled) Cluster identical unlabelled nodes: " + _clusterUnlabeled
      + "\n\t(feature-selection) Perform instance and feature selection: " + _selection
      + "\n\t(isolation-trees) Number of isolation trees for mass metric: " + _trees
      + "\n\t(alpha) Alpha parameter for LGC solver: " + _alpha
      + "\n\t(max-density) Maximum density parameter for instance selection: " + _maxDensity
      + "\n\t(min-node-size) Minimum node size: " + _minNodeSize
      + "\n\t(min-node-occ) Minimum node occurrence: " + _minOccSize
      + "\n\t(memory) Streaming memory: " + _memory)

    // Init all statistics values to zero
    var actualPositive, actualNegative, positiveFound, negativeFound = 0
    var supervisionGraphs = Map.empty[AtomSignature, SupervisionGraph]
    var stats = Evaluate.empty

    // Create a knowledge base and convert all functions
    val (kb, constants) = KB.fromFile(strMLNFileName, convertFunctions = true)
    val clauses = NormalForm.compileCNF(kb.formulas)(constants)

    val connector =
      if (_connector == kNN) kNNConnector(_k)
      else if (_connector == kNNLabeled) kNNLConnector(_k)
      else if (_connector == kNNTemporal) new kNNTemporalConnector(_k)
      else if (_connector == eNN) eNNConnector(_epsilon)
      else if (_connector == eNNLabeled) eNNLConnector(_epsilon)
      else if (_connector == eNNTemporal) new eNNTemporalConnector(_epsilon)
      else if (_connector == aNN) aNNConnector
      else if (_connector == aNNLabeled) new aNNLConnector
      else if (_connector == aNNTemporal) new aNNTemporalConnector
      else FullConnector

    // Keep only signatures having positive recall
    val signatures = kb.predicateSchema.keySet.filter(sig => modes(sig).recall > 0)

    val distance: Metric[_ <: AtomicFormula] =
      if (_distance == DistanceType.Hybrid) HybridMetric(AtomMetric(HungarianMatcher), MassMetric(signatures, modes, _trees))
      else if (_distance == DistanceType.Mass) MassMetric(signatures, modes, _trees)
      else if (_distance == DistanceType.Binary) BinaryMetric(HungarianMatcher)
      else if (_distance == DistanceType.Atomic) AtomMetric(HungarianMatcher)
      else EvidenceMetric(modes, HungarianMatcher)

    val useHoeffding = if (_cacheFilter == CacheFilter.Simple) false else true

    val defaultName = {
      val builder = StringBuilder.newBuilder

      builder ++= s"$connector"
      if (_clusterUnlabeled && (_solver != HFC_TLP &&
        _solver != LGC_TLP &&
        _connector != kNNTemporal &&
        _connector != eNNTemporal &&
        _connector != aNNTemporal)) builder ++= "[clustered]"

      builder ++= s".cf[${_cacheFilter}:${_minNodeSize}:${_minOccSize}]"

      if (_selection) builder ++= s".fs[${_maxDensity}]"

      if (_distance == DistanceType.Mass || _distance == DistanceType.Hybrid) builder ++= s".${_distance}[${_trees}]"
      else builder ++= s".${_distance}"

      if (_solver == LGC_TLP || _solver == LGC_SPLICE) builder ++= s".${_solver}[${_alpha}]"
      else builder ++= s".${_solver}"

      if (_solver == HFC_TLP || _solver == LGC_TLP) builder ++= s".mem[${_memory}]"

      builder.result
    }

    val resultName = _resultsFileName match {
      case Some(path) => new File(path).getName.reverse.dropWhile(_ != '.').tail.reverse
      case None       => defaultName
    }

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
      val mln = MLN(kb.schema, evidence, _nonEvidenceAtoms, clauses.toVector)

      // Create or update supervision graphs for each given non evidence atom
      _nonEvidenceAtoms.foreach { querySignature =>
        supervisionGraphs.get(querySignature) match {
          case Some(graph) => supervisionGraphs += querySignature -> (graph ++ (mln, annotationDB, modes))
          case None if _solver == LP_TLP =>
            supervisionGraphs += querySignature -> SupervisionGraph.TLP(
              mln, modes, annotationDB, querySignature, connector, distance, LP(), useHoeffding, _memory, _minNodeSize, _minOccSize)
          case None if _solver == HFC_TLP =>
            supervisionGraphs += querySignature -> SupervisionGraph.TLP(
              mln, modes, annotationDB, querySignature, connector, distance, new HFc, useHoeffding, _memory, _minNodeSize, _minOccSize)
          case None if _solver == LGC_TLP =>
            supervisionGraphs += querySignature -> SupervisionGraph.TLP(
              mln, modes, annotationDB, querySignature, connector, distance, LGCc(alpha = _alpha), useHoeffding, _memory, _minNodeSize, _minOccSize)
          case None if _solver == LP_SPLICE =>
            supervisionGraphs += querySignature -> SupervisionGraph.SPLICE(
              mln, modes, annotationDB, querySignature, connector, distance, LP(), _clusterUnlabeled, _selection, useHoeffding, _minNodeSize, _minOccSize)
          case None if _solver == HFC_SPLICE =>
            supervisionGraphs += querySignature -> SupervisionGraph.SPLICE(
              mln, modes, annotationDB, querySignature, connector, distance, new HFc, _clusterUnlabeled, _selection, useHoeffding, _minNodeSize, _minOccSize)
          case None if _solver == LGC_SPLICE =>
            supervisionGraphs += querySignature -> SupervisionGraph.SPLICE(
              mln, modes, annotationDB, querySignature, connector, distance, LGCc(alpha = _alpha), _clusterUnlabeled, _selection, useHoeffding, _minNodeSize, _minOccSize)
          case None if _solver == NN =>
            supervisionGraphs += querySignature -> SupervisionGraph.nearestNeighbor(
              mln, modes, annotationDB, querySignature, connector, distance, _clusterUnlabeled, useHoeffding, _minNodeSize, _minOccSize)
          case None if _solver == EXT_NN =>
            supervisionGraphs += querySignature -> SupervisionGraph.extNearestNeighbor(
              mln, modes, annotationDB, querySignature, connector, distance, _clusterUnlabeled, useHoeffding, _minNodeSize, _minOccSize)
        }
      }

      // Run supervision completion for all given non evidence atoms and collect the results
      val (completedEvidenceAtoms, completedEvidenceSet) = supervisionGraphs.values
        .map(graph => graph.completeSupervision())
        .foldLeft(Set.empty[EvidenceAtom] -> Set.empty[Evidence]) {
          case ((atoms, evidenceSet), tuple) => (atoms ++ tuple._1, evidenceSet + tuple._2)
        }

      logger.info(msecTimeToTextUntilNow("Supervision completion time until now: ", start))

        @inline
        def outputCompletedResults(output: PrintStream): Unit = {

          output.println("\n// Completed supervision")

          completedEvidenceSet.map(_.db).foreach { evidenceDB =>
            evidenceDB.foreach {
              case (signature, atomDB) =>

                output.println(s"// Positives for $signature")
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
                }.sortWith(NaturalComparator.compareBool).foreach(output.println)

                if (_outputNegatives) {
                  output.println(s"// Negatives for $signature")

                  atomIDF.indices.flatMap { id =>
                    atomIDF.decode(id) match {
                      case Success(terms) if atomDB(id) == FALSE =>
                        Some(s"!${signature.symbol}(${terms.mkString(",")})")
                      case Failure(exception) => throw exception
                      case _                  => None
                    }
                  }.sortWith(NaturalComparator.compareBool).foreach(output.println)
                }
            }
          }
          output.close()
        }

      /*
       * OK, lets store the resulted completed supervision
       */
      if (_compressResults) {
        val compressedOutput =
          if (step < 1) new PrintStream(s"${currentTrainingFile.getParentFile.getParent}/$resultName.db")
          else new PrintStream(new FileOutputStream(s"${currentTrainingFile.getParentFile.getParent}/$resultName.db", true))

        compressedOutput.println {
          s"""
             |Step ${step + 1} / ${strTrainingFileNames.length}:
             |Chunk ${currentTrainingFile.getName}
           """.stripMargin
        }
        outputCompletedResults(compressedOutput)
      } else {
        val resultsFolder = new File(s"${currentTrainingFile.getParentFile.getParent}/$resultName")
        resultsFolder.mkdirs

        val completedBatch = new PrintStream(resultsFolder.getCanonicalPath + "/" + currentTrainingFile.getName)

        val inputStream = Source.fromFile(currentTrainingFile)
        inputStream.getLines.filter { line =>
          _nonEvidenceAtoms.map(_.symbol).forall(!line.contains(_)) && line.nonEmpty
        }.foreach(completedBatch.println)
        inputStream.close()

        outputCompletedResults(completedBatch)
      }

      // In case annotation files are given, output statistics
      if (strAnnotationFileNames.nonEmpty) {

        val currentAnnotationFile = new File(strAnnotationFileNames(step))

        if (resultsStream != System.out) resultsStream.println {
          s"""
             |Step ${step + 1} / ${strTrainingFileNames.length}:
             |Chunk ${currentTrainingFile.getName}
             |Annotation ${currentAnnotationFile.getName}
           """.stripMargin
        }

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
