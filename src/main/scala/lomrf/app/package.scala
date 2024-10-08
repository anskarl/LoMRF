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

package lomrf

import enumeratum._
import scala.collection.immutable
import java.text.DecimalFormat

package object app {

  final val numFormat = new DecimalFormat("0.############")

  sealed class WeightsMode(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }

  object WeightsMode extends Enum[WeightsMode] {

    val values: immutable.IndexedSeq[WeightsMode] = findValues

    case object Keep extends WeightsMode("keep")
    case object Remove_Soft extends WeightsMode("remove soft")
    case object Remove_All extends WeightsMode("remove all")
  }

  sealed class GraphSolverType(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }

  object GraphSolverType extends Enum[GraphSolverType] {

    val values: immutable.IndexedSeq[GraphSolverType] = findValues

    case object NN extends GraphSolverType("nn")
    case object EXT_NN extends GraphSolverType("ext.nn")
    case object LP_SPLICE extends GraphSolverType("lp.splice")
    case object HFC_SPLICE extends GraphSolverType("hfc.splice")
    case object LGC_SPLICE extends GraphSolverType("lgc.splice")
    case object LP_TLP extends GraphSolverType("lp.tlp")
    case object HFC_TLP extends GraphSolverType("hfc.tlp")
    case object LGC_TLP extends GraphSolverType("lgc.tlp")
  }

  sealed class ConnectorStrategy(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }

  object ConnectorStrategy extends Enum[ConnectorStrategy] {

    val values: immutable.IndexedSeq[ConnectorStrategy] = findValues

    case object Full extends ConnectorStrategy("full")
    case object aNN extends ConnectorStrategy("aNN")
    case object aNNLabeled extends ConnectorStrategy("aNN.labeled")
    case object aNNTemporal extends ConnectorStrategy("aNN.temporal")
    case object eNN extends ConnectorStrategy("eNN")
    case object eNNLabeled extends ConnectorStrategy("eNN.labeled")
    case object eNNTemporal extends ConnectorStrategy("eNN.temporal")
    case object kNN extends ConnectorStrategy("kNN")
    case object kNNLabeled extends ConnectorStrategy("kNN.labeled")
    case object kNNTemporal extends ConnectorStrategy("kNN.temporal")
  }

  sealed class DistanceType(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }

  object DistanceType extends Enum[DistanceType] {

    val values: immutable.IndexedSeq[DistanceType] = findValues

    case object Binary extends DistanceType("binary")
    case object Atomic extends DistanceType("atomic")
    case object Evidence extends DistanceType("evidence")
    case object Mass extends DistanceType("mass")
    case object Hybrid extends DistanceType("hybrid")
  }

  sealed class CacheFilter(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }

  object CacheFilter extends Enum[CacheFilter] {

    val values: immutable.IndexedSeq[CacheFilter] = findValues

    case object Simple extends CacheFilter("simple")
    case object Hoeffding extends CacheFilter("hoeffding")
  }
}
