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
    case object HGC extends GraphSolverType("hgc")
    case object LGC extends GraphSolverType("lgc")
  }

  sealed class ConnectorType(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }

  object ConnectorType extends Enum[ConnectorType] {

    val values: immutable.IndexedSeq[ConnectorType] = findValues

    case object Full extends ConnectorType("full")
    case object Temporal extends ConnectorType("temporal")
    case object eNN extends ConnectorType("enn")
    case object kNN extends ConnectorType("kNN")
  }

  sealed class DistanceType(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }

  object DistanceType extends Enum[DistanceType] {

    val values: immutable.IndexedSeq[DistanceType] = findValues

    case object Binary extends DistanceType("binary")
    case object Atomic extends DistanceType("atomic")
    case object Structure extends DistanceType("structure")
  }
}
