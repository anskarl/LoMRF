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
}
