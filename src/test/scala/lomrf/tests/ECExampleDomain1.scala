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

package lomrf.tests

import lomrf.logic.AtomSignature
import lomrf.logic.predef._
import lomrf.mln.model.{ConstantsSet, FunctionMapper}

/**
 * Simple function-free EC domain for testing
 */
object ECExampleDomain1 {

  val LAST_TIME_POINT = 10

  lazy val constants = Map[String, ConstantsSet](
    "time" -> ConstantsSet((1 to LAST_TIME_POINT).map(_.toString): _* ),
    "event" -> ConstantsSet("Walking", "Running", "Active", "Inactive", "Exit", "Enter"),
    "fluent" -> ConstantsSet("Move", "Meet")
  )

  lazy val predicateSchema = Map[AtomSignature, Vector[String]](
    AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Next", 2) -> Vector("time", "time")
  )

  lazy val functionsSchema =  Map.empty[AtomSignature, (String, Vector[String])] //Map(AtomSignature("next", 1) -> ("time", Vector("time")))

  lazy val dynamicAtoms = dynAtoms

  lazy val dynamicFunctions = dynFunctions

  lazy val functionMappers =  Map.empty[AtomSignature, FunctionMapper]

  lazy val queryAtoms = Set(AtomSignature("HoldsAt", 2))

  lazy val hiddenAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

  lazy val cwa = Set(AtomSignature("Next", 2), AtomSignature("Happens", 2))

  lazy val owa = Set(AtomSignature("HoldsAt", 2), AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))
  
  lazy val probabilisticAtoms = Set.empty[AtomSignature]

  lazy val tristateAtoms = Set.empty[AtomSignature]

}
