package lomrf.mln.grounding

import lomrf.logic.{KBParser, AtomSignature, Clause}
import lomrf.util.ConstantsSet

/**
 * @author Anastasios Skarlatidis
 */
object ExpGrounding {

  private val constants = Map[String, ConstantsSet](
    "time" -> ConstantsSet("1", "2", "3", "4"),
    "event" -> ConstantsSet("Walking", "Running", "Active", "Inactive", "Exit", "Enter"),
    "fluent" -> ConstantsSet("Move", "Meet"),
    "dist" -> ConstantsSet("24", "30", "35"),
    "id" -> ConstantsSet("ID1", "ID2", "ID3")
  )

  private val predicateSchema = Map[AtomSignature, Vector[String]](
    AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Next", 2) -> Vector("time", "time"),
    AtomSignature("Close", 2) -> Vector("dist", "time"),
    AtomSignature("OrientationMove", 1) -> Vector("time")
  )

  private val functionsSchema = Map[AtomSignature, (String, Vector[String])](
    AtomSignature("walking", 1) ->("event", Vector("id")),
    AtomSignature("running", 1) ->("event", Vector("id")),
    AtomSignature("active", 1) ->("event", Vector("id")),
    AtomSignature("inactive", 1) ->("event", Vector("id")),
    AtomSignature("exit", 1) ->("event", Vector("id")),
    AtomSignature("move", 2) ->("fluent", Vector("id", "id")),
    AtomSignature("meet", 2) ->("fluent", Vector("id", "id"))
  )

  private val parser = new KBParser(predicateSchema, functionsSchema)

  def main (args: Array[String]): Unit ={

    val theory = parser.parseFormula("Happens(Walking, t) => InitiatedAt(Moving,t)")
    val clause = theory.toCNF(constants).head



  }


}
