package lomrf.mln.grounding

import lomrf.logic.{Literal, KBParser, AtomSignature}
import lomrf.util.{AtomIdentityFunction, ConstantsSet}
import org.scalatest.{FunSpec, Matchers}
import scala.collection.breakOut


/**
 * @author Anastasios Skarlatidis
 */
class GroundingSpecTest extends FunSpec with Matchers {

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
    AtomSignature("walking", 1) -> ("event", Vector("id")),
    AtomSignature("running", 1) -> ("event", Vector("id")),
    AtomSignature("active", 1) -> ("event", Vector("id")),
    AtomSignature("inactive", 1) -> ("event", Vector("id")),
    AtomSignature("exit", 1) -> ("event", Vector("id")),
    AtomSignature("move", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("meet", 2) -> ("fluent", Vector("id", "id"))
  )

  val dynamicAtoms = Map[AtomSignature, Vector[String] => Boolean]()

  val identityFunctions =  Map[AtomSignature, AtomIdentityFunction]()

  private val parser = new KBParser(predicateSchema, functionsSchema)



  val formulaStr = "Next(t,tNext) ^ HoldsAt(f,t) ^ !TerminatedAt(f,t) => HoldsAt(f,tNext)."

  describe("Formula '"+formulaStr+"'"){
    val theory = parser.parseFormula(formulaStr)
    val clauses = theory.toCNF(constants)

    it("produces a single clause"){
      clauses.size should be (1)
    }

    val clause = clauses.head

    it("contains three variables"){
      clause.variables.size should be (3)
    }


    /**
     * A utility Map that associates AtomSignatures with AtomIdentityFunction (= Bijection of ground atoms to integers).
     * This Map contains information only for ordinary atoms (not dynamic atoms).
     */
    val identities: Map[AtomSignature, AtomIdentityFunction] =
      (for (literal <- clause.literals if !dynamicAtoms.contains(literal.sentence.signature))
      yield literal.sentence.signature -> identityFunctions(literal.sentence.signature))(breakOut)


   /* val orderedLiterals: Array[(Literal, AtomIdentityFunction)] = clause
      .literals
      .view
      .map(lit => (lit, identities.getOrElse(lit.sentence.signature, null)))
      .toArray
      .sortBy(entry => entry._1)(ClauseLiteralsOrdering(mln))*/





  }





}
