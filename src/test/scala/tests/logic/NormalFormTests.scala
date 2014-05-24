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

package tests.logic

import org.scalatest.junit.AssertionsForJUnit
import org.junit.{Before, Test}
import java.io.File
import org.junit.Assert._
import lomrf.logic._
import lomrf.mln.model.MLN
import lomrf.util.{Logging, ConstantsSet}

/**
 * @author Anastasios Skarlatidis
 */

class NormalFormTests extends AssertionsForJUnit with Logging {

  private val sep = System.getProperty("file.separator")
  private val testFilesPath = System.getProperty("user.dir") + sep + "data" + sep + "tests" + sep

  private val constants = Map[String, ConstantsSet](
    "time" -> ConstantsSet("1", "2", "3", "4"),
    "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
    "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
    "dist" -> ConstantsSet("24", "30", "35"),
    "id" -> ConstantsSet("ID1", "ID2", "ID3"),

    "a" -> ConstantsSet((1 to 4).map(_.toString): _*),
    "b" -> ConstantsSet("B1", "B2")
  )

  private val predicateSchema = Map[AtomSignature, List[String]](
    AtomSignature("InitiatedAt", 2) -> List("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> List("fluent", "time"),
    AtomSignature("Initiates", 3) -> List("event", "fluent", "time"),
    AtomSignature("Terminates", 3) -> List("event", "fluent", "time"),
    AtomSignature("Happens", 2) -> List("event", "time"),
    AtomSignature("HoldsAt", 2) -> List("fluent", "time"),
    AtomSignature("Next", 2) -> List("time", "time"),
    AtomSignature("Close", 2) -> List("dist", "time"),
    AtomSignature("Close", 4) -> List("id", "id", "dist", "time"),
    AtomSignature("OrientationMove", 1) -> List("time"),
    AtomSignature("StartedAt", 3) -> List("event", "fluent", "time"),
    AtomSignature("StoppedAt", 3) -> List("event", "fluent", "time"),

    AtomSignature("Predicate1", 1) -> List("a"),
    AtomSignature("Predicate2", 2) -> List("a", "b"),
    AtomSignature("Predicate3", 1) -> List("b")
  )

  private val functionsSchema = Map[AtomSignature, (String, List[String])](
    AtomSignature("walking", 1) ->("event", List("id")),
    AtomSignature("abrupt", 1) ->("event", List("id")),
    AtomSignature("running", 1) ->("event", List("id")),
    AtomSignature("active", 1) ->("event", List("id")),
    AtomSignature("inactive", 1) ->("event", List("id")),
    AtomSignature("exit", 1) ->("event", List("id")),
    AtomSignature("fight", 2) ->("fluent", List("id", "id")),
    AtomSignature("move", 2) ->("fluent", List("id", "id")),
    AtomSignature("meet", 2) ->("fluent", List("id", "id")),
    AtomSignature("leaving_object", 2) ->("fluent", List("id", "id"))
  )


  private val kbParser = new ExtendedKBParser(predicateSchema, functionsSchema)

  @Before def initialize() {
    info("Checking if the required files are available")
    val testExistential = new File(testFilesPath + "TestExistential.mln")
    assertTrue("Cannot find file" + testExistential.toString, testExistential.exists)
  }


  @Test def validateCNF0() {
    import kbParser.{parseFormula, parseLiteral}
    val formula = parseFormula("InitiatedAt(Fight,t) => Happens(Abrupt, t).")

    val clauses = NormalForm.toCNF(constants, formula)
    assertEquals(1, clauses.size)

    val literals = clauses.head.literals
    assertEquals(2, literals.size)
    assertTrue(literals.contains(parseLiteral("!InitiatedAt(Fight,t)")))
    assertTrue(literals.contains(parseLiteral("Happens(Abrupt, t)")))

  }

  @Test def validateCNF1() {
    val formula = kbParser.parseFormula("InitiatedAt(Fight,t) <=> Happens(Abrupt, t).")
    val clauses = NormalForm.toCNF(constants, formula)
    clauses.foreach(clause => info(clause.toString))
    assertEquals(2, clauses.size)

    assertContainsClause(clauses, "{!InitiatedAt(Fight,t) | Happens(Abrupt, t)}")
    assertContainsClause(clauses, "{InitiatedAt(Fight,t) | !Happens(Abrupt, t)}")

  }

  @Test def validateCNF2() {
    import kbParser.parseFormula

    val f0 = parseFormula("InitiatedAt(Fight,t) <=> Happens(Abrupt, t) ^ Close(10,t).")
    val clauses0 = f0.toCNF(constants)
    assertEquals(3, clauses0.size)
    assertContainsClause(clauses0, "{InitiatedAt(Fight, t) | !Happens(Abrupt, t) | !Close(10,t)}")
    assertContainsClause(clauses0, "{Happens(Abrupt,t) | !InitiatedAt(Fight, t)}")
    assertContainsClause(clauses0, "{Close(10,t) | !InitiatedAt(Fight, t)}")
    val f1 = parseFormula("InitiatedAt(Fight,t) => Happens(Abrupt, t) ^ Close(10,t).")
    val f2 = parseFormula("Happens(Abrupt, t) ^ Close(10,t) => InitiatedAt(Fight,t).")
    val clauses_1and2 = f1.toCNF(constants) ++ f2.toCNF(constants)
    assertEquals(3, clauses_1and2.size)
    assertContainsClause(clauses_1and2, "{InitiatedAt(Fight, t) | !Happens(Abrupt, t) | !Close(10,t)}")
    assertContainsClause(clauses_1and2, "{Happens(Abrupt,t) | !InitiatedAt(Fight, t)}")
    assertContainsClause(clauses_1and2, "{Close(10,t) | !InitiatedAt(Fight, t)}")
    assertTrue(clauses_1and2.forall(c => clauses0.contains(c)))
  }

  @Test def validateExistentialExample() {
    import kbParser.parseFormula

    // formula (1)
    println("\n\nformula (1): (Exist a Predicate1(a) ^ Predicate2(a,b)) => Predicate3(b).")
    println("CNF:")
    val f1 = parseFormula("(Exist a Predicate1(a) ^ Predicate2(a,b)) => Predicate3(b).")
    val clauses1 = f1.toCNF(constants)
    clauses1.foreach(clause => println(clause.toString))

    // formula (2)
    println("formula (2): Predicate1(a) ^ Predicate2(a,b) => Predicate3(b).")
    println("CNF:")
    val f2 = parseFormula("Predicate1(a) ^ Predicate2(a,b) => Predicate3(b).")
    val clauses2 = f2.toCNF(constants)
    clauses2.foreach(clause => println(clause.toString))

    // formula (3)
    println("\n\nformula (3): (Exist a Predicate1(a) ^ Predicate2(a,b) => Predicate3(b)).")
    println("CNF:")
    val f3 = parseFormula("(Exist a Predicate1(a) ^ Predicate2(a,b) => Predicate3(b)).")
    val clauses3 = f3.toCNF(constants)
    clauses3.foreach(clause => println(clause.toString))

    // formula (4)
    println("\n\nformula (4): Predicate3(b) => Exist a Predicate1(a) ^ Predicate2(a,b).")
    println("CNF:")
    val f4 = parseFormula("Predicate3(b) => (Exist a Predicate1(a) ^ Predicate2(a,b)).")
    val clauses4 = f4.toCNF(constants)
    clauses4.foreach(clause => println(clause.toString))
  }

  @Test def validateCNF3() {
    import kbParser.parseFormula
    val f0 = parseFormula("1 Happens(Abrupt, t) ^ Close(24,t) ^ !Happens(Inactive,t ) => InitiatedAt(Fight, t)")
    val f1 = parseFormula("3 InitiatedAt(Fight,t) => Happens(Abrupt, t) ^ !Happens(Inactive, t) ^ Close(24,t)")
    val f3 = parseFormula("4 Happens(Abrupt, t) ^ Close(24,t) ^ !Happens(Inactive,t ) <=> InitiatedAt(Fight, t)")
    val clauses01 = f0.toCNF(constants) ++ f1.toCNF(constants)
    val clauses3 = f3.toCNF(constants)

    assertEquals(4, clauses01.size)
    assertEquals(4, clauses3.size)

    assertTrue(clauses01.forall(c => clauses3.contains(c)))
  }


  @Test def validateCNF4a() {
    import kbParser.parseFormula

    val constants = Map[String, ConstantsSet](
      "time" -> ConstantsSet("1", "2", "3", "4"),
      "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
      "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
      "dist" -> ConstantsSet("24", "30", "35")
    )
    info("Will tranform the Discrete Event Calculus (ver.1) axioms in Conjunctional Normal Form")

    val dec1 = parseFormula("Next(t1,t0) ^ Happens(e,t0) ^ Initiates(e,f,t0) => HoldsAt(f,t1).")
    val dec2 = parseFormula("Next(t1,t0) ^ Happens(e,t0) ^ Terminates(e,f,t0) => !HoldsAt(f,t1).")
    val dec3 = parseFormula("Next(t1,t0) ^ HoldsAt(f,t0) ^ !(Exist e Happens(e,t0) ^ Terminates(e,f,t0) ) => HoldsAt(f,t1).")
    val dec4 = parseFormula("Next(t1,t0) ^ !HoldsAt(f,t0) ^ !(Exist e Happens(e,t0) ^ Initiates(e,f,t0) ) => !HoldsAt(f,t1).")
    info( """
    DEC formulas:
    DEC1: """ + dec1.toText + """
    DEC2: """ + dec2.toText + """
    DEC3: """ + dec3.toText + """
    DEC4: """ + dec4.toText)
    info("Constants: \n" + constants)

    val clauses1 = dec1.toCNF(constants)
    assertEquals(1, clauses1.size)
    clInfo("DEC1", clauses1)

    val clauses2 = dec2.toCNF(constants)
    assertEquals(1, clauses2.size)
    clInfo("DEC2", clauses2)

    val clauses3 = dec3.toCNF(constants)
    assertEquals(64, clauses3.size)
    clInfo("DEC3", clauses3)

    val clauses4 = dec4.toCNF(constants)
    assertEquals(64, clauses4.size)
    clInfo("DEC4", clauses4)

  }


  @Test def validateGV() {
    import kbParser.parseFormula

    val constants = Map[String, ConstantsSet](
      "time" -> ConstantsSet("1", "2", "3", "4"),
      "event" -> ConstantsSet("E1", "E2"), // "E3"), //, "E4", "E5", "E6"),
      "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
      "dist" -> ConstantsSet("24", "30", "35")
    )
    val dec3 = parseFormula("Next(t1,t0) ^ HoldsAt(f,t0) ^ !(Exist e Happens(e,t0) ^ Terminates(e,f,t0) ) => HoldsAt(f,t1).")
    info(dec3.toText)

    /*val clauses3 = dec3.toCNF(constants)
    assertEquals(math.pow(2,3).toInt, clauses3.size)
    clInfo("DEC3",clauses3)*/


    val step1 = NormalForm.removeImplications(dec3)
    info("Step1: \n" + step1.toText)
    val step2 = NormalForm.negationsIn(step1)
    info("Step2: \n" + step2.toText)
    val step3 = NormalForm.standardizeVariables(step2)
    info("Step3: \n" + step3.toText)
    val step4 = NormalForm.removeExistentialQuantifiers(constants, step3)
    info("Step4: \n" + step4.toText)
    val step5 = NormalForm.removeUniversalQuantifiers(step4)
    info("Step5: \n" + step5.toText)
    val step6 = NormalForm.distribute(step5)
    info("Step6:" + step6.toText)
    val step7 = NormalForm.extractClauses(step6)
    info("Step7, final clauses:")
    step7.foreach(c => info(c.toString))
  }

  @Test def validateCNF4b() {
    import kbParser.parseFormula

    val constants = Map[String, ConstantsSet](
      "time" -> ConstantsSet("1", "2", "3", "4"),
      "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
      "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
      "dist" -> ConstantsSet("24", "30", "35")
    )

    info("Will tranform the Discrete Event Calculus (ver.2) axioms in Conjunctional Normal Form")

    val dec1 = parseFormula("Happens(e,t0) ^ Initiates(e,f,t0) => StartedAt(e,f,t0).")
    val dec2 = parseFormula("Happens(e,t0) ^ Terminates(e,f,t0) => StoppedAt(e,f,t0).")

    val dec3 = parseFormula("Next(t1,t0) ^ StartedAt(e,f,t0) => HoldsAt(f,t1).")
    val dec4 = parseFormula("Next(t1,t0) ^ StoppedAt(e,f,t0) => !HoldsAt(f,t1).")

    val dec5 = parseFormula("Next(t1,t0) ^ HoldsAt(f,t0) ^ !(Exist e StoppedAt(e,f,t0) ) => HoldsAt(f,t1).")
    val dec6 = parseFormula("Next(t1,t0) ^ !HoldsAt(f,t0) ^ !(Exist e StartedAt(e,f,t0) ) => !HoldsAt(f,t1).")

    info( """
    DEC formulas:
    DEC1: """ + dec1.toText + """
    DEC2: """ + dec2.toText + """
    DEC3: """ + dec3.toText + """
    DEC4: """ + dec4.toText + """" +
    DEC5: """ + dec5.toText + """" +
    DEC6: """ + dec6.toText)
    info("Constants: \n" + constants)

    val clauses1 = dec1.toCNF(constants)
    assertEquals(1, clauses1.size)
    clInfo("DEC1", clauses1)

    val clauses2 = dec2.toCNF(constants)
    assertEquals(1, clauses2.size)
    clInfo("DEC2", clauses2)

    val clauses3 = dec3.toCNF(constants)
    assertEquals(1, clauses3.size)
    clInfo("DEC3", clauses3)

    val clauses4 = dec4.toCNF(constants)
    assertEquals(1, clauses4.size)
    clInfo("DEC4", clauses4)

    val clauses5 = dec5.toCNF(constants)
    assertEquals(1, clauses5.size)
    clInfo("DEC4", clauses5)

    val clauses6 = dec6.toCNF(constants)
    assertEquals(1, clauses6.size)
    clInfo("DEC4", clauses6)

  }

  @Test def validateCNF4c() {
    import kbParser.parseFormula

    val constants = Map[String, ConstantsSet](
      "time" -> ConstantsSet("1", "2", "3", "4"),
      "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
      "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
      "dist" -> ConstantsSet("24", "30", "35")
    )
    info("Will tranform the Discrete Event Calculus (ver.3) axioms in Conjunctional Normal Form")

    val dec1 = parseFormula("Next(t1,t0) ^ InitiatedAt(f,t0) => HoldsAt(f,t1).")
    val dec2 = parseFormula("Next(t1,t0) ^ TerminatedAt(f,t0) => !HoldsAt(f,t1).")
    val dec3 = parseFormula("Next(t1,t0) ^ HoldsAt(f,t0) ^ !TerminatedAt(f,t0) => HoldsAt(f,t1).")
    val dec4 = parseFormula("Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(f,t0) => !HoldsAt(f,t1).")
    info( """
    DEC formulas:
    DEC1: """ + dec1.toText + """
    DEC2: """ + dec2.toText + """
    DEC3: """ + dec3.toText + """
    DEC4: """ + dec4.toText)
    info("Constants: \n" + constants)

    val clauses1 = dec1.toCNF(constants)
    assertEquals(1, clauses1.size)
    clInfo("DEC1", clauses1)

    val clauses2 = dec2.toCNF(constants)
    assertEquals(1, clauses2.size)
    clInfo("DEC2", clauses2)

    val clauses3 = dec3.toCNF(constants)
    assertEquals(1, clauses3.size)
    clInfo("DEC3", clauses3)

    val clauses4 = dec4.toCNF(constants)
    assertEquals(1, clauses4.size)
    clInfo("DEC4", clauses4)

  }

  @Test def validateCNF5() {
    import kbParser.parseFormula
    val f = parseFormula("Happens(walking(id1),t) => Initiates(walking(id1), move(id,id2), t).")
    val clauses = f.toCNF(constants)
    assertEquals(1, clauses.size)
    clInfo("Formula: " + f.toText, clauses)
  }

  @Test def validateCNF6() {
    import kbParser.parseFormula
    val f = parseFormula("Exist x,t Happens(walking(x), t).")
    val clauses = f.toCNF(constants)
    assertEquals(1, clauses.size)
    assertEquals(12, clauses.head.literals.size)
    clInfo("Formula: " + f.toText, clauses)
  }

  @Test def validateCNF7() {
    import kbParser.parseFormula

    val f0 = parseFormula("1 Happens(abrupt(p1), t) ^ Close(p1,p2,24,t) ^ !Happens(inactive(p2),t ) => InitiatedAt(fight(p1,p2), t)")
    val f1 = parseFormula("3 InitiatedAt(fight(p1,p2),t) => Happens(abrupt(p1), t) ^ !Happens(inactive(p2), t) ^ Close(p1,p2,24,t)")
    val f3 = parseFormula("4 Happens(abrupt(p1), t) ^ Close(p1,p2,24,t) ^ !Happens(inactive(p2),t ) <=> InitiatedAt(fight(p1,p2), t)")

    val clauses01 = f0.toCNF(constants) ++ f1.toCNF(constants)
    clInfo("Formulas: " + f0.toText + " and " + f1.toText, clauses01)
    val clauses3 = f3.toCNF(constants)
    clInfo("Formula: " + f3.toText, clauses3)

    assertEquals(4, clauses01.size)
    assertEquals(4, clauses3.size)

    assertTrue(clauses01.forall(c => clauses3.contains(c)))
  }

  @Test def loadDEC() {
    val q = Set(AtomSignature("HoldsAt", 2))
    val c: Set[AtomSignature] = Set(AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2))
    val o: Set[AtomSignature] = Set(AtomSignature("Initiates", 3), AtomSignature("Terminates", 3), AtomSignature("StartsAt", 3), AtomSignature("StopsAt", 3))


    info("Loading KB from " + testFilesPath + "DEC.mln")
    val mln = MLN(
      mlnFileName = testFilesPath + "DEC.mln",
      evidenceFileName = testFilesPath + "DEC.db",
      queryAtoms = q,
      cwa = c,
      owa = o)


    info("Found " + mln.formulas.size + " formulas")
    info("Found " + mln.constants.size + " constant types")
    info("Found " + mln.schema.size + " predicate schemas")
    info("Found " + mln.functionSchema.size + " function schemas")

    assertEquals(14, mln.formulas.size)
    assertEquals(5, mln.constants.size)
    assertEquals(8, mln.schema.size)
    assertEquals(11, mln.functionSchema.size)

    for {
      f <- mln.formulas
      clauses = f.toCNF(constants)} {
      assertTrue(clauses.size > 0)
      clInfo("Formula '" + f.toText + "'", clauses)
    }
  }

  @Test def testNNF() {
    info("Testing Negation Normal Form (NNF)")
    val src = "InitiatedAt(f,t) => !(Happens(e, t) ^ Exist x Close(x,t))"
    val result = "!InitiatedAt(f,t) v !Happens(e,t) v (Forall x !Close(x,t))"
    val formula = kbParser.parseFormula(src)
    info(src + " parsed as " + formula.toText)
    val nnf = NormalForm.toNNF(formula)
    info("NNF:= " + nnf.toText)
    assertTrue(result == nnf.toText)
  }

  @Test def testPNF() {
    info("Testing Prenex Normal Form (PNF)")
    val src = "InitiatedAt(f,t) => !(Happens(e, t) ^ Exist x Close(x,t))"
    val result = "(Forall x !InitiatedAt(f,t) v !Happens(e,t) v !Close(x,t))"
    val formula = kbParser.parseFormula(src)
    info(src + " parsed as " + formula.toText)
    val pnf = NormalForm.toPNF(formula)
    info("PNF:= " + pnf.toText)
    assertTrue(result == pnf.toText)
  }

  @Test def dynamicFunctions1() {
    import kbParser.parseFormula

    info("Testing dynamic function (succ(int):int)")
    val dec1 = parseFormula("InitiatedAt(f,t) => HoldsAt(f, succ(t)).")

    val strFunctions = dec1.functions.map(_.toText).reduceLeft((f, rest) => f + "\n" + rest)
    info("The functions that appear in formula '" + dec1.toText + "', are:\n" + strFunctions)
    assertEquals(1, dec1.functions.size)
    val function = dec1.functions.head
    assertEquals("succ", function.symbol)
    assertEquals("time", function.domain)
    assertEquals(1, function.args.size)
    val arg: Variable = function.args.head match {
      case v: Variable => v
      case _ => fatal("The argument of function " + function + " should be a variable.")
    }
    assertEquals("time", arg.domain)

  }

  @Test def dynamicPredicates1() {
    import kbParser.parseFormula

    val formula1 = parseFormula("Happens(e,t) ^ Happens(e,t1) ^ equals(t,t1) => InitiatedAt(f,t1)")
    //println(formula1.toString)
    println(formula1.toText)
    println(formula1.toCNF(constants))

    val formula2 = parseFormula("Happens(e,t) ^ Happens(e,t1) ^ t = t1 => InitiatedAt(f,t1)")
    //println(formula2.toString)
    println(formula2.toText)
    println(formula2.toCNF(constants))
  }




  private def assertContainsClause(clauses: Iterable[Clause], src: String) {
    val txtLiterals = src.replace("{", " ").replace("}", " ").split('|')
    var literals = Set[Literal]()
    for (lit <- txtLiterals) literals += kbParser.parseLiteral(lit)
    assertTrue {
      clauses.find(c => c.literals == literals) match {
        case Some(_) => true
        case _ => false
      }
    }
  }

  private def clInfo(decName: String, clauses: Iterable[Clause]) {
    info(decName + " produced the following " + clauses.size + " clauses:\n"
      + clauses.foldRight("")((clause, txt) => clause.toString + "\n" + txt))
  }
}