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
import org.junit.Assert._
import lomrf.logic._
import org.junit.Test
import scala.collection.mutable
import collection.mutable.LinkedHashSet
import lomrf.util.Logging

/**
 * @author Anastasios Skarlatidis
 */

class KBCompilationTests extends AssertionsForJUnit with Logging {


  /*private val constants = Map[String, ConstantsSet](
    "time" -> ConstantsSet("1", "2", "3", "4"),
    "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
    "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
    "dist" -> ConstantsSet("24", "30", "35"),
    "id" -> ConstantsSet("ID1", "ID2", "ID3")
  )*/

  private implicit val predicateSchema = Map[AtomSignature, List[String]](
    AtomSignature("InitiatedAt", 2) -> List("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> List("fluent", "time"),
    AtomSignature("Initiates", 3) -> List("event", "fluent", "time"),
    AtomSignature("Terminates", 3) -> List("event", "fluent", "time"),
    AtomSignature("Happens", 2) -> List("event", "time"),
    AtomSignature("HoldsAt", 2) -> List("fluent", "time"),
    AtomSignature("Next", 2) -> List("time", "time"),
    AtomSignature("Close", 2) -> List("dist", "time"),
    AtomSignature("Close", 4) -> List("id", "id", "dist", "time"),
    AtomSignature("OrientationMove", 3) -> List("id", "id", "time"),
    AtomSignature("StartTime", 1) -> List("time")
  )

  private implicit val functionsSchema = Map[AtomSignature, (String, List[String])](
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

  // Theory:
  val pec1 = "Next(t1,t0) ^ InitiatedAt(f, t0) => HoldsAt(f, t1)."
  val pec2 = "Next(t1,t0) ^ HoldsAt(f,t0) ^ !TerminatedAt(f, t0) => HoldsAt(f,t1)."
  val pec3 = "Next(t1,t0) ^ TerminatedAt(f, t0) => !HoldsAt(f, t1)."
  val pec4 = "Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(f, t0) => !HoldsAt(f,t1)."
  val constraint1 = "StartTime(t) => !HoldsAt(f, t)."
  val constraint2 = "!HoldsAt(meet(p,p), t)."
  val meet_init1 = "InitiatedAt(meet(p1,p2),  t) :- Happens(active(p1), t) ^ !Happens(running(p2), t) ^  Close(p1,p2,25,t)"
  val meet_init2 = "InitiatedAt(meet(p1,p2),  t) :- Happens(inactive(p1),t) ^ !Happens(running(p2),t) ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t)"
  val meet_term1 = "TerminatedAt(meet(p1,p2), t) :- Happens(walking(p1),t) ^ !Close(p1,p2,34,t)"
  val meet_term2 = "TerminatedAt(meet(p1,p2), t) :- Happens(walking(p2),t) ^ !Close(p2,p1,34,t)"
  val meet_term3 = "TerminatedAt(meet(p1,p2), t) :- Happens(running(p1),t)"
  val meet_term4 = "TerminatedAt(meet(p1,p2), t) :- Happens(running(p2),t)"
  val meet_term5 = "TerminatedAt(meet(p1,p2), t) :- Happens(exit(p1), t)."
  val meet_term6 = "TerminatedAt(meet(p1,p2), t) :- Happens(exit(p2), t)."

  val move_init1 = "InitiatedAt(move(p1,p2), t) :- Happens(walking(p1), t) ^ Happens(walking(p2), t) ^ OrientationMove(p1,p2,t) ^ Close(p1,p2,34,t)."
  val move_term1 = "TerminatedAt(move(p1,p2), t) :- Happens(exit(p1), t)."
  val move_term2 = "TerminatedAt(move(p1,p2), t) :- Happens(exit(p2), t)."


  @Test def naivePredicateCompletion() {
    val definiteClauses = Set(
      kbParser.parseDefiniteClause(meet_init1),
      kbParser.parseDefiniteClause(meet_init2),
      kbParser.parseDefiniteClause(meet_term1),
      kbParser.parseDefiniteClause(meet_term2),
      kbParser.parseDefiniteClause(meet_term3),
      kbParser.parseDefiniteClause(meet_term4),
      kbParser.parseDefiniteClause(meet_term5),
      kbParser.parseDefiniteClause(meet_term6)
    )
    /*val correctResult = mutable.Set(
      // Initiation rules
      "Happens(active(p1),t) ^ !Happens(running(p2),t) ^ Close(p1,p2,25,t) => InitiatedAt(meet(p1, p2),t)",
      "Happens(inactive(p1),t) ^ !Happens(running(p2),t) ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t) => InitiatedAt(meet(p1, p2),t)",
      "InitiatedAt(meet(p1, p2),t) => (Happens(active(p1),t) ^ !Happens(running(p2),t) ^ Close(p1,p2,25,t)) v (Happens(inactive(p1),t) ^ !Happens(running(p2),t) ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t)).",
      // Termination rules
      "Happens(walking(p1),t) ^ !Close(p1,p2,34,t) => TerminatedAt(meet(p1, p2),t)",
      "Happens(walking(p2),t) ^ !Close(p2,p1,34,t) => TerminatedAt(meet(p1, p2),t)",
      "Happens(running(p1),t) => TerminatedAt(meet(p1, p2),t)",
      "Happens(running(p2),t) => TerminatedAt(meet(p1, p2),t)",
      "Happens(exit(p1),t) => TerminatedAt(meet(p1, p2),t).",
      "Happens(exit(p2),t) => TerminatedAt(meet(p1, p2),t).",
      "TerminatedAt(meet(p1, p2),t) => Happens(exit(p2),t) v Happens(running(p1),t) v Happens(running(p2),t) v Happens(exit(p1),t) v (Happens(walking(p2),t) ^ !Close(p2,p1,34,t)) v (Happens(walking(p1),t) ^ !Close(p1,p2,34,t))."
    )*/

    val correctResult = kbParser.parseTheory(
      "Happens(active(p1),t) ^ !Happens(running(p2),t) ^ Close(p1,p2,25,t) => InitiatedAt(meet(p1, p2),t)\n" +
              "Happens(inactive(p1),t) ^ !Happens(running(p2),t) ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t) => InitiatedAt(meet(p1, p2),t)\n" +
              "InitiatedAt(meet(p1, p2),t) => (Happens(active(p1),t) ^ !Happens(running(p2),t) ^ Close(p1,p2,25,t)) v (Happens(inactive(p1),t) ^ !Happens(running(p2),t) ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t)).\n" +
              "Happens(walking(p1),t) ^ !Close(p1,p2,34,t) => TerminatedAt(meet(p1, p2),t)\n" +
              "Happens(walking(p2),t) ^ !Close(p2,p1,34,t) => TerminatedAt(meet(p1, p2),t)\n" +
              "Happens(running(p1),t) => TerminatedAt(meet(p1, p2),t)\n" +
              "Happens(running(p2),t) => TerminatedAt(meet(p1, p2),t)\n" +
              "Happens(exit(p1),t) => TerminatedAt(meet(p1, p2),t).\n" +
              "Happens(exit(p2),t) => TerminatedAt(meet(p1, p2),t).\n" +
              "TerminatedAt(meet(p1, p2),t) => Happens(exit(p2),t) v Happens(running(p1),t) v Happens(running(p2),t) v Happens(exit(p1),t) v (Happens(walking(p2),t) ^ !Close(p2,p1,34,t)) v (Happens(walking(p1),t) ^ !Close(p1,p2,34,t))."
    )

    //correctResult.foreach(f => println(f.toText))

    /*val correctResultFormulas = mutable.Set(
          // Initiation rules
          "Happens(active(p1),t) ^ !Happens(running(p2),t) ^ Close(p1,p2,25,t) => InitiatedAt(meet(p1, p2),t)",
          "Happens(inactive(p1),t) ^ !Happens(running(p2),t) ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t) => InitiatedAt(meet(p1, p2),t)",
          "InitiatedAt(meet(p1, p2),t) => (Happens(active(p1),t) ^ !Happens(running(p2),t) ^ Close(p1,p2,25,t)) v (Happens(inactive(p1),t) ^ !Happens(running(p2),t) ^ !Happens(active(p2),t) ^ Close(p1,p2,25,t)).",
          // Termination rules
          "Happens(walking(p1),t) ^ !Close(p1,p2,34,t) => TerminatedAt(meet(p1, p2),t)",
          "Happens(walking(p2),t) ^ !Close(p2,p1,34,t) => TerminatedAt(meet(p1, p2),t)",
          "Happens(running(p1),t) => TerminatedAt(meet(p1, p2),t)",
          "Happens(running(p2),t) => TerminatedAt(meet(p1, p2),t)",
          "Happens(exit(p1),t) => TerminatedAt(meet(p1, p2),t).",
          "Happens(exit(p2),t) => TerminatedAt(meet(p1, p2),t).",
          "TerminatedAt(meet(p1, p2),t) => Happens(exit(p2),t) v Happens(running(p1),t) v Happens(running(p2),t) v Happens(exit(p1),t) v (Happens(walking(p2),t) ^ !Close(p2,p1,34,t)) v (Happens(walking(p1),t) ^ !Close(p1,p2,34,t))."
        )

    val pcResult = NaivePredicateCompletion(definiteClauses)
    println("\nPredicate completion result: ")
    //pcResult.foreach(f => println(f.toText))
    for (result <- pcResult) {
      val txtResult = result.toText
      val tmpflag = correctResult.remove(txtResult)
      assertTrue("Formula: " + txtResult + " is not included in the set of correct predicate completion formulas.", tmpflag)
      println(txtResult + " --- CORRECT")
    }
    assertTrue(correctResult.isEmpty)*/
  }


  @Test def predicateCompletionAndReplacement1() {
    val formulas = Set(kbParser.parseFormula(constraint1), kbParser.parseFormula(constraint2),
      kbParser.parseFormula(pec1), kbParser.parseFormula(pec2), kbParser.parseFormula(pec3), kbParser.parseFormula(pec4))

    val definiteClauses = Set(
      kbParser.parseDefiniteClause(meet_init1),
      kbParser.parseDefiniteClause(meet_init2),
      kbParser.parseDefiniteClause(meet_term1),
      kbParser.parseDefiniteClause(meet_term2),
      kbParser.parseDefiniteClause(meet_term3),
      kbParser.parseDefiniteClause(meet_term4),
      kbParser.parseDefiniteClause(meet_term5),
      kbParser.parseDefiniteClause(meet_term6)
    )


    val correctResult = mutable.Set(
      "StartTime(t) => !HoldsAt(f,t).",
      "!HoldsAt(meet(p, p),t).",
      "Next(t1,t0) ^ ((Happens(active(p1),t0) ^ !Happens(running(p2),t0) ^ Close(p1,p2,25,t0)) v (Happens(inactive(p1),t0) ^ !Happens(running(p2),t0) ^ !Happens(active(p2),t0) ^ Close(p1,p2,25,t0))) => HoldsAt(meet(p1, p2),t1).",
      "Next(t1,t0) ^ !HoldsAt(meet(p1, p2),t0) ^ !((Happens(active(p1),t0) ^ !Happens(running(p2),t0) ^ Close(p1,p2,25,t0)) v (Happens(inactive(p1),t0) ^ !Happens(running(p2),t0) ^ !Happens(active(p2),t0) ^ Close(p1,p2,25,t0))) => !HoldsAt(meet(p1, p2),t1).",
      "Next(t1,t0) ^ (Happens(running(p1),t0) v Happens(running(p2),t0) v Happens(exit(p2),t0) v Happens(exit(p1),t0) v (Happens(walking(p2),t0) ^ !Close(p2,p1,34,t0)) v (Happens(walking(p1),t0) ^ !Close(p1,p2,34,t0))) => !HoldsAt(meet(p1, p2),t1).",
      "Next(t1,t0) ^ HoldsAt(meet(p1, p2),t0) ^ !(Happens(running(p1),t0) v Happens(running(p2),t0) v Happens(exit(p2),t0) v Happens(exit(p1),t0) v (Happens(walking(p2),t0) ^ !Close(p2,p1,34,t0)) v (Happens(walking(p1),t0) ^ !Close(p1,p2,34,t0))) => HoldsAt(meet(p1, p2),t1)."
    )
    val pcResult = PredicateCompletion(formulas, definiteClauses)
    println("\nPredicate completion (with replacement) result: ")
    //pcResult.foreach(f => println(f.toText))

    for (result <- pcResult) {
      val txtResult = result.toText
      val tmpflag = correctResult.remove(txtResult)
      assertTrue("Formula: " + txtResult + " is not included in the set of correct predicate completion formulas.", tmpflag)
      println(txtResult + " --- CORRECT")
    }
    assertTrue(correctResult.isEmpty)
  }

  @Test def predicateCompletionAndReplacement2() {
    val formulas = Set(kbParser.parseFormula(constraint1), kbParser.parseFormula(constraint2),
      kbParser.parseFormula(pec1), kbParser.parseFormula(pec2), kbParser.parseFormula(pec3), kbParser.parseFormula(pec4))

    val definiteClauses1 = Set(
      kbParser.parseDefiniteClause("InitiatedAt(meet(x,A), t) :- Happens(active(x),t) ^ Happens(inactive(A),t)"),
      kbParser.parseDefiniteClause(meet_init1),
      kbParser.parseDefiniteClause(meet_init2)
    )

    val definiteClauses2 = Set(
      kbParser.parseDefiniteClause(meet_init1),
      kbParser.parseDefiniteClause("InitiatedAt(meet(x,A), t) :- Happens(active(x),t) ^ Happens(inactive(A),t)"),
      kbParser.parseDefiniteClause(meet_init2)
    )


    val correctResult1 = mutable.Set(
      "StartTime(t) => !HoldsAt(f,t).",
      "!HoldsAt(meet(p, p),t).",
      "Next(t1,t0) ^ TerminatedAt(f,t0) => !HoldsAt(f,t1).",
      "Next(t1,t0) ^ HoldsAt(f,t0) ^ !TerminatedAt(f,t0) => HoldsAt(f,t1).",
      "Next(t1,t0) ^ !HoldsAt(meet(x, p2),t0) ^ !((equals(p2,A) ^ Happens(active(x),t0) ^ Happens(inactive(A),t0)) v (Happens(active(x),t0) ^ !Happens(running(p2),t0) ^ Close(x,p2,25,t0)) v (Happens(inactive(x),t0) ^ !Happens(running(p2),t0) ^ !Happens(active(p2),t0) ^ Close(x,p2,25,t0))) => !HoldsAt(meet(x, p2),t1).",
      "Next(t1,t0) ^ ((equals(p2,A) ^ Happens(active(x),t0) ^ Happens(inactive(A),t0)) v (Happens(active(x),t0) ^ !Happens(running(p2),t0) ^ Close(x,p2,25,t0)) v (Happens(inactive(x),t0) ^ !Happens(running(p2),t0) ^ !Happens(active(p2),t0) ^ Close(x,p2,25,t0))) => HoldsAt(meet(x, p2),t1)."
    )

    val correctResult2 = mutable.Set(
      "StartTime(t) => !HoldsAt(f,t).",
      "!HoldsAt(meet(p, p),t).",
      "Next(t1,t0) ^ TerminatedAt(f,t0) => !HoldsAt(f,t1).",
      "Next(t1,t0) ^ HoldsAt(f,t0) ^ !TerminatedAt(f,t0) => HoldsAt(f,t1).",
      "Next(t1,t0) ^ !HoldsAt(meet(p1, p2),t0) ^ !((Happens(active(p1),t0) ^ !Happens(running(p2),t0) ^ Close(p1,p2,25,t0)) v (Happens(inactive(p1),t0) ^ !Happens(running(p2),t0) ^ !Happens(active(p2),t0) ^ Close(p1,p2,25,t0)) v (equals(p2,A) ^ Happens(active(p1),t0) ^ Happens(inactive(A),t0))) => !HoldsAt(meet(p1, p2),t1).",
      "Next(t1,t0) ^ ((Happens(active(p1),t0) ^ !Happens(running(p2),t0) ^ Close(p1,p2,25,t0)) v (Happens(inactive(p1),t0) ^ !Happens(running(p2),t0) ^ !Happens(active(p2),t0) ^ Close(p1,p2,25,t0)) v (equals(p2,A) ^ Happens(active(p1),t0) ^ Happens(inactive(A),t0))) => HoldsAt(meet(p1, p2),t1)."
    )


    val pc1 = PredicateCompletion(formulas, definiteClauses1)
    /*println("PC1: ")
    pc1.foreach(x=> println("\t --- "+x.toText))*/
    for (f <- pc1) {
      val txtResult = f.toText
      val tmpflag = correctResult1.remove(txtResult)
      assertTrue("Formula: " + txtResult + " is not included in the set of correct predicate completion formulas.", tmpflag)
      println(txtResult + " --- CORRECT")
    }
    assertTrue(correctResult1.isEmpty)


    val pc2 = PredicateCompletion(formulas, definiteClauses2)
    /*println("PC2: ")
    pc2.foreach(x=> println("\t --- "+x.toText))*/
    for (f <- pc2) {
      val txtResult = f.toText
      val tmpflag = correctResult2.remove(txtResult)
      assertTrue("Formula: " + txtResult + " is not included in the set of correct predicate completion formulas.", tmpflag)
      println(txtResult + " --- CORRECT")
    }
    assertTrue(correctResult2.isEmpty)

  }

  @Test def predicateCompletionAndReplacement3() {

    val formulas = Set(kbParser.parseFormula(constraint1), kbParser.parseFormula(constraint2),
      kbParser.parseFormula(pec1), kbParser.parseFormula(pec2), kbParser.parseFormula(pec3), kbParser.parseFormula(pec4))
    val definiteClauses1 = mutable.LinkedHashSet(
      kbParser.parseDefiniteClause("InitiatedAt(f,t):- !Happens(abrupt(x),t)."),
      kbParser.parseDefiniteClause(meet_init1),
      kbParser.parseDefiniteClause(meet_init2),
      kbParser.parseDefiniteClause(meet_term1),
      kbParser.parseDefiniteClause(meet_term2),
      kbParser.parseDefiniteClause(move_init1),
      kbParser.parseDefiniteClause(move_term1),
      kbParser.parseDefiniteClause(move_term2))

    val definiteClauses2 = mutable.LinkedHashSet(
      kbParser.parseDefiniteClause(meet_init1),
      kbParser.parseDefiniteClause(meet_init2),
      kbParser.parseDefiniteClause(meet_term1),
      kbParser.parseDefiniteClause(meet_term2),
      kbParser.parseDefiniteClause("InitiatedAt(f,t):- !Happens(abrupt(x),t)."),
      kbParser.parseDefiniteClause(move_init1),
      kbParser.parseDefiniteClause(move_term1),
      kbParser.parseDefiniteClause(move_term2))

    val definiteClauses3 = mutable.LinkedHashSet(
      kbParser.parseDefiniteClause(meet_init1),
      kbParser.parseDefiniteClause(meet_init2),
      kbParser.parseDefiniteClause(meet_term1),
      kbParser.parseDefiniteClause(meet_term2),
      kbParser.parseDefiniteClause(move_init1),
      kbParser.parseDefiniteClause(move_term1),
      kbParser.parseDefiniteClause(move_term2),
      kbParser.parseDefiniteClause("InitiatedAt(f,t):- !Happens(abrupt(x),t).")
    )

    val pc1 = PredicateCompletion(formulas, definiteClauses1)
    println("PC1: ")
    pc1.foreach(x => println("\t --- " + x.toText))
  }

}
