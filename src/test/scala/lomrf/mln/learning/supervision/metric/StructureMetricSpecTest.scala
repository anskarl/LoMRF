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

package lomrf.mln.learning.supervision.metric

import lomrf.logic.{ AtomSignature, Constant, EvidenceAtom, FunctionMapping }
import lomrf.mln.model.ConstantsSet
import lomrf.mln.model.builders.EvidenceBuilder
import org.scalatest.{ FunSpec, Matchers }

final class StructureMetricSpecTest extends FunSpec with Matchers {

  describe("Distances over predicates having NO functions.") {

    val metric = StructureMetric()

    val predicateA =
      EvidenceAtom.asTrue(
        "Friends",
        Vector("A", "B").map(Constant))
    val predicateB =
      EvidenceAtom.asTrue(
        "Friends",
        Vector("B", "A").map(Constant))
    val predicateC =
      EvidenceAtom.asTrue(
        "Friends",
        Vector("B", "C").map(Constant))
    val predicateD =
      EvidenceAtom.asFalse(
        "Friends",
        Vector("B", "C").map(Constant))
    val predicateE =
      EvidenceAtom.asTrue(
        "Enemies",
        Vector("B", "C").map(Constant))
    val predicateF =
      EvidenceAtom.asTrue(
        "Person",
        Vector("George", "Tall", "Thin", "Young", "Handsome").map(Constant))
    val predicateG =
      EvidenceAtom.asTrue(
        "Person",
        Vector("Pedro", "Tall", "Thin", "Old", "Ugly").map(Constant))
    val predicateH =
      EvidenceAtom.asTrue(
        "Person",
        Vector("Jason", "Short", "Thin", "Young", "Handsome").map(Constant))

    it("Distance of Friends(A, B) to itself should be 0") {
      metric.distance(predicateA, predicateA) shouldEqual 0
    }

    it("Distance of Friends(A, B) to Friends(B, A) should be 0.5") {
      metric.distance(predicateA, predicateB) shouldEqual 0.5
    }

    it("Distance of Friends(B, A) to Friends(B, C) should be 0.25") {
      metric.distance(predicateB, predicateC) shouldEqual 0.25
    }

    it("Distance of Friends(B, C) to !Friends(B, C) should be 1") {
      metric.distance(predicateC, predicateD) shouldEqual 1
    }

    it("Distance of Friends(B, C) to Enemies(B, C) should be 1") {
      metric.distance(predicateC, predicateE) shouldEqual 1
    }

    it("Distance of Person(George, Tall, Thin, Young, Handsome) to Person(Pedro, Tall, Thin, Old, Ugly) should be 0.3") {
      metric.distance(predicateF, predicateG) shouldEqual 0.3 +- 1E-6
    }

    it("Distance of Person(George, Tall, Thin, Young, Handsome) to Person(Jason, Tall, Fat, Young, Handsome) should be 0.2") {
      metric.distance(predicateF, predicateH) shouldEqual 0.2 +- 1E-6
    }

    it("Associative property should hold for any distance over predicates") {
      List(predicateA, predicateB, predicateC, predicateD, predicateE, predicateF, predicateG, predicateH)
        .sliding(2).foreach { pair =>
          metric.distance(pair.head, pair.last) shouldEqual metric.distance(pair.last, pair.head)
        }
    }
  }

  describe("Distances over predicates having one level functions.") {

    // Predicate schema
    val predicateSchema = Map(
      AtomSignature("Happens", 2) -> Vector("event", "time"))

    // Function schema
    val functionSchema = Map(
      AtomSignature("walking", 1) -> ("event", Vector("id")),
      AtomSignature("inactive", 1) -> ("event", Vector("id")))

    // Constants domain
    val constantsDomain = Map(
      "time" -> ConstantsSet((0 to 10).map(_.toString)),
      "event" -> ConstantsSet("Walking_A", "Walking_B", "Inactive_A", "Inactive_B"),
      "id" -> ConstantsSet("A", "B"))

    val builder = EvidenceBuilder(
      predicateSchema,
      functionSchema,
      Set.empty,
      Set.empty,
      constantsDomain,
      convertFunctions = true)

    // Append function mappings for walking/1, inactive/1 and active/1
    builder.functions += FunctionMapping("Inactive_A", "inactive", Vector(Constant("A")))
    builder.functions += FunctionMapping("Inactive_B", "inactive", Vector(Constant("B")))
    builder.functions += FunctionMapping("Walking_A", "walking", Vector(Constant("A")))
    builder.functions += FunctionMapping("Walking_B", "walking", Vector(Constant("B")))

    val metric = StructureMetric(builder.result().db)

    val predicateA =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("Walking_A", "5").map(Constant))
    val predicateB =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("Walking_A", "6").map(Constant))
    val predicateC =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("Walking_B", "5").map(Constant))
    val predicateD =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("Walking_B", "6").map(Constant))
    val predicateE =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("Inactive_A", "5").map(Constant))
    val predicateF =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("Inactive_A", "6").map(Constant))

    it("Distance of Happens(walking(A), 5) to itself should be 0") {
      metric.distance(predicateA, predicateA) shouldEqual 0
    }

    it("Distance of Happens(walking(A), 5) to Happens(walking(A), 6) should be 0.25") {
      metric.distance(predicateA, predicateB) shouldEqual 0.25
    }

    it("Distance of Happens(walking(A), 5) to Happens(walking(B), 5) should be 0.125") {
      metric.distance(predicateA, predicateC) shouldEqual 0.125
    }

    it("Distance of Happens(walking(A), 5) to Happens(walking(B), 6) should be 0.375") {
      metric.distance(predicateA, predicateD) shouldEqual 0.375
    }

    it("Distance of Happens(walking(A), 5) to Happens(inactive(A), 5) should be 0.25") {
      metric.distance(predicateA, predicateE) shouldEqual 0.25
    }

    it("Distance of Happens(walking(A), 5) to Happens(inactive(A), 6) should be 0.5") {
      metric.distance(predicateA, predicateF) shouldEqual 0.5
    }

    it("Associative property should hold for any distance over predicates") {
      List(predicateA, predicateB, predicateC, predicateD, predicateE, predicateF)
        .sliding(2).foreach { pair =>
          metric.distance(pair.head, pair.last) shouldEqual metric.distance(pair.last, pair.head)
        }
    }

  }

  describe("Distances over predicates having multiple level functions.") {

    // Predicate schema
    val predicateSchema = Map(
      AtomSignature("HugeWoodenBox", 2) -> Vector("largeBoxDomain"))

    // Function schema
    val functionSchema = Map(
      AtomSignature("largeBox", 2) -> ("largeBoxDomain", Vector("smallBoxDomain", "color")),
      AtomSignature("smallBox", 2) -> ("smallBoxDomain", Vector("tinyBoxDomain", "color")),
      AtomSignature("tinyBox", 1) -> ("tinyBoxDomain", Vector("color")))

    // Constants domain
    val constantsDomain = Map(
      "color" -> ConstantsSet("R", "G"),
      "largeBoxDomain" -> ConstantsSet("Large_RRR_Box", "Large_RRG_Box", "Large_RGR_Box", "Large_RGG_Box",
        "Large_GRR_Box", "Large_GRG_Box", "Large_GGR_Box", "Large_GGG_Box"),
      "smallBoxDomain" -> ConstantsSet("Small_RR_Box", "Small_RG_Box", "Small_GR_Box", "Small_GG_Box"),
      "tinyBoxDomain" -> ConstantsSet("Tiny_R_Box", "Tiny_G_Box"))

    // Evidence Builder
    val builder = EvidenceBuilder(
      predicateSchema,
      functionSchema,
      Set.empty,
      Set.empty,
      constantsDomain,
      convertFunctions = true)

    // Append function mappings for largeBox/2, smallBox/2, tinyBox/1
    builder.functions += FunctionMapping("Large_RRR_Box", "largeBox", Vector("Small_RR_Box", "R").map(Constant))
    builder.functions += FunctionMapping("Large_RRG_Box", "largeBox", Vector("Small_RR_Box", "G").map(Constant))
    builder.functions += FunctionMapping("Large_RGR_Box", "largeBox", Vector("Small_RG_Box", "R").map(Constant))
    builder.functions += FunctionMapping("Large_RGG_Box", "largeBox", Vector("Small_RG_Box", "G").map(Constant))
    builder.functions += FunctionMapping("Large_GRR_Box", "largeBox", Vector("Small_GR_Box", "R").map(Constant))
    builder.functions += FunctionMapping("Large_GRG_Box", "largeBox", Vector("Small_GR_Box", "G").map(Constant))
    builder.functions += FunctionMapping("Large_GGR_Box", "largeBox", Vector("Small_GG_Box", "R").map(Constant))
    builder.functions += FunctionMapping("Large_GGG_Box", "largeBox", Vector("Small_GG_Box", "G").map(Constant))

    builder.functions += FunctionMapping("Small_RR_Box", "smallBox", Vector("Tiny_R_Box", "R").map(Constant))
    builder.functions += FunctionMapping("Small_RG_Box", "smallBox", Vector("Tiny_R_Box", "G").map(Constant))
    builder.functions += FunctionMapping("Small_GR_Box", "smallBox", Vector("Tiny_G_Box", "R").map(Constant))
    builder.functions += FunctionMapping("Small_GG_Box", "smallBox", Vector("Tiny_G_Box", "G").map(Constant))

    builder.functions += FunctionMapping("Tiny_R_Box", "tinyBox", Vector("R").map(Constant))
    builder.functions += FunctionMapping("Tiny_G_Box", "tinyBox", Vector("G").map(Constant))

    val metric = StructureMetric(builder.result().db)

    val predicateA =
      EvidenceAtom.asTrue(
        "HugeWoodenBox",
        Vector("Large_RRR_Box").map(Constant))
    val predicateB =
      EvidenceAtom.asTrue(
        "HugeWoodenBox",
        Vector("Large_RRG_Box").map(Constant))
    val predicateC =
      EvidenceAtom.asTrue(
        "HugeWoodenBox",
        Vector("Large_GRG_Box").map(Constant))
    val predicateD =
      EvidenceAtom.asTrue(
        "HugeWoodenBox",
        Vector("Large_GGR_Box").map(Constant))

    it("Distance of HugeWoodenBox(largeBox(smallBox(tinyBox(R), R), R)) to itself should be 0") {
      metric.distance(predicateA, predicateA) shouldEqual 0
    }

    it("Distance of HugeWoodenBox(largeBox(smallBox(tinyBox(R), R), R)) to HugeWoodenBox(largeBox(smallBox(tinyBox(R), R), G)) should be 0.125") {
      metric.distance(predicateA, predicateB) shouldEqual 0.125
    }

    it("Distance of HugeWoodenBox(largeBox(smallBox(tinyBox(G), R), G)) to HugeWoodenBox(largeBox(smallBox(tinyBox(G), G), R)) should be 0.15625") {
      metric.distance(predicateC, predicateD) shouldEqual 0.15625
    }
  }

  describe("Distances over predicates having numbers") {

    // Predicate schema
    val predicateSchema = Map(
      AtomSignature("Happens", 2) -> Vector("event", "time"),
      AtomSignature("AUXavg_speed", 3) -> Vector("event", "id", "speed"))

    // Function schema
    val functionSchema = Map(
      AtomSignature("avg_speed", 2) -> ("event", Vector("id", "speed")))

    // Constants domain
    val constantsDomain = Map(
      "event" -> ConstantsSet("AvgSpeed_85_4354"),
      "id" -> ConstantsSet("4354"),
      "speed" -> ConstantsSet("85"),
      "time" -> ConstantsSet("10", "99"))

    // Evidence Builder
    val builder = EvidenceBuilder(
      predicateSchema,
      functionSchema,
      Set.empty,
      Set.empty,
      constantsDomain,
      convertFunctions = true)

    // Append function mappings for largeBox/2, smallBox/2, tinyBox/1
    builder.functions += FunctionMapping("AvgSpeed_85_4354", "avg_speed", Vector("4354", "85").map(Constant))

    val metric = StructureMetric(predicateSchema, builder.result().db)
      .makeNumeric((x: Double, y: Double) => math.abs(x - y) / (x + y), Set("speed"))

    val predicateA =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("AvgSpeed_85_4354", "99").map(Constant))

    val predicateB =
      EvidenceAtom.asTrue(
        "Happens",
        Vector("AvgSpeed_85_4354", "26").map(Constant))

    println(metric.distance(predicateA, predicateB))

  }
}
