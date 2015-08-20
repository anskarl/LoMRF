package lomrf.util.collection.mutable

import org.scalatest.{Matchers, FunSpec}

/**
 * Specification test for TuplesDB holding tuples of cartesian products
 * over variable constants.
 */
final class TuplesDBSpecTest extends FunSpec with Matchers {

  describe("Check simple insertions and deletion") {

    info("Check simple insertions and deletion:")

    describe("Insert variable x and 4 constants { 1, 2, 3, 4 }") {

      val db = new TuplesDB[String, Int]()

      db += ("x", 1)
      db += ("x", 2)
      db += ("x", 3)
      db += ("x", 4)

      // duplicates should be ignored
      db += ("x", 2)

      it("should contain 1 variable and 4 tuples") {
        db.contains("x") shouldBe true
        db.numberOfVariables shouldBe 1
        db.numberOfConstants("x") shouldBe 4
        db.size shouldBe 4
      }

      info("1. Database:\n" + db)
    }

    describe("Insert another variable y and 1 constant { 5 }") {

      val db = new TuplesDB[String, Int]()

      db += ("x", Set(1, 2, 3, 4))
      db += ("y", 5)

      it("should contain 2 variable and and 4 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe true
        db.numberOfVariables shouldBe 2
        db.numberOfConstants("x") shouldBe 4
        db.numberOfConstants("y") shouldBe 1
        db.size shouldBe 4
      }

      info("2. Database:\n" + db)
    }

    describe("Insert another variable z and 2 constants { 4, 9 }") {

      val db = new TuplesDB[String, Int]()

      db += ("x", Set(1, 2, 3, 4))
      db += ("y", 5)
      db += ("z", 4)
      db += ("z", 9)

      it("should contain 3 variable and and 8 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe true
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 4
        db.numberOfConstants("y") shouldBe 1
        db.numberOfConstants("z") shouldBe 2
        db.size shouldBe 8
      }

      info("3. Database:\n" + db)
    }

    describe("Delete constant { 1 } from variable x") {

      val db = new TuplesDB[String, Int]()

      db += ("x", Set(1, 2, 3, 4))
      db += ("y", 5)
      db += ("z", 4)
      db += ("z", 9)

      db -= ("x", 1)

      // does not exist and should be ignored
      db -= ("x", 9)

      it("should contain 3 variable and and 6 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe true
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 3
        db.numberOfConstants("y") shouldBe 1
        db.numberOfConstants("z") shouldBe 2
        db.size shouldBe 6
      }

      info("4. Database:\n" + db)
    }

    describe("Delete constant { 5 } from variable y") {

      val db = new TuplesDB[String, Int]()

      db += ("x", Set(1, 2, 3, 4))
      db += ("y", 5)
      db += ("z", 4)
      db += ("z", 9)

      db -= ("x", 1)

      // does not exist and should be ignored
      db -= ("y", 10)

      db -= ("y", 5)

      it("should contain 2 variable and and 6 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe false
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 2
        db.numberOfConstants("x") shouldBe 3
        db.numberOfConstants("z") shouldBe 2
        db.size shouldBe 6
      }

      info("5. Database:\n" + db)
    }

  }

  describe("Check multiple insertions and deletions") {

    info("Check multiple insertions and deletions:")

    describe("Insert variables x, y and z having 4 constants each") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("A", "B", "C", "D"))
      db += ("y", Set("!", "@", "#", "$"))
      db += ("z", Set("1", "2", "3", "4"))

      it("should contain 3 variables and 64 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe true
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 4
        db.numberOfConstants("y") shouldBe 4
        db.numberOfConstants("z") shouldBe 4
        db.size shouldBe 64
      }

      info("1. Database:\n" + db)
    }

    describe("Delete constants { A, B } from variable x") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("A", "B", "C", "D"))
      db += ("y", Set("!", "@", "#", "$"))
      db += ("z", Set("1", "2", "3", "4"))

      db -= ("x", Set("A", "B"))

      it("should contain 3 variables and 32 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe true
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 2
        db.numberOfConstants("y") shouldBe 4
        db.numberOfConstants("z") shouldBe 4
        db.size shouldBe 32
      }

      info("2. Database:\n" + db)
    }

    describe("Delete constants { 1, 2, 3 } from variable z") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("A", "B", "C", "D"))
      db += ("y", Set("!", "@", "#", "$"))
      db += ("z", Set("1", "2", "3", "4"))

      db -= ("x", Set("A", "B"))
      db -= ("z", Set("1", "2", "3"))

      it("should contain 3 variables and 8 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe true
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 2
        db.numberOfConstants("y") shouldBe 4
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 8
      }

      info("3. Database:\n" + db)
    }

    describe("Delete constants { C } from variable x") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("A", "B", "C", "D"))
      db += ("y", Set("!", "@", "#", "$"))
      db += ("z", Set("1", "2", "3", "4"))

      db -= ("x", Set("A", "B"))
      db -= ("z", Set("1", "2", "3"))
      db -= ("x", "C")

      it("should contain 3 variables and 4 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe true
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 1
        db.numberOfConstants("y") shouldBe 4
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 4
      }

      info("4. Database:\n" + db)
    }

    describe("Delete constants all constants from variable y") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("A", "B", "C", "D"))
      db += ("y", Set("!", "@", "#", "$"))
      db += ("z", Set("1", "2", "3", "4"))

      db -= ("x", Set("A", "B"))
      db -= ("z", Set("1", "2", "3"))
      db -= ("x", "C")
      db -= ("y", Set("!", "@", "#", "$"))

      it("should contain 3 variables and 4 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe false
        db.contains("z") shouldBe true
        db.numberOfVariables shouldBe 2
        db.numberOfConstants("x") shouldBe 1
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 1
      }

      info("5. Database:\n" + db)
    }

    describe("Delete constant { 4 } from variable z") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("A", "B", "C", "D"))
      db += ("y", Set("!", "@", "#", "$"))
      db += ("z", Set("1", "2", "3", "4"))

      db -= ("x", Set("A", "B"))
      db -= ("z", Set("1", "2", "3"))
      db -= ("x", "C")
      db -= ("y", Set("!", "@", "#", "$"))
      db -= ("z", "4")

      it("should contain 3 variables and 4 tuples") {
        db.contains("x") shouldBe true
        db.contains("y") shouldBe false
        db.contains("z") shouldBe false
        db.numberOfVariables shouldBe 1
        db.numberOfConstants("x") shouldBe 1
        db.size shouldBe 1
      }

      info("6. Database:\n" + db)
    }

  }

  describe("Check insertion of many variables and constants") {

    val db = new TuplesDB[String, String]()

    val variables = (1 to 5) map (vid => "x" + vid)

    val domains = variables map (vname => (1 to 10).map(vname + "_" + _).toSet)

    for(i <- variables.indices) db += (variables(i), domains(i))

    it("should contain 5 variables and 100000 tuples") {
      db.numberOfVariables shouldBe 5
      db.size shouldBe 100000
    }

    it("should contain 10000 tuples having x1 = x1_5") {
      db.countTuples(Map("x1" -> "x1_5")) shouldBe 10000
    }

    it("should contain 1000 tuples having x1 = x1_5 and x2 = x2_1") {
      db.countTuples(Map("x2" -> "x2_1", "x1" -> "x1_5")) shouldBe 1000
    }
  }

  describe("Check count tuples operation") {

    describe("Count tuples having y = A") {
      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      val mapping = Map("y" -> "A")
      val count = db.countTuples(mapping)

      it("should be 5") {
        count shouldBe 5
      }
    }

    describe("Count tuples having z = something") {
      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      val mapping = Map("z" -> "something")
      val count = db.countTuples(mapping)

      it("Count should be 15") {
        count shouldBe 15
      }
    }

    describe("Count tuples having y = B and x = 3") {
      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      val mapping = Map("y" -> "B", "x" -> "3")
      val count = db.countTuples(mapping)

      it("should be 1") {
        count shouldBe 1
      }
    }

  }

  describe("Check delete matched tuples operation") {

    info("Check delete matched tuples operation:")

    describe("Delete all tuples having x = 1 and y = A") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db deleteMatchedTuples Map("x" -> "1", "y" -> "A")

      it("should contain 3 variables and 14 tuples") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 5
        db.numberOfConstants("z") shouldBe 1
        db.numberOfConstants("y") shouldBe 3
        db.size shouldBe 14
      }

      info("1. Database\n" + db)
    }

    describe("Delete all tuples having x = 1") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db deleteMatchedTuples Map("x" -> "1")

      it("should contain 3 variables and 13 tuples") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 4
        db.numberOfConstants("y") shouldBe 3
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 12
      }

      info("2. Database\n" + db)
    }

    describe("Delete all tuples having z = something") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db deleteMatchedTuples Map("z" -> "something")

      it("should contain 3 variables and 0 tuples") {
        db.numberOfVariables shouldBe 0
        db.size shouldBe 0
      }

      info("3. Database\n" + db)
    }

    describe("Delete all tuples having z = something and x = 4") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db deleteMatchedTuples Map("z" -> "something", "x" -> "4")

      it("should contain 3 variables and 12 tuples") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 4
        db.numberOfConstants("y") shouldBe 3
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 12
      }

      info("4. Database\n" + db)
    }

  }

  describe("Check delete not matched tuples operation") {

    info("Check delete not matched tuples operation:")

    describe("Delete all tuples NOT having x = 1 and y = A") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db deleteNotMatchedTuples Map("x" -> "1", "y" -> "A")

      it("should contain 3 variables and 1 tuple") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 1
        db.numberOfConstants("y") shouldBe 1
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 1
      }

      info("1. Database\n" + db)
    }

    describe("Delete all tuples NOT having x = 1") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db deleteNotMatchedTuples Map("x" -> "1")

      it("should contain 3 variables and 3 tuple") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 1
        db.numberOfConstants("y") shouldBe 3
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 3
      }

      info("2. Database\n" + db)
    }

    describe("Delete all tuples NOT having z = something") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db deleteNotMatchedTuples Map("z" -> "something")

      it("should contain 3 variables and 15 tuple") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 5
        db.numberOfConstants("y") shouldBe 3
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 15
      }

      info("3. Database\n" + db)
    }

    describe("Delete all tuples NOT having x = 6") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      // should delete everything, because there is no tuple having this constant
      db deleteNotMatchedTuples Map("x" -> "6")

      it("should contain 0 variables and 0 tuple") {
        db.numberOfVariables shouldBe 0
        db.size shouldBe 0
      }

      info("4. Database\n" + db)
    }

  }

  describe("Allow only tuples matching one of the given mappings operation") {

    info("Allow only tuples matching one of the given mappings operation:")

    describe("Allow only tuples having x = 1 and y = A or x = 2 and y = B") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db allowOnly Map("x" -> Vector("1", "2"), "y" -> Vector("A", "B"))

      it("should contain only 2 tuples") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 2
        db.numberOfConstants("y") shouldBe 2
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 2
      }

      info("1. Database\n" + db)
    }

    describe("Allow only tuples having x = 1") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db allowOnly Map("x" -> Vector("1"))

      it("should contain only 3 tuples") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 1
        db.numberOfConstants("y") shouldBe 3
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 3
      }

      info("2. Database\n" + db)
    }

    describe("Allow only tuples having x = 6") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db allowOnly Map("x" -> Vector("6"))

      // should not be able to insert this constant, because x is locked to 6
      db += ("x", "7")

      // should be able to insert this constant
      db += ("y", "F")

      it("should contain only 4 tuples") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 1
        db.numberOfConstants("y") shouldBe 4
        db.numberOfConstants("z") shouldBe 1
        db.size shouldBe 4
      }

      info("3. Database\n" + db)
    }

    describe("Allow only tuples having x = 6 and y = A then delete y and insert other constants") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C"))
      db += ("z", "something")

      db allowOnly Map("x" -> Vector("6"), "y" -> Vector("A"))

      // should not be able to insert multiple constants, because y is locked
      db += ("y", Set("K", "L"))

      // should be able to insert this set
      db += ("z", Set("someone", "somebody"))

      db -= ("y", "A")

      // here should be possible to insert the set of constants, because y was deleted and therefore unlocked
      db += ("y", Set("K", "L"))

      it("should contain only 6 tuples") {
        db.numberOfVariables shouldBe 3
        db.numberOfConstants("x") shouldBe 1
        db.numberOfConstants("y") shouldBe 2
        db.numberOfConstants("z") shouldBe 3
        db.isLocked("y") shouldBe false
        db.isLocked("x") shouldBe true
        db.size shouldBe 6
      }

      info("4. Database\n" + db)
    }

    describe("Allow only tuples having some constant and then further constraint the space") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C", "D"))
      db += ("z", Set("!", "@", "#"))
      db += ("t", Set("+"))

      db allowOnly Map("x" -> Vector("1", "2", "6"), "y" -> Vector("A", "D", "F"))

      db allowOnly Map("x" -> Vector("2", "1"), "z" -> Vector("!", "!"))

      // should be able to insert, because t is not locked
      db += ("t", "-")

      it("should contain only 4 tuples") {
        db.numberOfVariables shouldBe 4
        db.numberOfConstants("x") shouldBe 2
        db.numberOfConstants("y") shouldBe 2
        db.numberOfConstants("z") shouldBe 1
        db.numberOfConstants("t") shouldBe 2
        db.isLocked("y") shouldBe true
        db.isLocked("x") shouldBe true
        db.isLocked("z") shouldBe true
        db.isLocked("t") shouldBe false
        db.size shouldBe 4
      }

      info("5. Database\n" + db)
    }

    describe("Allow only tuples that are disjoint and therefore resulting in an empty database") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C", "D"))
      db += ("z", Set("!", "@"))
      db += ("t", Set("+"))

      db allowOnly Map("x" -> Vector("1", "2", "6"), "y" -> Vector("A", "D", "C"))

      db allowOnly Map("x" -> Vector("3"), "z" -> Vector("!"))

      it("should contain only 0 tuples") {
        db.numberOfVariables shouldBe 0
        db.isLocked("y") shouldBe false
        db.isLocked("x") shouldBe false
        db.isLocked("z") shouldBe false
        db.isLocked("t") shouldBe false
        db.size shouldBe 0
      }

      info("6. Database\n" + db)
    }

    describe("Allow only tuples having x = 1 and y = A") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C", "D"))
      db += ("z", Set("!", "@"))
      db += ("t", Set("+"))

      db allowOnly Map("x" -> Vector("1"))

      db allowOnly Map("y" -> Vector("A"))

      it("should contain only 2 tuples") {
        db.numberOfVariables shouldBe 4
        db.isLocked("y") shouldBe true
        db.isLocked("x") shouldBe true
        db.isLocked("z") shouldBe false
        db.isLocked("t") shouldBe false
        db.size shouldBe 2
      }

      info("7. Database\n" + db)
    }

    describe("Allow only tuples having x = 1 and y = A then allow everything") {

      val db = new TuplesDB[String, String]()

      db += ("x", Set("1", "2", "3", "4", "5"))
      db += ("y", Set("A", "B", "C", "D"))
      db += ("z", Set("!", "@"))
      db += ("t", Set("+"))

      db allowOnly Map("x" -> Vector("1"))

      db allowOnly Map("y" -> Vector("A"))

      db allowEverything()

      db += ("y", Set("A", "B", "C"))
      db += ("x", Set("10", "11"))

      it("should contain only 18 tuples") {
        db.numberOfVariables shouldBe 4
        db.isLocked("y") shouldBe false
        db.isLocked("x") shouldBe false
        db.isLocked("z") shouldBe false
        db.isLocked("t") shouldBe false
        db.size shouldBe 18
      }

      info("8. Database\n" + db)
    }

  }

}