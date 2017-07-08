package lomrf.logic

import org.scalatest.{FunSpec, Matchers}

/**
  * A series of specification tests for [[lomrf.logic.TriState]]
  */
final class TriStateSpecTest extends FunSpec with Matchers {

  describe("Conjunction of Tri-States") {

    it("TRUE ^ TRUE should be TRUE") {
      TRUE ^ TRUE shouldEqual TRUE
    }

    it("TRUE ^ FALSE should be FALSE") {
      TRUE ^ FALSE shouldEqual FALSE
    }

    it("TRUE ^ UNKNOWN should be UNKNOWN") {
      TRUE ^ UNKNOWN shouldEqual UNKNOWN
    }

    it("FALSE ^ FALSE should be FALSE") {
      FALSE ^ FALSE shouldEqual FALSE
    }

    it("FALSE ^ UNKNOWN should be FALSE") {
      FALSE ^ UNKNOWN shouldEqual FALSE
    }

    it("UNKNOWN ^ UNKNOWN should be UNKNOWN") {
      UNKNOWN ^ UNKNOWN shouldEqual UNKNOWN
    }
  }

  describe("Disjunction of Tri-States") {

    it("TRUE v TRUE should be TRUE") {
      TRUE v TRUE shouldEqual TRUE
    }

    it("TRUE v FALSE should be TRUE") {
      TRUE v FALSE shouldEqual TRUE
    }

    it("TRUE v UNKNOWN should be TRUE") {
      TRUE v UNKNOWN shouldEqual TRUE
    }

    it("FALSE v FALSE should be FALSE") {
      FALSE ^ FALSE shouldEqual FALSE
    }

    it("FALSE v UNKNOWN should be UNKNOWN") {
      FALSE v UNKNOWN shouldEqual UNKNOWN
    }

    it("UNKNOWN v UNKNOWN should be UNKNOWN") {
      UNKNOWN v UNKNOWN shouldEqual UNKNOWN
    }
  }

  describe("Flip Tri-States") {

    it("!TRUE should be FALSE") {
      TRUE.flip shouldEqual FALSE
    }

    it("!FALSE should be TRUE") {
      FALSE.flip shouldEqual TRUE
    }

    it("!UNKNOWN should be UNKNOWN") {
      UNKNOWN.flip shouldEqual UNKNOWN
    }
  }

}
