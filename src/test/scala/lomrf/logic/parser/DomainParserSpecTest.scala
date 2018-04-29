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

package lomrf.logic.parser

import lomrf.logic.{ AtomicType, ConstantTypeDefinition, FunctionType, IntegerTypeDefinition }
import org.scalatest.{ FunSpec, Matchers }

final class DomainParserSpecTest extends FunSpec with Matchers {

  val domainParser = new DomainParser

  describe("Constant type domain definitions") {

    val time = "time = {1, 2, 3, 4, 5}"

    it(s"$time should be a valid constant domain definition") {
      domainParser.parseConstantType(time) shouldEqual
        ConstantTypeDefinition("time", (1 to 5).map(_.toString))
    }

    val person = "person = { Anna, Bob, George }"

    it(s"$person should be a valid constant domain definition") {
      domainParser.parseConstantType(person) shouldEqual
        ConstantTypeDefinition("person", Vector("Anna", "Bob", "George"))
    }
  }

  describe("Integer type domain definitions") {

    val time = "time = {1, ..., 100}"

    it(s"$time should be a valid integer domain definition") {
      domainParser.parseIntegerType(time) shouldEqual
        IntegerTypeDefinition("time", 1, 100)
    }

    val number = "number = {5, ..., 25}"

    it(s"$number should be a valid integer domain definition") {
      domainParser.parseIntegerType(number) shouldEqual
        IntegerTypeDefinition("number", 5, 25)
    }
  }

  describe("Atomic type domain definitions") {

    val parent = "Parent(person, person)"

    it(s"$parent should be a valid atomic domain definition") {
      domainParser.parseAtomicType(parent) shouldEqual
        AtomicType("Parent", Vector("person", "person"))
    }

    val unary = "UnaryPredicate"

    it(s"$unary should be a valid atomic domain definition") {
      domainParser.parseAtomicType(unary) shouldEqual
        AtomicType("UnaryPredicate", Vector.empty[String])
    }
  }

  describe("Function type domain definitions") {

    val active = "event active(id)"

    it(s"$active should be a valid function domain definition") {
      domainParser.parseFunctionType(active) shouldEqual
        FunctionType("event", "active", Vector("id"))
    }

    val f = "y f(x, z)"

    it(s"$f should be a valid function domain definition") {
      domainParser.parseFunctionType(f) shouldEqual
        FunctionType("y", "f", Vector("x", "z"))
    }
  }
}
