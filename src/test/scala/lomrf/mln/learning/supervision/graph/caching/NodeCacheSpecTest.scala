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

package lomrf.mln.learning.supervision.graph.caching

import lomrf.logic._
import lomrf.mln.learning.supervision.graph.Node
import org.scalatest.{ FunSpec, Matchers }

final class NodeCacheSpecTest extends FunSpec with Matchers {

  implicit class StringOps(symbol: String) {
    def C: Constant = Constant(symbol)
    def V: Variable = Variable(symbol)
  }

  private def trueEvidenceAtom(symbol: String, constants: String*): EvidenceAtom =
    EvidenceAtom.asTrue(symbol, constants.map(Constant).toVector)

  private def falseEvidenceAtom(symbol: String, constants: String*): EvidenceAtom =
    EvidenceAtom.asFalse(symbol, constants.map(Constant).toVector)

  private def atom(symbol: String, terms: Term*): AtomicFormula =
    AtomicFormula(symbol, terms.toVector)

  private def pLit(symbol: String, terms: Term*): Literal =
    Literal.asPositive(AtomicFormula(symbol, terms.toVector))

  private def nLit(symbol: String, terms: Term*): Literal =
    Literal.asNegative(AtomicFormula(symbol, terms.toVector))

  private def clause(literals: Literal*): Option[Clause] =
    Some(Clause(literals.toSet))

  describe("Cache a single positive example node [ Q(A) v !E(A) ].") {
    var simpleCache = SimpleNodeCache(AtomSignature("Q", 1))
    var fastCache = FastNodeCache(AtomSignature("Q", 1))

    val node = Node(
      trueEvidenceAtom("Q", "A"),
      IndexedSeq(falseEvidenceAtom("E", "A")),
      clause(pLit("Q", "x".V), nLit("E", "x".V)),
      clause(nLit("E", "x".V)),
      atom("Q", "x".V)
    )

    simpleCache += node
    fastCache += node

    it ("should have size 1") {
      simpleCache.size shouldEqual 1
      fastCache.size shouldEqual 1
    }

    it ("should have 1 positives node and 0 negatives") {
      simpleCache.numberOfPositive shouldEqual 1
      simpleCache.numberOfNegative shouldEqual 0

      fastCache.numberOfPositive shouldEqual 1
      fastCache.numberOfNegative shouldEqual 0
    }

    it ("should contain the cached nodes") {
      simpleCache.contains(node) shouldBe true
      simpleCache.get(node) shouldBe Some(1)

      fastCache.contains(node) shouldBe true
      fastCache.get(node) shouldBe Some(1)
    }

    it ("should collect a single node") {
      simpleCache.collectNodes shouldEqual fastCache.collectNodes
    }
  }

  describe("Cache a negative example node [ !Q(A) v !E(A) ] 10 times.") {

    var simpleCache = SimpleNodeCache(AtomSignature("Q", 1))
    var fastCache = FastNodeCache(AtomSignature("Q", 1))

    val node = Node(
      falseEvidenceAtom("Q", "A"),
      IndexedSeq(falseEvidenceAtom("E", "A")),
      clause(nLit("Q", "x".V), nLit("E", "x".V)),
      clause(nLit("E", "x".V)),
      atom("Q", "x".V)
    )

    for (_ <- 1 to 10) {
      simpleCache += node
      fastCache += node
    }

    it ("should have size 1") {
      simpleCache.size shouldEqual 1
      fastCache.size shouldEqual 1
    }

    it ("should have 0 positives node and 1 negatives") {
      simpleCache.numberOfPositive shouldEqual 0
      simpleCache.numberOfNegative shouldEqual 1

      fastCache.numberOfPositive shouldEqual 0
      fastCache.numberOfNegative shouldEqual 1
    }

    it ("should contain the cached nodes") {
      simpleCache.contains(node) shouldBe true
      simpleCache.get(node) shouldBe Some(10)

      fastCache.contains(node) shouldBe true
      fastCache.get(node) shouldBe Some(10)
    }

    it ("should collect a single node") {
      simpleCache.collectNodes shouldEqual fastCache.collectNodes
    }
  }

  describe("Cache an example node [ Q(A) v !E(A) ] and its opposite [ !Q(B) v !E(B) ].") {

    var simpleCache = SimpleNodeCache(AtomSignature("Q", 1))
    var fastCache = FastNodeCache(AtomSignature("Q", 1))

    val pNode = Node(
      trueEvidenceAtom("Q", "A"),
      IndexedSeq(falseEvidenceAtom("E", "A")),
      clause(pLit("Q", "x".V), nLit("E", "x".V)),
      clause(nLit("E", "x".V)),
      atom("Q", "x".V)
    )

    val nNode = Node(
      falseEvidenceAtom("Q", "B"),
      IndexedSeq(falseEvidenceAtom("E", "B")),
      clause(nLit("Q", "y".V), nLit("E", "y".V)),
      clause(nLit("E", "y".V)),
      atom("Q", "y".V)
    )

    simpleCache ++= Seq(pNode, nNode)
    fastCache ++= Seq(pNode, nNode)

    it ("should have size 2") {
      simpleCache.size shouldEqual 2
      fastCache.size shouldEqual 2
    }

    it ("should have 1 positives node and 1 negatives") {
      simpleCache.numberOfPositive shouldEqual 1
      simpleCache.numberOfNegative shouldEqual 1

      fastCache.numberOfPositive shouldEqual 1
      fastCache.numberOfNegative shouldEqual 1
    }

    it ("should contain the cached nodes") {
      simpleCache.contains(pNode) shouldBe true
      simpleCache.get(pNode) shouldBe Some(1)
      simpleCache.contains(nNode) shouldBe true
      simpleCache.get(nNode) shouldBe Some(1)

      fastCache.contains(pNode) shouldBe true
      fastCache.get(pNode) shouldBe Some(1)
      fastCache.contains(nNode) shouldBe true
      fastCache.get(nNode) shouldBe Some(1)
    }

    it ("should collect both nodes") {
      simpleCache.collectNodes.length shouldEqual 2
      fastCache.collectNodes.length shouldEqual 2
      simpleCache.collectNodes shouldEqual fastCache.collectNodes
    }
  }

  describe("Cache an example node [ Q(A) v !E(A) ] and its opposite [ !Q(B) v !E(B) ] 20 times.") {

    var simpleCache = SimpleNodeCache(AtomSignature("Q", 1))
    var fastCache = FastNodeCache(AtomSignature("Q", 1))

    val pNode = Node(
      trueEvidenceAtom("Q", "A"),
      IndexedSeq(falseEvidenceAtom("E", "A")),
      clause(pLit("Q", "x".V), nLit("E", "x".V)),
      clause(nLit("E", "x".V)),
      atom("Q", "x".V)
    )

    val nNode = Node(
      falseEvidenceAtom("Q", "B"),
      IndexedSeq(falseEvidenceAtom("E", "B")),
      clause(nLit("Q", "y".V), nLit("E", "y".V)),
      clause(nLit("E", "y".V)),
      atom("Q", "y".V)
    )

    simpleCache += pNode
    fastCache += pNode
    for (_ <- 1 to 20) {
      simpleCache += nNode
      fastCache += nNode
    }

    it ("should have size 2") {
      simpleCache.size shouldEqual 2
      fastCache.size shouldEqual 2
    }

    it ("should have 1 positives node and 1 negatives") {
      simpleCache.numberOfPositive shouldEqual 1
      simpleCache.numberOfNegative shouldEqual 1

      fastCache.numberOfPositive shouldEqual 1
      fastCache.numberOfNegative shouldEqual 1
    }

    it ("should contain the cached nodes") {
      simpleCache.contains(pNode) shouldBe true
      simpleCache.get(pNode) shouldBe Some(1)
      simpleCache.contains(nNode) shouldBe true
      simpleCache.get(nNode) shouldBe Some(20)

      fastCache.contains(pNode) shouldBe true
      fastCache.get(pNode) shouldBe Some(1)
      fastCache.contains(nNode) shouldBe true
      fastCache.get(nNode) shouldBe Some(20)
    }

    it ("should collect a single node, since the other one is filtered out") {
      simpleCache.collectNodes.length shouldEqual 1
      fastCache.collectNodes.length shouldEqual 1
      simpleCache.collectNodes shouldEqual fastCache.collectNodes
    }
  }

  describe("Cache an example node [ Q(A) v !E(A) ] and its opposite [ !Q(A) v !E(A) ] 20 times. Then add another example.") {

    var simpleCache = SimpleNodeCache(AtomSignature("Q", 1))
    var fastCache = FastNodeCache(AtomSignature("Q", 1))

    val pNode = Node(
      trueEvidenceAtom("Q", "A"),
      IndexedSeq(falseEvidenceAtom("E", "A")),
      clause(pLit("Q", "x".V), nLit("E", "x".V)),
      clause(nLit("E", "x".V)),
      atom("Q", "x".V)
    )

    val nNode = Node(
      falseEvidenceAtom("Q", "B"),
      IndexedSeq(falseEvidenceAtom("E", "B")),
      clause(nLit("Q", "y".V), nLit("E", "y".V)),
      clause(nLit("E", "y".V)),
      atom("Q", "y".V)
    )

    simpleCache += pNode
    fastCache += pNode
    for (_ <- 1 to 20) {
      simpleCache += nNode
      fastCache += nNode
    }

    it ("should have size 2") {
      simpleCache.size shouldEqual 2
      fastCache.size shouldEqual 2
    }

    it ("should have 1 positives node and 1 negatives") {
      simpleCache.numberOfPositive shouldEqual 1
      simpleCache.numberOfNegative shouldEqual 1

      fastCache.numberOfPositive shouldEqual 1
      fastCache.numberOfNegative shouldEqual 1
    }

    it ("should contain the cached nodes") {
      simpleCache.contains(pNode) shouldBe true
      simpleCache.get(pNode) shouldBe Some(1)
      simpleCache.contains(nNode) shouldBe true
      simpleCache.get(nNode) shouldBe Some(20)

      fastCache.contains(pNode) shouldBe true
      fastCache.get(pNode) shouldBe Some(1)
      fastCache.contains(nNode) shouldBe true
      fastCache.get(nNode) shouldBe Some(20)
    }

    it ("should collect a single node, since the other one is filtered out") {
      simpleCache.collectNodes.length shouldEqual 1
      fastCache.collectNodes.length shouldEqual 1
      simpleCache.collectNodes shouldEqual fastCache.collectNodes
    }

    val node = Node(
      trueEvidenceAtom("Q", "A"),
      IndexedSeq(falseEvidenceAtom("E", "A"), falseEvidenceAtom("Z", "A")),
      clause(pLit("Q", "x".V), nLit("E", "x".V), nLit("Z", "x".V)),
      clause(pLit("E", "x".V)),
      atom("Q", "x".V)
    )

    it ("add another example node [ Q(A) v E(A) v Z(A) ]") {
      simpleCache += node
      fastCache += node
    }

    it ("should have size 3") {
      simpleCache.size shouldEqual 3
      fastCache.size shouldEqual 3
    }

    it ("should have 2 positives node and 1 negatives") {
      simpleCache.numberOfPositive shouldEqual 2
      simpleCache.numberOfNegative shouldEqual 1

      fastCache.numberOfPositive shouldEqual 2
      fastCache.numberOfNegative shouldEqual 1
    }

    it ("should contain one more cached node") {
      simpleCache.contains(pNode) shouldBe true
      simpleCache.get(pNode) shouldBe Some(1)
      simpleCache.contains(nNode) shouldBe true
      simpleCache.get(nNode) shouldBe Some(20)
      simpleCache.contains(node) shouldBe true
      simpleCache.get(node) shouldBe Some(1)

      fastCache.contains(pNode) shouldBe true
      fastCache.get(pNode) shouldBe Some(1)
      fastCache.contains(nNode) shouldBe true
      fastCache.get(nNode) shouldBe Some(20)
      fastCache.contains(node) shouldBe true
      fastCache.get(node) shouldBe Some(1)
    }

    it ("should collect a 2 example nodes, since one is filtered out") {
      simpleCache.collectNodes.length shouldEqual 2
      fastCache.collectNodes.length shouldEqual 2
      simpleCache.collectNodes shouldEqual fastCache.collectNodes
    }
  }

  describe("Cache an example node [ Q(A) v !E(A) ] 20 times. Then remove it.") {

    var simpleCache = SimpleNodeCache(AtomSignature("Q", 1))
    var fastCache = FastNodeCache(AtomSignature("Q", 1))

    val pNode = Node(
      trueEvidenceAtom("Q", "A"),
      IndexedSeq(falseEvidenceAtom("E", "A")),
      clause(pLit("Q", "x".V), nLit("E", "x".V)),
      clause(nLit("E", "x".V)),
      atom("Q", "x".V)
    )

    for (_ <- 1 to 20) {
      simpleCache += pNode
      fastCache += pNode
    }

    simpleCache -= pNode
    fastCache -= pNode

    it ("should have size 0") {
      simpleCache.size shouldEqual 0
      fastCache.size shouldEqual 0
    }

    it ("should have 0 positives node and 0 negatives") {
      simpleCache.numberOfPositive shouldEqual 0
      simpleCache.numberOfNegative shouldEqual 0

      fastCache.numberOfPositive shouldEqual 0
      fastCache.numberOfNegative shouldEqual 0
    }

    it ("should not contain the previously cached nodes") {
      simpleCache.contains(pNode) shouldBe false
      simpleCache.get(pNode) shouldBe None

      fastCache.contains(pNode) shouldBe false
      fastCache.get(pNode) shouldBe None
    }

    it ("should collect no node, since they are removed") {
      simpleCache.collectNodes.length shouldEqual 0
      fastCache.collectNodes.length shouldEqual 0
      simpleCache.collectNodes shouldEqual fastCache.collectNodes
    }
  }
}
