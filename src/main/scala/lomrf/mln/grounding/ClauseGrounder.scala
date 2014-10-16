package lomrf.mln.grounding

import lomrf.logic.{Variable, AtomSignature}

import scala.collection.{Iterable, Map}

/**
 * @author Anastasios Skarlatidis
 */
trait ClauseGrounder {

  def collectedSignatures: Set[AtomSignature]

  def getVariableDomains : Map[Variable, Iterable[String]]

  def computeGroundings(): Unit

}
