package lomrf.mln.learning.weight

import java.io.PrintStream
import lomrf.logic.AtomSignature
import lomrf.util.{AtomEvidenceDB, Logging}
import lomrf.mln.model.mrf._

/**
 * Under development
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class MaxMarginLearner(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB]) extends Logging {

  /*val state = MRFState(mrf)

  val annotation = annotationDB.get(AtomSignature("HoldsAt", 2)).get

  val iterator = mrf.atoms.iterator()
  while(iterator.hasNext) {
    iterator.advance()
    val atom = iterator.value()
    val value = annotation.get(atom.id)
    if(value == TRUE)
      atom.state = true
    else atom.state = false
  }

  state.evaluateState()*/

  def learn() = ???

  def writeResults(out: PrintStream = System.out) = ???
}
