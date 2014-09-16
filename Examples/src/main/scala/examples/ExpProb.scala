package examples

import lomrf.logic.AtomSignature
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model.MLN
import lomrf.mln.model.mrf.MRF
import lomrf.util.Logging

object ExpProb extends Logging {
  val sep = System.getProperty("file.separator")
  val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "probabilistic" + sep

  def main(args: Array[String]){
    val mrf1 = crispEvidence()
    val mrf2 = crispEvidenceUnk()
    val mrf3 = prob()

    val mrf4 = crispEvidence2()
    val mrf5 = crispEvidenceUnk2()

    println("----------------------------------->")
    println("--- crisp.db and kb.mln VS crisp2.db and kb2.mln")
    println("\tMRF1.clauses = "+mrf1.constraints.size())
    println("\tMRF1.atoms = "+mrf1.atoms.size())
    println("VS")
    println("\tMRF4.clauses = "+mrf4.constraints.size())
    println("\tMRF4.atoms = "+mrf4.atoms.size())

    println("\n--- crisp_unk.db and kb.mln VS crisp_unk2.db and kb2.mln")
    println("\tMRF2.clauses = "+mrf2.constraints.size())
    println("\tMRF2.atoms = "+mrf2.atoms.size())
    println("VS")
    println("\tMRF5.clauses = "+mrf5.constraints.size())
    println("\tMRF5.atoms = "+mrf5.atoms.size())

    println("\n--- prob.db and kb.mln")
    println("MRF3.clauses = "+mrf3.constraints.size())
    println("MRF3.atoms = "+mrf3.atoms.size())
    println("<-----------------------------------")

  }

  /**
   * This is an example of standard evidence with closed-world assumption. All evidence
   * predicate are either True or False and the absence of a ground evidence atom is
   * interpreted as an atom with False state value.
   *
   * @return the constructed MRF
   */
  private def crispEvidence(): MRF = {
    val queryAtoms = Set(AtomSignature("HoldsAt", 2))
    val cwAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    val mln = MLN(path_prefix+"kb.mln", path_prefix+"crisp.db", queryAtoms, cwa = cwAtoms)

    /*mln.atomStateDB.foreach{
      case (signature, db) => {
        println(signature+"{")
        db.dumpContents()
        println("}")
      }
    }*/
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    kbmc.buildNetwork
  }

  /**
   * This is an example of standard evidence with closed-world assumption.
   * The difference with the previous example is that the InitiatedAt/2 and
   * TerminatedAt/2 predicates are now with open-world assumption. The evidence
   * is given by the predicates InitObs/2 and TermObs/2 and in order to
   * introduce closed-world assumption to InitiatedAt/2 and TerminatedAt/2
   * predicates, the KB contains additionally completion constraints.
   *
   * @return the constructed MRF
   */
  private def crispEvidence2(): MRF = {
    val queryAtoms = Set(AtomSignature("HoldsAt", 2))
    val cwAtoms = Set(AtomSignature("InitObs", 2), AtomSignature("TermObs", 2))
    val owAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    val mln = MLN(path_prefix+"kb2.mln", path_prefix+"crisp2.db", queryAtoms, cwa = cwAtoms, owa = owAtoms)

    /*mln.atomStateDB.foreach{
      case (signature, db) => {
        println(signature+"{")
        db.dumpContents()
        println("}")
      }
    }*/
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    kbmc.buildNetwork
  }

  /**
   * This is an example of evidence with closed-world assumption and explicit definitions
   * of evidence ground atoms with Unknown state value.
   * The absence of a ground evidence atom is interpreted as an atom with False state value.
   * However, when an atom is prefixed with a question-mark symbol (?), then its state is
   * defined as Unknown.
   *
   * @return the constructed MRF
   */
  private def crispEvidenceUnk(): MRF = {
    val queryAtoms = Set(AtomSignature("HoldsAt", 2))
    val cwAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    val mln = MLN(path_prefix+"kb.mln", path_prefix+"crisp_unk.db", queryAtoms, cwa = cwAtoms)

    /*mln.atomStateDB.foreach{
      case (signature, db) => {
        println(signature+"{")
        db.dumpContents()
        println("}")
      }
    }*/
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    kbmc.buildNetwork
  }

  private def crispEvidenceUnk2(): MRF = {
    val queryAtoms = Set(AtomSignature("HoldsAt", 2))
    val cwAtoms = Set(AtomSignature("InitObs", 2), AtomSignature("TermObs", 2))
    val owAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    val mln = MLN(path_prefix+"kb2.mln", path_prefix+"crisp_unk2.db", queryAtoms, cwa = cwAtoms, owa = owAtoms)

    /*mln.atomStateDB.foreach{
      case (signature, db) => {
        println(signature+"{")
        db.dumpContents()
        println("}")
      }
    }*/
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    kbmc.buildNetwork
  }

  /**
   * This is an example of probabilistic evidence with closed-world assumption.
   * The absence of a ground probabilistic atom from the evidence is interpreted
   * as an atom with False state value.
   *
   * @return the constructed MRF
   */
  private def prob(): MRF = {
    val queryAtoms = Set(AtomSignature("HoldsAt", 2))
    val probAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    val mln = MLN(path_prefix+"kb.mln", path_prefix+"prob.db", queryAtoms, cwa = probAtoms)

    /*mln.atomStateDB.foreach{
      case (signature, db) => {
        println(signature+"{")
        db.dumpContents()
        println("}")
      }
    }*/
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    kbmc.buildNetwork
  }
}
