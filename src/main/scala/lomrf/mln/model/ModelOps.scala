package lomrf.mln.model

import auxlib.log.Logger

object ModelOps {

  private lazy val log = Logger(this.getClass)

  /**
   * Implicit class for operations over mln objects.
   */
  implicit class MLNOps(val mln: MLN) extends AnyVal {

    /**
     * Info about this mln instance.
     */
    def info() = {
      log.info("Markov Logic:"
        + "\n\tConstant domains   : " + mln.evidence.constants.size
        + "\n\tSchema definitions : " + mln.schema.predicates.size
        + "\n\tClauses            : " + mln.clauses.size
        + "\n\tEvidence atoms     : " + mln.cwa.mkString(",")
        + "\n\tNon-evidence atoms : " + mln.owa.mkString(","))
    }
  }

}
