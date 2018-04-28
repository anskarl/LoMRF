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

/**
 * LoMRF utilities.
 */
package object lomrf {

  final val NO_ENTRY_KEY = -1
  final val DEFAULT_CAPACITY = 43
  final val DEFAULT_LOAD_FACTOR = 0.75f

  // predicate prefix when functions are converted into auxiliary predicates
  final val AUX_PRED_PREFIX = "AUX"

  // function return value prefix
  final val FUNC_RET_VAR_PREFIX = "funcRetVar"

  final val ASCIILogo =
    """
      |o                        o     o   o         o
      ||             o          |     |\ /|         | /
      ||    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
      ||    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
      |O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
      |            |
      |         o--o
      |o--o              o               o--o       o    o
      ||   |             |               |    o     |    |
      |O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
      ||  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
      |o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
      |
      |Logical Markov Random Fields LoMRF (LoMRF).
    """.stripMargin


  final val processors = sys.runtime.availableProcessors

  object BuildVersion {

    import java.net.URL

    final val version: String = "Version: " + {
      val clazz = lomrf.BuildVersion.getClass
      try {
        val classPath = clazz.getResource("package$" + clazz.getSimpleName + ".class").toString

        if (classPath.startsWith("jar")) {
          val manifestPath = classPath.substring(0, classPath.lastIndexOf("!") + 1) + "/META-INF/MANIFEST.MF"
          val manifest0 = new java.util.jar.Manifest(new URL(manifestPath).openStream)
          val attr = manifest0.getMainAttributes

          //val build = attr.getValue("Implementation-Build")
          val version = attr.getValue("Specification-Version")

          version
        } else "(undefined version)"
      } catch {
        case ex: NullPointerException => "(undefined version)"
      }
    }

    def apply(): String = version

    override def toString: String = version
  }

}
