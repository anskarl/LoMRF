/*
 * o                        o     o   o         o
 * |             o          |     |\ /|         | /
 * |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 * |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 * O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *             |
 *          o--o
 * o--o              o               o--o       o    o
 * |   |             |               |    o     |    |
 * O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 * |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 * o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 * Logical Markov Random Fields.
 *
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

/**
 * LoMRF utilities.
 */
package object lomrf {

  val NO_ENTRY_KEY = -1
  val DEFAULT_CAPACITY = 43
  val DEFAULT_LOAD_FACTOR = 0.75f

  // predicate prefix when functions are converted into auxiliary predicates
  val AUX_PRED_PREFIX = "AUX"

  val ASCIILogo =
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
    """.stripMargin




  val processors = sys.runtime.availableProcessors

  object BuildVersion {

    import java.net.URL

    val version: String = "LoMRF: " + {
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
