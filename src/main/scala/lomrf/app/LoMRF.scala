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
package lomrf.app

import auxlib.log.Logging
import auxlib.opt.MasterOptionParser

object LoMRF extends App with Logging with MasterOptionParser {

  if(args.isEmpty) {
    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)
  }

  addOpt("infer",   "Perform probabilistic inference", InferenceCLI.main)
  addOpt("wlearn",  "Perform weight learning", WeightLearningCLI.main)
  addOpt("slearn",  "Perform structure learning", StructureLearningCLI.main)
  addOpt("compile", "Perform knowledge base compilation", KBCompilerCLI.main)
  addOpt("diff",    "Perform knowledge base diff", KBDifferenceCLI.main)
  addOpt("export",  "Export a knowledge base into other supported formats", MRFWriterCLI.main)
  addOpt("help",    "Show basic usage", _ => {
    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)

    println(usage)
  })

  parse(args)

}

