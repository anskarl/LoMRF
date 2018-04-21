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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.app

import auxlib.opt.MasterOptionParser

object LoMRF extends App with MasterOptionParser {

  if(args.isEmpty) {
    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)
  }

  addOpt("infer",   "Perform probabilistic inference", InferenceCLI.main)
  addOpt("wlearn",  "Perform weight learning", WeightLearningCLI.main)
  addOpt("slearn",  "Perform structure learning", StructureLearningCLI.main)
  addOpt("supervision",  "Perform supervision completion", SemiSupervisionCLI.main)
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

