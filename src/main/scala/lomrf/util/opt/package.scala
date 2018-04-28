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

package lomrf.util

package object opt {

  final val NL = System.getProperty("line.separator")
  final val TB = "        " //8 whitespace chars
  final val NLTB = NL + TB
  final val NLNL = NL + NL

  final val defaultKeyName = "<key>"
  final val defaultValueName = "<value>"

  def wrapText(description: String, maxLength: Int): String = {
    if(description.length < maxLength) description
    else if(description.substring(0,maxLength).contains(NL)){
      val idxNL = description.indexOf(NL)
      description.substring(0, idxNL).trim() +NLTB+wrapText(description.substring(idxNL+1).trim(), maxLength)
    }
    else{
      val idx = math.max(math.max(description.lastIndexOf(" ", maxLength), description.lastIndexOf(TB,maxLength)), description.lastIndexOf("-",maxLength))
      description.substring(0, idx).trim()+NLTB+wrapText(description.substring(idx).trim(), maxLength)
    }
  }
}
