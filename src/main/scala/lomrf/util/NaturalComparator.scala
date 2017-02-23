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

package lomrf.util

object NaturalComparator {

  private def isDigit(ch: Char) = ch >= 48 && ch <= 57

  private def getChunkOffset(s: String, length: Int, mark: Int): Int = {
    var marker = mark + 1
    var c = s.charAt(marker)

    if (isDigit(c)) while (marker < length) {
      c = s.charAt(marker)
      if (!isDigit(c)) return marker
      marker += 1
    }
    else while (marker < length) {
      c = s.charAt(marker)
      if (isDigit(c)) return marker
      marker += 1
    }
    marker
  }

  private def compareRegion(s1: String, off1: Int, len1: Int, s2: String, off2: Int, len2: Int): Int = {
    val lim = Math.min(len1, len2)
    var i = 0

    while (i < lim) {
      val c1 = s1.charAt(off1 + i)
      val c2 = s2.charAt(off2 + i)
      if (c1 != c2) return c1 - c2
      i += 1
    }
    len1 - len2
  }

  def compare(s1: String, s2: String): Int = {
    var thisMarker = 0
    var thatMarker = 0
    val s1Length = s1.length
    val s2Length = s2.length
    while (thisMarker < s1Length && thatMarker < s2Length) {
      if (s1.charAt(thisMarker) == s2.charAt(thatMarker)) {
        thisMarker += 1
        thatMarker += 1
      }
      else {
        var result = 0
        val thisChunk = getChunkOffset(s1, s1Length, thisMarker)
        val thatChunk = getChunkOffset(s2, s2Length, thatMarker)
        val thisChunkLength = thisChunk - thisMarker
        val thatChunkLength = thatChunk - thatMarker

        if (isDigit(s1.charAt(thisMarker)) && isDigit(s2.charAt(thatMarker))) {
          result = thisChunkLength - thatChunkLength
          if (result == 0) {
            var i = thisMarker
            var j = thatMarker
            while (i < thisChunk) {
              result = s1.charAt(i) - s2.charAt(j)
              if (result != 0) return result
              i += 1
              j += 1
            }
          }
        }
        else result =
          compareRegion(s1, thisMarker, thisChunkLength, s2, thatMarker, thatChunkLength)

        if (result != 0) return result

        thisMarker = thisChunk
        thatMarker = thatChunk
      }
    }
    s1Length - s2Length
  }

  def compareBool(s1: String, s2: String): Boolean = compare(s1, s2) < 0
}