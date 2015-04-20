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
 * Copyright (C) 2012  Anastasios Skarlatidis.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf

import logic.AtomSignature
import lomrf.mln.model.mrf.Constraint
import mln.model.MLN
import gnu.trove.map.TIntObjectMap
import gnu.trove.set.TIntSet
import scalaxy.streams.optimize

/**
 *
 */
package object util {

  object seg {

    @inline
    def fetchKey[T](idx: Int, elements: Array[TIntSet]): Int ={
      var sum = 0
      var found = false
      var segIndex = -1

      while(!found && segIndex < elements.length - 1){
        segIndex += 1
        if((elements(segIndex) ne null) && !elements(segIndex).isEmpty){
          val next_sum = sum + elements(segIndex).size()
          if(idx < next_sum) found = true
          else sum = next_sum
        }
      }

      if(!found) throw new IndexOutOfBoundsException(idx.toString)

      val position = idx - sum
      val iterator = elements(segIndex).iterator()

      optimize {
        for( i <- 0 until position) iterator.next()
      }


      iterator.next()
    }

    @inline
    def fetchKey[T](idx: Int, elements: Array[TIntObjectMap[T]]): Int ={
      var sum = 0
      var found = false
      var segIndex = -1

      while(!found && segIndex < elements.length - 1){
        segIndex += 1
        if((elements(segIndex) ne null) && !elements(segIndex).isEmpty){
          val next_sum = sum + elements(segIndex).size()
          if(idx < next_sum) found = true
          else sum = next_sum
        }
      }

      if(!found) throw new IndexOutOfBoundsException(idx.toString)

      val position = idx - sum
      val iterator = elements(segIndex).iterator()

      optimize{
        for( i <- 0 to position) iterator.advance()
      }

      iterator.key()
    }

    @inline
    def fetchKeyValue[T](idx: Int, elements: Array[TIntObjectMap[T]]): (Int, T) ={
      var sum = 0
      var found = false
      var segIndex = -1

      while(!found && segIndex < elements.length - 1){
        segIndex += 1
        if((elements(segIndex) ne null) && !elements(segIndex).isEmpty){
          val next_sum = sum + elements(segIndex).size()
          if(idx < next_sum) found = true
          else sum = next_sum
        }
      }

      if(!found) throw new IndexOutOfBoundsException(idx.toString)

      val position = idx - sum
      val iterator = elements(segIndex).iterator()

      optimize{
        for( i <- 0 to position) iterator.advance()
      }


      (iterator.key(), iterator.value())
    }

  }

  def parseAtomSignature(s: String): Option[AtomSignature] = {
    s.indexOf('/') match {
      case pos: Int if pos > -1 || pos < s.length => try {
        Some(AtomSignature(s.slice(0, pos), s.slice(pos + 1, s.length).toInt))
      } catch {
        case ex: NumberFormatException => None // error("Cannot parse the arity of "+s)
      }
      case _ => None //error("The arity of query atom "+ s +" is not defined.")
    }

  }

  def decodeLiteral(literal: Int)(implicit mln: MLN): Option[String] = {
    val atomID = math.abs(literal)
    val signature = signatureOf(atomID)
    val idf = mln.space.identities(signature)
    idf.decode(atomID) match{
      case Some(x) => Some((if(literal < 0)"!" else "") + signature.symbol+"("+x.map((t: String) => t).reduceLeft( _ + "," + _ )+")")
      case _ => None
    }
  }

  def decodeAtom(literal: Int)(implicit mln: MLN): Option[String] = {
    val atomID = math.abs(literal)
    val signature = signatureOf(atomID)
    val idf = mln.space.identities(signature)
    idf.decode(atomID) match{
      case Some(x) => Some( signature.symbol+"("+x.map((t: String) => t).reduceLeft( _ + "," + _ )+")")
      case _ => None
    }
  }

  def decodeFeature(feature: Constraint, hardWeight: Double = 0)(implicit mln: MLN): Option[String] ={
    val buffer = new StringBuilder()

    feature.getWeight match {
      case Double.NaN =>
      case Double.PositiveInfinity =>
        if(hardWeight != 0) buffer.append(hardWeight.toString)

        buffer.append(' ')
      case _ =>
        buffer.append(feature.getWeight.toString)
        buffer.append(' ')
    }

    optimize {
      for(i <- feature.literals.indices){
        decodeLiteral(feature.literals(i)) match{
          case Some(litTXT) =>
            buffer.append(litTXT)
            if(i != feature.literals.length - 1) buffer.append(" v ")
          case None => return None
        }
      }
    }

    if(feature.getWeight.isInfinite && hardWeight != 0) buffer.append('.')


    Some(buffer.result())
  }

  def signatureOf(literal: Int)(implicit mln: MLN): AtomSignature = {

    val atomID = math.abs(literal)
    val result = java.util.Arrays.binarySearch(mln.space.orderedStartIDs, atomID)
    val position = if(result < 0) (-result) - 2 else result

    mln.space.orderedAtomSignatures(position)
  }


}
