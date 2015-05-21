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

package lomrf.logic

import lomrf.mln.model.MLN

import scala.util.{Failure, Try}

/**
 * Atom signature defines uniquely an atom. A signature is composed of the atom's name and atom's arity (i.e. the number of its arguments).
 *
 * @example {{{
 *  Happens(event, time)
 *  HoldsAt(fluent, time)
 * }}}
 *
 * In the above example, the atom signature of ''Happens(event, time)'' is Happens/2, while the signature of ''HoldsAt(fluent, time)'' is HoldsAt/2.
 * <br/><br/>
 * The same signature representation can be used of FOL functions.
 * @example {{{
 * event walking(person)
 * fluent meeting(person, person)
 * }}}
 *
 * The atom signature of function ''walking(person)'' is walking/1 and similarly for the ''meeting(person, person)'' the signature is meeting/2.
 *
 * @param symbol atom's/function's name (e.g. predicates ''Happens'', ''HoldsAt'', as well as functions ''walking'' and ''meeting'').
 * @param arity atom's arity (e.g. both example predicates have 2 arguments).
 *
 *
 */
final class AtomSignature private(val symbol: String, val arity: Int) {

  val hash = symbol.hashCode ^ arity

  override def equals(obj: Any): Boolean = obj match {
    case other: AtomSignature => other.arity == this.arity && other.symbol == this.symbol
    case _ => false
  }

  override def hashCode() = hash

  override def toString = symbol + "/" + arity

}

object AtomSignature {

  /**
   * Constructs an atom signature from atom's/function's name and arity.
   *
   * @param symbol atom/function name
   * @param arity number of arguments
   *
   * @return the resulting signature
   */
  def apply(symbol: String, arity: Int): AtomSignature = new AtomSignature(symbol, arity)

  /**
   * Constructs the signature of an atomic formula.
   *
   * @param atom an atomic formula (also known as atom or predicate)
   *
   * @return the resulting signature
   */
  def apply(atom: AtomicFormula): AtomSignature = AtomSignature(atom.symbol, atom.arity)

  /**
   * Constructs the signature of an FOL formula.
   *
   * @param fun the FOL function
   * @return the resulting signature
   */
  def apply(fun: TermFunction): AtomSignature = AtomSignature(fun.symbol, fun.arity)


  def parseString(sentence: String): Try[AtomSignature] = {
      val pos = sentence.indexOf('/')

      if(pos < 0) Failure(new NoSuchElementException(s"Cannot find the character '/' in the specified sentence ($sentence)"))
      else Try(AtomSignature(sentence.slice(0, pos), sentence.slice(pos + 1, sentence.length).toInt))
  }

  def signatureOf(literal: Int)(implicit mln: MLN): AtomSignature = {

    val atomID = math.abs(literal)
    val result = java.util.Arrays.binarySearch(mln.evidence.predicateSpace.orderedStartIDs, atomID)
    val position = if(result < 0) (-result) - 2 else result

    mln.evidence.predicateSpace.orderedAtomSignatures(position)
  }

}

object AtomSignatureOps {

  implicit def tuple2Signature(tuple: (String, Int)): AtomSignature = AtomSignature(tuple._1, tuple._2)

  implicit class AtomSignatureString(val sentence: String) extends AnyVal{
    def signature: Try[AtomSignature] = AtomSignature.parseString(sentence)
  }

  implicit class AtomSignatureInt(val literal: Int) extends AnyVal{
    def signature(implicit mln: MLN): AtomSignature = AtomSignature.signatureOf(literal)
  }


}
