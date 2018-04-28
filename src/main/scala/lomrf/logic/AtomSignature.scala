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

package lomrf.logic

import lomrf.mln.model.MLN

import scala.language.implicitConversions
import scala.util.{Failure, Try}

/**
  * Atom signature uniquely identifies an atom. A signature is composed of
  * the atom's symbol and arity (i.e. the number of its arguments).
  *
  * @example {{{
  *           Happens(event, time)
  *           HoldsAt(fluent, time)
  *          }}}
  *
  * The atom signature of ''Happens(event, time)'' is Happens/2, while the
  * signature of ''HoldsAt(fluent, time)'' is HoldsAt/2.
  *
  * <br/><br/>
  *
  * The same signature representation can be used for FOL functions.
  *
  * @example {{{
  *           event walking(person)
  *           fluent meeting(person, person)
  *          }}}
  *
  * The atom signature of function ''walking(person)'' is walking/1, while the
  * atom signature of ''meeting(person, person)'' is meeting/2.
  *
  * @param symbol an atom or function symbol
  * @param arity the atom's or function's arity
  */
final class AtomSignature private(val symbol: String, val arity: Int) extends Serializable {

  @transient val hash: Int = symbol.hashCode ^ arity

  override def equals(obj: Any): Boolean = obj match {
    case other: AtomSignature => other.arity == this.arity && other.symbol == this.symbol
    case _ => false
  }

  override def hashCode: Int = hash

  override def toString: String = symbol + "/" + arity
}

object AtomSignature {

  /**
    * Constructs an atom signature given an atom or function symbol and arity.
    *
    * @param symbol atom/function symbol
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
    * Constructs the signature of a FOL function.
    *
    * @param f the FOL function
    * @return the resulting signature
    */
  def apply(f: TermFunction): AtomSignature = AtomSignature(f.symbol, f.arity)

  /**
    * Parses an atom signature from a string. The result is a Success only
    * in case the given string is a valid signature representation.
    *
    * @param sentence an atom signature as a string
    * @return the resulting signature
    */
  def parseString(sentence: String): Try[AtomSignature] = {
      val pos = sentence.indexOf('/')

      if(pos < 0) Failure(new NoSuchElementException(s"Cannot find the character '/' in the specified sentence ($sentence)"))
      else Try(AtomSignature(sentence.slice(0, pos), sentence.slice(pos + 1, sentence.length).toInt))
  }
}

object AtomSignatureOps {

  implicit def tuple2Signature(tuple: (String, Int)): AtomSignature = AtomSignature(tuple._1, tuple._2)

  implicit class AtomSignatureString(val sentence: String) extends AnyVal{

    /**
      * Parses an atom signature from the string. The result is a Success only
      * in case the string is a valid signature representation.
      *
      * @return the resulting signature
      */
    def signature: Try[AtomSignature] = AtomSignature.parseString(sentence)
  }

  implicit class AtomSignatureInt(val literal: Int) extends AnyVal{

    /**
      * @param mln an MLN
      * @return the atom signature of the integer if present in the given MLN
      */
    def signature(implicit mln: MLN): AtomSignature = mln.space.signatureOf(literal)
  }


}
