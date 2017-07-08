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

package lomrf.logic

/**
  * The state of a ground atoms in a ''First Order Logic'' knowledge base can be:
  *
  * <ol>
  *   <li>[[lomrf.logic.TRUE `TRUE`]]</li>
  *   <li>[[lomrf.logic.FALSE `FALSE`]]</li>
  *   <li>[[lomrf.logic.UNKNOWN `UNKNOWN`]]</li>
  * </ol>
  */
sealed trait TriState {

  val value: Byte

  /**
    * Gives the conjunction of two states.
    *
    * @example For a state X and a state Y, the table displays all possible conjunction outcomes:
    *
    * <table style="border-collapse: separate; border-spacing: 5px;">
    *   <tr><td></td>   <td>Y</td>        <td></td>         <td></td>       <td></td></tr>
    *   <tr><td>X</td>  <td></td>         <td>True</td>     <td>False</td>  <td>Unknown</td></tr>
    *   <tr><td></td>   <td>True</td>     <td>True</td>     <td>False</td>  <td>Unknown</td></tr>
    *   <tr><td></td>   <td>False</td>    <td>False</td>    <td>False</td>  <td>False</td></tr>
    *   <tr><td></td>   <td>Unknown</td>  <td>Unknown</td>  <td>False</td>  <td>Unknown</td></tr>
    * </table>
    */
  def ^(other: TriState): TriState = {
    val result = math.min(this.value, other.value)

    if (result == 1) TRUE
    else if (result == -1) FALSE
    else UNKNOWN
  }

  /**
    * Gives the disjunction of two states.
    *
    * @example For a state X and a state Y, the table displays all possible disjunction outcomes:
    *
    * <table style="border-collapse: separate; border-spacing: 5px;">
    *   <tr><td></td>   <td>Y</td>        <td></td>      <td></td>         <td></td></tr>
    *   <tr><td>X</td>  <td></td>         <td>True</td>  <td>False</td>    <td>Unknown</td></tr>
    *   <tr><td></td>   <td>True</td>     <td>True</td>  <td>True</td>     <td>True</td></tr>
    *   <tr><td></td>   <td>False</td>    <td>True</td>  <td>False</td>    <td>Unknown</td></tr>
    *   <tr><td></td>   <td>Unknown</td>  <td>True</td>  <td>Unknown</td>  <td>Unknown</td></tr>
    * </table>
    */
  def v(other: TriState): TriState = {
    val result = math.max(this.value, other.value)

    if (result == 1) TRUE
    else if (result == -1) FALSE
    else UNKNOWN
  }

  /**
    * Flips the current state.
    *
    * @example For a state X, the table displays all possible outcomes:
    *
    * <table style="border-collapse: separate; border-spacing: 5px;">
    *   <tr><td>X</td>           <td>True</td>   <td>False</td>  <td>Unknown</td></tr>
    *   <tr><td>Flipped X</td>   <td>False</td>  <td>True</td>   <td>Unknown</td></tr>
    * </table>
    */
  def flip: TriState =
    if (this.value == -1) TRUE else if (this.value == 1) FALSE else UNKNOWN

  /**
    * Each state has a unique value that also represents its hash code.
    * @return the hash code value for this state
    */
  override def hashCode(): Int = value

  /**
    * Compares this state to another object.
    *
    * @param obj the object to compare this state
    * @return true if obj is a `TriState` object and has identical value
    */
  override def equals(obj: Any): Boolean = obj match {
    case other: TriState => other.value == this.value
    case _ => false
  }
}

/**
 * This object represents a `TRUE` state.
 */
case object TRUE extends TriState {

  val value: Byte = 1

  override def toString = "True"
}

/**
 * This object represents a `FALSE` state.
 */
case object FALSE extends TriState {

  val value: Byte = -1

  override def toString = "False"
}

/**
 * This object represents an `UNKNOWN` state.
 */
case object UNKNOWN extends TriState {

  val value: Byte = 0
  
  override def toString = "Unknown"
}
