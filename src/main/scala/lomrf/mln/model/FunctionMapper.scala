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

package lomrf.mln.model

import gnu.trove.map.TIntObjectMap
import lomrf.mln.model.builders.FunctionMapperBuilder

trait FunctionMapper extends Serializable {

  /**
    * Gives the associated string value for a given vector
    * of string arguments or '''null''' in case no value exists.
    *
    * @param args a vector of string arguments
    * @return the associated value for the given arguments
    *         if one exists, null otherwise
    */
  def apply(args: Vector[String]): String

  /**
    * Gets the associated string value for given vector of
    * string arguments, if one exists.
    *
    * @param args a vector of string arguments
    * @return the associated value for the given arguments
    *         if one exists, none otherwise
    */
  def get(args: Vector[String]): Option[String]
}

/**
  * A default function mapper that uses an atom identity function
  * and a map of encoded string arguments to associate input string
  * arguments to string values.
  *
  * @param identityFunction an atom identity function
  * @param args2Value a map from encoded string argument ids (integers) to string values
  */
final class FunctionMapperDefaultImpl(
    identityFunction: AtomIdentityFunction,
    args2Value: TIntObjectMap[String]) extends FunctionMapper {

  def apply(args: Vector[String]): String = {
    val id = identityFunction.encode(args)
    args2Value.get(id)
  }

  def get(args: Vector[String]): Option[String] = {
    val id = identityFunction.encode(args)
    val result = args2Value.get(id)
    if (result eq null) None else Some(result)
  }

  override def toString =
    s"FunctionMapperDefaultImpl{signature:= ${identityFunction.signature}, size:=${args2Value.size}}"
}

/**
  * A special function mapper that uses a function to associate input
  * string arguments to string values. The function should encode the
  * logic of the function mapper.
  *
  * @example {{{
  *           val concat = (x: Vector[String]) => x.reduce(_ ++ _)
  *           val fm = new FunctionMapperSpecialImpl(concat)
  * }}}
  *
  * @param f a function from vector of string arguments to string values
  */
final class FunctionMapperSpecialImpl(f: Vector[String] => String) extends FunctionMapper {

  def apply(args: Vector[String]): String = f(args)

  def get(args: Vector[String]): Option[String] = Some(f(args))

  override def toString = s"FunctionMapperSpecialImpl{f:= $f}"
}

object FunctionMapper {

  /**
    * Create a function mapper from a given builder.
    *
    * @param builder a function mapper builder
    * @return a FunctionMapper instance
    */
  def apply(builder: FunctionMapperBuilder): FunctionMapper = builder.result()

  /**
    * Create a function mapper from a given atom identity function
    * and a map of encoded string arguments to string values.
    * @see [[lomrf.mln.model.FunctionMapperDefaultImpl]]
    *
    * @param identityFunction an atom identity function
    * @param args2Value a map from encoded string arguments ids (integers) to string values
    * @return a FunctionMapper instance
    */
  def apply(identityFunction: AtomIdentityFunction, args2Value: TIntObjectMap[String]): FunctionMapper =
    new FunctionMapperDefaultImpl(identityFunction, args2Value)

  /**
    * Creates a function mapper from a given function.
    * @see [[lomrf.mln.model.FunctionMapperSpecialImpl]]
    *
    * @param f a function from vector of string arguments to string values
    * @return a FunctionMapper instance
    */
  def apply(f: Vector[String] => String): FunctionMapper = new FunctionMapperSpecialImpl(f)
}
