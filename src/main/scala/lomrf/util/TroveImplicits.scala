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

package lomrf.util

/**
 * This object contains utility implicit definitions that convert
 * Scala's functions to the corresponding Trove Functions and Procedures.
 *
 * The code is based on the following blog article:
 * http://blog.juma.me.uk/tag/trove/
 *
 * @author Anastasios Skarlatidis
 */
object TroveImplicits {

  import gnu.trove.procedure._
  import gnu.trove.function._


  /* ----------------------------------------------------------
   * Scala functions to Trove Functions implicit definitions
   * ----------------------------------------------------------
   */

  implicit def toByteFunction(f: Byte => Byte): TByteFunction = new TByteFunction {
    def execute(p1: Byte) = f(p1)
  }

  implicit def toShortFunction(f: Short => Short): TShortFunction = new TShortFunction {
    def execute(p1: Short) = f(p1)
  }

  implicit def toIntFunction(f: Int => Int): TIntFunction = new TIntFunction {
    def execute(p1: Int) = f(p1)
  }

  implicit def toLongFunction(f: Long => Long): TLongFunction = new TLongFunction {
    def execute(p1: Long) = f(p1)
  }

  implicit def toFloatFunction(f: Float => Float): TFloatFunction = new TFloatFunction {
    def execute(p1: Float) = f(p1)
  }

  implicit def toDoubleFunction(f: Double => Double): TDoubleFunction = new TDoubleFunction {
    def execute(p1: Double) = f(p1)
  }


  implicit def toObjectFunction[T, R](f: T => R): TObjectFunction[T, R] = new TObjectFunction[T, R] {
    def execute(p1: T): R = f(p1)
  }


  /* ----------------------------------------------------------
  * Scala functions to Trove Procedures implicit definitions
  * ----------------------------------------------------------
  */

  //===================== Byte =====================

  trait ByteProcedure[+R] extends ((Byte) => R) {
    def apply(i: Byte): R
  }

  trait ByteByteProcedure[+R] extends ((Byte, Byte) => R) {
    def apply(v1: Byte, v2: Byte): R
  }

  trait ByteShortProcedure[+R] extends ((Byte, Short) => R) {
    def apply(v1: Byte, v2: Short): R
  }

  trait ByteIntProcedure[+R] extends ((Byte, Int) => R) {
    def apply(v1: Byte, v2: Int): R
  }

  trait ByteLongProcedure[+R] extends ((Byte, Long) => R) {
    def apply(v1: Byte, v2: Long): R
  }

  trait ByteFloatProcedure[+R] extends ((Byte, Float) => R) {
    def apply(v1: Byte, v2: Float): R
  }

  trait ByteDoubleProcedure[+R] extends ((Byte, Double) => R) {
    def apply(v1: Byte, v2: Double): R
  }

  trait ByteObjectProcedure[O, +R] extends ((Byte, O) => R) {
    def apply(v1: Byte, v2: O): R
  }

  /*implicit def toByteFunction(f: Byte => Byte): TByteFunction = new TByteFunction {
    def execute(p1: Byte) = f(p1)
  }*/

  implicit def toByteProcedure(f: Byte => Unit): TByteProcedure = new TByteProcedure() {
    def execute(p1: Byte) = {
      f.asInstanceOf[ByteProcedure[_]](p1)
      true
    }
  }

  implicit def toByteByteProcedure(f: (Byte, Byte) => Unit): TByteByteProcedure = new TByteByteProcedure() {
    def execute(p1: Byte, p2: Byte) = {
      f.asInstanceOf[ByteByteProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toByteShortProcedure(f: (Byte, Short) => Unit): TByteShortProcedure = new TByteShortProcedure() {
    def execute(p1: Byte, p2: Short) = {
      f.asInstanceOf[ByteShortProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toByteIntProcedure(f: (Byte, Int) => Unit): TByteIntProcedure = new TByteIntProcedure() {
    def execute(p1: Byte, p2: Int) = {
      f.asInstanceOf[ByteIntProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toByteLongProcedure(f: (Byte, Long) => Unit): TByteLongProcedure = new TByteLongProcedure() {
    def execute(p1: Byte, p2: Long) = {
      f.asInstanceOf[ByteLongProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toByteFloatProcedure(f: (Byte, Float) => Unit): TByteFloatProcedure = new TByteFloatProcedure() {
    def execute(p1: Byte, p2: Float) = {
      f.asInstanceOf[ByteFloatProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toByteDoubleProcedure(f: (Byte, Double) => Unit): TByteDoubleProcedure = new TByteDoubleProcedure() {
    def execute(p1: Byte, p2: Double) = {
      f.asInstanceOf[ByteDoubleProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toByteObjectProcedure[O](f: (Byte, O) => Unit): TByteObjectProcedure[O] = new TByteObjectProcedure[O]() {
    def execute(p1: Byte, p2: O) = {
      f.asInstanceOf[ByteObjectProcedure[O, _]](p1, p2)
      true
    }
  }


  //===================== Short =====================

  trait ShortProcedure[+R] extends ((Short) => R) {
    def apply(i: Short): R
  }

  trait ShortByteProcedure[+R] extends ((Short, Byte) => R) {
    def apply(v1: Short, v2: Byte): R
  }

  trait ShortShortProcedure[+R] extends ((Short, Short) => R) {
    def apply(v1: Short, v2: Short): R
  }

  trait ShortIntProcedure[+R] extends ((Short, Int) => R) {
    def apply(v1: Short, v2: Int): R
  }

  trait ShortLongProcedure[+R] extends ((Short, Long) => R) {
    def apply(v1: Short, v2: Long): R
  }

  trait ShortFloatProcedure[+R] extends ((Short, Float) => R) {
    def apply(v1: Short, v2: Float): R
  }

  trait ShortDoubleProcedure[+R] extends ((Short, Double) => R) {
    def apply(v1: Short, v2: Double): R
  }

  trait ShortObjectProcedure[O, +R] extends ((Short, O) => R) {
    def apply(v1: Short, v2: O): R
  }

  implicit def toShortProcedure(f: Short => Unit): TShortProcedure = new TShortProcedure() {
    def execute(p1: Short) = {
      f.asInstanceOf[ShortProcedure[_]](p1)
      true
    }
  }

  implicit def toShortByteProcedure(f: (Short, Byte) => Unit): TShortByteProcedure = new TShortByteProcedure() {
    def execute(p1: Short, p2: Byte) = {
      f.asInstanceOf[ShortByteProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toByteShortProcedure(f: (Short, Short) => Unit): TShortShortProcedure = new TShortShortProcedure() {
    def execute(p1: Short, p2: Short) = {
      f.asInstanceOf[ShortShortProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toShortIntProcedure(f: (Short, Int) => Unit): TShortIntProcedure = new TShortIntProcedure() {
    def execute(p1: Short, p2: Int) = {
      f.asInstanceOf[ShortIntProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toShortLongProcedure(f: (Short, Long) => Unit): TShortLongProcedure = new TShortLongProcedure() {
    def execute(p1: Short, p2: Long) = {
      f.asInstanceOf[ShortLongProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toShortFloatProcedure(f: (Short, Float) => Unit): TShortFloatProcedure = new TShortFloatProcedure() {
    def execute(p1: Short, p2: Float) = {
      f.asInstanceOf[ShortFloatProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toShortDoubleProcedure(f: (Short, Double) => Unit): TShortDoubleProcedure = new TShortDoubleProcedure() {
    def execute(p1: Short, p2: Double) = {
      f.asInstanceOf[ShortDoubleProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toShortObjectProcedure[O](f: (Short, O) => Unit): TShortObjectProcedure[O] = new TShortObjectProcedure[O]() {
    def execute(p1: Short, p2: O) = {
      f.asInstanceOf[ShortObjectProcedure[O, _]](p1, p2)
      true
    }
  }

  //===================== Int =====================
  trait IntProcedure[+R] extends ((Int) => R) {
    def apply(i: Int): R
  }

  trait IntByteProcedure[+R] extends ((Int, Byte) => R) {
    def apply(v1: Int, v2: Byte): R
  }

  trait IntShortProcedure[+R] extends ((Int, Short) => R) {
    def apply(v1: Int, v2: Short): R
  }

  trait IntIntProcedure[+R] extends ((Int, Int) => R) {
    def apply(v1: Int, v2: Int): R
  }

  trait IntLongProcedure[+R] extends ((Int, Long) => R) {
    def apply(v1: Int, v2: Long): R
  }

  trait IntFloatProcedure[+R] extends ((Int, Float) => R) {
    def apply(v1: Int, v2: Float): R
  }

  trait IntDoubleProcedure[+R] extends ((Int, Double) => R) {
    def apply(v1: Int, v2: Double): R
  }

  trait IntObjectProcedure[O, +R] extends ((Int, O) => R) {
    def apply(v1: Int, v2: O): R
  }

  implicit def toIntProcedure(f: Int => Unit): TIntProcedure = new TIntProcedure() {
    def execute(p1: Int) = {
      f.asInstanceOf[IntProcedure[_]](p1)
      true
    }
  }

  implicit def toIntByteProcedure(f: (Int, Byte) => Unit): TIntByteProcedure = new TIntByteProcedure() {
    def execute(p1: Int, p2: Byte) = {
      f.asInstanceOf[IntByteProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toIntShortProcedure(f: (Int, Short) => Unit): TIntShortProcedure = new TIntShortProcedure() {
    def execute(p1: Int, p2: Short) = {
      f.asInstanceOf[IntShortProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toIntIntProcedure(f: (Int, Int) => Unit): TIntIntProcedure = new TIntIntProcedure() {
    def execute(p1: Int, p2: Int) = {
      f.asInstanceOf[IntIntProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toIntLongProcedure(f: (Int, Long) => Unit): TIntLongProcedure = new TIntLongProcedure() {
    def execute(p1: Int, p2: Long) = {
      f.asInstanceOf[IntLongProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toIntFloatProcedure(f: (Int, Float) => Unit): TIntFloatProcedure = new TIntFloatProcedure() {
    def execute(p1: Int, p2: Float) = {
      f.asInstanceOf[IntFloatProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toIntDoubleProcedure(f: (Int, Double) => Unit): TIntDoubleProcedure = new TIntDoubleProcedure() {
    def execute(p1: Int, p2: Double) = {
      f.asInstanceOf[IntDoubleProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toIntObjectProcedure[O](f: (Int, O) => Unit): TIntObjectProcedure[O] = new TIntObjectProcedure[O]() {
    def execute(p1: Int, p2: O) = {
      f.asInstanceOf[IntObjectProcedure[O, _]](p1, p2)
      true
    }
  }

  //===================== Long =====================
  trait LongProcedure[+R] extends ((Long) => R) {
    def apply(i: Long): R
  }

  trait LongByteProcedure[+R] extends ((Long, Byte) => R) {
    def apply(v1: Long, v2: Byte): R
  }

  trait LongShortProcedure[+R] extends ((Long, Short) => R) {
    def apply(v1: Long, v2: Short): R
  }

  trait LongIntProcedure[+R] extends ((Long, Int) => R) {
    def apply(v1: Long, v2: Int): R
  }

  trait LongLongProcedure[+R] extends ((Long, Long) => R) {
    def apply(v1: Long, v2: Long): R
  }

  trait LongFloatProcedure[+R] extends ((Long, Float) => R) {
    def apply(v1: Long, v2: Float): R
  }

  trait LongDoubleProcedure[+R] extends ((Long, Double) => R) {
    def apply(v1: Long, v2: Double): R
  }

  trait LongObjectProcedure[O, +R] extends ((Long, O) => R) {
    def apply(v1: Long, v2: O): R
  }

  implicit def toLongProcedure(f: Long => Unit): TLongProcedure = new TLongProcedure() {
    def execute(p1: Long) = {
      f.asInstanceOf[LongProcedure[_]](p1)
      true
    }
  }

  implicit def toLongByteProcedure(f: (Long, Byte) => Unit): TLongByteProcedure = new TLongByteProcedure() {
    def execute(p1: Long, p2: Byte) = {
      f.asInstanceOf[LongByteProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongShortProcedure(f: (Long, Short) => Unit): TLongShortProcedure = new TLongShortProcedure() {
    def execute(p1: Long, p2: Short) = {
      f.asInstanceOf[LongShortProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongIntProcedure(f: (Long, Int) => Unit): TLongIntProcedure = new TLongIntProcedure() {
    def execute(p1: Long, p2: Int) = {
      f.asInstanceOf[LongIntProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongLongProcedure(f: (Long, Long) => Unit): TLongLongProcedure = new TLongLongProcedure() {
    def execute(p1: Long, p2: Long) = {
      f.asInstanceOf[LongLongProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongFloatProcedure(f: (Long, Float) => Unit): TLongFloatProcedure = new TLongFloatProcedure() {
    def execute(p1: Long, p2: Float) = {
      f.asInstanceOf[LongFloatProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongDoubleProcedure(f: (Long, Double) => Unit): TLongDoubleProcedure = new TLongDoubleProcedure() {
    def execute(p1: Long, p2: Double) = {
      f.asInstanceOf[LongDoubleProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongObjectProcedure[O](f: (Long, O) => Unit): TLongObjectProcedure[O] = new TLongObjectProcedure[O]() {
    def execute(p1: Long, p2: O) = {
      f.asInstanceOf[LongObjectProcedure[O, _]](p1, p2)
      true
    }
  }

  //===================== Float =====================
  trait FloatProcedure[+R] extends ((Float) => R) {
    def apply(i: Float): R
  }

  trait FloatByteProcedure[+R] extends ((Float, Byte) => R) {
    def apply(v1: Float, v2: Byte): R
  }

  trait FloatShortProcedure[+R] extends ((Float, Short) => R) {
    def apply(v1: Float, v2: Short): R
  }

  trait FloatIntProcedure[+R] extends ((Float, Int) => R) {
    def apply(v1: Float, v2: Int): R
  }

  trait FloatLongProcedure[+R] extends ((Float, Long) => R) {
    def apply(v1: Float, v2: Long): R
  }

  trait FloatFloatProcedure[+R] extends ((Float, Float) => R) {
    def apply(v1: Float, v2: Float): R
  }

  trait FloatDoubleProcedure[+R] extends ((Float, Double) => R) {
    def apply(v1: Float, v2: Double): R
  }

  trait FloatObjectProcedure[O, +R] extends ((Float, O) => R) {
    def apply(v1: Float, v2: O): R
  }

  implicit def toFloatProcedure(f: Float => Unit): TFloatProcedure = new TFloatProcedure() {
    def execute(p1: Float) = {
      f.asInstanceOf[FloatProcedure[_]](p1)
      true
    }
  }

  implicit def toFloatByteProcedure(f: (Float, Byte) => Unit): TFloatByteProcedure = new TFloatByteProcedure() {
    def execute(p1: Float, p2: Byte) = {
      f.asInstanceOf[FloatByteProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongShortProcedure(f: (Float, Short) => Unit): TFloatShortProcedure = new TFloatShortProcedure() {
    def execute(p1: Float, p2: Short) = {
      f.asInstanceOf[FloatShortProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toLongIntProcedure(f: (Float, Int) => Unit): TFloatIntProcedure = new TFloatIntProcedure() {
    def execute(p1: Float, p2: Int) = {
      f.asInstanceOf[FloatIntProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toFloatLongProcedure(f: (Float, Long) => Unit): TFloatLongProcedure = new TFloatLongProcedure() {
    def execute(p1: Float, p2: Long) = {
      f.asInstanceOf[FloatLongProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toFloatFloatProcedure(f: (Float, Float) => Unit): TFloatFloatProcedure = new TFloatFloatProcedure() {
    def execute(p1: Float, p2: Float) = {
      f.asInstanceOf[FloatFloatProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toFloatDoubleProcedure(f: (Float, Double) => Unit): TFloatDoubleProcedure = new TFloatDoubleProcedure() {
    def execute(p1: Float, p2: Double) = {
      f.asInstanceOf[FloatDoubleProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toFloatObjectProcedure[O](f: (Float, O) => Unit): TFloatObjectProcedure[O] = new TFloatObjectProcedure[O]() {
    def execute(p1: Float, p2: O) = {
      f.asInstanceOf[FloatObjectProcedure[O, _]](p1, p2)
      true
    }
  }

  //===================== Double =====================
  trait DoubleProcedure[+R] extends ((Double) => R) {
    def apply(i: Double): R
  }

  trait DoubleByteProcedure[+R] extends ((Double, Byte) => R) {
    def apply(v1: Double, v2: Byte): R
  }

  trait DoubleShortProcedure[+R] extends ((Double, Short) => R) {
    def apply(v1: Double, v2: Short): R
  }

  trait DoubleIntProcedure[+R] extends ((Double, Int) => R) {
    def apply(v1: Double, v2: Int): R
  }

  trait DoubleLongProcedure[+R] extends ((Double, Long) => R) {
    def apply(v1: Double, v2: Long): R
  }

  trait DoubleFloatProcedure[+R] extends ((Double, Float) => R) {
    def apply(v1: Double, v2: Float): R
  }

  trait DoubleDoubleProcedure[+R] extends ((Double, Double) => R) {
    def apply(v1: Double, v2: Double): R
  }

  trait DoubleObjectProcedure[O, +R] extends ((Double, O) => R) {
    def apply(v1: Double, v2: O): R
  }

  implicit def toDoubleProcedure(f: Double => Unit): TDoubleProcedure = new TDoubleProcedure() {
    def execute(p1: Double) = {
      f.asInstanceOf[DoubleProcedure[_]](p1)
      true
    }
  }

  implicit def toDoubleByteProcedure(f: (Double, Byte) => Unit): TDoubleByteProcedure = new TDoubleByteProcedure() {
    def execute(p1: Double, p2: Byte) = {
      f.asInstanceOf[DoubleByteProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toDoubleShortProcedure(f: (Double, Short) => Unit): TDoubleShortProcedure = new TDoubleShortProcedure() {
    def execute(p1: Double, p2: Short) = {
      f.asInstanceOf[DoubleShortProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toDoubleIntProcedure(f: (Double, Int) => Unit): TDoubleIntProcedure = new TDoubleIntProcedure() {
    def execute(p1: Double, p2: Int) = {
      f.asInstanceOf[DoubleIntProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toDoubleLongProcedure(f: (Double, Long) => Unit): TDoubleLongProcedure = new TDoubleLongProcedure() {
    def execute(p1: Double, p2: Long) = {
      f.asInstanceOf[DoubleLongProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toDoubleFloatProcedure(f: (Double, Float) => Unit): TDoubleFloatProcedure = new TDoubleFloatProcedure() {
    def execute(p1: Double, p2: Float) = {
      f.asInstanceOf[DoubleFloatProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toDoubleDoubleProcedure(f: (Double, Double) => Unit): TDoubleDoubleProcedure = new TDoubleDoubleProcedure() {
    def execute(p1: Double, p2: Double) = {
      f.asInstanceOf[DoubleDoubleProcedure[_]](p1, p2)
      true
    }
  }

  implicit def toDoubleObjectProcedure[O](f: (Double, O) => Unit): TDoubleObjectProcedure[O] = new TDoubleObjectProcedure[O]() {
    def execute(p1: Double, p2: O) = {
      f.asInstanceOf[DoubleObjectProcedure[O, _]](p1, p2)
      true
    }
  }


  //===================== Object =====================
  trait ObjectProcedure[O, +R] extends ((O) => R) {
    def apply(i: O): R
  }

  trait ObjectByteProcedure[O, +R] extends ((O, Byte) => R) {
    def apply(v1: O, v2: Byte): R
  }

  trait ObjectShortProcedure[O, +R] extends ((O, Short) => R) {
    def apply(v1: O, v2: Short): R
  }

  trait ObjectIntProcedure[O, +R] extends ((O, Int) => R) {
    def apply(v1: O, v2: Int): R
  }

  trait ObjectLongProcedure[O, +R] extends ((O, Long) => R) {
    def apply(v1: O, v2: Long): R
  }

  trait ObjectFloatProcedure[O, +R] extends ((O, Float) => R) {
    def apply(v1: O, v2: Float): R
  }

  trait ObjectDoubleProcedure[O, +R] extends ((O, Double) => R) {
    def apply(v1: O, v2: Double): R
  }

  trait ObjectObjectProcedure[O1, O2, +R] extends ((O1, O2) => R) {
    def apply(v1: O1, v2: O2): R
  }


  implicit def toObjectProcedure[O](f: O => Unit): TObjectProcedure[O] = new TObjectProcedure[O]() {
    def execute(p1: O) = {
      f.asInstanceOf[ObjectProcedure[O, _]](p1)
      true
    }
  }

  implicit def toObjectByteProcedure[O](f: (O, Byte) => Unit): TObjectByteProcedure[O] = new TObjectByteProcedure[O]() {
    def execute(p1: O, p2: Byte) = {
      f.asInstanceOf[ObjectByteProcedure[O, _]](p1, p2)
      true
    }
  }

  implicit def toObjectShortProcedure[O](f: (O, Short) => Unit): TObjectShortProcedure[O] = new TObjectShortProcedure[O]() {
    def execute(p1: O, p2: Short) = {
      f.asInstanceOf[ObjectShortProcedure[O, _]](p1, p2)
      true
    }
  }

  implicit def toObjectIntProcedure[O](f: (O, Int) => Unit): TObjectIntProcedure[O] = new TObjectIntProcedure[O]() {
    def execute(p1: O, p2: Int) = {
      f.asInstanceOf[ObjectIntProcedure[O, _]](p1, p2)
      true
    }
  }

  implicit def toObjectLongProcedure[O](f: (O, Long) => Unit): TObjectLongProcedure[O] = new TObjectLongProcedure[O]() {
    def execute(p1: O, p2: Long) = {
      f.asInstanceOf[ObjectLongProcedure[O, _]](p1, p2)
      true
    }
  }

  implicit def toObjectFloatProcedure[O](f: (O, Float) => Unit): TObjectFloatProcedure[O] = new TObjectFloatProcedure[O]() {
    def execute(p1: O, p2: Float) = {
      f.asInstanceOf[ObjectFloatProcedure[O, _]](p1, p2)
      true
    }
  }

  implicit def toObjectDoubleProcedure[O](f: (O, Double) => Unit): TObjectDoubleProcedure[O] = new TObjectDoubleProcedure[O]() {
    def execute(p1: O, p2: Double) = {
      f.asInstanceOf[ObjectDoubleProcedure[O, _]](p1, p2)
      true
    }
  }

  implicit def toObjectObjectProcedure[O1, O2](f: (O1, O2) => Unit): TObjectObjectProcedure[O1, O2] = new TObjectObjectProcedure[O1, O2]() {
    def execute(p1: O1, p2: O2) = {
      f.asInstanceOf[ObjectObjectProcedure[O1, O2, _]](p1, p2)
      true
    }
  }
}