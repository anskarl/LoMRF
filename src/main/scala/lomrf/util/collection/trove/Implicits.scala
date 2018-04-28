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

package lomrf.util.collection.trove

import scala.language.implicitConversions

/**
  * This object contains utility implicit definitions that convert
  * Scala's functions to the corresponding Trove Functions and Procedures.
  *
  * The code is based on the following blog article:
  * http://blog.juma.me.uk/tag/trove/
  *
  * @author Anastasios Skarlatidis
  */
object Implicits {

  import gnu.trove.function._
  import gnu.trove.procedure._


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

  implicit def toByteProcedure(f: Byte => Boolean): TByteProcedure = new TByteProcedure() {
    def execute(p1: Byte): Boolean = f.asInstanceOf[ByteProcedure[Boolean]](p1)
  }

  implicit def toByteByteProcedure(f: (Byte, Byte) => Boolean): TByteByteProcedure = new TByteByteProcedure() {
    def execute(p1: Byte, p2: Byte): Boolean = f.asInstanceOf[ByteByteProcedure[Boolean]](p1, p2)
  }

  implicit def toByteShortProcedure(f: (Byte, Short) => Boolean): TByteShortProcedure = new TByteShortProcedure() {
    def execute(p1: Byte, p2: Short): Boolean = f.asInstanceOf[ByteShortProcedure[Boolean]](p1, p2)
  }

  implicit def toByteIntProcedure(f: (Byte, Int) => Boolean): TByteIntProcedure = new TByteIntProcedure() {
    def execute(p1: Byte, p2: Int): Boolean = f.asInstanceOf[ByteIntProcedure[Boolean]](p1, p2)
  }

  implicit def toByteLongProcedure(f: (Byte, Long) => Boolean): TByteLongProcedure = new TByteLongProcedure() {
    def execute(p1: Byte, p2: Long): Boolean = f.asInstanceOf[ByteLongProcedure[Boolean]](p1, p2)
  }

  implicit def toByteFloatProcedure(f: (Byte, Float) => Boolean): TByteFloatProcedure = new TByteFloatProcedure() {
    def execute(p1: Byte, p2: Float): Boolean = f.asInstanceOf[ByteFloatProcedure[Boolean]](p1, p2)
  }

  implicit def toByteDoubleProcedure(f: (Byte, Double) => Boolean): TByteDoubleProcedure = new TByteDoubleProcedure() {
    def execute(p1: Byte, p2: Double): Boolean = f.asInstanceOf[ByteDoubleProcedure[Boolean]](p1, p2)
  }

  implicit def toByteObjectProcedure[O](f: (Byte, O) => Boolean): TByteObjectProcedure[O] = new TByteObjectProcedure[O]() {
    def execute(p1: Byte, p2: O): Boolean = f.asInstanceOf[ByteObjectProcedure[O, Boolean]](p1, p2)
  }


  //===================== Short =====================

  trait ShortProcedure[+R] extends ((Short) => R) {
    override def apply(i: Short): R
  }

  trait ShortByteProcedure[+R] extends ((Short, Byte) => R) {
    override def apply(v1: Short, v2: Byte): R
  }

  trait ShortShortProcedure[+R] extends ((Short, Short) => R) {
    override def apply(v1: Short, v2: Short): R
  }

  trait ShortIntProcedure[+R] extends ((Short, Int) => R) {
    override def apply(v1: Short, v2: Int): R
  }

  trait ShortLongProcedure[+R] extends ((Short, Long) => R) {
    override def apply(v1: Short, v2: Long): R
  }

  trait ShortFloatProcedure[+R] extends ((Short, Float) => R) {
    override def apply(v1: Short, v2: Float): R
  }

  trait ShortDoubleProcedure[+R] extends ((Short, Double) => R) {
    override def apply(v1: Short, v2: Double): R
  }

  trait ShortObjectProcedure[O, +R] extends ((Short, O) => R) {
    override def apply(v1: Short, v2: O): R
  }

  implicit def toShortProcedure(f: Short => Boolean): TShortProcedure = new TShortProcedure() {
    override def execute(p1: Short): Boolean = f.asInstanceOf[ShortProcedure[Boolean]](p1)
  }

  implicit def toShortByteProcedure(f: (Short, Byte) => Boolean): TShortByteProcedure = new TShortByteProcedure() {
    override def execute(p1: Short, p2: Byte): Boolean = f.asInstanceOf[ShortByteProcedure[Boolean]](p1, p2)
  }

  implicit def toByteShortProcedure(f: (Short, Short) => Boolean): TShortShortProcedure = new TShortShortProcedure() {
    override def execute(p1: Short, p2: Short): Boolean = f.asInstanceOf[ShortShortProcedure[Boolean]](p1, p2)
  }

  implicit def toShortIntProcedure(f: (Short, Int) => Boolean): TShortIntProcedure = new TShortIntProcedure() {
    override def execute(p1: Short, p2: Int): Boolean = f.asInstanceOf[ShortIntProcedure[Boolean]](p1, p2)
  }

  implicit def toShortLongProcedure(f: (Short, Long) => Boolean): TShortLongProcedure = new TShortLongProcedure() {
    override def execute(p1: Short, p2: Long): Boolean = f.asInstanceOf[ShortLongProcedure[Boolean]](p1, p2)
  }

  implicit def toShortFloatProcedure(f: (Short, Float) => Boolean): TShortFloatProcedure = new TShortFloatProcedure() {
    override def execute(p1: Short, p2: Float): Boolean = f.asInstanceOf[ShortFloatProcedure[Boolean]](p1, p2)
  }

  implicit def toShortDoubleProcedure(f: (Short, Double) => Boolean): TShortDoubleProcedure = new TShortDoubleProcedure() {
    override def execute(p1: Short, p2: Double): Boolean = f.asInstanceOf[ShortDoubleProcedure[Boolean]](p1, p2)
  }

  implicit def toShortObjectProcedure[O](f: (Short, O) => Boolean): TShortObjectProcedure[O] = new TShortObjectProcedure[O]() {
    override def execute(p1: Short, p2: O): Boolean = f.asInstanceOf[ShortObjectProcedure[O, Boolean]](p1, p2)
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

  implicit def toIntProcedure(f: Int => Boolean): TIntProcedure = new TIntProcedure() {
    def execute(p1: Int): Boolean = f.asInstanceOf[IntProcedure[Boolean]](p1)
  }

  implicit def toIntByteProcedure(f: (Int, Byte) => Boolean): TIntByteProcedure = new TIntByteProcedure() {
    def execute(p1: Int, p2: Byte): Boolean = f.asInstanceOf[IntByteProcedure[Boolean]](p1, p2)
  }

  implicit def toIntShortProcedure(f: (Int, Short) => Boolean): TIntShortProcedure = new TIntShortProcedure() {
    def execute(p1: Int, p2: Short): Boolean = f.asInstanceOf[IntShortProcedure[Boolean]](p1, p2)
  }

  implicit def toIntIntProcedure(f: (Int, Int) => Boolean): TIntIntProcedure = new TIntIntProcedure() {
    def execute(p1: Int, p2: Int): Boolean = f.asInstanceOf[IntIntProcedure[Boolean]](p1, p2)
  }

  implicit def toIntLongProcedure(f: (Int, Long) => Boolean): TIntLongProcedure = new TIntLongProcedure() {
    def execute(p1: Int, p2: Long): Boolean = f.asInstanceOf[IntLongProcedure[Boolean]](p1, p2)
  }

  implicit def toIntFloatProcedure(f: (Int, Float) => Boolean): TIntFloatProcedure = new TIntFloatProcedure() {
    def execute(p1: Int, p2: Float): Boolean = f.asInstanceOf[IntFloatProcedure[Boolean]](p1, p2)
  }

  implicit def toIntDoubleProcedure(f: (Int, Double) => Boolean): TIntDoubleProcedure = new TIntDoubleProcedure() {
    def execute(p1: Int, p2: Double): Boolean = f.asInstanceOf[IntDoubleProcedure[Boolean]](p1, p2)
  }

  implicit def toIntObjectProcedure[O](f: (Int, O) => Boolean): TIntObjectProcedure[O] = new TIntObjectProcedure[O]() {
    def execute(p1: Int, p2: O): Boolean = f.asInstanceOf[IntObjectProcedure[O, Boolean]](p1, p2)
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

  implicit def toLongProcedure(f: Long => Boolean): TLongProcedure = new TLongProcedure() {
    def execute(p1: Long): Boolean = f.asInstanceOf[LongProcedure[Boolean]](p1)
  }

  implicit def toLongByteProcedure(f: (Long, Byte) => Boolean): TLongByteProcedure = new TLongByteProcedure() {
    def execute(p1: Long, p2: Byte): Boolean = f.asInstanceOf[LongByteProcedure[Boolean]](p1, p2)
  }

  implicit def toLongShortProcedure(f: (Long, Short) => Boolean): TLongShortProcedure = new TLongShortProcedure() {
    def execute(p1: Long, p2: Short): Boolean = f.asInstanceOf[LongShortProcedure[Boolean]](p1, p2)
  }

  implicit def toLongIntProcedure(f: (Long, Int) => Boolean): TLongIntProcedure = new TLongIntProcedure() {
    def execute(p1: Long, p2: Int): Boolean = f.asInstanceOf[LongIntProcedure[Boolean]](p1, p2)
  }

  implicit def toLongLongProcedure(f: (Long, Long) => Boolean): TLongLongProcedure = new TLongLongProcedure() {
    def execute(p1: Long, p2: Long): Boolean = f.asInstanceOf[LongLongProcedure[Boolean]](p1, p2)
  }

  implicit def toLongFloatProcedure(f: (Long, Float) => Boolean): TLongFloatProcedure = new TLongFloatProcedure() {
    def execute(p1: Long, p2: Float): Boolean = f.asInstanceOf[LongFloatProcedure[Boolean]](p1, p2)
  }

  implicit def toLongDoubleProcedure(f: (Long, Double) => Boolean): TLongDoubleProcedure = new TLongDoubleProcedure() {
    def execute(p1: Long, p2: Double): Boolean = f.asInstanceOf[LongDoubleProcedure[Boolean]](p1, p2)
  }

  implicit def toLongObjectProcedure[O](f: (Long, O) => Boolean): TLongObjectProcedure[O] = new TLongObjectProcedure[O]() {
    def execute(p1: Long, p2: O): Boolean = f.asInstanceOf[LongObjectProcedure[O, Boolean]](p1, p2)
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

  implicit def toFloatProcedure(f: Float => Boolean): TFloatProcedure = new TFloatProcedure() {
    def execute(p1: Float): Boolean = f.asInstanceOf[FloatProcedure[Boolean]](p1)
  }

  implicit def toFloatByteProcedure(f: (Float, Byte) => Boolean): TFloatByteProcedure = new TFloatByteProcedure() {
    def execute(p1: Float, p2: Byte): Boolean = f.asInstanceOf[FloatByteProcedure[Boolean]](p1, p2)
  }

  implicit def toLongShortProcedure(f: (Float, Short) => Boolean): TFloatShortProcedure = new TFloatShortProcedure() {
    def execute(p1: Float, p2: Short): Boolean = f.asInstanceOf[FloatShortProcedure[Boolean]](p1, p2)
  }

  implicit def toLongIntProcedure(f: (Float, Int) => Boolean): TFloatIntProcedure = new TFloatIntProcedure() {
    def execute(p1: Float, p2: Int): Boolean = f.asInstanceOf[FloatIntProcedure[Boolean]](p1, p2)
  }

  implicit def toFloatLongProcedure(f: (Float, Long) => Boolean): TFloatLongProcedure = new TFloatLongProcedure() {
    def execute(p1: Float, p2: Long): Boolean = f.asInstanceOf[FloatLongProcedure[Boolean]](p1, p2)
  }

  implicit def toFloatFloatProcedure(f: (Float, Float) => Boolean): TFloatFloatProcedure = new TFloatFloatProcedure() {
    def execute(p1: Float, p2: Float): Boolean = f.asInstanceOf[FloatFloatProcedure[Boolean]](p1, p2)
  }

  implicit def toFloatDoubleProcedure(f: (Float, Double) => Boolean): TFloatDoubleProcedure = new TFloatDoubleProcedure() {
    def execute(p1: Float, p2: Double): Boolean = f.asInstanceOf[FloatDoubleProcedure[Boolean]](p1, p2)
  }

  implicit def toFloatObjectProcedure[O](f: (Float, O) => Boolean): TFloatObjectProcedure[O] = new TFloatObjectProcedure[O]() {
    def execute(p1: Float, p2: O): Boolean = f.asInstanceOf[FloatObjectProcedure[O, Boolean]](p1, p2)
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

  implicit def toDoubleProcedure(f: Double => Boolean): TDoubleProcedure = new TDoubleProcedure() {
    def execute(p1: Double): Boolean = f.asInstanceOf[DoubleProcedure[Boolean]](p1)
  }

  implicit def toDoubleByteProcedure(f: (Double, Byte) => Boolean): TDoubleByteProcedure = new TDoubleByteProcedure() {
    def execute(p1: Double, p2: Byte): Boolean = f.asInstanceOf[DoubleByteProcedure[Boolean]](p1, p2)
  }

  implicit def toDoubleShortProcedure(f: (Double, Short) => Boolean): TDoubleShortProcedure = new TDoubleShortProcedure() {
    def execute(p1: Double, p2: Short): Boolean = f.asInstanceOf[DoubleShortProcedure[Boolean]](p1, p2)
  }

  implicit def toDoubleIntProcedure(f: (Double, Int) => Boolean): TDoubleIntProcedure = new TDoubleIntProcedure() {
    def execute(p1: Double, p2: Int): Boolean = f.asInstanceOf[DoubleIntProcedure[Boolean]](p1, p2)
  }

  implicit def toDoubleLongProcedure(f: (Double, Long) => Boolean): TDoubleLongProcedure = new TDoubleLongProcedure() {
    def execute(p1: Double, p2: Long): Boolean = f.asInstanceOf[DoubleLongProcedure[Boolean]](p1, p2)
  }

  implicit def toDoubleFloatProcedure(f: (Double, Float) => Boolean): TDoubleFloatProcedure = new TDoubleFloatProcedure() {
    def execute(p1: Double, p2: Float): Boolean = f.asInstanceOf[DoubleFloatProcedure[Boolean]](p1, p2)
  }

  implicit def toDoubleDoubleProcedure(f: (Double, Double) => Boolean): TDoubleDoubleProcedure = new TDoubleDoubleProcedure() {
    def execute(p1: Double, p2: Double): Boolean = f.asInstanceOf[DoubleDoubleProcedure[Boolean]](p1, p2)
  }

  implicit def toDoubleObjectProcedure[O](f: (Double, O) => Boolean): TDoubleObjectProcedure[O] = new TDoubleObjectProcedure[O]() {
    def execute(p1: Double, p2: O): Boolean = f.asInstanceOf[DoubleObjectProcedure[O, Boolean]](p1, p2)
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


  implicit def toObjectProcedure[O](f: O => Boolean): TObjectProcedure[O] = new TObjectProcedure[O]() {
    def execute(p1: O): Boolean = f.asInstanceOf[ObjectProcedure[O, Boolean]](p1)
  }

  implicit def toObjectByteProcedure[O](f: (O, Byte) => Boolean): TObjectByteProcedure[O] = new TObjectByteProcedure[O]() {
    def execute(p1: O, p2: Byte): Boolean = f.asInstanceOf[ObjectByteProcedure[O, Boolean]](p1, p2)
  }

  implicit def toObjectShortProcedure[O](f: (O, Short) => Boolean): TObjectShortProcedure[O] = new TObjectShortProcedure[O]() {
    def execute(p1: O, p2: Short): Boolean = f.asInstanceOf[ObjectShortProcedure[O, Boolean]](p1, p2)
  }

  implicit def toObjectIntProcedure[O](f: (O, Int) => Boolean): TObjectIntProcedure[O] = new TObjectIntProcedure[O]() {
    def execute(p1: O, p2: Int): Boolean = f.asInstanceOf[ObjectIntProcedure[O, Boolean]](p1, p2)
  }

  implicit def toObjectLongProcedure[O](f: (O, Long) => Boolean): TObjectLongProcedure[O] = new TObjectLongProcedure[O]() {
    def execute(p1: O, p2: Long): Boolean = f.asInstanceOf[ObjectLongProcedure[O, Boolean]](p1, p2)
  }

  implicit def toObjectFloatProcedure[O](f: (O, Float) => Boolean): TObjectFloatProcedure[O] = new TObjectFloatProcedure[O]() {
    def execute(p1: O, p2: Float): Boolean = f.asInstanceOf[ObjectFloatProcedure[O, Boolean]](p1, p2)
  }

  implicit def toObjectDoubleProcedure[O](f: (O, Double) => Boolean): TObjectDoubleProcedure[O] = new TObjectDoubleProcedure[O]() {
    def execute(p1: O, p2: Double): Boolean = f.asInstanceOf[ObjectDoubleProcedure[O, Boolean]](p1, p2)

  }

  implicit def toObjectObjectProcedure[O1, O2](f: (O1, O2) => Boolean): TObjectObjectProcedure[O1, O2] = new TObjectObjectProcedure[O1, O2]() {
    def execute(p1: O1, p2: O2): Boolean = f.asInstanceOf[ObjectObjectProcedure[O1, O2, Boolean]](p1, p2)

  }
}