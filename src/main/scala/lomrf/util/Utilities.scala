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

import scala.collection.mutable
import java.io.File

/**
 * @author Anastasios Skarlatidis
 */

object Utilities {
  implicit def strToFile(str:String) = new File(str)

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }
  
  def findFiles(rootDir: File, matcher: String => Boolean): List[File] = {
      var resultFiles = List[File]()
      val queue = mutable.Queue[File](rootDir)

      while (queue.nonEmpty) {
        queue.dequeue().listFiles.foreach {
          x =>
            if (x.isDirectory) queue.enqueue(x)
            else if (matcher(x.getAbsolutePath)) resultFiles ::= x
        }
      }
      resultFiles
  }

  sealed trait FileType
  case object DIR extends FileType
  case object FILE extends FileType
  case object ANY extends FileType

  def findFiles(rootDir: File, fileType: FileType, matcher: String => Boolean): List[File] = {
    var resultFiles = List[File]()
    val queue = mutable.Queue[File](rootDir)

    while (queue.nonEmpty) {
      queue.dequeue().listFiles.foreach {
        x =>{
          //println("READING "+x.getAbsolutePath)
          if (x.isDirectory) queue.enqueue(x)

          fileType match {
            case ANY => if (matcher(x.getPath)) resultFiles ::= x
            case DIR if x.isDirectory => if (matcher(x.getPath)) resultFiles ::= x
            case FILE if x.isFile => if (matcher(x.getPath)) resultFiles ::= x
            case _ => //println("IGNORING "+x)
          }
        }
      }
    }
    resultFiles
  }


  def findFilesOpt(rootDir: File, matcher: String => Option[File]): List[File] = {
      var resultFiles = List[File]()
      val queue = mutable.Queue[File](rootDir)

      while (queue.nonEmpty) {
        queue.dequeue().listFiles.foreach {
          x =>
            if (x.isDirectory) queue.enqueue(x)
            matcher(x.getAbsolutePath) match{
              case Some(entry) => resultFiles ::= entry
              case _ =>
            }
        }
      }
      resultFiles
  }

  def msecTimeToText(msg:String, milliseconds: Long): String = msg + " = "+msecTimeToText(milliseconds)
  
  def msecTimeToText(milliseconds: Long): String = {
    val time = milliseconds / 1000
    val seconds = time % 60
    val minutes = (time % 3600) / 60
    val hours = time / 3600

    hours + "h " + minutes + "m " + seconds + "s " + (milliseconds % 1000)+"ms"
  }

  def msecTimeToTextUntilNow(msg:String, fromTime: Long) = msg + " = "+msecTimeToText(System.currentTimeMillis - fromTime)
  def msecTimeToTextUntilNow(fromTime: Long) = msecTimeToText(System.currentTimeMillis - fromTime)

  def nsecTimeToText(msg:String, nanoseconds: Long): String = msg + " = "+nsecTimeToText(nanoseconds)
  
  def nsecTimeToText(nanoseconds: Long): String = {
    val nsec = nanoseconds % 1000000L
    val milliseconds = nanoseconds / 1000000L
    val msec = milliseconds % 1000
    val time = milliseconds / 1000
    val sec = time % 60

    sec + "s " + msec+"ms "+nsec+"ns"
  }

  /**
   * Measures the time spend in a function call
   * @return a tuple: (total execution time, result)
   */
  def time[T](f: () => T) = {
    val begin = System.currentTimeMillis
    val result = f()
    (System.currentTimeMillis - begin, result)
  }

  def powerset[T](xs: Set[T]) = (Set(Set.empty[T]) /: xs)((xss, x) => xss ++ xss.map(_ + x))

  def naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))


  def printTable(table: Array[Array[String]], firstRowAsHeader:Boolean = true){
    // Find the maximum number of columns
    var maxColumns = 0
    for(i <- 0 until table.length) {
      if(table(i).length > maxColumns) maxColumns = table(i).length
    }

    // Find the maximum length of a string in each column
    val lengths = new Array[Int](maxColumns)
    for(i <- 0 until table.length) {
      for(j <- 0 until table(i).length){
        if(lengths(j) < table(i)(j).length()) lengths(j) = table(i)(j).length()
      }
    }

    for(i <- 0 until table.length) {
      if(firstRowAsHeader && i == 1) println()

      for(j <- 0 until table(i).length){
        printf(
          "%1$" + lengths(j)+"s"+ (if (j + 1 == lengths.length) "\n" else " "),
          table(i)(j)
        )
      }
    }
  }





  object Memoize {

    def apply[T, R](f: T => R) = new Memoize1(f)

    def apply[T1, T2, R](f: (T1, T2) => R) = new Memoize2(f)

    def apply[T1, T2, T3, R](f: (T1, T2, T3) => R) = new Memoize3(f)

    class Memoize1[T, R](f: T => R) extends (T => R) {
      private[this] val cache = new mutable.WeakHashMap[T, R]()

      def apply(x: T): R = cache.getOrElseUpdate(x, f(x))
    }

    class Memoize2[T1, T2, R](f: (T1, T2) => R) extends ((T1, T2) => R) {
      private[this] val cache = new mutable.WeakHashMap[(T1, T2), R]()

      def apply(a: T1, b: T2): R = {
        val x = a -> b
        cache.getOrElseUpdate(x, f(a, b))
      }
    }

    class Memoize3[T1, T2, T3, R](f: (T1, T2, T3) => R) extends ((T1, T2, T3) => R) {
      private[this] val cache = new mutable.WeakHashMap[(T1, T2, T3), R]()

      def apply(a: T1, b: T2, c: T3): R = {
        val x = (a, b, c)
        cache.getOrElseUpdate(x, f(a, b, c))
      }
    }


  }

}