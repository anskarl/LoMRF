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

import java.nio.file.Path
import lomrf.logic.{FALSE, TRUE, AtomSignature}
import lomrf.mln.model.mrf.MRF
import scala.collection.mutable
import java.io.{IOException, File}
import scala.reflect._

/**
 * Various utility functions.
 *
 *
 *
 */
object Utilities {

  /**
   * Calculates the actual time in hours, minutes, seconds and milliseconds given the
   * total time in milliseconds and concatenates it along a given message.
   *
   * @param msg the message to be displayed along the actual time
   * @param milliseconds the total time in milliseconds
   *
   * @return the converted time along with the given message
   */
  def msecTimeToText(msg: String, milliseconds: Long): String = msg + " = " + msecTimeToText(milliseconds)

  /**
   * Calculates the actual time in hours, minutes, seconds and milliseconds given the
   * total time in milliseconds.
   *
   * @param milliseconds the total time in milliseconds
   *
   * @return the converted time
   */
  def msecTimeToText(milliseconds: Long): String = {
    val time = milliseconds / 1000
    val seconds = time % 60
    val minutes = (time % 3600) / 60
    val hours = time / 3600

    hours + "h " + minutes + "m " + seconds + "s " + (milliseconds % 1000) + "ms"
  }

  /**
   * Calculates the actual time in hours, minutes, seconds and milliseconds until now
   * given a starting time in milliseconds and concatenates it along a given message.
   *
   * @param msg the message to be displayed along the actual time
   * @param fromTime the starting time in milliseconds
   *
   * @return the converted time until now along with the given message
   */
  def msecTimeToTextUntilNow(msg: String, fromTime: Long) = msg + " = " + msecTimeToText(System.currentTimeMillis - fromTime)

  /**
   * Calculates the actual time in hours, minutes, seconds and milliseconds until now
   * given a starting time in milliseconds
   *
   * @param fromTime the starting time in milliseconds
   *
   * @return the converted time until now
   */
  def msecTimeToTextUntilNow(fromTime: Long) = msecTimeToText(System.currentTimeMillis - fromTime)

  /**
   * Calculates the actual time in seconds, milliseconds and nanoseconds given the
   * total time in nanoseconds and concatenates it along a given message.
   *
   * @param msg the message to be displayed along the actual time
   * @param nanoseconds the total time in nanoseconds
   *
   * @return the converted time along with the given message
   */
  def nsecTimeToText(msg: String, nanoseconds: Long): String = msg + " = " + nsecTimeToText(nanoseconds)

  /**
   * Calculates the actual time in seconds, milliseconds and nanoseconds given the
   * total time in nanoseconds.
   *
   * @param nanoseconds the total time in nanoseconds
   *
   * @return the converted time
   */
  def nsecTimeToText(nanoseconds: Long): String = {
    val nsec = nanoseconds % 1000000L
    val milliseconds = nanoseconds / 1000000L
    val msec = milliseconds % 1000
    val time = milliseconds / 1000
    val sec = time % 60

    sec + "s " + msec + "ms " + nsec + "ns"
  }

  /**
   * Measures the time spend in a function call
   *
   * @return a tuple: (total execution time, result)
   */
  def time[T](f: () => T) = {
    val begin = System.currentTimeMillis
    val result = f()
    (System.currentTimeMillis - begin, result)
  }

  def powerSet[T](xs: Set[T]) = (Set(Set.empty[T]) /: xs)((xss, x) => xss ++ xss.map(_ + x))

  def naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))

  def printTable(table: Array[Array[String]], firstRowAsHeader: Boolean = true) {
    // Find the maximum number of columns
    var maxColumns = 0
    for (i <- 0 until table.length) {
      if (table(i).length > maxColumns) maxColumns = table(i).length
    }

    // Find the maximum length of a string in each column
    val lengths = new Array[Int](maxColumns)
    for (i <- 0 until table.length) {
      for (j <- 0 until table(i).length) {
        if (lengths(j) < table(i)(j).length()) lengths(j) = table(i)(j).length()
      }
    }

    for (i <- 0 until table.length) {
      if (firstRowAsHeader && i == 1) println()

      for (j <- 0 until table(i).length) {
        printf(
          "%1$" + lengths(j) + "s" + (if (j + 1 == lengths.length) "\n" else " "),
          table(i)(j)
        )
      }
    }
  }

  object Metrics {

    /**
     * Calculates the FMeasure relative to an annotated state of ground atoms.
     *
     * @param mrf the ground Markov network
     * @param annotationDB the annotated state of ground atoms
     * @param beta parameter for specific FMeasure function (i.e. for beta = 1 we get F1Score)
     *
     * @return FMeasure
     */
    def FMeasure(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB], beta: Double): Double = {

      var Tpositive, Tnegative, Fpositive, Fnegative = 0.0

      // Count true positives and negatives as well as false positives and negatives
      val atoms = mrf.atoms.iterator()
      while(atoms.hasNext) {
        atoms.advance()
        val atomID = atoms.key()
        val value = atoms.value().getState
        val annotation = annotationDB(signatureOf(atomID)(mrf.mln))(atomID)
        (annotation, value) match {
          case (TRUE, true) => Tpositive += 1
          case (FALSE, false) => Tnegative += 1
          case (FALSE, true) => Fpositive += 1
          case _ => Fnegative +=1
        }
      }

      // Calculate precision and recall
      var precision, recall = 0.0
      if(Tpositive + Fpositive != 0.0) precision = Tpositive / (Tpositive + Fpositive)
      if(Tpositive + Fnegative != 0.0) recall = Tpositive / (Tpositive + Fnegative)

      if(precision + recall != 0)
        ((1 + math.pow(beta, 2)) * precision * recall) / ((math.pow(beta, 2) * precision) + recall)
      else
        0.0
    }

  }

  /**
   * Safe dereference operator.
   *
   * This operator returns the specified default value (see parameter alt) when the current
   * instance is null. To do that, we implicitly extend that instance with an anonymous inner
   * class that carries the actual operator).
   *
   * @param alt the value to return when the current instance is null
   * @tparam T the type of the return value
   * @return the instance when is not null, otherwise the specified alternative value
   */
  implicit def dereferenceOperator[T](alt: => T) = new {
    def ??:[A >: T](pred: A) = if (pred == null) alt else pred
  }

  object io {

    implicit def strToFile(str: String): File = new File(str)

    implicit def pathToFile(path: Path): File = path.toFile

    /**
     * Search recursively for files/directories in a directory.
     *
     * @param targetDir: the target directory to search
     * @param matcherFunction: a simple filtering function (maps files to Boolean values, where the value 'true'
     *                       represents that the file matches the filtering criteria of the function)

     * @param recursively When is set true the function searches recursively in all dirs. It is true by default.
     *
     * @return A list of matched files
     *
     * @throws IOException: when the target directory does not exists
     *                    or when the application does not have read permissions to access.
     *
     * @throws IllegalArgumentException: when the 'targetDir' parameter is not a directory
     *
     */
    def findFiles(targetDir: File, matcherFunction: File => Boolean, recursively: Boolean = true): List[File] = {

      if (!targetDir.exists())
        throw new IOException("The specified target directory does not exists")

      if (!targetDir.isDirectory)
        throw new IllegalArgumentException("The specified target directory does not seem to be a directory")

      if (!targetDir.canRead)
        throw new IOException("Cannot read the specified target directory, please check if you have sufficient permissions.")

      val directories = mutable.Queue[File](targetDir)

      var resultFiles = List[File]()

      while (directories.nonEmpty) {
        for (currentFile <- directories.dequeue().listFiles) {
          // When the current file is a directory and the recursively=true, then
          // simply enqueue this file in the directories queue, otherwise continue.
          if (recursively && currentFile.isDirectory) directories.enqueue(currentFile)

          // When the current file is matching (according to the given matcher function), then
          // add this file to the result list, otherwise continue.
          if (matcherFunction(currentFile)) resultFiles ::= currentFile
        }
      }

      resultFiles
    }

    /**
     * Search recursively for files/directories in a directory and give the fist matching file.
     *
     * @param targetDir: the target directory to search
     * @param matcherFunction: a simple filtering function (maps files to Boolean values, where the value 'true'
     *                       represents that the file matches the filtering criteria of the function)

     * @param recursively When is set true the function searches recursively in all dirs. It is true by default.
     *
     * @return A list of matched files
     *
     * @throws IOException: when the target directory does not exists
     *                    or when the application does not have read permissions to access.
     *
     * @throws IllegalArgumentException: when the 'targetDir' parameter is not a directory
     */
    def findFirstFile(targetDir: File, matcherFunction: File => Boolean, recursively: Boolean = true): Option[File] = {

      if (!targetDir.exists())
        throw new IOException("The specified target directory does not exists")

      if (!targetDir.isDirectory)
        throw new IllegalArgumentException("The specified target directory does not seem to be a directory")

      if (!targetDir.canRead)
        throw new IOException("Cannot read the specified target directory, please check if you have sufficient permissions.")

      if (!targetDir.isDirectory) return None

      val directories = mutable.Queue[File](targetDir)


      while (directories.nonEmpty) {
        for (currentFile <- directories.dequeue().listFiles) {
          // When the current file is a directory and the recursively=true, then
          // simply enqueue this file in the directories queue, otherwise continue.
          if (recursively && currentFile.isDirectory) directories.enqueue(currentFile)

          // When the current file is matching (according to the given matcher function), then
          // add this file to the result list, otherwise continue.
          if (matcherFunction(currentFile)) return Some(currentFile)
        }
      }

      None
    }

  }

  object reflect {

    /**
     * Utility function to test whether the given instance
     * is of a particular type.
     *
     * @param obj the object instance to check
     * @tparam T the type to test against
     *
     * @return true if obj is of type T, otherwise false
     */
    def isOfType[T: ClassTag](obj: Any): Boolean = {

      val clazzTag = classTag[T].runtimeClass

      def isPrimitiveClass[C: ClassTag] =
        classTag[C].runtimeClass.isAssignableFrom(obj.asInstanceOf[AnyRef].getClass)

      def isRegularClass[C: ClassTag] =
        clazzTag.isAssignableFrom(obj.asInstanceOf[AnyRef].getClass)

      clazzTag.toString match {
        case "char" => isPrimitiveClass[java.lang.Character]
        case "byte" => isPrimitiveClass[java.lang.Byte]
        case "short" => isPrimitiveClass[java.lang.Short]
        case "boolean" => isPrimitiveClass[java.lang.Boolean]
        case "int" => isPrimitiveClass[java.lang.Integer]
        case "float" => isPrimitiveClass[java.lang.Float]
        case "long" => isPrimitiveClass[java.lang.Long]
        case "double" => isPrimitiveClass[java.lang.Double]
        case _  => isRegularClass
      }
    }

  }

}
