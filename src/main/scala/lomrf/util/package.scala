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

package lomrf

import java.io.{ File, IOException }
import java.nio.file.{ Path, Paths }

import gnu.trove.map.TIntObjectMap
import gnu.trove.set.TIntSet

import scala.collection.mutable
import scala.reflect._
import spire.syntax.cfor._
import scala.language.implicitConversions

package object util {

  object seg {

    @inline
    def fetchKey[T](idx: Int, elements: Array[TIntSet]): Int = {
      var sum = 0
      var found = false
      var segIndex = -1

      while (!found && segIndex < elements.length - 1) {
        segIndex += 1
        if ((elements(segIndex) ne null) && !elements(segIndex).isEmpty) {
          val next_sum = sum + elements(segIndex).size()
          if (idx < next_sum) found = true
          else sum = next_sum
        }
      }

      if (!found) throw new IndexOutOfBoundsException(idx.toString)

      val position = idx - sum
      val iterator = elements(segIndex).iterator()

      cfor(0)(_ < position, _ + 1){ i: Int => iterator.next() }

      iterator.next()
    }

    @inline
    def fetchKey[T](idx: Int, elements: Array[TIntObjectMap[T]]): Int = {
      var sum = 0
      var found = false
      var segIndex = -1

      while (!found && segIndex < elements.length - 1) {
        segIndex += 1
        if ((elements(segIndex) ne null) && !elements(segIndex).isEmpty) {
          val next_sum = sum + elements(segIndex).size()
          if (idx < next_sum) found = true
          else sum = next_sum
        }
      }

      if (!found) throw new IndexOutOfBoundsException(idx.toString)

      val position = idx - sum
      val iterator = elements(segIndex).iterator()

      cfor(0)(_ <= position, _ + 1){ i: Int => iterator.advance() }

      iterator.key()
    }

    @inline
    def fetchKeyValue[T](idx: Int, elements: Array[TIntObjectMap[T]]): (Int, T) = {
      var sum = 0
      var found = false
      var segIndex = -1

      while (!found && segIndex < elements.length - 1) {
        segIndex += 1
        if ((elements(segIndex) ne null) && !elements(segIndex).isEmpty) {
          val next_sum = sum + elements(segIndex).size()
          if (idx < next_sum) found = true
          else sum = next_sum
        }
      }

      if (!found) throw new IndexOutOfBoundsException(idx.toString)

      val position = idx - sum
      val iterator = elements(segIndex).iterator()

      cfor(0)(_ <= position, _ + 1){ i: Int => iterator.advance() }

      (iterator.key(), iterator.value())
    }

  }

  object time {

    import lomrf.util.TimeGranularity.TimeGranularity

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
    def msecTimeToTextUntilNow(msg: String, fromTime: Long): String =
      msg + " = " + msecTimeToText(System.currentTimeMillis - fromTime)

    /**
      * Calculates the actual time in hours, minutes, seconds and milliseconds until now
      * given a starting time in milliseconds
      *
      * @param fromTime the starting time in milliseconds
      *
      * @return the converted time until now
      */
    def msecTimeToTextUntilNow(fromTime: Long): String =
      msecTimeToText(System.currentTimeMillis - fromTime)

    /**
      * Calculates the actual time in seconds, milliseconds and nanoseconds given the
      * total time in nanoseconds and concatenates it along a given message.
      *
      * @param msg the message to be displayed along the actual time
      * @param nanoseconds the total time in nanoseconds
      *
      * @return the converted time along with the given message
      */
    def nsecTimeToText(msg: String, nanoseconds: Long): String =
      msg + " = " + nsecTimeToText(nanoseconds)

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
      *
      * Measures the time spend in a function call
      *
      * @param body a function representing the execution of code to be measured
      * @param granularity the time granularity to use (default is millisecond)
      * @tparam T the type of the result when executing
      * @return a tuple: (total execution time, result)
      */
    def measureTime[T](body: => T)(implicit granularity: TimeGranularity = TimeGranularity.Millisecond): (Long, T) = {
      granularity match {
        case TimeGranularity.Millisecond =>
          val begin = System.currentTimeMillis
          val result = body
          (System.currentTimeMillis - begin, result)

        case TimeGranularity.Nanosecond =>
          val begin = System.nanoTime()
          val result = body
          (System.nanoTime() - begin, result)
      }
    }
  }

  def powerSet[T](xs: Set[T]) = (Set(Set.empty[T]) /: xs)((xss, x) => xss ++ xss.map(_ + x))

  def naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))

  object TimeGranularity extends Enumeration {
    type TimeGranularity = Value
    val Millisecond, Nanosecond = Value
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
    def ??:[A >: T](predicate: A) = if (predicate == null) alt else predicate
  }

  object io {

    import scala.language.postfixOps

    private final val SEP = System.getProperty("file.separator")
    final val USER_DIR = System.getProperty("user.dir")

    implicit def strToFile(str: String): File = new File(str)

    implicit def pathToFile(path: Path): File = path.toFile

    implicit class StringPathWrapper(val path: String) extends AnyVal {
      def /(otherPath: String): String = path + SEP + otherPath

      def / : String = {
        if (path.endsWith(SEP)) path
        else path + SEP
      }

      def toFile: File = new File(path)

      def toPath: Path = Paths.get(path)

    }

    /**
      * Search recursively for files/directories in a directory.
      *
      * @param targetDir: the target directory to search
      * @param matcherFunction: a simple filtering function (maps files to Boolean values, where the value 'true'
      *                       represents that the file matches the filtering criteria of the function)
      *
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
          // When the current file is a directory and the recursively is true, then
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
      *
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
          // When the current file is a directory and the recursively is true, then
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
        case "char"    => isPrimitiveClass[java.lang.Character]
        case "byte"    => isPrimitiveClass[java.lang.Byte]
        case "short"   => isPrimitiveClass[java.lang.Short]
        case "boolean" => isPrimitiveClass[java.lang.Boolean]
        case "int"     => isPrimitiveClass[java.lang.Integer]
        case "float"   => isPrimitiveClass[java.lang.Float]
        case "long"    => isPrimitiveClass[java.lang.Long]
        case "double"  => isPrimitiveClass[java.lang.Double]
        case _         => isRegularClass
      }
    }

  }

  def collectByKey[K, V](collection: Traversable[(K, V)]): Map[K, Set[V]] = {
    collection
      .groupBy(_._1)
      .map {
        case (group: K, traversable) =>
          group -> traversable.map(_._2).toSet
      }
  }

}
