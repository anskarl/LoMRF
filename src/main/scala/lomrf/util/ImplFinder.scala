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

package lomrf.util

import java.io.{File, FilenameFilter, IOException}
import java.util.jar.{JarEntry, JarFile}
import lomrf.util.logging.Implicits._
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable

/**
 * Implementations finder.
 */
final class ImplFinder(traitSet: Set[Class[_]]) extends LazyLogging {

  private val classFF = new FilenameFilter() {
    def accept(dir: File, name: String): Boolean = dir != null && name.toLowerCase.endsWith("class")
  }

  private val jarFF = new FilenameFilter() {
    def accept(dir: File, name: String): Boolean = dir != null && name.toLowerCase.endsWith("jar")
  }

  private val dirFF = new FilenameFilter() {
    def accept(dir: File, name: String): Boolean = dir != null && new File(dir.getPath + File.separator + name).isDirectory
  }

  private var map = mutable.Map[Class[_], mutable.HashSet[Class[_]]]()

  def result: ImplFinder.ImplementationsMap = {
    val result = map
    map = null
    result
  }

  def searchPaths(dirPaths: Array[String]) {
    check()
    val classLoader = this.getClass.getClassLoader
    dirPaths.foreach(dirPath => loadDir(dirPath, dirPath, classLoader))
  }

  def searchPaths(dirPaths: String*) {
    check()
    val classLoader = this.getClass.getClassLoader
    dirPaths.foreach(dirPath => loadDir(dirPath, dirPath, classLoader))
  }

  def searchPath(dirPath: String) {
    check()
    loadDir(dirPath, dirPath, this.getClass.getClassLoader)
  }

  private def check() {
    if (map eq null) map = mutable.Map[Class[_], mutable.HashSet[Class[_]]]()
  }

  private def loadDir(basePath: String, dirPath: String, clazzLoader: ClassLoader) {
    val dirFile = new File(dirPath)
    if (!dirFile.isDirectory) logger.fatal(dirPath + " it's not a directory!")

    val classFiles = dirFile.listFiles(classFF)
    val jarFiles = dirFile.listFiles(jarFF)
    val dirFiles = dirFile.listFiles(dirFF)

    classFiles.foreach(f => addClassFile(basePath, f, clazzLoader))
    jarFiles.foreach(f => loadJar(f.getPath))
    dirFiles.foreach(dir => loadDir(basePath, dir.getPath, clazzLoader))
  }

  private def loadJar(jarFilePath: String) {
    val file = new File(jarFilePath)

    if (!file.isFile || !file.getName.endsWith(".jar")) logger.fatal(jarFilePath + " it's not a jar file!")

    try {
      addJarFile(new JarFile(file), getClass.getClassLoader)
    } catch {
      case e: IOException => logger.fatal("Cannot open jar file: " + file.getName)
    }
  }

  private def addJarFile(jarFile: JarFile, clazzLoader: ClassLoader) {
    val entries: java.util.Enumeration[JarEntry] = jarFile.entries

    while (entries.hasMoreElements) {
      val name = entries.nextElement.getName
      if (name.toLowerCase.endsWith(".class")) addIfMatch(name, clazzLoader)
    }
  }

  private def addClassFile(basePath: String, classFile: File, clazzLoader: ClassLoader) {
    if (classFile.isFile && classFile.getName.toLowerCase.endsWith(".class"))
      addIfMatch(classFile.getPath.substring(basePath.length), clazzLoader)
  }

  private def addIfMatch(n: String, clazzLoader: ClassLoader) {
    var name = n
    if (name.indexOf('$') == -1) {
      // clear string
      if (name.toLowerCase.endsWith(".class")) name = name.substring(0, name.length - 6)

      name = name.replace('\\', '.').replace('/', '.')
      while (name.startsWith(".")) name = name.substring(1)


      // check if this class implements one of the specified Interfaces
      val clazz = try {
        Class.forName(name, true, clazzLoader)
      }
      catch {
        case e: ClassNotFoundException => null
        case e: NoClassDefFoundError => null
      }

      if (clazz != null && !clazz.isInterface) {
        val interfaces: Set[Class[_]] = extractInterfaces(clazz)

        interfaces.find(interface => traitSet.contains(interface)) match {
          case Some(entry) =>
            map.get(entry) match {
              case Some(entries) => map(entry) += clazz
              case None => map(entry) = mutable.HashSet[Class[_]](clazz)
            }
          case _ => // ignore
        }
      }
    }
  }

  private def extractInterfaces(clazz: Class[_]): Set[Class[_]] = {
    var current = clazz
    var result = Set[Class[_]]()

    while (current != null) {
      clazz.getInterfaces.foreach(interface => result += interface)
      current = current.getSuperclass
    }
    result
  }
}

object ImplFinder {

  type ImplementationsMap = scala.collection.Map[Class[_], scala.collection.Set[Class[_]]]

  def apply(traits: Class[_]*) = new ImplFinder(traits.toSet)

  def apply(traitsSet: Set[Class[_]]) = new ImplFinder(traitsSet)
}




