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

import java.io.{IOException, FilenameFilter, File}
import java.util.jar.{JarFile, JarEntry}
import auxlib.log.Logging

import scala.Predef._
import scala.collection.mutable

/**
 *
 * @author Anastasios Skarlatidis
 */
final class ImplFinder(traitSet: Set[Class[_]]) extends Logging {

  private val classFF = new FilenameFilter() {
    def accept(dir: File, name: String) = dir != null && name.toLowerCase.endsWith("class")
  }

  private val jarFF = new FilenameFilter() {
    def accept(dir: File, name: String) = dir != null && name.toLowerCase.endsWith("jar")
  }

  private val dirFF = new FilenameFilter() {
    def accept(dir: File, name: String) = dir != null && new File(dir.getPath + File.separator + name).isDirectory
  }

  private var map = mutable.Map[Class[_], mutable.HashSet[Class[_]]]()

  def result: ImplFinder.ImplementationsMap = {
    val result = map
    map = null
    result
  }

  def searchPaths(dirPaths: Array[String]){
    check()
    val classLoader = this.getClass.getClassLoader
    dirPaths.foreach(dirPath => loadDir(dirPath, dirPath, classLoader))
  }

  def searchPaths(dirPaths: String*){
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
    if (!dirFile.isDirectory) fatal(dirPath + " it's not a directory!")

    val classFiles = dirFile.listFiles(classFF)
    val jarFiles = dirFile.listFiles(jarFF)
    val dirFiles = dirFile.listFiles(dirFF)

    //.class files
    classFiles.foreach(f => addClassFile(basePath, f, clazzLoader))
    jarFiles.foreach(f => loadJar(f.getPath))
    dirFiles.foreach(dir => loadDir(basePath, dir.getPath, clazzLoader))
  }

  private def loadJar(jarFilePath: String) {
    val file = new File(jarFilePath)

    if (!file.isFile || !file.getName.endsWith(".jar")) fatal(jarFilePath + " it's not a jar file!")

    try {
      addJarFile(new JarFile(file), getClass.getClassLoader)
    } catch {
      case e: IOException => fatal("Cannot open jar file: " + file.getName)
    }
  }

  private def addJarFile(jarFile: JarFile, clazzLoader: ClassLoader) {
    //info("Opening jar file: " + jarFile.getName)
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
      //clear string
      if (name.toLowerCase.endsWith(".class")) name = name.substring(0, name.length - 6)

      name = name.replace('\\', '.').replace('/', '.')
      while (name.startsWith(".")) name = name.substring(1)


      //Now check if this class implements one of the specified Interfaces
      val clazz = try {
        Class.forName(name, true, clazzLoader)
      }
      catch {
        case e: ClassNotFoundException => null
        case e: NoClassDefFoundError => null
      }

      if (clazz != null && !clazz.isInterface) {

        val interfaces: Set[Class[_]] = extractInterfaces(clazz)

        interfaces.find(iface => traitSet.contains(iface)) match {
          case Some(entry) =>

            map.get(entry) match {
              case Some(entries) => map(entry) += clazz
              case None => map(entry) = mutable.HashSet[Class[_]](clazz)
            }
          case _ => //ignore
        }
      }
    } //end if(name.indexOf('$') == -1)
  }

  private def extractInterfaces(clazz: Class[_]): Set[Class[_]] = {
    var current = clazz
    var result = Set[Class[_]]()

    while (current != null) {
      clazz.getInterfaces.foreach(iface => result += iface)
      current = current.getSuperclass
    }
    result
  }
}

object ImplFinder{

  type ImplementationsMap = scala.collection.Map[Class[_], scala.collection.Set[Class[_]]]

  def apply(traits: Class[_]*) = new ImplFinder(traits.toSet)

  def apply(traitsSet: Set[Class[_]]) = new ImplFinder(traitsSet)
}




