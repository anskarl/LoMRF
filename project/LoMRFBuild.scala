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
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */


import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import sbt._
import sbt.Keys._
import com.typesafe.sbt.packager.debian.DebianPlugin.autoImport._
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._
import sbt.dsl._


object LoMRFBuild {
  val javaVersion = sys.props("java.specification.version").toDouble

  lazy val settings: Seq[Setting[_]] = {
    if(javaVersion < 1.7)
      sys.error("Java 7 or higher is required for this project")
    else if(javaVersion == 1.7){
      println("[info] Loading settings for Java 7. However it is strongly recommended to use Java 8 or higher.")
      jdk7Settings
    }
    else {
      println("[info] Loading settings for Java 8 or higher.")
      jdk8Settings
    }
  }

  private val commonSettings: Seq[Setting[_]] = Seq(

    organization := "com.github.anskarl",
    scalaVersion := "2.11.7",
    packageDescription in Debian := "LoMRF: Logical Markov Random Fields",
    maintainer in Debian := "Anastasios Skarlatidis",

    autoScalaLibrary := true,
    managedScalaInstance := true,

    logLevel in Test := Level.Info,
    logLevel in Compile := Level.Error,

    javaOptions ++= Seq(
      "-XX:+DoEscapeAnalysis",
      "-XX:+UseFastAccessorMethods",
      "-XX:+OptimizeStringConcat",
      "-Dlogback.configurationFile=src/main/resources/logback.xml"),

    // fork a new JVM for 'run' and 'test:run'
    fork := true,

    // fork a new JVM for 'test:run', but not 'run'
    fork in Test := true,

    conflictManager := ConflictManager.latestRevision,

    publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))),

    // Include utility bash scripts in the 'bin' directory
    mappings in Universal <++= (packageBin in Compile) map { jar =>
      val scriptsDir = new java.io.File("scripts/")
      scriptsDir.listFiles.toSeq.map { f =>
        f -> ("bin/" + f.getName)
      }
    },

    // Include logger configuration file to the final distribution
    mappings in Universal <++= (packageBin in Compile) map { jar =>
      val scriptsDir = new java.io.File("src/main/resources/")
      scriptsDir.listFiles.toSeq.map { f =>
        f -> ("etc/" + f.getName)
      }
    }

  )

  private lazy val jdk7Settings: Seq[Setting[_]] = commonSettings ++ Seq(
    javacOptions ++= Seq("-source", "1.7", "-target", "1.7", "-Xlint:unchecked", "-Xlint:deprecation"),
    scalacOptions ++= Seq(
      "-Yclosure-elim",
      "-Yinline",
      "-feature",
      "-target:jvm-1.7",
      "-language:implicitConversions",
      "-optimize" // old optimisation level
      )
  )
  private lazy val jdk8Settings: Seq[Setting[_]] = commonSettings ++ Seq(
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-Xlint:deprecation"),
    scalacOptions ++= Seq(
      "-Yclosure-elim",
      "-Yinline",
      "-feature",
      "-target:jvm-1.8",
      "-language:implicitConversions",
      "-Ybackend:GenBCode" //use the new optimisation level
    )
  )

}

