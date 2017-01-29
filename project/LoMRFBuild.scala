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

import sbt._
import sbt.Keys._
import sbt.plugins.JvmPlugin
import de.heikoseeberger.sbtheader._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport.headers
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.stage
import sbtdocker.DockerPlugin.autoImport._
import sbtdocker.mutable.Dockerfile


object LoMRFBuild extends AutoPlugin {

  println {
    """
      | o                        o     o   o         o
      | |             o          |     |\ /|         | /
      | |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
      | |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
      | O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
      |             |
      |          o--o
      | o--o              o               o--o       o    o
      | |   |             |               |    o     |    |
      | O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
      | |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
      | o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
      |
      | Logical Markov Random Fields.
    """.stripMargin
  }

  private final val javaVersion: Double = sys.props("java.specification.version").toDouble

  override def requires: Plugins = {
    JvmPlugin && JavaAppPackaging && HeaderPlugin && AutomateHeaderPlugin
  }

  override def projectSettings: Seq[Setting[_]] = settings

  /**
    * Allow the plug-in to be included automatically
    */
  override def trigger: PluginTrigger = allRequirements


  private lazy val settings: Seq[Setting[_]] = {
    if(javaVersion < 1.8) sys.error("Java 8 or higher is required for this project")
    else {
      baseProjectSettings ++
        ScalaSettings ++
        JavaSettings ++
        PackagingOptions ++
        DockerSettings
    }
  }

  private lazy val baseProjectSettings: Seq[Setting[_]] = Seq(

    organization := "com.github.anskarl",
    scalaVersion := "2.11.8",
    name := "LoMRF",
    headers := projectHeaders,

    autoScalaLibrary := false,
    managedScalaInstance := true,

    // fork a new JVM for 'run' and 'test:run'
    fork := true,

    // fork a new JVM for 'test:run', but not 'run'
    fork in Test := true,

    conflictManager := ConflictManager.latestRevision,

    publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))),

    resolvers ++= Seq(
      Resolver.typesafeRepo("releases"),
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
    ),

    dependencyOverrides ++= Set(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
    )
  )

  private lazy val PackagingOptions: Seq[Setting[_]] = Seq(

    // Include utility bash scripts in the 'bin' directory
    mappings in Universal ++= {
      val scriptsDir = file("scripts/")
      scriptsDir.listFiles.toSeq.map { f =>
        f -> ("bin/" + f.getName)
      }
    },

    // Include logger configuration file to the final distribution
    mappings in Universal ++= {
      val scriptsDir = file("src/main/resources/")
      scriptsDir.listFiles.toSeq.map { f =>
        f -> ("etc/" + f.getName)
      }
    },

    // File name of universal distribution
    packageName in Universal := s"${name.value}-${version.value}"
  )

  private lazy val JavaSettings: Seq[Setting[_]] = Seq(
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-Xlint:deprecation"),

    javaOptions ++= Seq(
      "-XX:+DoEscapeAnalysis",
      "-XX:+UseFastAccessorMethods",
      "-XX:+OptimizeStringConcat",
      "-Dlogback.configurationFile=src/main/resources/logback.xml")
  )

  private lazy val ScalaSettings: Seq[Setting[_]] = Seq(
    scalacOptions ++= Seq(
      "-Yclosure-elim",
      "-Yinline",
      "-feature",
      "-target:jvm-1.8",
      "-language:implicitConversions",
      "-Ybackend:GenBCode" //use the new optimisation level
    )
  )

  private lazy val DockerSettings: Seq[Setting[_]] = Seq(
    dockerfile in docker := {
      val DistName = s"${name.value}-${version.value}"

      val universalBuildDir: File = stage.value

      val targetDir = s"/opt/$DistName"

      new Dockerfile {

        // Base image
        from("frolvlad/alpine-oraclejdk8")

        // Copy to docker
        copy(universalBuildDir, targetDir)

        // Add Bash support
        runRaw("apk add --update bash")

        // clean up package cache to reduce space
        runRaw("rm -rf /var/cache/apk/*")

        // Make consumer script executable
        runRaw(s"chmod +x $targetDir/bin/lomrf")


        runRaw("mkdir /data")
        // set working dir
        workDir(s"/data")

        volume("/data")

        entryPoint(s"$targetDir/bin/lomrf")
      }
    },

    imageNames in docker := {
      val localTag = ImageName(s"${name.value.toLowerCase}:latest")

      // Set a name with a tag that contains the project version.
      val versionTag = ImageName(s"${organization.value}/${name.value.toLowerCase}:${version.value}")

      // Set a name with the latest tag, only for stable versions.
      if(!isSnapshot.value) {
        val latestTag = ImageName(s"${organization.value}/${name.value.toLowerCase}:latest")
        Seq(versionTag, latestTag, localTag)
      }
      else Seq(versionTag, localTag)

    },

    buildOptions in docker := BuildOptions(cache = false)
  )

  lazy val projectHeaders = Map(
    "scala" -> (
      HeaderPattern.cStyleBlockComment,
      """
        |/*
        | * o                        o     o   o         o
        | * |             o          |     |\ /|         | /
        | * |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
        | * |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
        | * O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
        | *             |
        | *          o--o
        | * o--o              o               o--o       o    o
        | * |   |             |               |    o     |    |
        | * O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
        | * |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
        | * o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
        | *
        | * Logical Markov Random Fields.
        | *
        | * Copyright (c) Anastasios Skarlatidis.
        | *
        | * This file is part of Logical Markov Random Fields (LoMRF).
        | *
        | * LoMRF is free software: you can redistribute it and/or modify it
        | * under the terms of the GNU Lesser General Public License as published
        | * by the Free Software Foundation, either version 3 of the License,
        | * or (at your option) any later version.
        | *
        | * LoMRF is distributed in the hope that it will be useful, but WITHOUT
        | * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
        | * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
        | * License for more details.
        | *
        | * You should have received a copy of the GNU Lesser General Public License
        | * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
        | *
        | */
      """.stripMargin.trim + "\n\n"
    )
  )

}

