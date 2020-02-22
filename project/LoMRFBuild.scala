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

import sbt._
import sbt.Keys._
import sbt.plugins.JvmPlugin
import de.heikoseeberger.sbtheader.HeaderPlugin
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.stage
import sbtdocker.DockerPlugin.autoImport._
import sbtdocker.mutable.Dockerfile


object LoMRFBuild extends AutoPlugin {

  val logger = ConsoleLogger()

  final val logo =
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
      | Logical Markov Random Fields (LoMRF).
    """.stripMargin

  logger.info(logo)

  private final val javaVersion: Double = sys.props("java.specification.version").toDouble

  override def requires: Plugins = {
    JvmPlugin && JavaAppPackaging && HeaderPlugin
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
        PublishingSettings ++
        DockerSettings ++
        Formatting.formatSettings
    }
  }

  private lazy val baseProjectSettings: Seq[Setting[_]] = Seq(

    organization := "com.github.anskarl",
    scalaVersion := "2.12.10",
    crossScalaVersions := Seq("2.12.10", "2.11.12"),
    name := "LoMRF",
    maintainer := "Anastasios Skarlatidis",
    developers in ThisBuild := List(
      Developer("anskarl", "Anastasios Skarlatidis", "@anskarl", url("https://github.com/anskarl")),
      Developer("vagmcs", "Evangelos Michelioudakis", "@vagmcs", url("https://github.com/vagmcs"))
    ),
    headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cStyleBlockComment),
    headerLicense := Some(HeaderLicense.Custom(logo + "\n\n")),
    autoScalaLibrary := true,
    managedScalaInstance := true,

    // fork a new JVM for 'run' and 'test:run'
    fork := true,

    // fork a new JVM for 'test:run', but not 'run'
    fork in Test := true,

    conflictManager := ConflictManager.latestRevision,

    publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))),

    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.typesafeRepo("releases"),
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
    ),

    dependencyOverrides ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
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
      "-XX:+OptimizeStringConcat",
      "-Dlogback.configurationFile=src/main/resources/logback.xml")
  )

  private lazy val ScalaSettings: Seq[Setting[_]] = Seq(
    scalacOptions := {
      scalaBinaryVersion.value match {

        case "2.11" =>
          // Scala compiler settings for Scala 2.11.x
          Seq(
            "-deprecation",       // Emit warning and location for usages of deprecated APIs.
            "-unchecked",         // Enable additional warnings where generated code depends on assumptions.
            "-feature",           // Emit warning and location for usages of features that should be imported explicitly.
            "-target:jvm-1.8",    // Target JVM version 1.8
            "-Ywarn-dead-code",   // Warn when dead code is identified.
            "-Yinline-warnings",  // Emit inlining warnings
            "-Yclosure-elim",     // Perform closure elimination
            "-Ybackend:GenBCode"  // Use the new optimisation level
          )

        case "2.12" =>
          // Scala compiler settings for Scala 2.12.x
          Seq(
            "-deprecation",       // Emit warning and location for usages of deprecated APIs.
            "-unchecked",         // Enable additional warnings where generated code depends on assumptions.
            "-feature",           // Emit warning and location for usages of features that should be imported explicitly.
            "-target:jvm-1.8",    // Target JVM version 1.8
            "-Ywarn-dead-code"    // Warn when dead code is identified.
          )

        case "2.13" => // todo: it is a WIP
          // Scala compiler settings for Scala 2.13.x
          Seq(
            "-deprecation",       // Emit warning and location for usages of deprecated APIs.
            "-unchecked",         // Enable additional warnings where generated code depends on assumptions.
            "-feature",           // Emit warning and location for usages of features that should be imported explicitly.
            "-target:jvm-1.8",    // Target JVM version 1.8
            "-Ywarn-dead-code"    // Warn when dead code is identified.
          )
        case _ => sys.error(s"Unsupported version of Scala '${scalaBinaryVersion.value}'")
      }
    }
  )

  private lazy val DockerSettings: Seq[Setting[_]] = Seq(
    dockerfile in docker := {
      val DistName = s"${name.value}-${version.value}"

      val universalBuildDir: File = stage.value

      val targetDir = s"/opt/$DistName"

      new Dockerfile {

        // Base image
        from("openjdk:11-slim")

        // Copy to docker
        copy(universalBuildDir, targetDir)

        // Add Bash support

        runRaw("apt update && apt install -y bash lp-solve wget zip unzip && rm -rf /var/lib/apt/lists/*")

        runRaw (
          "cd /tmp && " +
            "wget https://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.5/lp_solve_5.5.2.5_java.zip && " +
            "unzip lp_solve_5.5.2.5_java.zip && " +
            "cp lp_solve_5.5_java/lib/ux64/liblpsolve55j.so /usr/lib/lp_solve && " +
            "ldconfig && " +
            "rm -rf /tmp/lp_solve_5.5_java && " +
            "rm /tmp/lp_solve_5.5.2.5_java.zip"
          )

        env("LD_LIBRARY_PATH", "/usr/lib/lp_solve")

        runRaw(s"chmod +x $targetDir/bin/lomrf")
        runRaw("mkdir /data")
        workDir(s"/data")
        volume("/data")
        entryPoint(s"$targetDir/bin/lomrf")
      }
    },

    imageNames in docker := {
      val tags =
        sys.env.get("REPOSITORY_OWNER") match {
          case Some(repositoryOwner) =>
            Seq(
              s"docker.pkg.github.com/${repositoryOwner.toLowerCase}/lomrf:${version.value}",
              s"docker.pkg.github.com/${repositoryOwner.toLowerCase}/lomrf:latest")
          case None =>
            Seq(
              s"lomrf:${version.value}",
              s"lomrf:latest")
        }

      tags.map(tag => ImageName(tag))

    },

    buildOptions in docker := BuildOptions(cache = false)
  )


  private lazy val PublishingSettings: Seq[Setting[_]] = Seq(
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },

    publishTo := Option {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) "snapshots" at nexus + "content/repositories/snapshots"
      else "releases" at nexus + "service/local/staging/deploy/maven2"
    },

    scmInfo := Some(
      ScmInfo(url("https://github.com/anskarl/LoMRF"), "scm:git:git@github.com:anskarl/LoMRF.git")
    ),
    // Information required in order to sync in Maven Central
    pomExtra :=
      <url>https://github.com/anskarl</url>
        <licenses>
          <license>
            <name>Apache License Version 2.0</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0</url>
          </license>
        </licenses>
        <developers>
          <developer>
            <id>anskarl</id>
            <name>Anastasios Skarlatidis</name>
            <url>https://anskarl.github.io/</url>
          </developer>
          <developer>
            <id>vagmcs</id>
            <name>Evangelos Michelioudakis</name>
            <url>https://users.iit.demokritos.gr/~vagmcs/</url>
          </developer>
        </developers>
  )
}

