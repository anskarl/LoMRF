/** Project */
name := "LoMRF"

version := "0.4-beta2_201505"

organization := "com.github.anskarl"

scalaVersion := "2.11.6"

autoScalaLibrary := true

managedScalaInstance := true

packageDescription in Debian := "LoMRF: Logical Markov Random Fields"

maintainer in Debian := "Anastasios Skarlatidis"

enablePlugins(JavaAppPackaging)

logLevel in Test := Level.Info
logLevel in Compile := Level.Error

// Append several options to the list of options passed to the Java compiler
javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-Xlint:deprecation")

// Append scalac options
scalacOptions ++= Seq(
	"-Yclosure-elim",
	"-Yinline",
	"-feature",
	"-target:jvm-1.8",
	"-language:implicitConversions",
	"-Ybackend:GenBCode" //use the new optimisation level
)


// fork a new JVM for 'run' and 'test:run'
fork := true

// fork a new JVM for 'test:run', but not 'run'
fork in Test := true

// add a JVM option to use when forking a JVM for 'run'
javaOptions ++= Seq(
        "-XX:+DoEscapeAnalysis",
        "-XX:+UseFastAccessorMethods",
        "-XX:+OptimizeStringConcat",
        "-Dlogback.configurationFile=src/main/resources/logback.xml")


conflictManager := ConflictManager.latestRevision

/** Dependencies */
resolvers ++= Seq(
	"typesafe" at "http://repo.typesafe.com/typesafe/releases/",
	"sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/")

resolvers += Resolver.sonatypeRepo("snapshots")
	
// Scala-lang
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)


// Scala-modules
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

// Akka.io
libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor"  % "2.3.11",
	"com.typesafe.akka" %% "akka-remote" % "2.3.11",
	"com.typesafe.akka" %% "akka-slf4j"  % "2.3.11"
)

// Logging with slf4j and logback
libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-classic" % "1.1.3",
	"ch.qos.logback" % "logback-core" % "1.1.3",
	"org.slf4j" % "slf4j-api" % "1.7.12"
)

// GNU Trove4j for high performance and memory efficient data-structures
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3"

// Unit testing
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Optimized Range foreach loops
libraryDependencies += "com.nativelibs4java" %% "scalaxy-streams" % "0.3.4" % "provided"

// JTS Topology API for modelling and manipulating 2-dimensional linear geometry
libraryDependencies += "com.vividsolutions" % "jts" % "1.13"

// Adding auxlib library requires local publishing (for details see https://github.com/anskarl/auxlib)
libraryDependencies ++= Seq(
	"com.github.anskarl" %% "auxlib-log" % "0.1-SNAPSHOT",
	"com.github.anskarl" %% "auxlib-opt" % "0.1-SNAPSHOT",
	"com.github.anskarl" %% "auxlib-trove" % "0.1-SNAPSHOT"
)

// Adding optimus library requires local publishing (for details see https://github.com/vagm/Optimus)
libraryDependencies += "com.github.vagm" %% "optimus" % "1.2.1"

// oJalgo library for optimization
libraryDependencies += "org.ojalgo" % "ojalgo" % "38.0" from "https://repo1.maven.org/maven2/org/ojalgo/ojalgo/38.0/ojalgo-38.0.jar"

// lpsolve library for optimization
libraryDependencies += "com.datumbox" % "lpsolve" % "5.5.2.0"


dependencyOverrides += "org.scala-lang" % "scala-compiler" % scalaVersion.value
dependencyOverrides += "org.scala-lang" % "scala-library" % scalaVersion.value
dependencyOverrides += "org.scala-lang" %% "scala-parser-combinators" % scalaVersion.value
dependencyOverrides += "org.scala-lang" % "scala-reflect" % scalaVersion.value
dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

// Include utility bash scripts in the 'bin' directory
mappings in Universal <++= (packageBin in Compile) map { jar =>
  val scriptsDir = new java.io.File("scripts/")
  scriptsDir.listFiles.toSeq.map { f =>
    f -> ("bin/" + f.getName)
  }
}

// Include logger configuration file to the final distribution
mappings in Universal <++= (packageBin in Compile) map { jar =>
  val scriptsDir = new java.io.File("src/main/resources/")
  scriptsDir.listFiles.toSeq.map { f =>
    f -> ("etc/" + f.getName)
  }
}

// Manage support for commercial ILP solver(s)
excludeFilter := {
  var excludeNames = Set[String]()
  try {
    val jars = unmanagedBase.value.list().map(_.toLowerCase).filter(_.endsWith(".jar"))
    if (jars.contains("gurobi.jar")){
      println("[info] Gurobi solver support is enabled")
    } else {
      println(s"[warn] Will build without the support of Gurobi solver ('gurobi.jar' is missing from '${unmanagedBase.value.getName}' directory)")
      excludeNames += "Gurobi"
    }
    excludeNames.map(n => new SimpleFileFilter(_.getName.contains(n)).asInstanceOf[FileFilter]).reduceRight(_ || _)
  } catch {
    case _ : Exception => new SimpleFileFilter(_ => false)
  }
}


val publishSettings: Setting[_] = publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
