import com.typesafe.sbt.SbtNativePackager._

/** Project */
name := "LoMRF"

version := "0.3.1-beta_201503"

organization := "com.github.anskarl"

scalaVersion := "2.11.6"

autoScalaLibrary := true

managedScalaInstance := true

logLevel := Level.Warn

packageArchetype.java_application


// Append several options to the list of options passed to the Java compiler
javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-Xlint:deprecation")

// Append scalac options
scalacOptions ++= Seq(
	//"-optimise", // old optimisation is removed
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
javaOptions += "-Xmx4G"


/** Dependencies */
resolvers ++= Seq(
	"typesafe" at "http://repo.typesafe.com/typesafe/releases/",
	"sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/")
	
// Scala-lang
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Scala-modules
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

// Akka.io
libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor"  % "2.3.9",
	"com.typesafe.akka" %% "akka-remote" % "2.3.9",
	"com.typesafe.akka" %% "akka-slf4j"  % "2.3.9"
)

// Logging with slf4j and logback
libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-classic" % "1.1.2",
	"ch.qos.logback" % "logback-core" % "1.1.2",
	"org.slf4j" % "slf4j-api" % "1.7.10"
)

// GNU Trove4j for high performance and memory efficient data-structures
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3"

// Unit testing
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"


// Optimized Range foreach loops
//libraryDependencies += "com.nativelibs4java" %% "scalaxy-loops" % "0.3.4" % "provided"
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
libraryDependencies += "com.github.vagm" %% "optimus" % "1.0"

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
