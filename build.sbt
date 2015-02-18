import com.typesafe.sbt.SbtNativePackager._

/** Project */
name := "LoMRF"

version := "0.3.0-beta_201412"

organization := "com.github.anskarl"

scalaVersion := "2.10.4"

autoScalaLibrary := true

managedScalaInstance := true

packageArchetype.java_application


// Append several options to the list of options passed to the Java compiler
javacOptions ++= Seq("-source", "1.7", "-target", "1.7", "-Xlint:unchecked", "-Xlint:deprecation")

// Append scalac options
scalacOptions ++= Seq(
	"-optimise",
	"-Yclosure-elim",
	"-Yinline",
	"-feature",
	"-target:jvm-1.7",
	"-language:implicitConversions"
)


// fork a new JVM for 'run' and 'test:run'
fork := true

// fork a new JVM for 'test:run', but not 'run'
fork in Test := true

// add a JVM option to use when forking a JVM for 'run'
javaOptions += "-Xmx1G"


/** Dependencies */
resolvers ++= Seq(
	"typesafe" at "http://repo.typesafe.com/typesafe/releases/",
	"sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/")
	
// Scala-lang
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Akka.io
libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor"  % "2.3.7",
	"com.typesafe.akka" %% "akka-remote" % "2.3.7",
	"com.typesafe.akka" %% "akka-slf4j"  % "2.3.7"
)

// Logging with slf4j and logback
libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-classic" % "1.1.2",
	"ch.qos.logback" % "logback-core" % "1.1.2",
	"org.slf4j" % "slf4j-api" % "1.7.7"
)

// GNU Trove4j for high performance and memory efficient data-structures
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3"

// Unit testing
libraryDependencies ++= Seq(
	"junit" % "junit" % "4.12" % "test",
	"org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

// Optimized Range foreach loops
libraryDependencies += "com.nativelibs4java" %% "scalaxy-loops" % "0.3.3" % "provided"

// JTS Topology API for modelling and manipulating 2-dimensional linear geometry
libraryDependencies += "com.vividsolutions" % "jts" % "1.13"

// add auxlib-* requires local publishing (for details see https://github.com/anskarl/auxlib)
libraryDependencies ++= Seq(
	"com.github.anskarl" %% "auxlib-log" % "0.1-SNAPSHOT",
	"com.github.anskarl" %% "auxlib-opt" % "0.1-SNAPSHOT",
	"com.github.anskarl" %% "auxlib-trove" % "0.1-SNAPSHOT"
)

// add optimus requires local publishing (for details see https://github.com/vagm/Optimus)
//libraryDependencies += "com.github.vagm" %% "Optimus" % "1.0"

// TODO: add abide support (see https://github.com/scala/scala-abide)

// Include utility BASH scripts in the 'bin' directory
mappings in Universal <++= (packageBin in Compile) map { jar =>
  val scriptsDir = new java.io.File("scripts/")
  scriptsDir.listFiles.toSeq.map { f =>
    f -> ("bin/" + f.getName)
  }
}
