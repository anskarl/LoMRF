name := "LoMRF"

version := "0.6.0-SNAPSHOT"

// Load LoMRF Build settings
LoMRFBuild.settings

enablePlugins(JavaAppPackaging)

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

// Akka.io
libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor"  % "2.3.14",
	"com.typesafe.akka" %% "akka-remote" % "2.3.14",
	"com.typesafe.akka" %% "akka-slf4j"  % "2.3.14"
)

// Logging with slf4j and logback
libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-classic" % "1.1.7",
	"ch.qos.logback" % "logback-core" % "1.1.7",
	"org.slf4j" % "slf4j-api" % "1.7.21"
)

// GNU Trove4j for high performance and memory efficient data-structures
libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3"

// Unit testing
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

// Optimized Range foreach loops
libraryDependencies += "com.nativelibs4java" %% "scalaxy-streams" % "0.3.4" % "provided"

// JTS Topology API for modelling and manipulating 2-dimensional linear geometry
libraryDependencies += "com.vividsolutions" % "jts-core" % "1.14.0"

// Adding auxlib library requires local publishing (for details see https://github.com/anskarl/auxlib)
libraryDependencies += "com.github.anskarl" %% "auxlib" % "0.1"

// Adding optimus library requires local publishing (for details see https://github.com/vagm/Optimus)
libraryDependencies += "com.github.vagm" %% "optimus" % "1.2.2"

// oJalgo library for optimisation
libraryDependencies += "org.ojalgo" % "ojalgo" % "39.0" 

//from "https://repo1.maven.org/maven2/org/ojalgo/ojalgo/38.1/ojalgo-38.1.jar"

// lpsolve library for optimization
libraryDependencies += "com.datumbox" % "lpsolve" % "5.5.2.0"

// jansi
libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.11"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

// Scala-modules
dependencyOverrides += "org.scala-lang" % "scala-compiler" % scalaVersion.value
dependencyOverrides += "org.scala-lang" % "scala-library" % scalaVersion.value
dependencyOverrides += "org.scala-lang" % "scala-reflect" % scalaVersion.value
dependencyOverrides += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
