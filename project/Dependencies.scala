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

object Dependencies {

  object v {
    final val Akka = "2.5.21"

    final val ScalaLogging = "3.9.2"
    final val Logback = "1.2.3"
    final val SLF4J = "1.7.25"
    final val JANSI = "1.11"

    final val ScalaTest = "3.0.5"

    final val Trove4j = "3.0.3" // todo upgrade to 3.1
    final val JTS = "1.14.0"

    final val Enums = "1.5.13"

    final val Optimus = "3.0.0"
    final val Breeze = "0.13.2"

    final val Spire = "0.13.0"
  }

  // Akka.io
  lazy val Akka = Seq(
    "com.typesafe.akka" %% "akka-actor"  % v.Akka,
    "com.typesafe.akka" %% "akka-remote" % v.Akka,
    "com.typesafe.akka" %% "akka-slf4j"  % v.Akka
  )

  // Logging with slf4j and logback
  lazy val Logging = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % v.ScalaLogging,
    "ch.qos.logback" % "logback-classic" % v.Logback,
    "ch.qos.logback" % "logback-core" % v.Logback,
    "org.slf4j" % "slf4j-api" % v.SLF4J
  )

  // ScalaTest got Unit testing
  lazy val Testing = Seq(
    "org.scalatest" %% "scalatest" % v.ScalaTest % "test"
  )

  lazy val Utils = Seq(
    // GNU Trove4j for high performance and memory efficient data-structures
    "net.sf.trove4j" % "trove4j" % v.Trove4j, // todo: v3.1 and change artifact from 'trove4j' to 'core'

    // JTS Topology API for modelling and manipulating 2-dimensional linear geometry
    "com.vividsolutions" % "jts-core" % v.JTS,

    // Optimized Range foreach loops
    //"com.nativelibs4java" %% "scalaxy-streams" % v.Scalaxy % "provided",

    "org.fusesource.jansi" % "jansi" % v.JANSI,

    // Breeze library for efficient numerical processing
    "org.scalanlp" %% "breeze" % v.Breeze,
    "org.scalanlp" %% "breeze-natives" % v.Breeze,

    "com.beachape" %% "enumeratum" % v.Enums,

    "org.spire-math" %% "spire" % v.Spire,
    "org.spire-math" %% "spire-macros" % v.Spire
  )

  // Optimus library for linear and quadratic optimization
  lazy val Optimus = Seq(
    "com.github.vagmcs" %% "optimus" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-oj" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-lp" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-gurobi" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-mosek" % v.Optimus
  )

}