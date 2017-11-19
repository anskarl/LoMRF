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

object Dependencies {

  object v {
    final val Akka = "2.5.6"
    final val Logback = "1.2.3"
    final val SLF4JVersion = "1.7.25"
    final val ScalaTest = "3.0.4"
    final val Optimus = "2.0.0"
    final val Trove4j = "3.0.3"
    final val JTS = "1.14.0"
    final val Scalaxy = "0.3.4"
    final val AuxLib = "0.3.0"
    final val JANSI = "1.11"
  }


  // Akka.io
  lazy val Akka = Seq(
    "com.typesafe.akka" %% "akka-actor"  % v.Akka,
    "com.typesafe.akka" %% "akka-remote" % v.Akka,
    "com.typesafe.akka" %% "akka-slf4j"  % v.Akka
  )


  // Logging with slf4j and logback
  lazy val Logging = Seq(
    "ch.qos.logback" % "logback-classic" % v.Logback,
    "ch.qos.logback" % "logback-core" % v.Logback,
    "org.slf4j" % "slf4j-api" % v.SLF4JVersion
  )

  // ScalaTest got Unit testing
  lazy val ScalaTest = "org.scalatest" %% "scalatest" % v.ScalaTest % "test"


  lazy val Utils = Seq(
    // GNU Trove4j for high performance and memory efficient data-structures
    "net.sf.trove4j" % "trove4j" % v.Trove4j,

    // JTS Topology API for modelling and manipulating 2-dimensional linear geometry
    "com.vividsolutions" % "jts-core" % v.JTS,

    // Optimized Range foreach loops
    "com.nativelibs4java" %% "scalaxy-streams" % v.Scalaxy % "provided",

    // Adding auxlib library requires local publishing (for details see https://github.com/anskarl/auxlib)
    "com.github.anskarl" %% "auxlib-log" % v.AuxLib,
    "com.github.anskarl" %% "auxlib-opt" % v.AuxLib,
    "com.github.anskarl" %% "auxlib-trove" % v.AuxLib,

    "org.fusesource.jansi" % "jansi" % v.JANSI
  )

  lazy val Optimus = Seq(
    "com.github.vagmcs" %% "optimus" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-oj" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-lp" % v.Optimus,
    "com.github.vagmcs" %% "optimus-solver-gurobi" % v.Optimus
  )




}