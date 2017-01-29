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

  final val AkkaVersion = "2.3.14"
  final val LogbackVersion = "1.1.7"
  final val SLF4JVersion = "1.7.21"
  final val ScalaTestVesion = "2.2.6"
  final val OptimusVersion = "2.0.0"


  // Akka.io
  lazy val Akka = Seq(
    "com.typesafe.akka" %% "akka-actor"  % AkkaVersion,
    "com.typesafe.akka" %% "akka-remote" % AkkaVersion,
    "com.typesafe.akka" %% "akka-slf4j"  % AkkaVersion
  )


  // Logging with slf4j and logback
  lazy val Logging = Seq(
    "ch.qos.logback" % "logback-classic" % LogbackVersion,
    "ch.qos.logback" % "logback-core" % LogbackVersion,
    "org.slf4j" % "slf4j-api" % SLF4JVersion
  )

  // ScalaTest got Unit testing
  lazy val ScalaTest = "org.scalatest" %% "scalatest" % ScalaTestVesion % "test"


  lazy val Utils = Seq(
    // GNU Trove4j for high performance and memory efficient data-structures
    "net.sf.trove4j" % "trove4j" % "3.0.3",

    // JTS Topology API for modelling and manipulating 2-dimensional linear geometry
    "com.vividsolutions" % "jts-core" % "1.14.0",

    // Optimized Range foreach loops
    "com.nativelibs4java" %% "scalaxy-streams" % "0.3.4" % "provided",

    // Adding auxlib library requires local publishing (for details see https://github.com/anskarl/auxlib)
    "com.github.anskarl" %% "auxlib" % "0.2.0",

    "org.fusesource.jansi" % "jansi" % "1.11"
  )

  lazy val Optimus = Seq(
    "com.github.vagmcs" %% "optimus" % OptimusVersion,
    "com.github.vagmcs" %% "optimus-solver-oj" % OptimusVersion,
    "com.github.vagmcs" %% "optimus-solver-lp" % OptimusVersion,
    "com.github.vagmcs" %% "optimus-solver-gurobi" % OptimusVersion
  )




}