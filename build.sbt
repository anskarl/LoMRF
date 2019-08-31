addCommandAlias("build", ";headerCreate;compile;test;package")
addCommandAlias("rebuild", ";clean;build")

lazy val lomrf = Project("LoMRF", file("."))
  .enablePlugins(JavaAppPackaging, sbtdocker.DockerPlugin, AutomateHeaderPlugin)
	.settings(Test / logLevel := Level.Info)
	.settings(Compile / logLevel := Level.Error)
	.settings(libraryDependencies ++= Dependencies.Akka)
	.settings(libraryDependencies ++= Dependencies.Logging)
	.settings(libraryDependencies ++= Dependencies.Utils)
	.settings(libraryDependencies ++= Dependencies.Optimus)
	.settings(libraryDependencies += Dependencies.ScalaTest)
	.settings(libraryDependencies ++= {
		CrossVersion.partialVersion(scalaVersion.value) match {
			case Some((2, major)) if major >= 13 =>
				Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
			case _ =>
				Seq()
		}
	})
