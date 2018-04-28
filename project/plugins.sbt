resolvers += Resolver.typesafeRepo("releases")
resolvers += Resolver.
  url("hmrc-sbt-plugin-releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns)


addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.4")

addSbtPlugin("com.scalapenos" % "sbt-prompt" % "1.0.2")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.8")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.0.0")

addSbtPlugin("se.marcuslonnberg" % "sbt-docker" % "1.5.0")