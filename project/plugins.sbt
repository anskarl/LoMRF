resolvers += Resolver.typesafeRepo("releases")
resolvers += Resolver.
  url("hmrc-sbt-plugin-releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns)


addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.1.5")
addSbtPlugin("com.scalapenos" % "sbt-prompt" % "1.0.0")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.4")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.6.0")