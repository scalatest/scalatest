addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.18")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
