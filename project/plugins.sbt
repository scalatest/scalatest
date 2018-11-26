addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.25")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.scala-native" % "sbt-crossproject"         % "0.2.0")

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.8")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")
