addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.4")

val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.27")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.scala-native" % "sbt-crossproject"         % "0.2.2")

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.2")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.6")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.2.0")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.3.0")
