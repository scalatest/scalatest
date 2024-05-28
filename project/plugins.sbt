addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.4")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.6")

val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.15.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.1")

val scalaNativeVersion = Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.4.16")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeVersion)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.0.1")