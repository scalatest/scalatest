addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.6")

val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.19.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.1")

val scalaNativeVersion = Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.5.8")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeVersion)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.0.1")