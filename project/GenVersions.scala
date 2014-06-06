import java.io.{File, FileWriter, BufferedWriter}

object GenVersions {

  def genMain(targetDir: File, version: String, scalaVersion: String) {
    val shortScalaVersion = scalaVersion.split("\\.").take(2).mkString(".")
    targetDir.mkdirs()

    val versionsFile = new File(targetDir, "Versions.scala")
    val versionsFileWriter = new BufferedWriter(new FileWriter(versionsFile))

    versionsFileWriter.write(
      s"""package org.scalactic
        |
        |private[scalactic] object Versions {
        |  val version: String = "$version"
        |  val buildForScalaVersion: String = "$shortScalaVersion"
        |}
      """.stripMargin
    )

    versionsFileWriter.flush()
    versionsFileWriter.close()
    println("Generated " + versionsFile.getAbsolutePath)
  }

}