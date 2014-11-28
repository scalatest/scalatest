import java.io.{File, FileWriter, BufferedWriter}

object GenVersions {

  def genMain(targetDir: File, version: String, scalaVersion: String) {
    val shortScalaVersion = scalaVersion.split("\\.").take(2).mkString(".")
    targetDir.mkdirs()

    val scalaTestVersionsFile = new File(targetDir, "ScalaTestVersions.scala")
    val scalaTestVersionsFileWriter = new BufferedWriter(new FileWriter(scalaTestVersionsFile))

    scalaTestVersionsFileWriter.write(
      s"""package org.scalatest
        |
        |private[scalatest] object ScalaTestVersions {
        |  val ScalaTestVersion: String = "$version"
        |  val BuiltForScalaVersion: String = "$shortScalaVersion"
        |}
      """.stripMargin
    )

    scalaTestVersionsFileWriter.flush()
    scalaTestVersionsFileWriter.close()
    println("Generated " + scalaTestVersionsFile.getAbsolutePath)
  }

  def genMainScalactic(targetDir: File, version: String, scalaVersion: String) {
    val shortScalaVersion = scalaVersion.split("\\.").take(2).mkString(".")
    targetDir.mkdirs()

    val scalacticVersionsFile = new File(targetDir, "ScalacticVersions.scala")
    val scalacticVersionsFileWriter = new BufferedWriter(new FileWriter(scalacticVersionsFile))

    scalacticVersionsFileWriter.write(
      s"""package org.scalactic
        |
        |private[scalactic] object ScalacticVersions {
        |  val ScalacticVersion: String = "$version"
        |  val BuiltForScalaVersion: String = "$shortScalaVersion"
        |}
      """.stripMargin
    )

    scalacticVersionsFileWriter.flush()
    scalacticVersionsFileWriter.close()
    println("Generated " + scalacticVersionsFile.getAbsolutePath)
  }
}
