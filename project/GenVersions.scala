import java.io.{File, FileWriter, BufferedWriter}

object GenVersions {

  val generatorSource = new File("GenVersions.scala")

  def genScalacticVersions(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val shortScalaVersion = scalaVersion.split("\\.").take(2).mkString(".")
    targetDir.mkdirs()

    val scalacticVersionsFile = new File(targetDir, "ScalacticVersions.scala")
    if (!scalacticVersionsFile.exists || generatorSource.lastModified > scalacticVersionsFile.lastModified) {
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

    Seq(scalacticVersionsFile)

  }

  def genScalaTestVersions(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val shortScalaVersion = scalaVersion.split("\\.").take(2).mkString(".")

    val scalaTestVersionsFile = new File(targetDir, "ScalaTestVersions.scala")
    if (!scalaTestVersionsFile.exists || generatorSource.lastModified > scalaTestVersionsFile.lastModified) {
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

    Seq(scalaTestVersionsFile)
  }

}
