import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenEvery {

  def transformLine(line: String): String = line

  private def copyFile(sourceFile: File, destFile: File, scalaVersion: String): File = {
    destFile.getParentFile.mkdirs()
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      for (line <- lines) {
        destWriter.write(transformLine(line))
        destWriter.newLine()
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Generated " + destFile.getAbsolutePath)
    }
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    Seq(
      copyFile(new File("project/templates/Every.scala.template"), new File(targetDir, "Every.scala"), scalaVersion)
    )

}