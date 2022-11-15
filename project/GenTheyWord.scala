
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenTheyWord {

  val generatorSource = new File("GenTheyWord.scala")

  private def copyFile(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipMode = false
      for (line <- lines) {
          destWriter.write(line)
          destWriter.newLine()
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
    }
  }
  
  def generateFile(srcFileDir: String, srcClassName: String, targetFileDir: String, targetClassName: String): File = {
    val targetDir = new File(targetFileDir)
    targetDir.mkdirs()
    val targetFile = new File(targetFileDir, targetClassName + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
      val writer = new BufferedWriter(new FileWriter(targetFile))
      try {
        val itLines = Source.fromFile(new File(srcFileDir, srcClassName + ".scala")).getLines().toList // for 2.8
        for (itLine <- itLines) {
          //.replaceAll("\"An it clause", "\"A they clause")
          //.replaceAll("an it clause", "a they clause")
          val theyLine = itLine.replaceAll("\\sit\\(", " they\\(")
            .replaceAll("\\sit\\s", " they ")
            .replaceAll("\"it\\s", "\"they ")
            .replaceAll("they or they clause.\"", "it or they clause.\"")
            .replaceAll("\"An they clause", "\"A they clause")
            .replaceAll("an they or a they clause.\"", "an it or a they clause.\"")
            .replaceAll(srcClassName, targetClassName)
          writer.write(theyLine)
          writer.newLine() // add for 2.8
        }
        targetFile
      }
      finally {
        writer.flush()
        writer.close()
      }
    }
    targetFile
  }
  
  def main(args: Array[String]) {
  }
  
  def genTest(dir: File, version: String, scalaVersion: String): Seq[File] = {
    Seq(
      generateFile("jvm/funspec-test/src/test/scala/org/scalatest/funspec",
                   "AnyFunSpecSuite",
                   dir.getAbsolutePath,
                   "FunSpecSuiteUsingThey"),
      generateFile("jvm/funspec-test/src/test/scala/org/scalatest/funspec",
                   "AnyFunSpecSpec",
                   dir.getAbsolutePath,
                   "FunSpecSpecUsingThey"),
      generateFile("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec",
                   "AnyFlatSpecSpec",
                   dir.getAbsolutePath,
                   "FlatSpecSpecUsingThey"),
      copyFile(new File("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec/SlowTest.scala"), 
               new File(dir.getAbsolutePath, "SlowTest.scala")),              
      generateFile("jvm/scalatest-test/src/test/scala/org/scalatest/path",
                   "FunSpecSpec",
                   (new File(dir, "path")).getAbsolutePath,
                   "PathFunSpecSpecUsingThey"),
      generateFile("jvm/funspec-test/src/test/scala/org/scalatest/funspec",
                   "FixtureAnyFunSpecSpec",
                   dir.getAbsolutePath,
                   "FixtureFunSpecSpecUsingThey"),
      generateFile("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec",
                   "FixtureAnyFlatSpecSpec",
                   dir.getAbsolutePath,
                   "FixtureFlatSpecSpecUsingThey")
    )
  }
}