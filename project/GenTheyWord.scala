
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenTheyWord {

  val generatorSource = new File("GenTheyWord.scala")
  
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
                   "FunSpecSuite",
                   dir.getAbsolutePath,
                   "FunSpecSuiteUsingThey"),
      generateFile("jvm/funspec-test/src/test/scala/org/scalatest/funspec",
                   "FunSpecSpec",
                   dir.getAbsolutePath,
                   "FunSpecSpecUsingThey"),
      generateFile("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec",
                   "FlatSpecSpec",
                   dir.getAbsolutePath,
                   "FlatSpecSpecUsingThey"),
      generateFile("jvm/scalatest-test/src/test/scala/org/scalatest/path",
                   "FunSpecSpec",
                   (new File(dir, "path")).getAbsolutePath,
                   "PathFunSpecSpecUsingThey"),
      generateFile("jvm/scalatest-test/src/test/scala/org/scalatest/fixture",
                   "FunSpecSpec",
                   (new File(dir, "fixture")).getAbsolutePath,
                   "FixtureFunSpecSpecUsingThey"),
      generateFile("jvm/scalatest-test/src/test/scala/org/scalatest/fixture",
                   "FlatSpecSpec",
                   (new File(dir, "fixture")).getAbsolutePath,
                   "FixtureFlatSpecSpecUsingThey")
    )
  }
}