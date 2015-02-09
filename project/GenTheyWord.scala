
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenTheyWord {
  
  def generateFile(srcFileDir: String, srcClassName: String, targetFileDir: String, targetClassName: String) {
    val targetDir = new File(targetFileDir)
    targetDir.mkdirs()
    val writer = new BufferedWriter(new FileWriter(new File(targetFileDir, targetClassName + ".scala")))
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
    }
    finally {
      writer.close()
    }
  }
  
  def main(args: Array[String]) {
  }
  
  def genTest(dir: File, version: String, scalaVersion: String) {
    generateFile("scalatest-test/src/test/scala/org/scalatest",
                 "FunSpecSuite", 
                 dir.getAbsolutePath, 
                 "FunSpecSuiteUsingThey")
    generateFile("scalatest-test/src/test/scala/org/scalatest",
                 "FunSpecSpec", 
                 dir.getAbsolutePath, 
                 "FunSpecSpecUsingThey")
    generateFile("scalatest-test/src/test/scala/org/scalatest",
                 "FlatSpecSpec", 
                 dir.getAbsolutePath, 
                 "FlatSpecSpecUsingThey")
    generateFile("scalatest-test/src/test/scala/org/scalatest/path",
                 "FunSpecSpec", 
                 dir.getAbsolutePath, 
                 "PathFunSpecSpecUsingThey")
    generateFile("scalatest-test/src/test/scala/org/scalatest/fixture",
                 "FunSpecSpec", 
                 dir.getAbsolutePath, 
                 "FixtureFunSpecSpecUsingThey")
    generateFile("scalatest-test/src/test/scala/org/scalatest/fixture",
                 "FlatSpecSpec",
                 dir.getAbsolutePath,
                 "FixtureFlatSpecSpecUsingThey")
  }
}