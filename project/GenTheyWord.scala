
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
        val theyLine = itLine.replaceAll("\\sit\\(", " they\\(")
                             .replaceAll("\\sit\\s", " they ")
                             .replaceAll("\"it\\s", "\"they ")
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
    generateFile("src/test/scala/org/scalatest", 
                 "FunSpecSuite", 
                 "target/generated/src/test/scala/org/scalatest", 
                 "FunSpecSuiteUsingThey")
    generateFile("src/test/scala/org/scalatest", 
                 "FunSpecSpec", 
                 "target/generated/src/test/scala/org/scalatest", 
                 "FunSpecSpecUsingThey")
    generateFile("src/test/scala/org/scalatest", 
                 "FlatSpecSpec", 
                 "target/generated/src/test/scala/org/scalatest", 
                 "FlatSpecSpecUsingThey")
    generateFile("src/test/scala/org/scalatest/path", 
                 "FunSpecSpec", 
                 "target/generated/src/test/scala/org/scalatest/path", 
                 "FunSpecSpecUsingThey")
    generateFile("src/test/scala/org/scalatest/fixture", 
                 "FunSpecSpec", 
                 "target/generated/src/test/scala/org/scalatest/fixture", 
                 "FunSpecSpecUsingThey")
  }
  
  def genTest(dir: File, scalaVersion: String) {
    generateFile("src/test/scala/org/scalatest", 
                 "FunSpecSuite", 
                 dir.getAbsolutePath, 
                 "FunSpecSuiteUsingThey")
    generateFile("src/test/scala/org/scalatest", 
                 "FunSpecSpec", 
                 dir.getAbsolutePath, 
                 "FunSpecSpecUsingThey")
    generateFile("src/test/scala/org/scalatest", 
                 "FlatSpecSpec", 
                 dir.getAbsolutePath, 
                 "FlatSpecSpecUsingThey")
    generateFile("src/test/scala/org/scalatest/path", 
                 "FunSpecSpec", 
                 dir.getAbsolutePath, 
                 "FunSpecSpecUsingThey")
    generateFile("src/test/scala/org/scalatest/fixture", 
                 "FunSpecSpec", 
                 dir.getAbsolutePath, 
                 "FunSpecSpecUsingThey")
  }
}