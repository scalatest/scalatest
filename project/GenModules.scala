import java.io.{File, BufferedWriter, FileWriter}
import scala.io.Source

object GenModules {

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

  private def copyDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  def genScalaTestCompatible(targetDir: File, version: String, scalaVersion: String): Seq[File] = { 
    copyDir("scalatest/src/main/java/org/scalatest/compatible", "org/scalatest/compatible", targetDir,List.empty)
  }

  def genScalaTestCore(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("scalatest/src/main/scala/org/scalatest", "org/scalatest", targetDir, 
      List("package.scala")
    ) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,List.empty) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir,List.empty) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir,List.empty) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/events", "org/scalatest/events", targetDir,List.empty) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir, 
      List("package.scala")
    ) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/prop", "org/scalatest/prop", targetDir,List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/tagobjects", "org/scalatest/tagobjects", targetDir,List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/time", "org/scalatest/time", targetDir,List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/tools", "org/scalatest/tools", targetDir,List.empty) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/verbs", "org/scalatest/verbs", targetDir,List.empty)
  }

  def genScalaTestCoreJava(targetDir: File, version: String, scalaVersion: String): Seq[File] = { 
    copyDir("scalatest/src/main/java/org/scalatest", "org/scalatest", targetDir,List.empty) ++ 
    copyDir("scalatest/src/main/java/org/scalatest/tags", "org/scalatest/tags", targetDir,List.empty)
  }

  def genScalaTestCoreTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir, List.empty)
  }

  def genScalaTestFeatureSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty)

  def genScalaTestFlatSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/flatspec", "org/scalatest/flatspec", targetDir, List.empty)  

  def genScalaTestFreeSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/freespec", "org/scalatest/freespec", targetDir, List.empty)

  def genScalaTestFunSuite(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, List.empty)

  def genScalaTestFunSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, List.empty)  

  def genScalaTestPropSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/propspec", "org/scalatest/propspec", targetDir, List.empty)

  def genScalaTestRefSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/refspec", "org/scalatest/refspec", targetDir, List.empty)

  def genScalaTestWordSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/wordspec", "org/scalatest/wordspec", targetDir, List.empty)

  def genScalaTestDiagrams(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, List.empty)

  def genScalaTestMatchersCore(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++ 
    copyDir("scalatest/src/main/scala/org/scalatest/matchers/dsl", "org/scalatest/matchers/dsl", targetDir, List.empty)

  def genScalaTestShouldMatchers(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest/src/main/scala/org/scalatest/matchers/should", "org/scalatest/matchers/should", targetDir, List.empty)             
}