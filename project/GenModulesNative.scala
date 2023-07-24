import java.io.{File, BufferedWriter, FileWriter}
import scala.io.Source

object GenModulesNative {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26)
    else if (line.trim.startsWith("//SCALATESTNATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTNATIVE-ONLY ") + 23)
    else
      line

  private def transformLine(line: String): String =
    uncommentJsExport(line)

  private def copyFile(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipMode = false
      for (line <- lines) {
        if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START" || line.trim == "// SKIP-SCALATESTNATIVE-START")
          skipMode = true
        else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END" || line.trim == "// SKIP-SCALATESTNATIVE-END")
          skipMode = false
        else if (!skipMode) {
          destWriter.write(transformLine(line))
          destWriter.newLine()
        }
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
    }
  }

  def copyDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && f.getName.endsWith(".scala")).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }
  }

  def genScalaTestCore(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest", 
        "org/scalatest/concurrent", 
        "org/scalatest/enablers", 
        "org/scalatest/exceptions", 
        "org/scalatest/events", 
        "org/scalatest/fixture",  
        "org/scalatest/prop", 
        "org/scalatest/tagobjects", 
        "org/scalatest/time", 
        "org/scalatest/tools",  
        "org/scalatest/verbs", 
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/core/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList
  }

  def genScalaTestFeatureSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/featurespec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/featurespec/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestFlatSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/flatspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/flatspec/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestFreeSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/freespec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/freespec/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestFunSuite(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/funsuite"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/funsuite/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestFunSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/funspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/funspec/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList   

  def genScalaTestPropSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/propspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/propspec/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestWordSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/wordspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/wordspec/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestDiagrams(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/diagrams"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/diagrams/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestExpectations(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/expectations"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/expectations/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList    

  def genScalaTestMatchersCore(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/matchers", 
        "org/scalatest/matchers/dsl"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/matchers-core/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestShouldMatchers(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/matchers/should"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("jvm/shouldmatchers/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList   

}