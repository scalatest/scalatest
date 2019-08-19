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
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, 
              if (packagePath == "org/scalatest" || packagePath == "org/scalatest/fixture") skipList ++ List("package.scala") else skipList)
    }.toList ++ 
    copyDir("scalatest.native/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++
    copyDir("scalatest.native/src/main/scala/org/scalatest/compatible", "org/scalatest/compatible", targetDir, List.empty) ++
    copyDir("scalatest.native/src/main/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir, List.empty) ++
    copyDir("scalatest.native/src/main/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, List.empty) ++
    copyDir("scalatest.native/src/main/scala/org/scalatest/tools", "org/scalatest/tools", targetDir, List.empty) ++ 
    copyDir("modules/jvm/scalatest-core/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++ 
    copyDir("modules/jvm/scalatest-core/src/main/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir, List.empty)
  }

  def genScalaTestFeatureSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/featurespec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestFlatSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/flatspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestFreeSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/freespec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestFunSuite(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/funsuite"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestPropSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/propspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestWordSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/wordspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestDiagrams(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/diagrams"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestMatchersCore(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/matchers", 
        "org/scalatest/matchers/dsl"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList  

  def genScalaTestShouldMatchers(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestNative.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/matchers/should"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList   

}