import java.io.{File, BufferedWriter, FileWriter}
import scala.io.Source

object GenModulesDotty {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
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
        if (line.trim == "// SKIP-DOTTY-START" || line.trim == "// SKIP-DOTTY-START")
          skipMode = true
        else if (line.trim == "// SKIP-DOTTY-END" || line.trim == "// SKIP-DOTTY-END")
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
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  def genScalaTestCore(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest", 
        "org/scalatest/compatible", 
        "org/scalatest/concurrent", 
        "org/scalatest/enablers", 
        "org/scalatest/exceptions", 
        "org/scalatest/events", 
        "org/scalatest/fixture",  
        "org/scalatest/prop", 
        "org/scalatest/tagobjects", 
        "org/scalatest/tags", 
        "org/scalatest/time", 
        "org/scalatest/tools",  
        "org/scalatest/verbs", 
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, 
             if (packagePath == "org/scalatest" || packagePath == "org/scalatest/fixture") skipList ++ List("package.scala") else skipList)
    }.toList ++
    copyDir("scalatest.dotty/src/main/scala/org/scalatest", "org/scalatest", targetDir, 
      List(
        "package.scala"
      )
    ) ++ 
    copyDir("scalatest.dotty/src/main/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir,List.empty)
  }

  def genScalaTestCoreJava(targetDir: File, version: String, scalaVersion: String): Seq[File] = { 
    copyDir("scalatest/src/main/java/org/scalatest", "org/scalatest", targetDir,List.empty) ++ 
    copyDir("scalatest/src/main/java/org/scalatest/tags", "org/scalatest/tags", targetDir,List.empty)
  }

  def genScalaTestFeatureSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/featurespec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestFlatSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/flatspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestFreeSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/freespec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestFunSuite(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/funsuite"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestPropSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/propspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestRefSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/refspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestWordSpec(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/wordspec"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestDiagrams(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/diagrams"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genScalaTestMatchersCore(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/matchers", 
        "org/scalatest/matchers/dsl"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList ++ 
    copyDir("scalatest.dotty/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir,List.empty) ++ 
    copyDir("scalatest.dotty/src/main/scala/org/scalatest/matchers/dsl", "org/scalatest/matchers/dsl", targetDir,List.empty)

  def genScalaTestShouldMatchers(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    GenScalaTestDotty.genScalaPackages.filter { case (packagePath, skipList) =>
      List(
        "org/scalatest/matchers/should"
      ).contains(packagePath)
    }.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList ++ 
    copyDir("scalatest.dotty/src/main/scala/org/scalatest/matchers/should", "org/scalatest/matchers/should", targetDir,List.empty)

  def genScalaTestMustMatchers(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("scalatest.dotty/src/main/scala/org/scalatest/matchers/must", "org/scalatest/matchers/must", targetDir,List.empty)  

}