/*
* Copyright 2001-2015 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

import sbt.IO

import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenScalaTestDotty {

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

  def copyFiles(sourceDirName: String, packageDirName: String, targetDir: File, files: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    files.map { sourceFileName =>
      val sourceFile = new File(sourceDir, sourceFileName)
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  def copyStartsWithFiles(sourceDirName: String, packageDirName: String, startsWith: String, targetDir: File): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && f.getName.startsWith(startsWith) && f.getName.endsWith(".scala")).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
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

  def copyResourceDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName)).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        IO.copyFile(sourceFile, destFile)
      destFile
    }
  }

  def genJava(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("scalatest/src/main/java/org/scalatest", "org/scalatest", targetDir,
      List(
        "Finders.java",
        "TagAnnotation.java",
        "WrapWith.java",
        "DoNotDiscover.java",
        "Ignore.java"
      )) ++ 
    copyDir("scalatest/src/main/java/org/scalatest/tags", "org/scalatest/tags", targetDir, List.empty)  
  }

  def genHtml(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyResourceDir("scalatest/src/main/html", "html", targetDir, List.empty) ++
    copyResourceDir("scalatest/src/main/resources/images", "images", targetDir, List.empty) ++
    copyResourceDir("scalatest/src/main/resources/org/scalatest", "org/scalatest", targetDir, List.empty)
  }

  val genScalaPackages: Map[String, List[String]] = 
    Map(
      "org/scalatest" -> List(
        "Assertions.scala",                 // Re-implemented
        "AssertionsMacro.scala",            // Re-implemented
        "CompileMacro.scala",               // Re-implemented
        "DiagrammedAssertions.scala",       // Re-implemented
        "DiagrammedAssertionsMacro.scala",  // Re-implemented
        "DiagrammedExprMacro.scala",        // Re-implemented
        "DiagrammedExpr.scala",             // Re-implemented
        "Expectations.scala",               // Re-implemented
        "ExpectationsMacro.scala",          // Re-implemented
        "StreamlinedXml.scala",             // Hmm, not sure what to do with XML support, let's ask.
        "StreamlinedXmlEquality.scala",     // Hmm, not sure what to do with XML support, let's ask.
        "StreamlinedXmlNormMethods.scala"   // Hmm, not sure what to do with XML support, let's ask.
      ), 
      "org/scalatest/concurrent" -> List.empty, 
      "org/scalatest/diagrams" -> List(
        "Diagrams.scala", 
        "DiagramsMacro.scala"
      ), 
      "org/scalatest/exceptions" -> List.empty, 
      "org/scalatest/enablers" -> List.empty, 
      "org/scalatest/events" -> List.empty, 
      "org/scalatest/fixture" -> List.empty, 
      "org/scalatest/featurespec" -> List.empty, 
      "org/scalatest/funspec" -> List.empty, 
      "org/scalatest/funsuite" -> List.empty, 
      "org/scalatest/freespec" -> List.empty, 
      "org/scalatest/flatspec" -> List.empty, 
      "org/scalatest/matchers" -> List(
        "Matcher.scala",           // Re-implemented with new macro
        "MatchPatternMacro.scala", // Re-implemented with new macro
        "TypeMatcherMacro.scala"   // Re-implemented with new macro
      ), 
      "org/scalatest/matchers/dsl" -> List(
        "BeWord.scala", 
        "JavaCollectionWrapper.scala",
        "JavaMapWrapper.scala",
        "MatchPatternWord.scala",
        "NotWord.scala",
        "ResultOfNotWordForAny.scala"
      ), 
      "org/scalatest/matchers/should" -> List.empty, 
      "org/scalatest/path" -> List.empty, 
      "org/scalatest/prop" -> List.empty, 
      "org/scalatest/propspec" -> List.empty, 
      "org/scalatest/tagobjects" -> List.empty, 
      "org/scalatest/time" -> List.empty, 
      "org/scalatest/verbs" -> List.empty, 
      "org/scalatest/tools" -> List.empty, 
      "org/scalatest/refspec" -> List.empty, 
      "org/scalatest/words" -> List.empty, 
      "org/scalatest/wordspec" -> List.empty
    )

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    genScalaPackages.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "AssertionsSpec.scala",
        // "ShouldCompileSpec.scala",
        // "ShouldNotCompileSpec.scala",
        // "ShouldNotTypeCheckSpec.scala"
      )
    ) ++
    copyDir("scalatest-test/src/test/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, List.empty) ++
    copyDir("scalatest-test/src/test/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir,
      List(
        "DirectExpectationsSpec.scala"
      )
    )
    /*++
      copyDir("scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,
        List(
          "WaitersSpec.scala",    // skipped because Waiters not supported.
          "AsyncAssertionsSpec.scala",    // skipped because AsyncAssertions (deprecated name for Waiters) not supported.
          "ConductorFixtureSuite.scala",  // skipped because Conductors not supported.
          "ConductorMethodsSuite.scala",   // skipped because Conductors not supported.
          "ConductorSuite.scala",   // skipped because Conductors not supported.
          "ConductorFixtureDeprecatedSuite.scala",  // skipped because Conductors not supported.
          "ConductorMethodsDeprecatedSuite.scala",   // skipped because Conductors not supported.
          "ConductorDeprecatedSuite.scala",   // skipped because Conductors not supported.
          "EventuallySpec.scala",   // skipped because Eventually not supported.
          "IntegrationPatienceSpec.scala",  // skipped because depends on Eventually
          "DeprecatedIntegrationPatienceSpec.scala",
          "JavaFuturesSpec.scala",      // skipped because depends on java futures
          "TestThreadsStartingCounterSpec.scala",   // skipped because depends on Conductors
          "DeprecatedTimeLimitedTestsSpec.scala",   // skipped because DeprecatedTimeLimitedTests not supported.
          "TimeoutsSpec.scala",            // skipped because Timeouts not supported.
          "UltimatelySpec.scala"   // skipped because Eventually not supported.
        )) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir,
        List(
          "TestLocationJUnit3Suite.scala",
          "TestLocationJUnitSuite.scala",
          "TestLocationTestNGSuite.scala",
          "TestLocationMethodJUnit3Suite.scala",
          "TestLocationMethodJUnitSuite.scala",
          "TestLocationMethodTestNGSuite.scala",
          "LocationMethodSuiteProp.scala"
        )) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir,
        List(
          "SpecSpec.scala",     // skipped because depends on java reflections
          "SuiteSpec.scala"    // skipped because depends on java reflections
        )) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/suiteprop", "org/scalatest/suiteprop", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/time", "org/scalatest/time", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/words", "org/scalatest/words", targetDir, List.empty) ++
      copyDir("scalatest-test/src/test/scala/org/scalatest/tools", "org/scalatest/tools", targetDir,
        List(
          "DashboardReporterSpec.scala",
          "DiscoverySuiteSuite.scala",
          "FilterReporterSpec.scala",
          "FrameworkSuite.scala",
          "HtmlReporterSpec.scala",
          "JUnitXmlReporterSuite.scala",
          "MemoryReporterSuite.scala",
          "RunnerSpec.scala",
          "SbtCommandParserSpec.scala",
          "ScalaTestAntTaskSpec.scala",
          "ScalaTestFrameworkSuite.scala",
          "ScalaTestRunnerSuite.scala",
          "SomeApiClass.scala",
          "SomeApiClassRunner.scala",
          "SomeApiSubClass.scala",
          "StringReporterAlertSpec.scala",
          "StringReporterSuite.scala",
          "StringReporterSummarySpec.scala",
          "SuiteDiscoveryHelperSuite.scala",
          "XmlSocketReporterSpec.scala"
        )
      )*/
  }

}
