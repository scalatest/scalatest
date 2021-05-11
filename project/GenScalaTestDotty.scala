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

import scala.io.Source
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

  private def uncommentJsExportJS(line: String): String =
    if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26)
    else if (line.trim.startsWith("//SCALATESTJS-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19)  
    else
      line

  private def transformLineJS(line: String): String =
    uncommentJsExportJS(line)

  private def copyFileJS(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipDottyMode = false
      var skipJSMode = false
      for (line <- lines) {
        if (!skipJSMode && line.trim == "// SKIP-DOTTY-START")
          skipDottyMode = true
        else if (!skipJSMode && line.trim == "// SKIP-DOTTY-END")
          skipDottyMode = false
        else if (!skipDottyMode && (line.trim == "// SKIP-SCALATESTJS,NATIVE-START" || line.trim == "// SKIP-SCALATESTJS-START"))
          skipJSMode = true
        else if (!skipDottyMode && (line.trim == "// SKIP-SCALATESTJS,NATIVE-END" || line.trim == "// SKIP-SCALATESTJS-END"))
          skipJSMode = false  
        else if (!skipDottyMode && !skipJSMode) {
          destWriter.write(transformLineJS(line))
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

  def copyDirJS(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFileJS(sourceFile, destFile)

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
    copyFiles("jvm/core/src/main/java/org/scalatest", "org/scalatest", targetDir,
      List(
        "Finders.java",
        "TagAnnotation.java",
        "WrapWith.java",
        "DoNotDiscover.java",
        "Ignore.java"
      )) ++ 
    copyDir("jvm/core/src/main/java/org/scalatest/tags", "org/scalatest/tags", targetDir, List.empty)  
  }

  def genHtml(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyResourceDir("scalatest-doc/src/main/html", "html", targetDir, List.empty) ++
    copyResourceDir("jvm/core/src/main/resources/images", "images", targetDir, List.empty) ++
    copyResourceDir("jvm/core/src/main/resources/org/scalatest", "org/scalatest", targetDir, List.empty)
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
        "Inspectors.scala",                 // Re-implemented without path-dependent type
      ), 
      "org/scalatest/concurrent" -> List.empty, 
      "org/scalatest/diagrams" -> List(
        "Diagrams.scala", 
        "DiagramsMacro.scala", 
        "DiagrammedExprMacro.scala",        // Re-implemented
        "DiagrammedExpr.scala",             // Re-implemented
      ), 
      "org/scalatest/exceptions" -> List.empty, 
      "org/scalatest/enablers" -> List(
        "InspectorAsserting.scala"     // Re-implemented without path-dependent type
      ), 
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
      "org/scalatest/expectations" -> List.empty,  
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

  val genScalaPackagesJS: Map[String, List[String]] = 
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
        "Inspectors.scala",                 // Re-implemented without path-dependent type
        "Shell.scala",                      // Not supported on scala-js
        "run.scala",                        // Not supported on scala-js
        "SuiteRerunner.scala",              // Not supported on scala-js
        "JavaClassesWrappers.scala",        // Re-implemented in scala-js
        "DispatchReporter.scala"            // Not supported on scala-js
      ), 
      "org/scalatest/concurrent" -> List(
        "SleepHelper.scala"
      ), 
      "org/scalatest/diagrams" -> List(
        "Diagrams.scala", 
        "DiagramsMacro.scala"
      ), 
      "org/scalatest/exceptions" -> List.empty, 
      "org/scalatest/enablers" -> List(
        "InspectorAsserting.scala"     // Re-implemented without path-dependent type
      ), 
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
      "org/scalatest/expectations" -> List.empty,  
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

  /*def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    genScalaPackages.filter(_._1 != "org/scalatest/tools").flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/org/scalatest/tools" + packagePath, packagePath, targetDir, skipList)
    }.toList*/

  def genScalaJS(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("dotty/core/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++
    copyDir("dotty/core/src/main/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    copyDir("dotty/core/src/main/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, List.empty) ++ 
    copyDir("js/core/src/main/scala/org/scalatest/compatible", "org/scalatest/compatible", targetDir, List.empty) ++ 
    copyDir("js/core/src/main/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir, List.empty) ++ 
    copyDir("js/core/src/main/scala/org/scalatest/tools", "org/scalatest/tools", targetDir, List.empty) ++ 
    copyDir("js/core/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++ 
    copyDirJS("jvm/core/src/main/scala/org/scalatest/tools", "org/scalatest/tools", targetDir, 
      List(
        "AboutJDialog.scala",
        //"AnsiColor.scala",
        "AnsiReset.scala",
        "ColorBar.scala",
        "DashboardReporter.scala",
        "DiscoverySuite.scala",
        "Durations.scala",
        "EventHolder.scala",
        "EventToPresent.scala",
        "EventHolderDefaultListModel.scala", 
        "EventHolderListCellRenderer.scala", 
        "FileReporter.scala",
        "FilterReporter.scala",
        "Framework.scala",
        "FriendlyParamsTranslator.scala",
        "HtmlReporter.scala",
        "IconEmbellishedListCellRenderer.scala",
        "JUnitXmlReporter.scala",
        "Memento.scala",
        "MemoryReporter.scala",
        "NarrowJOptionPane.scala",
        "NestedSuiteParam.scala",
        //"ParsedArgs.scala",
        "PrintReporter.scala",
        "ProgressBarPanel.scala",
        //"ReporterConfigParam.scala",
        "ReporterConfiguration.scala",
        "ReporterFactory.scala",
        "RunDoneListener.scala",
        "Runner.scala",
        "RunnerGUI.scala",
        "RunnerGUIState.scala",
        "RunnerJFrame.scala",
        "SbtCommandParser.scala",
        "SbtDispatchReporter.scala",
        "ScalaTestAntTask.scala",
        "ScalaTestFramework.scala",
        "SocketReporter.scala",
        "StandardErrReporter.scala",
        "StandardOutReporter.scala",
        "StatusJPanel.scala",
        "SuiteDiscoveryHelper.scala",
        "SuiteParam.scala",
        "SuiteResult.scala",
        "SuiteResultHolder.scala",
        //"SuiteRunner.scala",
        "TestSpec.scala",
        "XmlReporter.scala",
        "XmlSocketReporter.scala"
      ))

  def genMatchersCoreScalaJS(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDirJS("dotty/matchers-core/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
    copyDirJS("dotty/matchers-core/src/main/scala/org/scalatest/matchers/dsl", "org/scalatest/matchers/dsl", targetDir, List.empty)

  def genShouldMatchersScalaJS(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDirJS("dotty/shouldmatchers/src/main/scala/org/scalatest/matchers/should", "org/scalatest/matchers/should", targetDir, List.empty)

  def genMustMatchersScalaJS(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDirJS("dotty/mustmatchers/src/main/scala/org/scalatest/matchers/must", "org/scalatest/matchers/must", targetDir, List.empty)

  def genDiagramsScalaJS(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDirJS("dotty/diagrams/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++
    copyDirJS("dotty/diagrams/src/main/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, List.empty)    

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir, 
      List(
        "SeveredStackTracesFailureSpec.scala", // skipped because tests failed 
        "SeveredStackTracesSpec.scala", // skipped because tests failed 
      )
    ) ++ 
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir,
      List(
        "NoArgSpec.scala",  // skipped because tests failed.
      )) ++ 
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, 
      List(
        "CommonGeneratorsSpec.scala", 
        "GeneratorSpec.scala"
      )) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/suiteprop", "org/scalatest/suiteprop", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/time", "org/scalatest/time", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/verbs", "org/scalatest/verbs", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/tools", "org/scalatest/tools", targetDir, List.empty) ++ 
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/tools/scalasbt", "org/scalatest/tools/scalasbt", targetDir, List.empty)
  }

  def genTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir, 
      List(
        "BigSuiteSuite.scala",
        "CatchReporterProp.scala",   // skipped because heavily depends on java reflection
        "DeprecatedCatchReporterProp.scala",   // skipped because heavily depends on java reflection
        "ClassTaggingProp.scala",    // skipped because annotation not supported
        "DeprecatedClassTaggingProp.scala",    // skipped because annotation not supported
        "ConfigMapWrapperSuiteSpec.scala",    // skipped because depends on java reflection
        "DispatchReporterSpec.scala",   // skipped because DispatchReporter uses thread.
        "DocSpecSpec.scala",   // skipped because DocSpecSpec is not supported yet
        "EncodedOrderingSpec.scala",  // skipped because use scala.reflect.NameTransformer.encode
        "EntrySpec.scala",    // skipped because Entry extends java.util.Map
        "FunSuiteSuite.scala",          // skipped because depends on java reflection
        "InheritedTagProp.scala",         // skipped because depends on java reflection
        "OldDocSpec.scala",             // Do we still need this?
        "PrivateMethodTesterSpec.scala",   // skipped because depends on java reflection
        "PropertyFunSuite.scala",   // skipped because depends on java reflection
        "SavesConfigMapSuite.scala",    // skipped because depends on java reflection
        "SeveredStackTracesFailureSpec.scala", // skipped because tests failed 
        "SeveredStackTracesSpec.scala", // skipped because tests failed 
        "ShellSuite.scala",             // skipped because execute is not supported for now, asmounting brackets it depends on Suite.execute, which in turns depends on StandardOutReporter, PrintReporter that depends on java classes.
        "ShouldBeAnSymbolSpec.scala",    // skipped because depends on java reflections
        "ShouldBeASymbolSpec.scala",       // skipped because depends on java reflections.
        "ShouldBeSymbolSpec.scala",       // skipped because depends on java reflections.
        "ShouldFileBePropertyMatcherSpec.scala",    // skipped because depends on java.io.File
        "ShouldLogicalMatcherExprSpec.scala",       // skipped because depends on mockito
        "ShouldNotTypeCheckSpec.scala", // skipped because tests failed 
        "ShouldSameInstanceAsSpec.scala",     // skipped because identical string in js env is always the same instance.
        "RefSpecSpec.scala",          // skipped because depends on java reflections.
        "SpecSpec.scala",          // skipped because depends on java reflections.
        "StatusProp.scala",        // skipped because uses VirtualMachineError
        "DeprecatedStatusProp.scala",        // skipped because uses VirtualMachineError
        "StreamlinedXmlEqualitySpec.scala",    // skipped because use scala.xml
        "StreamlinedXmlNormMethodsSpec.scala", // skipped because use scala.xml
        "StreamlinedXmlSpec.scala",            // skipped because use scala.xml
        "SuiteSuite.scala",          // skipped because it depends on java reflection
        "MatchersSerializableSpec.scala",   // skipped because testing java serialization
        "SeveredStackTracesSpec.scala", // skipped because stack trace isn't really helpful after linked in different js env like node.
        "SeveredStackTracesFailureSpec.scala" // skipped because stack trace isn't really helpful after linked in different js env like node.
      )
    ) ++ 
    copyDirJS("dotty/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++
    copyDirJS("dotty/scalatest-test/src/test/scala/org/scalatest/matchers/should", "org/scalatest/matchers/should", targetDir, List.empty) ++
    copyDirJS("dotty/scalatest-test/src/test/scala/org/scalatest/matchers/must", "org/scalatest/matchers/must", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, List.empty) ++ 
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir, 
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
      )
    ) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir,
      List(
        "TestLocationJUnit3Suite.scala", // Not supported by scala-js
        "TestLocationJUnitSuite.scala", // Not supported by scala-js
        "TestLocationTestNGSuite.scala", // Not supported by scala-js
        "TestLocationMethodJUnit3Suite.scala", // Not supported by scala-js
        "TestLocationMethodJUnitSuite.scala", // Not supported by scala-js
        "TestLocationMethodTestNGSuite.scala", // Not supported by scala-js
        "LocationMethodSuiteProp.scala", // Not supported by scala-js
      )
    ) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir,
      List(
        "NoArgSpec.scala",  // skipped because tests failed.
        "SuiteSpec.scala"    // skipped because depends on java reflections
      )) ++ 
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, 
      List(
        "CommonGeneratorsSpec.scala", 
        "GeneratorSpec.scala"
      )) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/suiteprop", "org/scalatest/suiteprop", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/time", "org/scalatest/time", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/verbs", "org/scalatest/verbs", targetDir, List.empty) ++
    copyDirJS("jvm/scalatest-test/src/test/scala/org/scalatest/tools", "org/scalatest/tools", targetDir,
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
    )
  }

  def genDiagramsTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/diagrams-test/src/test/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, List.empty)

  def genDiagramsTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/diagrams-test/src/test/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, List.empty)  

  def genFeatureSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/featurespec-test/src/test/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty)

  def genFeatureSpecTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/featurespec-test/src/test/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty)  

  def genFlatSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec", "org/scalatest/flatspec", targetDir, List.empty)

  def genFlatSpecTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec", "org/scalatest/flatspec", targetDir, List.empty)      

  def genFreeSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/freespec-test/src/test/scala/org/scalatest/freespec", "org/scalatest/freespec", targetDir, List.empty)

  def genFreeSpecTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/freespec-test/src/test/scala/org/scalatest/freespec", "org/scalatest/freespec", targetDir, List.empty)  

  def genFunSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/funspec-test/src/test/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, List.empty)

  def genFunSpecTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/funspec-test/src/test/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, List.empty)      

  def genFunSuiteTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/funsuite-test/src/test/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, List.empty)

  def genFunSuiteTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/funsuite-test/src/test/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, List.empty)  

  def genPropSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/propspec-test/src/test/scala/org/scalatest/propspec", "org/scalatest/propspec", targetDir, List.empty)

  def genPropSpecTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/propspec-test/src/test/scala/org/scalatest/propspec", "org/scalatest/propspec", targetDir, List.empty)  

  def genWordSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/wordspec-test/src/test/scala/org/scalatest/wordspec", "org/scalatest/wordspec", targetDir, List.empty)

  def genWordSpecTestJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDirJS("jvm/wordspec-test/src/test/scala/org/scalatest/wordspec", "org/scalatest/wordspec", targetDir, List.empty)      
}
