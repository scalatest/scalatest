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

object GenScalaTestJS {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26)
    else if (line.trim.startsWith("//SCALATESTJS-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19)
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
        if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START" || line.trim == "// SKIP-SCALATESTJS-START")
          skipMode = true
        else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END" || line.trim == "// SKIP-SCALATESTJS-END")
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

  def copyFiles(sourceDirName: String, packageDirName: String, files: List[String], targetDir: File): Seq[File] = {
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
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && f.getName.endsWith(".scala")).map { sourceFile =>
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
    copyFiles("jvm/core/src/main/java/org/scalatest", "org/scalatest",
            List(
              "Finders.java",
              "TagAnnotation.java",
              "WrapWith.java",
              "DoNotDiscover.java",
              "Ignore.java"
            ), targetDir)
  }

  def genHtml(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyResourceDir("scalatest-doc/src/main/html", "html", targetDir, List.empty)
  }

  val genScalaPackages: Map[String, List[String]] = 
    Map(
      "org/scalatest" -> List(
        "DispatchReporter.scala",
        "Doc.scala",
        "DocSpec.scala",
        "DocSpecLike.scala",
        "ConfigMapWrapperSuite.scala",    // skipped because depends on java reflection.
        "JavaClassesWrappers.scala",
        "Shell.scala",
        "StreamlinedXml.scala",
        "StreamlinedXmlEquality.scala",
        "StreamlinedXmlNormMethods.scala",
        "SuiteRerunner.scala",
        "SuiteRerunner.scala",
        "run.scala",
        "SeveredStackTraces.scala"         // skipped because stack trace isn't really helpful after linked in different js env like node.
      ), 
      "org/scalatest/compatible" -> List.empty, 
      "org/scalatest/expectations" -> List.empty, 
      "org/scalatest/fixture" -> List(
        "Spec.scala",
        "SpecLike.scala"
      ), 
      "org/scalatest/diagrams" -> List.empty, 
      "org/scalatest/events" -> List.empty, 
      "org/scalatest/expectations" -> List.empty, 
      "org/scalatest/matchers" -> List.empty, 
      "org/scalatest/matchers/dsl" -> List(
        "JavaCollectionWrapper.scala",
        "JavaMapWrapper.scala"
      ), 
      "org/scalatest/matchers/should" -> List.empty, 
      "org/scalatest/tools" -> List(
        "AboutJDialog.scala",
        //"AnsiColor.scala",
        "AnsiReset.scala",
        "ColorBar.scala",
        "ConcurrentDistributor.scala",
        "DashboardReporter.scala",
        "DiscoverySuite.scala",
        "Durations.scala",
        "EventHolder.scala",
        "EventToPresent.scala",
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
        "PrettyPrinter.scala", 
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
        "XmlSocketReporter.scala", 
        "XmlUtility.scala"
      ), 
      "org/scalatest/exceptions" -> List(
        "StackDepthExceptionHelper.scala"
      ), 
      "org/scalatest/time" -> List.empty, 
      "org/scalatest/verbs" -> List.empty, 
      "org/scalatest/words" -> List.empty, 
      "org/scalatest/enablers" -> List.empty, 
      "org/scalatest/funsuite" -> List.empty, 
      "org/scalatest/featurespec" -> List.empty,
      "org/scalatest/funspec" -> List.empty,
      "org/scalatest/freespec" -> List.empty,
      "org/scalatest/flatspec" -> List.empty,
      "org/scalatest/prop" -> List.empty,
      "org/scalatest/propspec" -> List.empty,
      "org/scalatest/wordspec" -> List.empty, 
      "org/scalatest/concurrent" -> List(
        "Waiters.scala",        // skipeed because doesn't really make sense on js's single-thread environment.
        "Conductors.scala",             // skipped because depends on PimpedReadWriteLock
        "ConductorFixture.scala",       // skipped because depends on Conductors
        "ConductorMethods.scala",       // skipped because depends on Conductors
        "DoNotInterrupt.scala",         // skipped because no practical way to interrupt in js.
        "Eventually.scala",             // skipped because js is single thread and does not share memory.
        "Interruptor.scala",            // skipped because no practical way to interrupt in js.
        "JavaFutures.scala",            // skipped because depends on java futures.
        "PimpedReadWriteLock.scala",    // skipped because use java concurrent classes
        "PimpedThreadGroup.scala",      // skipped because doesn't really make sense under js's single-threaded environment.
        "SelectorInterruptor.scala",    // skipped because it is for java selector
        "SleepHelper.scala",            // skipped because scalatest.js has its own version
        "SocketInterruptor.scala",       // skipped because it is for java socket.
        "TestThreadsStartingCounter.scala",    // skipped because doesn't really make sense under js's single-threaded environment.
        "ThreadInterruptor.scala",          // skipped because no interrupt in js.
        "DeprecatedTimeLimitedTests.scala",       // skipped because js is single-threaded and does not share memory, there's no practical way to interrupt in js.
        "Timeouts.scala",               // skipped because js is single-threaded and does not share memory, there's no practical way to interrupt in js.
        "TimeoutTask.scala",            // skipped because timeout is not supported.,
        "Ultimately.scala"              // skipped because js is single thread and does not share memory.
      ), 
      "org/scalatest/path" -> List.empty, 
      "org/scalatest/tagobjects" -> List(
        "ChromeBrowser.scala",  // skipped because selenium not supported.
        "FirefoxBrowser.scala",  // skipped because selenium not supported.
        "HtmlUnitBrowser.scala",  // skipped because selenium not supported.
        "InternetExplorerBrowser.scala",  // skipped because selenium not supported.
        "SafariBrowser.scala"  // skipped because selenium not supported.
      )
    )

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    genScalaPackages.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    //copyStartsWithFiles("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", "Async", targetDir) ++
    //copyFiles("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", List("FutureOutcomeSpec.scala"), targetDir)
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir,
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
        "ShellSuite.scala",             // skipped because execute is not supported for now, as it depends on Suite.execute, which in turns depends on StandardOutReporter, PrintReporter that depends on java classes.
        "ShouldBeAnSymbolSpec.scala",    // skipped because depends on java reflections
        "ShouldBeASymbolSpec.scala",       // skipped because depends on java reflections.
        "ShouldBeSymbolSpec.scala",       // skipped because depends on java reflections.
        "ShouldFileBePropertyMatcherSpec.scala",    // skipped because depends on java.io.File
        "ShouldLogicalMatcherExprSpec.scala",       // skipped because depends on mockito
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
      )) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,
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
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir,
      List(
        "TestLocationJUnit3Suite.scala",
        "TestLocationJUnitSuite.scala",
        "TestLocationTestNGSuite.scala",
        "TestLocationMethodJUnit3Suite.scala",
        "TestLocationMethodJUnitSuite.scala",
        "TestLocationMethodTestNGSuite.scala",
        "LocationMethodSuiteProp.scala"
      )) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir,
      List(
        "SpecSpec.scala",     // skipped because depends on java reflections
        "SuiteSpec.scala"    // skipped because depends on java reflections
      )) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/suiteprop", "org/scalatest/suiteprop", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/tagobjects", "org/scalatest/tagobjects", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/time", "org/scalatest/time", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/tools", "org/scalatest/tools", targetDir,
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

  def genExpectationsTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/expectations-test/src/test/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, List.empty)  

  def genFeatureSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/featurespec-test/src/test/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty)

  def genFlatSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec", "org/scalatest/flatspec", targetDir, List.empty)

  def genFreeSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/freespec-test/src/test/scala/org/scalatest/freespec", "org/scalatest/freespec", targetDir, List.empty)

  def genFunSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/funspec-test/src/test/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, List.empty)

  def genFunSuiteTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/funsuite-test/src/test/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, List.empty)          

  def genPropSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/propspec-test/src/test/scala/org/scalatest/propspec", "org/scalatest/propspec", targetDir, List.empty)

  def genWordSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    copyDir("jvm/wordspec-test/src/test/scala/org/scalatest/wordspec", "org/scalatest/wordspec", targetDir, List.empty)  

}
