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
    copyFiles("scalatest/src/main/java/org/scalatest", "org/scalatest", targetDir,
      List(
        "Finders.java",
        "TagAnnotation.java",
        "WrapWith.java",
        "DoNotDiscover.java",
        "Ignore.java"
      ))
  }

  def genHtml(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyResourceDir("scalatest/src/main/html", "html", targetDir, List.empty)
  }

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("scalatest/src/main/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "Outcome.scala",
        "AppendedClues.scala",
        "PendingStatement.scala",
        "Suite.scala",
        "Reporter.scala", 
        "DispatchReporter.scala", 
        "ResourcefulReporter.scala",
        "CatchReporter.scala",
        "DistributedSuiteSorter.scala",
        "JavaClassesWrappers.scala",
        "SlowpokeDetector.scala",
        "Slowpoke.scala",
        "RunningTest.scala",
        "Informer.scala",
        "Tracker.scala",
        "Filter.scala",
        "DynaTags.scala",
        "Args.scala",
        "Stopper.scala",
        "Distributor.scala",
        "DistributedTestSorter.scala",
        "Status.scala",
        "EncodedOrdering.scala",
        "TestSuite.scala",
        "Informing.scala",
        "Notifying.scala",
        "Alerting.scala",
        "Documenting.scala",
        "Alerter.scala",
        "Documenter.scala",
        "Notifier.scala",
        "SuiteHelpers.scala",
        "TestData.scala",
        "DeferredAbortedSuite.scala",
        "Engine.scala",
        "ConcurrentInformer.scala",
        "Tag.scala",
        "UnquotedString.scala",
        "OneInstancePerTest.scala",
        "InsertionOrderSet.scala",
        "SuiteMixin.scala",
        "Transformer.scala",
        "OutcomeOf.scala",
        "TestRegistration.scala",
        "AsyncTestSuite.scala",
        "RecoverMethods.scala",
        "CompleteLastly.scala",
        "AsyncOutcome.scala",
        "FutureOutcome.scala",
        "ParallelTestExecution.scala",
        "RandomTestOrder.scala", 
        "AsyncTestRegistration.scala",
        "AsyncEngine.scala"
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/compatible", "org/scalatest/compatible", targetDir, List.empty) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,
      List(
        "SerialExecutionContext.scala"
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, List.empty) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir,
      List(
        "Containing.scala",
        "Aggregating.scala",
        "KeyMapping.scala",
        "ValueMapping.scala",
        "Futuristic.scala"
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/events", "org/scalatest/events", targetDir, List.empty) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir,
      List(
        "Transformer.scala",
        "NoArgTestWrapper.scala",
        "Suite.scala",
        "TestSuite.scala",
        "TestRegistration.scala",
        "AsyncTestSuite.scala",
        "AsyncTestRegistration.scala"
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/time", "org/scalatest/time", targetDir, List.empty) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/tools", "org/scalatest/tools", targetDir,
      List(
        "StringReporter.scala",
        "SuiteSortingReporter.scala",
        "StandardOutReporter.scala",
        "PrintReporter.scala",
        "SuiteDiscoveryHelper.scala",
        "Fragment.scala",
        "AnsiColor.scala",
        "TestSpec.scala",
        "SuiteParam.scala",
        "NestedSuiteParam.scala",
        "DiscoverySuite.scala",
        "TestSortingReporter.scala",
        "DistributedTestRunnerSuite.scala",
        "TestSpecificReporter.scala"
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/refspec", "org/scalatest/refspec", targetDir, List.empty) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/words", "org/scalatest/words", targetDir,
      List(
        "ArrayWrapper.scala",
        "BehaveWord.scala"
      )
    )
    /*

      copyDir("scalatest/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
      copyDir("scalatest/src/main/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, List.empty) ++
      copyDir("scalatest/src/main/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty) ++
      copyDir("scalatest/src/main/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, List.empty) ++
      copyDir("scalatest/src/main/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty) ++
      copyDir("scalatest/src/main/scala/org/scalatest/tagobjects", "org/scalatest/tagobjects", targetDir,
        List(
          "ChromeBrowser.scala",  // skipped because selenium not supported.
          "FirefoxBrowser.scala",  // skipped because selenium not supported.
          "HtmlUnitBrowser.scala",  // skipped because selenium not supported.
          "InternetExplorerBrowser.scala",  // skipped because selenium not supported.
          "SafariBrowser.scala"  // skipped because selenium not supported.
        )
      )*/
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    //copyStartsWithFiles("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", "Async", targetDir) ++
    //copyFiles("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", List("FutureOutcomeSpec.scala"), targetDir)
    copyDir("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir,
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
      )
  }

}
