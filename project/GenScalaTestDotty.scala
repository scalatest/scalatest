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
    copyResourceDir("scalatest/src/main/html", "html", targetDir, List.empty) ++
    copyResourceDir("scalatest/src/main/resources/images", "images", targetDir, List.empty) ++
    copyResourceDir("scalatest/src/main/resources/org/scalatest", "org/scalatest", targetDir, List.empty)
  }

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("scalatest/src/main/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "Alerter.scala",
        "Alerting.scala",
        "AppendedClues.scala",
        "Args.scala",
        "AsyncEngine.scala",
        "AsyncFeatureSpec.scala",
        "AsyncFlatSpec.scala",
        "AsyncFlatSpecLike.scala",
        "AsyncFreeSpec.scala",
        "AsyncFreeSpecLike.scala",
        "AsyncTestSuite.scala",
        "AsyncFunSpec.scala",
        "AsyncFunSuite.scala",
        "AsyncPropSpec.scala",
        "AsyncPropSpecLike.scala",
        "AsyncOutcome.scala",
        "AsyncTestRegistration.scala",
        "AsyncTestSuiteMixin.scala",
        "AsyncWordSpec.scala",
        "AsyncWordSpecLike.scala",
        "BeforeAndAfter.scala",
        "BeforeAndAfterAll.scala",
        "BeforeAndAfterAllConfigMap.scala",
        "BeforeAndAfterEach.scala",
        "BeforeAndAfterEachTestData.scala",
        "CancelAfterFailure.scala",
        "CatchReporter.scala",
        "Checkpoints.scala",
        "CompleteLastly.scala",
        "ConcurrentInformer.scala",
        "ConfigMapWrapperSuite.scala",
        "DispatchReporter.scala",
        "DistributedSuiteSorter.scala",
        "DynaTags.scala",
        "Distributor.scala",
        "DistributedTestSorter.scala",
        "Documenting.scala",
        "Documenter.scala",
        "DeferredAbortedSuite.scala",
        "EncodedOrdering.scala",
        "Engine.scala",
        "Entry.scala",
        "Fact.scala",
        "Filter.scala",
        "FutureOutcome.scala",
        "Informer.scala",
        "Informing.scala",
        "InsertionOrderSet.scala",
        "JavaClassesWrappers.scala",
        "MatchersHelper.scala",
        "Matchers.scala",
        "Notifying.scala",
        "Notifier.scala",
        "OneInstancePerTest.scala",
        "OutcomeOf.scala",
        "OptionValues.scala",
        "Outcome.scala",
        "PendingStatement.scala",
        "ParallelTestExecution.scala",
        "package.scala",
        "Reporter.scala",
        "ResourcefulReporter.scala",
        "RunningTest.scala",
        "RecoverMethods.scala",
        "RandomTestOrder.scala",
        "Rerunner.scala",
        "Shell.scala",
        "Suite.scala",
        "Stopper.scala",
        "Status.scala",
        "SlowpokeDetector.scala",
        "Slowpoke.scala",
        "SuiteHelpers.scala",
        "SuiteMixin.scala",
        "SuiteRerunner.scala",
        "Tracker.scala",
        "TestSuite.scala",
        "TestData.scala",
        "Tag.scala",
        "Transformer.scala",
        "TestRegistration.scala",
        "TestRerunner.scala",
        "TestSuiteMixin.scala",
        "UnquotedString.scala"
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/compatible", "org/scalatest/compatible", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir,
      List(
        "PropCheckerAsserting.scala"
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/events", "org/scalatest/events", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir,
      List(
        "Matcher.scala",           // Re-implemented with new macro
        "MatchPatternMacro.scala", // Re-implemented with new macro
        "TypeMatcherMacro.scala"   // Re-implemented with new macro
      )
    ) ++
    copyDir("scalatest/src/main/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/time", "org/scalatest/time", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/tools", "org/scalatest/tools", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/refspec", "org/scalatest/refspec", targetDir, List.empty) ++
    copyDir("scalatest/src/main/scala/org/scalatest/words", "org/scalatest/words", targetDir,
      List(
        "BeWord.scala",
        "MatchPatternWord.scala",
        "NotWord.scala",
        "ResultOfNotWordForAny.scala"
      )
    )
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "AssertionsSpec.scala"
      )
    ) /*++
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
