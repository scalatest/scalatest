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

import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenScalaTestJS {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//SCALATESTJS-ONLY "))
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
        if (line.trim == "// SKIP-SCALATESTJS-START")
          skipMode = true
        else if (line.trim == "// SKIP-SCALATESTJS-END")
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
      copyFile(sourceFile, destFile)
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

  def genJava(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("scalatest/src/main/java/org/scalatest", "org/scalatest",
            List(
              "Finders.java",
              "TagAnnotation.java",
              "WrapWith.java",
              "DoNotDiscover.java",
              "Ignore.java"
            ), targetDir)
  }

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = {

    copyFiles("scalatest/src/main/scala/org/scalatest", "org/scalatest",
            List(
              "Suite.scala",
              "OutcomeOf.scala",
              "Assertions.scala",
              "Outcome.scala",
              "TestData.scala",
              "ConfigMap.scala",
              "Reporter.scala",
              //"DispatchReporter.scala",
              "CatchReporter.scala",
              //"ConfigMapWrapperSuite.scala",    // skipped because depends on java reflection.
              "ResourcefulReporter.scala",
              "Tracker.scala",
              "Filter.scala",
              "DynaTags.scala",
              "Status.scala",
              "Args.scala",
              "Stopper.scala",
              "Distributor.scala",
              "DistributedTestSorter.scala",
              "DistributedSuiteSorter.scala",
              "Informer.scala",
              "EncodedOrdering.scala",
              "ConcurrentInformer.scala",
              "Documenter.scala",
              "SuiteHelpers.scala",
              "PendingNothing.scala",
              "AssertionsMacro.scala",
              "CompileMacro.scala",
              "AppendedClues.scala",
              "Notifier.scala",
              "Alerter.scala",
              "SlowpokeDetector.scala",
              "Slowpoke.scala",
              "RunningTest.scala",
              "ParallelTestExecution.scala",
              "OneInstancePerTest.scala",
              "SuiteMixin.scala",
              "Engine.scala",
              "Tag.scala",
              "FunSuiteLike.scala",
              "FunSuite.scala",
              "TestRegistration.scala",
              "Informing.scala",
              "Notifying.scala",
              "Alerting.scala",
              "Documenting.scala",
              "Transformer.scala",
              "DeferredAbortedSuite.scala",
              "Suites.scala",
              "FunSpecLike.scala",
              "FunSpec.scala",
              "UnquotedString.scala",
              "FlatSpecLike.scala",
              "FlatSpec.scala",
              "WordSpecLike.scala",
              "WordSpec.scala",
              "FreeSpecLike.scala",
              "FreeSpec.scala",
              "PropSpecLike.scala",
              "PropSpec.scala",
              "FeatureSpecLike.scala",
              "FeatureSpec.scala",
              "MatchersHelper.scala",
              "Matchers.scala",
              "Entry.scala",
              "Inspectors.scala",
              "OptionValues.scala",
              "Inside.scala",
              "NonImplicitAssertions.scala",
              "BeforeAndAfterAll.scala",
              "BeforeAndAfterEachTestData.scala",
              "BeforeAndAfterAllConfigMap.scala",
              "BeforeAndAfterEach.scala", 
              "GivenWhenThen.scala",
              "SeveredStackTraces.scala",
              "BeforeAndAfter.scala",
              "CancelAfterFailure.scala",
              "StopOnFailureReporter.scala",
              //"ConfigMapWrapperSuite.scala",     // skipped because depends on java reflection
              "Checkpoints.scala",
              "DiagrammedAssertions.scala",
              "DiagrammedExpr.scala",
              "DiagrammedAssertionsMacro.scala",
              "DiagrammedExprMacro.scala",
              "EitherValues.scala",
              "LoneElement.scala",
              "Retries.scala",
              "PartialFunctionValues.scala",
              "RandomTestOrder.scala",
              "SequentialNestedSuiteExecution.scala",
              "StepwiseNestedSuiteExecution.scala",
              "StopOnFailure.scala",
              "Sequential.scala",
              "Stepwise.scala",
              "TryValues.scala",
              "Payloads.scala"
            ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/fixture", "org/scalatest/fixture",
            List(
              "Suite.scala",
              "TestDataFixture.scala",
              "TestRegistration.scala",
              "Transformer.scala",
              "UnitFixture.scala",
              "NoArg.scala",
              "NoArgTestWrapper.scala",
              "FixtureNodeFamily.scala",
              "FunSuiteLike.scala",
              "FunSuite.scala",
              "FlatSpecLike.scala",
              "FlatSpec.scala",
              "FunSpecLike.scala",
              "FunSpec.scala",
              "WordSpecLike.scala",
              "WordSpec.scala",
              "FreeSpecLike.scala",
              "FreeSpec.scala",
              "PropSpecLike.scala",
              "PropSpec.scala",
              "FeatureSpecLike.scala",
              "FeatureSpec.scala",
              "ConfigMapFixture.scala"
            ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/events", "org/scalatest/events",
            List(
              "Event.scala",
              "Ordinal.scala",
              "Formatter.scala",
              "Location.scala",
              "Summary.scala",
              "NameInfo.scala"
            ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers",
            List(
              "MatchResult.scala",
              "AMatcher.scala",
              "AnMatcher.scala",
              "BeMatcher.scala",
              "BePropertyMatchResult.scala",
              "BePropertyMatcher.scala",
              "HavePropertyMatchResult.scala",
              "HavePropertyMatcher.scala",
              "LazyArg.scala",
              "LazyMessage.scala",
              "Matcher.scala",
              "TypeMatcherMacro.scala",
              "MatchPatternMacro.scala",
              "MatcherProducers.scala",
              "MatchFailed.scala",
              "MatchPatternHelper.scala",
              "MatchSucceeded.scala",
              "TypeMatcherHelper.scala"
            ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/tools", "org/scalatest/tools",
      List(
        "SuiteDiscoveryHelper.scala",
        "StringReporter.scala",
        "SuiteRunner.scala",
        "Fragment.scala",
        "ParsedArgs.scala",
        "ReporterConfiguration.scala",
        "AnsiColor.scala",
        "ReporterConfigParam.scala",
        "EventToPresent.scala",
        "DiscoverySuite.scala",
        "SuiteSortingReporter.scala",
        //"ConcurrentDistributor.scala",
        "FilterReporter.scala",
        "SuiteResult.scala",
        "SuiteParam.scala",
        "NestedSuiteParam.scala",
        "TestSpec.scala",
        "DistributedTestRunnerSuite.scala",
        "SuiteResultHolder.scala",
        "Durations.scala",
        "TestSortingReporter.scala",
        "RunDoneListener.scala",
        "SbtDispatchReporter.scala",
        "FriendlyParamsTranslator.scala"
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/exceptions", "org/scalatest/exceptions",
      List(
        "StackDepthException.scala",
        "NotAllowedException.scala",
        "StackDepth.scala",
        "TestPendingException.scala",
        "TestCanceledException.scala",
        "ModifiableMessage.scala",
        "PayloadField.scala",
        "ModifiablePayload.scala",
        "TestFailedException.scala",
        "PropertyCheckFailedException.scala",
        "TableDrivenPropertyCheckFailedException.scala",
        "DuplicateTestNameException.scala",
        "TestRegistrationClosedException.scala",
        "GeneratorDrivenPropertyCheckFailedException.scala",
        "DiscardedEvaluationException.scala",
        "TimeoutField.scala",
        "TestFailedDueToTimeoutException.scala"
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/time", "org/scalatest/time",
      List(
        "Now.scala",
        "Span.scala",
        "SpanSugar.scala",
        "Units.scala"
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/words", "org/scalatest/words",
      List(
        "TypeCheckWord.scala",
        "CompileWord.scala",
        "ArrayWrapper.scala",
        "BehaveWord.scala",
        "ResultOfTaggedAsInvocation.scala",
        "ResultOfStringPassedToVerb.scala",
        "ShouldVerb.scala",
        "MustVerb.scala",
        "CanVerb.scala",
        "StringVerbBlockRegistration.scala",
        "ResultOfAfterWordApplication.scala",
        "RegexWithGroups.scala",
        "DefinedWord.scala",
        "ResultOfOnlyApplication.scala",
        "ResultOfTheSameInstanceAsApplication.scala",
        "EmptyWord.scala",
        "ReadableWord.scala",
        "WritableWord.scala",
        "ResultOfNotExist.scala",
        "ExistWord.scala",
        "ResultOfATypeInvocation.scala",
        "ResultOfAnTypeInvocation.scala",
        "SortedWord.scala",
        "ResultOfAtMostOneOfApplication.scala",
        "ResultOfValueWordApplication.scala",
        "ResultOfKeyWordApplication.scala",
        "ResultOfInOrderApplication.scala",
        "ResultOfInOrderOnlyApplication.scala",
        "ResultOfAllOfApplication.scala",
        "ResultOfTheSameElementsInOrderAsApplication.scala",
        "ResultOfTheSameElementsAsApplication.scala",
        "ResultOfNoneOfApplication.scala",
        "ResultOfAtLeastOneOfApplication.scala",
        "ResultOfOneOfApplication.scala",
        "ResultOfDefinedAt.scala",
        "ResultOfRegexWordApplication.scala",
        "ResultOfAnWordToAnMatcherApplication.scala",
        "ResultOfAnWordToBePropertyMatcherApplication.scala",
        "ResultOfAWordToAMatcherApplication.scala",
        "ResultOfAnWordToSymbolApplication.scala",
        "ResultOfAWordToBePropertyMatcherApplication.scala",
        "ResultOfAWordToSymbolApplication.scala",
        "ResultOfGreaterThanOrEqualToComparison.scala",
        "ResultOfLessThanOrEqualToComparison.scala",
        "ResultOfGreaterThanComparison.scala",
        "ResultOfLessThanComparison.scala",
        "ResultOfMessageWordApplication.scala",
        "ResultOfSizeWordApplication.scala",
        "ResultOfLengthWordApplication.scala",
        "ContainWord.scala",
        "NotWord.scala",
        "BeWord.scala",
        "HaveWord.scala",
        "IncludeWord.scala",
        "EndWithWord.scala",
        "StartWithWord.scala",
        "FullyMatchWord.scala",
        "MatcherWords.scala",
        "PleaseUseNoExceptionShouldSyntaxInstead.scala",
        "ResultOfOfTypeInvocation.scala",
        "ResultOfThrownByApplication.scala",
        "ResultOfBeWordForAnType.scala",
        "ResultOfBeWordForAType.scala",
        "MatchPatternWord.scala",
        "NoExceptionWord.scala",
        "SizeWord.scala",
        "LengthWord.scala",
        "ResultOfBeWordForNoException.scala",
        "ResultOfContainWord.scala",
        "ResultOfNotWordForAny.scala",
        "ResultOfTheTypeInvocation.scala",
        "ResultOfAllElementsOfApplication.scala",
        "ResultOfOneElementOfApplication.scala",
        "ResultOfAtLeastOneElementOfApplication.scala",
        "ResultOfNoElementsOfApplication.scala"
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/enablers", "org/scalatest/enablers",
      List(
        "Containing.scala",
        "Aggregating.scala",
        "KeyMapping.scala",
        "ValueMapping.scala",
        "Sequencing.scala",
        "Sortable.scala",
        "Readability.scala",
        "Writability.scala",
        "Emptiness.scala",
        "Definition.scala",
        "Length.scala",
        "Existence.scala",
        "Size.scala",
        "Messaging.scala",
        "Collecting.scala"
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/prop", "org/scalatest/prop",
      List(
        //"Configuration.scala",
        "Checkers.scala",
        //"PropertyChecks.scala",
        "GenDrivenPropertyChecks.scala", 
        "Generator.scala", 
        "Configuration.scala", 
        "Randomizer.scala",
        "Edges.scala", 
        "Whenever.scala"
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/concurrent", "org/scalatest/concurrent",
      List(
        "ScalaFutures.scala",
        "Futures.scala",
        "PatienceConfiguration.scala",
        "AbstractPatienceConfiguration.scala",
        //"Eventually.scala",      // not supported because js is single thread and does not share memory.
        "ScaledTimeSpans.scala"
        //"PimpedReadWriteLock.scala",    // skipped because use java concurrent classes
        //"TestThreadsStartingCounter.scala",    // skipped because doesn't really make sense under js's single-threaded environment.
        //"PimpedThreadGroup.scala",    // skipped because doesn't really make sense under js's single-threaded environment.
        //"Conductors.scala",             // skipped because depends on PimpedReadWriteLock
        //"ConductorFixture.scala",       // skipped because depends on Conductors
        //"ConductorMethods.scala",       // skipped because depends on Conductors
        //"AsyncAssertions.scala",        // skipeed because doesn't really make sense on js's single-thread environment.
        //"TimeLimitedTests.scala",       // skipped because js is single-threaded and does not share memory, there's no practical way to interrupt in js.
        //"Timeouts.scala",               // skipped because js is single-threaded and does not share memory, there's no practical way to interrupt in js.
        //"TimeoutTask.scala",             // skipped because timeout is not supported.
        //"Interruptor.scala",             // skipped because no practical way to interrupt in js.
        //"SelectorInterruptor.scala",     // skipped because it is for java selector
        //"SocketInterruptor.scala",       // skipped because it is for java socket.
        //"ThreadInterrupt.scala"          // skipped because no interrupt in js.
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/path", "org/scalatest/path",
      List(
        "FreeSpec.scala",
        "FreeSpecLike.scala",
        "FunSpec.scala",
        "FunSpecLike.scala"
      ), targetDir) ++
    copyFiles("scalatest/src/main/scala/org/scalatest/tagobjects", "org/scalatest/tagobjects",
      List(
        "Retryable.scala",
        "CPU.scala",
        "Disk.scala",
        "Network.scala",
        "Slow.scala"
      ), targetDir)
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "BigSuiteSuite.scala",
        "CatchReporterProp.scala",   // skipped because heavily depends on java reflection
        "ClassTaggingProp.scala",    // skipped because annotation not supported
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
        "SpecSpec.scala",          // skipped because depends on java reflections.
        "StatusProp.scala",        // skipped because uses VirtualMachineError
        "StreamlinedXmlEqualitySpec.scala",    // skipped because use scala.xml
        "StreamlinedXmlNormMethodsSpec.scala", // skipped because use scala.xml
        "StreamlinedXmlSpec.scala",            // skipped because use scala.xml
        "SuiteSuite.scala"         // skipped because it depends on java reflection
      )) ++
    copyDir("scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,
      List(
        "AsyncAssertionsSpec.scala",    // skipped because AsyncAssertions not supported.
        "ConductorFixtureSuite.scala",  // skipped because Conductors not supported.
        "ConductorMethodsSuite.scala",   // skipped because Conductors not supported.
        "ConductorSuite.scala",   // skipped because Conductors not supported.
        "EventuallySpec.scala",   // skipped because Eventually not supported.
        "FuturesSpec.scala",      // temporary skipped, should refactor Futures.futureValue method and re-enable this.
        "IntegrationPatienceSpec.scala",  // skipped because depends on Eventually
        "JavaFuturesSpec.scala",      // skipped because depends on java futures
        "TestThreadsStartingCounterSpec.scala",   // skipped because depends on Conductors
        "TimeLimitedTestsSpec.scala",   // skipped because TimeLimitedTests not supported.
        "TimeoutsSpec.scala"            // skipped because Timeouts not supported.
      )) ++
    copyDir("scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    copyDir("scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, List.empty) ++
    copyDir("scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir,
      List(
        //"LocationFunctionSuiteProp.scala",   // temporary skip for now
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
    copyDir("scalatest-test/src/test/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
    copyFiles("scalatest-test/src/test/scala/org/scalatest/words", "org/scalatest/words",
      List(
        "ResultOfAllElementsOfApplicationSpec.scala",
        "ResultOfAtLeastOneElementOfApplicationSpec.scala",
        "ResultOfNoElementsOfApplicationSpec.scala",
        "ResultOfOneElementOfApplicationSpec.scala"
      ), targetDir)
  }

  def genResource(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val sourceResourceFile = new File("scalatest/src/main/resources/org/scalatest/ScalaTestBundle.properties")
    val destResourceDir = new File(targetDir, "resources/org/scalatest")
    destResourceDir.mkdirs()
    val destResourceFile = new File(destResourceDir, "ScalaTestBundle.properties")
    copyFile(sourceResourceFile, destResourceFile)
    List(destResourceFile)
  }
}
