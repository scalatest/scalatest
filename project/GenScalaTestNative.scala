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

object GenScalaTestNative {

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

  def copyStartsWithFiles(sourceDirName: String, packageDirName: String, startsWith: String, targetDir: File): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && f.getName.startsWith(startsWith) && f.getName.endsWith(".scala")).map { sourceFile =>
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

  def copyResourceDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName)).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
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
      "org/scalatest" -> (List(
        "DispatchReporter.scala",
        "ConfigMapWrapperSuite.scala",    // skipped because depends on java reflection.
        "JavaClassesWrappers.scala",
        "Shell.scala",
        "SuiteRerunner.scala",
        "SuiteRerunner.scala",
        "run.scala"
      )),
      "org/scalatest/diagrams" -> List.empty, 
      "org/scalatest/fixture" -> (List(
        "Spec.scala",
        "SpecLike.scala"
      )),
      "org/scalatest/events" -> List.empty, 
      "org/scalatest/expectations" -> List.empty, 
      "org/scalatest/matchers" -> List.empty, 
      "org/scalatest/matchers/should" -> List.empty, 
      "org/scalatest/matchers/dsl" -> List(
        "JavaCollectionWrapper.scala",
        "JavaMapWrapper.scala"
      ), 
      "org/scalatest/tools" -> List(
        "AboutJDialog.scala",
        //"AnsiColor.scala",
        "AnsiReset.scala",
        "ColorBar.scala",
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
      ), 
      "org/scalatest/exceptions" -> List(
        "StackDepthExceptionHelper.scala"
      ), 
      "org/scalatest/expectations" -> List.empty, 
      "org/scalatest/time" -> List.empty, 
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
      "org/scalatest/concurrent" -> List.empty,
      "org/scalatest/path" -> List.empty, 
      "org/scalatest/tagobjects" -> List(
        "ChromeBrowser.scala",  // skipped because selenium not supported.
        "FirefoxBrowser.scala",  // skipped because selenium not supported.
        "HtmlUnitBrowser.scala",  // skipped because selenium not supported.
        "InternetExplorerBrowser.scala",  // skipped because selenium not supported.
        "SafariBrowser.scala"  // skipped because selenium not supported.
      ), 
      "org/scalatest/verbs" -> List.empty, 

    )

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyStartsWithFiles("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", "Async", targetDir) ++
    copyFiles("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", List("FutureOutcomeSpec.scala"), targetDir) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "AnMatcherSpec.scala",
        "AnyValMatchersSpec.scala",
        "AppendedCluesSpec.scala",
        "ArgsSpec.scala",
        "AssertionsSpec.scala",
        "BigSuite.scala",
        "BigSuiteSuite.scala", // don't know why but skipped in Scala.js
        "CancelAfterFailureSpec.scala",
        "CatchReporterProp.scala",   // skipped because heavily depends on java reflection
        "CatchReporterSpec.scala",
        "CheckpointsSpec.scala",
        "ClassTaggingProp.scala",    // skipped because annotation not supported
        "ClueSpec.scala",
        "CompleteLastlySpec.scala",
        "ConfigMapSpec.scala",
        "ConfigMapWrapperSuiteSpec.scala",    // skipped because depends on java reflection
        "ContainMatcherAndOrDeciderSpec.scala",
        "ContainMatcherAndOrEqualitySpec.scala",
        "ContainMatcherAndOrExplicitEqualitySpec.scala",
        "ContainMatcherAndOrSpec.scala",
        "ConversionCheckedAssertionsSpec.scala",
        "CustomMatcherSpec.scala",
        "DeprecatedAsyncFeatureSpecLikeSpec.scala",
        "DeprecatedAsyncFeatureSpecLikeSpec2.scala",
        "DeprecatedAsyncFeatureSpecSpec.scala",
        "DeprecatedAsyncFeatureSpecSpec2.scala",
        "DeprecatedBeforeAndAfterAllProp.scala",
        "DeprecatedCatchReporterProp.scala",   // skipped because heavily depends on java reflection
        "DeprecatedClassTaggingProp.scala",    // skipped because annotation not supported
        "DeprecatedFeatureSpecSpec.scala",
        "DeprecatedParallelTestExecutionInfoExamples.scala",
        "DeprecatedParallelTestExecutionOrderExamples.scala",
        "DeprecatedParallelTestExecutionSuiteTimeoutExamples.scala",
        "DeprecatedParallelTestExecutionTestTimeoutExamples.scala",
        "DeprecatedRandomTestOrderSpec.scala",
        "DeprecatedStatusProp.scala",        // skipped because uses VirtualMachineError
        "DeprecatedStopOnFailureProp.scala",
        "DeprecatedTestDataProp.scala",
        "DeprecatedTestNameProp.scala",
        "DiagrammedAssertionsSpec.scala",
        "DirectAssertionsSpec.scala",
        "DirectDiagrammedAssertionsSpec.scala",
        "DirectExpectationsSpec.scala",
        "DispatchReporterSpec.scala",   // skipped because DispatchReporter uses thread.
        "DocSpecSpec.scala",   // skipped because DocSpecSpec is not supported yet
        "EasySuite.scala",
        "EitherValuesSpec.scala",
        "EncodedOrderingSpec.scala",  // skipped because use scala.reflect.NameTransformer.encode
        "EntrySpec.scala",    // skipped because Entry extends java.util.Map
        "ExampleBeforeAfterParallelSpec.scala",
        "EveryShouldContainAllElementsOfSpec.scala", // skipped because causing crash
        "EveryShouldContainAtLeastOneElementOfSpec.scala", // skipped because causing crash
        "EveryShouldContainNoneOfSpec.scala", // skipped because causing crash
        "ExampleParallelSpec.scala",
        "ExampleStackSpec.scala",
        "ExampleSuiteTimeoutSpec.scala",
        "ExampleTimeoutParallelSpec.scala",
        "ExamplesSuite.scala",
        "ExpectationHavePropertyMatchers.scala",
        "ExpectationsSpec.scala",
        "FactSpec.scala",
        "FailureMessagesSuite.scala",
        "FeatureSpecSpec.scala",
        "FilterProp.scala",
        "FilterSpec.scala",
        "FixtureContextSpec.scala",
        "FlatSpecImportedMatchersSpec.scala",
        "FlatSpecMixedInMatchersSpec.scala",
        "FlatSpecSpec.scala",
        "FreeSpecSpec.scala",
        "FunSpecSpec.scala",
        "FunSpecSuite.scala",
        "FunSuiteSpec.scala",
        "FunSuiteSuite.scala",          // skipped because depends on java reflection
        "FutureOutcomeSpec.scala",
        "GivenWhenThenSpec.scala",
        "InOrderContainMatcherDeciderSpec.scala",
        "InOrderContainMatcherEqualitySpec.scala",
        "InOrderContainMatcherSpec.scala",
        "InOrderElementsOfContainMatcherDeciderSpec.scala",
        "InOrderElementsOfContainMatcherEqualitySpec.scala",
        "InOrderElementsOfContainMatcherSpec.scala",
        "InOrderOnlyContainMatcherDeciderSpec.scala",
        "InOrderOnlyContainMatcherEqualitySpec.scala",
        "InOrderOnlyContainMatcherSpec.scala",
        "InformerSpec.scala",
        "InheritedTagProp.scala",         // skipped because depends on java reflection
        "InsertionOrderSetSpec.scala",
        "InsideMixinSpec.scala",
        "InsideSpec.scala",
        "InspectorShorthandsRegexWithGroupsSpec.scala",
        "InspectorShorthandsSpec.scala",
        "InspectorsForMapSpec.scala",
        "InspectorsSpec.scala",
        "JavaMapLoneElementSpec.scala",
        "ListLoneElementSpec.scala",
        "ListShouldBeEmptyLogicalAndSpec.scala",
        "ListShouldBeEmptyLogicalOrSpec.scala",
        "ListShouldBeEmptySpec.scala",
        "ListShouldContainOnlyLogicalAndSpec.scala",
        "ListShouldContainOnlyLogicalOrSpec.scala",
        "ListShouldContainOnlySpec.scala",
        "ListShouldContainSpec.scala",
        "ListShouldContainTheSameElementsAsLogicalAndSpec.scala",
        "ListShouldContainTheSameElementsAsLogicalOrSpec.scala",
        "ListShouldContainTheSameElementsAsSpec.scala",
        "ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala",
        "ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala",
        "ListShouldContainTheSameElementsInOrderAsSpec.scala",
        "MapShouldBeDefinedAtSpec.scala",
        "MatcherGenSpec.scala",
        "MatcherStackDepthSpec.scala",
        "MatchersSerializableSpec.scala",   // skipped because testing java serialization
        "MatchersSpec.scala",
        "NoElementsOfContainMatcherDeciderSpec.scala",
        "NoElementsOfContainMatcherEqualitySpec.scala",
        "NoElementsOfContainMatcherSpec.scala",
        "NonImplicitAssertionsSuite.scala",
        "NoneOfContainMatcherDeciderSpec.scala",
        "NoneOfContainMatcherEqualitySpec.scala",
        "NoneOfContainMatcherSpec.scala",
        "NotifierSpec.scala",
        "OldDocSpec.scala",             // Do we still need this?
        "OneElementOfContainMatcherDeciderSpec.scala",
        "OneElementOfContainMatcherEqualitySpec.scala",
        "OneElementOfContainMatcherSpec.scala",
        "OneInstancePerTestSpec.scala",
        "OneOfContainMatcherDeciderSpec.scala",
        "OneOfContainMatcherEqualitySpec.scala",
        "OneOfContainMatcherSpec.scala",
        "OnlyContainMatcherDeciderSpec.scala",
        "OnlyContainMatcherEqualitySpec.scala",
        "OnlyContainMatcherSpec.scala",
        "OptionShouldContainOneElementOfLogicalAndSpec.scala",
        "OptionShouldContainOneElementOfLogicalOrSpec.scala",
        "OptionShouldContainOneElementOfSpec.scala",
        "OptionShouldContainOneOfLogicalAndSpec.scala",
        "OptionShouldContainOneOfLogicalOrSpec.scala",
        "OptionShouldContainOneOfSpec.scala",
        "OptionShouldContainSpec.scala",
        "OptionValuesSpec.scala",
        "OutcomeSpec.scala",
        "ParallelTestExecutionInfoExamples.scala",
        "ParallelTestExecutionOrderExamples.scala",
        "ParallelTestExecutionParallelSuiteExamples.scala",
        "ParallelTestExecutionProp.scala",
        "ParallelTestExecutionSpec.scala",
        "ParallelTestExecutionSuiteTimeoutExamples.scala",
        "ParallelTestExecutionTestTimeoutExamples.scala",
        "PartialFunctionValuesSpec.scala",
        "PrettyAstSpec.scala",
        "PrivateMethodTesterSpec.scala",   // skipped because depends on java reflection
        "PropSpecSpec.scala",
        "PropertyFunSuite.scala",   // skipped because depends on java reflection
        "RandomAsyncTestExecutionSpec.scala",
        "RandomTestOrderSpec.scala",
        "RecoverMethodsSpec.scala",
        "RefSpecSpec.scala",          // skipped because depends on java reflections.
        "ResultOfAtLeastOneOfApplication.scala", // skipped because causing crash
        "RetriesSpec.scala",
        "RunningTestSpec.scala",
        "SavesConfigMapSuite.scala",    // skipped because depends on java reflection
        "SequentialNestedSuiteExecutionSpec.scala",
        "SeveredStackTracesFailureSpec.scala",
        "SeveredStackTracesSpec.scala",
        "ShellSuite.scala",             // skipped because execute is not supported for now, as it depends on Suite.execute, which in turns depends on StandardOutReporter, PrintReporter that depends on java classes.
        "ShorthandShouldBeThrownBySpec.scala",
        "ShorthandShouldNotBeThrownBySpec.scala",
        "ShouldBeAMatcherAndOrSpec.scala",
        "ShouldBeASymbolSpec.scala",       // skipped because depends on java reflections.
        "ShouldBeATypeSpec.scala",
        "ShouldBeAnMatcherAndOrSpec.scala",
        "ShouldBeAnSymbolSpec.scala",    // skipped because depends on java reflections
        "ShouldBeAnTypeSpec.scala",
        "ShouldBeAnySpec.scala",
        "ShouldBeDefinedAtForAllSpec.scala",
        "ShouldBeDefinedAtSpec.scala",
        "ShouldBeDefinedExplicitSpec.scala",
        "ShouldBeDefinedImplicitSpec.scala",
        "ShouldBeDefinedLogicalAndExplicitSpec.scala",
        "ShouldBeDefinedLogicalAndImplicitSpec.scala",
        "ShouldBeDefinedLogicalAndSpec.scala",
        "ShouldBeDefinedLogicalOrExplicitSpec.scala",
        "ShouldBeDefinedLogicalOrImplicitSpec.scala",
        "ShouldBeDefinedLogicalOrSpec.scala",
        "ShouldBeDefinedSpec.scala",
        "ShouldBeDefinedStructuralLogicalAndSpec.scala",
        "ShouldBeDefinedStructuralLogicalOrSpec.scala",
        "ShouldBeDefinedStructuralSpec.scala",
        "ShouldBeEmptyExplicitSpec.scala",
        "ShouldBeEmptyImplicitSpec.scala",
        "ShouldBeEmptyLogicalAndExplicitSpec.scala",
        "ShouldBeEmptyLogicalAndImplicitSpec.scala",
        "ShouldBeEmptyLogicalOrExplicitSpec.scala",
        "ShouldBeEmptyLogicalOrImplicitSpec.scala",
        "ShouldBeEmptyStructuralLogicalAndSpec.scala",
        "ShouldBeEmptyStructuralLogicalOrSpec.scala",
        "ShouldBeEmptyStructuralSpec.scala",
        "ShouldBeMatcherSpec.scala",
        "ShouldBeNullSpec.scala",
        "ShouldBePropertyMatcherSpec.scala",
        "ShouldBeReadableExplicitSpec.scala",
        "ShouldBeReadableImplicitSpec.scala",
        "ShouldBeReadableLogicalAndExplicitSpec.scala",
        "ShouldBeReadableLogicalAndImplicitSpec.scala",
        "ShouldBeReadableLogicalAndSpec.scala",
        "ShouldBeReadableLogicalOrExplicitSpec.scala",
        "ShouldBeReadableLogicalOrImplicitSpec.scala",
        "ShouldBeReadableLogicalOrSpec.scala",
        "ShouldBeReadableSpec.scala",
        "ShouldBeReadableStructuralLogicalAndSpec.scala",
        "ShouldBeReadableStructuralLogicalOrSpec.scala",
        "ShouldBeReadableStructuralSpec.scala",
        "ShouldBeShorthandForAllSpec.scala",
        "ShouldBeShorthandSpec.scala",
        "ShouldBeSortedLogicalAndSpec.scala",
        "ShouldBeSortedLogicalOrSpec.scala",
        "ShouldBeSortedSpec.scala",
        "ShouldBeSymbolSpec.scala",       // skipped because depends on java reflections.
        "ShouldBeThrownBySpec.scala",
        "ShouldBeWritableExplicitSpec.scala",
        "ShouldBeWritableImplicitSpec.scala",
        "ShouldBeWritableLogicalAndExplicitSpec.scala",
        "ShouldBeWritableLogicalAndImplicitSpec.scala",
        "ShouldBeWritableLogicalAndSpec.scala",
        "ShouldBeWritableLogicalOrExplicitSpec.scala",
        "ShouldBeWritableLogicalOrImplicitSpec.scala",
        "ShouldBeWritableLogicalOrSpec.scala",
        "ShouldBeWritableSpec.scala",
        "ShouldBeWritableStructuralLogicalAndSpec.scala",
        "ShouldBeWritableStructuralLogicalOrSpec.scala",
        "ShouldBeWritableStructuralSpec.scala",
        "ShouldBehaveLikeSpec.scala",
        "ShouldCollectedTripleEqualsSpec.scala",
        "ShouldCollectedTripleEqualsToleranceSpec.scala",
        "ShouldCompileSpec.scala",
        "ShouldContainElementNewSpec.scala",
        "ShouldContainElementSpec.scala",
        "ShouldContainKeySpec.scala",
        "ShouldContainValueSpec.scala",
        "ShouldConversionCheckedTripleEqualsEqualitySpec.scala",
        "ShouldEndWithRegexSpec.scala",
        "ShouldEndWithSubstringSpec.scala",
        "ShouldEqualEqualitySpec.scala",
        "ShouldEqualExplicitlySpec.scala",
        "ShouldEqualNullSpec.scala",
        "ShouldEqualSpec.scala",
        "ShouldEqualTokenToleranceSpec.scala",
        "ShouldEqualToleranceSpec.scala",
        "ShouldExistExplicitSpec.scala",
        "ShouldExistImplicitSpec.scala",
        "ShouldExistLogicalAndExplicitSpec.scala",
        "ShouldExistLogicalAndImplicitSpec.scala",
        "ShouldExistLogicalAndSpec.scala",
        "ShouldExistLogicalOrExplicitSpec.scala",
        "ShouldExistLogicalOrImplicitSpec.scala",
        "ShouldExistLogicalOrSpec.scala",
        "ShouldExistSpec.scala",
        "ShouldFileBePropertyMatcherSpec.scala",    // skipped because depends on java.io.File
        "ShouldFullyMatchSpec.scala",
        "ShouldHavePropertiesSpec.scala",
        "ShouldIncludeRegexSpec.scala",
        "ShouldIncludeSubstringSpec.scala",
        "ShouldLengthSizeSpec.scala",
        "ShouldLengthSpec.scala",
        "ShouldLogicalMatcherExprSpec.scala",       // skipped because depends on mockito
        "ShouldMatchPatternSpec.scala",
        "ShouldMatcherSpec.scala",
        "ShouldMessageSpec.scala",
        "ShouldNotBeThrownBySpec.scala",
        "ShouldNotCompileSpec.scala",
        "ShouldNotShorthandForAllSpec.scala",
        "ShouldNotShorthandSpec.scala",
        "ShouldNotTypeCheckSpec.scala",
        "ShouldOrderedSpec.scala",
        "ShouldPlusOrMinusSpec.scala",
        "ShouldSameInstanceAsSpec.scala",     // skipped because identical string in js env is always the same instance.
        "ShouldSizeSpec.scala",
        "ShouldStartWithRegexSpec.scala",
        "ShouldStartWithSubstringSpec.scala",
        "ShouldStructuralLengthSpec.scala",
        "ShouldStructuralSizeSpec.scala",
        "ShouldThrowSpec.scala",
        "ShouldTripleEqualsEqualitySpec.scala",
        "ShouldTripleEqualsSpec.scala",
        "ShouldTripleEqualsToleranceSpec.scala",
        "ShouldTypeCheckedTripleEqualsEqualitySpec.scala",
        "SlowpokeDetectorSpec.scala",
        "SpecSpec.scala",          // skipped because depends on java reflections.
        "StatefulStatusSpec.scala",
        "StatusProp.scala",        // skipped because uses VirtualMachineError
        "StatusSpec.scala",
        "StepwiseNestedSuiteExecutionSpec.scala",
        "StopOnFailureProp.scala",
        "StopOnFailureSpec.scala",
        "StreamlinedXmlEqualitySpec.scala",    // skipped because use scala.xml
        "StreamlinedXmlNormMethodsSpec.scala", // skipped because use scala.xml
        "StreamlinedXmlSpec.scala",            // skipped because use scala.xml
        "StringLoneElementSpec.scala",
        "SuiteCompletedStatusReporter.scala",
        "SuiteSpec.scala",
        "SuiteSuite.scala",          // skipped because it depends on java reflection
        "SuitesSpec.scala",
        "TagGroupsSpec.scala",
        "TaggingScopesSpec.scala",
        "TestColonEscapeProp.scala",
        "TestDataProp.scala",
        "TestNameProp.scala",
        "TheSameElementsAsContainMatcherDeciderSpec.scala",
        "TheSameElementsAsContainMatcherEqualitySpec.scala",
        "TheSameElementsAsContainMatcherSpec.scala",
        "TheSameElementsInOrderAsContainMatcherDeciderSpec.scala",
        "TheSameElementsInOrderAsContainMatcherEqualitySpec.scala",
        "TheSameElementsInOrderAsContainMatcherSpec.scala",
        "ThreadNameSpec.scala",
        "TryValuesSpec.scala",
        "TypeCheckedAssertionsSpec.scala",
        "VariousWordSpec.scala",
        "WordSpecImportedMatchersSpec.scala",
        "WordSpecMixedInMatchersSpec.scala",
        "WordSpecSpec.scala"
      )) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,
      List(
        // requires SelectableChannel
        "TimeLimitsSpec.scala", 
        "AbstractPatienceConfigurationSpec.scala" 
      )) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir,
      List(
        "DeprecatedLocationFunctionSuiteProp.scala",
        "DeprecatedScopePendingProp.scala", 
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
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/time", "org/scalatest/time", targetDir, List.empty) ++
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/tagobjects", "org/scalatest/tagobjects", targetDir, List.empty) ++
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
        "XmlSocketReporterSpec.scala",
        "ArgsParserSpec.scala"   // skipped because it is causing crash
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
