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
    copyResourceDir("jvm/core/src/main/html", "html", targetDir, List.empty) ++
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

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    genScalaPackages.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir, 
      List(
        "AllSuiteProp.scala", // skipped because does not compile yet
        "AMatcherSpec.scala", // skipped because does not compile yet 
        "AnMatcherSpec.scala",  // skipped because does not compile yet
        "AnyValMatchersSpec.scala",  // skipped because does not compile yet
        "AppendedCluesSpec.scala", // skipped because does not compile yet 
        "ArgsSpec.scala",  // skipped because does not compile yet
        "BeforeAndAfterAllConfigMapSpec.scala", // skipped because does not compile yet 
        "BeforeAndAfterAllProp.scala", // skipped because does not compile yet 
        "BeforeAndAfterAllSpec.scala", // skipped because does not compile yet 
        "CatchReporterProp.scala", // skipped because does not compile yet  
        "CheckpointsSpec.scala", // skipped because does not compile yet 
        "ClassTaggingProp.scala", // skipped because does not compile yet  
        "ClueSpec.scala", // skipped because does not compile yet  
        "ConfigMapWrapperSuiteSpec.scala", // skipped because does not compile yet  
        "DeprecatedBeforeAndAfterAllProp.scala", // skipped because does not compile yet 
        "DeprecatedCatchReporterProp.scala", // skipped because does not compile yet 
        "DeprecatedClassTaggingProp.scala", // skipped because does not compile yet 
        "DeprecatedFeatureSpecSpec.scala", // skipped because does not compile yet 
        "DeprecatedParallelTestExecutionInfoExamples.scala", // skipped because does not compile yet 
        "DeprecatedParallelTestExecutionOrderExamples.scala", // skipped because does not compile yet 
        "DeprecatedParallelTestExecutionSuiteTimeoutExamples.scala", // skipped because does not compile yet 
        "DeprecatedParallelTestExecutionTestTimeoutExamples.scala", // skipped because does not compile yet 
        "DeprecatedStatusProp.scala", // skipped because does not compile yet 
        "DeprecatedStopOnFailureProp.scala", // skipped because does not compile yet 
        "DeprecatedTestDataProp.scala", // skipped because does not compile yet 
        "DeprecatedTestNameProp.scala", // skipped because does not compile yet 
        "DirectAssertionsSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAllElementsOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAllOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtLeastOneElementOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtLeastOneOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtMostOneElementOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtMostOneOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainInOrderElementsOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainInOrderOnlySpec.scala", // skipped because does not compile yet 
        "EveryShouldContainInOrderSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainNoElementsOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainNoneOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainOneElementOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainOneOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainOnlyLogicalAndSpec.scala", // skipped because tests failed
        "EveryShouldContainOnlyLogicalOrSpec.scala", // skipped because tests failed 
        "EveryShouldContainOnlySpec.scala", // skipped because does not compile yet 
        "EveryShouldContainSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainTheSameElementsAsSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainTheSameElementsInOrderAsSpec.scala", // skipped because does not compile yet 
        "FactSpec.scala", // skipped because does not compile yet 
        "FilterProp.scala", // skipped because does not compile yet 
        "FilterSpec.scala", // skipped because does not compile yet 
        "InheritedTagProp.scala", // skipped because does not compile yet 
        "InspectorsForMapSpec.scala", // skipped because tests failed 
        "InspectorShorthandsSpec.scala", // skipped because does not compile yet 
        "InspectorsSpec.scala", // skipped because tests failed 
        "ListShouldContainAllElementsOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainAllOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainAtLeastOneElementOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainAtLeastOneOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainAtMostOneElementOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainAtMostOneOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainInOrderElementsOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainInOrderOnlySpec.scala", // skipped because does not compile yet 
        "ListShouldContainInOrderSpec.scala", // skipped because does not compile yet 
        "ListShouldContainNoElementsOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainNoneOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainOneElementOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainOneOfSpec.scala", // skipped because does not compile yet 
        "ListShouldContainOnlyLogicalAndSpec.scala", // skipped because does not compile yet 
        "ListShouldContainOnlyLogicalOrSpec.scala", // skipped because does not compile yet 
        "ListShouldContainOnlySpec.scala", // skipped because does not compile yet 
        "ListShouldContainSpec.scala", // skipped because does not compile yet 
        "ListShouldContainTheSameElementsAsSpec.scala", // skipped because does not compile yet 
        "ListShouldContainTheSameElementsInOrderAsSpec.scala", // skipped because does not compile yet 
        "MatchersSerializableSpec.scala", // skipped because does not compile yet 
        "MatchersSpec.scala", // skipped because does not compile yet 
        "MatcherStackDepthSpec.scala", // skipped because does not compile yet 
        "MethodSuiteExamples.scala", // skipped because does not compile yet 
        "MethodSuiteProp.scala", // skipped because does not compile yet 
        "NoElementsOfContainMatcherEqualitySpec.scala", // skipped because does not compile yet 
        "NoneOfContainMatcherEqualitySpec.scala", // skipped because does not compile yet 
        "OptionShouldContainOneElementOfLogicalAndSpec.scala", // skipped because does not compile yet 
        "OptionShouldContainOneElementOfLogicalOrSpec.scala", // skipped because does not compile yet 
        "OptionShouldContainOneElementOfSpec.scala", // skipped because does not compile yet 
        "OptionShouldContainOneOfLogicalAndSpec.scala", // skipped because does not compile yet 
        "OptionShouldContainOneOfLogicalOrSpec.scala", // skipped because does not compile yet 
        "OptionShouldContainOneOfSpec.scala", // skipped because does not compile yet 
        "OptionShouldContainSpec.scala", // skipped because does not compile yet 
        "OutcomeSpec.scala", // skipped because does not compile yet 
        "ParallelTestExecutionInfoExamples.scala", // skipped because does not compile yet 
        "ParallelTestExecutionOrderExamples.scala", // skipped because does not compile yet 
        "ParallelTestExecutionParallelSuiteExamples.scala", // skipped because does not compile yet 
        "ParallelTestExecutionProp.scala", // skipped because does not compile yet 
        "ParallelTestExecutionSpec.scala", // skipped because does not compile yet 
        "ParallelTestExecutionSuiteTimeoutExamples.scala", // skipped because does not compile yet 
        "ParallelTestExecutionTestTimeoutExamples.scala", // skipped because does not compile yet 
        "PrivateMethodTesterSpec.scala", // skipped because does not compile yet 
        "PropertyFunSuite.scala", // skipped because does not compile yet 
        "PropSpecSpec.scala", // skipped because does not compile yet  
        "RecoverMethodsSpec.scala", // skipped because does not compile yet 
        "RefSpecSpec.scala", // skipped because does not compile yet 
        "SequentialNestedSuiteExecutionSpec.scala", // skipped because does not compile yet 
        "SeveredStackTracesFailureSpec.scala", // skipped because tests failed 
        "SeveredStackTracesSpec.scala", // skipped because tests failed 
        "ShellSuite.scala", // skipped because does not compile yet 
        "ShorthandShouldBeThrownBySpec.scala", // skipped because tests failed 
        "ShorthandShouldNotBeThrownBySpec.scala", // skipped because does not compile yet 
        "ShouldBeAnSymbolSpec.scala", // skipped because does not compile yet 
        "ShouldBeAnTypeSpec.scala", // skipped because does not compile yet
        "ShouldBeASymbolSpec.scala", // skipped because does not compile yet  
        "ShouldBeATypeSpec.scala", // skipped because does not compile yet 
        "ShouldBeDefinedAtForAllSpec.scala", // skipped because does not compile yet 
        "ShouldBeDefinedAtSpec.scala", // skipped because does not compile yet 
        "ShouldBeNullSpec.scala", // skipped because does not compile yet 
        "ShouldBePropertyMatcherSpec.scala", // skipped because does not compile yet
        "ShouldBeShorthandForAllSpec.scala", // skipped because does not compile yet   
        "ShouldBeShorthandSpec.scala", // skipped because does not compile yet 
        "ShouldBeSortedLogicalAndSpec.scala", // skipped because does not compile yet 
        "ShouldBeSortedLogicalOrSpec.scala", // skipped because does not compile yet 
        "ShouldBeSortedSpec.scala", // skipped because does not compile yet 
        "ShouldBeSymbolSpec.scala", // skipped because does not compile yet 
        "ShouldBeThrownBySpec.scala", // skipped because does not compile yet 
        "ShouldContainElementNewSpec.scala", // skipped because does not compile yet 
        "ShouldContainElementSpec.scala", // skipped because does not compile yet  
        "ShouldEqualEqualitySpec.scala", // skipped because does not compile yet 
        "ShouldHavePropertiesSpec.scala", // skipped because does not compile yet 
        "ShouldLengthSizeSpec.scala", // skipped because does not compile yet 
        "ShouldLengthSpec.scala", // skipped because does not compile yet 
        "ShouldMatchPatternSpec.scala", // skipped because does not compile yet 
        "ShouldNotBeThrownBySpec.scala", // skipped because does not compile yet 
        "ShouldNotCompileSpec.scala", // skipped because tests failed
        "ShouldNotShorthandForAllSpec.scala", // skipped because does not compile yet 
        "ShouldNotShorthandSpec.scala", // skipped because does not compile yet 
        "ShouldNotTypeCheckSpec.scala", // skipped because tests failed 
        "ShouldSizeSpec.scala", // skipped because does not compile yet 
        "ShouldStructuralLengthSpec.scala", // skipped because does not compile yet 
        "ShouldStructuralSizeSpec.scala", // skipped because does not compile yet 
        "ShouldTripleEqualsEqualitySpec.scala", // skipped because does not compile yet 
        "ShouldTypeCheckedTripleEqualsEqualitySpec.scala", // skipped because does not compile yet 
        "StatefulStatusSpec.scala", // skipped because does not compile yet 
        "StatusProp.scala", // skipped because does not compile yet 
        "StatusSpec.scala", // skipped because does not compile yet 
        "StepwiseNestedSuiteExecutionSpec.scala", // skipped because does not compile yet 
        "StopOnFailureProp.scala", // skipped because does not compile yet 
        "StreamlinedXmlEqualitySpec.scala", // skipped because does not compile yet 
        "StreamlinedXmlNormMethodsSpec.scala", // skipped because does not compile yet 
        "StreamlinedXmlSpec.scala", // skipped because does not compile yet 
        "SuiteExamples.scala", // skipped because does not compile yet 
        "SuiteProp.scala", // skipped because does not compile yet 
        "SuiteSpec.scala", // skipped because does not compile yet 
        "SuiteSuite.scala", // skipped because does not compile yet 
        "TestColonEscapeProp.scala", // skipped because does not compile yet 
        "TestDataProp.scala", // skipped because does not compile yet 
        "TestNameProp.scala", // skipped because does not compile yet 
        "TypeCheckedAssertionsSpec.scala", // skipped because does not compile yet 
      )
    ) ++ 
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, 
      List(
        "DirectExpectationsSpec.scala"
      )
    ) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,
        List(
          //"DeprecatedIntegrationPatienceSpec.scala",
          "JavaFuturesSpec.scala",      // skipped because depends on java futures
          "TestThreadsStartingCounterSpec.scala",   // skipped because depends on Conductors
          "DeprecatedTimeLimitedTestsSpec.scala",   // skipped because DeprecatedTimeLimitedTests not supported.
          "TimeLimitsSpec.scala",  // skipped because failed with line number tests.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, 
        List(
          "PropCheckerAssertingAsyncSpec.scala", // skipped for failing tests.
          "PropCheckerAssertingSpec.scala" // skipped for failing tests.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, 
        List(
          "ExampleCancelSpec.scala", // skipped because does not compile yet.
          "ExampleCancelInNestedSuite.scala" // skipped because does not compile yet.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir,
        List(
          "TestLocationJUnit3Suite.scala",
          "TestLocationJUnitSuite.scala",
          "TestLocationTestNGSuite.scala",
          "TestLocationMethodJUnit3Suite.scala",
          "TestLocationMethodJUnitSuite.scala",
          "TestLocationMethodTestNGSuite.scala",
          "LocationMethodSuiteProp.scala", 
          "LocationSuiteProp.scala", // skipped because does not compile yet.
          "ScopePendingProp.scala", // skipped because does not compile yet.
          "LocationSpec.scala",  // skipped because does not compile yet.
          "LocationFunctionSuiteProp.scala", // skipped because does not compile yet.
          "EventSpec.scala", // skipped because does not compile yet.
          "DeprecatedScopePendingProp.scala",  // skipped because does not compile yet.
          "DeprecatedLocationSuiteProp.scala", // skipped because does not compile yet.
          "DeprecatedLocationFunctionSuiteProp.scala" // skipped because does not compile yet.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, 
        List(
          "StackDepthExceptionSpec.scala", // skipped because does not compile yet.
          "PayloadSpec.scala" // skipped because does not compile yet.
        )) ++
      /*copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir,
        List(
          "SpecSpec.scala",     // skipped because depends on java reflections
          "SuiteSpec.scala"    // skipped because depends on java reflections
        )) ++ */
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/path", "org/scalatest/path", targetDir, 
        List(
          "StackSpec.scala",  // skipped because does not compile yet.
          "FunSpecSpec.scala",  // skipped because does not compile yet.
          "FreeSpecSpec.scala" // skipped because does not compile yet.
        )) ++
      /*copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, List.empty) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/suiteprop", "org/scalatest/suiteprop", targetDir, List.empty) ++*/
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, 
        List(
          "TypeMatcherMacroSpec.scala", // skipped because does not compile yet.
          "MatcherProducersSpec.scala" // skipped because does not compile yet.
        )
      ) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/time", "org/scalatest/time", targetDir, 
        List(
          "SpanSugarSpec.scala" // skipped because does not compile yet.
        )
      ) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/verbs", "org/scalatest/verbs", targetDir, List.empty) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/tools", "org/scalatest/tools", targetDir,
        List(
          "DiscoverySuiteSuite.scala",  // skipped because failing test.
          "FilterReporterSpec.scala",  // skipped because does not compile yet.
          "FrameworkSuite.scala", // skipped because hang when tests execute.
          "ScalaTestFrameworkSuite.scala", // skipped because does not compile yet.
          "ScalaTestRunnerSuite.scala", // skipped because does not compile yet.
          "SuiteDiscoveryHelperSuite.scala",  // skipped because does not compile yet.
          "XmlSocketReporterSpec.scala", // skipped because tests failed execute.
          "SuiteSortingReporterSpec.scala",  // skipped because does not compile yet.
          "TestSortingReporterSpec.scala" // skipped because does not compile yet.
        )
      )
    }

    def genDiagramsTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/diagrams-test/src/test/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, 
        List(
          "DiagramsSpec.scala", // skipped because tests failed execute.
          "DirectDiagrammedAssertionsSpec.scala" // skipped because tests failed execute.
        )
      )

    def genFeatureSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/featurespec-test/src/test/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty)

    def genFlatSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec", "org/scalatest/flatspec", targetDir, 
        List(
          "FlatSpecImportedMatchersSpec.scala"
        )
      )    

    def genFreeSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/freespec-test/src/test/scala/org/scalatest/freespec", "org/scalatest/freespec", targetDir, 
        List(
          "FixtureFreeSpecSpec.scala" // skipped because tests failed
        )
      )

    def genFunSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/funspec-test/src/test/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, 
        List(
          "FixtureFunSpecSpec.scala" // skipped because tests failed
        )
      )    

    def genFunSuiteTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/funsuite-test/src/test/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, 
        List(
          "FunSuiteSpec.scala", 
          "FixtureFunSuiteSpec.scala"
        )
      )

    def genPropSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/propspec-test/src/test/scala/org/scalatest/propspec", "org/scalatest/propspec", targetDir, 
        List(
          "PropSpecSpec.scala", 
          "FixturePropSpecSpec.scala"
        )
      )

    def genWordSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/wordspec-test/src/test/scala/org/scalatest/wordspec", "org/scalatest/wordspec", targetDir, 
        List(
          "AsyncWordSpecLikeSpec.scala", // skipped because does not compile yet
          "AsyncWordSpecSpec.scala", // skipped because does not compile yet
          "FixtureAsyncWordSpecLikeSpec.scala", // skipped because does not compile yet
          "FixtureAsyncWordSpecSpec.scala", // skipped because does not compile yet
          "WordSpecImportedMatchersSpec.scala", // skipped because does not compile yet
          "WordSpecSpec.scala", // skipped because does not compile yet
          "FixtureWordSpecImportedMatchersSpec.scala", // skipped because does not compile yet
          "FixtureWordSpecSpec.scala" // skipped because does not compile yet
        )
      )    
}
