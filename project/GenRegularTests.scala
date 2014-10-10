/*
* Copyright 2001-2014 Artima, Inc.
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

import collection.mutable.ListBuffer
import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenRegularTests1 extends GenRegularTestsBase {

  def includeFile(file: File): Boolean =
    file.isFile &&
    (file.getName.startsWith("Should") || file.getName.startsWith("ListShould") || file.getName.startsWith("EveryShould") || file.getName.startsWith("OptionShould")) &&
    (file.getName.hashCode % 4 == 0)

  val baseDirPath: String = "src/test/scala/org/scalatest"
  val name: String = "genregular1"
}

object GenRegularTests2 extends GenRegularTestsBase {

  def includeFile(file: File): Boolean =
    file.isFile &&
    (file.getName.startsWith("Should") || file.getName.startsWith("ListShould") || file.getName.startsWith("EveryShould") || file.getName.startsWith("OptionShould")) &&
    (file.getName.hashCode % 3 == 1)

  val baseDirPath: String = "src/test/scala/org/scalatest"
  val name: String = "genregular2"
}

object GenRegularTests3 extends GenRegularTestsBase {

  def includeFile(file: File): Boolean =
    file.isFile &&
    (file.getName.startsWith("Should") || file.getName.startsWith("ListShould") || file.getName.startsWith("EveryShould") || file.getName.startsWith("OptionShould")) &&
    (file.getName.hashCode % 3 == 2)

  val baseDirPath: String = "src/test/scala/org/scalatest"
  val name: String = "genregular3"
}

object GenRegularTests4 extends GenRegularTestsBase {

  def includeFile(file: File): Boolean =
    file.isFile &&
    !(file.getName.startsWith("Should") || file.getName.startsWith("ListShould") || file.getName.startsWith("EveryShould") || file.getName.startsWith("OptionShould")) &&
    (file.getName.hashCode % 2 == 0)

  val baseDirPath: String = "src/test/scala/org/scalatest"
  val name: String = "genregular4"

  override def genTest(targetBaseDir: File, version: String, scalaVersion: String) {
    copyJavaDir(targetBaseDir)

    // these are accessed through reflections.
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/EventHelpers.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/ParallelTestExecutionTestTimeoutExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/ParallelTestExecutionSuiteTimeoutExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/ParallelTestExecutionParallelSuiteExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/UnitSpec.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/FunctionSuiteProp.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/FunctionSuiteExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/ExampleBeforeAfterParallelSpec.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/ExampleTimeoutParallelSpec.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/events/TestLocationFunctionServices.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/SuiteProp.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/SuiteExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/events/TestLocationServices.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/events/TestLocationJUnitSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/events/examples/ExampleCancelSpec.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/jmock/SuiteExpectations.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/jmock/TestReporter.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/RunInSpurtsSpec1.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/suiteprop/SuiteExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/SlowSampleSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/AbortedSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/CustomTaggedSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/DiskTaggedSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/DoNotDiscoverSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/NestedConfigMapSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/SlowSampleSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/scalasbt/SuiteWithFailedCanceledTests.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/tools/RunnerSpec.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/enablers/NoParamSpec.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/FilterSpec.scala")

    super.genTest(targetBaseDir, version, scalaVersion)
  }
}

object GenRegularTests5 extends GenRegularTestsBase {

  def includeFile(file: File): Boolean =
    file.isFile &&
      !(file.getName.startsWith("Should") || file.getName.startsWith("ListShould") || file.getName.startsWith("EveryShould") || file.getName.startsWith("OptionShould")) &&
      (file.getName.hashCode % 2 == 1)

  val baseDirPath: String = "src/test/scala/org/scalatest"
  val name: String = "genregular5"

  override def genTest(targetBaseDir: File, version: String, scalaVersion: String) {
    copyJavaDir(targetBaseDir)

    // these are accessed through reflections.
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/SuiteExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/StringFixture.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/events/examples/ExampleCancelInNestedSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/MethodSuiteProp.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/events/TestLocationMethodServices.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/MethodSuiteExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/events/TestLocationMethodJUnitSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/junit/junit4helpers/TestWasCalledSuite.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/time/SpanMatchers.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/UnitSpec.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/suiteprop/SuiteExamples.scala")
    copyFile(targetBaseDir, "src/test/scala/org/scalatest/EasySuite.scala")

    super.genTest(targetBaseDir, version, scalaVersion)
  }
}
