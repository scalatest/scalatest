/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.tools

import org.scalatest._
import org.scalatest.events.Ordinal
import org.scalatest.events.TestStarting
import org.scalatest.events.TestFailed
import org.scalatest.events.TestSucceeded
import org.scalatest.events.TestCanceled
import org.scalatest.events.SuiteAborted
import org.scalatest.events.RecordableEvent
import org.scalatest.events.RunCompleted
import org.scalatest.events.RunStarting
import org.scalatest.ConfigMap

import java.io.File
import org.scalatest.funsuite.AnyFunSuite

class MemoryReporterSuite extends AnyFunSuite {
  val OutputFileName = "target/MemoryReporterSuite.out"

  val runComplete =
    RunCompleted(new Ordinal(0))

  val runStarting =
    RunStarting(
      new Ordinal(1),
      testCount = 3,
      configMap = new ConfigMap(Map[String, Any]()))

  // testName is set to "Some(say one)" here to verify proper encoding/decoding
  // by Memento when name might be confused for an Option
  val testStarting1 =
    TestStarting(
      new Ordinal(2),
      suiteName = "org.example.OneSuite",
      suiteId = "org.example.OneSuite",
      suiteClassName = Some("org.example.OneSuite"),
      testName = "Some(say one)",
      testText = "say one")

  val testFailed =
    TestFailed(
      new Ordinal(3),
      message = "say what?",
      suiteName = "org.example.OneSuite",
      suiteId = "org.example.OneSuite",
      suiteClassName = Some("org.example.OneSuite"),
      testName = "Some(say one)",
      testText = "say one",
      recordedEvents = Vector.empty[RecordableEvent],
      analysis = Vector.empty[String],
      rerunner = Some("org.example.OneSuite"))

  val testStarting2 =
    TestStarting(
      new Ordinal(4),
      suiteName = "org.example.YourSuite",
      suiteId = "org.example.YourSuite",
      suiteClassName = Some("org.example.YourSuite"),
      testName = "say hey",
      testText = "say hey")

  val testSucceeded =
    TestSucceeded(
      new Ordinal(5),
      suiteName = "org.example.YourSuite",
      suiteId = "org.example.YourSuite",
      suiteClassName = Some("org.example.YourSuite"),
      testName = "say hey",
      testText = "say hey",
      recordedEvents = Vector.empty[RecordableEvent])

  val testStarting3 =
    TestStarting(
      new Ordinal(6),
      suiteName = "org.example.NevermindSuite",
      suiteId = "org.example.NevermindSuite",
      suiteClassName = Some("org.example.NevermindSuite"),
      testName = "None",
      testText = "say nevermind")

  // testName is set to "None" here to verify proper encoding/decoding
  // by Memento when name might be confused for an Option
  val testCanceled =
    TestCanceled(
      new Ordinal(7),
      message = "nevermind",
      suiteName = "org.example.NevermindSuite",
      suiteId = "org.example.NevermindSuite",
      suiteClassName = Some("org.example.NevermindSuite"),
      testName = "None",
      testText = "say nevermind",
      recordedEvents = Vector.empty[RecordableEvent],
      rerunner = Some("org.example.NevermindSuite"))

  val testStarting4 =
    TestStarting(
      new Ordinal(8),
      suiteName = "org.example.BailSuite",
      suiteId = "org.example.BailSuite",
      suiteClassName = Some("org.example.BailSuite"),
      testName = "say bail",
      testText = "say bail")

  val suiteAborted =
    SuiteAborted(
      new Ordinal(9),
      message = "bail",
      suiteName = "org.example.BailSuite",
      suiteId = "org.example.BailSuite",
      suiteClassName = Some("org.example.BailSuite"),
      rerunner = Some("org.example.BailSuite"))

  test("""|MemoryReporter writes empty file upon RunComplete
          |event if no failure events were reported""".stripMargin) {
    val file = new File(OutputFileName)
    file.delete

    val reporter = new MemoryReporter(OutputFileName)

    reporter(runStarting)
    reporter(testStarting2)
    reporter(testSucceeded)
    reporter(runComplete)

    assert(file.exists)
    assert(file.length === 0)
    file.delete
  }

  test("MemoryReporter records a failure event") {
    val file = new File(OutputFileName)
    file.delete

    val reporter = new MemoryReporter(OutputFileName)

    reporter(runStarting)
    reporter(testStarting1)
    reporter(testFailed)
    reporter(testStarting2)
    reporter(testSucceeded)
    reporter(runComplete)

    assert(file.exists)
    assert(file.length !== 0)

    val mementos = Memento.readFromFile(OutputFileName)
    assert(mementos.length === 1)

    val suiteParam = mementos(0).toSuiteParam
    assert(suiteParam.className === testFailed.suiteClassName.get)
    assert(suiteParam.testNames.length === 1)
    assert(suiteParam.testNames(0) === testFailed.testName)
    assert(suiteParam.wildcardTestNames.length === 0)
    assert(suiteParam.nestedSuites.length === 0)

    file.delete
  }

  test("MemoryReporter records multiple events") {
    val file = new File(OutputFileName)
    file.delete

    val reporter = new MemoryReporter(OutputFileName)

    reporter(runStarting)
    reporter(testStarting1)
    reporter(testFailed)
    reporter(testStarting2)
    reporter(testSucceeded)
    reporter(testStarting3)
    reporter(testCanceled)
    reporter(testStarting4)
    reporter(suiteAborted)
    reporter(runComplete)

    assert(file.exists)
    assert(file.length !== 0)

    val mementos = Memento.readFromFile(OutputFileName)
    assert(mementos.length === 3)

    val suiteParam0 = mementos(0).toSuiteParam
    assert(suiteParam0.className === suiteAborted.suiteClassName.get)
    assert(suiteParam0.testNames.length === 0)
    assert(suiteParam0.wildcardTestNames.length === 0)
    assert(suiteParam0.nestedSuites.length === 0)

    val suiteParam1 = mementos(1).toSuiteParam
    assert(suiteParam1.className === testCanceled.suiteClassName.get)
    assert(suiteParam1.testNames.length === 1)
    assert(suiteParam1.testNames(0) === testCanceled.testName)
    assert(suiteParam1.wildcardTestNames.length === 0)
    assert(suiteParam1.nestedSuites.length === 0)

    val suiteParam2 = mementos(2).toSuiteParam
    assert(suiteParam2.className === testFailed.suiteClassName.get)
    assert(suiteParam2.testNames.length === 1)
    assert(suiteParam2.testNames(0) === testFailed.testName)
    assert(suiteParam2.wildcardTestNames.length === 0)
    assert(suiteParam2.nestedSuites.length === 0)

    file.delete
  }
}
