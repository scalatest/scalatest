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
package org.scalatest.tools

import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class SuiteRunnerSpec extends AnyFunSpec {

  describe("SuiteRunner") {

    it("should fire SuiteAborted event when after function in BeforeAndAfter throws RuntimeException") {

      class ExampleSuite extends AnyFunSuite with BeforeAndAfter {

        test("test 1") {}

        after {
          throw new RuntimeException("oops!")
        }

      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      val runner = new SuiteRunner(suite, Args(rep), new ScalaTestStatefulStatus)
      runner.run()
      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterAll function in BeforeAndAfterAll throws RuntimeException") {

      class ExampleSuite extends AnyFunSuite with BeforeAndAfterAll {

        test("test 1") {}

        override protected def afterAll(): Unit = {
          throw new RuntimeException("oops!")
        }

      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      val runner = new SuiteRunner(suite, Args(rep), new ScalaTestStatefulStatus)
      runner.run()
      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterAll function in BeforeAndAfterAllConfigMap throws RuntimeException") {

      class ExampleSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {

        test("test 1") {}

        override protected def afterAll(configMap: ConfigMap): Unit = {
          throw new RuntimeException("oops!")
        }

      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      val runner = new SuiteRunner(suite, Args(rep), new ScalaTestStatefulStatus)
      runner.run()
      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterEach function in BeforeAndAfterEach throws RuntimeException") {

      class ExampleSuite extends AnyFunSuite with BeforeAndAfterEach {

        test("test 1") {}

        override protected def afterEach(): Unit = {
          throw new RuntimeException("oops!")
        }

      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      val runner = new SuiteRunner(suite, Args(rep), new ScalaTestStatefulStatus)
      runner.run()
      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterEach function in BeforeAndAfterEachTestData throws RuntimeException") {

      class ExampleSuite extends AnyFunSuite with BeforeAndAfterEachTestData {

        test("test 1") {}

        override protected def afterEach(testData: TestData): Unit = {
          throw new RuntimeException("oops!")
        }

      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      val runner = new SuiteRunner(suite, Args(rep), new ScalaTestStatefulStatus)
      runner.run()
      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterEach function in BeforeAndAfterEachTestData throws RuntimeException with a null message") {

      class ExampleSuite extends AnyFunSuite with BeforeAndAfterEachTestData {

        test("test 1") {}

        override protected def afterEach(testData: TestData): Unit = {
          throw new RuntimeException(null: String)
        }

      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      val runner = new SuiteRunner(suite, Args(rep), new ScalaTestStatefulStatus)
      runner.run()
      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

  }

}