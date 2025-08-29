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
package org.scalatest

import SharedHelpers.EventRecordingReporter
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class TestsBeforeNestedSuitesSpec extends AnyFunSpec {

  var testHasBeenRun: Boolean = false
  var nestedSuiteWasRunLater: Boolean = false

  describe("TestBeforeNestedSuiteSpec") {

    it("should run test first before nested suite(s)") {
      class TestSpec extends AnyFunSuite with TestsBeforeNestedSuites {
        test("this is a test") {
          testHasBeenRun = true
        }
        
        override val nestedSuites = Vector(
          new AnyFunSuite {
            test("this is nested test") {
              if (testHasBeenRun)
                nestedSuiteWasRunLater = true
            }
          }
        )
      }
      
      val suite = new TestSpec
      val myRep = new EventRecordingReporter
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 2)
      assert(testStarting(0).testName == "this is a test")
      assert(testStarting(1).testName == "this is nested test") 
      assert(testStarting(1).timeStamp >= testStarting(0).timeStamp)
      assert(nestedSuiteWasRunLater)
    }
  }
}
