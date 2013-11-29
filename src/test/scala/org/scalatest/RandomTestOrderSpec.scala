/*
 * Copyright 2001-2013 Artima, Inc.
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

import collection.mutable.ListBuffer
import SharedHelpers.EventRecordingReporter
import concurrent.Eventually._

class RandomTestOrderSpec extends Spec {

  object `RandomTestOrder ` {

    class ExampleSpec(listBuffer: ListBuffer[Int]) extends FunSpec with RandomTestOrder {

      it("test 1") {
        listBuffer += 0
      }

      it("test 2") {
        listBuffer += 1
      }

      it("test 3") {
        listBuffer += 2
      }

      override def newInstance = new ExampleSpec(listBuffer)

    }

    def `execute tests in random order, but fire events in original order` {
      val rep =
        eventually {
          val buffer = new ListBuffer[Int]
          val spec = new ExampleSpec(buffer)
          val rep = new EventRecordingReporter
          spec.run(None, Args(reporter = rep))
          assert(buffer(0) != 0 || buffer(1) != 1 || buffer(2) != 2)
          rep
        }

      val testStartingList = rep.testStartingEventsReceived
      assert(testStartingList.size == 3)
      assert(testStartingList(0).testName == "test 1")
      assert(testStartingList(1).testName == "test 2")
      assert(testStartingList(2).testName == "test 3")

      val testSucceededList = rep.testSucceededEventsReceived
      assert(testSucceededList.size == 3)
      assert(testSucceededList(0).testName == "test 1")
      assert(testSucceededList(1).testName == "test 2")
      assert(testSucceededList(2).testName == "test 3")
    }

  }

}