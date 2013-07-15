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

import time.SpanSugar._

class SlowpokeDetectorSpec extends Spec with Matchers {

  object `The Slowpoke detector` {
    def `should allow a timeout and/or interval to be specified at construction` {
      // Timeout is how long something needs to be running before we send an info provided complaining about it
      // interval is how long we sleep between looking to see if anything has been there longer htan the timeout
      new SlowpokeDetector(timeout = 1 minute, interval = 10 seconds)
      new SlowpokeDetector(timeout = 1 minute)
      new SlowpokeDetector(interval = 10 seconds)
    }
    def `should offer a method to register a starting suite` {
      val spd = new SlowpokeDetector
      spd.suiteStarting(
        suiteId = "the suite name",
        suiteName = "the suite ID",
        timeStamp = 10
      )
      a [NullPointerException] should be thrownBy {
        spd.suiteStarting(
          suiteId = null,
          suiteName = "the suite ID",
          timeStamp = 10
        )
      }
      a [NullPointerException] should be thrownBy {
        spd.suiteStarting(
          suiteId = "the suite name",
          suiteName = null,
          timeStamp = 10
        )
      }
      an [IllegalArgumentException] should be thrownBy {
        spd.suiteStarting(
          suiteId = "the suite name",
          suiteName = "the suite ID",
          timeStamp = -10
        )
      }
    }
    def `should offer a method to register a finished (completed or aborted) suite` {
      val spd = new SlowpokeDetector
      spd.suiteFinished(
        suiteId = "the suite name",
        suiteName = "the suite ID",
        timeStamp = 10
      )
      a [NullPointerException] should be thrownBy {
        spd.suiteFinished(
          suiteId = null,
          suiteName = "the suite ID",
          timeStamp = 10
        )
      }
      a [NullPointerException] should be thrownBy {
        spd.suiteFinished(
          suiteId = "the suite name",
          suiteName = null,
          timeStamp = 10
        )
      }
      an [IllegalArgumentException] should be thrownBy {
        spd.suiteFinished(
          suiteId = "the suite name",
          suiteName = "the suite ID",
          timeStamp = -10
        )
      }
    }
    def `should offer a method to register a starting test` {
      val spd = new SlowpokeDetector
      spd.testStarting(
        suiteId = "the suite name",
        suiteName = "the suite ID",
        testName = "the test name",
        timeStamp = 10
      )
      a [NullPointerException] should be thrownBy {
        spd.testStarting(
          suiteId = null,
          suiteName = "the suite ID",
          testName = "the test name",
          timeStamp = 10
        )
      }
      a [NullPointerException] should be thrownBy {
        spd.testStarting(
          suiteId = "the suite name",
          suiteName = null,
          testName = "the test name",
          timeStamp = 10
        )
      }
      a [NullPointerException] should be thrownBy {
        spd.testStarting(
          suiteId = "the suite name",
          suiteName = "the suite ID",
          testName = null,
          timeStamp = 10
        )
      }
      an [IllegalArgumentException] should be thrownBy {
        spd.testStarting(
          suiteId = "the suite name",
          suiteName = "the suite ID",
          testName = "the test name",
          timeStamp = -10
        )
      }
    }
    def `should offer a method to register a finished (succeeded, failed, pending, canceled, or omitted) test` {
      val spd = new SlowpokeDetector
      spd.testFinished(
        suiteId = "the suite name",
        suiteName = "the suite ID",
        testName = "the test name",
        timeStamp = 10
      )
      a [NullPointerException] should be thrownBy {
        spd.testFinished(
          suiteId = null,
          suiteName = "the suite ID",
          testName = "the test name",
          timeStamp = 10
        )
      }
      a [NullPointerException] should be thrownBy {
        spd.testFinished(
          suiteId = "the suite name",
          suiteName = null,
          testName = "the test name",
          timeStamp = 10
        )
      }
      a [NullPointerException] should be thrownBy {
        spd.testFinished(
          suiteId = "the suite name",
          suiteName = "the suite ID",
          testName = null,
          timeStamp = 10
        )
      }
      an [IllegalArgumentException] should be thrownBy {
        spd.testFinished(
          suiteId = "the suite name",
          suiteName = "the suite ID",
          testName = "the test name",
          timeStamp = -10
        )
      }
    }
  }
}
