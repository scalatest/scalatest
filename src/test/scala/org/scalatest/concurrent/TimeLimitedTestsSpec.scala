/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.concurrent

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.time.{Span, Millis}
import org.scalatest._
import org.scalatest.exceptions.TestFailedDueToTimeoutException

class TimeLimitedTestsSpec extends FunSpec with ShouldMatchers with SharedHelpers with SeveredStackTraces {
  describe("A time-limited test") {
    describe("when it does not timeout") {
      describe("when it succeeds") {
        it("should succeed") {
          val a =
            new FunSuite with TimeLimitedTests {
              val timeLimit = Span(100L, Millis)
              test("plain old success") { assert(1 + 1 === 2) }
            }
          val rep = new EventRecordingReporter
          a.run(None, Args(rep))
          val ts = rep.testSucceededEventsReceived
          ts.size should be (1)
        }
      }
      describe("when it fails") {
        it("should fail") {
          val a =
            new FunSuite with TimeLimitedTests {
              val timeLimit = Span(100L, Millis)
              test("plain old failure") { assert(1 + 1 === 3) }
            }
          val rep = new EventRecordingReporter
          a.run(None, Args(rep))
          val tf = rep.testFailedEventsReceived
          tf.size should be (1)
        }
      }
    }
    describe("when it times out") {
      it("should fail with a timeout exception with the proper error message") {
        val a =
          new FunSuite with TimeLimitedTests {
            val timeLimit = Span(100L, Millis)
            test("time out failure") { Thread.sleep(500) }
          }
        val rep = new EventRecordingReporter
        a.run(None, Args(rep))
        val tf = rep.testFailedEventsReceived
        tf.size should be (1)
        val tfe = tf(0)
        tfe.message should be (Resources("testTimeLimitExceeded", "100 milliseconds"))
      }
      it("should fail with a timeout exception with the proper cause, if the test timed out after it completed abruptly") {
        val a =
          new FunSuite with TimeLimitedTests {
            val timeLimit = Span(10L, Millis)
            override val defaultTestInterruptor = DoNotInterrupt
            test("time out failure") {
              Thread.sleep(50)
              throw new RuntimeException("oops!")
            }
          }
        val rep = new EventRecordingReporter
        a.run(None, Args(rep))
        val tf = rep.testFailedEventsReceived
        tf.size should be (1)
        val tfe = tf(0)
        tfe.message should be (Resources("testTimeLimitExceeded", "10 milliseconds"))
        import org.scalatest.OptionValues._
        tfe.throwable.value match {
          case tfe: TestFailedDueToTimeoutException =>
          	tfe.cause.value match {
              case re: RuntimeException =>
                re.getMessage should be ("oops!")
              case e => fail("Cause was not a RuntimeException", e)
          	}
          case e => fail("Was not a TestFailedDueToTimeoutException", e)
        }
      }
    }
  }
}

