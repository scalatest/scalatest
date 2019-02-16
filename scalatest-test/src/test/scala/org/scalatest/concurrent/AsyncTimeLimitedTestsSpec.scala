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
package org.scalatest.concurrent

import org.scalatest._
import SharedHelpers._
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.{Span, Millis}
import scala.concurrent.Future

class AsyncTimeLimitedTestsSpec extends FunSpec with Matchers {
  describe("A time-limited test") {
    describe("when it does not timeout ") {
      describe("when it succeeds") {
        it("should succeed without Future") {
          val a =
            new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
              val timeLimit = Span(1000L, Millis)
              test("plain old success") { assert(1 + 1 === 2) }
            }
          val rep = new EventRecordingReporter
          val status = a.run(None, Args(rep))
          val ts = rep.testSucceededEventsReceived
          ts.size should be (1)
        }
        it("should succeed with Future") {
          val a =
            new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
              val timeLimit = Span(100L, Millis)
              test("plain old success") { Future { assert(1 + 1 === 2) } }
            }
          val rep = new EventRecordingReporter
          val status = a.run(None, Args(rep))
          val ts = rep.testSucceededEventsReceived
          ts.size should be (1)
        }
      }
      describe("when it fails") {
        it("should fail without Future") {
          val a =
            new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
              val timeLimit = Span(100L, Millis)
              test("plain old failure") { assert(1 + 1 === 3) }
            }
          val rep = new EventRecordingReporter
          val status = a.run(None, Args(rep))
          val tf = rep.testFailedEventsReceived
          tf.size should be (1)
        }
        it("should fail with Future") {
          val a =
            new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
              val timeLimit = Span(100L, Millis)
              test("plain old failure") { Future { assert(1 + 1 === 3) } }
            }
          val rep = new EventRecordingReporter
          val status = a.run(None, Args(rep))
          val tf = rep.testFailedEventsReceived
          tf.size should be (1)
        }
      }
    }
    describe("when it times out") {
      it("should fail with a timeout exception with the proper error message test when timeout from main code") {
        val a =
          new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
            val timeLimit = Span(100L, Millis)
            test("time out failure") { SleepHelper.sleep(500); succeed }
          }
        val rep = new EventRecordingReporter
        a.run(None, Args(rep))
        val tf = rep.testFailedEventsReceived
        tf.size should be (1)
        val tfe = tf(0)
        tfe.message should be (Resources.testTimeLimitExceeded("100 milliseconds"))
      }
      it("should fail with a timeout exception with the proper error message test when timeout from future returned") {
        val a =
          new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
            val timeLimit = Span(100L, Millis)
            test("time out failure") { Future { SleepHelper.sleep(500); succeed } }
          }
        val rep = new EventRecordingReporter
        a.run(None, Args(rep))
        val tf = rep.testFailedEventsReceived
        tf.size should be (1)
        val tfe = tf(0)
        tfe.message should be (Resources.testTimeLimitExceeded("100 milliseconds"))
      }
      it("should fail directly with thrown exception, if the test timed out after it completed abruptly from main code") {
        val a =
          new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
            val timeLimit = Span(10L, Millis)
            test("time out failure") {
              SleepHelper.sleep(50)
              throw new RuntimeException("oops!")
            }
          }
        val rep = new EventRecordingReporter
        a.run(None, Args(rep))
        val tf = rep.testFailedEventsReceived
        tf.size should be (1)
        val tfe = tf(0)
        tfe.message should be ("oops!")
        import org.scalatest.OptionValues._
        tfe.throwable.value match {
          case re: RuntimeException => re.getMessage shouldBe "oops!"
          case e => fail("throwable was not a RuntimeException", e)
        }
      }
      it("should fail with a timeout exception with the proper cause, if the test timed out after it completed abruptly from future returned") {
        val a =
          new AsyncFunSuite with DefaultFutureAssertionConverter with AsyncTimeLimitedTests {
            val timeLimit = Span(1000L, Millis)
            test("time out failure") {
              Future {
                SleepHelper.sleep(5000)
                throw new RuntimeException("oops!")
              }
            }
          }
        val rep = new EventRecordingReporter
        a.run(None, Args(rep))
        val tf = rep.testFailedEventsReceived
        tf.size should be (1)
        val tfe = tf(0)
        tfe.message should be (Resources.testTimeLimitExceeded("1000 milliseconds"))
        import org.scalatest.OptionValues._
        tfe.throwable.value match {
          case tfe2: TestFailedDueToTimeoutException =>
            tfe2.cause should not be defined

          case e => fail("Was not a TestFailedDueToTimeoutException", e)
        }
      }
    }
  }
}
