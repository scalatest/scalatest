/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalatest.OptionValues._
import AsyncTimeouts._
import org.scalatest.SharedHelpers.thisLineNumber
import java.io.ByteArrayInputStream
import java.net.SocketException
import java.net.ServerSocket
import java.net.Socket
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import java.nio.channels.SocketChannel
import org.scalatest.exceptions.{TestFailedException, TestCanceledException}
import org.scalatest.time._
import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.Retries._
import org.scalatest.tagobjects.Retryable

import scala.concurrent.Future

class AsyncTimeoutsSpec extends AsyncFunSpec with Matchers {

  /*override def withFixture(test: NoArgTest) = {
    if (isRetryable(test))
      withRetry { super.withFixture(test) }
    else
      super.withFixture(test)
  }*/

  describe("The failingAfter construct") {

    describe(" when used with Outcome") {

      it("should not catch exception thrown from the test") {
        assertThrows[InterruptedException] {
          failingAfter(Span(100, Millis)) {
            throw new InterruptedException
            Future.successful(Succeeded)
          }
        }
      }

      it("should fail with TestFailedException when it times out", Retryable) {
        val future: Future[Outcome] =
          failingAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            Future.successful(Succeeded)
          }
        future.map { outcome =>
          outcome match {
            case Failed(tfe: TestFailedException) =>
              tfe.message.value should be(Resources.timeoutFailingAfter("100 milliseconds"))
              tfe.failedCodeLineNumber.value should equal(thisLineNumber - 8)
              tfe.failedCodeFileName.value should be("AsyncTimeoutsSpec.scala")

            case other => fail("Expect Failed as the result, but got: " + other)
          }
        }
      }

      it("should pass normally when the timeout is not reached") {
        failingAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          Future.successful(Succeeded)
        }
      }
    }

    describe(" when used with Assertion") {

      it("should not catch exception thrown from the test") {
        assertThrows[InterruptedException] {
          failingAfter(Span(100, Millis)) {
            throw new InterruptedException
            assert(1 == 1)
          }
        }
      }

      it("should fail with TestFailedException when it times out", Retryable) {
        val tfe =
          intercept[TestFailedException] {
            failingAfter(Span(100, Millis)) {
              SleepHelper.sleep(200)
              assert(1 == 1)
            }
          }

        tfe.message.value should be(Resources.timeoutFailingAfter("100 milliseconds"))
        tfe.failedCodeLineNumber.value should equal(thisLineNumber - 7)
        tfe.failedCodeFileName.value should be("AsyncTimeoutsSpec.scala")
      }

      it("should pass normally when the timeout is not reached") {
        failingAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          assert(1 == 1)
        }
      }

      it("should set the exception thrown from the test after timeout as cause of TestFailedException") {
        val tfe =
          intercept[TestFailedException] {
            failingAfter(Span(100, Millis)) {
              for (i <- 1 to 10) {
                SleepHelper.sleep(50)
                assert(1 == 1)
              }
              throw new IllegalArgumentException("Something went wrong!")
              succeed
            }
          }
        assert(tfe.getCause().getClass == classOf[IllegalArgumentException])
      }
    }
  }

  describe("The cancelingAfter construct") {

    describe(" when used with Outcome") {

      it("should be canceled with TestCanceledException when it times out") {
        val futureOfOutcome: Future[Outcome] =
          cancelingAfter(Span(1000, Millis)) {
            SleepHelper.sleep(2000)
            Future.successful(Succeeded)
          }

        futureOfOutcome.map { outcome =>
          outcome match {
            case Canceled(t) =>
              t.message.value should be(Resources.timeoutCancelingAfter("1000 milliseconds"))
              t.failedCodeLineNumber.value should equal(thisLineNumber - 9)
              t.failedCodeFileName.value should be("AsyncTimeoutsSpec.scala")

              succeed

            case other => fail("Expect Canceled as the result, but got: " + other)
          }
        }
      }

      it("should pass normally when timeout is not reached") {
        cancelingAfter(Span(2000, Millis)) {
          SleepHelper.sleep(1000)
          Future.successful(Succeeded)
        }
      }

      it("should not catch exception thrown from the test") {
        assertThrows[InterruptedException] {
          cancelingAfter(Span(1000, Millis)) {
            throw new InterruptedException
            succeed
          }
        }
      }
    }

    describe(" when used with Assertion") {

      it("should not catch exception thrown from the test") {
        assertThrows[InterruptedException] {
          cancelingAfter(Span(100, Millis)) {
            throw new InterruptedException
            assert(1 == 1)
          }
        }
      }

      it("should be canceled with TestCanceledException when it times out", Retryable) {
        val tce =
          intercept[TestCanceledException] {
            cancelingAfter(Span(100, Millis)) {
              SleepHelper.sleep(200)
              assert(1 == 1)
            }
          }

        tce.message.value should be(Resources.timeoutCancelingAfter("100 milliseconds"))
        tce.failedCodeLineNumber.value should equal(thisLineNumber - 7)
        tce.failedCodeFileName.value should be("AsyncTimeoutsSpec.scala")
      }

      it("should pass normally when the timeout is not reached") {
        cancelingAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          assert(1 == 1)
        }
      }

      it("should set the exception thrown from the test after timeout as cause of TestCanceledException") {
        val tce =
          intercept[TestCanceledException] {
            cancelingAfter(Span(100, Millis)) {
              for (i <- 1 to 10) {
                SleepHelper.sleep(50)
                assert(1 == 1)
              }
              throw new IllegalArgumentException("Something went wrong!")
              succeed
            }
          }
        assert(tce.getCause().getClass == classOf[IllegalArgumentException])
      }
    }
  }
}