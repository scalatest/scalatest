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

import org.scalatest.Matchers
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.OptionValues
import org.scalatest.FunSpec
import scala.concurrent.{Future => FutureOfScala}
import scala.concurrent.Promise
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.CanAwait
import scala.concurrent.Awaitable
import scala.concurrent.ExecutionContext
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import org.scalatest._
import time._
import exceptions.{TestCanceledException, TestFailedException, TestPendingException}
import util.Try
import util.Success
import util.Failure

class ScalaFuturesSpec extends FunSpec with Matchers with OptionValues with ScalaFutures with SeveredStackTraces {

  describe("A FutureConcept") {

    val alreadySucceededFuture: FutureOfScala[String] = {
      val promise = Promise[String]()
      promise.success("hi")
      promise.future
    }
    val neverReadyFuture: FutureOfScala[String] = {
      val promise = Promise[String]
      promise.future
    }
    def newNeverReadyCountingFuture(increment: => Unit): FutureOfScala[String] =
      new FutureOfScala[String] {
        def isCompleted = neverReadyFuture.isCompleted
        def onComplete[U](func: Try[String] => U)(implicit executor: ExecutionContext): Unit = neverReadyFuture.onComplete(func)
        def value: Option[Try[String]] = {
          increment
          neverReadyFuture.value
        }

        @throws(classOf[Exception])
        def result(atMost: Duration)(implicit permit: CanAwait): String = neverReadyFuture.result(atMost)

        @throws(classOf[TimeoutException])
        @throws(classOf[InterruptedException])
        def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
          neverReadyFuture.ready(atMost)
          this
        }
      }

    def newAlreadySucceededCountingFuture(increment: => Unit): FutureOfScala[String] =
      new FutureOfScala[String] {
        def isCompleted = alreadySucceededFuture.isCompleted
        def onComplete[U](func: Try[String] => U)(implicit executor: ExecutionContext): Unit = alreadySucceededFuture.onComplete(func)
        def value: Option[Try[String]] = {
          increment
          alreadySucceededFuture.value
        }

        @throws(classOf[Exception])
        def result(atMost: Duration)(implicit permit: CanAwait): String = alreadySucceededFuture.result(atMost)

        @throws(classOf[TimeoutException])
        @throws(classOf[InterruptedException])
        def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
          alreadySucceededFuture.ready(atMost)
          this
        }
      }

    describe("when using the isReadyWithin method") {

      it("should just return the result if the future completes normally") {
        val futureIsNow = alreadySucceededFuture
        futureIsNow.isReadyWithin(Span(1, Second)) should be (true)
      }

      it("should query a never-ready future by at least the specified timeout") {
        var startTime = System.currentTimeMillis
        neverReadyFuture.isReadyWithin(Span(1250, Milliseconds)) should be (false)
        (System.currentTimeMillis - startTime).toInt should be >= (1250)
      }

      it("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {

        val reFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new RuntimeException("oops"))
          promise.future
        }
        val caught =
          intercept[TestFailedException] {
            reFuture.isReadyWithin(Span(1, Millisecond))
          }
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be ("oops")
      }

      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {

        val vmeFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new VirtualMachineError {})
          promise.future
        }
        intercept[VirtualMachineError] {
          vmeFuture.isReadyWithin(Span(1, Millisecond))
        }
      }

      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new TestPendingException)
          promise.future
        }
        intercept[TestPendingException] {
          tpeFuture.isReadyWithin(Span(1, Millisecond))
        }
      }

      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val tceFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new TestCanceledException(0))
          promise.future
        }
        intercept[TestCanceledException] {
          tceFuture.isReadyWithin(Span(1, Millisecond))
        }
      }
    }

    describe("when using the futureValue method") {

      it("should just return the result if the future completes normally") {
        val futureIsNow = alreadySucceededFuture
        futureIsNow.futureValue should equal ("hi")
      }

      it("should eventually blow up with a TFE if the future is never ready") {

        var count = 0
        val neverReadyCountingFuture = newNeverReadyCountingFuture { count += 1 }
        val caught = evaluating {
          neverReadyCountingFuture.futureValue
        } should produce [TestFailedException]

        caught.message.value should be (Resources("wasNeverReady", count.toString, "15 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      it("should provide the correct stack depth") {
        val caught1 = evaluating {
          neverReadyFuture.futureValue(timeout(Span(100, Millis)), interval(Span(1, Millisecond)))
        } should produce [TestFailedException]
        caught1.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught1.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught3 = evaluating {
          neverReadyFuture.futureValue(timeout(Span(100, Millis)))
        } should produce [TestFailedException]
        caught3.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught3.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught4 = evaluating {
          neverReadyFuture.futureValue(interval(Span(1, Millisecond)))
        } should produce [TestFailedException]
        caught4.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught4.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      it("should by default query a never-ready future for at least 1 second") {
        var startTime = System.currentTimeMillis
        evaluating {
          neverReadyFuture.futureValue
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (150)
      }

      it("should, if an alternate implicit Timeout is provided, query a never-ready by at least the specified timeout") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(1500, Millis))

        var startTime = System.currentTimeMillis
        evaluating {
          neverReadyFuture.futureValue
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (1500)
      }

      it("should, if an alternate explicit timeout is provided, query a never-ready future by at least the specified timeout") {
        var startTime = System.currentTimeMillis
        evaluating {
          neverReadyFuture.futureValue(timeout(Span(1250, Milliseconds)))
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (1250)
      }

      it("should, if an alternate explicit timeout is provided along with an explicit interval, query a never-ready future by at least the specified timeout, even if a different implicit is provided") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(500, Millis), interval = Span(2, Millis))

        var startTime = System.currentTimeMillis
        evaluating {
          neverReadyFuture.futureValue(timeout(Span(1388, Millis)), interval(Span(1, Millisecond)))
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (1388)
      }

      it("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {

        val reFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new RuntimeException("oops"))
          promise.future
        }
        val caught =
          intercept[TestFailedException] {
            reFuture.futureValue
          }
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be ("oops")
      }

      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {

        val vmeFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new VirtualMachineError {})
          promise.future
        }
        intercept[VirtualMachineError] {
          vmeFuture.futureValue
        }
      }

      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestPendingException))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestPendingException
          }
        intercept[TestPendingException] {
          tpeFuture.futureValue
        }
      }

      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestCanceledException(0)))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestCanceledException(0)
          }
        intercept[TestCanceledException] {
          tpeFuture.futureValue
        }
      }
    }

    describe("when using the whenReady construct") {

      it("should just return if the function arg returns normally") {
        val futureIsNow = alreadySucceededFuture
        whenReady(futureIsNow) { s =>
          s should equal ("hi")
        }
      }

      it("should return the last value if the function arg returns normally") {
        val futureIsNow = alreadySucceededFuture
        val result =
          whenReady(futureIsNow) { s =>
            s should equal ("hi")
            99
          }
        result should equal (99)
      }

      it("should, if the function arg completes abruptly with a TFE, complete abruptly with the same exception") {
        val futureIsNow = alreadySucceededFuture
        val caught =
          evaluating {
            whenReady(futureIsNow) { s =>
              s should equal ("ho")
            }
          } should produce [TestFailedException]
        caught.message.value should be ("\"h[i]\" did not equal \"h[o]\"")
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      it("should, if the function arg completes abruptly with a non-stack depth exception, complete abruptly with the same exception") {
        val futureIsNow = alreadySucceededFuture
        val caught =
          evaluating {
            whenReady(futureIsNow) { s =>
              s should equal ("hi")
              throw new RuntimeException("oops")
            }
          } should produce [RuntimeException]
        caught.getMessage should be ("oops")
      }

      it("should query the future just once if the future is ready the first time") {
        var count = 0
        val countingFuture = newAlreadySucceededCountingFuture { count += 1 }
        whenReady(countingFuture) { s =>
          s should equal ("hi")
        }
        count should equal (1)
      }

      it("should query the future five times if the future is not ready four times before finally being ready the fifth time") {
        var count = 0
        val countingFuture =
          new FutureOfScala[String] {
            var gotToFive = false
            def isCompleted = gotToFive
            def onComplete[U](func: Try[String] => U)(implicit executor: ExecutionContext): Unit = {}
            def value: Option[Try[String]] = {
              count += 1
              if (count < 5) None 
              else {
                gotToFive = true
                Some(Success("hi"))
              }
            }

            @throws(classOf[Exception])
            def result(atMost: Duration)(implicit permit: CanAwait): String = neverReadyFuture.result(atMost)

            @throws(classOf[TimeoutException])
            @throws(classOf[InterruptedException])
            def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
              neverReadyFuture.ready(atMost)
              this
            }
          }
        whenReady(countingFuture) { s =>
          s should equal ("hi")
        }
        count should equal (5)
      }

      it("should eventually blow up with a TFE if the future is never ready") {

        var count = 0
        val neverReadyCountingFuture = newNeverReadyCountingFuture { count += 1 }
        val caught = evaluating {
          whenReady(neverReadyCountingFuture) { s =>
            s should equal ("hi")
          }
        } should produce [TestFailedException]

        caught.message.value should be (Resources("wasNeverReady", count.toString, "15 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 6)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      it("should provides correct stack depth") {
        val caught1 = evaluating {
          whenReady(neverReadyFuture, timeout(Span(100, Millis)), interval(Span(1, Millisecond))) { s => s should equal ("hi") }
        } should produce [TestFailedException]
        caught1.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught1.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught3 = evaluating {
         whenReady(neverReadyFuture, timeout(Span(100, Millis))) {  s => s should equal ("hi") }
        } should produce [TestFailedException]
        caught3.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught3.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught4 = evaluating {
          whenReady(neverReadyFuture, interval(Span(1, Millisecond))) { s => s should equal ("hi")  }
        } should produce [TestFailedException]
        caught4.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught4.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      it("should by default query a never-ready future for at least 1 second") {
        var startTime = System.currentTimeMillis
        evaluating {
          whenReady(neverReadyFuture) { s =>
            s should equal ("hi")
          }
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (150)
      }

      it("should, if an alternate implicit Timeout is provided, query a never-ready by at least the specified timeout") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(1500, Millis))

        var startTime = System.currentTimeMillis
        evaluating {
          whenReady(neverReadyFuture) { s =>
            s should equal ("hi")
          }
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (1500)
      }

      it("should, if an alternate explicit timeout is provided, query a never-ready future by at least the specified timeout") {
        var startTime = System.currentTimeMillis
        evaluating {
          whenReady(neverReadyFuture, timeout(Span(1250, Milliseconds))) { s =>
            s should equal ("hi")
          }
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (1250)
      }

      it("should, if an alternate explicit timeout is provided along with an explicit interval, query a never-ready future by at least the specified timeout, even if a different implicit is provided") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(500, Millis), interval = Span(2, Millis))

        var startTime = System.currentTimeMillis
        evaluating {
          whenReady(neverReadyFuture, timeout(Span(1388, Millis)), interval(Span(1, Millisecond))) { s =>
            s should equal ("hi")
          }
        } should produce [TestFailedException]
        (System.currentTimeMillis - startTime).toInt should be >= (1388)
      }

      it("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {

        val reFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new RuntimeException("oops"))
          promise.future
        }
        val caught =
          intercept[TestFailedException] {
            whenReady(reFuture) { s =>
              s should equal ("hi")
            }
          }
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be ("oops")
      }

      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {

        val vmeFuture: FutureOfScala[String] = {
          val promise = Promise[String]
          promise.failure(new VirtualMachineError {})
          promise.future
        }
        intercept[VirtualMachineError] {
          whenReady(vmeFuture) { s =>
            s should equal ("hi")
          }
        }
      }

      // Same thing here and in 2.0 need to add a test for TestCanceledException
      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestPendingException))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestPendingException
          }
        intercept[TestPendingException] {
          whenReady(tpeFuture) { s =>
            s should equal ("hi")
          }
        }
      }
      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestCanceledException(0)))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestCanceledException(0)
          }
        intercept[TestCanceledException] {
          whenReady(tpeFuture) { s =>
            s should equal ("hi")
          }
        }
      }
    }
  }
}

