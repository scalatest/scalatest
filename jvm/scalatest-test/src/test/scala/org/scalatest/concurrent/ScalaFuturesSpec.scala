/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.OptionValues
import scala.concurrent.{Future => FutureOfScala}
import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import scala.concurrent.CanAwait
import scala.concurrent.ExecutionContext
// SKIP-SCALATESTJS-START
import java.io.{ObjectOutputStream, ByteArrayOutputStream}
// SKIP-SCALATESTJS-END
import java.util.concurrent.TimeoutException
import org.scalatest._
import time._
import exceptions.{TestCanceledException, TestFailedException, TestPendingException}
import util.Try
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ScalaFuturesSpec extends AnyFunSpec with Matchers with OptionValues with ScalaFutures {

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
        // These 2 ??? is needed to compile in 2.12, not really used though.
        def transform[S](f: Try[String] => Try[S])(implicit executor: ExecutionContext): FutureOfScala[S] = ???
        def transformWith[S](f: Try[String] => FutureOfScala[S])(implicit executor: ExecutionContext): FutureOfScala[S] = ???
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
        // These 2 ??? is needed to compile in 2.12, not really used though.
        def transform[S](f: Try[String] => Try[S])(implicit executor: ExecutionContext): FutureOfScala[S] = ???
        def transformWith[S](f: Try[String] => FutureOfScala[S])(implicit executor: ExecutionContext): FutureOfScala[S] = ???
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

      // SKIP-SCALATESTJS-START
      it("should query a never-ready future by at least the specified timeout") {
        var startTime = System.currentTimeMillis
        neverReadyFuture.isReadyWithin(Span(1250, Milliseconds)) should be (false)
        (System.currentTimeMillis - startTime).toInt should be >= (1250)
      }
      // SKIP-SCALATESTJS-END

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

      // SKIP-SCALATESTJS,NATIVE-START
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
      // SKIP-SCALATESTJS,NATIVE-END

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
        val caught = the [TestFailedException] thrownBy {
          neverReadyCountingFuture.futureValue
        }

        caught.message.value should be (Resources.wasNeverReady("150 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should eventually blow up with a serialized TestFailedException") {
        val objectOutputStream: ObjectOutputStream = new ObjectOutputStream(new ByteArrayOutputStream())
        val caught = the [TestFailedException] thrownBy {
          neverReadyFuture.futureValue
        }

        noException should be thrownBy objectOutputStream.writeObject(caught)
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should provide the correct stack depth") {
        val caught1 = the [TestFailedException] thrownBy {
          neverReadyFuture.futureValue(timeout(Span(100, Millis)), interval(Span(1, Millisecond)))
        }
        caught1.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught1.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught3 = the [TestFailedException] thrownBy {
          neverReadyFuture.futureValue(timeout(Span(100, Millis)))
        }
        caught3.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught3.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught4 = the [TestFailedException] thrownBy {
          neverReadyFuture.futureValue(interval(Span(1, Millisecond)))
        }
        caught4.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught4.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      // SKIP-SCALATESTJS-START
      it("should by default query a never-ready future for at least 1 second") {
        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          neverReadyFuture.futureValue
        }
        (System.currentTimeMillis - startTime).toInt should be >= (150)
      }

      it("should, if an alternate implicit Timeout is provided, query a never-ready by at least the specified timeout") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(1500, Millis))

        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          neverReadyFuture.futureValue
        }
        (System.currentTimeMillis - startTime).toInt should be >= (1500)
      }

      it("should, if an alternate explicit timeout is provided, query a never-ready future by at least the specified timeout") {
        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          neverReadyFuture.futureValue(timeout(Span(1250, Milliseconds)))
        }
        (System.currentTimeMillis - startTime).toInt should be >= (1250)
      }

      it("should, if an alternate explicit timeout is provided along with an explicit interval, query a never-ready future by at least the specified timeout, even if a different implicit is provided") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(500, Millis), interval = Span(2, Millis))

        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          neverReadyFuture.futureValue(timeout(Span(1388, Millis)), interval(Span(1, Millisecond)))
        }
        (System.currentTimeMillis - startTime).toInt should be >= (1388)
      }
      // SKIP-SCALATESTJS-END

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

      // SKIP-SCALATESTJS,NATIVE-START
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
      // SKIP-SCALATESTJS,NATIVE-END

      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture = FutureOfScala.failed(new TestPendingException)  
        intercept[TestPendingException] {
          tpeFuture.futureValue
        }
      }

      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture = FutureOfScala.failed(new TestCanceledException(0))  
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
          the [TestFailedException] thrownBy {
            whenReady(futureIsNow) { s =>
              s should equal ("ho")
            }
          }
        caught.message.value should be ("\"h[i]\" did not equal \"h[o]\"")
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      it("should, if the function arg completes abruptly with a non-stack depth exception, complete abruptly with the same exception") {
        val futureIsNow = alreadySucceededFuture
        val caught =
          the [RuntimeException] thrownBy {
            whenReady(futureIsNow) { s =>
              s should equal ("hi")
              throw new RuntimeException("oops")
            }
          }
        caught.getMessage should be ("oops")
      }

      it("should eventually blow up with a TFE if the future is never ready") {

        var count = 0
        val neverReadyCountingFuture = newNeverReadyCountingFuture { count += 1 }
        val caught = the [TestFailedException] thrownBy {
          whenReady(neverReadyCountingFuture) { s =>
            s should equal ("hi")
          }
        }

        caught.message.value should be (Resources.wasNeverReady("150 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 6)
        caught.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      it("should provides correct stack depth") {
        val caught1 = the [TestFailedException] thrownBy {
          whenReady(neverReadyFuture, timeout(Span(100, Millis)), interval(Span(1, Millisecond))) { s => s should equal ("hi") }
        }
        caught1.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught1.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught3 = the [TestFailedException] thrownBy {
         whenReady(neverReadyFuture, timeout(Span(100, Millis))) {  s => s should equal ("hi") }
        }
        caught3.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught3.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")

        val caught4 = the [TestFailedException] thrownBy {
          whenReady(neverReadyFuture, interval(Span(1, Millisecond))) { s => s should equal ("hi")  }
        }
        caught4.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught4.failedCodeFileName.value should be ("ScalaFuturesSpec.scala")
      }

      // SKIP-SCALATESTJS-START
      it("should by default query a never-ready future for at least 1 second") {
        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          whenReady(neverReadyFuture) { s =>
            s should equal ("hi")
          }
        }
        (System.currentTimeMillis - startTime).toInt should be >= (150)
      }

      it("should, if an alternate implicit Timeout is provided, query a never-ready by at least the specified timeout") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(1500, Millis))

        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          whenReady(neverReadyFuture) { s =>
            s should equal ("hi")
          }
        }
        (System.currentTimeMillis - startTime).toInt should be >= (1500)
      }

      it("should, if an alternate explicit timeout is provided, query a never-ready future by at least the specified timeout") {
        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          whenReady(neverReadyFuture, timeout(Span(1250, Milliseconds))) { s =>
            s should equal ("hi")
          }
        }
        (System.currentTimeMillis - startTime).toInt should be >= (1250)
      }

      it("should, if an alternate explicit timeout is provided along with an explicit interval, query a never-ready future by at least the specified timeout, even if a different implicit is provided") {
        implicit val patienceConfig = PatienceConfig(timeout = Span(500, Millis), interval = Span(2, Millis))

        var startTime = System.currentTimeMillis
        a [TestFailedException] should be thrownBy {
          whenReady(neverReadyFuture, timeout(Span(1388, Millis)), interval(Span(1, Millisecond))) { s =>
            s should equal ("hi")
          }
        }
        (System.currentTimeMillis - startTime).toInt should be >= (1388)
      }
      // SKIP-SCALATESTJS-END

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

      // SKIP-SCALATESTJS,NATIVE-START
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
      // SKIP-SCALATESTJS,NATIVE-END

      // Same thing here and in 2.0 need to add a test for TestCanceledException
      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val promise = Promise[String]
        promise.failure(new TestPendingException)
        val tpeFuture = promise.future  
        intercept[TestPendingException] {
          whenReady(tpeFuture) { s =>
            s should equal ("hi")
          }
        }
      }
      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val promise = Promise[String]
        promise.failure(new TestCanceledException(0))
        val tpeFuture = promise.future  
        intercept[TestCanceledException] {
          whenReady(tpeFuture) { s =>
            s should equal ("hi")
          }
        }
      }
    }
  }
}

