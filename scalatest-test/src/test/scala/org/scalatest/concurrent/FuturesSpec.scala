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

import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.OptionValues
import org.scalatest.FunSpec
import java.util.concurrent.{Future => FutureOfJava}
import java.util.concurrent.TimeUnit
import org.scalatest._
import time._
import exceptions.{TestCanceledException, TestFailedException, TestPendingException}
import org.scalactic.source

class FuturesSpec extends FunSpec with Matchers with OptionValues with Futures with SeveredStackTraces {

  import scala.language.implicitConversions

  // SKIP-SCALATESTJS-START
  implicit def convertJavaFuture[T](javaFuture: FutureOfJava[T]): FutureConcept[T] =
    new FutureConcept[T] {
      def eitherValue: Option[Either[Throwable, T]] =
        if (javaFuture.isDone())
          Some(Right(javaFuture.get))
        else
          None
      def isExpired: Boolean = false // Java Futures don't support the notion of a timeout
      def isCanceled: Boolean = javaFuture.isCancelled // Two ll's in Canceled. The verbosity of Java strikes again!
      // This one doesn't override futureResult, so that I can test the polling code
    }
  // SKIP-SCALATESTJS-END

  describe("A FutureConcept") {

    // SKIP-SCALATESTJS-START
    class SuperFutureOfJava extends FutureOfJava[String] {
      def cancel(mayInterruptIfRunning: Boolean): Boolean = false
      def get: String = "hi"
      def get(timeout: Long, unit: TimeUnit): String = "hi"
      def isCancelled: Boolean = false
      def isDone: Boolean = true
    }
    // SKIP-SCALATESTJS-END

    describe("when using the isReadyWithin method") {
      // SKIP-SCALATESTJS-START
      it("should just return the result if the future completes normally") {
        val futureIsNow = new SuperFutureOfJava
        futureIsNow.isReadyWithin(Span(1, Second)) should be (true)
      }

      it("should throw TFE with appropriate detail message if the future is canceled") {
        val canceledFuture =
          new SuperFutureOfJava {
            override def isCancelled = true
          }
        val caught = the [TestFailedException] thrownBy {
          canceledFuture.isReadyWithin(Span(1, Second))
        }
        caught.message.value should be (Resources.futureWasCanceled)
        withClue(caught.getStackTraceString) {
          caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        }
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }
      // SKIP-SCALATESTJS-END

      it("should throw TFE with appropriate detail message if the future expires") {
        val expiredFuture =
          new FutureConcept[Int] {
            def eitherValue = Some(Right(99))
            def isCanceled = false
            def isExpired = true
            def awaitAtMost(span: Span) = 99
          }
        val caught = the [TestFailedException] thrownBy {
          expiredFuture.isReadyWithin(Span(1, Second))
        }
        caught.message.value should be (Resources.futureExpired("1", "15 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 3)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

      // SKIP-SCALATESTJS-START
      val neverReadyFuture =
        new SuperFutureOfJava {
          override def isDone = false
        }

      it("should query a never-ready future by at least the specified timeout") {
        var startTime = System.currentTimeMillis
        neverReadyFuture.isReadyWithin(Span(1250, Milliseconds)) should be (false)
        (System.currentTimeMillis - startTime).toInt should be >= (1250)
      }
      // SKIP-SCALATESTJS-END

      it("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {

        val vmeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new RuntimeException("oops")))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new RuntimeException("oops")
          }
        val caught =
          intercept[TestFailedException] {
            vmeFuture.isReadyWithin(Span(1, Millisecond))
          }
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be ("oops")
      }

      // SKIP-SCALATESTJS-START
      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {

        val vmeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new VirtualMachineError {}))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new VirtualMachineError {}
          }
        intercept[VirtualMachineError] {
          vmeFuture.isReadyWithin(Span(1, Millisecond))
        }
      }
      // SKIP-SCALATESTJS-END

      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestPendingException))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestPendingException
          }
        intercept[TestPendingException] {
          tpeFuture.isReadyWithin(Span(1, Millisecond))
        }
      }

      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val tpeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestCanceledException(Some(source.Position.here), 0)))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestCanceledException(Some(source.Position.here), 0)
          }
        intercept[TestCanceledException] {
          tpeFuture.isReadyWithin(Span(1, Millisecond))
        }
      }
    }

    describe("when using the futureValue method") {

      // SKIP-SCALATESTJS-START
      it("should just return the result if the future completes normally") {
        val futureIsNow = new SuperFutureOfJava
        futureIsNow.futureValue should equal ("hi")
      }

      it("should throw TFE with appropriate detail message if the future is canceled") {
        val canceledFuture =
          new SuperFutureOfJava {
            override def isCancelled = true
          }
        val caught = the [TestFailedException] thrownBy {
          canceledFuture.futureValue
        }
        caught.message.value should be (Resources.futureWasCanceled)
        withClue(caught.getStackTraceString) {
          caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        }
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }
      // SKIP-SCALATESTJS-END

      it("should throw TFE with appropriate detail message if the future expires") {
        val expiredFuture =
          new FutureConcept[Int] {
            def eitherValue = Some(Right(99))
            def isCanceled = false
            def isExpired = true
            def awaitAtMost(span: Span) = 99
          }
        val caught = the [TestFailedException] thrownBy {
          expiredFuture.futureValue
        }
        caught.message.value should be (Resources.futureExpired("1", "15 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 3)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

      // SKIP-SCALATESTJS-START
      it("should eventually blow up with a TFE if the future is never ready") {

        var count = 0
        val neverReadyCountingFuture =
          new SuperFutureOfJava {
            override def isDone = {
              count += 1
              false
            }
          }
        val caught = the [TestFailedException] thrownBy {
          neverReadyCountingFuture.futureValue
        }

        caught.message.value should be (Resources.wasNeverReady(count.toString, "15 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

      val neverReadyFuture =
        new SuperFutureOfJava {
          override def isDone = false
        }

      it("should provides correct stack depth") {
        val caught1 = the [TestFailedException] thrownBy {
          neverReadyFuture.futureValue(timeout(Span(100, Millis)), interval(Span(1, Millisecond)))
        }
        caught1.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught1.failedCodeFileName.value should be ("FuturesSpec.scala")

        val caught3 = the [TestFailedException] thrownBy {
          neverReadyFuture.futureValue(timeout(Span(100, Millis)))
        }
        caught3.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught3.failedCodeFileName.value should be ("FuturesSpec.scala")

        val caught4 = the [TestFailedException] thrownBy {
          neverReadyFuture.futureValue(interval(Span(1, Millisecond)))
        }
        caught4.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught4.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

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

        val vmeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new RuntimeException("oops")))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new RuntimeException("oops")
          }
        val caught =
          intercept[TestFailedException] {
            vmeFuture.futureValue
          }
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be ("oops")
      }

      // SKIP-SCALATESTJS-START
      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {

        val vmeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new VirtualMachineError {}))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new VirtualMachineError {}
          }
        intercept[VirtualMachineError] {
          vmeFuture.futureValue
        }
      }
      // SKIP-SCALATESTJS-END

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
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestCanceledException(Some(source.Position.here), 0)))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestCanceledException(Some(source.Position.here), 0)
          }
        intercept[TestCanceledException] {
          tpeFuture.futureValue
        }
      }
    }

    describe("when using the whenReady construct") {

      // SKIP-SCALATESTJS-START
      class SuperFutureOfJava extends FutureOfJava[String] {
          def cancel(mayInterruptIfRunning: Boolean): Boolean = false
          def get: String = "hi"
          def get(timeout: Long, unit: TimeUnit): String = "hi"
          def isCancelled: Boolean = false
          def isDone: Boolean = true
        }

      it("should just return if the function arg returns normally") {
        val futureIsNow = new SuperFutureOfJava
        whenReady(futureIsNow) { s =>
          s should equal ("hi")
        }
      }

      it("should return the last value if the function arg returns normally") {
        val futureIsNow = new SuperFutureOfJava
        val result =
          whenReady(futureIsNow) { s =>
            s should equal ("hi")
            99
          }
        result should equal (99)
      }

      it("should, if the function arg completes abruptly with a TFE, complete abruptly with the same exception") {
        val futureIsNow = new SuperFutureOfJava
        val caught =
          the [TestFailedException] thrownBy {
            whenReady(futureIsNow) { s =>
              s should equal ("ho")
            }
          }
        caught.message.value should be ("\"h[i]\" did not equal \"h[o]\"")
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

      it("should, if the function arg completes abruptly with a non-stack depth exception, complete abruptly with the same exception") {
        val futureIsNow = new SuperFutureOfJava
        val caught =
          the [RuntimeException] thrownBy {
            whenReady(futureIsNow) { s =>
              s should equal ("hi")
              throw new RuntimeException("oops")
            }
          }
        caught.getMessage should be ("oops")
      }

      it("should query the future just once if the future is ready the first time") {
        var count = 0
        val countingFuture =
          new SuperFutureOfJava {
            override def isDone = {
              count += 1
              true
            }
        }
        whenReady(countingFuture) { s =>
          s should equal ("hi")
        }
        count should equal (1)
      }

      it("should query the future five times if the future is not ready four times before finally being ready the fifth time") {
        var count = 0
        val countingFuture =
          new SuperFutureOfJava {
            override def isDone = {
              count += 1
              count >= 5
            }
        }
        whenReady(countingFuture) { s =>
          s should equal ("hi")
        }
        count should equal (5)
      }
  // TODO: tests for isDropped and isExpired
      it("should throw TFE with appropriate detail message if the future is canceled") {
        val canceledFuture =
          new SuperFutureOfJava {
            override def isCancelled = true
          }
        val caught = the [TestFailedException] thrownBy {
          whenReady(canceledFuture) { s =>
            s should equal ("hi")
          }
        }
        caught.message.value should be (Resources.futureWasCanceled)
        withClue(caught.getStackTraceString) {
          caught.failedCodeLineNumber.value should equal (thisLineNumber - 6)
        }
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }
      // SKIP-SCALATESTJS-END

      it("should throw TFE with appropriate detail message if the future expires") {
        val expiredFuture =
          new FutureConcept[Int] {
            def eitherValue = Some(Right(99))
            def isCanceled = false
            def isExpired = true
            def awaitAtMost(span: Span) = 99
          }
        val caught = the [TestFailedException] thrownBy {
          whenReady(expiredFuture) { s =>
            s should equal (99)
          }
        }
        caught.message.value should be (Resources.futureExpired("1", "15 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 5)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

      // SKIP-SCALATESTJS-START
      it("should eventually blow up with a TFE if the future is never ready") {

        var count = 0
        val neverReadyCountingFuture =
          new SuperFutureOfJava {
            override def isDone = {
              count += 1
              false
            }
          }
        val caught = the [TestFailedException] thrownBy {
          whenReady(neverReadyCountingFuture) { s =>
            s should equal ("hi")
          }
        }

        caught.message.value should be (Resources.wasNeverReady(count.toString, "15 milliseconds"))
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 6)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

      val neverReadyFuture =
        new SuperFutureOfJava {
          override def isDone = false
        }

      it("should provides correct stack depth") {
        val caught1 = the [TestFailedException] thrownBy {
          whenReady(neverReadyFuture, timeout(Span(100, Millis)), interval(Span(1, Millisecond))) { s => s should equal ("hi") }
        }
        caught1.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught1.failedCodeFileName.value should be ("FuturesSpec.scala")

        val caught3 = the [TestFailedException] thrownBy {
         whenReady(neverReadyFuture, timeout(Span(100, Millis))) {  s => s should equal ("hi") }
        }
        caught3.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught3.failedCodeFileName.value should be ("FuturesSpec.scala")

        val caught4 = the [TestFailedException] thrownBy {
          whenReady(neverReadyFuture, interval(Span(1, Millisecond))) { s => s should equal ("hi")  }
        }
        caught4.failedCodeLineNumber.value should equal (thisLineNumber - 2)
        caught4.failedCodeFileName.value should be ("FuturesSpec.scala")
      }

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

        val vmeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new RuntimeException("oops")))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new RuntimeException("oops")
          }
        val caught =
          intercept[TestFailedException] {
            whenReady(vmeFuture) { s =>
              s should equal ("hi")
            }
          }
        caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
        caught.failedCodeFileName.value should be ("FuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be ("oops")
      }

      // SKIP-SCALATESTJS-START
      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {

        val vmeFuture =
          new FutureConcept[String] {
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new VirtualMachineError {}))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new VirtualMachineError {}
          }
        intercept[VirtualMachineError] {
          whenReady(vmeFuture) { s =>
            s should equal ("hi")
          }
        }
      }
      // SKIP-SCALATESTJS-END

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
            def eitherValue: Option[Either[Throwable, String]] = Some(Left(new TestCanceledException(Some(source.Position.here), 0)))
            def isExpired: Boolean = false
            def isCanceled: Boolean = false
            def awaitAtMost(span: Span): String = throw new TestCanceledException(Some(source.Position.here), 0)
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

