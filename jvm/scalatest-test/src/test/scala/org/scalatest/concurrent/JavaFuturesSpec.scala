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
package org.scalatest.concurrent

import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest._
import time._
import java.util.concurrent.{ExecutionException, Callable, ExecutorService, Executors, Future => FutureOfJava, TimeUnit, FutureTask}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.StackDepthException
import org.scalactic.source
import System.{lineSeparator => EOL}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Flicker

class JavaFuturesSpec extends AnyFunSpec with Matchers with OptionValues with JavaFutures with SeveredStackTraces {

  // This one tests the real JavaFutures trait, so it doesn't poll, which means some of the tests I used
  // to test the polling code in the default implementation of awaitResult in FutureConcept don't work.

  class Sleeper(napTime: Span) extends Callable[String] {
    def call(): String = {
      Thread.sleep(napTime.millisPart, napTime.nanosPart)
      "hi"
    }
  }
  class Sleeper2(napTime: Long) extends Callable[String] {
    def call(): String = {
      Thread.sleep(napTime)
      "hi"
    }
  }

  class CallableTask(callable: Callable[String]) extends FutureTask[String](callable)

  class ThrowingTask(throwable: Throwable) extends FutureTask[String](new Sleeper(Span(1, Second))) {
    setException(throwable)
  }

  describe("With a java.util.concurrent.Future") {

    describe("when using the isReadyWithin method") {

      it("can be queried to make sure it is ready within a certain time span", Flicker) {
        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val futureIsNow = execSvc.submit(new Sleeper(Span(0, Millis)))
          assert(futureIsNow.isReadyWithin(Span(1, Millisecond)))
        }
        finally {
          execSvc.shutdown()
        }
      }

      it("should throw TFE from isReadyWithin with appropriate detail message if the future is canceled") {
        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(1, Second)))
          val canceledFuture = execSvc.submit(task)
          canceledFuture.cancel(true)
          val caught = the [TestFailedException] thrownBy {
            canceledFuture.isReadyWithin(Span(1, Millisecond))
          }
          caught.message.value should be(Resources.futureWasCanceled)
          withClue(caught.getStackTrace().mkString("", EOL, EOL)) {
            caught.failedCodeLineNumber.value should equal(thisLineNumber - 4)
          }
          caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        }
        finally {
          execSvc.shutdown()
        }
      }
      it("should eventually return false if the future is never ready") {

        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(2, Seconds)))
          val neverReadyFuture = execSvc.submit(task)
          neverReadyFuture.isReadyWithin(Span(1, Millisecond)) should be (false)
        }
        finally {
          execSvc.shutdown()
        }
      }

      it("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {

        val task = new ThrowingTask(new RuntimeException("oops"))
        val caught =
          intercept[TestFailedException] {
            task.isReadyWithin(Span(1, Millisecond))
          }
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 2)
        caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be("oops")
      }

      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {
        // Wrong, should just go up
        val task = new ThrowingTask(new VirtualMachineError {})
        val caught =
          intercept[VirtualMachineError] {
            task.isReadyWithin(Span(1, Millisecond))
          }
      }

      // Same thing here and in 2.0 need to add a test for TestCanceledException
      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val task = new ThrowingTask(new TestPendingException)
        intercept[TestPendingException] {
          task.isReadyWithin(Span(1, Millisecond))
        }
      }
    }

    describe("when using the awaitResult method") {

      it("should just return the result if the future completes normally") {

        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val futureIsNow = execSvc.submit(new Sleeper(Span(0, Millis)))

          futureIsNow.futureValue should equal("hi")
        }
        finally {
          execSvc.shutdown()
        }
      }

      // Note: Can't test isExpired, because the Java future does not have an expiration concept
      it("should throw TFE with appropriate detail message if the future is canceled") {
        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(1, Second)))
          val canceledFuture = execSvc.submit(task)
          canceledFuture.cancel(true)
          val caught = the [TestFailedException] thrownBy {
            canceledFuture.futureValue
          }
          caught.message.value should be(Resources.futureWasCanceled)
          withClue(caught.getStackTrace().mkString("", EOL, EOL)) {
            caught.failedCodeLineNumber.value should equal(thisLineNumber - 4)
          }
          caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        }
        finally {
          execSvc.shutdown()
        }
      }

      it("should eventually blow up with a TFE if the future is never ready", Flicker) {

        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(2, Seconds)))
          val neverReadyFuture = execSvc.submit(task)
          val caught = the [TestFailedException] thrownBy {
            neverReadyFuture.futureValue
          }

          caught.message.value should be(Resources.wasNeverReady(1, UnquotedString("15 milliseconds")))
          caught.failedCodeLineNumber.value should equal(thisLineNumber - 4)
          caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        }
        finally {
          execSvc.shutdown()
        }
      }

      it("should provide correct stack depth") {

        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(2, Seconds)))
          def neverReadyFuture = execSvc.submit(task)
          val caught1 = the [TestFailedException] thrownBy {
            neverReadyFuture.futureValue(timeout(Span(100, Millis)), interval(Span(1, Millisecond)))
          }
          caught1.failedCodeLineNumber.value should equal(thisLineNumber - 2)
          caught1.failedCodeFileName.value should be("JavaFuturesSpec.scala")

          val caught3 = the [TestFailedException] thrownBy {
            neverReadyFuture.futureValue(timeout(Span(100, Millis)))
          }
          caught3.failedCodeLineNumber.value should equal(thisLineNumber - 2)
          caught3.failedCodeFileName.value should be("JavaFuturesSpec.scala")

          val caught4 = the [TestFailedException] thrownBy {
            neverReadyFuture.futureValue(interval(Span(1, Millisecond)))
          }
          caught4.failedCodeLineNumber.value should equal(thisLineNumber - 2)
          caught4.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        }
        finally {
          execSvc.shutdown()
        }
      }

      it("should by default query a never-ready future for at least 1 second") {
        var startTime = System.currentTimeMillis
        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(2, Seconds)))
          val neverReadyFuture = execSvc.submit(task)
          a [TestFailedException] should be thrownBy {
            neverReadyFuture.futureValue
          }
          (System.currentTimeMillis - startTime).toInt should be >= (150)
        }
        finally {
          execSvc.shutdown()
        }
      }

      it("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {

        val task = new ThrowingTask(new RuntimeException("oops"))
        val caught =
          intercept[TestFailedException] {
            task.futureValue
          }
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 2)
        caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be("oops")
      }

      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {
        // Wrong, should just go up
        val task = new ThrowingTask(new VirtualMachineError {})
        val caught =
          intercept[VirtualMachineError] {
            task.futureValue
          }
      }

      // Same thing here and in 2.0 need to add a test for TestCanceledException
      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val task = new ThrowingTask(new TestPendingException)
          intercept[TestPendingException] {
            task.futureValue
          }
      }
    }
    
    describe("when using the whenReady construct") {
      
      it("should just return if the function arg returns normally") {

        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val futureIsNow = execSvc.submit(new Sleeper(Span(0, Millis)))
          whenReady(futureIsNow) { s =>
            s should equal("hi")
          }
        }
        finally {
          execSvc.shutdown()
        }
      }

      // Note: Can't test isExpired, because the Java future does not have an expiration concept
      ignore("should throw TFE with appropriate detail message if the future is canceled") {
        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(1, Second)))
          val canceledFuture = execSvc.submit(task)
          canceledFuture.cancel(true)
          val caught = the [TestFailedException] thrownBy {
            whenReady(canceledFuture) { s =>
              s should equal ("hi")
            }
          }
          caught.message.value should be(Resources.futureWasCanceled)
          withClue(caught.getStackTrace().mkString("", EOL, EOL)) {
            caught.failedCodeLineNumber.value should equal(thisLineNumber - 6)
          }
          caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        }
        finally {
          execSvc.shutdown()
        }
      }

      ignore("should eventually blow up with a TFE if the future is never ready") {

        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(2, Seconds)))
          val neverReadyFuture = execSvc.submit(task)
          val caught = the [TestFailedException] thrownBy {
            whenReady(neverReadyFuture) { s =>
              s should equal ("hi")
            }
          }

          caught.message.value should be(Resources.wasNeverReady(1, UnquotedString("15 milliseconds")))
          caught.failedCodeLineNumber.value should equal(thisLineNumber - 6)
          caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        }
        finally {
          execSvc.shutdown()
        }
      }

      ignore("should provide correct stack depth") {

        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(2, Seconds)))
          def neverReadyFuture = execSvc.submit(task)
          val caught1 = the [TestFailedException] thrownBy {
            whenReady(neverReadyFuture, timeout(Span(100, Millis)), interval(Span(1, Millisecond))) { s =>
              s should be ("hi")
            }
          }
          caught1.failedCodeLineNumber.value should equal(thisLineNumber - 4)
          caught1.failedCodeFileName.value should be("JavaFuturesSpec.scala")

          val caught3 = the [TestFailedException] thrownBy {
            whenReady(neverReadyFuture, timeout(Span(100, Millis))) { s =>
              s should be ("hi")
            }
          }
          caught3.failedCodeLineNumber.value should equal(thisLineNumber - 4)
          caught3.failedCodeFileName.value should be("JavaFuturesSpec.scala")

          val caught4 = the [TestFailedException] thrownBy {
            whenReady(neverReadyFuture, interval(Span(1, Millisecond))) { s =>
              s should be ("hi")
            }
          }
          caught4.failedCodeLineNumber.value should equal(thisLineNumber - 4)
          caught4.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        }
        finally {
          execSvc.shutdown()
        }
      }

      it("should by default query a never-ready future for at least 1 second") {
        var startTime = System.currentTimeMillis
        val execSvc: ExecutorService = Executors.newFixedThreadPool(1)
        try {
          val task = new CallableTask(new Sleeper(Span(2, Seconds)))
          val neverReadyFuture = execSvc.submit(task)
          a [TestFailedException] should be thrownBy {
            whenReady(neverReadyFuture) { s =>
              s should be ("hi")
            }
          }
          (System.currentTimeMillis - startTime).toInt should be >= (150)
        }
        finally {
          execSvc.shutdown()
        }
      }

      ignore("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {

        val task = new ThrowingTask(new RuntimeException("oops"))
        val caught =
          intercept[TestFailedException] {
            whenReady(task) { s =>
              s should be ("hi")
            }
          }
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 4)
        caught.failedCodeFileName.value should be("JavaFuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be("oops")
      }

      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {
        // Wrong, should just go up
        val task = new ThrowingTask(new VirtualMachineError {})
        val caught =
          intercept[VirtualMachineError] {
            whenReady(task) { s =>
              s should be ("hi")
            }
          }
      }

      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val task = new ThrowingTask(new TestPendingException)
        intercept[TestPendingException] {
          whenReady(task) { s =>
            s should be ("hi")
          }
        }
      }
      
      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val task = new ThrowingTask(new TestCanceledException((_: StackDepthException) => None, None, Left(source.Position.here), None))
        intercept[TestCanceledException] {
          whenReady(task) { s =>
            s should be ("hi")
          }
        }
      }
    }
  }
}

