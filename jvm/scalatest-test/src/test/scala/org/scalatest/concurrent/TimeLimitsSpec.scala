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

import org.scalatest.OptionValues._
import TimeLimits._
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
import org.scalatest.exceptions.{TestPendingException, TestFailedException, TestCanceledException}
import org.scalatest.time._
import org.scalatest._
import org.scalatest.exceptions.{TestPendingException, TestFailedException, TestCanceledException}
import org.scalatest.tagobjects.Retryable
import org.scalatest.tagobjects.Flicker
import scala.concurrent.Future

import scala.util.{Try, Success, Failure}
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers

class TimeLimitsSpec extends AsyncFunSpec with Matchers {

/*
  override def withFixture(test: NoArgTest) = {
    if (isRetryable(test))
      withRetry { super.withFixture(test) }
    else
      super.withFixture(test)
  }
*/

  describe("The failAfter construct") {

    describe("when work with T") {

      it("should blow up with TestFailedException when it times out", Retryable) {
        val caught = the[TestFailedException] thrownBy {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
          }
        }
        caught.message.value should be(Resources.timeoutFailedAfter("100 milliseconds"))
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 6)
      }

      it("should pass normally when the timeout is not reached", Retryable, Flicker) {
        failAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
        }
        succeed
      }


      // SKIP-SCALATESTJS,NATIVE-START
      it("should blow up with TestFailedException when the task does not response interrupt request and pass after the timeout") {
        a[TestFailedException] should be thrownBy {
          failAfter(timeout = Span(100, Millis)) {
            for (i <- 1 to 10) {
              try {
                SleepHelper.sleep(50)
              }
              catch {
                case _: InterruptedException =>
                  Thread.interrupted() // Swallow the interrupt
              }
            }
          }
        }
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should not catch exception thrown from the test") {
        an[InterruptedException] should be thrownBy {
          failAfter(Span(100, Millis)) {
            throw new InterruptedException
            true
          }
        }
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should set the exception thrown from the test after timeout as cause of TestFailedException") {
        val caught = the[TestFailedException] thrownBy {
          failAfter(Span(100, Millis)) {
            for (i <- 1 to 10) {
              try {
                SleepHelper.sleep(50)
              }
              catch {
                case _: InterruptedException =>
                  Thread.interrupted() // Swallow the interrupt
              }
            }
            throw new IllegalArgumentException("Something went wrong!")
            true
          }
        }
        assert(caught.getCause().getClass === classOf[IllegalArgumentException])
      }

      it("should close a Socket connection via SocketSignaler when the timeout is reached") {
        val serverSocket = new ServerSocket(0)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            val clientSocket = serverSocket.accept()
            while (drag) {
              try {
                SleepHelper.sleep(100)
              }
              catch {
                case _: InterruptedException => Thread.interrupted()
              }
            }
            serverSocket.close()
          }
        }
        serverThread.start()
        val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
        val inputStream = clientSocket.getInputStream()

        a[TestFailedException] should be thrownBy {
          failAfter(Span(100, Millis)) {
            inputStream.read()
          }(SocketSignaler(clientSocket))
        }
        clientSocket.close()
        drag = false
        succeed
      }

      it("should close a Socket connection via FunSignaler when the timeout is reached") {
        val serverSocket = new ServerSocket(0)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            val clientSocket = serverSocket.accept()
            while (drag) {
              try {
                SleepHelper.sleep(100)
              }
              catch {
                case _: InterruptedException => Thread.interrupted()
              }
            }
            serverSocket.close()
          }
        }
        serverThread.start()
        val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
        val inputStream = clientSocket.getInputStream()

        a[TestFailedException] should be thrownBy {
          failAfter(Span(100, Millis)) {
            inputStream.read()
          }(Signaler { t => clientSocket.close() })
        }
        clientSocket.close()
        drag = false
        succeed
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should wait for the test to finish when DoNotSignal.") {
        var x = 0
        val caught = the[TestFailedException] thrownBy {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            x = 1
          }(DoNotSignal)
        }
        x should be(1)
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should close a Selector connection via SelectorSignaler when the timeout is reached") {
        val selector = Selector.open()
        val ssChannel = ServerSocketChannel.open()
        ssChannel.configureBlocking(false)
        ssChannel.socket().bind(new InetSocketAddress(0))
        ssChannel.register(selector, SelectionKey.OP_ACCEPT)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            selector.select()
            val it = selector.selectedKeys.iterator
            while (it.hasNext) {
              val selKey = it.next().asInstanceOf[SelectionKey]
              it.remove()
              if (selKey.isAcceptable()) {
                val ssChannel = selKey.channel().asInstanceOf[ServerSocketChannel]
                while (drag) {
                  try {
                    SleepHelper.sleep(100)
                  }
                  catch {
                    case _: InterruptedException => Thread.interrupted()
                  }
                }
              }
            }
            ssChannel.close()
          }
        }

        val clientSelector = Selector.open();
        val sChannel = SocketChannel.open()
        sChannel.configureBlocking(false);
        sChannel.connect(new InetSocketAddress("localhost", ssChannel.socket().getLocalPort()));
        sChannel.register(selector, sChannel.validOps());

        a[TestFailedException] should be thrownBy {
          failAfter(Span(100, Millis)) {
            clientSelector.select()
          }(SelectorSignaler(clientSelector))
        }
        clientSelector.close()
        drag = false
        succeed
      }
      // SKIP-SCALATESTJS,NATIVE-END

    }

    describe("when work with Future[T]") {

      it("should blow up with TestFailedException when it times out in main block that create the Future", Retryable) {
        val caught = the[TestFailedException] thrownBy {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            Future.successful(Success("test"))
          }
        }
        caught.message.value should be(Resources.timeoutFailedAfter("100 milliseconds"))
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestFailedException when it times out in the Future that gets returned", Retryable) {

        val future = recoverToExceptionIf[TestFailedException] { failAfter(Span(1000, Millis)) {
            Future {
              SleepHelper.sleep(2000)
              Succeeded
            }
          }
        }

        future map { tfe =>
          tfe.message.value should be(Resources.timeoutFailedAfter("1000 milliseconds"))
          tfe.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tfe.failedCodeLineNumber.value should equal(thisLineNumber - 11)
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future", Retryable, Flicker) {
        failAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          Future.successful(Success("test"))
        } map { r =>
          succeed
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future and in the future itself", Retryable, Flicker) {
        failAfter(Span(200, Millis)) {
          Future {
            SleepHelper.sleep(100)
            Success("test")
          }
        } map { r =>
          succeed
        }
      }

      it("should not catch exception thrown from the main block that create the future") {
        an[InterruptedException] should be thrownBy {
          failAfter(Span(100, Millis)) {
            throw new InterruptedException
            Future.successful(Success("test"))
          }
        }
      }

      it("should not catch exception thrown from the future block") {
        implicit val execContext = new SerialExecutionContext
        val future: Future[Outcome] =
        failAfter(Span(100, Millis)) {
          Future {
            throw new UnknownError // Scala.js 2.13.0 is catching InterruptedException, whereas JVM is not, so use a different "fatal" exception
            Succeeded
          }
        }

        assertThrows[UnknownError] {
          execContext.runNow(future)
        }
      }

      it("should wait for the test to finish when DoNotSignal.") {
        var x = 0
        val caught = the[TestFailedException] thrownBy {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            x = 1
            Success("test")
          }(DoNotSignal)
        }
        x should be (1)
      }
    }

    describe("when work with FutureOutcome") {

      it("should blow up with TestFailedException when it times out in main block that create the Future", Retryable) {
        val tfe = intercept[TestFailedException] {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            FutureOutcome(Future.successful(Succeeded))
          }
        }
        tfe.message.value should be(Resources.timeoutFailedAfter("100 milliseconds"))
        tfe.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        tfe.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestCanceledException when it blows up with TestCanceledException before times out in main block that create the Future", Retryable) {
        val caught = intercept[TestCanceledException] {
          failAfter(Span(100, Millis)) {
            cancel("cancel message")
            SleepHelper.sleep(200)
            FutureOutcome(Future.successful(Succeeded))
          }
        }
        caught.message.value should be ("cancel message")
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestCanceledException when it blows up with TestCanceledException after times out in main block that create the Future", Retryable) {
        val caught = the[TestCanceledException] thrownBy {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            cancel("cancel message")
            FutureOutcome(Future.successful(Succeeded))
          }
        }
        caught.message.value should be ("cancel message")
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 6)
      }

      it("should return Pending when it blows up with TestPendingException before times out in main block that create the Future", Retryable) {
        assertThrows[TestPendingException] {
          failAfter(Span(100, Millis)) {
            pending
            SleepHelper.sleep(200)
            FutureOutcome(Future.successful(Succeeded))
          }
        }
      }

      it("should blow up with TestPendingException when it blows up with TestPendingException after times out in main block that create the Future", Retryable) {
        assertThrows[TestPendingException] {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            pending
            FutureOutcome(Future.successful(Succeeded))
          }
        }
      }

      it("should blow up with TestFailedException when it times out in the Future that gets returned", Retryable) {
        val futureOutcome =
          failAfter(Span(1000, Millis)) {
            FutureOutcome(Future {
              SleepHelper.sleep(2000)
              Succeeded
            })
          }

        futureOutcome.toFuture map { outcome =>
          outcome shouldBe a [Failed]
          val failed = outcome.asInstanceOf[Failed]
          failed.exception shouldBe a [TestFailedException]
          val tfe = failed.exception.asInstanceOf[TestFailedException]
          tfe.message.value should be (Resources.timeoutFailedAfter("1000 milliseconds"))
          tfe.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tfe.failedCodeLineNumber.value should equal(thisLineNumber - 14)
        }
      }

      it("should return Failed with exception it throws from Future block before times out in the Future that gets returned", Retryable) {
        val futureOutcome =
          failAfter(Span(1000, Millis)) {
            FutureOutcome(Future {
              throw new RuntimeException("oops")
              Succeeded
            })
          }

        futureOutcome.toFuture map { outcome =>
          outcome shouldBe a [Failed]
          val failed = outcome.asInstanceOf[Failed]
          failed.exception shouldBe a [RuntimeException]
        }
      }

      it("should blows up with VirtualMachineError it throws from Future block before times out in the Future that gets returned", Retryable) {
        implicit val execContext = new SerialExecutionContext
        val future: FutureOutcome =
          failAfter(Span(100, Millis)) {
            FutureOutcome(Future {
              throw new VirtualMachineError {}
              Succeeded
            })
          }

        assertThrows[VirtualMachineError] {
          execContext.runNow(future.toFuture)
        }
      }

      it("should return Future[Canceled] when it blows up with TestCanceledException before times out in the Future that gets returned", Retryable) {
        try {
          val future: FutureOutcome =
            failAfter(Span(1000, Millis)) {
              FutureOutcome(Future {
                cancel("cancel message")
                SleepHelper.sleep(2000)
                Succeeded
              })
            }
          future.toFuture map { outcome =>
            outcome shouldBe a[Canceled]
            val tce = outcome.asInstanceOf[Canceled].exception
            tce.message.value should be ("cancel message")
            tce.failedCodeFileName.value should be("TimeLimitsSpec.scala")
            tce.failedCodeLineNumber.value should equal(thisLineNumber - 10)
          } recover {
            case tce: TestCanceledException => fail("Not suppose to get a TestCanceledException here, it should be translated to Canceled")
          }
        }
        catch {
          case tce: TestCanceledException => fail("Not suppose to get a TestCanceledException here, it should be translated to Canceled")
        }
      }

      it("should blow up with TestFailedException when TestCanceledException is thrown after times out in the Future that gets returned", Retryable) {
        val futureOutcome =
          failAfter(Span(1000, Millis)) {
            FutureOutcome(Future {
              SleepHelper.sleep(2000)
              cancel("cancel message")
              Succeeded
            })
          }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe a [Failed]
          val failed = outcome.asInstanceOf[Failed]
          failed.exception shouldBe a [TestFailedException]
          val tfe = failed.exception.asInstanceOf[TestFailedException]
          tfe.message.value should be (Resources.timeoutFailedAfter("1000 milliseconds"))
          tfe.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tfe.failedCodeLineNumber.value should equal(thisLineNumber - 14)
        }
      }

      it("should return Future[Pending] when it blows up with TestPendingException before times out in the Future that gets returned", Retryable) {
        try {
          val future: FutureOutcome =
            failAfter(Span(1000, Millis)) {
              FutureOutcome(Future {
                pending
                SleepHelper.sleep(2000)
                Succeeded
              })
            }
          future.toFuture map { outcome =>
            outcome shouldBe Pending
          } recover {
            case tce: TestPendingException => fail("Not suppose to get a TestPendingException here, it should be translated to Pending")
          }
        }
        catch {
          case tce: TestPendingException => fail("Not suppose to get a TestPendingException here, it should be translated to Pending")
        }
      }

      it("should blow up with TestFailedException when TestPendingException is thrown after times out in the Future that gets returned", Retryable) {
        val futureOutcome =
          failAfter(Span(1000, Millis)) {
            FutureOutcome(Future {
              SleepHelper.sleep(2000)
              pending
              Succeeded
            })
          }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe a [Failed]
          val failed = outcome.asInstanceOf[Failed]
          failed.exception shouldBe a [TestFailedException]
          val tfe = failed.exception.asInstanceOf[TestFailedException]
          tfe.message.value should be (Resources.timeoutFailedAfter("1000 milliseconds"))
          tfe.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tfe.failedCodeLineNumber.value should equal(thisLineNumber - 14)
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future", Retryable, Flicker) {
        val futureOutcome = failAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          FutureOutcome(Future.successful(Succeeded))
        }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe Succeeded
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future and in the future itself", Flicker) {
        val futureOutcome = failAfter(Span(200, Millis)) {
          FutureOutcome(Future {
            SleepHelper.sleep(100)
            Succeeded
          })
        }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe Succeeded
        }
      }

      it("should not catch exception thrown from the main block that create the future") {
        assertThrows[InterruptedException] {
          failAfter(Span(100, Millis)) {
            throw new InterruptedException
            FutureOutcome(Future.successful(Succeeded))
          }
        }
      }

      it("should not catch exception thrown from the future block") {
        implicit val execContext = new SerialExecutionContext
        val future: FutureOutcome =
          failAfter(Span(100, Millis)) {
            FutureOutcome(Future {
              throw new UnknownError // Scala.js 2.13.0 is catching InterruptedException, whereas JVM is not, so use a different "fatal" exception
              Succeeded
            })
          }

        assertThrows[UnknownError] {
          execContext.runNow(future.toFuture)
        }
      }

      it("should wait for the test to finish when DoNotSignal.") {
        var x = 0
        val caught = the[TestFailedException] thrownBy {
          failAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            x = 1
            FutureOutcome(Future.successful(Succeeded))
          }(DoNotSignal)
        }
        x should be (1)
      }
    }
  }

  describe("The cancelAfter construct") {

    describe("when work with T") {

      it("should blow up with TestCanceledException when it times out") {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(1000, Millis)) {
            SleepHelper.sleep(2000)
          }
        }
        caught.message.value should be(Resources.timeoutCanceledAfter("1000 milliseconds"))
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 5)
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
      }

      it("should pass normally when timeout is not reached") {
        cancelAfter(Span(2000, Millis)) {
          SleepHelper.sleep(1000)
        }
        succeed
      }

      it("should blow up with TestCanceledException when the task does not response interrupt request and pass after the timeout") {
        a[TestCanceledException] should be thrownBy {
          cancelAfter(timeout = Span(1000, Millis)) {
            for (i <- 1 to 10) {
              try {
                SleepHelper.sleep(500)
              }
              catch {
                case _: InterruptedException =>
                  Thread.interrupted() // Swallow the interrupt
              }
            }
          }
        }
      }

      it("should not catch exception thrown from the test") {
        an[InterruptedException] should be thrownBy {
          cancelAfter(Span(1000, Millis)) {
            throw new InterruptedException
            true
          }
        }
      }

      it("should set exception thrown from the test after timeout as cause of TestCanceledException") {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(1000, Millis)) {
            for (i <- 1 to 10) {
              try {
                SleepHelper.sleep(500)
              }
              catch {
                case _: InterruptedException =>
                  Thread.interrupted() // Swallow the interrupt
              }
            }
            throw new IllegalArgumentException("Something goes wrong!")
            true
          }
        }
        assert(caught.getCause().getClass === classOf[IllegalArgumentException])
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should close Socket connection via SocketSignaler when timeout reached") {
        val serverSocket = new ServerSocket(0)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            val clientSocket = serverSocket.accept()
            while (drag) {
              try {
                SleepHelper.sleep(1000)
              }
              catch {
                case _: InterruptedException => Thread.interrupted()
              }
            }
            serverSocket.close()
          }
        }
        serverThread.start()
        val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
        val inputStream = clientSocket.getInputStream()

        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(1000, Millis)) {
            inputStream.read()
          }(SocketSignaler(clientSocket))
        }
        clientSocket.close()
        drag = false
        succeed // TODO: Chee Seng, why is caught captured? It isn't used.
      }

      it("should close Socket connection via FunSignaler when timeout reached") {
        val serverSocket = new ServerSocket(0)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            val clientSocket = serverSocket.accept()
            while (drag) {
              try {
                SleepHelper.sleep(1000)
              }
              catch {
                case _: InterruptedException => Thread.interrupted()
              }
            }
            serverSocket.close()
          }
        }
        serverThread.start()
        val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
        val inputStream = clientSocket.getInputStream()

        a[TestCanceledException] should be thrownBy {
          cancelAfter(Span(1000, Millis)) {
            inputStream.read()
          }(Signaler { t => clientSocket.close() })
        }
        clientSocket.close()
        drag = false
        succeed
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should wait for the test to finish when DoNotSignal is used.") {
        var x = 0
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(1000, Millis)) {
            SleepHelper.sleep(2000)
            x = 1
          }(DoNotSignal)
        }
        x should be(1)
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should close Selector connection via SelectorSignaler when timeout reached") {
        val selector = Selector.open()
        val ssChannel = ServerSocketChannel.open()
        ssChannel.configureBlocking(false)
        ssChannel.socket().bind(new InetSocketAddress(0))
        ssChannel.register(selector, SelectionKey.OP_ACCEPT)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            selector.select()
            val it = selector.selectedKeys.iterator
            while (it.hasNext) {
              val selKey = it.next().asInstanceOf[SelectionKey]
              it.remove()
              if (selKey.isAcceptable()) {
                val ssChannel = selKey.channel().asInstanceOf[ServerSocketChannel]
                while (drag) {
                  try {
                    SleepHelper.sleep(1000)
                  }
                  catch {
                    case _: InterruptedException => Thread.interrupted()
                  }
                }
              }
            }
            ssChannel.close()
          }
        }

        val clientSelector = Selector.open();
        val sChannel = SocketChannel.open()
        sChannel.configureBlocking(false);
        sChannel.connect(new InetSocketAddress("localhost", ssChannel.socket().getLocalPort()));
        sChannel.register(selector, sChannel.validOps());

        a[TestCanceledException] should be thrownBy {
          cancelAfter(Span(1000, Millis)) {
            clientSelector.select()
          }(SelectorSignaler(clientSelector))
        }
        clientSelector.close()
        drag = false
        succeed
      }
      // SKIP-SCALATESTJS,NATIVE-END

    }

    describe("when work with Future[T]") {
      it("should blow up with TestCanceledException when it times out in main block that create the Future", Retryable) {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            Future.successful(Success("test"))
          }
        }
        caught.message.value should be(Resources.timeoutCanceledAfter("100 milliseconds"))
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestCanceledException when it times out in the Future that gets returned", Retryable) {

        val future = recoverToExceptionIf[TestCanceledException] { cancelAfter(Span(1000, Millis)) {
            Future {
              SleepHelper.sleep(2000)
              Succeeded
            }
          }
        }

        future map { tce =>
          tce.message.value should be(Resources.timeoutCanceledAfter("1000 milliseconds"))
          tce.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tce.failedCodeLineNumber.value should equal(thisLineNumber - 11)
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future", Flicker) {
        cancelAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          Future.successful(Success("test"))
        } map { r =>
          succeed
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future and in the future itself", Flicker) {
        cancelAfter(Span(200, Millis)) {
          Future {
            SleepHelper.sleep(100)
            Success("test")
          }
        } map { r =>
          succeed
        }
      }

      it("should not catch exception thrown from the main block that create the future") {
        an[InterruptedException] should be thrownBy {
          cancelAfter(Span(100, Millis)) {
            throw new InterruptedException
            Future.successful(Success("test"))
          }
        }
      }

      it("should not catch exception thrown from the future block") {
        implicit val execContext = new SerialExecutionContext
        val future: Future[Outcome] =
          cancelAfter(Span(100, Millis)) {
            Future {
              throw new UnknownError // Scala.js 2.13.0 is catching InterruptedException, whereas JVM is not, so use a different "fatal" exception
              Succeeded
            }
          }

        assertThrows[UnknownError] {
          execContext.runNow(future)
        }
      }

      it("should wait for the test to finish when DoNotSignal.") {
        var x = 0
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            x = 1
            Success("test")
          }(DoNotSignal)
        }
        x should be (1)
      }
    }

    describe("when work with FutureOutcome") {

      it("should blow up with TestCanceledException when it times out in main block that create the Future", Retryable) {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            FutureOutcome(Future.successful(Succeeded))
          }
        }
        caught.message.value should be(Resources.timeoutCanceledAfter("100 milliseconds"))
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestCanceledException when it blows up with TestCanceledException before times out in main block that create the Future", Retryable) {
        val caught = intercept[TestCanceledException] {
          cancelAfter(Span(100, Millis)) {
            cancel("cancel message")
            SleepHelper.sleep(200)
            FutureOutcome(Future.successful(Succeeded))
          }
        }
        caught.message.value should be ("cancel message")
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestCanceledException when it blows up with TestCanceledException after times out in main block that create the Future", Retryable) {
        val caught = intercept[TestCanceledException] {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            cancel("cancel message")
            FutureOutcome(Future.successful(Succeeded))
          }
        }
        caught.message.value should be ("cancel message")
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 6)
      }

      it("should blow up with TestPendingException when it blows up with TestPendingException before times out in main block that create the Future", Retryable) {
        assertThrows[TestPendingException] {
          cancelAfter(Span(100, Millis)) {
            pending
            SleepHelper.sleep(200)
            FutureOutcome(Future.successful(Succeeded))
          }
        }
      }

      it("should blow up with TestPendingException when it blows up with TestPendingException after times out in main block that create the Future", Retryable) {
        assertThrows[TestPendingException] {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            pending
            FutureOutcome(Future.successful(Succeeded))
          }
        }
      }

      it("should blow up with TestCanceledException when it times out in the Future that gets returned", Retryable) {
        val futureOutcome =
          cancelAfter(Span(1000, Millis)) {
            FutureOutcome(Future {
              SleepHelper.sleep(2000)
              Succeeded
            })
          }

        futureOutcome.toFuture map { outcome =>
          outcome shouldBe a [Canceled]
          val canceled = outcome.asInstanceOf[Canceled]
          canceled.exception shouldBe a [TestCanceledException]
          val tce = canceled.exception.asInstanceOf[TestCanceledException]
          tce.message.value should be(Resources.timeoutCanceledAfter("1000 milliseconds"))
          tce.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tce.failedCodeLineNumber.value should equal(thisLineNumber - 14)
        }
      }

      it("should return Future[Canceled] when it blows up with TestCanceledException before times out in the Future that gets returned", Retryable) {
        try {
          val future: FutureOutcome =
            cancelAfter(Span(1000, Millis)) {
              FutureOutcome(Future {
                cancel("cancel message")
                SleepHelper.sleep(2000)
                Succeeded
              })
            }
          future.toFuture map { outcome =>
            outcome shouldBe a[Canceled]
            val tce = outcome.asInstanceOf[Canceled].exception
            tce.message.value should be ("cancel message")
            tce.failedCodeFileName.value should be("TimeLimitsSpec.scala")
            tce.failedCodeLineNumber.value should equal(thisLineNumber - 10)
          } recover {
            case tce: TestCanceledException => fail("Not suppose to get a TestCanceledException here, it should be translated to Canceled")
          }
        }
        catch {
          case tce: TestCanceledException => fail("Not suppose to get a TestCanceledException here, it should be translated to Canceled")
        }
      }

      it("should blow up with TestCanceledException when TestCanceledException is thrown after times out in the Future that gets returned", Retryable) {
        val futureOutcome =
          cancelAfter(Span(1000, Millis)) {
            FutureOutcome(Future {
              SleepHelper.sleep(2000)
              cancel("cancel message")
              Succeeded
            })
          }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe a [Canceled]
          val canceled = outcome.asInstanceOf[Canceled]
          canceled.exception shouldBe a [TestCanceledException]
          val tce = canceled.exception.asInstanceOf[TestCanceledException]
          tce.message.value should be (Resources.timeoutCanceledAfter("1000 milliseconds"))
          tce.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tce.failedCodeLineNumber.value should equal(thisLineNumber - 14)
        }
      }

      it("should return Future[Pending] when it blows up with TestPendingException before times out in the Future that gets returned", Retryable) {
        try {
          val future: FutureOutcome =
            cancelAfter(Span(1000, Millis)) {
              FutureOutcome(Future {
                pending
                SleepHelper.sleep(2000)
                Succeeded
              })
            }
          future.toFuture map { outcome =>
            outcome shouldBe Pending
          } recover {
            case tce: TestPendingException => fail("Not suppose to get a TestPendingException here, it should be translated to Pending")
          }
        }
        catch {
          case tce: TestPendingException => fail("Not suppose to get a TestPendingException here, it should be translated to Pending")
        }
      }

      it("should blow up with TestCanceledException when TestPendingException is thrown after times out in the Future that gets returned", Retryable) {
        val futureOutcome =
          cancelAfter(Span(1000, Millis)) {
            FutureOutcome(Future {
              SleepHelper.sleep(2000)
              pending
              Succeeded
            })
          }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe a [Canceled]
          val canceled = outcome.asInstanceOf[Canceled]
          canceled.exception shouldBe a [TestCanceledException]
          val tce = canceled.exception.asInstanceOf[TestCanceledException]
          tce.message.value should be (Resources.timeoutCanceledAfter("1000 milliseconds"))
          tce.failedCodeFileName.value should be("TimeLimitsSpec.scala")
          tce.failedCodeLineNumber.value should equal(thisLineNumber - 14)
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future", Flicker) {
        val futureOutcome = cancelAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          FutureOutcome(Future.successful(Succeeded))
        }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe Succeeded
        }
      }

      it("should pass normally when the timeout is not reached in main block that create the future and in the future itself", Flicker) {
        val futureOutcome = cancelAfter(Span(2000, Millis)) {
          FutureOutcome(Future {
            SleepHelper.sleep(1000)
            Succeeded
          })
        }
        futureOutcome.toFuture map { outcome =>
          outcome shouldBe Succeeded
        }
      }

      it("should not catch exception thrown from the main block that create the future") {
        assertThrows[InterruptedException] {
          cancelAfter(Span(100, Millis)) {
            throw new InterruptedException
            FutureOutcome(Future.successful(Succeeded))
          }
        }
      }

      it("should not catch exception thrown from the future block") {
        implicit val execContext = new SerialExecutionContext
        val future: FutureOutcome =
          cancelAfter(Span(100, Millis)) {
            FutureOutcome(Future {
              throw new UnknownError // Scala.js 2.13.0 is catching InterruptedException, whereas JVM is not, so use a different "fatal" exception
              Succeeded
            })
          }

        assertThrows[UnknownError] {
          execContext.runNow(future.toFuture)
        }
      }

      it("should wait for the test to finish when DoNotSignal.") {
        var x = 0
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            x = 1
            FutureOutcome(Future.successful(Succeeded))
          }(DoNotSignal)
        }
        x should be (1)
      }
    }

    describe("when work with Outcome") {

      it("should blow up with TestCanceledException when it times out", Retryable) {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            Succeeded
          }
        }
        caught.message.value should be(Resources.timeoutCanceledAfter("100 milliseconds"))
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestCanceledException when TestCanceledException is thrown before it times out", Retryable) {
        val caught =
          intercept[TestCanceledException] {
            cancelAfter(Span(100, Millis)) {
              cancel("cancel message")
              SleepHelper.sleep(200)
              Succeeded
            }
          }
        caught.message.value should be("cancel message")
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 7)
      }

      it("should blow up with TestPendingException when TestPendingException is thrown before it times out", Retryable) {
        assertThrows[TestPendingException] {
          cancelAfter(Span(100, Millis)) {
            pending
            SleepHelper.sleep(200)
            Succeeded
          }
        }
      }

      it("should blow up with TestCanceledException when TestCanceledException is thrown after it times out", Retryable) {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            cancel("cancel message")
            Succeeded
          }
        }
        caught.message.value should be(Resources.timeoutCanceledAfter("100 milliseconds"))
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 8)
        caught.cause.value shouldBe a [TestCanceledException]
        val tce = caught.cause.value.asInstanceOf[TestCanceledException]
        tce.message.value should be("cancel message")
        tce.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        tce.failedCodeLineNumber.value should equal(thisLineNumber - 11)
      }

      it("should blow up with TestCanceledException when TestPendingException is thrown after it times out", Retryable) {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            pending
            Succeeded
          }
        }
        caught.message.value should be(Resources.timeoutCanceledAfter("100 milliseconds"))
        caught.failedCodeFileName.value should be("TimeLimitsSpec.scala")
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 8)
        caught.cause.value shouldBe a [TestPendingException]
      }

      it("should pass normally when the timeout is not reached", Flicker) {
        cancelAfter(Span(200, Millis)) {
          SleepHelper.sleep(100)
          Succeeded
        }
        succeed
      }


      // SKIP-SCALATESTJS,NATIVE-START
      it("should blow up with TestCanceledException when the task does not response interrupt request and pass after the timeout") {
        a[TestCanceledException] should be thrownBy {
          cancelAfter(timeout = Span(100, Millis)) {
            for (i <- 1 to 10) {
              try {
                SleepHelper.sleep(50)
              }
              catch {
                case _: InterruptedException =>
                  Thread.interrupted() // Swallow the interrupt
              }
            }
            Succeeded
          }
        }
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should not catch exception thrown from the test") {
        an[InterruptedException] should be thrownBy {
          cancelAfter(Span(100, Millis)) {
            throw new InterruptedException
            Succeeded
          }
        }
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should set the exception thrown from the test after timeout as cause of TestFailedException") {
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            for (i <- 1 to 10) {
              try {
                SleepHelper.sleep(50)
              }
              catch {
                case _: InterruptedException =>
                  Thread.interrupted() // Swallow the interrupt
              }
            }
            throw new IllegalArgumentException("Something went wrong!")
            Succeeded
          }
        }
        assert(caught.getCause().getClass === classOf[IllegalArgumentException])
      }

      it("should close a Socket connection via SocketSignaler when the timeout is reached") {
        val serverSocket = new ServerSocket(0)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            val clientSocket = serverSocket.accept()
            while (drag) {
              try {
                SleepHelper.sleep(100)
              }
              catch {
                case _: InterruptedException => Thread.interrupted()
              }
            }
            serverSocket.close()
          }
        }
        serverThread.start()
        val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
        val inputStream = clientSocket.getInputStream()

        a[TestCanceledException] should be thrownBy {
          cancelAfter(Span(100, Millis)) {
            inputStream.read()
            Succeeded
          }(SocketSignaler(clientSocket))
        }
        clientSocket.close()
        drag = false
        succeed
      }

      it("should close a Socket connection via FunSignaler when the timeout is reached") {
        val serverSocket = new ServerSocket(0)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            val clientSocket = serverSocket.accept()
            while (drag) {
              try {
                SleepHelper.sleep(100)
              }
              catch {
                case _: InterruptedException => Thread.interrupted()
              }
            }
            serverSocket.close()
          }
        }
        serverThread.start()
        val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
        val inputStream = clientSocket.getInputStream()

        a[TestCanceledException] should be thrownBy {
          cancelAfter(Span(100, Millis)) {
            inputStream.read()
          }(Signaler { t => clientSocket.close() })
        }
        clientSocket.close()
        drag = false
        succeed
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should wait for the test to finish when DoNotSignal.") {
        var x = 0
        val caught = the[TestCanceledException] thrownBy {
          cancelAfter(Span(100, Millis)) {
            SleepHelper.sleep(200)
            x = 1
          }(DoNotSignal)
        }
        x should be(1)
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should close a Selector connection via SelectorSignaler when the timeout is reached") {
        val selector = Selector.open()
        val ssChannel = ServerSocketChannel.open()
        ssChannel.configureBlocking(false)
        ssChannel.socket().bind(new InetSocketAddress(0))
        ssChannel.register(selector, SelectionKey.OP_ACCEPT)
        @volatile
        var drag = true
        val serverThread = new Thread() {
          override def run(): Unit = {
            selector.select()
            val it = selector.selectedKeys.iterator
            while (it.hasNext) {
              val selKey = it.next().asInstanceOf[SelectionKey]
              it.remove()
              if (selKey.isAcceptable()) {
                val ssChannel = selKey.channel().asInstanceOf[ServerSocketChannel]
                while (drag) {
                  try {
                    SleepHelper.sleep(100)
                  }
                  catch {
                    case _: InterruptedException => Thread.interrupted()
                  }
                }
              }
            }
            ssChannel.close()
          }
        }

        val clientSelector = Selector.open();
        val sChannel = SocketChannel.open()
        sChannel.configureBlocking(false);
        sChannel.connect(new InetSocketAddress("localhost", ssChannel.socket().getLocalPort()));
        sChannel.register(selector, sChannel.validOps());

        a[TestCanceledException] should be thrownBy {
          cancelAfter(Span(100, Millis)) {
            clientSelector.select()
          }(SelectorSignaler(clientSelector))
        }
        clientSelector.close()
        drag = false
        succeed
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

  }
}
