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

import org.scalatest.enablers.TimeLimiting
import org.scalatest.time.Span
import scala.concurrent.{Future, Promise, ExecutionContext}
import org.scalatest.{Exceptional, Timer, TimerTask, Resources}
import org.scalatest.exceptions.{StackDepthException, TimeoutField, TestFailedDueToTimeoutException}
import scala.util.{Failure, Success}
import org.scalatest.exceptions.StackDepthExceptionHelper._
import org.scalactic._

/**
  * Trait that provides a <code>failingAfter</code> and <code>cancelingAfter</code> construct, which allows you to specify a time limit for an
  * asynchronous operation passed as a by-name parameter.
  *
  * <p>
  * The time limit is passed as the first parameter, as a <a href="../time/Span.html"><code>Span</code></a>. The asynchronous operation is
  * passed as the second parameter. Here's a simple example of its use:
  * </p>
  *
  * <pre class="stHighlight">
  * failingAfter(Span(100, Millis)) {
  *   Future.successful(Thread.sleep(200))
  * }
  * </pre>
  *
  * <p>
  * The above code will return <code>Future[Unit]</code> immediately, but after 100 milliseconds, the Future will complete with a <a href="../exceptions/TestFailedDueToTimeoutException.html"><code>TestFailedDueToTimeoutException</code></a> with a message
  * that indicates a timeout expired:
  * </p>
  *
  * <p>
  * <code>The code passed to failingAfter did not complete within 100 milliseconds.</code>
  * </p>
  *
  * <p>
  * If you use <code>cancelingAfter</code> in place of <code>failingAfter</code>, a <a href="../exceptions/TestCanceledException.html"><code>TestCanceledException</code></a> with a message
  * that indicates a timeout expired:
  * </p>
  *
  * <p>
  * <code>The code passed to cancelingAfter did not complete within 100 milliseconds.</code>
  * </p>
  *
  * <p>
  * If you prefer you can mix in or import the members of <a href="../time/SpanSugar.html"><code>SpanSugar</code></a> and place a units value after the integer timeout.
  * Here are some examples:
  * </p>
  *
  * <pre class="stHighlight">
  * import org.scalatest.time.SpanSugar._
  *
  * failingAfter(100 millis) {
  *   Future.successful(Thread.sleep(200))
  * }
  *
  * failingAfter(1 second) {
  *   Future.successful(Thread.sleep(2000))
  * }
  * </pre>
  *
  * <p>
  * The code passed via the by-name parameter to <code>failingAfter</code> or <code>cancelingAfter</code> may be executed by thread different than the thread that invoked
  * <code>failingAfter</code> or <code>cancelingAfter</code>, so synchronization is needed to access variables declared outside the by-name.
  * </p>
  *
  * <pre class="stHighlight">
  * @volatile var result = -1 // Need to make this volatile
  * failingAfter(100 millis) {
  *   result = accessNetService()
  *   Future.successful("done")
  * }
  * </pre>
  *
  * <p>
  * The <code>failingAfter</code> or <code>cancelingAfter</code> method will create a timer that runs on a different thread than the thread that
  * invoked <code>failingAfter</code> or <code>cancelingAfter</code>, so that it can detect when the timeout has expired and complete the returned <code>Future</code>
  * early with related exception.  Different from [[org.scalatest.concurrent.Timeouts Timeouts]], <code>failingAfter</code> and <code>cancelingAfter</code> do not take
  * implicit parameter of type <code>Interruptor</code> that is responsible for interrupting the thread that execute the body of <code>Future</code>.
  * </p>
  *
  * <a name="executionContextConfig"></a><h2>Configuring <code>failingAfter</code> or <code>cancelingAfter</code> with a different <code>ExecutionContext</code></h2>
  *
  * <p>
  * <code>failingAfter</code> and <code>cancelingAfter</code> takes an implicit [[scala.concurrent.ExecutionContext ExecutionContext]],
  * you can import the global [[scala.concurrent.ExecutionContext ExecutionContext]] or you can provide a custom one, by just making it implicit visible to the invocation point
  * of <code>failingAfter</code> or <code>cancelingAfter</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * implicit val exeCtx = ...   // ExecutionContext to be used
  * failingAfter(100 millis) {
  *   Thread.sleep(500)
  * }
  * </pre>
  *
  * <p>
  * An important feature of <code>failingAfter</code> and <code>cancelingAfter</code> that worth noting: it will complete the <code>Future</code>
  * with a <code>TestFailedDueToTimeoutException</code> (or <code>TestCanceledException</code> in case of <code>cancelingAfter</code>)
  * if the code passed as the by-name parameter takes longer than the specified timeout to execute, even though it is allowed to run to completion
  * beyond the specified timeout and returns normally.
  * </p>
  *
  * @author Chua Chee Seng
  * @author Bill Venners
  */
trait AsyncTimeouts {

  private def timingOutAfter[T](
                                 timeLimit: Span,
                                 pos: source.Position,
                                 exceptionFun: (Option[Throwable], Span, StackDepthException => Int) => T)(block: => Future[T])(implicit executionContext: ExecutionContext): Future[T] = {

    class TimeoutTask[T](promise: Promise[T], span: Span, exceptionFun: (Option[Throwable], Span, StackDepthException => Int) => T) extends TimerTask {

      def run(): Unit = {
        def stackDepthFun(sde: StackDepthException): Int =
          getStackDepth(sde.getStackTrace, pos)
        if (!promise.isCompleted) {
          promise.complete(Success(exceptionFun(None, span, stackDepthFun)))
        }
      }

    }

    def stackDepthFun(sde: StackDepthException): Int =
      getStackDepth(sde.getStackTrace, pos)

    val limit = timeLimit.totalNanos / 1000 / 1000
    val startTime = scala.compat.Platform.currentTime

    try {
      val future: Future[T] = block

      val endTime = scala.compat.Platform.currentTime
      val produceFutureDuration = endTime - startTime

      if (produceFutureDuration > limit) {
        def stackDepthFun(sde: StackDepthException): Int =
          getStackDepth(sde.getStackTrace, pos)
        Future.successful(exceptionFun(None, timeLimit, stackDepthFun))
      }
      else {
        val promise = Promise[T]
        val task = new TimeoutTask(promise, timeLimit, exceptionFun)
        val delay = limit - (scala.compat.Platform.currentTime - startTime)
        val timer = new Timer

        future.onComplete { t =>
          t match {
            case Success(r) =>
              task.cancel()
              if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
              val endTime = scala.compat.Platform.currentTime
                val duration = endTime - startTime
                if (duration > limit)
                  promise.complete(Success(exceptionFun(None, timeLimit, stackDepthFun)))
                else
                  promise.success(r)
              }

            case Failure(e) =>
              task.cancel()
              if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
              val endTime = scala.compat.Platform.currentTime
                val duration = endTime - startTime
                if (duration > limit)
                  promise.complete(Success(exceptionFun(Some(e), timeLimit, stackDepthFun)))
                else
                  promise.failure(e) // Chee Seng: I wonder if in this case should we use this exception instead of the other one? Not sure.
              }
          }
        }
        timer.schedule(task, delay)
        promise.future
      }
    }
    catch {
      case e: org.scalatest.exceptions.ModifiableMessage[_] with TimeoutField => // Chee Seng: how can this happen? Oh, is it when producing the Future?
        Future.successful(exceptionFun(Some(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString)))), timeLimit, stackDepthFun))

      case t: Throwable =>
        val endTime = scala.compat.Platform.currentTime
        val produceFutureDuration = endTime - startTime

        if (produceFutureDuration > limit)  // if the test block blows up after the time limit, we'll still fail with timeout, setting the the thrown exception as cause.
          Future.successful(exceptionFun(Some(t), timeLimit, stackDepthFun))
        else
          throw t
    }
  }

  /**
    * Executes the passed asynchronous function, enforcing the passed time limit by completing the returned <code>Future</code> with <code>TestFailedDueToTimeoutException</code>
    * if the time limit is exceeded.
    *
    * <p>
    * If the asynchronous function completes <em>before</em> the timeout expires:
    * </p>
    *
    * <ul>
    * <li>If the function returns normally, the <code>Future</code> will be completed with the return value of the function.</li>
    * <li>If the function completes abruptly with an exception, this method will complete the <code>Future</code> with that same exception.</li>
    * </ul>
    *
    * <p>
    * If the asynchronous function completes <em>after</em> the timeout expires:
    * </p>
    *
    * <ul>
    * <li>If the function returns normally, this method will complete the <code>Future</code> with a <code>TestFailedDueToTimeoutException</code>.</li>
    * <li>If the function completes abruptly with an exception, this method will complete the <code>Future</code> with a <code>TestFailedDueToTimeoutException</code> that includes the exception thrown by the function as its cause.</li>
    * </ul>
    *
    * @param timeLimit the maximimum amount of time allowed for the passed function
    * @param block the function on which to enforce the passed timeout
    * @param executionContext a <code>ExecutionContext</code> for execution of the returned <code>Future</code>
    * @param failing a <code>TimeLimiting</code> type class that contains the code to call when time limit is exceeded.
    * @param pos <code>Position</code> of the calling site.
    */
  def failingAfter[T](timeLimit: Span)(block: => Future[T])(implicit executionContext: ExecutionContext, failing: TimeLimiting[T], pos: source.Position = implicitly[source.Position]): Future[T] =
    timingOutAfter(timeLimit, pos, failing.fail)(block)

  /**
    * Executes the passed asynchronous function, enforcing the passed time limit by completing the returned <code>Future</code> with <code>TestCanceledException</code> if the
    * time limit is exceeded.
    *
    * <p>
    * If the asynchronous function completes <em>before</em> the timeout expires:
    * </p>
    *
    * <ul>
    * <li>If the function returns normally, the <code>Future</code> will be completed with the return value of the function.</li>
    * <li>If the function completes abruptly with an exception, this method will complete the <code>Future</code> with that same exception.</li>
    * </ul>
    *
    * <p>
    * If the asynchronous function completes <em>after</em> the timeout expires:
    * </p>
    *
    * <ul>
    * <li>If the function returns normally, this method will complete the <code>Future</code> with a <code>TestCanceledException</code>.</li>
    * <li>If the function completes abruptly with an exception, this method will complete the <code>Future</code> with a <code>TestCanceledException</code> that includes the exception thrown by the function as its cause.</li>
    * </ul>
    *
    * @param timeLimit the maximimum amount of time allowed for the passed function
    * @param block the function on which to enforce the passed timeout
    * @param executionContext a <code>ExecutionContext</code> for execution of the returned <code>Future</code>
    * @param failing a <code>TimeLimiting</code> type class that contains the code to call when time limit is exceeded.
    * @param pos <code>Position</code> of the calling site.
    */
  def cancelingAfter[T](timeLimit: Span)(block: => Future[T])(implicit executionContext: ExecutionContext, failing: TimeLimiting[T], pos: source.Position = implicitly[source.Position]): Future[T] =
    timingOutAfter(timeLimit, pos, failing.cancel)(block)
}

/**
  * Companion object that facilitates the importing of <code>AsyncTimeouts</code> members as
  * an alternative to mixing in the trait. One use case is to import <code>AsyncTimeouts</code>'s members so you can use
  * them in the Scala interpreter.
  */
object AsyncTimeouts extends AsyncTimeouts