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
package org.scalatest.enablers

import org.scalatest._
import org.scalatest.concurrent.Signaler
import org.scalatest.exceptions.StackDepthException
import org.scalactic.{Bad, Good}
import scala.util.{Failure, Success}


//import java.util.TimerTask
//import java.util.Timer
import org.scalatest.time.Span
import org.scalatest.concurrent.SignalerTimeoutTask
import scala.concurrent.{Promise, Future, ExecutionContext}
import org.scalactic.source

/**
 * Trait that provides a <code>timeoutAfter</code> construct, which allows you to specify a timeout for an
 * operation passed as a by-name parameter, as well as a way to signal/interrupt it if the operation exceeds its time limit.
 */
trait Timed[T] {
  /**
    * Execute the passed in function <code>f</code> and time it, if the time it takes to complete the function the function
    * execution exceeds the passed in <code>timeout</code>, call the passed in <code>exceptionFun</code> to create an instance
    * of [[org.scalatest.exceptions.StackDepthException StackDepthException]] and the implementation is responsible to handle it.
    *
    * @param timeout the maximum amount of time allowed for the passed function
    * @param f the passed in function to be timed
    * @param signaler the <code>Signaler</code> used to signal/interrupt the function when time limit exceeded
    * @param exceptionFun the function to create <code>StackDepthException</code> for failure
    * @return the T returned by function <code>f</code>
    */
  def timeoutAfter(
    timeout: Span,
    f: => T,
    signaler: Signaler,
    exceptionFun: Option[Throwable] => StackDepthException
  ): T
}

/**
 * Companion object for <code>Timed</code> typeclass that offers three implicit providers: one for <code>FutureOutcome</code>,
 * one for <code>Future</code> of any type, and one for any other type.
 *
 * <p>
 * The details are in the documentation for the implicit providers themselves (methods <code>timed</code>, <code>timedFutureOf</code>,
 * and <code>timedFutureOutcome</code>), but in short if a time limit is exceeded:
 * </p>
 *
 * <ul>
 * <li>if the type <code>T</code> in <code>Timed[T]</code> is <code>FutureOutcome</code>
 * the <code>FutureOutcome</code> returned by <code>timeoutAfter</code> will result in either <code>Failed</code> or <code>Canceled</code></li>
 * <li>if the type is <code>Future[U]</code>, the <code>Future[U]</code> returned by <code>timeoutAfter</code> will fail with either a
 * <code>TestFailedDueToTimeoutException</code> or a <code>TestCanceledException</code>.</li>
 * <li>otherwise, the <code>timeoutAfter</code> method will itself complete abruptly with either <code>TestFailedDueToTimeoutException</code>
 * or <code>TestCanceledException.</li>
 * </p>
 */
object Timed {

/*
TODO: See if this bit of documentation from the old Timeouts is still relevant, and if so, insert it
below:
   <p>
   If the interrupted status of the main test thread (the thread that invoked <code>failAfter</code>) was not invoked
   when <code>failAfter</code> was invoked, but is set after the operation times out, it is reset by this method before
   it completes abruptly with a <code>TestFailedDueToTimeoutException</code>. The interrupted status will be set by
   <code>ThreadSignaler</code>, the default <code>Signaler</code> implementation.
   </p>
   
*/

  /**
   // SKIP-DOTTY-START
   * Implicit method that provides <code>Timed</code> implementation for any <code>T</code>.
   // SKIP-DOTTY-END
   //DOTTY-ONLY * Method that provides <code>Timed</code> implementation for any <code>T</code>.
   *
   * <p>
   * If the function completes <em>before</em> the timeout expires:
   * </p>
   *
   * <ul>
   * <li>If the function returns normally, this method will return normally.</li>
   * <li>If the function completes abruptly with an exception, this method will complete abruptly with that same exception.</li>
   * </ul>
   *
   * <p>
   * If the function completes <em>after</em> the timeout expires:
   * </p>
   *
   * <ul>
   * <li>If the function returns normally, this method will complete abruptly with a <code>StackDepthException</code> created from <code>exceptionFun</code>.</li>
   * <li>If the function completes abruptly with an exception, this method will complete abruptly with a <code>StackDepthException</code> created from <code>exceptionFun</code> that includes the exception thrown by the function as its cause.</li>
   * </ul>
   *
   * <p>
   * This implementation will start a timer that when the time limit is exceeded while the passed in function <code>f</code> is still running, it will attempt to call the passed in <code>Signaler</code> to signal/interrupt the running function <code>f</code>.
   * </p>
   */
  // SKIP-DOTTY-START
  implicit def timed[T]: Timed[T] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def timed[T]: Timed[T] =
    new Timed[T] {
      def timeoutAfter(
        timeout: Span,
        f: => T,
        signaler: Signaler,
        exceptionFun: Option[Throwable] => StackDepthException
      ): T = {
        val timer = new Timer
        val task = new SignalerTimeoutTask(Thread.currentThread(), signaler)
        val maxDuration = timeout.totalNanos / 1000 / 1000
        timer.schedule(task, maxDuration) // TODO: Probably use a sleep so I can use nanos
        val startTime = scala.compat.Platform.currentTime
        try {
          val result = f
          val endTime = scala.compat.Platform.currentTime
          task.cancel()
          timer.cancel()
          result match {
            case Exceptional(ex) => throw ex  // If the result is Exceptional, the exception is already wrapped, just re-throw it to get the old behavior.
            case _ => 
              if (task.timedOut || (endTime - startTime) > maxDuration) {
                if (task.needToResetInterruptedStatus)
                  Thread.interrupted() // To reset the flag probably. He only does this if it was not set before and was set after, I think.
                throw exceptionFun(None)
              }
          }
          result
        }
        catch {
          case t: Throwable =>
            val endTime = scala.compat.Platform.currentTime
            task.cancel() // Duplicate code could be factored out I think. Maybe into a finally? Oh, not that doesn't work. So a method.
            timer.cancel()
            if(task.timedOut || (endTime - startTime) > maxDuration) {
              if (task.needToResetInterruptedStatus)
                Thread.interrupted() // Clear the interrupt status (There's a race condition here, but not sure we an do anything about that.)
              throw exceptionFun(Some(t))
            }
            else
              throw t
        }
      }
    }

  //DOTTY-ONLY given given_timed[T]: Timed[T] = timed

  /**
   // SKIP-DOTTY-START
   * Implicit method that provides <code>Timed</code> implementation for any <code>Future[T]</code>.
   // SKIP-DOTTY-END
   //DOTTY-ONLY * Method that provides <code>Timed</code> implementation for any <code>Future[T]</code>.
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
    * <li>If the function returns normally, this method will complete the <code>Future</code> with a <code>StackDepthException</code> created from <code>exceptionFun</code>.</li>
    * <li>If the function completes abruptly with an exception, this method will complete the <code>Future</code> with a <code>StackDepthException</code> created from <code>exceptionFun</code> that includes the exception thrown by the function as its cause.</li>
    * </ul>
   *
   * <p>
   * This implementation will start a timer that when the time limit is exceeded while the passed in asynchronous function <code>f</code> is still running, it will attempt to call the passed in <code>Signaler</code> to signal/interrupt the running function <code>f</code>.
   * </p>
   */
  // SKIP-DOTTY-START
  implicit def timedFutureOf[T](implicit executionContext: ExecutionContext): Timed[Future[T]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def timedFutureOf[T](using executionContext: ExecutionContext): Timed[Future[T]] =
    new Timed[Future[T]] {
      def timeoutAfter(
                        timeout: Span,
                        f: => Future[T],
                        signaler: Signaler,
                        exceptionFun: Option[Throwable] => StackDepthException
                        ): Future[T] = {
        val timer = new Timer
        val maxDuration = timeout.totalNanos / 1000 / 1000
        val startTime = scala.compat.Platform.currentTime
        try {
          val result = f
          val endTime = scala.compat.Platform.currentTime

          if ((endTime - startTime) > maxDuration) {
            throw exceptionFun(None)
          }

          val promise = Promise[T]
          val task = new SignalerTimeoutTask(Thread.currentThread(), signaler)
          val delay = maxDuration - (scala.compat.Platform.currentTime - startTime)
          val timer = new Timer

          // This call should come before the onComplete call, to avoid race condition between the schedule call and cancel call in the onComplete block as reported in:
          // https://github.com/scalatest/scalatest/issues/1147
          timer.schedule(task, delay)
          result.onComplete { t =>
            t match {
              case Success(r) =>
                task.cancel()
                timer.cancel()
                if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
                val endTime = scala.compat.Platform.currentTime
                  val duration = endTime - startTime
                  if (duration > maxDuration)
                    promise.complete(Failure(exceptionFun(None)))
                  else
                    promise.success(r)
                }

              case Failure(e) =>
                task.cancel()
                timer.cancel()
                if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
                val endTime = scala.compat.Platform.currentTime
                  val duration = endTime - startTime
                  if (duration > maxDuration)
                    promise.complete(Failure(exceptionFun(Some(e))))
                  else
                    promise.failure(e) // Chee Seng: I wonder if in this case should we use this exception instead of the other one? Not sure.
                }
            }
          }

          promise.future
        }
        catch {
          case t: Throwable =>
            val endTime = scala.compat.Platform.currentTime
            if((endTime - startTime) > maxDuration) {
              throw exceptionFun(Some(t))
            }
            else
              throw t
        }
      }
    }

  //DOTTY-ONLY given given_timedFutureOf[T](using executionContext: ExecutionContext): Timed[Future[T]] = timedFutureOf(using executionContext)

  /*
  Chee Seng: This one should catch TestCanceledException and change it into
  a Canceled. It should catch TestPendingException and change it into
  a Pending. It should catch any other non-suite-aborting exception and
  turn it into a Failed. A timeout should become a Failed(TestFailedDueToTimeoutException).
  I believe this is what you did in AsyncTimeouts.
  */
  /**
    * Implicit method that provides <code>Timed</code> implementation for <code>FutureOutcome</code>.
    *
    * <p>
    * If the asynchronous function completes <em>before</em> the timeout expires:
    * </p>
    *
    * <ul>
    * <li>If the function returns normally, the <code>FutureOutcome</code> will be completed with the <code>Outcome</code> returned from the function.</li>
    * <li>If the function completes abruptly with an <code>TestPendingException</code>, this method will complete the <code>FutureOutcome</code> with <code>Pending</code>.</li>
    * <li>If the function completes abruptly with an <code>TestCanceledException</code>, this method will complete the <code>FutureOutcome</code> with <code>Canceled</code> that contains the thrown exception.</li>
    * <li>If the function completes abruptly with a run-aborting exception, this method will complete the <code>FutureOutcome</code> with <code>Failed</code> that contains the thrown exception.</li>
    * <li>If the function completes abruptly with a non-run-aborting exception, this method will fail the <code>FutureOutcome</code> with <code>ExecutionException</code> that contains the thrown exception.</li>
    * </ul>
    *
    * <p>
    * If the asynchronous function completes <em>after</em> the timeout expires:
    * </p>
    *
    * <ul>
    * <li>If the function returns normally, this method will complete the <code>FutureOutcome</code> with a <code>Outcome</code> that's mapped from the exception thrown from <code>exceptionFun</code>.</li>
    * <li>If the function completes abruptly with an exception, this method will complete the <code>FutureOutcome</code> with <code>Outcome</code> that's mapped from the exception thrown from <code>exceptionFun</code> that includes the exception thrown by the function as its cause.</li>
    * </ul>
    *
    * <p>
    * This implementation will start a timer that when the time limit is exceeded while the passed in asynchronous function <code>f</code> is still running, it will attempt to call the passed in <code>Signaler</code> to signal/interrupt the running function <code>f</code>.
    * </p>
    */
  // SKIP-DOTTY-START
  implicit def timedFutureOutcome(implicit executionContext: ExecutionContext): Timed[FutureOutcome] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def timedFutureOutcome(using executionContext: ExecutionContext): Timed[FutureOutcome] =
    new Timed[FutureOutcome] {
      def timeoutAfter(
                        timeout: Span,
                        f: => FutureOutcome,
                        signaler: Signaler,
                        exceptionFun: Option[Throwable] => StackDepthException
                        ): FutureOutcome = {
        val timer = new Timer
        val maxDuration = timeout.totalNanos / 1000 / 1000
        val startTime = scala.compat.Platform.currentTime

        val result = f
        val endTime = scala.compat.Platform.currentTime

        if ((endTime - startTime) > maxDuration)
          throw exceptionFun(None)

        val task = new SignalerTimeoutTask(Thread.currentThread(), signaler)
        val delay = maxDuration - (scala.compat.Platform.currentTime - startTime)

        // This call should come before the onCompletedThen call, to avoid race condition between the schedule call and cancel call in the onComplete block as reported in:
        // https://github.com/scalatest/scalatest/issues/1147
        timer.schedule(task, delay)
        val futureOutcome = result.onCompletedThen { t =>
          t match {
            case Good(r) =>
              task.cancel()
              timer.cancel()
              val endTime = scala.compat.Platform.currentTime
              val duration = endTime - startTime
              try {
                if (duration > maxDuration) {
                  throw exceptionFun(None)
                }
              }
              catch {
                case t: Throwable =>
                  throw t
              }

            case Bad(e) =>
              task.cancel()
              timer.cancel()
              val endTime = scala.compat.Platform.currentTime
              val duration = endTime - startTime
              if (duration > maxDuration)
                throw exceptionFun(None)
              else
                throw e
          }
        }

        futureOutcome
      }
    }

  //DOTTY-ONLY given given_timedFutureOutcome(using executionContext: ExecutionContext): Timed[FutureOutcome] = timedFutureOutcome(using executionContext)
}
