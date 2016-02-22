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
package org.scalatest.enablers

import org.scalatest.Exceptional
import org.scalatest.concurrent.Signaler
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.StackDepthExceptionHelper._

import scala.util.{Failure, Success}

//import java.util.TimerTask
//import java.util.Timer
import org.scalatest.TimerTask
import org.scalatest.Timer
import org.scalatest.time.Span
import org.scalatest.concurrent.SignalerTimeoutTask
import org.scalatest.Outcome
import scala.concurrent.{Promise, Future, ExecutionContext}
import org.scalatest.FutureOutcome

trait Timed[T] {
  def timeoutAfter(
    timeout: Span,
    f: => T,
    signaler: Signaler,
    exceptionFun: (Option[Throwable], Int) => StackDepthException
  ): T
}

object Timed {
  // Chee Seng: First step will be to get this working on Scala.js. I think there we
  // since we can't interrupt or signal, the default of DoNotSignal makes sense and
  // we'll just time it, and after it is finished, if it took too long, we fail it.
  implicit def timed[T]: Timed[T] =
    new Timed[T] {
      def timeoutAfter(
        timeout: Span,
        f: => T,
        signaler: Signaler,
        exceptionFun: (Option[Throwable], Int) => StackDepthException
      ): T = {

        // SKIP-SCALATESTJS-START
        def stackDepthFun(sde: StackDepthException): Int =
          getStackDepth(sde.getStackTrace, "Timed.scala", "timeoutAfter") + 2
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY def stackDepthFun(sde: StackDepthException): Int = 15

        val timer = new Timer
        val task = new SignalerTimeoutTask(Thread.currentThread(), signaler)
        val maxDuration = timeout.totalNanos / 1000 / 1000
        timer.schedule(task, maxDuration) // TODO: Probably use a sleep so I can use nanos
        val startTime = scala.compat.Platform.currentTime
        try {
          val result = f
          val endTime = scala.compat.Platform.currentTime
          task.cancel()
          result match {
            case Exceptional(ex) => throw ex  // If the result is Exceptional, the exception is already wrapped, just re-throw it to get the old behavior.
            case _ => 
              if (task.timedOut || (endTime - startTime) > maxDuration) {
                if (task.needToResetInterruptedStatus)
                  Thread.interrupted() // To reset the flag probably. He only does this if it was not set before and was set after, I think.
                throw exceptionFun(None, 4)
              }
          }
          result
        }
        catch {
          case t: Throwable =>
            val endTime = scala.compat.Platform.currentTime
            task.cancel() // Duplicate code could be factored out I think. Maybe into a finally? Oh, not that doesn't work. So a method.
            if(task.timedOut || (endTime - startTime) > maxDuration) {
              if (task.needToResetInterruptedStatus)
                Thread.interrupted() // Clear the interrupt status (There's a race condition here, but not sure we an do anything about that.)
              throw exceptionFun(Some(t), 4)
            }
            else
              throw t
        }
      }
    }

  /*
  Chee Seng: This one should catch TestCanceledException and change it into
  a Canceled. It should catch TestPendingException and change it into
  a Pending. It should catch any other non-suite-aborting exception and
  turn it into a Failed. A timeout should become a Failed(TestFailedDueToTimeoutException).
  I believe this is what you did in AsyncTimeouts.
  */
  implicit def timedFutureOfOutcome: Timed[Future[Outcome]] = ???

  /*
  Chee Seng: This one should allow any exception to just do the usual
  thing of go to scala.util.Failure. This is likely what you did with
  Future[Assertion] in AsyncTimeouts. But we should do it for any Future (except
  Future[Outcome] will be treated specially because it goes to the
  more specific implicit above.
  */
  implicit def timedFutureOf[T](implicit executionContext: ExecutionContext): Timed[Future[T]] =
    new Timed[Future[T]] {
      def timeoutAfter(
                        timeout: Span,
                        f: => Future[T],
                        signaler: Signaler,
                        exceptionFun: (Option[Throwable], Int) => StackDepthException
                        ): Future[T] = {
        val timer = new Timer
        val maxDuration = timeout.totalNanos / 1000 / 1000
        val startTime = scala.compat.Platform.currentTime
        try {
          val result = f
          val endTime = scala.compat.Platform.currentTime

          if ((endTime - startTime) > maxDuration) {
            throw exceptionFun(None, 2)
          }

          val promise = Promise[T]
          val task = new SignalerTimeoutTask(Thread.currentThread(), signaler)
          val delay = maxDuration - (scala.compat.Platform.currentTime - startTime)
          val timer = new Timer

          result.onComplete { t =>
            t match {
              case Success(r) =>
                task.cancel()
                if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
                val endTime = scala.compat.Platform.currentTime
                  val duration = endTime - startTime
                  if (duration > maxDuration)
                    promise.complete(Failure(exceptionFun(None, 2)))
                  else
                    promise.success(r)
                }

              case Failure(e) =>
                task.cancel()
                if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
                val endTime = scala.compat.Platform.currentTime
                  val duration = endTime - startTime
                  if (duration > maxDuration)
                    promise.complete(Failure(exceptionFun(Some(e), 2)))
                  else
                    promise.failure(e) // Chee Seng: I wonder if in this case should we use this exception instead of the other one? Not sure.
                }
            }
          }

          timer.schedule(task, delay)
          promise.future
        }
        catch {
          case t: Throwable =>
            val endTime = scala.compat.Platform.currentTime
            if((endTime - startTime) > maxDuration) {
              throw exceptionFun(Some(t), 2)
            }
            else
              throw t
        }
      }
    }

  /*
  Chee Seng: This one should catch TestCanceledException and change it into
  a Canceled. It should catch TestPendingException and change it into
  a Pending. It should catch any other non-suite-aborting exception and
  turn it into a Failed. A timeout should become a Failed(TestFailedDueToTimeoutException).
  I believe this is what you did in AsyncTimeouts.
  */
  implicit def timedFutureOutcome: Timed[FutureOutcome] = ???
}
