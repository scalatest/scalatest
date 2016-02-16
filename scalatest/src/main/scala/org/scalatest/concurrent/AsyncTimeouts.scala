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

trait AsyncTimeouts {

  private def timingOutAfter[T](
    timeLimit: Span,
    methodName: String,
    exceptionFun: (Option[Throwable], Span, StackDepthException => Int) => T)(block: => Future[T])(implicit executionContext: ExecutionContext): Future[T] = {

    class TimeoutTask[T](promise: Promise[T], span: Span, exceptionFun: (Option[Throwable], Span, StackDepthException => Int) => T) extends TimerTask {

      def run(): Unit = {
        // SKIP-SCALATESTJS-START
        def stackDepthFun(sde: StackDepthException): Int =
          getStackDepth(sde.getStackTrace, "AsyncTimeouts.scala", methodName) + 2
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY def stackDepthFun(sde: StackDepthException): Int = { sde.printStackTrace(); 15 }
        if (!promise.isCompleted) {
          promise.complete(Success(exceptionFun(None, span, stackDepthFun)))
        }
      }

    }

    // SKIP-SCALATESTJS-START
    def stackDepthFun(sde: StackDepthException): Int =
      getStackDepth(sde.getStackTrace, "AsyncTimeouts.scala", methodName) + 2
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY def stackDepthFun(sde: StackDepthException): Int = 15

    val limit = timeLimit.totalNanos / 1000 / 1000
    val startTime = scala.compat.Platform.currentTime

    try {
      val future: Future[T] = block

      val endTime = scala.compat.Platform.currentTime
      val produceFutureDuration = endTime - startTime

      if (produceFutureDuration > limit) {
        // SKIP-SCALATESTJS-START
        def stackDepthFun(sde: StackDepthException): Int =
          getStackDepth(sde.getStackTrace, "AsyncTimeouts.scala", methodName) + 2
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY def stackDepthFun(sde: StackDepthException): Int = if (methodName == "cancelingAfter") 14 else 15
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

  def failingAfter[T](timeLimit: Span)(block: => Future[T])(implicit executionContext: ExecutionContext, failing: TimeLimiting[T]): Future[T] =
    timingOutAfter(timeLimit, "failingAfter", failing.fail)(block)

  def cancelingAfter[T](timeLimit: Span)(block: => Future[T])(implicit executionContext: ExecutionContext, failing: TimeLimiting[T]): Future[T] =
    timingOutAfter(timeLimit, "cancelingAfter", failing.cancel)(block)
}

object AsyncTimeouts extends AsyncTimeouts
