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

trait AsyncTimeouts {

  private def timingOutAfter[T](
                                 timeLimit: Span,
                                 pos: source.Position,
                                 exceptionFun: (Option[Throwable], Span, source.Position) => T)(block: => Future[T])(implicit executionContext: ExecutionContext): Future[T] = {

    class TimeoutTask[T](promise: Promise[T], span: Span, exceptionFun: (Option[Throwable], Span, source.Position) => T) extends TimerTask {

      def run(): Unit = {
        def stackDepthFun(sde: StackDepthException): Int =
          getStackDepth(sde.getStackTrace, pos)
        if (!promise.isCompleted) {
          promise.complete(Success(exceptionFun(None, span, pos)))
        }
      }

    }

    val limit = timeLimit.totalNanos / 1000 / 1000
    val startTime = scala.compat.Platform.currentTime

    try {
      val future: Future[T] = block

      val endTime = scala.compat.Platform.currentTime
      val produceFutureDuration = endTime - startTime

      if (produceFutureDuration > limit)
        Future.successful(exceptionFun(None, timeLimit, pos))
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
                  promise.complete(Success(exceptionFun(None, timeLimit, pos)))
                else
                  promise.success(r)
              }

            case Failure(e) =>
              task.cancel()
              if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
              val endTime = scala.compat.Platform.currentTime
                val duration = endTime - startTime
                if (duration > limit)
                  promise.complete(Success(exceptionFun(Some(e), timeLimit, pos)))
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
        Future.successful(exceptionFun(Some(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString)))), timeLimit, pos))

      case t: Throwable =>
        val endTime = scala.compat.Platform.currentTime
        val produceFutureDuration = endTime - startTime

        if (produceFutureDuration > limit)  // if the test block blows up after the time limit, we'll still fail with timeout, setting the the thrown exception as cause.
          Future.successful(exceptionFun(Some(t), timeLimit, pos))
        else
          throw t
    }
  }

  def failingAfter[T](timeLimit: Span)(block: => Future[T])(implicit executionContext: ExecutionContext, failing: TimeLimiting[T], pos: source.Position = implicitly[source.Position]): Future[T] =
    timingOutAfter(timeLimit, pos, failing.fail)(block)

  def cancelingAfter[T](timeLimit: Span)(block: => Future[T])(implicit executionContext: ExecutionContext, failing: TimeLimiting[T], pos: source.Position = implicitly[source.Position]): Future[T] =
    timingOutAfter(timeLimit, pos, failing.cancel)(block)
}

object AsyncTimeouts extends AsyncTimeouts
