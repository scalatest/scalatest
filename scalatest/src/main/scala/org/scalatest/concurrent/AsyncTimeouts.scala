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

import org.scalatest.time.Span
import scala.concurrent.{Future, Promise, ExecutionContext}
import org.scalatest.{Timer, TimerTask}
import org.scalatest.exceptions.{TimeoutField, TestFailedDueToTimeoutException}
import scala.util.{Failure, Success}
import org.scalatest.Resources
import org.scalatest.exceptions.StackDepthExceptionHelper._

trait AsyncTimeouts[T] {

  class TimeoutTask(promise: Promise[T], span: Span) extends TimerTask {

    def run(): Unit = {
      if (!promise.isCompleted) {
        promise.complete(Success(failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("AsyncTimeouts.scala", "run"), None, span))))
      }
    }

  }

  protected def failure(e: Throwable): T

  def failingAfter(timeLimit: Span)(block: => Future[T])(implicit executionContext: ExecutionContext): Future[T] = {
    val limit = timeLimit.totalNanos / 1000 / 1000
    val startTime = scala.compat.Platform.currentTime
    try {
      val future: Future[T] = block

      val endTime = scala.compat.Platform.currentTime
      val produceFutureDuration = endTime - startTime

      if (produceFutureDuration > limit)
        Future.successful(failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(timeLimit.prettyString)), None, getStackDepthFun("AsyncTimeouts.scala", "failingAfter"), None, timeLimit)))
      else {
        val promise = Promise[T]
        val task = new TimeoutTask(promise, timeLimit)
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
                  promise.complete(Success(failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(timeLimit.prettyString)), None, getStackDepthFun("AsyncTimeouts.scala", "failingAfter"), None, timeLimit))))
                else
                  promise.success(r)
              }

            case Failure(e) =>
              task.cancel()
              if (!promise.isCompleted) { // If it completed already, it will fail or have failed with a timeout exception
                val endTime = scala.compat.Platform.currentTime
                val duration = endTime - startTime
                if (duration > limit)
                  promise.complete(Success(failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(timeLimit.prettyString)), Some(e), getStackDepthFun("AsyncTimeouts.scala", "failingAfter"), None, timeLimit))))
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
        Future.successful(failure(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString)))))

      case t: Throwable =>
        val endTime = scala.compat.Platform.currentTime
        val produceFutureDuration = endTime - startTime

        if (produceFutureDuration > limit)
          Future.successful(failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(timeLimit.prettyString)), Some(t), getStackDepthFun("AsyncTimeouts.scala", "failingAfter"), None, timeLimit)))
        else
          Future.successful(failure(t))
    }
  }
}
