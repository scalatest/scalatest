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
import java.util.{Timer, TimerTask}
import org.scalatest.exceptions.{TimeoutField, TestFailedDueToTimeoutException}
import scala.util.{Failure, Success}
import org.scalatest.Resources
import org.scalatest.exceptions.StackDepthExceptionHelper._

trait AsyncTimeouts[T] {

  class TimeoutTask(promise: Promise[T], span: Span) extends TimerTask {

    def run(): Unit = {
      if (!promise.isCompleted) {
        promise.complete(Success(failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "run"), None, span))))
        //promise.complete(Success(Exceptional(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "run"), None, span))))
      }
    }

  }

  protected def failure(e: Throwable): T

  def failingAfter(timeLimit: Span, interruptor: Interruptor)(block: => Future[T])(implicit executionContext: ExecutionContext): Future[T] = {
    try {
      val limit = timeLimit.totalNanos / 1000 / 1000
      val startTime = scala.compat.Platform.currentTime
      val future: Future[T] =
        Timeouts.failAfter(timeLimit) {
          block
        }(interruptor)

      val promise = Promise[T]
      val task = new TimeoutTask(promise, timeLimit)
      val delay = limit - (scala.compat.Platform.currentTime - startTime)
      val timer = new Timer

      future.onComplete { t =>
        t match {
          case Success(r) =>
            timer.cancel()
            if (!promise.isCompleted)
              promise.success(r)

          case Failure(e) =>
            timer.cancel()
            if (!promise.isCompleted)
              promise.failure(e)
        }
      }
      timer.schedule(task, delay)
      promise.future
    }
    catch {
      case e: org.scalatest.exceptions.ModifiableMessage[_] with TimeoutField =>
        //Future.successful(Exceptional(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString)))))
        Future.successful(failure(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString)))))

      case t: Throwable =>
        Future.successful(failure(t))
    }
  }

}