/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalatest.Resources
import org.scalatest.exceptions.StackDepthExceptionHelper._
import org.scalatest.exceptions.{StackDepthException, TestCanceledException, TestFailedDueToTimeoutException}
import org.scalatest.time.Span
import org.scalatest.{Failed, Canceled}

trait TimeLimiting[T] {

  def fail(cause: Option[Throwable], timeLimit: Span, stackDepthFun: StackDepthException => Int): T

  def cancel(cause: Option[Throwable], timeLimit: Span, stackDepthFun: StackDepthException => Int): T

}

object TimeLimiting {

  implicit def timeLimitFailingBehaviorOfOutcome[OUTCOME <: org.scalatest.Outcome]: TimeLimiting[OUTCOME] =
    new TimeLimiting[OUTCOME] {
      def fail(cause: Option[Throwable], timeLimit: Span, stackDepthFun: StackDepthException => Int): OUTCOME =
        Failed(new TestFailedDueToTimeoutException(sde => Some(Resources.timeoutFailingAfter(timeLimit.prettyString)), cause, stackDepthFun, None, timeLimit)).asInstanceOf[OUTCOME]

      def cancel(cause: Option[Throwable], timeLimit: Span, stackDepthFun: StackDepthException => Int): OUTCOME =
        Canceled(new TestCanceledException(sde => Some(Resources.timeoutCancelingAfter(timeLimit.prettyString)), cause, stackDepthFun, None)).asInstanceOf[OUTCOME]
    }

  implicit def timeLimitFailingBehaviorOfAssertion: TimeLimiting[org.scalatest.Assertion] =
    new TimeLimiting[org.scalatest.Assertion] {
      def fail(cause: Option[Throwable], timeLimit: Span, stackDepthFun: StackDepthException => Int): org.scalatest.Assertion =
        throw new TestFailedDueToTimeoutException(sde => Some(Resources.timeoutFailingAfter(timeLimit.prettyString)), cause, stackDepthFun, None, timeLimit)

      def cancel(cause: Option[Throwable], timeLimit: Span, stackDepthFun: StackDepthException => Int): org.scalatest.Assertion =
        throw new TestCanceledException(sde => Some(Resources.timeoutCancelingAfter(timeLimit.prettyString)), cause, stackDepthFun, None)
    }

}