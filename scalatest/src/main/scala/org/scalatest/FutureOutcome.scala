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
package org.scalatest

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.scalactic.{Or, Good, Bad}
import scala.util.{Try, Success, Failure}

class FutureOutcome(val underlying: Future[Outcome]) {
  // TODO: add tests for pretty toString

  def onCompletedThen(f: Outcome Or Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying map { outcome =>
        f(Good(outcome))
        outcome
      } // TODO: Don't forget the Failure case
    }
  }

  def onSucceededThen(f: => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying map { outcome =>
        if (outcome.isSucceeded) f // TODO: Deal with exceptions thrown by f
        outcome
      }
    }
  }

  def isCompleted: Boolean = underlying.isCompleted

  def value: Option[Outcome Or Throwable] =
    underlying.value match {
      case None => None
      case Some(Success(outcome)) => Some(Good(outcome))
      case Some(Failure(ex)) => Some(Bad(ex))
    }
}

object FutureOutcome {
  def apply(underlying: Future[Outcome]): FutureOutcome = new FutureOutcome(underlying)
}


