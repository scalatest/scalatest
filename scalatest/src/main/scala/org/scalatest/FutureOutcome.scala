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
import exceptions.TestCanceledException

class FutureOutcome(val underlying: Future[Outcome]) {
  // TODO: add tests for pretty toString

  def onCompletedThen(f: Outcome Or Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying recoverWith {
        case ex =>
          f(Bad(ex))
          Future.failed(ex)
      } map { outcome =>
        f(Good(outcome))
        outcome
      }
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

  def onFailedThen(f: Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying map { outcome =>
        outcome match {
          case Failed(ex) => f(ex) // TODO: Deal with exceptions thrown by f
          case _ =>
        }
        outcome
      }
    }
  }

  def onCanceledThen(f: TestCanceledException => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying map { outcome =>
        outcome match {
          case Canceled(ex) => f(ex) // TODO: Deal with exceptions thrown by f
          case _ =>
        }
        outcome
      }
    }
  }

  def onPendingThen(f: => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying map { outcome =>
        if (outcome.isPending) f // TODO: Deal with exceptions thrown by f
        outcome
      }
    }
  }

  def map(f: Outcome => Outcome)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying.map(f) // TODO: Deal with exceptions thrown by f
    }
  }

  def onAbortedThen(f: Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying recoverWith {
        case ex =>
          f(ex)
          Future.failed(ex)
      }
    }
  }

  def onOutcomeThen(f: Outcome => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying map { outcome =>
        f(outcome) // TODO: Deal with exceptions thrown by f
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


