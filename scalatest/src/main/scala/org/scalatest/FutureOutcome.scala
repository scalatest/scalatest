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
import exceptions.TestPendingException
import Suite.anExceptionThatShouldCauseAnAbort
import scala.concurrent.ExecutionException

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
      underlying flatMap { outcome =>
        if (outcome.isSucceeded) {
          try {
            f
            Future.successful(outcome)
          }
          catch {
            case _: TestPendingException => Future.successful(Pending)
            case ex: TestCanceledException => Future.successful(Canceled(ex))
            case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
            case ex: Throwable => Future.failed(new ExecutionException(ex))
          }
        } else Future.successful(outcome)
      }
    }
  }

  def onFailedThen(f: Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        outcome match {
          case Failed(originalEx) =>
            try {
              f(originalEx)
              Future.successful(outcome)
            }
            catch {
              case _: TestPendingException => Future.successful(Pending)
              case ex: TestCanceledException => Future.successful(Canceled(ex))
              case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
              case ex: Throwable => Future.failed(new ExecutionException(ex))
            }
          case _ =>
            Future.successful(outcome)
        }
      }
    }
  }

  def onCanceledThen(f: TestCanceledException => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        outcome match {
          case Canceled(originalEx) =>
            try {
              f(originalEx)
              Future.successful(outcome)
            }
            catch {
              case _: TestPendingException => Future.successful(Pending)
              case ex: TestCanceledException => Future.successful(Canceled(ex))
              case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
              case ex: Throwable => Future.failed(new ExecutionException(ex))
            }
          case _ =>
            Future.successful(outcome)
        }
      }
    }
  }

  def onPendingThen(f: => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        if (outcome.isPending) {
          try {
            f
            Future.successful(outcome)
          }
          catch {
            case _: TestPendingException => Future.successful(Pending)
            case ex: TestCanceledException => Future.successful(Canceled(ex))
            case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
            case ex: Throwable => Future.failed(new ExecutionException(ex))
          }
        } else Future.successful(outcome)
      }
    }
  }

  def change(f: Outcome => Outcome)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying.map(f) // TODO: Deal with exceptions thrown by f
    }
  }

  def onAbortedThen(f: Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying recoverWith {
        case originalEx =>
          try {
            f(originalEx)
            Future.failed(originalEx)
          }
          catch {
            case _: TestPendingException => Future.successful(Pending)
            case ex: TestCanceledException => Future.successful(Canceled(ex))
            case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
            case ex: Throwable => Future.failed(new ExecutionException(ex))
          }
      }
    }
  }

  def onOutcomeThen(f: Outcome => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
/*
    FutureOutcome {
      underlying map { outcome =>
        f(outcome) // TODO: Deal with exceptions thrown by f
        outcome
      }
    }
*/
    FutureOutcome {
      underlying flatMap { outcome =>
        try {
          f(outcome)
          Future.successful(outcome)
        }
        catch {
          // case _: TestPendingException => Future.successful(Pending)
          // case ex: TestCanceledException => Future.successful(Canceled(ex))
          case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
          // case ex: Throwable => Future.failed(new ExecutionException(ex))
        }
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

/*
 FutureOutcome.fromOutcome(Canceled("..."))
*/

