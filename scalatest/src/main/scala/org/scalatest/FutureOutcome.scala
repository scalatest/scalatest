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

class FutureOutcome(private[scalatest] val underlying: Future[Outcome]) {
  // TODO: add tests for pretty toString

  /**
   * Registers a callback function to be executed after this future completes, returning
   * a new future that completes only after the callback has finished execution.
   *
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    and, subsequently, the passed callback function have completed execution.
   */
  def onCompletedThen(callback: Outcome Or Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying recoverWith {
        case ex =>
          try {
            callback(Bad(ex))
            Future.failed(ex)
          }
          catch {
            case _: TestPendingException => Future.successful(Pending)
            case ex: TestCanceledException => Future.successful(Canceled(ex))
            case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
            case ex: Throwable => Future.failed(new ExecutionException(ex))
          }
      } flatMap { outcome =>
        try {
          callback(Good(outcome))
          Future.successful(outcome)
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

  /**
   * Registers a callback function to be executed if this future completes with
   * <code>Succeeded</code>, returning a new future that completes only after the
   * callback has finished execution.
   *
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes with <code>Succeeded</code>, the
   *    passed callback function has completed execution.
   */
  def onSucceededThen(callback: => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        if (outcome.isSucceeded) {
          try {
            callback
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

  /**
   * Registers a callback function to be executed if this future completes with
   * <code>Failed</code>, returning a new future that completes only after the
   * callback has finished execution.
   *
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes with <code>Failed</code>, the
   *    passed callback function has completed execution.
   */
  def onFailedThen(callback: Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        outcome match {
          case Failed(originalEx) =>
            try {
              callback(originalEx)
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

  /**
   * Registers a callback function to be executed if this future completes with
   * <code>Canceled</code>, returning a new future that completes only after the
   * callback has finished execution.
   *
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes with <code>Canceled</code>, the
   *    passed callback function has completed execution.
   */
  def onCanceledThen(callback: TestCanceledException => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        outcome match {
          case Canceled(originalEx) =>
            try {
              callback(originalEx)
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

  /**
   * Registers a callback function to be executed if this future completes with
   * <code>Pending</code>, returning a new future that completes only after the
   * callback has finished execution.
   *
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes with <code>Pending</code>, the
   *    passed callback function has completed execution.
   */
  def onPendingThen(callback: => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        if (outcome.isPending) {
          try {
            callback
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
      underlying flatMap { outcome =>
        try Future.successful(f(outcome))
        catch {
          case _: TestPendingException => Future.successful(Pending)
          case ex: TestCanceledException => Future.successful(Canceled(ex))
          case ex: Throwable if !anExceptionThatShouldCauseAnAbort(ex) => Future.successful(Failed(ex))
          case ex: Throwable => Future.failed(new ExecutionException(ex))
        }
      }
    }
  }

  /**
   * Registers a callback function to be executed if this future completes because
   * a suite-aborting exception was thrown, returning a new future that completes only after the
   * callback has finished execution.
   *
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes abnormally with
   *    a suite-aborting exception, the passed callback function has completed execution.
   */
  def onAbortedThen(callback: Throwable => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying recoverWith {
        case originalEx =>
          try {
            callback(originalEx)
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

  /**
   * Registers a callback function to be executed if this future completes with any
   * <code>Outcome</code> (<em>i.e.</em>, no suite-aborting exception is thrown), returning
   * a new future that completes only after the callback has finished execution.
   *
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes with a valid
   *    <code>Outcome</code>, the passed callback function has completed execution.
   */
  def onOutcomeThen(callback: Outcome => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
    FutureOutcome {
      underlying flatMap { outcome =>
        try {
          callback(outcome)
          Future.successful(outcome)
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

  /**
   * Indicates whether this <code>FutureOutcome</code> has completed.
   *
   * @return <code>true</code> if this <code>FutureOutcome</code> has completed; <code>false</code> otherwise.
   */
  def isCompleted: Boolean = underlying.isCompleted

  /**
   * Returns a value that indicates whether this <code>FutureOutcome</code> has completed,
   * and if so, indicates its result.
   *
   * <p>
   * If this <code>FutureOutcome</code> has not yet completed, this method will return
   * <code>None</code>. Otherwise, this method will return a <code>Some</code> that contains
   * either a <code>Good[Outcome]</code>, if this <code>FutureOutcome</code> completed with
   * a valid <code>Outcome</code> result, or if it completed with a thrown suite-aborting
   * exception, a <code>Bad[Throwable]</code>.
   * </p>
   *
   * @return a <code>Some</code> containing an <code>Or</code> value that indicates the result of this
   *    <code>FutureOutcome</code> if it has completed; <code>None</code> otherwise.
   */
  def value: Option[Outcome Or Throwable] =
    underlying.value match {
      case None => None
      case Some(Success(outcome)) => Some(Good(outcome))
      case Some(Failure(ex)) => Some(Bad(ex))
    }
}

object FutureOutcome {
  // Make this private so only ScalaTest can make one, so we can "promise" that
  // you'll never need to look for things like a TestCanceledException being passed
  // to onAbortedThen.
  private[scalatest] def apply(underlying: Future[Outcome]): FutureOutcome = new FutureOutcome(underlying)

  // TODO: Test and four (or five?) others, succeeded, failed, pending, aborted?
  def canceled(msg: String): FutureOutcome =
    FutureOutcome { Future.successful(Canceled(msg)) }
}

/*
 FutureOutcome.fromOutcome(Canceled("..."))
*/

