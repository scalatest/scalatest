/*
 * Copyright 2001-2024 Artima, Inc.
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

/*
Note, the reason Outcome or Throwable is used here instead of Try[Outcome] is
to avoid confusion over the Try that comes back from the Future[Outcome]. Only
run-aborting exceptions will be contained in scala.util.Failures in this case.
Other exceptions will show up as Success(org.scalatest.Failed) or Success(org.scalatest.Canceled).
And this confusion of Success(Failed) is what the Or is intended to alleviate.
*/

/**
 * Wrapper class for <code>Future[Outcome]</code> that presents a more convenient API for 
 * manipulation in <code>withFixture</code> methods in async styles.
 *
 * <p>
 * This type serves as the result type of both test functions and <code>withFixture</code> methods
 * in ScalaTest's async styles. A <code>Future[Outcome]</code> is not used as this result type
 * for two reasons. First, <code>Outcome</code> treats exceptions specially, and as a result
 * methods on <code>Future</code> would usually not yield the desired <code>Future[Outcome]</code> result.
 * Only run-aborting exceptions should result in a failed <code>Future[Outcome]</code>. Any other thrown exception
 * other than <code>TestCanceledException</code> or <code>TestPendingException</code>
 * should result in a successful<code>Future</code> containing a <code>org.scalatest.Failed</code>.
 * A thrown <code>TestCanceledException</code> should result in a successful <code>Future</code>
 * containing an <code>org.scalatest.Canceled</code>; A thrown <code>TestPendingException</code> should result in
 * a successful <code>Future</code> containing a <code>org.scalatest.Pending</code>.
 * If manipulating a <code>Future[Outcome]</code> directly, by contrast, any thrown exception would result in 
 * a failed <code>Future</code>.
 * </p>
 *
 * <p>
 * Additionally, to be consistent with corresponding transformations in traditional testing styles, 
 * methods registering callbacks should return a new future outcome that doesn't complete until
 * both the original future outcome has completed and the subsequent callback has completed execution.
 * Additionally, if the callback itself throws an exception, that exception should determine the result
 * of the future outcome returned by the callback registration method. This behavior is rather inconvenient
 * to obtain on the current <code>Future</code> API, so <code>FutureOutcome</code> provides well-named
 * methods that have this behavior.
 * </p>
 *
 * <p>
 * Lastly, the <code>FutureOutcome</code> is intended to help prevent confusion by eliminating the need
 * to work with types like <code>scala.util.Success(org.scalatest.Failed)</code>. For this purpose a
 * <code>org.scalactic.Or</code> is used instead of a <code>scala.util.Try</code> to describe results
 * of <code>FutureOutcome</code>.
 * </p>
 *
 * <p>
 * A <code>FutureOutcome</code> represents a computation that can result in an <code>Outcome</code> or an "abort." An abort means
 * that a run-aborting exception occurred during the computation. Any other, non-run-aborting exception will be represented
 * as an non-<code>Succeeded</code> <code>Outcome</code>: one of <code>Failed</code>, <code>Canceled</code>, or <code>Pending</code>.
 * </p>
 * 
 * <p>
 * The methods of <code>FutureOutcome</code> include the following callback registration methods:
 * </p>
 *
 * <ul>
 * <li><code>onSucceededThen</code> - registers a callback to be executed if the future outcome is <code>Succeeded</code>.</li>
 * <li><code>onFailedThen</code> - registers a callback to be executed if the future outcome is <code>Failed</code>.</li>
 * <li><code>onCanceledThen</code> - registers a callback to be executed if the future outcome is <code>Canceled</code>.</li>
 * <li><code>onPendingThen</code> - registers a callback to be executed if the future outcome is <code>Pending</code>.</li>
 * <li><code>onOutcomeThen</code> - registers a callback to be executed if the future outcome is actually an <code>Outcome</code>
 *      and not an abort.</li>
 * <li><code>onAbortedThen</code> - registers a callback to be executed if the future outcome aborts.</li>
 * <li><code>onCompletedThen</code> - registers a callback to be executed upon completion no matter how the future outcome completes.</li>
 * </ul>
 *
 * <p>
 * The callback methods listed previously can be used to perform a side effect once a <code>FutureOutcome</code> completes. To change an
 * <code>Outcome</code> into a different <code>Outcome</code> asynchronously, use the <code>change</code> registration method, which takes a function
 * from <code>Outcome</code> to <code>Outcome</code>. The other methods on <code>FutureOutcome</code>, <code>isCompleted</code> and
 * <code>value</code>, allow you to poll a <code>FutureOutcome</code>. None of the methods on <code>FutureOutcome</code> block.
 * Lastly, because an implicit <a href="enablers/Futuristic.html"><code>Futuristic</code></a> instance is provided for
 * <code>FutureOutcome</code>, you can use <a href="CompleteLastly.html"><code>complete</code>-<code>lastly</code> syntax</a>
 * with <code>FutureOutcome</code>.
 * </p>
 */
// SKIP-DOTTY-START 
class FutureOutcome(private[scalatest] val underlying: Future[Outcome]) {
// SKIP-DOTTY-END
//DOTTY-ONLY open class FutureOutcome(private[scalatest] val underlying: Future[Outcome]) {
  // TODO: add tests for pretty toString

  /**
   * Registers a callback function to be executed after this future completes, returning
   * a new future that completes only after the callback has finished execution.
   *
   * <p>
   * The resulting <code>FutureOutcome</code> will have the same result as this <code>FutureOutcome</code>, unless
   * the callback completes abruptly with an exception. In that case, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param callback a side-effecting function to execute when this <code>FutureOutcome</code> completes
   * @param executionContext an execution context that provides a strategy for executing the callback function
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    and, subsequently, the passed callback function have completed execution.
   */
  def onCompletedThen(callback: (Outcome Or Throwable) => Unit)(implicit executionContext: ExecutionContext): FutureOutcome = {
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
   * <p>
   * The resulting <code>FutureOutcome</code> will have the same result as this <code>FutureOutcome</code>, unless
   * the callback completes abruptly with an exception. In that case, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param callback a side-effecting function to execute if and when this <code>FutureOutcome</code> completes with <code>Succeeded</code>
   * @param executionContext an execution context that provides a strategy for executing the callback function
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
   * <p>
   * The resulting <code>FutureOutcome</code> will have the same result as this <code>FutureOutcome</code>, unless
   * the callback completes abruptly with an exception. In that case, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param callback a side-effecting function to execute if and when this <code>FutureOutcome</code> completes with <code>Failed</code>
   * @param executionContext an execution context that provides a strategy for executing the callback function
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
   * <p>
   * The resulting <code>FutureOutcome</code> will have the same result as this <code>FutureOutcome</code>, unless
   * the callback completes abruptly with an exception. In that case, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param callback a side-effecting function to execute if and when this <code>FutureOutcome</code> completes with <code>Canceled</code>
   * @param executionContext an execution context that provides a strategy for executing the callback function
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
   * <p>
   * The resulting <code>FutureOutcome</code> will have the same result as this <code>FutureOutcome</code>, unless
   * the callback completes abruptly with an exception. In that case, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param callback a side-effecting function to execute if and when this <code>FutureOutcome</code> completes with <code>Pending</code>
   * @param executionContext an execution context that provides a strategy for executing the callback function
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

  /**
   * Registers a transformation function to be executed if this future completes with any
   * <code>Outcome</code> (<em>i.e.</em>, no run-aborting exception is thrown), returning
   * a new <code>FutureOutcome</code> representing the result of passing
   * this <code>FutureOutcome</code>'s <code>Outcome</code> result to the given transformation function.
   *
   * <p>
   * If the passed function completes abruptly with an exception, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param f a transformation function to execute if and when this <code>FutureOutcome</code> completes with an <code>Outcome</code>
   * @param executionContext an execution context that provides a strategy for executing the transformation function
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes with a valid
   *    <code>Outcome</code>, the passed callback function has completed execution.
   */
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
   * a run-aborting exception was thrown, returning a new future that completes only after the
   * callback has finished execution.
   *
   * <p>
   * The resulting <code>FutureOutcome</code> will have the same result as this <code>FutureOutcome</code>, unless
   * the callback completes abruptly with an exception. In that case, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param callback a side-effecting function to execute if and when this <code>FutureOutcome</code> completes with an abort.
   * @param executionContext an execution context that provides a strategy for executing the callback function
   * @return a new <code>FutureOutcome</code> that will complete only after this <code>FutureOutcome</code>
   *    has completed and, if this <code>FutureOutcome</code> completes abnormally with
   *    a run-aborting exception, the passed callback function has completed execution.
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
   * <code>Outcome</code> (<em>i.e.</em>, no run-aborting exception is thrown), returning
   * a new future that completes only after the callback has finished execution.
   *
   * <p>
   * The resulting <code>FutureOutcome</code> will have the same result as this <code>FutureOutcome</code>, unless
   * the callback completes abruptly with an exception. In that case, the resulting <code>FutureOutcome</code>
   * will be determined by the type of the thrown exception:
   * </p>
   *
   * <ul>
   * <li><code>TestPendingException</code></li> - <code>Good(Pending)</code>
   * <li><code>TestCanceledException</code></li> - <code>Good(Canceled(&lt;the exception&gt;))</code>
   * <li>Any non-run-aborting <code>Throwable</code></li> - <code>Good(Failed(&lt;the exception&gt;))</code>
   * <li>A run-aborting <code>Throwable</code></li> - <code>Bad(&lt;the run-aborting exception&gt;)</code>
   * </ul>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
   * </p>
   *
   * @param callback a side-effecting function to execute if and when this <code>FutureOutcome</code> completes with an <code>Outcome</code>
   *    (<em>i.e.</em>, not an abort)
   * @param executionContext an execution context that provides a strategy for executing the callback function
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
   * <p>
   * This method does not block.
   * </p>
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
   * a valid <code>Outcome</code> result, or if it completed with a thrown run-aborting
   * exception, a <code>Bad[Throwable]</code>.
   * </p>
   *
   * <p>
   * For more information on <em>run-aborting</em> exceptions, see the <a href="Suite.html#errorHandling">Run-aborting exceptions</a> section
   * in the main Scaladoc for trait <code>Suite</code>.
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

  /**
   * Converts this <code>FutureOutcome</code> to a <code>Future[Outcome]</code>.
   *
   * @return the underlying <code>Future[Outcome]</code>
   */
  def toFuture: Future[Outcome] = underlying
}

/**
 * Companion object to <code>FutureOutcomes</code> that contains factory methods for creating already-completed
 * <code>FutureOutcomes</code>.
 */
object FutureOutcome {
  // Make this private so only ScalaTest can make one, so we can "promise" that
  // you'll never need to look for things like a TestCanceledException being passed
  // to onAbortedThen.
  private[scalatest] def apply(underlying: Future[Outcome]): FutureOutcome = new FutureOutcome(underlying)

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Canceled</code> result.
   */
  def canceled(): FutureOutcome =
    FutureOutcome { Future.successful(Canceled()) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Canceled</code> result
   * whose <code>TestCanceledException</code> contains the specified message.
   *
   * @message the message string to include in the <code>Canceled</code>'s <code>TestCanceledException</code>.
   */
  def canceled(message: String): FutureOutcome =
    FutureOutcome { Future.successful(Canceled(message)) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Canceled</code> result
   * whose <code>TestCanceledException</code> contains the specified cause.
   *
   * @cause exception to include as the <code>Canceled</code>'s <code>TestCanceledException</code> cause.
   */
  def canceled(cause: Throwable): FutureOutcome =
    FutureOutcome { Future.successful(Canceled(cause)) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Canceled</code> result
   * whose <code>TestCanceledException</code> contains the specified message and cause.
   *
   * @message the message string to include in the <code>Canceled</code>'s <code>TestCanceledException</code>.
   * @cause exception to include as the <code>Canceled</code>'s <code>TestCanceledException</code> cause.
   */
  def canceled(message: String, cause: Throwable) =
    FutureOutcome { Future.successful(Canceled(message, cause)) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Succeeded</code> result.
   */
  def succeeded: FutureOutcome =
    FutureOutcome { Future.successful(Succeeded) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Failed</code> result.
   */
  def failed(): FutureOutcome =
    FutureOutcome { Future.successful(Failed()) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Failed</code> result
   * containing a <code>TestFailedException</code> with the specified message.
   *
   * @message the message string to include in the <code>Failed</code>'s <code>TestFailedException</code>.
   */
  def failed(message: String): FutureOutcome =
    FutureOutcome { Future.successful(Failed(message)) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Failed</code> result
   * containing a <code>TestFailedException</code> with the specified message and cause.
   *
   * @message the message string to include in the <code>Failed</code>'s <code>TestFailedException</code>.
   * @cause exception to include as the <code>Failed</code>'s <code>TestFailedException</code> cause.
   */
  def failed(message: String, cause: Throwable) =
    FutureOutcome { Future.successful(Failed(message, cause)) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Failed</code> result
   * containing a <code>TestFailedException</code> with the specified cause.
   *
   * @cause exception to include as the <code>Failed</code>'s <code>TestFailedException</code> cause.
   */
  def failed(cause: Throwable): FutureOutcome =
    FutureOutcome { Future.successful(Failed(cause)) }

  /**
   * Factory method that creates an already completed <code>FutureOutcome</code> with a <code>Pending</code> result.
   */
  def pending: FutureOutcome =
    FutureOutcome { Future.successful(Pending) }
}

/*
 FutureOutcome.fromOutcome(Canceled("..."))
*/

