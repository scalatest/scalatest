/*
 * Copyright 2001-2013 Artima, Inc.
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

import scala.collection.GenSet
import java.io.Serializable
import scala.concurrent.{ExecutionException, Future, Promise}
import scala.util.{Try, Success, Failure}

/**
 * The result status of running a test or a suite, which is used to support parallel and asynchronous execution of tests.
 *
 * <p>
 * This trait is the result type of the "run" lifecycle methods of trait <a href="Suite.html#lifecycle-methods"><code>Suite</code></a>:
 * <code>run</code>, <code>runNestedSuites</code>, <code>runTests</code>, and <code>runTest</code>. It can be used to determine whether
 * a test or suite has completed, and if so, whether it succeeded, and if not, whether an exception was thrown that was
 * not yet reported via a ScalaTest event. A <code>Status</code> is like a domain-specific <code>Future[Boolean]</code>, where:
 * </p>
 *
 * <ul>
 * <li>an activity in which no test failed and no suite aborted is represented by <code>Success(true)</code></li>
 * <li>an activity during which at least one test failed or one suite aborted, but all exceptions that occurred
 *     were reported by a ScalaTest events (such as <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a>)
 *     is represented by <code>Success(false)</code></li>
 * <li>an activity during which at least one test failed or one suite aborted and at least one exception occurred that was
 *     <em>not</em> reported via a ScalaTest event is represented by <code>Failure(unreportedException)</code></li>
 * </ul>
 *
 * <p>
 * Note that pending and canceled tests will not cause a <code>Status</code> to fail. Only failed tests
 * and aborted suites will cause a <code>Status</code> to fail.
 * </p>
 *
 * <p>
 * One use case of <code>Status</code> is to ensure that "after" code (such as an <code>afterEach</code> or <code>afterAll</code> method)
 * does not execute until after the relevant entity (one test, one suite, or all of a suite's tests or nested suites) has completed.
 * Another use case is to implement the default behavior of asynchronous styles, in which subsequent each test does not begin
 * execution until after the previous test has completed.
 * </p>
 */
sealed trait Status { thisStatus =>

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Blocking call that waits until the entity represented by this <code>Status</code> (one test, one suite, or all of
   * a suite's tests or nested suites) has completed, then returns <code>true</code> if no tests failed and no
   * suites aborted, else returns <code>false</code>, or if an unreported exception has been installed, completes
   * abruptly with that exception.
   * 
   * <p>
   * This method only reports <code>false</code> if there was a failed test or aborted suite in the context of the "run" lifecycle method
   * from which it was returned.
   * For example, if you call <code>succeeds</code> on a <code>Status</code> returned by <code>runTest</code>, <code>succeeds</code>
   * will (after that test has completed) return <code>false</code> if the test whose name was passed to <code>runTest</code> fails,
   * else it will return <code>true</code>.
   * In other words, so long as the test doesn't fail &#8212;whether the test succeeds, is canceled, or is pending&#8212;<code>succeeds</code>
   * will return <code>true</code>. 
   * If you call <code>succeeds</code> on a <code>Status</code> returned by <code>runTests</code>, by contrast, <code>succeeds</code>
   * will (after the suite's
   * tests have completed) return <code>true</code> only if none of the tests in the suite fail. If any test in the suite fails,
   * <code>succeeds</code> will return <code>false</code>.
   * If you call <code>succeeds</code> on a <code>Status</code> returned by <code>runNestedSuites</code>, <code>succeeds</code> will
   * return true only if no tests fail and no suites abort when running all nested suites (and their nested suites, recursively).
   * Similarly, if you call <code>succeeds</code> on a <code>Status</code> returned by <code>run</code>, <code>succeeds</code> will
   * return true only if no tests fail and no suites abort when running all tests nested suites (and their nested suites, recursively).
   * </p>
   *
   * <p>
   * If this <code>Status</code> fails with an "unreported exception," an exception that occurred during the
   * activity represented by this <code>Status</code> that was not reported to the <code>Reporter</code> via a
   * ScalaTest event, the <code>succeeds</code> method will complete abruptly with that exception. If the
   * original exception was a run-aborting exception, such as <code>StackOverflowError</code>, the 
   * <code>unreportedException</code> method will return a <code>java.util.ExecutionException</code> that contains
   * the original run-aborting exception as its cause. The <code>succeeds</code> method will in that case
   * complete abruptly with the <code>ExecutionException</code> that wraps the original run-aborting exception.
   * </p>
   *
   * <p>
   * <em>Note: because blocking is not possible on Scala.js, this method is not available on Scala.js.</em>
   * </p>
   *
   * @return after waiting until completion, <code>true</code> if no tests failed and no suites aborted, <code>false</code> otherwise
   * @throws unreportedException if an exception occurred during the activity represented by this <code>Status</code> that was not reported
   *            via a ScalaTest event and therefore was installed as an unreported exception on this <code>Status</code>.
   */
  def succeeds(): Boolean
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Non-blocking call that indicates whether the entity represented by this
   * <code>Status</code> (one test, one suite, or all of a suite's tests or nested suites) has completed. Because this is non-blocking,
   * you can use this to poll the completion status.
   * 
   * <p>
   * Note: this method will not indicate whether a test has failed, suite has aborted, or an unreported exception has been installed.
   * It just indicates whether the <code>Status</code> has completed or not by returning <code>true</code> or <code>false</code>.
   * </p>
   *
   * @return <code>true</code> if the test or suite run is already completed, <code>false</code> otherwise.
   */
  def isCompleted: Boolean

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Blocking call that waits until the entity represented by this <code>Status</code> (one test, one suite, or all of
   * a suite's tests or nested suites) has completed, then either returns normally, or if an unreported exception has
   * been installed, completes abruptly with that unreported exception.
   *
   * <p>
   * If this <code>Status</code> fails with an "unreported exception," an exception that occurred during the
   * activity represented by this <code>Status</code> that was not reported to the <code>Reporter</code> via a
   * ScalaTest event, the <code>waitUntilCompleted</code> method will complete abruptly with that exception. If the
   * original exception was a run-aborting exception, such as <code>StackOverflowError</code>, the 
   * <code>unreportedException</code> method will return a <code>java.util.ExecutionException</code> that contains
   * the original run-aborting exception as its cause. The <code>waitUntilCompleted</code> method will in that case
   * complete abruptly with the <code>ExecutionException</code> that wraps the original run-aborting exception.
   * </p>
   *
   * <p>
   * <em>Note: because blocking is not possible on Scala.js, this method is not available on Scala.js.</em>
   * </p>
   *
   * @throws unreportedException if an exception occurred during the activity represented by this <code>Status</code> that was not reported
   *            via a ScalaTest event and therefore was installed as an unreported exception on this <code>Status</code>.
   */
  def waitUntilCompleted()
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Registers the passed callback function to be executed when this status completes.
   *
   * <p>
   * If an unreported exception has been installed on this <code>Status</code>, the 
   * <code>Try</code> passed into the callback function will be a <code>Failure</code> containing that exception. Otherwise
   * the <code>Try</code> will be a <code>Success</code> containing true if no tests failed
   * or suites aborted during the activity represented by this <code>Status</code>, else <code>false</code>. 
   * </p>
   *
   * <p>
   * The callback functions registered with <code>whenCompleted</code> will be executed <em>after</em> the <code>Status</code>
   * has completed, in an undefined order. If the <code>Status</code> has already completed, functions passed to this method will be
   * executed immediately by the calling thread before returning.
   * </p>
   *
   * <p>
   * Any exception thrown by a callback function will be propagated back on the thread used to invoke the callback.
   * </p>
   *
   * <p>
   * Internally ScalaTest uses this method to register callbacks that
   * fire completion events (<code>TestSucceeded</code>, <code>TestFailed</code>,
   * <code>SuiteCompleted</code>, <em>etc.</em>) to the <code>Reporter</code>.
   * </p>
   *
   * @param callback the callback function to execute once this <code>Status</code> has completed
   */
  def whenCompleted(callback: Try[Boolean] => Unit)

  // TODO: We are not yet propagating installed unreported exceptions in thenRun. Write the tests and implement the code.
  /**
   * Registers a <code>Status</code>-producing by-name function to execute after this
   * <code>Status</code> completes, returning a <code>Status</code> that mirrors the <code>Status</code>
   * returned by the by-name.
   *
   * <p>
   * The <code>Status</code> returned by this method will complete when the status produced by the 
   * <code>Status</code> produced by the passed-by name completes. The returned <code>Status</code>
   * will complete with the same <code>succeeds</code> and <code>unreportedException</code> values.
   * But unlike the <code>Status</code> produced by the by-name, the returned <code>Status</code> will
   * be available immediately.
   * </p>
   *
   * <p>
   * If the by-name function passed to this method completes abruptly with a <em>non-run-aborting</em> exception,
   * that exception will be caught and installed as the <code>unreportedException</code> on the
   * <code>Status</code> returned by this method. The <code>Status</code> returned by this method
   * will then complete. The thread that attempted to evaluate the by-name function will be allowed
   * to continue (<code>i.e.</code>, the non-run-aborting exception will <em>not</em> be rethrown
   * on that thread).
   * </p>
   *
   * <p>
   * If the by-name function passed to this method completes abruptly with a <em>run-aborting</em> exception,
   * such as <code>StackOverflowError</code>, that exception will be caught and a new
   * <code>java.util.concurrent.ExecutionException</code> that contains the run-aborting exception as its
   * cause will be installed as the <code>unreportedException</code> on the
   * <code>Status</code> returned by this method. The <code>Status</code> returned by this method
   * will then complete. The original run-aborting exception will then be rethrown on the
   * thread that attempted to evaluate the by-name function.
   * </p>
   *
   * <p>
   * If an unreported exception is installed on this <code>Status</code>, the passed by-name function will
   * <em>not</em> be executed. Instead, the same unreported exception will be installed on the <code>Status</code>
   * returned by this method.
   * </p>
   *
   * <p>
   * Internally, ScalaTest uses this method in async styles to ensure that by default, each subsequent test in an async-style
   * suite begins execution only after the previous test has completed. This method is <em>not</em> used if
   * <code>ParallelTestExection</code> is mixed into an async style. Instead, tests are allowed to begin
   * execution concurrently.
   * </p>
   *
   * @param status A <code>Status</code>-producing by-name function to invoke after this <code>Status</code> has completed.
   * @return a <code>Status </code> that represents the status of executing the by-name function passed to this method.
   */
  final def thenRun(f: => Status): Status = {
    val returnedStatus = new ScalaTestStatefulStatus
    whenCompleted { _ =>
      try {
        val innerStatus = f
        innerStatus.whenCompleted { tri =>
          tri match {
            case Success(false) =>
              returnedStatus.setFailed()
            case Failure(ex) =>
              returnedStatus.setFailed()
              returnedStatus.setFailedWith(ex)
            case _ =>
          }
          returnedStatus.setCompleted()
        }
      }
      catch {
        case ex: Throwable =>
          if (Suite.anExceptionThatShouldCauseAnAbort(ex)) {
            returnedStatus.setFailedWith(new ExecutionException(ex))
            returnedStatus.setCompleted()
            throw ex
          }
          else {
            returnedStatus.setFailedWith(ex)
            returnedStatus.setCompleted()
          }
      }
    }
    returnedStatus
  }

  /**
   * Converts this <code>Status</code> to a <code>Future[Boolean]</code> where <code>Success(true)</code> means
   * no tests failed and suites aborted, <code>Success(false)</code>, means at least one test failed or one
   * suite aborted and any thrown exception was was reported to the <code>Reporter</code> via a ScalaTest
   * event, <code>Failure(unreportedException)</code> means
   * an exception, <code>unreportedException</code>, was thrown that was not reported to the <code>Reporter</code>
   * via a ScalaTest event.
   *
   * @return a <code>Future[Boolean]</code> representing this <code>Status</code>.
   */
  final def toFuture: Future[Boolean] = {
    val promise = Promise[Boolean]
    whenCompleted { t => promise.complete(t) }
    promise.future
  }

  // TODO: Make sure to test what happens when before and after code throw exceptions.
  /**
   * An exception that was thrown during the activity represented by this <code>Status</code> that
   * was not reported via a ScalaTest event fired to the <code>Reporter</code>.
   *
   * <p>
   * When a test executes, "non-run-aborting" thrown exceptions are reported by events
   * fired to the reporter. A <a href="exceptions/TestPendingException.html"><code>TestPendingException</code></a> is reported via a
   * <a href="events/TestPending.html"><code>TestPending</code></a> event. A <a href="exceptions/TestCanceledException.html"><code>TestCanceledException</code></a> is reported via a
   * <a href="events/TestCanceled.html"><code>TestCanceled</code></a> event. Any other non-run-aborting exceptions, including
   * <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a> will be reported via a
   * <a href="events/TestFailed.html"><code>TestFailed</code></a> event.
   * </p>
   *
   * <p>
   * Run-aborting exceptions indicate critical
   * problems, such as <code>OutOfMemoryError</code>, that instead of being reported via a test completion event
   * should instead cause the entire suite to abort. In synchronous testing styles, this exception will be allowed
   * to just propagate up the call stack. But in async styles, the thread or threads executing the test will often
   * be taken from the async suite's execution context. Instead of propagating these run-aborting exceptions up
   * the call stack, they will be installed as an "unreported exception" in the test's <code>Status</code>.
   * They are "unreported" because no test completion event will be fired to report them. For more explanation and
   * a list of run-aborting exception types, see <a href="Suite.html#errorHandling">Treatment of <code>java.lang.Error</code>s</a>.
   * </p>
   *
   * <p>
   * Another way for an unreported exception to occur is if an exception of any type is thrown outside of the
   * body of an actual test. For example, traits <code>BeforeAndAfter</code>,  <code>BeforeAndAfterEach</code>,
   * and <code>BeforeAndAfterEachTestData</code> execute code before and after tests. Traits
   * <code>BeforeAndAfterAll</code> and </p><code>BeforeAndAfterAllConfigMap</code> execute code before
   * and after all tests and nested suites of a suite. If any "before" or "after"
   * code completes abruptly with an exception (of any type, not just run-aborting types) on a thread taken
   * from an async suite's execution context, this exception will
   * installed as an <code>unreportedException</code> of the relevant <code>Status</code>.
   * </p>
   *
   * <p>
   * In addition, ScalaTest <code>Suite</code> exposes four "run" lifecycle methods--<code>run</code>,
   * <code>runNestedSuites</code>, <code>runTests</code>, and <code>runTest</code>--that users can override to customize
   * the framework. If a "run" lifecycle methods completes abruptly with an exception, that exception occurs outside
   * the context of a test body. As a result, such exceptions will be
   * installed as an <code>unreportedException</code> of the relevant <code>Status</code>.
   * </p>
   *
   * <p>
   * The <code>toFuture</code> method on <code>Status</code> returns a <code>Future[Boolean]</code>. If the <code>Future</code>
   * succeeds with the <code>Boolean</code> value of <code>true</code>, that indicates no tests failed and no suites aborted
   * during the activity represented
   * by this <code>Status</code>. If a test failed or suite aborted, and that event was reported by a fired ScalaTest
   * <a href="events.Event.html"><code>Event</code></a>, the
   * <code>Future</code> will succeed with the value <code>false</code>. If an unreported exception has been installed
   * on the <code>Status</code>, however, the <code>Future</code> will fail with that exception.
   * </p>
   *
   * @return a optional unreported <code>Throwable</code>
   */
  def unreportedException: Option[Throwable] = None

  // TODO: Currently we are attempting to execution the after code. This is a change from how
  // ScalaTest has behaved from the beginning, and it is inconsistent also with thenRun. So 
  // I think I want to go back to how we were doing it before, if the before code blows up
  // or runTest, etc., then we don't attempt the after code.
  /**
   * Registers a by-name function (producing an optional exception) to execute
   * after this <code>Status</code> completes.
   *
   * <p>
   * If the by-name function passed to this method completes abruptly with a <em>non-run-aborting</em> exception,
   * that exception will be caught and installed as the <code>unreportedException</code> on the
   * <code>Status</code> returned by this method. The <code>Status</code> returned by this method
   * will then complete. The thread that attempted to evaluate the by-name function will be allowed
   * to continue (<code>i.e.</code>, the non-run-aborting exception will <em>not</em> be rethrown
   * on that thread).
   * </p>
   *
   * <p>
   * If the by-name function passed to this method completes abruptly with a <em>run-aborting</em> exception,
   * such as <code>StackOverflowError</code>, that exception will be caught and a new
   * <code>java.util.concurrent.ExecutionException</code> that contains the run-aborting exception as its
   * cause will be installed as the <code>unreportedException</code> on the
   * <code>Status</code> returned by this method. The <code>Status</code> returned by this method
   * will then complete. The original run-aborting exception will then be rethrown on the
   * thread that attempted to evaluate the by-name function.
   * </p>
   *
   * <p>
   * If an unreported exception is installed on this <code>Status</code>, the passed by-name function will
   * <em>not</em> be executed. Instead, the same unreported exception will be installed on the <code>Status</code>
   * returned by this method.
   * </p>
   *
   * <p>
   * Internally, ScalaTest uses this method in traits <code>BeforeAndAfter</code>,
   * <code>BeforeAndAfterEach</code>, and <code>BeforeAndAfterEachTestData</code> to ensure "after" code is
   * executed after the relevant test has completed, and in traits <code>BeforeAndAfterAll</code> and
   * <code>BeforeAndAfterAllConfigMap</code> to ensure "after" code is executed after the
   * relevant tests and nested suites have completed.
   * </p>
   *
   * @param f A by-name function to invoke after this <code>Status</code> has completed.
   * @return a <code>Status</code> that represents this <code>Status</code>,
   *         modified by any exception thrown by the passed by-name function.
   */
  final def withAfterEffect(f: => Unit): Status = {
    val returnedStatus = new ScalaTestStatefulStatus
    whenCompleted { tri =>
      tri match {
        case Success(result) =>
          try {
            f
            if (!result) returnedStatus.setFailed()
          }
          catch {
            case ex: Throwable if Suite.anExceptionThatShouldCauseAnAbort(ex) =>
              val execEx = new ExecutionException(ex)
              returnedStatus.setFailedWith(execEx)
              throw ex

            case ex: Throwable => returnedStatus.setFailedWith(ex)
          }
          finally {
            returnedStatus.setCompleted()
          }

        case Failure(originalEx) =>
          try {
            f
            returnedStatus.setFailedWith(originalEx)
          }
          catch {
            case ex: Throwable =>
              returnedStatus.setFailedWith(originalEx)
              println("ScalaTest can't report this exception because another preceded it, so printing its stack trace:")
              ex.printStackTrace()
          }
          finally {
            returnedStatus.setCompleted()
          }
      }
    }
    returnedStatus
  }
}

/**
 * Singleton status that represents an already completed run with no tests failed and no suites aborted.
 *
 * <p>
 * Note: the difference between this <code>SucceededStatus</code> object and the similarly named <a href="Succeeded$.html"><code>Succeeded</code></a>
 * object is that the <code>Succeeded</code> object indicates one test succeeded, whereas this <code>SucceededStatus</code> object indicates the absence
 * of any failed tests or aborted suites during a run. Both are used as the result type of <a href="Suite.html#lifecycle-methods"><code>Suite</code></a> lifecycle methods, but <code>Succeeded</code>
 * is a possible result of <code>withFixture</code>, whereas <code>SucceededStatus</code> is a possible result of <code>run</code>, <code>runNestedSuites</code>,
 * <code>runTests</code>, or <code>runTest</code>. In short, <code>Succeeded</code> is always just about one test, whereas <code>SucceededStatus</code> could be
 * about something larger: multiple tests or an entire suite.
 * </p>
 */
object SucceededStatus extends Status with Serializable {

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Always returns <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def succeeds() = true
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Always returns <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def isCompleted = true

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Always returns immediately.
   */
  def waitUntilCompleted(): Unit = {}
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Executes the passed function immediately on the calling thread.
   */
  def whenCompleted(f: Try[Boolean] => Unit): Unit = { f(Success(true)) }
}

/**
 * Singleton status that represents an already completed run with at least one failed test or aborted suite.
 *
 * <p>
 * Note: the difference between this <code>FailedStatus</code> object and the similarly named <a href="Failed.html"><code>Failed</code></a>
 * class is that a <code>Failed</code> instance indicates one test failed, whereas this <code>FailedStatus</code> object indicates either one or more tests failed
 * and/or one or more suites aborted during a run. Both are used as the result type of <code>Suite</code> lifecycle methods, but <code>Failed</code>
 * is a possible result of <code>withFixture</code>, whereas <code>FailedStatus</code> is a possible result of <code>run</code>, <code>runNestedSuites</code>,
 * <code>runTests</code>, or <code>runTest</code>. In short, <code>Failed</code> is always just about one test, whereas <code>FailedStatus</code> could be
 * about something larger: multiple tests or an entire suite.
 * </p>
 */
object FailedStatus extends Status with Serializable {

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Always returns <code>false</code>.
   * 
   * @return <code>true</code>
   */
  def succeeds() = false
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Always returns <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def isCompleted = true

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Always returns immediately.
   */
  def waitUntilCompleted(): Unit = {}
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Executes the passed function immediately on the calling thread.
   */
  def whenCompleted(f: Try[Boolean] => Unit): Unit = { f(Success(false)) }
}

// Used internally in ScalaTest. We don't use the StatefulStatus, because
// then user code could pattern match on it and then access the setCompleted
// and setFailed methods. We wouldn't want that.
private[scalatest] final class ScalaTestStatefulStatus extends Status with Serializable {

  @transient private final val latch = new CountDownLatch(1)

  private var succeeded = true

  private final val queue = new ConcurrentLinkedQueue[Try[Boolean] => Unit]

  private var asyncException: Option[Throwable] = None

  override def unreportedException: Option[Throwable] = {
    synchronized {
      asyncException
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  def succeeds() = {
    waitUntilCompleted()
    synchronized { succeeded }
  }
  // SKIP-SCALATESTJS,NATIVE-END

  def isCompleted = synchronized { latch.getCount == 0L }

  // SKIP-SCALATESTJS,NATIVE-START
  def waitUntilCompleted(): Unit = {
    synchronized { latch }.await()
    unreportedException match {
      case Some(ue) => throw ue
      case None => // Do nothing
    }
  }
  // SKIP-SCALATESTJS,NATIVE-END

  def setFailed(): Unit = {
    synchronized {
      if (isCompleted)
        throw new IllegalStateException("status is already completed")
      succeeded = false
    }
  }

  /**
   * Sets the status to failed with an unreported exception, without changing the completion status.
   *
   * <p>
   * This method may be invoked repeatedly, even though invoking it once is sufficient to set the state of the <code>Status</code> to failed, but only
   * up until <code>setCompleted</code> has been called. Once <code>setCompleted</code> has been called, invoking this method will result in a
   * thrown <code>IllegalStateException</code>. Also, only the first exception passed will be reported as the unreported exception. Any exceptions
   * passed via subsequent invocations of <code>setFailedWith</code> after the first will have their stack traces printed to standard output.
   * </p>
   *
   * @throws IllegalStateException if this method is invoked on this instance after <code>setCompleted</code> has been invoked on this instance.
   * @param ex an unreported exception
   */
  def setFailedWith(ex: Throwable): Unit = {
    synchronized {
      if (isCompleted)
        throw new IllegalStateException("status is already completed")
      succeeded = false
      if (asyncException.isEmpty)
        asyncException = Some(ex)
      else {
        println("ScalaTest can't report this exception because another preceded it, so printing its stack trace:")
        ex.printStackTrace()
      }
    }
  }

  def setCompleted(): Unit = {
    // Moved the for loop after the countdown, to avoid what I think is a race condition whereby we register a call back while
    // we are iterating through the list of callbacks prior to adding the last one.
    val it =
      synchronized {
        // OLD, OUTDATED COMMENT, left in here to ponder the depths of its meaning a bit longer:
        // Only release the latch after the callbacks finish execution, to avoid race condition with other thread(s) that wait
        // for this Status to complete.
        latch.countDown()
        queue.iterator
      }
    val tri: Try[Boolean] =
      unreportedException match {
        case Some(ex) => Failure(ex)
        case None => Success(succeeded)
      }
    for (f <- it)
      f(tri)
  }

  def whenCompleted(f: Try[Boolean] => Unit): Unit = {
    var executeLocally = false
    synchronized {
      if (!isCompleted)
        queue.add(f)
      else
        executeLocally = true
    }
    if (executeLocally) {
      val tri: Try[Boolean] =
        unreportedException match {
          case Some(ex) => Failure(ex)
          case None => Success(succeeded)
        }
      f(tri)
    }
  }
}

/**
 * Status implementation that can change its state over time.
 *
 * <p>
 * A <code>StatefulStatus</code> begins its life in a successful state, and will remain successful unless <code>setFailed</code> is called.
 * Once <code>setFailed</code> is called, the status will remain at failed. The <code>setFailed</code> method can be called multiple times (even
 * though invoking it once is sufficient to permanently set the status to failed), but only up until <code>setCompleted</code> has been called.
 * After <code>setCompleted</code> has been called, any invocation of <code>setFailed</code> will be greeted with an <code>IllegalStateException</code>.
 * </p>
 *
 * <p>
 * Instances of this class are thread safe.
 * </p>
 */
final class StatefulStatus extends Status with Serializable {
  @transient private final val latch = new CountDownLatch(1)
  private var succeeded = true
  private final val queue = new ConcurrentLinkedQueue[Try[Boolean] => Unit]

  private var asyncException: Option[Throwable] = None

  override def unreportedException: Option[Throwable] = {
    synchronized {
      asyncException
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Blocking call that waits until completion, as indicated by an invocation of <code>setCompleted</code> on this instance, then returns <code>false</code> 
   * if <code>setFailed</code> was called on this instance, else returns <code>true</code>.
   * 
   * @return <code>true</code> if no tests failed and no suites aborted, <code>false</code> otherwise
   */
  def succeeds() = {
    waitUntilCompleted()
    synchronized { succeeded }
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Non-blocking call that returns <code>true</code> if <code>setCompleted</code> has been invoked on this instance, <code>false</code> otherwise.
   * 
   * @return <code>true</code> if the test or suite run is already completed, <code>false</code> otherwise.
   */
  def isCompleted = synchronized { latch.getCount == 0L }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Blocking call that returns only after <code>setCompleted</code> has been invoked on this <code>StatefulStatus</code> instance.
   */
  def waitUntilCompleted(): Unit = {
    synchronized { latch }.await()
    unreportedException match {
      case Some(ue) => throw ue
      case None => // Do nothing
    }
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Sets the status to failed without changing the completion status.
   *
   * <p>
   * This method may be invoked repeatedly, even though invoking it once is sufficient to set the state of the <code>Status</code> to failed, but only
   * up until <code>setCompleted</code> has been called. Once <code>setCompleted</code> has been called, invoking this method will result in a
   * thrown <code>IllegalStateException</code>.
   * </p>
   *
   * @throws IllegalStateException if this method is invoked on this instance after <code>setCompleted</code> has been invoked on this instance.
   */
  def setFailed(): Unit = {
    synchronized {
      if (isCompleted)
        throw new IllegalStateException("status is already completed")
      succeeded = false
    }
  }

  /**
   * Sets the status to failed with an unreported exception, without changing the completion status.
   *
   * <p>
   * This method may be invoked repeatedly, even though invoking it once is sufficient to set the state of the <code>Status</code> to failed, but only
   * up until <code>setCompleted</code> has been called. Once <code>setCompleted</code> has been called, invoking this method will result in a
   * thrown <code>IllegalStateException</code>. Also, only the first exception passed will be reported as the unreported exception. Any exceptions
   * passed via subsequent invocations of <code>setFailedWith</code> after the first will have their stack traces printed to standard output.
   * </p>
   *
   * @throws IllegalStateException if this method is invoked on this instance after <code>setCompleted</code> has been invoked on this instance.
   * @param ex an unreported exception
   */
  def setFailedWith(ex: Throwable): Unit = {
    synchronized {
      if (isCompleted)
        throw new IllegalStateException("status is already completed")
      succeeded = false
      if (asyncException.isEmpty)
        asyncException = Some(ex)
      else {
        println("ScalaTest can't report this exception because another preceded it, so printing its stack trace:")
        ex.printStackTrace()
      }
    }
  }

  /**
   * Sets the status to completed.
   *
   * <p>
   * This method may be invoked repeatedly, even though invoking it once is sufficient to set the state of the <code>Status</code> to completed.
   * </p>
   *
   * <p>
   * <strong>TODO: Specify that this method invokes the callbacks on the invoking thread after it releases the lock
   * such that the Status has completed.</strong>
   * </p>
   */
  def setCompleted(): Unit = {
    // Moved the for loop after the countdown, to avoid what I think is a race condition whereby we register a call back while
    // we are iterating through the list of callbacks prior to adding the last one.
    val it =
      synchronized {
      // OLD, OUTDATED COMMENT, left in here to ponder the depths of its meaning a bit longer:
      // Only release the latch after the callbacks finish execution, to avoid race condition with other thread(s) that wait
      // for this Status to complete.
        latch.countDown()
        queue.iterator
      }
    val tri: Try[Boolean] =
      unreportedException match {
        case Some(ex) => Failure(ex)
        case None => Success(succeeded)
      }
    for (f <- it)
      f(tri)
  }

  /**
   * Registers the passed function to be executed when this status completes.
   *
   * <p>
   * You may register multiple functions, which on completion will be executed in an undefined
   * order.
   * </p>
   */
  def whenCompleted(f: Try[Boolean] => Unit): Unit = {
    var executeLocally = false
    synchronized {
      if (!isCompleted)
        queue.add(f)
      else
        executeLocally = true
    }
    if (executeLocally) {
      val tri: Try[Boolean] =
        unreportedException match {
          case Some(ex) => Failure(ex)
          case None => Success(succeeded)
        }
      f(tri)
    }
  }
}

/**
 * Composite <code>Status</code> that aggregates its completion and failed states of set of other <code>Status</code>es passed to its constructor.
 *
 * @param status the <code>Status</code>es out of which this status is composed.
 */
final class CompositeStatus(statuses: Set[Status]) extends Status with Serializable {
  
  // TODO: Ensure this is visible to another thread, because I'm letting the reference
  // escape with my for loop below prior to finishing this object's construction.
  @transient private final val latch = new CountDownLatch(statuses.size)

  @volatile private var succeeded = true
  // This is set possibly by the whenCompleted function registered on all the
  // inner statuses. If any of them are Failures, then that first one goes in
  // as this Composite's unreported exception. Any subsequent ones are just printed.
  // Then if it is the last inner status to complete, that unreported exception is passed
  // to the callback functions registered with this composite status.
  private var asyncException: Option[Throwable] = None

  private final val queue = new ConcurrentLinkedQueue[Try[Boolean] => Unit]

  for (status <- statuses) {
    status.whenCompleted { tri =>
      val youCompleteMe: Boolean =
        synchronized {
          latch.countDown()

          tri match {
            case Success(res) =>
              if (!res)
                succeeded = false
            case Failure(ex) =>
              succeeded = false
              if (asyncException.isEmpty)
                asyncException = Some(ex)
              else {
                println("ScalaTest can't report this exception because another preceded it, so printing its stack trace:") 
                ex.printStackTrace()
              }
          }

          latch.getCount == 0
        }
      if (youCompleteMe) {
        val tri: Try[Boolean] =
          unreportedException match {
            case Some(ex) => Failure(ex)
            case None => Success(succeeded)
          }
        for (f <- queue.iterator)
          f(tri)
      }
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Blocking call that waits until all composite <code>Status</code>es have completed, then returns
   * <code>true</code> only if all of the composite <code>Status</code>es succeeded. If any <code>Status</code> passed in the <code>statuses</code> set fails, this method
   * will return <code>false</code>.
   * 
   * @return <code>true</code> if all composite <code>Status</code>es succeed, <code>false</code> otherwise.
   */
  def succeeds() = {
    synchronized { latch }.await()
    synchronized { statuses }.forall(_.succeeds())
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Non-blocking call to check if the test or suite run is completed, returns <code>true</code> if all composite <code>Status</code>es have completed, 
   * <code>false</code> otherwise.  You can use this to poll the run status.
   * 
   * @return <code>true</code> if all composite <code>Status</code>es have completed, <code>false</code> otherwise.
   */
  def isCompleted = synchronized { statuses }.forall(_.isCompleted)

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Blocking call that returns only after all composite <code>Status</code>s have completed.
   */
  def waitUntilCompleted(): Unit = {
    // statuses.foreach(_.waitUntilCompleted())
    synchronized { latch }.await()
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Registers the passed function to be executed when this status completes.
   *
   * <p>
   * You may register multiple functions, which on completion will be executed in an undefined
   * order.
   * </p>
   */
  def whenCompleted(f: Try[Boolean] => Unit): Unit = {
    var executeLocally = false
    synchronized {
      if (!isCompleted)
        queue.add(f)
      else
        executeLocally = true
    }
    if (executeLocally) {
      val tri: Try[Boolean] =
        unreportedException match {
          case Some(ex) => Failure(ex)
          case None => Success(succeeded)
        }
      f(tri)
    }
  }

  /**
   * An optional exception that has not been reported to the reporter for this run.
   *
   * <p>
   * This will be defined if any of the composite <code>Status</code>s (passed to this <code>Status</code>'s 
   * constructor) has a defined <code>unreportedException</code>. If more than one composite <code>Status</code>
   * has a defined <code>unreportedException</code>, one of them (not specified) will be reported by this method
   * and the others will have their stack traces printed to standard output.
   * </p>
   */
  override def unreportedException: Option[Throwable] = {
    synchronized {
      if (asyncException.isDefined) asyncException
      else {
        val optStatusWithUnrepEx = statuses.find(_.unreportedException.isDefined)
        for {
          status <- optStatusWithUnrepEx
          unrepEx <- status.unreportedException 
        } yield unrepEx
      }
    }
  }
}

