/*
 * Copyright 2001-2014 Artima, Inc.
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
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions // To convert Assertion to Future[Assertion]
import enablers.Futuristic

/*
 * TODO: Fill in here and also add a lifecycle-methods to Suite, which is linked to
 * from SuiteMixin.
 */
/**
 * The base trait of ScalaTest's async testing styles, which enables testing of
 * asynchronous code without blocking.
 *
 * <p>
 * This trait provides a final override of <code>withFixture(NoArgTest)</code>, declared in
 * supertrait <code>Suite</code>, because the <code>withFixture(NoArgTest)</code> lifecycle
 * method assumes synchronous testing. Here is its signature:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: NoArgTest): Outcome
 * </pre>
 *
 * <p>
 * The test function interface, <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a>, offers an <code>apply</code> method
 * that also returns <a href="Outcome.html"><code>Outcome</code></a>:
 * </p>
 *
 * <pre class="stHighlight">
 * // In trait NoArgTest:
 * def apply(): Outcome
 * </pre>
 *
 * <p>
 * Because the result of a test is an <code>Outcome</code>, when the test function returns, the test body must have determined an outcome already. It
 * will already be one of <a href="Succeeded$.html"><code>Succeeded</code></a>, <a href="Failed.html"><code>Failed</code></a>, <a href="Canceled.html"><code>Canceled</code></a>, or <a href="Pending$.html"></code>Pending</code></a>. This is
 * also true when <code>withFixture(NoArgTest)</code> returns: because the result type of <code>withFixture(NoArgTest)</code> is <code>Outcome</code>,
 * the test has by definition already finished execution.
 * </p>
 *
 * <p>
 * This trait overrides and makes abstract the <code>runTest</code> method. Subtraits must 
 * must implement this method to call <code>withFixture(NoArgAsyncTest)</code> instead of <code>withFixture(NoArgTest)</code>,
 * where <code>withFixture(NoArgAsyncTest)</code> is a new method declared in this trait with the following
 * signature and implementation:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: NoArgAsyncTest): Future[Outcome] = {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * Instead of returning <code>Outcome</code> like <code>withFixture</code>, the <code>withFixture</code> method
 * returns a <code>Future[Outcome]</code>. Similarly, the <code>apply</code> method of test function interface,
 * <code>NoArgAsyncTest</code>, returns <code>Future[Outcome]</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * // In trait NoArgAsyncTest:
 * def apply(): Future[Outcome]
 * </pre>
 *
 * <p>
 * The <code>withFixture</code> method supports async testing, because when the test function returns,
 * the test body has not necessarily finished execution.
 * </p>
 *
 * <p>
 * The recommended way to ensure cleanup is performed after a test body finishes execution is
 * to use the <code>withCleanup</code> helper method, also defined in this trait, which will ensure that
 * cleanup will occur whether future-producing code completes abruptly by throwing an exception, or returns
 * normally yielding a future. In the latter case, <code>withCleanup</code> will register the cleanup code
 * to execute asynchronously when the future completes.
 * </p>
 *
 * <p>
 * The <code>withFixture</code> method is designed to be stacked, and to enable this, you should always call the <code>super</code> implementation
 * of <code>withFixture</code>, and let it invoke the test function rather than invoking the test function directly. In other words, instead of writing
 * &ldquo;<code>test()</code>&rdquo;, you should write &ldquo;<code>super.withFixture(test)</code>&rdquo;. Thus, the recommended
 * structure of a <code>withFixture</code> implementation that performs cleanup looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgAsyncTest) = {
 *
 *   // Perform setup here
 *
 *   withCleanup {
 *     super.withFixture(test) // Invoke the test function
 *   } {
 *     // Perform cleanup here
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you have no cleanup to perform, you can write <code>withFixture</code> like this instead:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgAsyncTest) = {
 *
 *   // Perform setup here
 *
 *   super.withFixture(test) // Invoke the test function
 * }
 * </pre>
 *
 * <p>
 * If you want to perform an action only for certain outcomes, you'll need to 
 * register code performing that action as a callback on the <code>Future</code> using
 * one of <code>Future</code> registration methods: <code>onComplete</code>, <code>onSuccess</code>,
 * or <code>onFailure</code>. Note that if a test fails, that will be treated as a
 * <code>scala.util.Success(org.scalatest.Failure)</code>. So if you want to perform an 
 * action if a test fails, for example, you'd register the callaback using <code>onSuccess</code>,
 * like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgAsyncTest) = {
 *
 *   // Perform setup here
 *
 *   val futureOutcome = super.withFixture(test) // Invoke the test function
 *
 *   futureOutcome onSuccess {
 *     case _: Failed =&gt;
 *       // perform action that you want to occur
 *       // only if a test fails here
 *   }
 *
 *   futureOutcome
 * }
 * </pre>
 *
 * <p>
 * Lastly, if you want to transform the outcome in some way in <code>withFixture</code>, you'll need to use either the
 * <code>map</code> or <code>transform</code> methods of <code>Future</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgAsyncTest) = {
 *
 *   // Perform setup here
 *
 *   val futureOutcome = super.withFixture(test) // Invoke the test function
 *
 *   futureOutcome map { outcome =&gt;
 *     // transform the outcome into a new outcome here
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Note that a <code>NoArgAsyncTest</code>'s <code>apply</code> method will only return a <code>Failure</code> if
 * the test completes abruptly with an exception (such as <code>OutOfMemoryError</code>) that should
 * cause the suite to abort rather than the test to fail. Thus usually you would use <code>map</code>
 * to transform future outcomes, not <code>transform</code>, so that such suite-aborting exceptions pass through
 * unchanged.  The suite will abort asynchronously with any exception returned in a <code>Failure</code>.
 * </p>
 */
trait AsyncTestSuite extends Suite with RecoverMethods { thisAsyncTestSuite =>

  /**
   * An implicit execution context used by async styles to transform <code>Future[Assertion]</code> values
   * returned by tests into <code>Future[Outcome]</code> values, and can be used within the async tests themselves,
   * for example, when mapping assertions onto futures.
   */
  private final val serialExecutionContext: ExecutionContext = new concurrent.SerialExecutionContext
  implicit def executionContext: ExecutionContext = serialExecutionContext

  private def anAsyncExceptionThatShouldCauseAnAbort(ex: Throwable): Boolean =
    ex match {
      // Not sure why a thrown OutOfMemoryError in our test is showing up nested inside
      // an ExecutionException, but since it is, look inside.
      case ee: java.util.concurrent.ExecutionException if ex.getCause != null => Suite.anExceptionThatShouldCauseAnAbort(ex.getCause)
      case other => Suite.anExceptionThatShouldCauseAnAbort(other)
    }

  /**
   * Transform the test outcome, `Registration` type to `AsyncOutcome`.
   *
   * @param testFun test function
   * @return function that returns `AsyncOutcome`
   */
  private[scalatest] def transformToOutcome(testFun: => Future[Assertion]): () => AsyncOutcome =
    () => {
      val futureSucceeded: Future[Succeeded.type] = testFun.map(_ => Succeeded)
      InternalFutureOutcome(
        futureSucceeded.recover {
          case ex: exceptions.TestCanceledException => Canceled(ex)
          case _: exceptions.TestPendingException => Pending
          case tfe: exceptions.TestFailedException => Failed(tfe)
          case ex: Throwable if !anAsyncExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
        }
      )/* fills in executionContext here */
    }

  /**
   * Implicitly converts an <code>Assertion</code> to a <code>Future[Assertion]</code>.
   *
   * <p>
   * This implicit conversion is used to allow synchronous tests to be included along with
   * asynchronous tests in an <code>AsyncTestSuite</code>. It will be 
   * </p>
   *
   * @param assertion the <code>Assertion</code> to convert
   * @return a <code>Future[Assertion]</code> that has already completed successfully
   *     (containing the <code>Succeeded</code> singleton).
   */
  implicit def convertAssertionToFutureAssertion(assertion: Assertion): Future[Assertion] = Future.successful(assertion)

  protected[scalatest] def parallelAsyncTestExecution: Boolean = thisAsyncTestSuite.isInstanceOf[org.scalatest.ParallelTestExecution] ||
      thisAsyncTestSuite.isInstanceOf[org.scalatest.RandomTestOrder]

  // TODO: Document how exceptions are treated. I.e., that TestConceledException becomes Success(Canceled), 
  // TestPendingException becomes Success(Pending), non-test-fatal exceptions become Success(Failed), and
  // test-fatal exceptions become Failure(ex)
  /**
   * A test function taking no arguments and returning a <code>Future[Outcome]</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the
   * <a href="AsyncFlatSpec.html#withFixtureNoArgTest">documentation for trait <code>AsyncFlatSpec</code></a>.
   * </p>
   */
  trait NoArgAsyncTest extends (() => FutureOutcome) with TestData {
    /**
     * Runs the body of the test, returning a <code>Future[Outcome]</code>.
     */
    def apply(): FutureOutcome
  }

  /**
   * Run the passed test function in the context of a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, register a callback
   * on the resulting <code>Future[Outcome]</code> to perform any clean
   * up needed after the test completes. Because the <code>NoArgTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as reassigning instance <code>var</code>s in this <code>Suite</code> or initializing
   * a globally accessible external database. If you want to avoid reassigning instance <code>var</code>s
   * you can use <a href="fixture/AsyncTestSuite.html">fixture.AsyncTestSuite</a>.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTest</code> invokes this method for each test, passing
   * in a <code>NoArgAsyncTest</code> whose <code>apply</code> method will execute the code of the test
   * and returns its result.
   * </p>
   *
   * <p>
   * This trait's implementation of this method simply invokes the passed <code>NoArgAsyncTest</code> function.
   * </p>
   *
   * @param test the no-arg async test function to run with a fixture
   */
  def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    test()
  }

  /**
   * Ensures a cleanup function is executed whether a future-producing function that produces a
   * valid future or completes abruptly with an exception. 
   *
   * <p>
   * If the by-name passed as the first parameter, <code>future</code>, completes abruptly with an exception
   * this method will catch that exception, invoke the cleanup function passed as the second parameter,
   * <code>cleanup</code>, then rethrow the exception. Otherwise, this method will register the cleanup
   * function to be invoked after the resulting future completes.
   * </p>
   *
   * @param future a by-name that will produce a future
   * @param cleanup a by-name that must be invoked when the future completes, or immediately if
   *            an exception is thrown by the future-producing function
   * @return the future produced by the first by-name parameter, with an invocation of the second
   *            by-name parameter registered to execute when the future completes.
   */
  def withCleanup[T](trial: => T)(cleanup: => Unit)(implicit futuristic: Futuristic[T]): T = {
    val result: T =
      try trial // evaluate the by-name once
      catch {
        case ex: Throwable =>
          cleanup  // execute the clean up
          throw ex // rethrow the same exception
      }
    futuristic.withCleanup(result) { cleanup }
/*
    // First deal with Failure, and mimic finally semantics
    // but in future-space. The recoverWith will only execute
    // if this is a Failure, and will only return a Failure,
    // so the subsequent map will only happen if this recoverWith
    // does not happen.
    result recoverWith {
      case firstEx: Throwable => 
        try {
          cleanup
          Future.failed(firstEx)
        }
        catch {
          case secondEx: Throwable =>
            Future.failed(secondEx)
        }
    } map { v => // Ensure cleanup happens for the Success case
      cleanup
      v
    }
*/
  }

  /**
   * Run an async test.
   *
   * <p>
   * This method is declared abstract in this trait. Subclasses must implement this method to call <code>withFixture</code>
   * instead of <code>withFixture</code>.
   * </p>
   *
   * @param testName the name of one async test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed.
   *
   * @throws NullArgumentException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status
}
