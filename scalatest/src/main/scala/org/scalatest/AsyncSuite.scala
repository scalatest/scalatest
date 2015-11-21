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

/*
 * TODO: Fill in here and also add a lifecycle-methods to Suite, which is linked to
 * from SuiteMixin.
 */
/**
 * The base trait of ScalaTest's async testing styles, which enables testing of
 * asynchronous code without blocking.
 *
 * <p>
 * This trait provides a final override of <code>withFixture</code>, declared in
 * supertrait <code>Suite</code>, because the <code>withFixture</code> lifecycle
 * method assumes synchronous testing:
 * </p>
 *
 * <pre>
 * def withFixture(test: NoArgTest): Outcome
 * </pre>
 *
 * <p>
 * The test function interface, <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a>, offers an <code>apply</code> method
 * that also returns <a href="Outcome.html"><code>Outcome</code></a>:
 * </p>
 *
 * <pre>
 * // In trait NoArgTest:
 * def apply(): Outcome
 * </pre>
 *
 * <p>
 * Because the result of a test is an <code>Outcome</code>, when the test function returns, the test must have completed already. It
 * will already be one of <a href="Succeeded$.html"><code>Succeeded</code></a>, <a href="Failed.html"><code>Failed</code></a>, <a href="Canceled.html"><code>Canceled</code></a>, or <a href="Pending$.html"></code>Pending</code></a>. This is
 * also true when <code>withFixture</code> returns: because the result type of <code>withFixture</code> is <code>Outcome</code>,
 * the test by definition has already completed.
 * </p>
 *
 * <p>
 * Huh, was just about to write that this trait overrides runTest, but it doesn't. That's done in all the Async*Like
 * traits. But I think it should be here. Going to check this in and try that.
 * </p>
 */
trait AsyncSuite extends Suite { thisAsyncSuite =>

  /**
   * An implicit execution context used by async styles to transform <code>Future[Assertion]</code> values
   * returned by tests into <code>Future[Outcome]</code> values, and can be used within the async tests themselves,
   * for example, when mapping assertions onto futures.
   */
  implicit def executionContext: ExecutionContext

  /**
   * Implicitly converts an <code>Assertion</code> to a <code>Future[Assertion]</code>.
   *
   * <p>
   * This implicit conversion is used to allow synchronous tests to be included along with
   * asynchronous tests in an <code>AsyncSuite</code>. It will be 
   * </p>
   *
   * @param assertion the <code>Assertion</code> to convert
   * @return a <code>Future[Assertion]</code> that has already completed successfully
   *     (containing the <code>Succeeded</code> singleton).
   */
  implicit def convertAssertionToFutureAssertion(assertion: Assertion): Future[Assertion] = Future.successful(assertion)

  protected[scalatest] def parallelAsyncTestExecution: Boolean = thisAsyncSuite.isInstanceOf[org.scalatest.ParallelTestExecution] ||
      thisAsyncSuite.isInstanceOf[org.scalatest.RandomTestOrder]

  /**
   * Throws <code>NotAllowedException</code>, because its signature assumes synchronous testing. Use <code>withAsyncFixture</code> instead.
   */
  final override def withFixture(test: NoArgTest): Outcome = {
    throw new exceptions.NotAllowedException(FailureMessages.withFixtureNotAllowedInAsyncFixtures, getStackDepthFun("AsyncSuite.scala", "withFixture"))
  }

  /**
   * A test function taking no arguments and returning a <code>Future[Outcome]</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the
   * <a href="AsyncFlatSpec.html#withAsyncFixtureNoArgTest">documentation for trait <code>AsyncFlatSpec</code></a>.
   * </p>
   */
  trait NoArgAsyncTest extends (() => Future[Outcome]) with TestData {
    /**
     * Runs the body of the test, returning a <code>Future[Outcome]</code>.
     */
    def apply(): Future[Outcome]
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
   * you can use <a href="fixture/AsyncSuite.html">fixture.AsyncSuite</a>.
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
  def withAsyncFixture(test: NoArgAsyncTest): Future[Outcome] = {
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
  def withCleanup[T](future: => Future[T])(cleanup: => Unit): Future[T] = {
    val result: Future[T] =
      try future // evaluate the by-name once
      catch {
        case ex: Throwable =>
          cleanup  // execute the clean up
          throw ex // rethrow the same exception
      }
    // Made it this far, so register the cleanup as a 
    // callback on the Future
    result onComplete { _ => cleanup }
    result
  }
}
