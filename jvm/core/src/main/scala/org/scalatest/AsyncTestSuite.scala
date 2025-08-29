/*
 * Copyright 2001-2025 Artima, Inc.
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
import enablers.Futuristic

/*
 * TODO: Fill in here and also add a lifecycle-methods to Suite, which is linked to
 * from SuiteMixin.
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
 */

/**
 * The base trait of ScalaTest's <em>asynchronous testing styles</em>, which defines a 
 * <code>withFixture</code> lifecycle method that accepts as its parameter a test function
 * that returns a <a href="FutureOutcome.html"><code>FutureOutcome</code></a>.
 *
 * <p>
 * The <code>withFixture</code> method add by this trait has the 
 * following signature and implementation:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: NoArgAsyncTest): FutureOutcome = {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * This trait enables testing of asynchronous code without blocking.  Instead of returning
 * <code>Outcome</code> like <a href="TestSuite.html"><code>TestSuite</code></a>'s 
 * <code>withFixture</code>, this trait's <code>withFixture</code> method returns a
 * <code>FutureOutcome</code>. Similarly, the <code>apply</code> method of test function interface,
 * <code>NoArgAsyncTest</code>, returns <code>FutureOutcome</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * // In trait NoArgAsyncTest:
 * def apply(): FutureOutcome
 * </pre>
 *
 * <p>
 * The <code>withFixture</code> method supports async testing, because when the test function returns,
 * the test body has not necessarily finished execution.
 * </p>
 *
 * <p>
 * The recommended way to ensure cleanup is performed after a test body finishes execution is
 * to use a <code>complete</code>-<code>lastly</code> clause, syntax that is defined in trait
 * <a href="CompleteLastly.html"><code>CompleteLastly</code></a>, which this trait extends.
 * Using <code>cleanup</code>-<code>lastly</code> will ensure that cleanup will occur whether
 * <code>FutureOutcome</code>-producing code completes abruptly by throwing an exception, or returns
 * normally yielding a <code>FutureOutcome</code>. In the latter case,
 * <code>complete</code>-<code>lastly</code> will
 * register the cleanup code to execute asynchronously when the <code>FutureOutcome</code> completes.
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
 *   // Perform setup here
 *   complete {
 *     super.withFixture(test) // Invoke the test function
 *   } lastly {
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
 *   // Perform setup here
 *   super.withFixture(test) // Invoke the test function
 * }
 * </pre>
 *
 * <p>
 * The test function and <code>withFixture</code> method returns a
 * <a href="FutureOutcome.html"><code>FutureOutcome</code></a>,
 * a ScalaTest class that wraps a Scala <code>Future[Outcome]</code> and offers methods
 * more specific to asynchronous test outcomes. In a Scala <code>Future</code>, any exception
 * results in a <code>scala.util.Failure</code>. In a <code>FutureOutcome</code>, a
 * thrown <code>TestPendingException</code> always results in a <code>Pending</code>,
 * a thrown <code>TestCanceledException</code> always results in a <code>Canceled</code>,
 * and any other exception, so long as it isn't suite-aborting, results in a
 * <code>Failed</code>. This is true of the asynchronous test code itself that's represented by
 * the <code>FutureOutcome</code> and any transformation or callback registered on the
 * <code>FutureOutcome</code> in <code>withFixture</code>.
 * </p>
 * 
 * <p>
 * If you want to perform an action only for certain outcomes, you'll need to 
 * register code performing that action on the <code>FutureOutcome</code> using
 * one of <code>FutureOutcome</code>'s callback registration methods:
 * </p>
 *
 * <ul>
 * <li><code>onSucceededThen</code> - executed if the <code>Outcome</code> is a <code>Succeeded</code>.
 * <li><code>onFailedThen</code> - executed if the <code>Outcome</code> is a <code>Failed</code>.
 * <li><code>onCanceledThen</code> - executed if the <code>Outcome</code> is a <code>Canceled</code>.
 * <li><code>onPendingThen</code> - executed if the <code>Outcome</code> is a <code>Pending</code>.
 * <li><code>onOutcomeThen</code> - executed on any <code>Outcome</code> (<em>i.e.</em>, no
 *        suite-aborting exception is thrown).
 * <li><code>onAbortedThen</code> - executed if a suite-aborting exception is thrown.
 * <li><code>onCompletedThen</code> - executed whether the result is an <code>Outcome</code>
 *        or a thrown suite-aborting exception.
 * </ul>
 *
 * <p>
 * For example, if you want to perform an action if a test fails, you'd register the
 * callback using <code>onFailedThen</code>, like this:
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
 *   futureOutcome onFailedThen { ex =&gt;
 *     // perform action that you want to occur
 *     // only if a test fails here
 *   }
 * }
 * </pre>
 *
 * <p>
 * Note that all callback registration methods, such as <code>onFailedThen</code> used in the
 * previous example, return a new <code>FutureOutcome</code> that won't complete until the
 * the original <code>FutureOutcome</code> <em>and the callback</em> has completed. If the callback
 * throws an exception, the resulting <code>FutureOutcome</code> will represent that exception.
 * For example, if a <code>FutureOutcome</code> results in <code>Failed</code>, but a callback
 * registered on that <code>FutureOutcome</code> with <code>onFailedThen</code> throws <code>TestPendingException</code>, the
 * result of the <code>FutureOutcome</code> returned by <code>onFailedThen</code> will
 * be <code>Pending</code>.
 * </p>
 *
 * <p>
 * Lastly, if you want to change the outcome in some way in <code>withFixture</code>, you'll need to use
 * the <code>change</code> method of <code>FutureOutcome</code>, like this:
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
 *   futureOutcome change { outcome =&gt;
 *     // transform the outcome into a new outcome here
 *   }
 * }
 * </pre>
 */
trait AsyncTestSuite extends Suite with RecoverMethods with CompleteLastly { thisAsyncTestSuite =>

  private final val serialExecutionContext: ExecutionContext = new concurrent.SerialExecutionContext

  /**
   * An implicit execution context used by async styles to transform <code>Future[Assertion]</code> values
   * returned by tests into <code>FutureOutcome</code> values, and can be used within the async tests themselves,
   * for example, when mapping assertions onto futures.
   */
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
  private[scalatest] def transformToOutcome(testFun: => Future[compatible.Assertion]): () => AsyncTestHolder =
    () => {
      val futureSucceeded: Future[Succeeded.type] = testFun.map(_ => Succeeded)
      FutureAsyncTestHolder(
        futureSucceeded.recover {
          case ex: exceptions.TestCanceledException => Canceled(ex)
          case _: exceptions.TestPendingException => Pending
          case tfe: exceptions.TestFailedException => Failed(tfe)
          case ex: Throwable if !anAsyncExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
        }
      )/* fills in executionContext here */
    }

  // SKIP-DOTTY-START 
  import scala.language.implicitConversions // To convert Assertion to Future[Assertion]
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
  implicit def convertAssertionToFutureAssertion(assertion: compatible.Assertion): Future[compatible.Assertion] = Future.successful(assertion)
  // SKIP-DOTTY-END
  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Converts an <code>Assertion</code> to a <code>Future[Assertion]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <p>
  //DOTTY-ONLY   * This conversion is used to allow synchronous tests to be included along with
  //DOTTY-ONLY   * asynchronous tests in an <code>AsyncTestSuite</code>. It will be 
  //DOTTY-ONLY   * </p>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param assertion the <code>Assertion</code> to convert
  //DOTTY-ONLY   * @return a <code>Future[Assertion]</code> that has already completed successfully
  //DOTTY-ONLY   *     (containing the <code>Succeeded</code> singleton).
  //DOTTY-ONLY   */
  //DOTTY-ONLY given Conversion[compatible.Assertion, Future[compatible.Assertion]] = Future.successful(_)

  //DOTTY-ONLY /** 
  //DOTTY-ONLY   * Provides an implicit conversion from a synchronous test function
  //DOTTY-ONLY   * to an asynchronous one returning a `Future`.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * This conversion allows a function of type 
  //DOTTY-ONLY   * `TestData => compatible.Assertion` 
  //DOTTY-ONLY   * (which produces an assertion result directly)
  //DOTTY-ONLY   * to be used in places where a function of type 
  //DOTTY-ONLY   * `TestData => Future[compatible.Assertion]` 
  //DOTTY-ONLY   * is expected.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * The conversion simply wraps the result of the original function
  //DOTTY-ONLY   * into a successfully completed `Future` using `Future.successful`.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * ==Example==
  //DOTTY-ONLY   * {{{
  //DOTTY-ONLY   *   val syncFun: TestData => compatible.Assertion = td => assert(td.name.nonEmpty)
  //DOTTY-ONLY   *
  //DOTTY-ONLY   *   // Thanks to this conversion, `syncFun` can be passed
  //DOTTY-ONLY   *   // to APIs expecting `TestData => Future[compatible.Assertion]`.
  //DOTTY-ONLY   *   val asyncFun: TestData => Future[compatible.Assertion] = syncFun
  //DOTTY-ONLY   * }}}
  //DOTTY-ONLY   */
  //DOTTY-ONLY given Conversion[TestData => compatible.Assertion, TestData => Future[compatible.Assertion]] with {
  //DOTTY-ONLY   def apply(fun: TestData => compatible.Assertion): TestData => Future[compatible.Assertion] =
  //DOTTY-ONLY     (testData: TestData) => Future.successful(fun(testData))
  //DOTTY-ONLY }

  protected[scalatest] def parallelAsyncTestExecution: Boolean = thisAsyncTestSuite.isInstanceOf[org.scalatest.ParallelTestExecution] ||
      thisAsyncTestSuite.isInstanceOf[org.scalatest.RandomTestOrder]

  // TODO: Document how exceptions are treated. I.e., that TestConceledException becomes Success(Canceled), 
  // TestPendingException becomes Success(Pending), non-test-fatal exceptions become Success(Failed), and
  // test-fatal exceptions become Failure(ex)
  /**
   * A test function taking no arguments and returning a <code>FutureOutcome</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the
   * <a href="AsyncFlatSpec.html#withFixtureNoArgTest">documentation for trait <code>AsyncFlatSpec</code></a>.
   * </p>
   */
  trait NoArgAsyncTest extends (() => FutureOutcome) with TestData {
    /**
     * Runs the body of the test, returning a <code>FutureOutcome</code>.
     */
    def apply(): FutureOutcome
  }

  /**
   * Run the passed test function in the context of a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, register a callback
   * on the resulting <code>FutureOutcome</code> to perform any clean
   * up needed after the test completes. Because the <code>NoArgAsyncTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as reassigning instance <code>var</code>s in this <code>Suite</code> or initializing
   * a globally accessible external database. If you want to avoid reassigning instance <code>var</code>s
   * you can use <a href="FixtureAsyncTestSuite.html">FixtureAsyncTestSuite</a>.
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

  /*
   * OLD SCALADOC FOR WITHCLEANUP
   *
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

  /**
   * Run an async test.
   *
   * <p>
   * This method is redefine in this trait solely to narrow its contract. Subclasses must implement
   * this method to call the <code>withFixture(NoArgAsyncTest)</code> method, which is defined in
   * this trait.
   * </p>
   *
   * <p>
   * This trait's implementation of this method simply returns <code>SucceededStatus</code> 
   * and has no other effect.
   * </p>
   *
   * @param testName the name of one async test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed.
   *
   * @throws NullArgumentException if either <code>testName</code> or <code>args</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = SucceededStatus
}
