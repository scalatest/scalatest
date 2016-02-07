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
package org.scalatest.fixture

import org.scalatest._
import org.scalatest.exceptions.StackDepthExceptionHelper._

import scala.concurrent.Future

// TODO: Scaladoc
/**
 * The base trait of ScalaTest's "fixture" async testing styles, which enable you to pass fixture objects into tests.
 *
 * <p>
 * This trait provides a final override of <code>withFixture(OneArgTest)</code>, declared in
 * supertrait <code>fixture.Suite</code>, because the <code>withFixture(OneArgTest)</code> lifecycle
 * method assumes synchronous testing. Here is its signature:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: OneArgTest): Outcome
 * </pre>
 *
 * <p>
 * The test function interface, <a href="Suite$OneArgTest.html"><code>OneArgTest</code></a>, offers an <code>apply</code> method
 * that takes a <code>FixtureParam</code> and returns <a href="Outcome.html"><code>Outcome</code></a>:
 * </p>
 *
 * <pre class="stHighlight">
 * // In trait OneArgTest:
 * def apply(fixture: FixtureParam): Outcome
 * </pre>
 *
 * <p>
 * Because the result of a test is an <code>Outcome</code>, when the test function returns, the test body must have determined an outcome already. It
 * will already be one of <a href="../Succeeded$.html"><code>Succeeded</code></a>, <a href="../Failed.html"><code>Failed</code></a>, <a href="../Canceled.html"><code>Canceled</code></a>, or <a href="../Pending$.html"></code>Pending</code></a>. This is
 * also true when <code>withFixture(OneArgTest)</code> returns: because the result type of <code>withFixture(OneArgTest)</code> is <code>Outcome</code>,
 * the test body has by definition has already finished execution.
 * </p>
 *
 * <p>
 * This trait overrides and makes abstract the <code>runTest</code> method. Subtraits must 
 * must implement this method to call <code>withAsyncFixture(OneArgAsyncTest)</code> instead of <code>withFixture(OneArgTest)</code>,
 * where <code>withAsyncFixture(OneArgAsyncTest)</code> is a new method declared in this trait with the following
 * signature and implementation:
 * </p>
 *
 * <pre class="stHighlight">
 * def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] = {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * Instead of returning <code>Outcome</code> like <code>withFixture</code>, the <code>withAsyncFixture</code> method
 * returns a <code>Future[Outcome]</code>. Similarly, the <code>apply</code> method of test function interface,
 * <code>OneArgAsyncTest</code>, returns <code>Future[Outcome]</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * // In trait OneArgAsyncTest:
 * def apply(fixture: FixtureParam): Future[Outcome]
 * </pre>
 *
 * <p>
 * The <code>withAsyncFixture</code> method supports async testing, because when the test function returns,
 * the test body has not necessarily finished execution.
 * </p>
 *
 * <p>
 * The recommended way to ensure cleanup is performed after a test body finishes execution is
 * to use the <code>withCleanup</code> helper method, defined in supertrait
 * <a href="../AsyncTestSuite.html"><code>org.scalatest.AsyncTestSuite</code></a>, which will ensure that
 * cleanup will occur whether future-producing code completes abruptly by throwing an exception, or returns
 * normally yielding a future. In the latter case, <code>withCleanup</code> will register the cleanup code
 * to execute asynchronously when the future completes.
 * </p>
 *
 * <p>
 * To enable the stacking of traits that define <code>withAsyncFixture(NoArgAsyncTest)</code>, it is a good idea to let
 * <code>withAsyncFixture(NoArgAsyncTest)</code> invoke the test function instead of invoking the test
 * function directly. To do so, you'll need to convert the <code>OneArgAsyncTest</code> to a <code>NoArgAsyncTest</code>. You can do that by passing
 * the fixture object to the <code>toNoArgAsyncTest</code> method of <code>OneArgAsyncTest</code>. In other words, instead of
 * writing &ldquo;<code>test(theFixture)</code>&rdquo;, you'd delegate responsibility for
 * invoking the test function to the <code>withAsyncFixture(NoArgAsyncTest)</code> method of the same instance by writing:
 * </p>
 *
 * <pre class="stHighlight">
 * withAsyncFixture(test.toNoArgAsyncTest(theFixture))
 * </pre>
 *
 * <p>
 * Thus, the recommended structure of a <code>withAsyncFixture</code> implementation that performs cleanup looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withAsyncFixture(test: OneArgAsyncTest) = {
 *
 *   // Perform setup here
 *   val theFixture = ...
 *
 *   withCleanup {
 *     withAsyncFixture(test.toNoArgAsyncTest(theFixture)) // Invoke the test function
 *   } {
 *     // Perform cleanup here
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you have no cleanup to perform, you can write <code>withAsyncFixture</code> like this instead:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withAsyncFixture(test: OneArgAsyncTest) = {
 *
 *   // Perform setup here
 *   val theFixture = ...
 *
 *   withAsyncFixture(test.toNoArgAsyncTest(theFixture)) // Invoke the test function
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
 * override def withAsyncFixture(test: OneArgAsyncTest) = {
 *
 *   // Perform setup here
 *   val theFixture = ...
 *
 *   val futureOutcome =
 *       withAsyncFixture(test.toNoArgAsyncTest(theFixture)) // Invoke the test function
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
 * Lastly, if you want to transform the outcome in some way in <code>withAsyncFixture</code>, you'll need to use either the
 * <code>map</code> or <code>transform</code> methods of <code>Future</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * // Your implementation
 * override def withAsyncFixture(test: OneArgAsyncTest) = {
 *
 *   // Perform setup here
 *   val theFixture = ...
 *
 *   val futureOutcome =
 *       withAsyncFixture(test.toNoArgAsyncTest(theFixture)) // Invoke the test function
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
 * unchanged. The suite will abort asynchronously with any exception returned in a <code>Failure</code>.
 * </p>
 */
trait AsyncTestSuite extends org.scalatest.fixture.Suite with org.scalatest.AsyncTestSuite {

  /**
   * Transform the test outcome, `Registration` type to `AsyncOutcome`.
   *
   * @param testFun test function
   * @return function that returns `AsyncOutcome`
   */
  private[scalatest] def transformToOutcome(testFun: FixtureParam => Future[Assertion]): FixtureParam => AsyncOutcome =
    (fixture: FixtureParam) => {
      val futureUnit = testFun(fixture)
      InternalFutureOutcome(
        futureUnit.map(u => Succeeded).recover {
          case ex: exceptions.TestCanceledException => Canceled(ex)
          case _: exceptions.TestPendingException => Pending
          case tfe: exceptions.TestFailedException => Failed(tfe)
          case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
        }
      )
    }

  /**
   * A test function taking no arguments and returning an <code>Future[Outcome]</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the
   * <a href="AsyncFlatSpec.html#withAsyncFixtureNoArgAsyncTest">documentation for trait <code>fixture.AsyncFlatSpec</code></a>.
   * </p>
   */
  trait OneArgAsyncTest extends (FixtureParam => FutureOutcome) with TestData { thisOneArgAsyncTest =>

    /**
     * Using the passed <code>FixtureParam</code>, produces a <code>Future[Outcome]</code> representing
     * the future outcome of this asynchronous test.
     *
     * @param fixture the <code>FixtureParam</code>
     * @return an instance of <code>Future[Outcome]</code>
     */
    def apply(fixture: FixtureParam): FutureOutcome

    /**
     * Convert this <code>OneArgAsyncTest</code> to a <code>NoArgAsyncTest</code> whose
     * <code>name</code> and <code>configMap</code> methods return the same values
     * as this <code>OneArgAsyncTest</code>, and whose <code>apply</code> method invokes
     * this <code>OneArgAsyncTest</code>'s apply method,
     * passing in the given <code>fixture</code>.
     *
     * <p>
     * This method makes it easier to invoke the <code>withAsyncFixture</code> method
     * that takes a <code>NoArgAsyncTest</code>.
     * Here's how that might look in a <code>fixture.AsyncTestSuite</code>
     * whose <code>FixtureParam</code> is <code>StringBuilder</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * def withAsyncFixture(test: OneArgAsyncTest) = {
     *   withAsyncFixture(test.toNoArgAsyncTest(new StringBuilder))
     * }
     * </pre>
     *
     * <p>
     * Invoking this method has no side effect. It just returns a <code>NoArgAsyncTest</code> whose
     * <code>apply</code> method invokes <code>apply</code> on this <code>OneArgAsyncTest</code>, passing
     * in the <code>FixtureParam</code> passed to <code>toNoArgAsyncTest</code>.
     * </p>
     *
     * @param fixture the <code>FixtureParam</code>
     * @return an new instance of <code>NoArgAsyncTest</code>
     */
    def toNoArgAsyncTest(fixture: FixtureParam): NoArgAsyncTest = 
      new NoArgAsyncTest {
        val name = thisOneArgAsyncTest.name
        val configMap = thisOneArgAsyncTest.configMap
        def apply(): FutureOutcome = { thisOneArgAsyncTest(fixture) }
        val scopes = thisOneArgAsyncTest.scopes
        val text = thisOneArgAsyncTest.text
        val tags = thisOneArgAsyncTest.tags
      }
  }

  /**
   *  Run the passed test function with a fixture created by this method.
   *
   * <p>
   * This method should create the fixture object needed by the tests of the
   * current suite, invoke the test function (passing in the fixture object),
   * and if needed, register any clean up needed after the test completes as
   * a callback on the <code>Future[Outcome]</code> returned by the test function.
   * For more detail and examples, see the
   * <a href="AsyncTestSuite.html">main documentation for this trait</a>.
   * </p>
   *
   * @param test the <code>OneArgAsyncTest</code> to invoke, passing in a fixture
   * @return an instance of <code>Future[Outcome]</code>
   */
  def withFixture(test: OneArgAsyncTest): FutureOutcome
}
