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

import org.scalatest._
import OutcomeOf.outcomeOf
import org.scalactic._

/**
 * The base trait for ScalaTest's synchronous testing styles that accept a fixture
 * object passed into tests. This trait defines a
 * <code>withFixture</code> lifecycle method that takes as its parameter a test function
 * that accepts a fixture object and returns an <a href="Outcome.html"><code>Outcome</code></a>.
 *
 * <p>
 * The abstract <code>withFixture</code> method add by this trait has the
 * following signature:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: OneArgTest): Outcome
 * </pre>
 *
 * The <code>apply</code> method of test function interface,
 * <code>OneArgTest</code>, also returns <code>Outcome</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * // In trait OneArgTest:
 * def apply(fixture: FixtureParam): Outcome
 * </pre>
 *
 * <p>
 * Because the result of a test is an <code>Outcome</code>, when the test function returns, the test body must have determined an outcome already. It
 * will already be one of <a href="Succeeded$.html"><code>Succeeded</code></a>, <a href="Failed.html"><code>Failed</code></a>, <a href="Canceled.html"><code>Canceled</code></a>, or <a href="Pending$.html"></code>Pending</code></a>. This is
 * also true when <code>withFixture(OneArgTest)</code> returns: because the result type of <code>withFixture(OneArgTest)</code> is <code>Outcome</code>,
 * the test has by definition already finished execution.
 * </p>
 *
 * <p>
 * The recommended way to ensure cleanup is performed after a test body finishes execution is
 * to use a <code>try</code>-<code>finally</code> clause.
 * Using <code>try</code>-<code>finally</code> will ensure that cleanup will occur whether
 * the test function completes abruptly by throwing a suite-aborting exception, or returns
 * normally yielding an <code>Outcome</code>. Note that the only situation in which a test function
 * will complete abruptly with an exception is if the test body throws a suite-aborting exception.
 * Any other exception will be caught and reported as either a <code>Failed</code>, <code>Canceled</code>,
 * or <code>Pending</code>.
 * </p>
 *
 * <p>
 * To enable the stacking of traits that define <code>withFixture(NoArgTest)</code>, it is a good idea to let
 * <code>withFixture(NoArgTest)</code> invoke the test function instead of invoking the test
 * function directly. To do so, you'll need to convert the <code>OneArgTest</code> to a <code>NoArgTest</code>. You can do that by passing
 * the fixture object to the <code>toNoArgTest</code> method of <code>OneArgTest</code>. In other words, instead of
 * writing &ldquo;<code>test(theFixture)</code>&rdquo;, you'd delegate responsibility for
 * invoking the test function to the <code>withFixture(NoArgTest)</code> method of the same instance by writing:
 * </p>
 *
 * <pre>
 * withFixture(test.toNoArgTest(theFixture))
 * </pre>
 *
 * <p>
 * The <code>withFixture</code> method is designed to be stacked, and to enable this, you should always call the <code>super</code> implementation
 * of <code>withFixture</code>, and let it invoke the test function rather than invoking the test function directly. In other words, instead of writing
 * &ldquo;<code>test(...)</code>&rdquo;, you should write &ldquo;<code>super.withFixture(test)</code>&rdquo;. Thus, the recommended
 * structure of a <code>withFixture</code> implementation that performs cleanup looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * type FixtureParam = String
 *
 * override def withFixture(test: OneArgTest) = {
 *   // Perform setup here
 *   val theFixture = "hello"
 *   try {
 *     withFixture(test.toNoArgTest(theFixture)) // Invoke the test function
 *   } finally {
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
 * type FixtureParam = String
 *
 * override def withFixture(test: NoArgTest) = {
 *   // Perform setup here
 *   val theFixture = "hello"
 *   withFixture(test.toNoArgTest(theFixture)) // Invoke the test function
 * }
 * </pre>
 *
 * <p>
 * If you want to perform an action only for certain outcomes, you can use
 * a pattern match.
 * For example, if you want to perform an action if a test fails, you'd
 * match on <code>Failed</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * type FixtureParam = String
 *
 * override def withFixture(test: NoArgTest) = {
 *
 *   // Perform setup here
 *   val theFixture = "hello"
 *
 *   val outcome = withFixture(test.toNoArgTest(theFixture)) // Invoke the test function
 *
 *   outcome match {
 *     case failed: Failed =&gt;
 *       // perform action that you want to occur
 *       // only if a test fails here
 *       failed
 *     case other =&gt; other
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you want to change the outcome in some way in <code>withFixture</code>, you can also
 * use a pattern match.
 * For example, if a particular exception intermittently causes a test to fail, and can
 * transform those failures into cancelations, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * type FixtureParam = String
 *
 * override def withFixture(test: NoArgTest) = {
 *
 *   val theFixture = "hello"
 *
 *   withFixture(test.toNoArgTest(theFixture)) match {
 *     case Failed(ex: ParticularException) =&gt;
 *       Canceled("Muting flicker", ex)
 *     case other =&gt; other
 *   }
 * }
 * </pre>
 */
trait FixtureTestSuite extends org.scalatest.FixtureSuite with org.scalatest.TestSuite { thisTestSuite =>

  /**
   * A test function taking a fixture parameter and returning an <code>Outcome</code>.
   *
   * <p>
   * For more detail and examples, see the
   * <a href="FlatSpec.html">documentation for trait <code>fixture.FlatSpec</code></a>.
   * </p>
   */
  protected trait OneArgTest extends (FixtureParam => Outcome) with TestData { thisOneArgTest =>

    /**
     * Runs the test, using the passed <code>FixtureParam</code>.
     *
     * @param fixture the <code>FixtureParam</code>
     * @return an instance of <code>Outcome</code>
     */
    def apply(fixture: FixtureParam): Outcome

    /**
     * Convert this <code>OneArgTest</code> to a <code>NoArgTest</code> whose
     * <code>name</code> and <code>configMap</code> methods return the same values
     * as this <code>OneArgTest</code>, and whose <code>apply</code> method invokes
     * this <code>OneArgTest</code>'s apply method,
     * passing in the given <code>fixture</code>.
     *
     * <p>
     * This method makes it easier to invoke the <code>withFixture</code> method
     * that takes a <code>NoArgTest</code>. For example, if a <code>FixtureSuite</code>
     * mixes in <code>SeveredStackTraces</code>, it will inherit an implementation
     * of <code>withFixture(NoArgTest)</code> provided by
     * <code>SeveredStackTraces</code> that implements the stack trace severing
     * behavior. If the <code>FixtureSuite</code> does not delegate to that
     * <code>withFixture(NoArgTest)</code> method, the stack trace severing behavior
     * will not happen. Here's how that might look in a <code>FixtureSuite</code>
     * whose <code>FixtureParam</code> is <code>StringBuilder</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * def withFixture(test: OneArgTest) = {
     *   withFixture(test.toNoArgTest(new StringBuilder))
     * }
     * </pre>
     *
     * <p>
     * Invoking this method has no side effect. It just returns a <code>NoArgTest</code> whose
     * <code>apply</code> method invokes <code>apply</code> on this <code>OneArgTest</code>, passing
     * in the <code>FixtureParam</code> passed to <code>toNoArgTest</code>.
     * </p>
     *
     * @param fixture the <code>FixtureParam</code>
     * @return an new instance of <code>NoArgTest</code>
     */
    def toNoArgTest(fixture: FixtureParam) =
      new NoArgTest {
        val name = thisOneArgTest.name
        val configMap = thisOneArgTest.configMap
        def apply(): Outcome = { thisOneArgTest(fixture) }
        val scopes = thisOneArgTest.scopes
        val text = thisOneArgTest.text
        val tags = thisOneArgTest.tags
        val pos = thisOneArgTest.pos
      }
  }

  /**
   * Companion object for <code>OneArgTest</code> that provides factory method to create new <code>OneArgTest</code>
   * instance by passing in a <code>OneArgTest</code> and a <code>FixtureParam</code> => <code>Outcome</code> function.
   */
  object OneArgTest {
    /**
     * Create new <code>OneArgTest</code> instance.
     *
     * @param test a <code>OneArgTest</code>
     * @param f a <code>FixtureParam</code> => <code>Outcome</code> function
     * @return a new instance of <code>OneArgTest</code>, which will call the passed <code>f</code> function in its <code>apply</code> method
     */
    def apply(test: OneArgTest)(f: FixtureParam => Outcome): OneArgTest = {
      new OneArgTest {
        def apply(fixture: FixtureParam): Outcome = { f(fixture) }
        val text: String = test.text
        val configMap: ConfigMap = test.configMap
        val scopes: collection.immutable.IndexedSeq[String] = test.scopes
        val name: String = test.name
        val tags: Set[String] = test.tags
        val pos: Option[source.Position] = test.pos
      }
    }
  }

  /**
   *  Run the passed test function with a fixture created by this method.
   *
   * <p>
   * This method should create the fixture object needed by the tests of the
   * current suite, invoke the test function (passing in the fixture object),
   * and if needed, perform any clean up needed after the test completes.
   * For more detail and examples, see the <a href="Suite.html">main documentation for this trait</a>.
   * </p>
   *
   * @param test the <code>OneArgTest</code> to invoke, passing in a fixture
   * @return an instance of <code>Outcome</code>
   */
  protected def withFixture(test: OneArgTest): Outcome

  private[scalatest] class TestFunAndConfigMap(val name: String, test: FixtureParam => Any, val configMap: ConfigMap)
    extends OneArgTest {

    def apply(fixture: FixtureParam): Outcome = {
      outcomeOf { test(fixture) }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
    val pos = testData.pos
  }

  private[scalatest] class FixturelessTestFunAndConfigMap(override val name: String, test: () => Any, override val configMap: ConfigMap)
    extends NoArgTest {

    def apply(): Outcome = {
      outcomeOf { test() }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
    val pos = testData.pos
  }
}

