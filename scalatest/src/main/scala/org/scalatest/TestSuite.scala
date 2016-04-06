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

import org.scalactic.source.SourceInfo

/**
 * The base trait of ScalaTest's <em>synchronous testing styles</em>, which defines a 
 * <code>withFixture</code> lifecycle method that accepts as its parameter a test function
 * that returns an <a href="Outcome.html"><code>Outcome</code></a>.
 *
 * <p>
 * The <code>withFixture</code> method add by this trait has the 
 * following signature and implementation:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: NoArgTest): Outcome = {
 *   test()
 * }
 * </pre>
 *
 * The <code>apply</code> method of test function interface,
 * <code>NoArgTest</code>, also returns <code>Outcome</code>:
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
 * The <code>withFixture</code> method is designed to be stacked, and to enable this, you should always call the <code>super</code> implementation
 * of <code>withFixture</code>, and let it invoke the test function rather than invoking the test function directly. In other words, instead of writing
 * &ldquo;<code>test()</code>&rdquo;, you should write &ldquo;<code>super.withFixture(test)</code>&rdquo;. Thus, the recommended
 * structure of a <code>withFixture</code> implementation that performs cleanup looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgTest) = {
 *   // Perform setup here
 *   try {
 *     super.withFixture(test) // Invoke the test function
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
 * override def withFixture(test: NoArgTest) = {
 *   // Perform setup here
 *   super.withFixture(test) // Invoke the test function
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
 * override def withFixture(test: NoArgTest) = {
 *
 *   // Perform setup here
 *
 *   val outcome = super.withFixture(test) // Invoke the test function
 *
 *   outcome match {
 *     case failed: Failed =>
 *       // perform action that you want to occur
 *       // only if a test fails here
 *       failed
 *     case other => other
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
 * override def withFixture(test: NoArgTest) = {
 *
 *   super.withFixture(test) match {
 *     case Failed(ex: ParticularException) =>
 *       Canceled("Muting flicker", ex)
 *     case other => other
 *   }
 * }
 * </pre>
 */
trait TestSuite extends Suite { thisTestSuite =>

  /**
   * A test function taking no arguments and returning an <code>Outcome</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the 
   * <a href="FlatSpec.html#withFixtureNoArgTest">documentation for trait <code>fixture.FlatSpec</code></a>.
   * </p>
   */
  protected trait NoArgTest extends (() => Outcome) with TestData {

    /**
     * Runs the body of the test, returning an <code>Outcome</code>.
     */
    def apply(): Outcome
  }

  // Keep this out of the public until there's a use case demonstrating its need
  private[scalatest] object NoArgTest {
    def apply(test: NoArgTest)(f: => Outcome): NoArgTest = {
      new NoArgTest {
        def apply(): Outcome = { f }
        val text: String = test.text
        val configMap: ConfigMap = test.configMap
        val scopes: collection.immutable.IndexedSeq[String] = test.scopes
        val name: String = test.name
        val tags: Set[String] = test.tags
        val sourceInfo: SourceInfo = test.sourceInfo
      }
    }
  }

  /**
   * Run the passed test function in the context of a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, perform any clean
   * up needed after the test completes. Because the <code>NoArgTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as reassigning instance <code>var</code>s in this <code>Suite</code> or initializing
   * a globally accessible external database. If you want to avoid reassigning instance <code>var</code>s
   * you can use <a href="fixture/Suite.html">fixture.Suite</a>.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTest</code> invokes this method for each test, passing
   * in a <code>NoArgTest</code> whose <code>apply</code> method will execute the code of the test.
   * </p>
   *
   * <p>
   * This trait's implementation of this method simply invokes the passed <code>NoArgTest</code> function.
   * </p>
   *
   * @param test the no-arg test function to run with a fixture
   */
  protected def withFixture(test: NoArgTest): Outcome = {
    test()
  }

  /**
   * Run an async test.
   *
   * <p>
   * This method is redefine in this trait solely to narrow its contract. Subclasses must implement
   * this method to call the <code>withFixture(NoArgTest)</code> method, which is defined in this trait.
   * </p>
   *
   * <p>
   * This trait's implementation of this method simply returns <code>SucceededStatus</code> 
   * and has no other effect.
   * </p>
   *
   * @param testName the name of one async test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method
   *     has completed, and whether or not it failed.
   *
   * @throws NullArgumentException if either <code>testName</code> or <code>args</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = SucceededStatus
}

