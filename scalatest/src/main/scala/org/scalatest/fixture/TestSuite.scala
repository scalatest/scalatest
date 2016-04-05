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
package org.scalatest.fixture

import org.scalatest._
import OutcomeOf.outcomeOf
import org.scalactic.SourceInfo

trait TestSuite extends org.scalatest.fixture.Suite with org.scalatest.TestSuite { thisTestSuite =>

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
     * that takes a <code>NoArgTest</code>. For example, if a <code>fixture.Suite</code> 
     * mixes in <code>SeveredStackTraces</code>, it will inherit an implementation
     * of <code>withFixture(NoArgTest)</code> provided by
     * <code>SeveredStackTraces</code> that implements the stack trace severing
     * behavior. If the <code>fixture.Suite</code> does not delegate to that
     * <code>withFixture(NoArgTest)</code> method, the stack trace severing behavior
     * will not happen. Here's how that might look in a <code>fixture.Suite</code>
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
        val sourceInfo = thisOneArgTest.sourceInfo
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
        val sourceInfo: SourceInfo = test.sourceInfo
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

  private[fixture] class TestFunAndConfigMap(val name: String, test: FixtureParam => Any, val configMap: ConfigMap)
    extends OneArgTest {

    def apply(fixture: FixtureParam): Outcome = {
      outcomeOf { test(fixture) }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
    val sourceInfo = testData.sourceInfo
  }

  private[fixture] class FixturelessTestFunAndConfigMap(override val name: String, test: () => Any, override val configMap: ConfigMap)
    extends NoArgTest {

    def apply(): Outcome = {
      outcomeOf { test() }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
    val sourceInfo = testData.sourceInfo
  }

}

