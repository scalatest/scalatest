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

trait AsyncSuite extends org.scalatest.fixture.Suite with org.scalatest.AsyncSuite {

  final override def withFixture(test: OneArgTest): Outcome = {
    throw new exceptions.NotAllowedException(FailureMessages.withFixtureNotAllowedInAsyncFixtures, getStackDepthFun("AsyncFixtures.scala", "withFixture"))
  }

  /**
   * A test function taking no arguments and returning an <code>Future[Outcome]</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the
   * <a href="AsyncFlatSpec.html#withAsyncFixtureNoArgAsyncTest">documentation for trait <code>fixture.AsyncFlatSpec</code></a>.
   * </p>
   */
  trait OneArgAsyncTest extends (FixtureParam => Future[Outcome]) with TestData { thisOneArgAsyncTest =>

    /**
     * Using the passed <code>FixtureParam</code>, produces a <code>Future[Outcome]</code> representing
     * the future outcome of this asynchronous test.
     *
     * @param fixture the <code>FixtureParam</code>
     * @return an instance of <code>Future[Outcome]</code>
     */
    def apply(fixture: FixtureParam): Future[Outcome]

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
     * Here's how that might look in a <code>fixture.AsyncSuite</code>
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
        def apply(): Future[Outcome] = { thisOneArgAsyncTest(fixture) }
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
   * <a href="AsyncSuite.html">main documentation for this trait</a>.
   * </p>
   *
   * @param test the <code>OneArgAsyncTest</code> to invoke, passing in a fixture
   * @return an instance of <code>Future[Outcome]</code>
   */
  def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome]
}
