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

/**
 * Trait that when mixed into a <a href="Suite.html"><code>fixture.Suite</code></a> passes the
 * <a href="../TestData.html"><code>TestData</code></a> passed to <code>withFixture</code> as a fixture into each test.
 *
 * <p>
 * For example, here's how you could access the test's name in each test using <code>TestDataFixture</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.testdatafixture
 *
 * import org.scalatest._
 *
 * class ExampleSpec extends fixture.FlatSpec with fixture.TestDataFixture {
 *
 *   "Accessing the test data" should "be easy!" in { td =&gt;
 *     assert(td.name == "Accessing the test data should be easy!")
 *   }
 *
 *   it should "be fun!" in { td =&gt;
 *     assert(td.name == "Accessing the test data should be fun!")
 *   }
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait TestDataFixture { this: fixture.Suite =>

  /**
   * The type of the fixture, which is <code>TestData</code>.
   */
  type FixtureParam = TestData

  /**
   * Invoke the test function, passing to the the test function 
   * the <code>TestData</code> for the test.
   *
   * <p>
   * To enable stacking of traits that define <code>withFixture(NoArgTest)</code>, this method does not
   * invoke the test function directly. Instead, it delegates responsibility for invoking the test function
   * to <code>withFixture(NoArgTest)</code>.
   * </p>
   *
   * @param test the <code>OneArgTest</code> to invoke, passing in the
   *   <code>TestData</code> fixture
   * @return an <code>Outcome</code> instance
   */
  def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(test))
  }
}
