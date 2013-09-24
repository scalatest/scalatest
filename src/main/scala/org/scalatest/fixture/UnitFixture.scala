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
 * Trait that when mixed into a <code>fixture.Suite</code> passes
 * the unit value as a fixture into each test.
 *
 * <p>
 * Since a unit value is unlikely to be of much use to a test, this trait is useful
 * when the unit value fixture is actually never passed into any tests. Instead
 * each test in the <code>fixture.Suite</code> is defined as a <em>no-arg</em> function; no tests are defined as one-arg functions.
 * This should be quite rare, but occasionally can be useful.
 * For an example, see the main documentation for trait <a href="NoArg.html"><code>NoArg</code></a>.
 * </p>
*/
trait UnitFixture { this: fixture.Suite =>

  /**
   * The type of the fixture, which is <code>Unit</code>.
   */
  type FixtureParam = Unit

  /**
   * Invoke the test function, passing <code>()</code>, the unit value, to the the test function.
   *
   * <p>
   * To enable stacking of traits that define <code>withFixture(NoArgTest)</code>, this method does not
   * invoke the test function directly. Instead, it delegates responsibility for invoking the test function
   * to <code>withFixture(NoArgTest)</code>.
   * </p>
   *
   * @param test the <code>OneArgTest</code> to invoke, passing in the unit value
   * @return an <code>Outcome</code> instance
   */
  def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(()))
  }
}
