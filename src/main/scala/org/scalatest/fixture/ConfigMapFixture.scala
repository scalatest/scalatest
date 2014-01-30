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
 * Trait that when mixed into a <a href="Suite.html"><code>fixture.Suite</code></a> passes
 * the config map passed to <code>runTest</code> as a fixture into each test.
 *
 * <p>
 * Here's an example in which tests just check to make sure <code>"hello"</code> and <code>"world"</code>
 * are defined keys in the config map:
 * </p>
 *
 * <pre>
 * package org.scalatest.examples.fixture.configmapfixture
 * 
 * import org.scalatest._
 * 
 * class ExampleSpec extends fixture.FlatSpec with fixture.ConfigMapFixture with ShouldMatchers {
 * 
 *   "The config map" should "contain hello" in { configMap =&gt;
 *     // Use the configMap passed to runTest in the test
 *     configMap should contain key "hello"
 *   }
 * 
 *   it should "contain world" in { configMap =&gt;
 *     configMap should contain key "world"
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this class without defining <code>"hello"</code> and <code>"world"</code>
 * in the confg map, the tests will fail:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSpec execute
 * <span class="stGreen">ExampleSpec:
 * The config map</span>
 * <span class="stRed">- should contain hello *** FAILED ***
 *   Map() did not contain key "hello" (<console>:20)
 * - should contain world *** FAILED ***
 *   Map() did not contain key "world" (<console>:24)</span>
 * </pre>
 * 
 * <p>
 * If you do define <code>"hello"</code> and <code>"world"</code> keys
 * in the confg map, the tests will success:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSpec execute (configMap = Map("hello" -&gt; "hi", "world" -&gt; "globe"))
 * <span class="stGreen">ExampleSpec:
 * The config map
 * - should contain hello
 * - should contain world</span>
 * </pre>
 * 
 * @author Bill Venners
 */
trait ConfigMapFixture { this: fixture.Suite =>

  /**
   * The type of the <code>configMap</code>, which is <code>ConfigMap</code>.
   */
  type FixtureParam = ConfigMap

  /**
   * Invoke the test function, passing to the the test function the <code>configMap</code>
   * obtained by invoking <code>configMap</code> on the passed <code>OneArgTest</code>.
   *
   * <p>
   * To enable stacking of traits that define <code>withFixture(NoArgTest)</code>, this method does not
   * invoke the test function directly. Instead, it delegates responsibility for invoking the test function
   * to <code>withFixture(NoArgTest)</code>.
   * </p>
   *
   * @param test the <code>OneArgTest</code> to invoke, passing in the
   *   <code>configMap</code> fixture
   */
  def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(test.configMap))
  }
}
