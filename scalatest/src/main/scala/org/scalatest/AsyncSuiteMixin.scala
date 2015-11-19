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

import scala.concurrent.Future

/**
 * Trait defining abstract "lifecycle" methods that are implemented in <a href="Suite.html#lifecycle-methods"><code>Suite</code></a> and can
 * be overridden in stackable modification traits.
 *
 * <p>
 * The main purpose of <code>SuiteMixin</code> is to differentiate core <code>Suite</code>
 * style traits, such as <a href="Spec.html"><code>Spec</code></a>, <a href="FunSuite.html"><code>FunSuite</code></a>, and <a href="FunSpec.html"><code>FunSpec</code></a> from stackable
 * modification traits for <code>Suite</code>s such as <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a>, <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>,
 * and <a href="SequentialNestedSuiteExecution.html"><code>SequentialNestedSuiteExecution</code></a>. Because these stackable traits extend <code>SuiteMixin</code>
 * instead of <code>Suite</code>, you can't define a suite by simply extending one of the stackable traits:
 * </p>
 *
 * <pre class="stHighlight">
 * class MySuite extends BeforeAndAfterEach // Won't compile
 * </pre>
 *
 * <p>
 * Instead, you need to extend a core <code>Suite</code> trait and mix the stackable <code>BeforeAndAfterEach</code> trait
 * into that, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * class MySuite extends FunSuite with BeforeAndAfterEach // Compiles fine
 * </pre>
 *
 * @author Bill Venners
 */
trait AsyncSuiteMixin extends SuiteMixin { this: AsyncSuite =>

  /**
   * Runs the passed test function with a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, perform any clean
   * up needed after the test completes. Because the <code>NoArgTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as initializing an external database.
   * </p>
   *
   * @param test the no-arg test function to run with a fixture
   */
  protected def withAsyncFixture(test: NoArgAsyncTest): Future[Outcome]
}

