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
package fixture

import java.util.concurrent.atomic.AtomicReference
import org.scalactic.Using

/**
 * A trait that facilitates automatic management and cleanup of resources 
 * in asynchronous tests using `org.scalactic.Using.Manager`.
 *
 * A **Test-scoped manager** is
 *    - Passed as a fixture parameter to each test (`FixtureParam`).  
 *    - Automatically created before and disposed after each test.  
 *    - Intended for resources needed only within a single test.
 *    - If you need to manage resources shared across tests,
 *      check out `ResourceManager` in the `org.scalatest` package
 *
 * Example usage:
 * {{{
 * class MyAsyncSuite extends AsyncFunSuite with AsyncResourceManagerFixture {
 *
 *   test("something async") { use =>
 *     val connection = use(getClass().getResourceAsStream("some-resource"))
 *     Future {
 *       // Use stream in the test
 *       // Stream will be closed after the test
 *     }
 *   }
 * }
 * }}}
 *
 * @note This trait extends `FixtureAsyncTestSuite` and sets `FixtureParam` to `Using.Manager`.
 * @see [[org.scalactic.Using.Manager]] for details on how resource management is handled safely.
 */
trait AsyncResourceManagerFixture extends org.scalatest.FixtureAsyncTestSuite {
  /**
   * The fixture parameter passed to each test, which is an instance of `Using.Manager`.
   * Resources registered with this manager are automatically released after the test.
   */
  override type FixtureParam = Using.Manager

  /**
   * Wraps the execution of a single asynchronous test with a test-scoped `Using.Manager`,
   * which is passed as the fixture parameter.
   *
   * The manager is automatically closed after the test completes, ensuring
   * all registered resources are properly released—even if the test returns a failed or
   * completed `Future`.
   *
   * @param test the asynchronous test function that receives the `Using.Manager`
   * @return a `FutureOutcome` representing the result of the test
   */
  protected def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val manager = new Using.Manager()
    try {
      val futureOutcome = withFixture(test.toNoArgAsyncTest(manager))
      futureOutcome.onCompletedThen { _ =>
        manager.close()
      }
    } catch {
      case e: Throwable => // When exception is thrown code not within the Future
        manager.close()
        throw e
    }
  }

}
