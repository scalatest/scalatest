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

import java.util.concurrent.atomic.AtomicReference
import org.scalactic.Using

/**
 * A trait that facilitates automatic management and cleanup of resources 
 * in tests using `org.scalactic.Using.Manager`.
 * This trait provides a **SuiteScoped `Using.Manager`**
 *
 *    - Accessible via the `suiteScoped` method.  
 *    - Intended for managing resources shared across multiple tests in the suite. If you
 *      need to manage resources/perform cleanup from individual tests, check out
 *      `ResourceManagerFixture` in the `org.scalatest.fixture` package
 *    - This manager is only available during test execution; calling `suiteScoped`
 *      outside of a test (e.g., during suite construction) will throw an `IllegalStateException`.
 *    - To use this safely, define a `lazy val` in your suite that calls `suiteScoped`
 *      when first accessed, ensuring it is evaluated during test execution.
 *      Note that this also means that the resource might not be initialized at all,
 *      depending on which tests in the suite are run and which ones access the resource. 
 * 
 * Example usage:
 * {{{
 * class MySuite extends ResourceManager {
 *   lazy val sharedClient = suiteScoped(new ExpensiveClient)
 *
 *   test("something") {
 *     // Use `sharedClient` inside the test.
 *     // `sharedClient` will be closed after all tests have finished executing
 *   }
 * }
 * }}}
 */
trait ResourceManager extends Suite {
  private val suiteManagerRef: AtomicReference[Option[Using.Manager]] = new AtomicReference(None)

  /**
   * Returns the suite-scoped `Using.Manager` instance.
   *
   * This manager is valid only during the execution of the test suite. It is intended
   * for managing shared resources that are reused across multiple tests.
   *
   * This method must be called during test execution, such as from within a `lazy val`
   * or a method invoked inside a test body. Calling this method outside of test execution
   * (e.g., during class construction) will result in an `IllegalStateException`.
   *
   * @throws IllegalStateException if called outside of test execution
   * @return the shared `Using.Manager` instance for the test suite
   */
  protected def suiteScoped = suiteManagerRef.get().getOrElse {
    throw new IllegalStateException(Resources.suiteScopedCannotBeCalledFromOutsideATest)
  }

  /**
   * Runs all tests in the suite with a suite-scoped `Using.Manager` instance.
   *
   * This manager is initialized at the beginning of test execution and released
   * after all tests have completed. It is stored internally and made available
   * via `suiteScoped`.
   *
   * @param testName an optional name of a specific test to run
   * @param args the test run arguments
   * @return a `Status` representing the asynchronous result of the test run
   */
  override protected def runTests(testName: Option[String], args: Args): Status =
    Using.Manager { manager =>
      suiteManagerRef.getAndSet(Some(manager))
      super.runTests(testName, args)
    }.get

}
