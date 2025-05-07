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
 * This trait provides two `Using.Manager` instances to support different scopes:
 *
 * 1. **Suite-scoped manager**:  
 *    - Accessible via the `suiteScoped` method.  
 *    - Intended for managing resources shared across multiple tests in the suite.  
 *    - This manager is only available during test execution; calling `suiteScoped`
 *      outside of a test (e.g., during suite construction) will throw an `IllegalStateException`.
 *    - To use this safely, define a `lazy val` in your suite that calls `suiteScoped`
 *      when first accessed, ensuring it is evaluated during test execution.
 *
 * 2. **Test-scoped manager**:  
 *    - Passed as a fixture parameter to each test (`FixtureParam`).  
 *    - Automatically created before and disposed after each test.  
 *    - Intended for resources needed only within a single test.
 *
 * Example usage:
 * {{{
 * class MyAsyncSuite extends AsyncResourceManagerFixture {
 *   lazy val sharedClient = suiteScoped(new ExpensiveClient)
 *
 *   test("something async") { use =>
 *     val connection = use(sharedClient.getConnection())
 *     Future {
 *       // Use connection asynchronously
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
   * Resources registered with this manager are automatically released after all tests in 
   * the suite finishes execution.
   */
  override type FixtureParam = Using.Manager

  private val suiteManagerRef: AtomicReference[Option[Using.Manager]] = new AtomicReference(None)

  /**
   * Returns the suite-scoped `Using.Manager` instance.
   *
   * This manager is valid only during the execution of test. It is intended
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
  override protected def runTests(testName: Option[String], args: Args): Status = {
    val manager = new Using.Manager()
    suiteManagerRef.getAndSet(Some(manager))
    val status = super.runTests(testName, args)
    status.whenCompleted { _ =>
      manager.close()}
    status
  }
}