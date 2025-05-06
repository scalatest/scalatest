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
 * A Trait that facilitates the management of resources that need to be
 * disposed of after using them.
 * It provides two separate instances of the <code>Using.Manager</code>
 * class from the Scala standard library (see the Scala standard library
 * documentationon <code>scala.util.Using</code> for details on how it
 * works).
 * 
 * The first instance can be obtained by calling the <code>suiteScoped</code>
 * method. Its purpose is to clean up resources that are shared between
 * different tests in a suite. This method can only be called after test
 * execution has started. It is therefore recommended to use it to initialize
 * a <code>lazy val</code> in the Suite class. This way, the 
 * <code>lazy val</code> can be used to access the resource while deferring
 * the <code>suiteScoped</code> call until test execution has started.
 * 
 * The second instance is passed as a fixture parameter to the test.
 * It is cleaned up after the test has completed and can therefore be used
 * to clean up resources that are only required inside a single test.
 *
*/
trait AsyncResourceManagerFixture extends org.scalatest.FixtureAsyncTestSuite {
  override type FixtureParam = Using.Manager

  private val suiteManagerRef: AtomicReference[Option[Using.Manager]] = new AtomicReference(None)

  protected def suiteScoped = suiteManagerRef.get().getOrElse {
    throw new IllegalStateException(
      "`suiteScoped`` cannot be called from outside a test. " +
      "Use a `lazy val` to store Suite-scoped resources in order to defer " +
      "initialization of such resources until the start of a test."
    )
  }
  
  protected def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val manager = new Using.Manager()
    val futureOutcome = withFixture(test.toNoArgAsyncTest(manager))
    futureOutcome.onCompletedThen { _ =>
      manager.close()
    }
  }

  override protected def runTests(testName: Option[String], args: Args): Status = {
    val manager = new Using.Manager()
    suiteManagerRef.getAndSet(Some(manager))
    val status = super.runTests(testName, args)
    status.whenCompleted { _ =>
      manager.close()}
    status
  }
}