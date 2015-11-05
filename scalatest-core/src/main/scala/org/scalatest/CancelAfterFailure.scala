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

/**
 * Trait that when mixed into a <a href="Suite.html"><code>Suite</code></a> cancels any remaining tests in that
 * <code>Suite</code> instance after a test fails.
 *
 * <p>
 * The intended use case for this trait is if you have a suite of long-running tests that are
 * related such that if one fails, you aren't interested in running the others, you can use this
 * trait to simply cancel any remaining tests, so you need not wait long for them to complete.
 * </p>
 *
 * <p>
 * Note that this trait only cancels tests in the same <code>Suite</code> instance, because
 * it uses a private volatile instance variable as a flag to indicate whether or not a test has failed.
 * If you are running each test in its own instance, therefore, it would not cancel the
 * remaining tests, because they would not see the same flag. For this reason, this trait contains
 * a final implementation of a method defined in <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>,
 * to prevent it from being mixed into any class that also mixes in <code>OneInstancePerTest</code>, 
 * including by mixing in <a href="ParallelTestExecution.html"><code>ParallelTestExecution</code></a> or a <a href="path/package.html">path traits</a>.
 * </p>
 */
trait CancelAfterFailure extends SuiteMixin { this: Suite =>

  @volatile private var cancelRemaining = false

  /**
   * Stackable implementation of <code>withFixture</code> that cancels the current test if
   * any previous test run in this <code>Suite</code> instance has failed.
   */
  abstract override def withFixture(test: NoArgTest): Outcome = {
    if (cancelRemaining) 
      Canceled("Canceled by CancelOnFailure because a test failed previously")
    else
      super.withFixture(test) match {
        case failed: Failed =>
          cancelRemaining = true
          failed
        case outcome => outcome
      }
  }

  /**
   * Method defined to prevent this trait from being mixed into any class that also mixes in <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.
   */
  final def newInstance: Suite with OneInstancePerTest = throw new UnsupportedOperationException
}
