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
package org.scalatest.concurrent

import org.scalatest.time.Span
import org.scalatest._

import scala.concurrent.Future

/*
This should be moved up to org.scalatest to be symetric to CancelAfterFailure. Also consider
adding the final newInstance method to prevent it being mixed in with OneInstancePerTest.
AFTER MOVING, DROP THE .. in the link to AsyncTestSuite below
*/

 /* ADD THIS LATER AFTER DOING THE ENHANCEMENT:
 * <p>
 * Note that this trait only cancels tests in the same <code>AsyncTestSuite</code> instance, because
 * it uses a private volatile instance variable as a flag to indicate whether or not a test has failed.
 * If you are running each test in its own instance, therefore, it would not cancel the
 * remaining tests, because they would not see the same flag. For this reason, this trait contains
 * a final implementation of a method defined in <a href="../OneInstancePerTest.html"><code>OneInstancePerTest</code></a>,
 * to prevent it from being mixed into any class that also mixes in <code>OneInstancePerTest</code>, 
 * including by mixing in <a href="../ParallelTestExecution.html"><code>ParallelTestExecution</code></a>
 * or a <a href="path/package.html">path traits</a>.
 * </p>
 */
/**
 * Trait that when mixed into a <a href="../AsyncTestSuite.html"><code>AsyncTestSuite</code></a> cancels any remaining tests in that
 * <code>AsyncTestSuite</code> instance after a test fails.
 *
 * <p>
 * The intended use case for this trait is if you have a suite of long-running tests that are
 * related such that if one fails, you aren't interested in running the others, you can use this
 * trait to simply cancel any remaining tests, so you need not wait long for them to complete.
 * </p>
 *
 */
trait AsyncCancelAfterFailure extends AsyncTestSuiteMixin { this: AsyncTestSuite =>

  @volatile private var cancelRemaining = false

  /**
   * Stackable implementation of <code>withFixture</code> that cancels the current test if
   * any previous test run in this <code>AsyncSuite</code> instance has failed.
   */
  abstract override def withFixture(test: NoArgAsyncTest): FutureOutcome = {

    if (cancelRemaining)
      FutureOutcome.canceled("Canceled by CancelOnFailure because a test failed previously")
    else
      super.withFixture(test) onFailedThen { _ =>
        cancelRemaining = true
      }
  }
}
