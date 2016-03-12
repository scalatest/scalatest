/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalatest.exceptions.TimeoutField
import org.scalatest.time.Span
import org.scalatest._

trait AsyncTimeLimitedTests extends AsyncTestSuiteMixin with TimeLimits { this: AsyncTestSuite =>

  abstract override def withFixture(test: NoArgAsyncTest): FutureOutcome = {

    try {
      failAfter(timeLimit) {
        super.withFixture(test)
      } change { outcome =>
        outcome match {
          case Exceptional(e: org.scalatest.exceptions.ModifiableMessage[_] with TimeoutField) =>
            Exceptional(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString))))
          case other => other
        }
      }
    }
    catch {
      case e: org.scalatest.exceptions.ModifiableMessage[_] with TimeoutField =>
        throw e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString)))
      case other: Throwable => throw other
    }
  }

  //protected def reportTimeout(e: Throwable): Outcome = Exceptional(e)

  /**
   * The time limit, in milliseconds, in which each test in a <code>Suite</code> that mixes in
   * <code>TimeLimitedTests</code> must complete.
   */
  def timeLimit: Span
}
