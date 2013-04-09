/*
 * Copyright 2001-2011 Artima, Inc.
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

import org.scalatest._
import matchers.ShouldMatchers

class TestThreadsStartingCounterSpec extends fixture.WordSpec with ShouldMatchers with ConductorFixture {
  "A TestThreadsStartingCounter" should {
    "wait if one or more threads have called increment that haven't yet called decrement" in { conductor => import conductor._
      val counter = new TestThreadsStartingCounter
      thread {
        counter.increment
        waitForBeat(1)
        counter.decrement
      }
      thread {
        counter.waitUntilAllTestThreadsHaveStarted()
        beat should be (1) // TODO: On Azul and Mac this failed with 0 was not equal to 1. I think there's a race condition in this test.
        // Is it possible that this second thread goes before the first one, so it doesn't know. Yes, because this is in a thread. Race condition.
        // The race condition was in the actual class. I fixed it, and so far this test hasn't passed. Makes one nervous to make a change like that. - bv 4/8/2011
      }
    }
    "go right ahead if the same number of threads have called increment and decrement" in { conductor => import conductor._
      val counter = new TestThreadsStartingCounter
      thread {
        counter.increment
        counter.decrement
        beat should be (0) // Failed with: 1 was not equal to 0
      }
      thread {
        waitForBeat(1)
        counter.waitUntilAllTestThreadsHaveStarted()
        beat should be (1)
      }
    }
  }
}
