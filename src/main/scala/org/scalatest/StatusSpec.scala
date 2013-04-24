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

class StatusSpec extends Spec {

  object `SucceededStatus ` {

    def `should invoke a function registered with whenCompleted, passing a succeeded value` {

      @volatile var callbackInvoked = false
      @volatile var succeeded = false

      val status = SucceededStatus

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        succeeded = st
      }

      // ensure it was executed
      assert(callbackInvoked)
      assert(succeeded === true)
    }

    def `should invoke multiple functions registered with whenCompleted, passing a succeeded value` {
      // register two callbacks
      // ensure neither was executed yet
      // complete the status
      // ensure both were executed
      @volatile var firstCallbackInvoked = false
      @volatile var secondCallbackInvoked = false
      @volatile var firstSucceeded = false
      @volatile var secondSucceeded = false

      val status = SucceededStatus

      // register callback 1
      status.whenCompleted { st =>
        firstCallbackInvoked = true
        firstSucceeded = st
      }

      // register callback 2
      status.whenCompleted { st =>
        secondCallbackInvoked = true
        secondSucceeded = st
      }

      // ensure it was executed
      assert(firstCallbackInvoked)
      assert(secondCallbackInvoked)

      // ensure it passed the correct success value
      assert(firstSucceeded === true)
      assert(secondSucceeded === true)
    }
  }

  object `FailedStatus ` {

    def `should invoke a function registered with whenCompleted, passing a failed value` {

      @volatile var callbackInvoked = false
      @volatile var succeeded = true

      val status = FailedStatus

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        succeeded = st
      }

      // ensure it was executed
      assert(callbackInvoked)
      assert(succeeded === false)
    }

    def `should invoke multiple functions registered with whenCompleted, passing a failed value` {
      // register two callbacks
      // ensure neither was executed yet
      // complete the status
      // ensure both were executed
      @volatile var firstCallbackInvoked = false
      @volatile var secondCallbackInvoked = false
      @volatile var firstSucceeded = true
      @volatile var secondSucceeded = true

      val status = FailedStatus

      // register callback 1
      status.whenCompleted { st =>
        firstCallbackInvoked = true
        firstSucceeded = st
      }

      // register callback 2
      status.whenCompleted { st =>
        secondCallbackInvoked = true
        secondSucceeded = st
      }

      // ensure it was executed
      assert(firstCallbackInvoked)
      assert(secondCallbackInvoked)

      // ensure it passed the correct success value
      assert(firstSucceeded === false)
      assert(secondSucceeded === false)
    }
  }
}

