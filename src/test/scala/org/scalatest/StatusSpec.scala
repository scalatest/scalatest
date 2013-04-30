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

class StatusSpec extends fixture.Spec {
  
  protected type FixtureParam = { 
    def setCompleted()
    def isCompleted: Boolean
    def succeeds(): Boolean
    def setFailed()
    def waitUntilCompleted()
  }
  
   override protected def withFixture(test: OneArgTest): Outcome = {
     val status1 = new ScalaTestStatefulStatus
     test(status1) match {
       case Succeeded =>
         val status2 = new StatefulStatus
         test(status2)
       case other => other
     }
   }
  
  object `StatefulStatus ` {
    def `should by default return false for isCompleted`(status: FixtureParam) {
      assert(!status.isCompleted)
    }
    
    def `should return true for isCompleted after completes() is called`(status: FixtureParam) {
      status.setCompleted()
      assert(status.isCompleted)
    }
    
    def `should return true for succeeds() after completes() is called without fails()`(status: FixtureParam) {
      status.setCompleted()
      assert(status.succeeds)
    }
    
    def `should return false for succeeds() after completes is called after fails()`(status: FixtureParam) {
      status.setFailed()
      status.setCompleted()
      assert(!status.succeeds)
    }
    
    def `waitUntilCompleted should not block after completes() is called`(status: FixtureParam) {
      status.setCompleted()
      status.waitUntilCompleted()
    }
    
    def `should throw IllegalStateException when setFailed() is called after setCompleted() is set`(status: FixtureParam) {
      status.setCompleted()
      intercept[IllegalStateException] {
        status.setFailed()
      }
    }
    
    def `should allow setCompleted() to be called multiple times`(status: FixtureParam) {
      status.setCompleted()
      assert(status.isCompleted)
      status.setCompleted()
      assert(status.isCompleted)
      status.setCompleted()
      assert(status.isCompleted)
    }

    def `should be serializable`(status: FixtureParam) {
      SharedHelpers.serializeRoundtrip(status)
    }
  }

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

  object `CompositeStatus ` {
    def `should invoke multiple functions registered with whenCompleted, passing a succeeded value, only after all composed statuses complete successfully` {

      @volatile var firstCallbackInvoked = false
      @volatile var secondCallbackInvoked = false
      @volatile var firstSucceeded = false
      @volatile var secondSucceeded = false

      val nestedStatus1 = new StatefulStatus
      val nestedStatus2 = new StatefulStatus
      val status = new CompositeStatus(Set(nestedStatus1, nestedStatus2))

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

      // ensure they were not executed yet
      assert(!firstCallbackInvoked)
      assert(!secondCallbackInvoked)

      // complete the first status
      nestedStatus1.setCompleted()

      // ensure they were not executed yet
      assert(!firstCallbackInvoked)
      assert(!secondCallbackInvoked)

      // complete the second status
      nestedStatus2.setCompleted()

      // ensure they were executed
      assert(firstCallbackInvoked)
      assert(secondCallbackInvoked)

      // ensure they were passed the correct success value
      assert(firstSucceeded === true)
      assert(secondSucceeded === true)
    }
  }
}
