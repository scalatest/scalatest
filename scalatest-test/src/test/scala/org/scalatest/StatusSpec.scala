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

import scala.util.Success

class StatusSpec extends fixture.FunSpec {
  
  protected type FixtureParam = { 
    def setCompleted()
    def isCompleted: Boolean
    // SKIP-SCALATESTJS-START
    def succeeds(): Boolean
    // SKIP-SCALATESTJS-END
    def setFailed()
    // SKIP-SCALATESTJS-START
    def waitUntilCompleted()
    // SKIP-SCALATESTJS-END
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
  
  describe("StatefulStatus ") {
    it("should by default return false for isCompleted") { status =>
      import scala.language.reflectiveCalls
      assert(!status.isCompleted)
    }
    
    it("should return true for isCompleted after completes() is called") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      assert(status.isCompleted)
    }
    
  // SKIP-SCALATESTJS-START
    it("should return true for succeeds() after completes() is called without fails()") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      assert(status.succeeds)
    }
    
    it("should return false for succeeds() after completes is called after fails()") { status =>
      import scala.language.reflectiveCalls
      status.setFailed()
      status.setCompleted()
      assert(!status.succeeds)
    }
    
    it("waitUntilCompleted should not block after completes() is called") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      status.waitUntilCompleted()
      succeed
    }
    // SKIP-SCALATESTJS-END
    
    it("should throw IllegalStateException when setFailed() is called after setCompleted() is set") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      assertThrows[IllegalStateException] {
        status.setFailed()
      }
    }
    
    it("should allow setCompleted() to be called multiple times") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      assert(status.isCompleted)
      status.setCompleted()
      assert(status.isCompleted)
      status.setCompleted()
      assert(status.isCompleted)
    }

    // SKIP-SCALATESTJS-START
    it("should be serializable") { status =>
      SharedHelpers.serializeRoundtrip(status)
      succeed
    }
    // SKIP-SCALATESTJS-END
  }

  describe("SucceededStatus ") {

    it("should invoke a function registered with whenCompleted, passing a succeeded value") { status =>

      @volatile var callbackInvoked = false
      @volatile var succeeded = false

      val status = SucceededStatus

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        succeeded = (st == Success(true))
      }

      // ensure it was executed
      assert(callbackInvoked)
      assert(succeeded === true)
    }

    it("should invoke multiple functions registered with whenCompleted, passing a succeeded value") { status =>
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
        firstSucceeded = (st == Success(true))
      }

      // register callback 2
      status.whenCompleted { st =>
        secondCallbackInvoked = true
        secondSucceeded = (st == Success(true))
      }

      // ensure it was executed
      assert(firstCallbackInvoked)
      assert(secondCallbackInvoked)

      // ensure it passed the correct success value
      assert(firstSucceeded === true)
      assert(secondSucceeded === true)
    }
  }

  describe("FailedStatus ") {

    it("should invoke a function registered with whenCompleted, passing a failed value") { status =>

      @volatile var callbackInvoked = false
      @volatile var succeeded = true

      val status = FailedStatus

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        succeeded = (st == Success(true))
      }

      // ensure it was executed
      assert(callbackInvoked)
      assert(succeeded === false)
    }

    it("should invoke multiple functions registered with whenCompleted, passing a failed value") { status =>
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
        firstSucceeded = (st == Success(true))
      }

      // register callback 2
      status.whenCompleted { st =>
        secondCallbackInvoked = true
        secondSucceeded = (st == Success(true))
      }

      // ensure it was executed
      assert(firstCallbackInvoked)
      assert(secondCallbackInvoked)

      // ensure it passed the correct success value
      assert(firstSucceeded === false)
      assert(secondSucceeded === false)
    }
  }

  describe("CompositeStatus ") {
    it("should invoke multiple functions registered with whenCompleted, passing a succeeded value, only after all composed statuses complete successfully") { status =>

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
        firstSucceeded = (st == Success(true))
      }

      // register callback 2
      status.whenCompleted { st =>
        secondCallbackInvoked = true
        secondSucceeded = (st == Success(true))
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
