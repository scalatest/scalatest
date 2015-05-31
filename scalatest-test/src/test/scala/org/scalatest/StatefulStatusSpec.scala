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

class StatefulStatusSpec extends fixture.FunSpec {

  protected type FixtureParam = {
    def setCompleted()
    def isCompleted: Boolean
    def succeeds(): Boolean
    def setFailed()
    def waitUntilCompleted()
    def whenCompleted(f: Boolean => Unit)
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
    }

    it("should throw IllegalStateException when setFailed() is called after setCompleted() is set") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      intercept[IllegalStateException] {
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

    it("should invoke a function registered with whenCompleted, passing a succeeded value, after the status completes successfully") { status =>
      import scala.language.reflectiveCalls

      @volatile var callbackInvoked = false
      @volatile var succeeded = false

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        succeeded = st
      }

      // ensure it was not executed yet
      assert(!callbackInvoked)

      // complete the status
      status.setCompleted()

      // ensure it was executed
      assert(callbackInvoked)

      // ensure it passed the correct success value
      assert(succeeded === true)
    }

    it("should invoke a function registered with whenCompleted, passing a failed value, after the status completes without success") { status =>
      import scala.language.reflectiveCalls

      @volatile var callbackInvoked = false
      @volatile var succeeded = true

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        succeeded = st
      }

      // ensure it was not executed yet
      assert(!callbackInvoked)

      // Fail it
      status.setFailed()

      // complete the status
      status.setCompleted()

      // ensure it was executed
      assert(callbackInvoked)

      // ensure it passed the correct success value
      assert(succeeded === false)
    }

    it("should invoke multiple functions registered with whenCompleted, passing a succeeded value, after the status completes successfully") { status =>
      import scala.language.reflectiveCalls

      // register two callbacks
      // ensure neither was executed yet
      // complete the status
      // ensure both were executed
      @volatile var firstCallbackInvoked = false
      @volatile var secondCallbackInvoked = false
      @volatile var firstSucceeded = false
      @volatile var secondSucceeded = false

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

      // complete the status
      status.setCompleted()

      // ensure they were executed
      assert(firstCallbackInvoked)
      assert(secondCallbackInvoked)

      // ensure they were passed the correct success value
      assert(firstSucceeded === true)
      assert(secondSucceeded === true)
    }

    it("should invoke multiple functions registered with whenCompleted, passing a failed value, after the status completes without success") { status =>

      import scala.language.reflectiveCalls

      // register two callbacks
      // ensure neither was executed yet
      // complete the status
      // ensure both were executed
      @volatile var firstCallbackInvoked = false
      @volatile var secondCallbackInvoked = false
      @volatile var firstSucceeded = true
      @volatile var secondSucceeded = true

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

      // Fail it
      status.setFailed()

      // complete the status
      status.setCompleted()

      // ensure they were executed
      assert(firstCallbackInvoked)
      assert(secondCallbackInvoked)

      // ensure they were passed the correct success value
      assert(firstSucceeded === false)
      assert(secondSucceeded === false)
    }

    // SKIP-SCALATESTJS-START
    it("should be serializable") { status =>
      SharedHelpers.serializeRoundtrip(status)
    }
    // SKIP-SCALATESTJS-END

  }
}

