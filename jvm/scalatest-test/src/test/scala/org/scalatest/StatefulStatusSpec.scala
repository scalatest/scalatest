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

import OptionValues._
import scala.util.{Try, Success}
import org.scalatest.funspec

class StatefulStatusSpec extends funspec.FixtureAnyFunSpec {

  protected type FixtureParam = StatefulStatus

   override protected def withFixture(test: OneArgTest): Outcome = {
     val status1 = new StatefulStatus
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
      assert(!status.isCompleted())
    }

    it("should return true for isCompleted after setCompleted() is called") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      assert(status.isCompleted())
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should return true for succeeds() after setCompleted() is called without setFailed()") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      assert(status.succeeds())
    }

    it("should return false for succeeds() after setCompleted() is called after setFailed()") { status =>
      import scala.language.reflectiveCalls
      status.setFailed()
      status.setCompleted()
      assert(!status.succeeds())
    }
    // SKIP-SCALATESTJS,NATIVE-END

    // SKIP-SCALATESTJS,NATIVE-START
    it("waitUntilCompleted should not block after setCompleted() is called") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      status.waitUntilCompleted()
    }
    // SKIP-SCALATESTJS,NATIVE-END

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
      assert(status.isCompleted())
      status.setCompleted()
      assert(status.isCompleted())
      status.setCompleted()
      assert(status.isCompleted())
    }

    it("should invoke a function registered with whenCompleted, passing a succeeded value, after the status completes successfully") { status =>
      import scala.language.reflectiveCalls

      @volatile var callbackInvoked = false
      @volatile var succeeded = false

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        succeeded = (st == Success(true))
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
        succeeded = (st == Success(true))
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

    // SKIP-SCALATESTJS,NATIVE-START
    it("should be serializable") { status =>
      SharedHelpers.serializeRoundtrip(status)
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should not replace previous failed exception if it is already set") { status =>
      val e1 = new RuntimeException("exception 1")
      val e2 = new RuntimeException("exception 2")

      status.setFailedWith(e1)
      assert(status.unreportedException.value.getMessage == "exception 1")

      status.setFailedWith(e2)
      assert(status.unreportedException.value.getMessage == "exception 1")
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should allow setCompleted to be called multiple times, any after the first being a no-op") { status =>

      var firstCallbackCompleted = false
      var secondCallbackCompleted = false
      status.whenCompleted { tri =>
        println(s"Thread ${Thread.currentThread.toString} starting first callback")
        Thread.sleep(100)
        firstCallbackCompleted = true
        assert(!secondCallbackCompleted)
        println(s"Thread ${Thread.currentThread.toString} finished first callback")
      }
      status.whenCompleted { tri =>
        println(s"Thread ${Thread.currentThread.toString} starting second callback")
        secondCallbackCompleted = true
        println(s"Thread ${Thread.currentThread.toString} finished second callback")
      }

      val runnable = 
        new Runnable {
          def run() = {
            status.setCompleted()
          }
        }

      (new Thread(runnable)).start() // And this should wait until the other thread also completes the second one
      status.setCompleted() // This should start executing the first callback,
      // Make sure the second thread isn't the one to complete the
      // second callback, while the first one is still in the thread.sleep.
      succeed
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
}

