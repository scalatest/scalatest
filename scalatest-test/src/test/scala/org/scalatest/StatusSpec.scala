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

import scala.concurrent.ExecutionException
import scala.util.{Failure, Success}


class StatusSpec extends fixture.FunSpec {
  
  protected type FixtureParam = { 
    def setCompleted()
    def isCompleted(): Boolean
    // SKIP-SCALATESTJS,NATIVE-START
    def succeeds(): Boolean
    // SKIP-SCALATESTJS,NATIVE-END
    def setFailed()
    // SKIP-SCALATESTJS,NATIVE-START
    def waitUntilCompleted()
    // SKIP-SCALATESTJS,NATIVE-END
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
      assert(!status.isCompleted())
    }
    
    it("should return true for isCompleted after completes() is called") { status =>
      import scala.language.reflectiveCalls
      status.setCompleted()
      assert(status.isCompleted())
    }
    
  // SKIP-SCALATESTJS,NATIVE-START
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

    // SKIP-SCALATESTJS,NATIVE-START
    it("should be serializable") { status =>
      SharedHelpers.serializeRoundtrip(status)
    }

    it("waitUntilCompleted should throw unreportedException if set") { () =>
      val status = new StatefulStatus
      val e = new IllegalArgumentException("test")
      status.setFailedWith(e)
      status.setCompleted()
      val t = intercept[IllegalArgumentException] {
        status.waitUntilCompleted()
      }
      assert(e eq t)
    }
    it("succeeds should throw unreportedException if set") { () =>
      val status = new StatefulStatus
      val e = new IllegalArgumentException("test")
      status.setFailedWith(e)
      status.setCompleted()
      val t = intercept[IllegalArgumentException] {
        status.succeeds
      }
      assert(e eq t)
    }
    it("withAfterEffect should wrap suite-aborting exceptions in ExecutionException before setting it as unreportedException, and rethrow the original suite-aborting exception") { () =>
      val status = new StatefulStatus
      val e = new java.lang.annotation.AnnotationFormatError("test")
      val returnedStatus =
        status.withAfterEffect {
          throw e
        }
      val t = intercept[java.lang.annotation.AnnotationFormatError] {
        status.setCompleted() // StatefulStatus.setCompleted() executes the callbacks on the calling thread
      }
      assert(t eq e)
      assert(returnedStatus.unreportedException.isDefined)
      assert(returnedStatus.unreportedException.get.isInstanceOf[ExecutionException])
      assert(returnedStatus.unreportedException.get.getCause eq e)
    }
    it("thenRun should wrap suite-aborting exceptions in ExecutionException before setting it as unreportedException, and rethrow the original suite-aborting exception") { () =>
      val status = new StatefulStatus
      val e = new java.lang.annotation.AnnotationFormatError("test")
      val returnedStatus =
        status.thenRun {
          throw e
        }
      val t = intercept[java.lang.annotation.AnnotationFormatError] {
        status.setCompleted() // StatefulStatus.setCompleted() executes the callbacks on the calling thread
      }
      assert(t eq e)
      assert(returnedStatus.unreportedException.isDefined)
      assert(returnedStatus.unreportedException.get.isInstanceOf[ExecutionException])
      assert(returnedStatus.unreportedException.get.getCause eq e)
    }

    // SKIP-SCALATESTJS,NATIVE-END

    it("toFuture should return Future[Boolean] that will be complete later and has correct value of Option[Try[Boolean]]") { () =>
      val status1 = new StatefulStatus
      val future1 = status1.toFuture
      assert(status1.unreportedException == None)
      assert(!future1.isCompleted)
      assert(future1.value == None)
      status1.setCompleted()
      assert(status1.unreportedException == None)
      assert(future1.isCompleted)
      assert(future1.value == Some(Success(true)))

      val status2 = new StatefulStatus
      val future2 = status2.toFuture
      assert(status2.unreportedException == None)
      assert(!future2.isCompleted)
      assert(future2.value == None)
      status2.setFailed()
      status2.setCompleted()
      assert(status2.unreportedException == None)
      assert(future2.isCompleted)
      assert(future2.value == Some(Success(false)))

      val status3 = new StatefulStatus
      val future3 = status3.toFuture
      assert(status3.unreportedException == None)
      assert(!future3.isCompleted)
      assert(future3.value == None)
      status3.setFailed()
      val e = new IllegalArgumentException("test")
      status3.setFailedWith(e)
      status3.setCompleted()
      assert(status3.unreportedException == Some(e))
      assert(future3.isCompleted)
      assert(future3.value == Some(Failure(e)))
    }
  }

  describe("SucceededStatus ") {

    it("should invoke a function registered with whenCompleted, passing a succeeded value") { () =>

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

    it("should invoke multiple functions registered with whenCompleted, passing a succeeded value") { () =>
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

    it("toFuture should return Future[Boolean] that is completed and has value Some(Success(true))") { () =>
      val status = SucceededStatus
      assert(status.unreportedException == None)
      val future = status.toFuture
      assert(future.isCompleted)
      assert(future.value == Some(Success(true)))
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("thenRun should propagate a suite-aborting exception thrown in thenRun code") { () =>
      val status = SucceededStatus
      val e = new java.lang.annotation.AnnotationFormatError("test")
      val t = intercept[java.lang.annotation.AnnotationFormatError] {
        status.thenRun {
          throw e
        }
      }
      assert(t eq e)
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }

  describe("FailedStatus ") {

    it("should invoke a function registered with whenCompleted, passing a failed value") { () =>

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

    it("should invoke multiple functions registered with whenCompleted, passing a failed value") { () =>
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

    it("toFuture should return Future[Boolean] that is completed and has value Some(Success(false))") { () =>
      val status = FailedStatus
      assert(status.unreportedException == None)
      val future = status.toFuture
      assert(future.isCompleted)
      assert(future.value == Some(Success(false)))
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("thenRun should propagate a suite-aborting exception thrown in thenRun code") { () =>
      val status = FailedStatus
      val e = new java.lang.annotation.AnnotationFormatError("test")
      val t = intercept[java.lang.annotation.AnnotationFormatError] {
        status.thenRun {
          throw e
        }
      }
      assert(t eq e)
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }

  describe("CompositeStatus ") {
    it("should invoke multiple functions registered with whenCompleted, passing a succeeded value, only after all composed statuses complete successfully") { () =>

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
    it("should search its nested status for unreportedExceptions") { () =>
      val nestedStatus1 = new ScalaTestStatefulStatus
      val nestedStatus2 = new ScalaTestStatefulStatus
      val compoStatus = new CompositeStatus(Set(nestedStatus1, nestedStatus2))

      assert(compoStatus.unreportedException.isEmpty)

      val ex = new Exception("oops")
      nestedStatus1.setFailedWith(ex)

      assert(compoStatus.unreportedException.isDefined)
      assert(compoStatus.unreportedException.get eq ex)
    }
  }

  describe("ScalaTestStatefulStatus") {
    it("toFuture should return Future[Boolean] that will be complete later and has correct value of Option[Try[Boolean]]") { () =>
      val status1 = new ScalaTestStatefulStatus
      val future1 = status1.toFuture
      assert(status1.unreportedException == None)
      assert(!future1.isCompleted)
      assert(future1.value == None)
      status1.setCompleted()
      assert(status1.unreportedException == None)
      assert(future1.isCompleted)
      assert(future1.value == Some(Success(true)))

      val status2 = new ScalaTestStatefulStatus
      val future2 = status2.toFuture
      assert(status2.unreportedException == None)
      assert(!future2.isCompleted)
      assert(future2.value == None)
      status2.setFailed()
      status2.setCompleted()
      assert(status2.unreportedException == None)
      assert(future2.isCompleted)
      assert(future2.value == Some(Success(false)))

      val status3 = new ScalaTestStatefulStatus
      val future3 = status3.toFuture
      assert(status3.unreportedException == None)
      assert(!future3.isCompleted)
      assert(future3.value == None)
      status3.setFailed()
      val e = new IllegalArgumentException("test")
      status3.setFailedWith(e)
      status3.setCompleted()
      assert(status3.unreportedException == Some(e))
      assert(future3.isCompleted)
      assert(future3.value == Some(Failure(e)))
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("waitUntilCompleted should throw unreportedException if set") { () =>
      val status = new ScalaTestStatefulStatus
      val e = new IllegalArgumentException("test")
      status.setFailedWith(e)
      status.setCompleted()
      val t = intercept[IllegalArgumentException] {
        status.waitUntilCompleted()
      }
      assert(e eq t)
    }
    it("succeeds should throw unreportedException if set") { () =>
      val status = new ScalaTestStatefulStatus
      val e = new IllegalArgumentException("test")
      status.setFailedWith(e)
      status.setCompleted()
      val t = intercept[IllegalArgumentException] {
        status.succeeds
      }
      assert(e eq t)
    }
    it("withAfterEffect should wrap suite-aborting exceptions in ExecutionException before setting it as unreportedException, and rethrow the original suite-aborting exception") { () =>
      val status = new ScalaTestStatefulStatus
      val e = new java.lang.annotation.AnnotationFormatError("test")
      val returnedStatus =
        status.withAfterEffect {
          throw e
        }
      val t = intercept[java.lang.annotation.AnnotationFormatError] {
        status.setCompleted()
      }
      assert(t eq e)
      assert(returnedStatus.unreportedException.isDefined)
      assert(returnedStatus.unreportedException.get.isInstanceOf[ExecutionException])
      assert(returnedStatus.unreportedException.get.getCause eq e)
    }
    it("thenRun should wrap suite-aborting exceptions in ExecutionException before setting it as unreportedException, and rethrow the original suite-aborting exception") { () =>
      val status = new ScalaTestStatefulStatus
      val e = new java.lang.annotation.AnnotationFormatError("test")
      val returnedStatus =
        status.thenRun {
          throw e
        }
      val t = intercept[java.lang.annotation.AnnotationFormatError] {
        status.setCompleted()
      }
      assert(t eq e)
      assert(returnedStatus.unreportedException.isDefined)
      assert(returnedStatus.unreportedException.get.isInstanceOf[ExecutionException])
      assert(returnedStatus.unreportedException.get.getCause eq e)
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
}
