/*
 * Copyright 2001-2012 Artima, Inc.
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
import SharedHelpers.thisLineNumber
import time.{Span, Millis}
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestFailedException

class AsyncAssertionsSpec extends fixture.FunSpec with ShouldMatchers with SharedHelpers with ConductorFixture with
    OptionValues with AsyncAssertions {
/*
  def withCause(cause: Throwable)(fun: => Unit) {
    try {
      fun  
    }
    catch {
      case e: TestFailedException =>
        def setCause(oldEx: Throwable) {
          if (oldEx.getCause == null)
            oldEx.initCause(cause)  
          else
            setCause(oldEx.getCause)
        }
        try {
          setCause(e)
        }
        catch {
          // Swallow IAEs and ISEs. IAE can occur if they try to set an exception's cause to itself
          // The ISE can happen if initCause was already invoked on an exception. In either case, withCause
          // will just fail.
          case iae: IllegalArgumentException => println("Got an IAE")
          case ise: IllegalStateException => println("Got an ISE")
        }
        throw e // Rethrow the same exception with its cause augmented
    }
  }
  */
  describe("A Waiter") {

    it("should throw a NotAllowedException if await is called by a thread different from the" +
    		"thread that constructed the Waiter") { con => import con._
      val w = new Waiter
      thread {
        val caught =
          intercept[NotAllowedException] {
  	        w.await()
          }
        caught.message.value should be (Resources("awaitMustBeCalledOnCreatingThread"))
        if (caught.failedCodeLineNumber.value != thisLineNumber - 3)
          fail("stack depth was " + caught.failedCodeLineNumber.value + " but expected " + (thisLineNumber - 4), caught)
        //caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      }
      con.conduct()
    }
 
    it("should continue when dismissed.") { con => import con._
      @volatile var w: Waiter = null
      thread {
        w = new Waiter
        w.await()
      }
      thread {
        waitForBeat(1)
        w.dismiss()
      }
      con.conduct()
    }

    it("should receive a complaint sent from a different thread sent during an await") { con => import con._
      @volatile var w: Waiter = null
      thread {
        w = new Waiter
        val caught =
          intercept[TestFailedException] {
            w.await()
          }
        caught.message.value should be ("I meant to do that!")
      }
      thread {
        waitForBeat(1)
        w { fail("I meant to do that!") }
        w.dismiss()
      }
      con.conduct()
    }
    
    it("should pass through even exceptions that would not normally cause a test to fail") { con => import con._
      @volatile var w: Waiter = null
      thread {
        w = new Waiter
        val caught =
          intercept[OutOfMemoryError] {
            w.await()
          }
        caught.getMessage() should be ("Sorry, I forgot.")
      }
      thread {
        waitForBeat(1)
        w { throw new OutOfMemoryError("Sorry, I forgot.") }
        w.dismiss()
      }
      con.conduct()
    }
  
    it("should wait for multiple dismissals when requested") { con => import con._
      @volatile var w: Waiter = null
      @volatile var doneWaiting = false
      @volatile var awaitReturnedPrematurely = false
      thread {
        w = new Waiter
        w.await(dismissals(3))
        doneWaiting = true
      }
      thread {
        waitForBeat(1)
        if (doneWaiting)
          awaitReturnedPrematurely = true
        w.dismiss()
      }
      thread {
        waitForBeat(2)
        if (doneWaiting)
          awaitReturnedPrematurely = true
        w.dismiss()
      }
      thread {
        waitForBeat(3)
        if (doneWaiting)
          awaitReturnedPrematurely = true
        w.dismiss()
      }
      whenFinished {
        awaitReturnedPrematurely should be (false)
      }
    }

    it("should throw a TFE from await if there's a timeout") { con => import con._
      val w = new Waiter
      val caught =
        intercept[TestFailedException] {
          w.await(timeout(Span(10, Millis)))
        }
      caught.message.value should be (Resources("awaitTimedOut"))
      if (caught.failedCodeLineNumber.value != thisLineNumber - 3)
        fail("stack depth was " + caught.failedCodeLineNumber.value + " but expected " + (thisLineNumber - 4), caught)
    }

    it("should still return the thrown exception if thrown before await is called") { con => import con._
      @volatile var w: Waiter = null
      thread {
        w = new Waiter
        waitForBeat(2)
        val caught =
          intercept[TestFailedException] {
            w.await()
          }
        caught.message.value should be ("I meant to do that!")
      }
      thread {
        waitForBeat(1)
        w { fail("I meant to do that!") }
        w.dismiss()
      }
      con.conduct()
    }
  
    it("should still complete normally if await without a timeout is called after dismiss is called.") { con => import con._
      @volatile var w: Waiter = null
      thread {
        w = new Waiter
        waitForBeat(2)
        w.await()
      }
      thread {
        waitForBeat(1)
        w.dismiss()
      }
      con.conduct()
    }

    it("should report just the first exception thrown at it") { con => import con._

      @volatile var w: Waiter = null
      thread {
        w = new Waiter
        waitForBeat(3)
        val caught =
          intercept[TestFailedException] {
            w.await()
          }
        caught.message.value should be ("I meant to do that!")
      }
      thread {
        waitForBeat(1)
        w { fail("I meant to do that!") }
        w.dismiss()
      }
      thread {
        waitForBeat(2)
        w { throw new IllegalArgumentException }
        w.dismiss()
      }
      con.conduct()
    }
    
    it("should await to be called multiple times") { con => import con._

      @volatile var w: Waiter = null

      thread {
        waitForBeat(1)
        w.dismiss()
        waitForBeat(3)
        w { fail("I meant to do that!") }
      }

      thread {
        w = new Waiter
        w.await()
        waitForBeat(2)
        val caught =
          intercept[TestFailedException] {
            w.await()
          }
        caught.message.value should be ("I meant to do that!")
      }

      con.conduct()
    }
  }
}
