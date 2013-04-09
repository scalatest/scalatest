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
import Eventually._
import matchers.ShouldMatchers
import SharedHelpers.thisLineNumber
import time.{Millisecond, Span, Millis}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
import org.scalatest.exceptions.TestFailedDueToTimeoutException

class EventuallySpec extends FunSpec with ShouldMatchers with OptionValues with SeveredStackTraces {

  describe("The eventually construct") {

    it("should just return if the by-name returns normally") {

      eventually { 1 + 1 should equal (2) }
    }

    it("should invoke the function just once if the by-name returns normally the first time") {

      var count = 0
      eventually {
        count += 1
        1 + 1 should equal (2)
      }
      count should equal (1)
    }

    it("should invoke the function just once and return the result if the by-name returns normally the first time") {

      var count = 0
      val result =
        eventually {
          count += 1
          99
        }
      count should equal (1)
      result should equal (99)
    }

    it("should invoke the function five times if the by-name throws an exception four times before finally returning normally the fifth time") {

      var count = 0
      eventually {
        count += 1
        if (count < 5) throw new Exception
        1 + 1 should equal (2)
      }
      count should equal (5)
    }

    it("should eventually blow up with a TestFailedDueToTimeoutException if the by-name continuously throws an exception") {

      var count = 0
      val caught = evaluating {
        eventually {
          count += 1
          throw new RuntimeException
          ()
        }
      } should produce [TestFailedDueToTimeoutException]

      caught.message.value should include ("Attempted " + count.toString + " times")
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 8)
      caught.failedCodeFileName.value should be ("EventuallySpec.scala")
      caught.timeout should be (Span(150, Millis))
    }

    it("should eventually blow up with a TFE if the by-name continuously throws an exception, and include the last failure message in the TFE message") {

      var count = 0
      val caught = evaluating {
        eventually {
          count += 1
          1 + 1 should equal (3)
        }
      } should produce [TestFailedException]

      caught.message.value should include ("Attempted " + count.toString + " times")
      caught.message.value should include ("2 did not equal 3")
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 8)
      caught.failedCodeFileName.value should be ("EventuallySpec.scala")
      caught.getCause.getClass.getName should be ("org.scalatest.exceptions.TestFailedException")
      caught.getCause.getMessage should be ("2 did not equal 3")
    }
    
    it("should provides correct stack depth when eventually is called from the overload method") {
      
      val caught1 = evaluating {
        eventually(timeout(Span(100, Millis)), interval(Span(1, Millisecond))) { 1 + 1 should equal (3) }
      } should produce [TestFailedException]
      caught1.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught1.failedCodeFileName.value should be ("EventuallySpec.scala")
      
      val caught3 = evaluating {
        eventually(timeout(Span(100, Millis))) { 1 + 1 should equal (3) }
      } should produce [TestFailedException]
      caught3.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught3.failedCodeFileName.value should be ("EventuallySpec.scala")
      
      val caught4 = evaluating {
        eventually(interval(Span(1, Millisecond))) { 1 + 1 should equal (3) }
      } should produce [TestFailedException]
      caught4.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught4.failedCodeFileName.value should be ("EventuallySpec.scala")
    }

    it("should by default invoke an always-failing by-name for at least 150 millis") {
      var startTime: Option[Long] = None
      evaluating {
        startTime = Some(System.currentTimeMillis)
        eventually {
          1 + 1 should equal (3)
        }
      } should produce [TestFailedException]
      (System.currentTimeMillis - startTime.get).toInt should be >= (150)
    }

    it("should, if an alternate implicit Timeout is provided, invoke an always-failing by-name by at least the specified timeout") {

      implicit val patienceConfig = PatienceConfig(timeout = Span(1500, Millis))

      var startTime: Option[Long] = None
      evaluating {
        eventually {
          if (startTime.isEmpty)
            startTime = Some(System.currentTimeMillis)
          1 + 1 should equal (3)
        }
      } should produce [TestFailedException]
      (System.currentTimeMillis - startTime.get).toInt should be >= (1500)
    }

    it("should, if an alternate explicit timeout is provided, invoke an always-failing by-name by at least the specified timeout") {

      var startTime: Option[Long] = None
      evaluating {
        eventually (timeout(Span(1250, Millis))) {
          if (startTime.isEmpty)
            startTime = Some(System.currentTimeMillis)
          1 + 1 should equal (3)
        } 
      } should produce [TestFailedException]
      (System.currentTimeMillis - startTime.get).toInt should be >= (1250)
    }

    it("should, if an alternate explicit timeout is provided along with an explicit interval, invoke an always-failing by-name by at least the specified timeout, even if a different implicit is provided") {

      implicit val patienceConfig = PatienceConfig(timeout = Span(500, Millis), interval = Span(2, Millis))
      
      var startTime: Option[Long] = None
      evaluating {
        eventually (timeout(Span(1388, Millis)), interval(Span(1, Millisecond))) {
          if (startTime.isEmpty)
            startTime = Some(System.currentTimeMillis)
          1 + 1 should equal (3)
        } 
      } should produce [TestFailedException]
      (System.currentTimeMillis - startTime.get).toInt should be >= (1388)
    }
    
    it("should allow errors that do not normally cause a test to fail through immediately when thrown") {

      var count = 0
      intercept[VirtualMachineError] {
        eventually {
          count += 1
          throw new VirtualMachineError {}
          1 + 1 should equal (3)
        }
      }
      count should equal (1)
    }
    
    it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {

      var count = 0
      intercept[TestPendingException] {
        eventually {
          count += 1
          pending
        }
      }
      count should equal (1)
    }
    
    it("should, when reach before first interval, wake up every 1/10 of the interval.") {
      var count = 0
      var startTime: Option[Long] = None
      evaluating {
        eventually(timeout(Span(1000, Millis)), interval(Span(100, Millis))) {
          if (startTime.isEmpty) {
            startTime = Some(System.nanoTime)
            count += 1
          }
          else {
            val durationMillis = (System.nanoTime - startTime.get) / 1000000
            if (durationMillis < 100)
              count += 1
          }
          1 + 1 should equal (3)
        }
      } should produce [TestFailedException]
      count should be > (1)
    }
  }
}

