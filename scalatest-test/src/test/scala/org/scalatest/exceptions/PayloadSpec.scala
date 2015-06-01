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
package exceptions

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.SharedHelpers._
import time.{Span, Second}
// SKIP-SCALATESTJS-START
import junit.JUnitTestFailedError
// SKIP-SCALATESTJS-END

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class PayloadSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks with Payloads with SeveredStackTraces {

  def examples =  // TODO, also support payloads in JUnit errors
    Table(
      "exception",
      new TestFailedException("message", 3),
      // SKIP-SCALATESTJS-START
      new JUnitTestFailedError("message", 3),
      // SKIP-SCALATESTJS-END
      new TestFailedDueToTimeoutException(e => Some("message"), None, e => 3, None, Span(1, Second)),
      new TableDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, List.empty, 3),
      new GeneratorDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, Option(List.empty), List.empty), 
      new TestCanceledException("message", 3)
   )

  "The modifyPayload method on TFE" should "return the an exception with an equal message option if passed a function that returns the same option passed to it" in {
    forAll (examples) { e =>
      e.modifyPayload(opt => opt) should equal (e)
    }
  }

  it should "return the new exception with the replaced payload" in {
    forAll (examples) { e =>
      e.modifyPayload(opt => Some("a payload")).payload.get should be ("a payload")
    }
  }

  "The withPayload construct" should "allow any non-ModifiablePayload exception to pass through" in {
    val iae = new IllegalArgumentException
    val caught = intercept[IllegalArgumentException] {
      withPayload("howdy") {
        throw iae 
      }
    }
    caught should be theSameInstanceAs (iae)
  }

  it should "given a null payload, rethrow the same ModifiablePayload exception" in {
    forAll (examples) { e =>
      val caught = intercept[PayloadField] {
        withPayload(null) {
          throw e 
        }
      }
      caught should be theSameInstanceAs (e)
    }
  }
  
  it should "given a payload, should throw a new ModifiablePayload of the same class with the given payload" in {
    forAll (examples) { e =>
      val caught = intercept[PayloadField] {
        withPayload("a payload") {
          throw e 
        }
      }
      caught should not be theSameInstanceAs (e)
      caught.payload should be (Some("a payload"))
      caught.getClass should be theSameInstanceAs (e.getClass)
    }
  }

  it should "forward the payload to be carried in TestFailed event" in {
    forAll (examples) { e =>
      val a = 
        new FunSpec {
          it("should do something") {
            withPayload("a payload") {
              throw e
            }
          }
        }
      val rep = new EventRecordingReporter()
      a.run(None, Args(rep))
      e match {
        case tce: TestCanceledException => 
          rep.testCanceledEventsReceived.length should be (1)
          rep.testCanceledEventsReceived(0).payload should be (Some("a payload"))
        case _ => 
          rep.testFailedEventsReceived.length should be (1)
          rep.testFailedEventsReceived(0).payload should be (Some("a payload"))
      }
    }
  }
  
  it should "infer the type of the result of the passed in function" in {
    val result: Int = withPayload("hi") { 22 }
    assert(result === 22)
  }
  
  it should "be able to accept by-name payload" in {
    val result: String = withPayload(() => 128) { "hello" }
    assert(result === "hello")
  }
  
  it should "work with withFixture" in {
    forAll(examples) { e => 
      val a = 
        new org.scalatest.fixture.FunSpec {
          type FixtureParam = String
        
          override def withFixture(test: OneArgTest) = {
            withPayload("a payload") {
              test("something")
            }
          }
        
          it("should do something") { p => 
            throw e
          }
        }
      val rep = new EventRecordingReporter()
      a.run(None, Args(rep))
      
      e match {
        case tce: TestCanceledException => 
          rep.testCanceledEventsReceived.length should be (1)
          rep.testCanceledEventsReceived(0).payload should be (Some("a payload"))
        case _ => 
          rep.testFailedEventsReceived.length should be (1)
          rep.testFailedEventsReceived(0).payload should be (Some("a payload"))
      }
    }
  }
  
  it should "return Failed that contains TestFailedException and added payload" in {
    val failed = Failed(new TestFailedException("boom!", 3))
    val result = withPayload("a payload") { failed }
    result shouldBe a [Failed]
    result.exception shouldBe a [TestFailedException]
    result.exception.asInstanceOf[TestFailedException].payload shouldBe Some("a payload")
  }
  
  it should "return original Failed that contains the RuntimeException and without payload" in {
    val failed = Failed(new RuntimeException("boom!"))
    val result = withPayload("a payload") { failed }
    result should be theSameInstanceAs failed
    result.exception.getMessage shouldBe "boom!"
  }
  
  it should "return Canceled that contains TestCanceledException and added payload" in {
    val canceled = Canceled(new TestCanceledException("rollback!", 3))
    val result = withPayload("a payload") { canceled }
    result shouldBe a [Canceled]
    result.exception shouldBe a [TestCanceledException]
    result.exception.asInstanceOf[TestCanceledException].payload shouldBe Some("a payload")
  }
  
  it should "return original Canceled that contains the RuntimeException and without payload" in {
    val canceled = Canceled(new RuntimeException("boom!"))
    val result = withPayload("a payload") { canceled }
    result shouldBe a [Canceled]
    result.exception shouldBe a [TestCanceledException]
    result.exception.asInstanceOf[TestCanceledException].payload shouldBe Some("a payload")
    result.exception.asInstanceOf[TestCanceledException].getCause.getMessage shouldBe "boom!"
  }
  
  it should "return original Pending" in {
    val pending = Pending
    val result = withPayload("a payload") { pending }
    result should be theSameInstanceAs pending
  }
  
  it should "return original Succeeded" in {
    val succeeded = Succeeded
    val result = withPayload("a payload") { succeeded }
    result should be theSameInstanceAs succeeded
  }
}
