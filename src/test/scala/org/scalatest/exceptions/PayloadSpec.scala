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
package org.scalatest
package exceptions

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import time.{Span, Second}
import junit.JUnitTestFailedError

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class PayloadSpec extends FlatSpec with SharedHelpers with ShouldMatchers with TableDrivenPropertyChecks with Payloads with SeveredStackTraces {

  def examples =  // TODO, also support payloads in JUnit errors
    Table(
      "exception",
      new TestFailedException("message", 3),
      new JUnitTestFailedError("message", 3),
      new TestFailedDueToTimeoutException(e => Some("message"), None, e => 3, None, Span(1, Second)),
      new TableDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, List.empty, 3),
      new GeneratorDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, Option(List.empty), List.empty)
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
      rep.testFailedEventsReceived.length should be (1)
      rep.testFailedEventsReceived(0).payload should be (Some("a payload"))
    }
  }
}
