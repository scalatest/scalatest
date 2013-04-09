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
package org.scalatest

import org.scalatest.Inside._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.thisLineNumber
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class InsideSpec extends FunSpec with ShouldMatchers with OptionValues {

  case class Address(street: String, city: String, state: String, zip: String)
  case class Name(first: String, middle: String, last: String)
  case class Record(name: Name, address: Address, age: Int)

  describe("The inside construct") {

    val rec = Record(
      Name("Sally", "Mary", "Jones"),
      Address("123 Main St", "Bluesville", "KY", "12345"),
      29
    )

    it("should return normally when nested properties are inspected with matcher expressions that all succeed") {
      inside (rec) { case Record(name, address, age) =>
        inside (name) { case Name(first, middle, last) =>
          first should be ("Sally")
          middle should startWith ("Ma")
          last should endWith ("nes")
        }
        inside (address) { case Address(street, city, state, zip) =>
          street should be ("123 Main St")
          city should be ("Bluesville")
          state.toLowerCase should be ("ky")
          zip should be ("12345")
        }
        age should be >= 21
      }
    }

    it("should throw a TFE when the partial function isn't defined at the passed value") {
      val caught = evaluating {
        inside (rec) { case Record(name, _, 99) =>
          name.first should be ("Sally")
        }
      } should produce [TestFailedException]
      caught.message.value should be (Resources("insidePartialFunctionNotDefined", rec.toString))
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 5)
      caught.failedCodeFileName.value should be ("InsideSpec.scala")
    }

    it("should include an inside clause when a matcher fails inside") {
      val caught = evaluating {
        inside (rec) { case Record(_, _, age) =>
          age should be <= 21
        }
      } should produce [TestFailedException]
      caught.message.value should be (Resources("insidePartialFunctionAppendSomeMsg", Resources("wasNotLessThanOrEqualTo", "29", "21"), rec.toString))
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
      caught.failedCodeFileName.value should be ("InsideSpec.scala")
    }

    it("should include a nested inside clause when a matcher fails inside a nested inside") {
      val caught = evaluating {
        inside (rec) { case Record(name, _, _) =>
          inside (name) { case Name(first, _, _) =>
            first should be ("Harry")
          }
        }
      } should produce [TestFailedException]
      caught.message.value should be (Resources("insidePartialFunctionAppendSomeMsg", Resources("insidePartialFunctionAppendSomeMsg", Resources("wasNotEqualTo", "\"[Sall]y\"", "\"[Harr]y\""), rec.name.toString), rec.toString))
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 5)
      caught.failedCodeFileName.value should be ("InsideSpec.scala")
    }
    
    it("should throw a TFE when matcher fails inside due to exception") {
      val caught = evaluating {
        inside (rec) { case Record(name, address, age) =>
          throw new TestFailedException(None, None, 0)
        }
      } should produce [TestFailedException]
      caught.message.value should be (Resources("insidePartialFunctionAppendNone", rec))
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 4)
      caught.failedCodeFileName.value should be ("InsideSpec.scala")
    }
  }
}

