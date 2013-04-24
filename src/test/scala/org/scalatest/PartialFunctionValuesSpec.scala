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

import org.scalatest.OptionValues._
import org.scalatest.PartialFunctionValues._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.thisLineNumber

class PartialFunctionValuesSpec extends FunSpec with ShouldMatchers {
  
  val pf = new PartialFunction[Int, Int]() {
    def isDefinedAt(x: Int): Boolean = x % 2 == 0
    def apply(x: Int): Int = x * x
  }
  
  describe("values on PartialFunction") {
    
    it("should return correct value when is defined") {
      pf.isDefinedAt(8) should be === (true)
      pf.valueAt(8) should be === (64)
    }
    
    it("should throw TestFailedException when is not defined") {
      val caught = 
        evaluating {
          pf.valueAt(5) should be === (25)
        } should produce [TestFailedException]
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("PartialFunctionValuesSpec.scala")
      caught.message.value should be (Resources("partialFunctionValueNotDefined", "5"))
    }
    
  }
}
