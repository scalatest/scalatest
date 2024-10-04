/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.matchers.dsl

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class MatcherWordsSpec extends AnyFunSpec with MatcherWords {
  
  describe("MatcherWords ") {
    
    describe("equal(Any) method returns MatcherFactory1") {
      
      val mtf = equal ("tommy")
      val mt = mtf.matcher[String]
      
      it("should have pretty toString") {
        mtf.toString should be ("equal (\"tommy\")")
        mt.toString should be ("equal (\"tommy\")")
      }
      
      val mr = mt("tomy")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"tom[]y\" did not equal \"tom[m]y\""
        mr.negatedFailureMessage shouldBe "\"tomy\" equaled \"tommy\""
        mr.midSentenceFailureMessage shouldBe "\"tom[]y\" did not equal \"tom[m]y\""
        mr.midSentenceNegatedFailureMessage shouldBe "\"tomy\" equaled \"tommy\""
        mr.rawFailureMessage shouldBe "{0} did not equal {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} equaled {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not equal {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} equaled {1}"
        mr.failureMessageArgs shouldBe Vector("tomy", "tommy")
        mr.negatedFailureMessageArgs shouldBe Vector("tomy", "tommy")
        mr.midSentenceFailureMessageArgs shouldBe Vector("tomy", "tommy")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("tomy", "tommy")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"tomy\" equaled \"tommy\""
        nmr.negatedFailureMessage shouldBe "\"tom[]y\" did not equal \"tom[m]y\""
        nmr.midSentenceFailureMessage shouldBe "\"tomy\" equaled \"tommy\""
        nmr.midSentenceNegatedFailureMessage shouldBe "\"tom[]y\" did not equal \"tom[m]y\""
        nmr.rawFailureMessage shouldBe "{0} equaled {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not equal {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} equaled {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not equal {1}"
        nmr.failureMessageArgs shouldBe Vector("tomy", "tommy")
        nmr.negatedFailureMessageArgs shouldBe Vector("tomy", "tommy")
        nmr.midSentenceFailureMessageArgs shouldBe Vector("tomy", "tommy")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("tomy", "tommy")

      }
      
    }
  }
  
}
