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
package org.scalatest.words

import org.scalatest._
import SharedHelpers.createTempDirectory
import java.io.File

class ExistWordSpec extends FunSpec with Matchers {
  
  describe("ExistWord ") {
    
    val existWord = new ExistWord
    
    describe("matcherFactory produces Matcher that") {
      
      val mtf = existWord.matcherFactory
      val mt = mtf.matcher[File]
      
      it("should have pretty toString") {
        mtf.toString should be ("exist")
        mt.toString should be ("exist")
      }
      
      val tempDir = createTempDirectory()
      val lhs = File.createTempFile("delete", "me", tempDir)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " does not exist"
        mr.negatedFailureMessage shouldBe lhs + " exists"
        mr.midSentenceFailureMessage shouldBe lhs + " does not exist"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " exists"
        mr.rawFailureMessage shouldBe "{0} does not exist"
        mr.rawNegatedFailureMessage shouldBe "{0} exists"
        mr.rawMidSentenceFailureMessage shouldBe "{0} does not exist"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} exists"
        mr.failureMessageArgs shouldBe Vector(lhs)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " exists"
        nmr.negatedFailureMessage shouldBe lhs + " does not exist"
        nmr.midSentenceFailureMessage shouldBe lhs + " exists"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " does not exist"
        nmr.rawFailureMessage shouldBe "{0} exists"
        nmr.rawNegatedFailureMessage shouldBe "{0} does not exist"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} exists"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} does not exist"
        nmr.failureMessageArgs shouldBe Vector(lhs)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs)

      }
    }
  }
  
}