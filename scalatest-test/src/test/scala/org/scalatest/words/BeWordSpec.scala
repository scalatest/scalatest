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
import org.scalatest.exceptions.NotAllowedException
import Matchers._
import matchers.{BePropertyMatcher, 
                 BePropertyMatchResult, 
                 AMatcher, 
                 AnMatcher, 
                 BeMatcher, 
                 MatchResult}

class BeWordSpec extends FunSpec with FileMocks {
  
  describe("BeWord ") {
    
    it("should have pretty toString") {
      be.toString should be ("be")
    }
    
    describe("< method returns Matcher") {
      
      val mt = be < 3
      
      it("should have pretty toString") {
        mt.toString should be ("be < 3")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "0 was not less than 3"
        mr.negatedFailureMessage shouldBe "0 was less than 3"
        mr.midSentenceFailureMessage shouldBe "0 was not less than 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was less than 3"
        mr.rawFailureMessage shouldBe "{0} was not less than {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was less than {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not less than {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was less than {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "0 was less than 3"
        nmr.negatedFailureMessage shouldBe "0 was not less than 3"
        nmr.midSentenceFailureMessage shouldBe "0 was less than 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was not less than 3"
        nmr.rawFailureMessage shouldBe "{0} was less than {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not less than {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was less than {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not less than {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe("> method returns Matcher") {
      
      val mt = be > 3
      
      it("should have pretty toString") {
        mt.toString should be ("be > 3")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "0 was not greater than 3"
        mr.negatedFailureMessage shouldBe "0 was greater than 3"
        mr.midSentenceFailureMessage shouldBe "0 was not greater than 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was greater than 3"
        mr.rawFailureMessage shouldBe "{0} was not greater than {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was greater than {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not greater than {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was greater than {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "0 was greater than 3"
        nmr.negatedFailureMessage shouldBe "0 was not greater than 3"
        nmr.midSentenceFailureMessage shouldBe "0 was greater than 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was not greater than 3"
        nmr.rawFailureMessage shouldBe "{0} was greater than {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not greater than {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was greater than {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not greater than {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe("<= method returns Matcher") {
      
      val mt = be <= 3
      
      it("should have pretty toString") {
        mt.toString should be ("be <= 3")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "0 was not less than or equal to 3"
        mr.negatedFailureMessage shouldBe "0 was less than or equal to 3"
        mr.midSentenceFailureMessage shouldBe "0 was not less than or equal to 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was less than or equal to 3"
        mr.rawFailureMessage shouldBe "{0} was not less than or equal to {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was less than or equal to {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not less than or equal to {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was less than or equal to {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "0 was less than or equal to 3"
        nmr.negatedFailureMessage shouldBe "0 was not less than or equal to 3"
        nmr.midSentenceFailureMessage shouldBe "0 was less than or equal to 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was not less than or equal to 3"
        nmr.rawFailureMessage shouldBe "{0} was less than or equal to {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not less than or equal to {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was less than or equal to {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not less than or equal to {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe(">= method returns Matcher") {
      
      val mt = be >= 3
      
      it("should have pretty toString") {
        mt.toString should be ("be >= 3")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "0 was not greater than or equal to 3"
        mr.negatedFailureMessage shouldBe "0 was greater than or equal to 3"
        mr.midSentenceFailureMessage shouldBe "0 was not greater than or equal to 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was greater than or equal to 3"
        mr.rawFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was greater than or equal to {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was greater than or equal to {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "0 was greater than or equal to 3"
        nmr.negatedFailureMessage shouldBe "0 was not greater than or equal to 3"
        nmr.midSentenceFailureMessage shouldBe "0 was greater than or equal to 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was not greater than or equal to 3"
        nmr.rawFailureMessage shouldBe "{0} was greater than or equal to {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was greater than or equal to {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe("=== method fails") {
      intercept[NotAllowedException] { val mt = be === "cheese" }
    }

    // SKIP-SCALATESTJS-START
    describe("a(Symbol) method returns Matcher") {
      val mt = be a ('file)
      
      it("should have pretty toString") {
        mt.toString should be ("be a 'file")
      }
      
      val mr = mt(fileMock)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe fileMock + " was not a file"
        mr.negatedFailureMessage shouldBe fileMock + " was a file"
        mr.midSentenceFailureMessage shouldBe fileMock + " was not a file"
        mr.midSentenceNegatedFailureMessage shouldBe fileMock + " was a file"
        mr.rawFailureMessage shouldBe "{0} was not a {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was a {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not a {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was a {1}"
        mr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe fileMock + " was a file"
        nmr.negatedFailureMessage shouldBe fileMock + " was not a file"
        nmr.midSentenceFailureMessage shouldBe fileMock + " was a file"
        nmr.midSentenceNegatedFailureMessage shouldBe fileMock + " was not a file"
        nmr.rawFailureMessage shouldBe "{0} was a {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was a {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not a {1}"
        nmr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
    }
    // SKIP-SCALATESTJS-END
    
    describe("a(BePropertyMatcher) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = be a (file)
      
      it("should have pretty toString") {
        mt.toString should be ("be a " + file)
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not a file"
        mr.negatedFailureMessage shouldBe myFile + " was a file"
        mr.midSentenceFailureMessage shouldBe myFile + " was not a file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was a file"
        mr.rawFailureMessage shouldBe "{0} was not a {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was a {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not a {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was a {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was a file"
        nmr.negatedFailureMessage shouldBe myFile + " was not a file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was a file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not a file"
        nmr.rawFailureMessage shouldBe "{0} was a {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was a {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not a {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }
    
    describe("a(AMatcher) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = be a (file)
      
      it("should have pretty toString") {
        mt.toString should be ("be a AMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not a file"
        mr.negatedFailureMessage shouldBe myFile + " was a file"
        mr.midSentenceFailureMessage shouldBe myFile + " was not a file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was a file"
        mr.rawFailureMessage shouldBe "{0} was not a {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was a {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not a {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was a {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was a file"
        nmr.negatedFailureMessage shouldBe myFile + " was not a file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was a file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not a file"
        nmr.rawFailureMessage shouldBe "{0} was a {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was a {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not a {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }

    // SKIP-SCALATESTJS-START
    describe("an(Symbol) method returns Matcher") {
      val mt = be an ('file)
      
      it("should have pretty toString") {
        mt.toString should be ("be an 'file")
      }
      
      val mr = mt(fileMock)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe fileMock + " was not an file"
        mr.negatedFailureMessage shouldBe fileMock + " was an file"
        mr.midSentenceFailureMessage shouldBe fileMock + " was not an file"
        mr.midSentenceNegatedFailureMessage shouldBe fileMock + " was an file"
        mr.rawFailureMessage shouldBe "{0} was not an {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was an {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not an {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an {1}"
        mr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe fileMock + " was an file"
        nmr.negatedFailureMessage shouldBe fileMock + " was not an file"
        nmr.midSentenceFailureMessage shouldBe fileMock + " was an file"
        nmr.midSentenceNegatedFailureMessage shouldBe fileMock + " was not an file"
        nmr.rawFailureMessage shouldBe "{0} was an {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was an {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an {1}"
        nmr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
    }
    // SKIP-SCALATESTJS-END
    
    describe("an(BePropertyMatcher) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = be an (file)
      
      it("should have pretty toString") {
        mt.toString should be ("be an " + file)
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not an file"
        mr.negatedFailureMessage shouldBe myFile + " was an file"
        mr.midSentenceFailureMessage shouldBe myFile + " was not an file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was an file"
        mr.rawFailureMessage shouldBe "{0} was not an {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was an {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not an {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was an file"
        nmr.negatedFailureMessage shouldBe myFile + " was not an file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was an file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an file"
        nmr.rawFailureMessage shouldBe "{0} was an {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was an {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }
    
    describe("an(AnMatcher) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AnMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = be an (file)
      
      it("should have pretty toString") {
        mt.toString should be ("be an AnMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not an file"
        mr.negatedFailureMessage shouldBe myFile + " was an file"
        mr.midSentenceFailureMessage shouldBe myFile + " was not an file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was an file"
        mr.rawFailureMessage shouldBe "{0} was not an {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was an {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not an {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was an file"
        nmr.negatedFailureMessage shouldBe myFile + " was not an file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was an file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an file"
        nmr.rawFailureMessage shouldBe "{0} was an {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was an {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }
    
    describe("apply(Spread) method returns Matcher") {
      val spread = 7.1 +- 0.2
      val mt = be (spread)
      
      it("should have pretty toString") {
        mt.toString should be ("be (7.1 +- 0.2)")
      }
      
      val mr = mt(7.0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        mr.negatedFailureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        mr.midSentenceFailureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        mr.midSentenceNegatedFailureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        mr.rawFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        mr.failureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        mr.negatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        mr.midSentenceFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        nmr.negatedFailureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        nmr.midSentenceFailureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        nmr.midSentenceNegatedFailureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        nmr.rawFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        nmr.failureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        nmr.negatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)

      }
    }
    
    describe("theSameInstanceAs(AnyRef) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val myFileLeft = MyFile("left", true, false)
      val myFileRight = MyFile("right", true, false)
      
      val mt = be theSameInstanceAs (myFileRight)
      
      it("should have pretty toString") {
        mt.toString should be ("be theSameInstanceAs " + myFileRight)
      }
      
      val mr = mt(myFileLeft)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        mr.negatedFailureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        mr.midSentenceFailureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        mr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        mr.rawFailureMessage shouldBe "{0} was not the same instance as {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was the same instance as {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not the same instance as {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was the same instance as {1}"
        mr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        nmr.negatedFailureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        nmr.midSentenceFailureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        nmr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        nmr.rawFailureMessage shouldBe "{0} was the same instance as {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not the same instance as {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was the same instance as {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not the same instance as {1}"
        nmr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

      }
    }
    
    describe("apply(Boolean) method returns Matcher") {
      val mt = be (true)
      
      it("should have pretty toString") {
        mt.toString should be ("be (true)")
      }
      
      val mr = mt(true)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "true was not true"
        mr.negatedFailureMessage shouldBe "true was true"
        mr.midSentenceFailureMessage shouldBe "true was not true"
        mr.midSentenceNegatedFailureMessage shouldBe "true was true"
        mr.rawFailureMessage shouldBe "{0} was not {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was {1}"
        mr.failureMessageArgs shouldBe Vector(true, true)
        mr.negatedFailureMessageArgs shouldBe Vector(true, true)
        mr.midSentenceFailureMessageArgs shouldBe Vector(true, true)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(true, true)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "true was true"
        nmr.negatedFailureMessage shouldBe "true was not true"
        nmr.midSentenceFailureMessage shouldBe "true was true"
        nmr.midSentenceNegatedFailureMessage shouldBe "true was not true"
        nmr.rawFailureMessage shouldBe "{0} was {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not {1}"
        nmr.failureMessageArgs shouldBe Vector(true, true)
        nmr.negatedFailureMessageArgs shouldBe Vector(true, true)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(true, true)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(true, true)

      }
    }
    
    describe("apply(Null) method returns Matcher") {
      val mt = be (null)
      
      it("should have pretty toString") {
        mt.toString should be ("be (null)")
      }
      
      val aString = "something"
      
      val mr = mt(aString)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"something\" was not null"
        mr.negatedFailureMessage shouldBe "The reference was null"
        mr.midSentenceFailureMessage shouldBe "\"something\" was not null"
        mr.midSentenceNegatedFailureMessage shouldBe "the reference was null"
        mr.rawFailureMessage shouldBe "{0} was not null"
        mr.rawNegatedFailureMessage shouldBe "The reference was null"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not null"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "the reference was null"
        mr.failureMessageArgs shouldBe Vector(aString)
        mr.negatedFailureMessageArgs shouldBe Vector.empty
        mr.midSentenceFailureMessageArgs shouldBe Vector(aString)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector.empty

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "The reference was null"
        nmr.negatedFailureMessage shouldBe "\"something\" was not null"
        nmr.midSentenceFailureMessage shouldBe "the reference was null"
        nmr.midSentenceNegatedFailureMessage shouldBe "\"something\" was not null"
        nmr.rawFailureMessage shouldBe "The reference was null"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not null"
        nmr.rawMidSentenceFailureMessage shouldBe "the reference was null"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not null"
        nmr.failureMessageArgs shouldBe Vector.empty
        nmr.negatedFailureMessageArgs shouldBe Vector(aString)
        nmr.midSentenceFailureMessageArgs shouldBe Vector.empty
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(aString)

      }
    }

    // SKIP-SCALATESTJS-START
    describe("apply(Symbol) method returns Matcher") {
      val mt = be ('file)
      
      it("should have pretty toString") {
        mt.toString should be ("be ('file)")
      }
      
      val mr = mt(fileMock)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe fileMock + " was not file"
        mr.negatedFailureMessage shouldBe fileMock + " was file"
        mr.midSentenceFailureMessage shouldBe fileMock + " was not file"
        mr.midSentenceNegatedFailureMessage shouldBe fileMock + " was file"
        mr.rawFailureMessage shouldBe "{0} was not {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was {1}"
        mr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe fileMock + " was file"
        nmr.negatedFailureMessage shouldBe fileMock + " was not file"
        nmr.midSentenceFailureMessage shouldBe fileMock + " was file"
        nmr.midSentenceNegatedFailureMessage shouldBe fileMock + " was not file"
        nmr.rawFailureMessage shouldBe "{0} was {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not {1}"
        nmr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
    }
    // SKIP-SCALATESTJS-END
    
    describe("apply(BeMatcher) method returns Matcher") {
      class OddMatcher extends BeMatcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            left % 2 == 1,
            left.toString + " was even",
            left.toString + " was odd", 
            Vector(left)
          )
        }
      }
      val odd = new OddMatcher
      
      val mt = be (odd)
      
      it("should have pretty toString") {
        mt.toString should be ("be (" + odd + ")")
      }
      
      val mr = mt(1)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "1 was even"
        mr.negatedFailureMessage shouldBe "1 was odd"
        mr.midSentenceFailureMessage shouldBe "1 was even"
        mr.midSentenceNegatedFailureMessage shouldBe "1 was odd"
        mr.rawFailureMessage shouldBe "1 was even"
        mr.rawNegatedFailureMessage shouldBe "1 was odd"
        mr.rawMidSentenceFailureMessage shouldBe "1 was even"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "1 was odd"
        mr.failureMessageArgs shouldBe Vector(1)
        mr.negatedFailureMessageArgs shouldBe Vector(1)
        mr.midSentenceFailureMessageArgs shouldBe Vector(1)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(1)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "1 was odd"
        nmr.negatedFailureMessage shouldBe "1 was even"
        nmr.midSentenceFailureMessage shouldBe "1 was odd"
        nmr.midSentenceNegatedFailureMessage shouldBe "1 was even"
        nmr.rawFailureMessage shouldBe "1 was odd"
        nmr.rawNegatedFailureMessage shouldBe "1 was even"
        nmr.rawMidSentenceFailureMessage shouldBe "1 was odd"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "1 was even"
        nmr.failureMessageArgs shouldBe Vector(1)
        nmr.negatedFailureMessageArgs shouldBe Vector(1)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(1)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(1)

      }
    }
    
    describe("apply(BePropertyMatcher) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = be (file)
      
      it("should have pretty toString") {
        mt.toString should be ("be (" + file + ")")
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not file"
        mr.negatedFailureMessage shouldBe myFile + " was file"
        mr.midSentenceFailureMessage shouldBe myFile + " was not file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was file"
        mr.rawFailureMessage shouldBe "{0} was not {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was file"
        nmr.negatedFailureMessage shouldBe myFile + " was not file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not file"
        nmr.rawFailureMessage shouldBe "{0} was {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }
    
    describe("apply(Any) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      val myFileLeft = MyFile("test left", true, false)
      val myFileRight = MyFile("test right", true, false)
      
      val mt = be (myFileRight)
      
      it("should have pretty toString") {
        mt.toString should be ("be (" + myFileRight + ")")
      }
      
      val mr = mt(myFileLeft)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
        mr.negatedFailureMessage shouldBe myFileLeft + " was equal to " + myFileRight
        mr.midSentenceFailureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
        mr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was equal to " + myFileRight
        mr.rawFailureMessage shouldBe "{0} was not equal to {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was equal to {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not equal to {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was equal to {1}"
        mr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFileLeft + " was equal to " + myFileRight
        nmr.negatedFailureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
        nmr.midSentenceFailureMessage shouldBe myFileLeft + " was equal to " + myFileRight
        nmr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
        nmr.rawFailureMessage shouldBe "{0} was equal to {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not equal to {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was equal to {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not equal to {1}"
        nmr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

      }
    }
    
    describe("apply(SortedWord) method returns MatcherFactory") {
      
      val mtf = be (sorted)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("be (sorted)")
        mt.toString should be ("be (sorted)")
      }
      
      val leftList = List(1, 2, 3)
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe leftList + " was not sorted"
        mr.negatedFailureMessage shouldBe leftList + " was sorted"
        mr.midSentenceFailureMessage shouldBe leftList + " was not sorted"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " was sorted"
        mr.rawFailureMessage shouldBe "{0} was not sorted"
        mr.rawNegatedFailureMessage shouldBe "{0} was sorted"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not sorted"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was sorted"
        mr.failureMessageArgs shouldBe Vector(leftList)
        mr.negatedFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe leftList + " was sorted"
        nmr.negatedFailureMessage shouldBe leftList + " was not sorted"
        nmr.midSentenceFailureMessage shouldBe leftList + " was sorted"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " was not sorted"
        nmr.rawFailureMessage shouldBe "{0} was sorted"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not sorted"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was sorted"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not sorted"
        nmr.failureMessageArgs shouldBe Vector(leftList)
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
    }
    
    describe("definedAt(PartialFunction) method returns Matcher") {
      
      val fraction = new PartialFunction[Int, Int] {
        def apply(d: Int) = 42 / d
        def isDefinedAt(d: Int) = d != 0
      }
      
      val mt = be definedAt (8)
      
      it("should have pretty toString") {
        mt.toString should be ("be definedAt 8")
      }
      
      val mr = mt(fraction)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe fraction + " was not defined at 8"
        mr.negatedFailureMessage shouldBe fraction + " was defined at 8"
        mr.midSentenceFailureMessage shouldBe fraction + " was not defined at 8"
        mr.midSentenceNegatedFailureMessage shouldBe fraction + " was defined at 8"
        mr.rawFailureMessage shouldBe "{0} was not defined at {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was defined at {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not defined at {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was defined at {1}"
        mr.failureMessageArgs shouldBe Vector(fraction, 8)
        mr.negatedFailureMessageArgs shouldBe Vector(fraction, 8)
        mr.midSentenceFailureMessageArgs shouldBe Vector(fraction, 8)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fraction, 8)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe fraction + " was defined at 8"
        nmr.negatedFailureMessage shouldBe fraction + " was not defined at 8"
        nmr.midSentenceFailureMessage shouldBe fraction + " was defined at 8"
        nmr.midSentenceNegatedFailureMessage shouldBe fraction + " was not defined at 8"
        nmr.rawFailureMessage shouldBe "{0} was defined at {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not defined at {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was defined at {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not defined at {1}"
        nmr.failureMessageArgs shouldBe Vector(fraction, 8)
        nmr.negatedFailureMessageArgs shouldBe Vector(fraction, 8)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fraction, 8)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fraction, 8)

      }
    }
    
    describe("apply(ResultOfDefinedAt) method returns Matcher") {
      
      val fraction = new PartialFunction[Int, Int] {
        def apply(d: Int) = 42 / d
        def isDefinedAt(d: Int) = d != 0
      }
      
      val resultOfDefinedAt = new ResultOfDefinedAt(8)
      
      val mt = be (resultOfDefinedAt)
      
      it("should have pretty toString") {
        mt.toString should be ("be definedAt 8")
      }
      
      val mr = mt(fraction)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe fraction + " was not defined at 8"
        mr.negatedFailureMessage shouldBe fraction + " was defined at 8"
        mr.midSentenceFailureMessage shouldBe fraction + " was not defined at 8"
        mr.midSentenceNegatedFailureMessage shouldBe fraction + " was defined at 8"
        mr.rawFailureMessage shouldBe "{0} was not defined at {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was defined at {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not defined at {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was defined at {1}"
        mr.failureMessageArgs shouldBe Vector(fraction, 8)
        mr.negatedFailureMessageArgs shouldBe Vector(fraction, 8)
        mr.midSentenceFailureMessageArgs shouldBe Vector(fraction, 8)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fraction, 8)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe fraction + " was defined at 8"
        nmr.negatedFailureMessage shouldBe fraction + " was not defined at 8"
        nmr.midSentenceFailureMessage shouldBe fraction + " was defined at 8"
        nmr.midSentenceNegatedFailureMessage shouldBe fraction + " was not defined at 8"
        nmr.rawFailureMessage shouldBe "{0} was defined at {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not defined at {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was defined at {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not defined at {1}"
        nmr.failureMessageArgs shouldBe Vector(fraction, 8)
        nmr.negatedFailureMessageArgs shouldBe Vector(fraction, 8)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fraction, 8)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fraction, 8)

      }
    }
    
    describe("apply(ResultOfATypeInvocation) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAType = new ResultOfATypeInvocation(clazz)
      
      val mt = be (resultOfAType)
      
      it("should have pretty toString") {
        mt.toString should be ("be (a [" + clazz.getName + "])")
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.negatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.midSentenceFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.rawFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.negatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.midSentenceFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.rawFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))

      }
    }
    
    describe("apply(ResultOfAnTypeInvocation) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAnType = new ResultOfAnTypeInvocation(clazz)
      
      val mt = be (resultOfAnType)
      
      it("should have pretty toString") {
        mt.toString should be ("be (an [" + clazz.getName + "])")
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.negatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.midSentenceFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.rawFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.negatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.midSentenceFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.rawFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))

      }
    }
    
    describe("apply(ReadableWord) method returns MatcherFactory") {
      
      class MyFile {
        def isReadable: Boolean = true
      }
      
      val mtf = be (readable)
      val mt = mtf.matcher[MyFile]
      
      it("should have pretty toString") {
        mtf.toString should be ("be (readable)")
        mt.toString should be ("be (readable)")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not readable"
        mr.negatedFailureMessage shouldBe myFile + " was readable"
        mr.midSentenceFailureMessage shouldBe myFile + " was not readable"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was readable"
        mr.rawFailureMessage shouldBe "{0} was not readable"
        mr.rawNegatedFailureMessage shouldBe "{0} was readable"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not readable"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was readable"
        mr.failureMessageArgs shouldBe Vector(myFile)
        mr.negatedFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was readable"
        nmr.negatedFailureMessage shouldBe myFile + " was not readable"
        nmr.midSentenceFailureMessage shouldBe myFile + " was readable"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not readable"
        nmr.rawFailureMessage shouldBe "{0} was readable"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not readable"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was readable"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not readable"
        nmr.failureMessageArgs shouldBe Vector(myFile)
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
    }
    
    describe("apply(WritableWord) method returns MatcherFactory") {
      
      class MyFile {
        def isWritable: Boolean = true
      }
      
      val mtf = be (writable)
      val mt = mtf.matcher[MyFile]
      
      it("should have pretty toString") {
        mtf.toString should be ("be (writable)")
        mt.toString should be ("be (writable)")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFile + " was not writable"
        mr.negatedFailureMessage shouldBe myFile + " was writable"
        mr.midSentenceFailureMessage shouldBe myFile + " was not writable"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was writable"
        mr.rawFailureMessage shouldBe "{0} was not writable"
        mr.rawNegatedFailureMessage shouldBe "{0} was writable"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not writable"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was writable"
        mr.failureMessageArgs shouldBe Vector(myFile)
        mr.negatedFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFile + " was writable"
        nmr.negatedFailureMessage shouldBe myFile + " was not writable"
        nmr.midSentenceFailureMessage shouldBe myFile + " was writable"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was not writable"
        nmr.rawFailureMessage shouldBe "{0} was writable"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not writable"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was writable"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not writable"
        nmr.failureMessageArgs shouldBe Vector(myFile)
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
    }
    
    describe("apply(EmptyWord) method returns MatcherFactory") {
      
      val mtf = be (empty)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("be (empty)")
        mt.toString should be ("be (empty)")
      }
      
      val leftList = List.empty[Int]
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe leftList + " was not empty"
        mr.negatedFailureMessage shouldBe leftList + " was empty"
        mr.midSentenceFailureMessage shouldBe leftList + " was not empty"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " was empty"
        mr.rawFailureMessage shouldBe "{0} was not empty"
        mr.rawNegatedFailureMessage shouldBe "{0} was empty"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not empty"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was empty"
        mr.failureMessageArgs shouldBe Vector(leftList)
        mr.negatedFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe leftList + " was empty"
        nmr.negatedFailureMessage shouldBe leftList + " was not empty"
        nmr.midSentenceFailureMessage shouldBe leftList + " was empty"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " was not empty"
        nmr.rawFailureMessage shouldBe "{0} was empty"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not empty"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was empty"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not empty"
        nmr.failureMessageArgs shouldBe Vector(leftList)
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
    }
    
    describe("apply(DefinedWord) method returns MatcherFactory") {
      
      val mtf = be (defined)
      val mt = mtf.matcher[Option[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("be (defined)")
        mt.toString should be ("be (defined)")
      }
      
      val leftOption = Some(1)
      val mr = mt(leftOption)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe leftOption + " was not defined"
        mr.negatedFailureMessage shouldBe leftOption + " was defined"
        mr.midSentenceFailureMessage shouldBe leftOption + " was not defined"
        mr.midSentenceNegatedFailureMessage shouldBe leftOption + " was defined"
        mr.rawFailureMessage shouldBe "{0} was not defined"
        mr.rawNegatedFailureMessage shouldBe "{0} was defined"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was not defined"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was defined"
        mr.failureMessageArgs shouldBe Vector(leftOption)
        mr.negatedFailureMessageArgs shouldBe Vector(leftOption)
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftOption)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftOption)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe leftOption + " was defined"
        nmr.negatedFailureMessage shouldBe leftOption + " was not defined"
        nmr.midSentenceFailureMessage shouldBe leftOption + " was defined"
        nmr.midSentenceNegatedFailureMessage shouldBe leftOption + " was not defined"
        nmr.rawFailureMessage shouldBe "{0} was defined"
        nmr.rawNegatedFailureMessage shouldBe "{0} was not defined"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was defined"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not defined"
        nmr.failureMessageArgs shouldBe Vector(leftOption)
        nmr.negatedFailureMessageArgs shouldBe Vector(leftOption)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftOption)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftOption)

      }
    }
  }
}
