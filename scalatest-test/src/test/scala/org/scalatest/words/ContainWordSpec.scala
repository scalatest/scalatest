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
import Matchers._
import matchers.{AMatcher, 
                 AnMatcher}
import FailureMessages.decorateToStringValue

class ContainWordSpec extends FunSpec {
  
  it("should have pretty toString") {
    contain.toString should be ("contain")
  }
  
  describe("ContainWord ") {
    
    describe("apply(Any) method returns MatcherFactory1") {
      
      val mtf = contain (2)
      val mt = mtf.matcher[Array[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain (2)")
        mt.toString should be ("contain (2)")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        mr.negatedFailureMessage shouldBe "Array(1, 2, 3) contained element 2"
        mr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        mr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) contained element 2"
        mr.rawFailureMessage shouldBe "{0} did not contain element {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} contained element {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain element {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained element {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, 2)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, 2)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 2)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 2)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "Array(1, 2, 3) contained element 2"
        nmr.negatedFailureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        nmr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) contained element 2"
        nmr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        nmr.rawFailureMessage shouldBe "{0} contained element {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain element {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained element {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain element {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, 2)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, 2)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 2)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 2)

      }
    }
    
    describe("key(Any) method returns MatcherFactory1") {
      
      val mtf = contain key ("2")
      val mt = mtf.matcher[Map[String, String]]
      
      it("should have pretty toString") {
        mt.toString should be ("contain key \"2\"")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        mr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        mr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        mr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        mr.rawFailureMessage shouldBe "{0} did not contain key {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} contained key {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain key {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained key {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, "2")
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, "2")
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "2")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "2")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        nmr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        nmr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        nmr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        nmr.rawFailureMessage shouldBe "{0} contained key {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain key {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained key {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain key {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, "2")
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, "2")
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "2")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "2")

      }
    }
    
    describe("value(Any) method returns MatcherFactory1") {
      
      val mtf = contain value ("two")
      val mt = mtf.matcher[Map[String, String]]
      
      it("should have pretty toString") {
        mt.toString should be ("contain value \"two\"")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        mr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        mr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        mr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        mr.rawFailureMessage shouldBe "{0} did not contain value {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} contained value {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain value {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained value {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, "two")
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, "two")
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "two")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "two")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        nmr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        nmr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        nmr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        nmr.rawFailureMessage shouldBe "{0} contained value {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain value {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained value {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain value {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, "two")
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, "two")
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "two")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "two")

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
      
      val mt = contain a (file)
      
      it("should have pretty toString") {
        mt.toString should be ("contain a AMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe leftList + " did not contain a file"
        mr.negatedFailureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        mr.midSentenceFailureMessage shouldBe leftList + " did not contain a file"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        mr.rawFailureMessage shouldBe "{0} did not contain a {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} contained a {1}: {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain a {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained a {1}: {2}"
        mr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        nmr.negatedFailureMessage shouldBe leftList + " did not contain a file"
        nmr.midSentenceFailureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " did not contain a file"
        nmr.rawFailureMessage shouldBe "{0} contained a {1}: {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain a {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained a {1}: {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain a {1}"
        nmr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))

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
      
      val mt = contain an (file)
      
      it("should have pretty toString") {
        mt.toString should be ("contain an AnMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe leftList + " did not contain an file"
        mr.negatedFailureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        mr.midSentenceFailureMessage shouldBe leftList + " did not contain an file"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        mr.rawFailureMessage shouldBe "{0} did not contain an {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} contained an {1}: {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain an {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained an {1}: {2}"
        mr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        nmr.negatedFailureMessage shouldBe leftList + " did not contain an file"
        nmr.midSentenceFailureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " did not contain an file"
        nmr.rawFailureMessage shouldBe "{0} contained an {1}: {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain an {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained an {1}: {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain an {1}"
        nmr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))

      }
    }
    
    describe("oneOf(Any*) method returns MatcherFactory1") {
      
      val mtf = contain oneOf (2, 8)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain oneOf (2, 8)")
        mt.toString should be ("contain oneOf (2, 8)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        mr.negatedFailureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        mr.midSentenceFailureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        mr.midSentenceNegatedFailureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        mr.rawFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        mr.rawNegatedFailureMessage shouldBe Resources.rawContainedOneOfElements
        mr.rawMidSentenceFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        mr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawContainedOneOfElements
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.negatedFailureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.midSentenceFailureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.midSentenceNegatedFailureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.rawFailureMessage shouldBe Resources.rawContainedOneOfElements
        nmr.rawNegatedFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        nmr.rawMidSentenceFailureMessage shouldBe Resources.rawContainedOneOfElements
        nmr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))

      }
    }
    
    describe("atLeastOneOf(Any*) method returns MatcherFactory1") {
      
      val mtf = contain atLeastOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain atLeastOneOf (1, 2)")
        mt.toString should be ("contain atLeastOneOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " contained at least one of (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained at least one of (1, 2)"
        mr.rawFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} contained at least one of ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained at least one of ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained at least one of (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " contained at least one of (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} contained at least one of ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained at least one of ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("noneOf(Any*) method returns MatcherFactory1") {
      
      val mtf = contain noneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain noneOf (1, 2)")
        mt.toString should be ("contain noneOf (1, 2)")
      }
      
      val lhs = List(7, 8, 9)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.rawFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        mr.rawNegatedFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        mr.rawMidSentenceFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        mr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.rawFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        nmr.rawNegatedFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        nmr.rawMidSentenceFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        nmr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("theSameElementsAs(GenTraversable) method returns MatcherFactory1") {
      
      val rhs = List(1, 2, 3)
      val mtf = contain theSameElementsAs rhs
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain theSameElementsAs List(1, 2, 3)")
        mt.toString should be ("contain theSameElementsAs List(1, 2, 3)")
      }
      
      val lhs = List(3, 2, 1)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        mr.negatedFailureMessage shouldBe lhs + " contained the same elements as " + rhs
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained the same elements as " + rhs
        mr.rawFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} contained the same elements as {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained the same elements as {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, rhs)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained the same elements as " + rhs
        nmr.negatedFailureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        nmr.midSentenceFailureMessage shouldBe lhs + " contained the same elements as " + rhs
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        nmr.rawFailureMessage shouldBe "{0} contained the same elements as {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained the same elements as {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
    }
    
    describe("theSameElementsInOrderAs(GenTraversable) method returns MatcherFactory1") {
      
      val rhs = List(1, 2, 3)
      val mtf = contain theSameElementsInOrderAs rhs
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain theSameElementsInOrderAs List(1, 2, 3)")
        mt.toString should be ("contain theSameElementsInOrderAs List(1, 2, 3)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        mr.negatedFailureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        mr.rawFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, rhs)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        nmr.negatedFailureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        nmr.midSentenceFailureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        nmr.rawFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
    }
    
    describe("only(Any*) method returns MatcherFactory1") {
      
      val mtf = contain only (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain only (1, 2)")
        mt.toString should be ("contain only (1, 2)")
      }
      
      val lhs = List(2, 1)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain only (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " contained only (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain only (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained only (1, 2)"
        mr.rawFailureMessage shouldBe "{0} did not contain only ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} contained only ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain only ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained only ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained only (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " did not contain only (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " contained only (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain only (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} contained only ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain only ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained only ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain only ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("inOrderOnly(Any*) method returns MatcherFactory1") {
      
      val mtf = contain inOrderOnly (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain inOrderOnly (1, 2)")
        mt.toString should be ("contain inOrderOnly (1, 2)")
      }
      
      val lhs = List(1, 2)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        mr.negatedFailureMessage shouldBe lhs + " contained only (1, 2) in order"
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained only (1, 2) in order"
        mr.rawFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        mr.rawNegatedFailureMessage shouldBe "{0} contained only ({1}) in order"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained only ({1}) in order"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained only (1, 2) in order"
        nmr.negatedFailureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        nmr.midSentenceFailureMessage shouldBe lhs + " contained only (1, 2) in order"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        nmr.rawFailureMessage shouldBe "{0} contained only ({1}) in order"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained only ({1}) in order"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("allOf(Any*) method returns MatcherFactory1") {
      
      val mtf = contain allOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain allOf (1, 2)")
        mt.toString should be ("contain allOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain all of (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " contained all of (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain all of (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained all of (1, 2)"
        mr.rawFailureMessage shouldBe "{0} did not contain all of ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} contained all of ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain all of ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained all of ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained all of (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " did not contain all of (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " contained all of (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain all of (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} contained all of ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain all of ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained all of ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain all of ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("inOrder(Any*) method returns MatcherFactory1") {
      
      val mtf = contain inOrder (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain inOrder (1, 2)")
        mt.toString should be ("contain inOrder (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        mr.negatedFailureMessage shouldBe lhs + " contained all of (1, 2) in order"
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained all of (1, 2) in order"
        mr.rawFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        mr.rawNegatedFailureMessage shouldBe "{0} contained all of ({1}) in order"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained all of ({1}) in order"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained all of (1, 2) in order"
        nmr.negatedFailureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        nmr.midSentenceFailureMessage shouldBe lhs + " contained all of (1, 2) in order"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        nmr.rawFailureMessage shouldBe "{0} contained all of ({1}) in order"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained all of ({1}) in order"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("atMostOneOf(Any*) method returns MatcherFactory1") {
      
      val mtf = contain atMostOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("contain atMostOneOf (1, 2)")
        mt.toString should be ("contain atMostOneOf (1, 2)")
      }
      
      val lhs = List(1, 6, 8)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " contained at most one of (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " contained at most one of (1, 2)"
        mr.rawFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} contained at most one of ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained at most one of ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " contained at most one of (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " contained at most one of (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} contained at most one of ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} contained at most one of ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
  }
  
}