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
import org.scalactic.Prettifier

class ContainWordSpec extends Spec {
  
  def `should have pretty toString` {
    contain.toString should be ("contain")
  }
  
  object `ContainWord ` {
    
    object `apply(Any) method returns MatcherFactory1` {
      
      val mtf = contain (2)
      val mt = mtf.matcher[Array[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain (2)")
        mt.toString should be ("contain (2)")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("Array(1, 2, 3) did not contain element 2"),
          'negatedFailureMessage ("Array(1, 2, 3) contained element 2"),
          'midSentenceFailureMessage ("Array(1, 2, 3) did not contain element 2"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) contained element 2"),
          'rawFailureMessage ("{0} did not contain element {1}"),
          'rawNegatedFailureMessage ("{0} contained element {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain element {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained element {1}"),
          'failureMessageArgs(Vector(lhs, 2)),
          'negatedFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 2))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("Array(1, 2, 3) contained element 2"),
          'negatedFailureMessage ("Array(1, 2, 3) did not contain element 2"),
          'midSentenceFailureMessage ("Array(1, 2, 3) contained element 2"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) did not contain element 2"),
          'rawFailureMessage ("{0} contained element {1}"),
          'rawNegatedFailureMessage ("{0} did not contain element {1}"),
          'rawMidSentenceFailureMessage ("{0} contained element {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain element {1}"),
          'failureMessageArgs(Vector(lhs, 2)),
          'negatedFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 2))    
        )
      }
    }
    
    object `key(Any) method returns MatcherFactory1` {
      
      val mtf = contain key ("2")
      val mt = mtf.matcher[Map[String, String]]
      
      def `should have pretty toString` {
        mt.toString should be ("contain key \"2\"")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (decorateToStringValue(lhs) + " did not contain key \"2\""),
          'negatedFailureMessage (decorateToStringValue(lhs) + " contained key \"2\""),
          'midSentenceFailureMessage (decorateToStringValue(lhs) + " did not contain key \"2\""),
          'midSentenceNegatedFailureMessage (decorateToStringValue(lhs) + " contained key \"2\""),
          'rawFailureMessage ("{0} did not contain key {1}"),
          'rawNegatedFailureMessage ("{0} contained key {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain key {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained key {1}"),
          'failureMessageArgs(Vector(lhs, "2")),
          'negatedFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "2"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (decorateToStringValue(lhs) + " contained key \"2\""),
          'negatedFailureMessage (decorateToStringValue(lhs) + " did not contain key \"2\""),
          'midSentenceFailureMessage (decorateToStringValue(lhs) + " contained key \"2\""),
          'midSentenceNegatedFailureMessage (decorateToStringValue(lhs) + " did not contain key \"2\""),
          'rawFailureMessage ("{0} contained key {1}"),
          'rawNegatedFailureMessage ("{0} did not contain key {1}"),
          'rawMidSentenceFailureMessage ("{0} contained key {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain key {1}"),
          'failureMessageArgs(Vector(lhs, "2")),
          'negatedFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "2"))    
        )
      }
    }
    
    object `value(Any) method returns MatcherFactory1` {
      
      val mtf = contain value ("two")
      val mt = mtf.matcher[Map[String, String]]
      
      def `should have pretty toString` {
        mt.toString should be ("contain value \"two\"")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (decorateToStringValue(lhs) + " did not contain value \"two\""),
          'negatedFailureMessage (decorateToStringValue(lhs) + " contained value \"two\""),
          'midSentenceFailureMessage (decorateToStringValue(lhs) + " did not contain value \"two\""),
          'midSentenceNegatedFailureMessage (decorateToStringValue(lhs) + " contained value \"two\""),
          'rawFailureMessage ("{0} did not contain value {1}"),
          'rawNegatedFailureMessage ("{0} contained value {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain value {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained value {1}"),
          'failureMessageArgs(Vector(lhs, "two")),
          'negatedFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "two"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (decorateToStringValue(lhs) + " contained value \"two\""),
          'negatedFailureMessage (decorateToStringValue(lhs) + " did not contain value \"two\""),
          'midSentenceFailureMessage (decorateToStringValue(lhs) + " contained value \"two\""),
          'midSentenceNegatedFailureMessage (decorateToStringValue(lhs) + " did not contain value \"two\""),
          'rawFailureMessage ("{0} contained value {1}"),
          'rawNegatedFailureMessage ("{0} did not contain value {1}"),
          'rawMidSentenceFailureMessage ("{0} contained value {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain value {1}"),
          'failureMessageArgs(Vector(lhs, "two")),
          'negatedFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "two"))    
        )
      }
    }
    
    object `a(AMatcher) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = contain a (file)
      
      def `should have pretty toString` {
        mt.toString should be ("contain a AMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (leftList + " did not contain a file"),
          'negatedFailureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'midSentenceFailureMessage (leftList + " did not contain a file"),
          'midSentenceNegatedFailureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'rawFailureMessage ("{0} did not contain a {1}"),
          'rawNegatedFailureMessage ("{0} contained a {1}: {2}"),
          'rawMidSentenceFailureMessage ("{0} did not contain a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained a {1}: {2}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'negatedFailureMessage (leftList + " did not contain a file"),
          'midSentenceFailureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'midSentenceNegatedFailureMessage (leftList + " did not contain a file"),
          'rawFailureMessage ("{0} contained a {1}: {2}"),
          'rawNegatedFailureMessage ("{0} did not contain a {1}"),
          'rawMidSentenceFailureMessage ("{0} contained a {1}: {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain a {1}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file")))    
        )
      }
    }
    
    object `an(AnMatcher) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AnMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = contain an (file)
      
      def `should have pretty toString` {
        mt.toString should be ("contain an AnMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (leftList + " did not contain an file"),
          'negatedFailureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'midSentenceFailureMessage (leftList + " did not contain an file"),
          'midSentenceNegatedFailureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'rawFailureMessage ("{0} did not contain an {1}"),
          'rawNegatedFailureMessage ("{0} contained an {1}: {2}"),
          'rawMidSentenceFailureMessage ("{0} did not contain an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained an {1}: {2}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'negatedFailureMessage (leftList + " did not contain an file"),
          'midSentenceFailureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'midSentenceNegatedFailureMessage (leftList + " did not contain an file"),
          'rawFailureMessage ("{0} contained an {1}: {2}"),
          'rawNegatedFailureMessage ("{0} did not contain an {1}"),
          'rawMidSentenceFailureMessage ("{0} contained an {1}: {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain an {1}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file")))    
        )
      }
    }
    
    object `oneOf(Any*) method returns MatcherFactory1` {
      
      val mtf = contain oneOf (2, 8)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain oneOf (2, 8)")
        mt.toString should be ("contain oneOf (2, 8)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (FailureMessages("didNotContainOneOfElements", lhs, UnquotedString("2, 8"))),
          'negatedFailureMessage (FailureMessages("containedOneOfElements", lhs, UnquotedString("2, 8"))),
          'midSentenceFailureMessage (FailureMessages("didNotContainOneOfElements", lhs, UnquotedString("2, 8"))),
          'midSentenceNegatedFailureMessage (FailureMessages("containedOneOfElements", lhs, UnquotedString("2, 8"))),
          'rawFailureMessage (FailureMessages("didNotContainOneOfElements")),
          'rawNegatedFailureMessage (FailureMessages("containedOneOfElements")),
          'rawMidSentenceFailureMessage (FailureMessages("didNotContainOneOfElements")),
          'rawMidSentenceNegatedFailureMessage (FailureMessages("containedOneOfElements")),
          'failureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (FailureMessages("containedOneOfElements", lhs, UnquotedString("2, 8"))),
          'negatedFailureMessage (FailureMessages("didNotContainOneOfElements", lhs, UnquotedString("2, 8"))),
          'midSentenceFailureMessage (FailureMessages("containedOneOfElements", lhs, UnquotedString("2, 8"))),
          'midSentenceNegatedFailureMessage (FailureMessages("didNotContainOneOfElements", lhs, UnquotedString("2, 8"))),
          'rawFailureMessage (FailureMessages("containedOneOfElements")),
          'rawNegatedFailureMessage (FailureMessages("didNotContainOneOfElements")),
          'rawMidSentenceFailureMessage (FailureMessages("containedOneOfElements")),
          'rawMidSentenceNegatedFailureMessage (FailureMessages("didNotContainOneOfElements")),
          'failureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8")))    
        )
      }
    }
    
    object `oneElementOf(col) method returns MatcherFactory1` {
      
      val mtf = contain oneElementOf Seq(2, 8)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain oneElementOf " + Prettifier.default(Seq(2, 8)))
        mt.toString should be ("contain oneElementOf " + Prettifier.default(Seq(2, 8)))
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (FailureMessages("didNotContainOneElementOf", lhs, Seq(2, 8))),
          'negatedFailureMessage (FailureMessages("containedOneElementOf", lhs, Seq(2, 8))),
          'midSentenceFailureMessage (FailureMessages("didNotContainOneElementOf", lhs, Seq(2, 8))),
          'midSentenceNegatedFailureMessage (FailureMessages("containedOneElementOf", lhs, Seq(2, 8))),
          'rawFailureMessage (FailureMessages("didNotContainOneElementOf")),
          'rawNegatedFailureMessage (FailureMessages("containedOneElementOf")),
          'rawMidSentenceFailureMessage (FailureMessages("didNotContainOneElementOf")),
          'rawMidSentenceNegatedFailureMessage (FailureMessages("containedOneElementOf")),
          'failureMessageArgs(Vector(lhs, Seq(2, 8))),
          'negatedFailureMessageArgs(Vector(lhs, Seq(2, 8))),
          'midSentenceFailureMessageArgs(Vector(lhs, Seq(2, 8))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, Seq(2, 8)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (FailureMessages("containedOneElementOf", lhs, Seq(2, 8))),
          'negatedFailureMessage (FailureMessages("didNotContainOneElementOf", lhs, Seq(2, 8))),
          'midSentenceFailureMessage (FailureMessages("containedOneElementOf", lhs, Seq(2, 8))),
          'midSentenceNegatedFailureMessage (FailureMessages("didNotContainOneElementOf", lhs, Seq(2, 8))),
          'rawFailureMessage (FailureMessages("containedOneElementOf")),
          'rawNegatedFailureMessage (FailureMessages("didNotContainOneElementOf")),
          'rawMidSentenceFailureMessage (FailureMessages("containedOneElementOf")),
          'rawMidSentenceNegatedFailureMessage (FailureMessages("didNotContainOneElementOf")),
          'failureMessageArgs(Vector(lhs, Seq(2, 8))),
          'negatedFailureMessageArgs(Vector(lhs, Seq(2, 8))),
          'midSentenceFailureMessageArgs(Vector(lhs, Seq(2, 8))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, Seq(2, 8)))    
        )
      }
    }
    
    object `atLeastOneOf(Any*) method returns MatcherFactory1` {
      
      val mtf = contain atLeastOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain atLeastOneOf (1, 2)")
        mt.toString should be ("contain atLeastOneOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain at least one of (1, 2)"),
          'negatedFailureMessage (lhs + " contained at least one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain at least one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained at least one of (1, 2)"),
          'rawFailureMessage ("{0} did not contain at least one of ({1})"),
          'rawNegatedFailureMessage ("{0} contained at least one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain at least one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained at least one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained at least one of (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain at least one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained at least one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain at least one of (1, 2)"),
          'rawFailureMessage ("{0} contained at least one of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain at least one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained at least one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain at least one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `noneOf(Any*) method returns MatcherFactory1` {
      
      val mtf = contain noneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain noneOf (1, 2)")
        mt.toString should be ("contain noneOf (1, 2)")
      }
      
      val lhs = List(7, 8, 9)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (FailureMessages("containedAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'negatedFailureMessage (FailureMessages("didNotContainAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessage (FailureMessages("containedAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessage (FailureMessages("didNotContainAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'rawFailureMessage (FailureMessages("containedAtLeastOneOf")),
          'rawNegatedFailureMessage (FailureMessages("didNotContainAtLeastOneOf")),
          'rawMidSentenceFailureMessage (FailureMessages("containedAtLeastOneOf")),
          'rawMidSentenceNegatedFailureMessage (FailureMessages("didNotContainAtLeastOneOf")),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (FailureMessages("didNotContainAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'negatedFailureMessage (FailureMessages("containedAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessage (FailureMessages("didNotContainAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessage (FailureMessages("containedAtLeastOneOf", lhs, UnquotedString("1, 2"))),
          'rawFailureMessage (FailureMessages("didNotContainAtLeastOneOf")),
          'rawNegatedFailureMessage (FailureMessages("containedAtLeastOneOf")),
          'rawMidSentenceFailureMessage (FailureMessages("didNotContainAtLeastOneOf")),
          'rawMidSentenceNegatedFailureMessage (FailureMessages("containedAtLeastOneOf")),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `theSameElementsAs(GenTraversable) method returns MatcherFactory1` {
      
      val rhs = List(1, 2, 3)
      val mtf = contain theSameElementsAs rhs
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain theSameElementsAs List(1, 2, 3)")
        mt.toString should be ("contain theSameElementsAs List(1, 2, 3)")
      }
      
      val lhs = List(3, 2, 1)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain the same elements as " + rhs),
          'negatedFailureMessage (lhs + " contained the same elements as " + rhs),
          'midSentenceFailureMessage (lhs + " did not contain the same elements as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " contained the same elements as " + rhs),
          'rawFailureMessage ("{0} did not contain the same elements as {1}"),
          'rawNegatedFailureMessage ("{0} contained the same elements as {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain the same elements as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained the same elements as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained the same elements as " + rhs),
          'negatedFailureMessage (lhs + " did not contain the same elements as " + rhs),
          'midSentenceFailureMessage (lhs + " contained the same elements as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " did not contain the same elements as " + rhs),
          'rawFailureMessage ("{0} contained the same elements as {1}"),
          'rawNegatedFailureMessage ("{0} did not contain the same elements as {1}"),
          'rawMidSentenceFailureMessage ("{0} contained the same elements as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain the same elements as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
    }
    
    object `theSameElementsInOrderAs(GenTraversable) method returns MatcherFactory1` {
      
      val rhs = List(1, 2, 3)
      val mtf = contain theSameElementsInOrderAs rhs
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain theSameElementsInOrderAs List(1, 2, 3)")
        mt.toString should be ("contain theSameElementsInOrderAs List(1, 2, 3)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'negatedFailureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'midSentenceFailureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'rawFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'rawNegatedFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'negatedFailureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'midSentenceFailureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'rawFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'rawNegatedFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
    }
    
    object `only(Any*) method returns MatcherFactory1` {
      
      val mtf = contain only (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain only (1, 2)")
        mt.toString should be ("contain only (1, 2)")
      }
      
      val lhs = List(2, 1)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain only (1, 2)"),
          'negatedFailureMessage (lhs + " contained only (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain only (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained only (1, 2)"),
          'rawFailureMessage ("{0} did not contain only ({1})"),
          'rawNegatedFailureMessage ("{0} contained only ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain only ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained only ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained only (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain only (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained only (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain only (1, 2)"),
          'rawFailureMessage ("{0} contained only ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain only ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained only ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain only ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `inOrderOnly(Any*) method returns MatcherFactory1` {
      
      val mtf = contain inOrderOnly (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain inOrderOnly (1, 2)")
        mt.toString should be ("contain inOrderOnly (1, 2)")
      }
      
      val lhs = List(1, 2)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain only (1, 2) in order"),
          'negatedFailureMessage (lhs + " contained only (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " did not contain only (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " contained only (1, 2) in order"),
          'rawFailureMessage ("{0} did not contain only ({1}) in order"),
          'rawNegatedFailureMessage ("{0} contained only ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} did not contain only ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained only ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained only (1, 2) in order"),
          'negatedFailureMessage (lhs + " did not contain only (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " contained only (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain only (1, 2) in order"),
          'rawFailureMessage ("{0} contained only ({1}) in order"),
          'rawNegatedFailureMessage ("{0} did not contain only ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} contained only ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain only ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `allOf(Any*) method returns MatcherFactory1` {
      
      val mtf = contain allOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain allOf (1, 2)")
        mt.toString should be ("contain allOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain all of (1, 2)"),
          'negatedFailureMessage (lhs + " contained all of (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain all of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained all of (1, 2)"),
          'rawFailureMessage ("{0} did not contain all of ({1})"),
          'rawNegatedFailureMessage ("{0} contained all of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain all of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained all of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained all of (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain all of (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained all of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain all of (1, 2)"),
          'rawFailureMessage ("{0} contained all of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain all of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained all of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain all of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `inOrder(Any*) method returns MatcherFactory1` {
      
      val mtf = contain inOrder (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain inOrder (1, 2)")
        mt.toString should be ("contain inOrder (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain all of (1, 2) in order"),
          'negatedFailureMessage (lhs + " contained all of (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " did not contain all of (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " contained all of (1, 2) in order"),
          'rawFailureMessage ("{0} did not contain all of ({1}) in order"),
          'rawNegatedFailureMessage ("{0} contained all of ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} did not contain all of ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained all of ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained all of (1, 2) in order"),
          'negatedFailureMessage (lhs + " did not contain all of (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " contained all of (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain all of (1, 2) in order"),
          'rawFailureMessage ("{0} contained all of ({1}) in order"),
          'rawNegatedFailureMessage ("{0} did not contain all of ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} contained all of ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain all of ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `atMostOneOf(Any*) method returns MatcherFactory1` {
      
      val mtf = contain atMostOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("contain atMostOneOf (1, 2)")
        mt.toString should be ("contain atMostOneOf (1, 2)")
      }
      
      val lhs = List(1, 6, 8)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain at most one of (1, 2)"),
          'negatedFailureMessage (lhs + " contained at most one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain at most one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained at most one of (1, 2)"),
          'rawFailureMessage ("{0} did not contain at most one of ({1})"),
          'rawNegatedFailureMessage ("{0} contained at most one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain at most one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained at most one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " contained at most one of (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain at most one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained at most one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain at most one of (1, 2)"),
          'rawFailureMessage ("{0} contained at most one of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain at most one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained at most one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain at most one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
  }
  
}