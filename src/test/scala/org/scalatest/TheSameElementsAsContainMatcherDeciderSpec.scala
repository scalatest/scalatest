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

import org.scalactic.Equality
import org.scalactic.Explicitly
import org.scalactic.StringNormalizations._
import org.scalactic.Uniformity
import org.scalactic.Entry
import SharedHelpers._
import Matchers._

class TheSameElementsAsContainMatcherDeciderSpec extends Spec with Explicitly {
  
  val mapTrimmed: Uniformity[(Int, String)] =
    new Uniformity[(Int, String)] {
      def normalized(s: (Int, String)): (Int, String) = (s._1, s._2.trim)
      def normalizedCanHandle(b: Any) = 
        b match {
          case (_: Int, _: String) => true
          case _ => false
        }
      def normalizedOrSame(b: Any) = 
        b match {
          case (k: Int, v: String) => normalized((k, v))
          case _ => b
        }
    }
  
  val javaMapTrimmed: Uniformity[java.util.Map.Entry[Int, String]] =
    new Uniformity[java.util.Map.Entry[Int, String]] {
      def normalized(s: java.util.Map.Entry[Int, String]): java.util.Map.Entry[Int, String] = Entry(s.getKey, s.getValue.trim)
      def normalizedCanHandle(b: Any) = 
        b match {
          case entry: java.util.Map.Entry[_, _] => 
            (entry.getKey, entry.getValue) match {
              case (_: Int, _: String) => true
              case _ => false
            }
          case _ => false
        }
      def normalizedOrSame(b: Any) = 
        b match {
          case entry: java.util.Map.Entry[_, _] => 
            (entry.getKey, entry.getValue) match {
              case (k: Int, v: String) => normalized(Entry(k, v))
              case _ => b
            }
          case _ => b
        }
    }
  
  val incremented: Uniformity[Int] = 
    new Uniformity[Int] {
      var count = 0
      def normalized(s: Int): Int = {
        count += 1
        s + count
      }
      def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[Int]
      def normalizedOrSame(b: Any) =
        b match {
          case i: Int => normalized(i)
          case _ => b
        }
    }
  
  val mapIncremented: Uniformity[(Int, String)] = 
    new Uniformity[(Int, String)] {
      var count = 0
      def normalized(s: (Int, String)): (Int, String) = {
        count += 1
        (s._1 + count, s._2)
      }
      def normalizedCanHandle(b: Any) = 
        b match {
          case (_: Int, _: String) => true
          case _ => false
        }
      def normalizedOrSame(b: Any) = 
        b match {
          case (k: Int, v: String) => normalized((k, v))
          case _ => b
        }
    }
  
  val javaMapIncremented: Uniformity[java.util.Map.Entry[Int, String]] = 
    new Uniformity[java.util.Map.Entry[Int, String]] {
      var count = 0
      def normalized(s: java.util.Map.Entry[Int, String]): java.util.Map.Entry[Int, String] = {
        count += 1
        Entry(s.getKey + count, s.getValue)
      }
      def normalizedCanHandle(b: Any) = 
        b match {
          case entry: java.util.Map.Entry[_, _] => 
            (entry.getKey, entry.getValue) match {
              case (_: Int, _: String) => true
              case _ => false
            }
          case _ => false
        }
      def normalizedOrSame(b: Any) = 
        b match {
          case entry: java.util.Map.Entry[_, _] => 
            (entry.getKey, entry.getValue) match {
              case (k: Int, v: String) => normalized(Entry(k, v))
              case _ => b
            }
          case _ => b
        }
    }
  
  val lowerCaseEquality = 
    new Equality[String] {
      def areEqual(left: String, right: Any) = 
        left.toLowerCase == (right match {
          case s: String => s.toLowerCase
          case other => other
        })
    }
  
  val mapLowerCaseEquality = 
    new Equality[(Int, String)] {
      def areEqual(left: (Int, String), right: Any) = 
        right match {
          case t2: Tuple2[_, _] =>  
            left._1 == t2._1 && 
            left._2.toLowerCase == (t2._2 match {
              case s: String => s.toLowerCase
              case other => other
            })
          case right => left == right
      }
    }
  
  val javaMapLowerCaseEquality = 
    new Equality[java.util.Map.Entry[Int, String]] {
      def areEqual(left: java.util.Map.Entry[Int, String], right: Any) = 
        right match {
          case entry: java.util.Map.Entry[_, _] =>  
            left.getKey == entry.getKey && 
            left.getValue.toLowerCase == (entry.getValue match {
              case s: String => s.toLowerCase
              case other => other
            })
          case right => left == right
      }
    }
  
  val reverseEquality = 
    new Equality[String] {
      def areEqual(left: String, right: Any) = 
        left.reverse == (right match {
          case s: String => s.toLowerCase
          case other => other
        })
    }
  
  val mapReverseEquality = 
    new Equality[(Int, String)] {
      def areEqual(left: (Int, String), right: Any) = 
        right match {
          case t2: Tuple2[_, _] =>  
            left._1 == t2._1 && 
            left._2.reverse == (t2._2 match {
              case s: String => s.toLowerCase
              case other => other
            })
          case right => left == right
      }
    }
  
  val javaMapReverseEquality = 
    new Equality[java.util.Map.Entry[Int, String]] {
      def areEqual(left: java.util.Map.Entry[Int, String], right: Any) = 
        right match {
          case entry: java.util.Map.Entry[_, _] =>  
            left.getKey == entry.getKey && 
            left.getValue.reverse == (entry.getValue match {
              case s: String => s.toLowerCase
              case other => other
            })
          case right => left == right
      }
    }

  object `theSameElementsAs ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      val rightText = FailureMessages.decorateToStringValue(right)
      e.message should be (Some(leftText + " did not contain the same elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameElementsAsContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      val rightText = FailureMessages.decorateToStringValue(right)
      e.message should be (Some(leftText + " contained the same elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameElementsAsContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take specified normalization when 'should contain' is used` {
      (List("1 ", "2", "3 ") should contain theSameElementsAs List("1", "2 ", "3")) (after being trimmed)
      (Set("1 ", "2", "3 ") should contain theSameElementsAs List("1", "2 ", "3")) (after being trimmed)
      (Array("1 ", "2", "3 ") should contain theSameElementsAs List("1", "2 ", "3")) (after being trimmed)
      (javaList("1 ", "2", "3 ") should contain theSameElementsAs List("1", "2 ", "3")) (after being trimmed)
      (javaSet("1 ", "2", "3 ") should contain theSameElementsAs List("1", "2 ", "3")) (after being trimmed)
        
      (Map(1 -> "one ", 2 -> "two", 3 -> "three ") should contain theSameElementsAs Map(1 -> "one", 2 -> "two ", 3 -> "three")) (after being mapTrimmed)
      (javaMap(Entry(1, "one "), Entry(2, "two"), Entry(3, "three ")) should contain theSameElementsAs List(Entry(1, "one"), Entry(2, "two "), Entry(3, "three"))) (after being javaMapTrimmed)
    }
    
    def `should take specified normalization in scope when 'should not contain' is used` {
      (List(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3))) (after being incremented)
      (Set(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3))) (after being incremented)
      (Array(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3))) (after being incremented)
      (javaList(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3))) (after being incremented)
      (javaSet(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3))) (after being incremented)
      
      (Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three"))) (after being mapIncremented)
      (javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain theSameElementsAs (List(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")))) (after being javaMapIncremented)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified normalization in scope` {
      
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameElementsAs right1) (after being incremented)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameElementsAs right2) (after being incremented)
      }
      checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val right3 = List(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameElementsAs right3) (after being incremented)
      }
        checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val right4 = List(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain theSameElementsAs right4) (after being incremented)
      }
      checkShouldContainStackDepth(e4, left4, right4, thisLineNumber - 2)
        
      
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain theSameElementsAs right5) (after being mapIncremented)
      }
      checkShouldContainStackDepth(e5, left5, right5, thisLineNumber - 2)
      
      val left6 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val right6 = List(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain theSameElementsAs right6) (after being javaMapIncremented)
      }
      checkShouldContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
      
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization in scope` {
      
      val left1 = List("1 ", "2", " 3")
      val right1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain theSameElementsAs (right1)) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set("1 ", "2", " 3")
      val right2 = List("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain theSameElementsAs (right2)) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array("1 ", "2", " 3")
      val right3 = List("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain theSameElementsAs (right3)) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList("1 ", "2", " 3")
      val right4 = List("1", " 2", "3")
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should not contain theSameElementsAs (right4)) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e4, left4, right4, thisLineNumber - 2)
      
      val left5 = Map(1 -> "one ", 2 -> "two", 3 -> " three")
      val right5 = Map(1 -> "one", 2 -> "two ", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should not contain theSameElementsAs (right5)) (after being mapTrimmed)
      }
      checkShouldNotContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = javaMap(Entry(1, "one "), Entry(2, "two"), Entry(3, " three"))
      val right6 = List(Entry(1, "one"), Entry(2, "two "), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should not contain theSameElementsAs (right6)) (after being javaMapTrimmed)
      }
      checkShouldNotContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      
      (List("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Set("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (javaList("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
       
      (Map(1 -> "ONE ", 2 -> "TWO", 3 -> " THREE") should contain theSameElementsAs Map(1 -> "one", 2 -> " two", 3 -> "three")) (decided by mapLowerCaseEquality afterBeing mapTrimmed)
      (javaMap(Entry(1, "ONE "), Entry(2, "TWO"), Entry(3, " THREE")) should contain theSameElementsAs List(Entry(1, "one"), Entry(2, " two"), Entry(3, "three"))) (decided by javaMapLowerCaseEquality afterBeing javaMapTrimmed)
    }
      
    def `should take specified explicit equality and normalization when 'should not contain' is used` {
      
      (List("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three"))) (decided by reverseEquality afterBeing trimmed)
      (Set("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three"))) (decided by reverseEquality afterBeing trimmed)
      (Array("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three"))) (decided by reverseEquality afterBeing trimmed)
      (javaList("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three"))) (decided by reverseEquality afterBeing trimmed)
        
      (Map(1 -> "one ", 2 -> " two", 3 -> "three ") should not contain theSameElementsAs (Map(1 -> " one", 2 -> "two ", 3 -> " three"))) (mapReverseEquality)
      (javaMap(Entry(1, "one "), Entry(2, " two"), Entry(3, "three ")) should not contain theSameElementsAs (List(Entry(1, " one"), Entry(2, "two "), Entry(3, " three")))) (javaMapReverseEquality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified equality and normalization` {
      
      val left1 = List("one ", " two", "three ")
      val right1 = List(" one", "two ", " three")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameElementsAs right1) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set("one ", " two", "three ")
      val right2 = List(" one", "two ", " three")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameElementsAs right2) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array("one ", " two", "three ")
      val right3 = List(" one", "two ", " three")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameElementsAs right3) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList("one ", " two", "three ")
      val right4 = List(" one", "two ", " three")
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain theSameElementsAs right4) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e4, left4, right4, thisLineNumber - 2)
      
      val left5 = Map(1 -> "one ", 2 -> " two", 3 -> "three ")
      val right5 = Map(1 -> " one", 2 -> "two ", 3 -> " three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain theSameElementsAs right5) (decided by mapReverseEquality afterBeing mapTrimmed)
      }
      checkShouldContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = javaMap(Entry(1, "one "), Entry(2, " two"), Entry(3, "three "))
      val right6 = List(Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain theSameElementsAs right6) (decided by javaMapReverseEquality afterBeing javaMapTrimmed)
      }
      checkShouldContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
      
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified equality and normalization` {
      
      val left1 = List("ONE ", " TWO", "THREE ")
      val right1 = List(" one", "two ", " three")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain theSameElementsAs (right1)) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set("ONE ", " TWO", "THREE ")
      val right2 = List(" one", "two ", " three")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain theSameElementsAs (right2)) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array("ONE ", " TWO", "THREE ")
      val right3 = List(" one", "two ", " three")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain theSameElementsAs (right3)) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList("ONE ", " TWO", "THREE ")
      val right4 = List(" one", "two ", " three")
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should not contain theSameElementsAs (right4)) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e4, left4, right4, thisLineNumber - 2)
      
      val left5 = Map(1 -> "ONE ", 2 -> "TWO", 3 -> " THREE ")
      val right5 = Map(1 -> "one", 2 -> " two", 3 -> "three ")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should not contain theSameElementsAs (right5)) (decided by mapLowerCaseEquality afterBeing mapTrimmed)
      }
      checkShouldNotContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = javaMap(Entry(1, "ONE "), Entry(2, "TWO"), Entry(3, " THREE "))
      val right6 = List(Entry(1, "one"), Entry(2, " two"), Entry(3, "three "))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should not contain theSameElementsAs (right6)) (decided by javaMapLowerCaseEquality afterBeing javaMapTrimmed)
      }
      checkShouldNotContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
  }
}
