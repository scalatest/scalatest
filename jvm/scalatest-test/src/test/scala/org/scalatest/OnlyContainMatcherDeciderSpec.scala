/*
 * Copyright 2001-2025 Artima, Inc.
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
import org.scalactic.Prettifier
import org.scalactic.ColCompatHelper.Iterable
import SharedHelpers._

import org.scalactic.ArrayHelper.deep
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class OnlyContainMatcherDeciderSpec extends AnyFunSpec with Explicitly {

  private val prettifier = Prettifier.default

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

  // SKIP-SCALATESTJS,NATIVE-START
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
  // SKIP-SCALATESTJS,NATIVE-END
  
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
  
  val appended: Uniformity[String] = 
    new Uniformity[String] {
      var count = 0
      def normalized(s: String): String = {
        count += 1
        s + count
      }
      def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
      def normalizedOrSame(b: Any) =
        b match {
          case s: String => normalized(s)
          case _ => b
        }
    }
  
  val mapAppended: Uniformity[(Int, String)] = 
    new Uniformity[(Int, String)] {
      var count = 0
      def normalized(s: (Int, String)): (Int, String) = {
        count += 1
        (s._1, s._2 + count)
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

  // SKIP-SCALATESTJS,NATIVE-START
  val javaMapAppended: Uniformity[java.util.Map.Entry[Int, String]] = 
    new Uniformity[java.util.Map.Entry[Int, String]] {
      var count = 0
      def normalized(s: java.util.Map.Entry[Int, String]): java.util.Map.Entry[Int, String] = {
        count += 1
        Entry(s.getKey, s.getValue + count)
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
  // SKIP-SCALATESTJS,NATIVE-END
  
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

  // SKIP-SCALATESTJS,NATIVE-START
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
  // SKIP-SCALATESTJS,NATIVE-END
  
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

  // SKIP-SCALATESTJS,NATIVE-START
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
  // SKIP-SCALATESTJS,NATIVE-END
  
  describe("only ") {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " did not contain only (" + right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OnlyContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " contained only (" + right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OnlyContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should take specified normalization when 'should contain' is used") {
      
      (List("1", " 2", "3") should contain only (" 1", "2 ", " 3")) (after being trimmed)
      (Set("1", " 2", "3") should contain only (" 1", "2 ", " 3")) (after being trimmed)
      (Array("1", " 2", "3") should contain only (" 1", "2 ", " 3")) (after being trimmed)
      (Map(1 -> "one", 2 -> " two", 3 -> "three") should contain only (1 -> " one", 2 -> "two ", 3 -> " three")) (after being mapTrimmed)

      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("1", " 2", "3") should contain only (" 1", "2 ", " 3")) (after being trimmed)
      (javaSet("1", " 2", "3") should contain only (" 1", "2 ", " 3")) (after being trimmed)
      (javaMap(Entry(1, "one"), Entry(2, " two"), Entry(3, "three")) should contain only (Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))) (after being javaMapTrimmed)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should take specified normalization when 'should not contain' is used") {
      
      (List("1", "2", "3") should not contain only ("1", "2", "3")) (/* DOTTY-ONLY using */ after being appended)
      (Set("1", "2", "3") should not contain only ("1", "2", "3")) (/* DOTTY-ONLY using */ after being appended)
      (Array("1", "2", "3") should not contain only ("1", "2", "3")) (/* DOTTY-ONLY using */ after being appended)
      (Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain only (1 -> "one", 2 -> "two", 3 -> "three")) (/* DOTTY-ONLY using */ after being mapAppended)

      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("1", "2", "3") should not contain only ("1", "2", "3")) (/* DOTTY-ONLY using */ after being appended)
      (javaSet("1", "2", "3") should not contain only ("1", "2", "3")) (/* DOTTY-ONLY using */ after being appended)
      (javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain only (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))) (/* DOTTY-ONLY using */ after being javaMapAppended)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified normalization") {
      
      val left1 = List("1", "2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain only ("1", "2", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e1, left1, deep(Array("1", "2", "3")), thisLineNumber - 2)
        
      val left2 = Set("1", "2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain only ("1", "2", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e2, left2, deep(Array("1", "2", "3")), thisLineNumber - 2)
        
      val left3 = Array("1", "2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain only ("1", "2", "3")) (after being appended)
      }
        checkShouldContainStackDepth(e3, left3, deep(Array("1", "2", "3")), thisLineNumber - 2)
       
      val left4 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain only (1 -> "one", 2 -> "two", 3 -> "three")) (after being mapAppended)
      }
      checkShouldContainStackDepth(e4, left4, deep(Array(1 -> "one", 2 -> "two", 3 -> "three")), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left5 = javaList("1", "2", "3")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain only ("1", "2", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e5, left5, deep(Array("1", "2", "3")), thisLineNumber - 2)

      val left6 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain only (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))) (after being javaMapAppended)
      }
      checkShouldContainStackDepth(e6, left6, deep(Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization") {
      
      val left1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain only (" 1", "2 ", " 3")) (/* DOTTY-ONLY using */ after being trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)
        
      val left2 = Set("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain only (" 1", "2 ", " 3")) (/* DOTTY-ONLY using */ after being trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)
        
      val left3 = Array("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain only (" 1", "2 ", " 3")) (/* DOTTY-ONLY using */ after being trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)
      
      val left4 = Map(1 -> "one", 2 -> " two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should not contain only (1 -> " one", 2 -> "two ", 3 -> " three")) (/* DOTTY-ONLY using */ after being mapTrimmed)
      }
      checkShouldNotContainStackDepth(e4, left4, deep(Array(1 -> " one", 2 -> "two ", 3 -> " three")), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left5 = javaList("1", " 2", "3")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should not contain only (" 1", "2 ", " 3")) (/* DOTTY-ONLY using */ after being trimmed)
      }
      checkShouldNotContainStackDepth(e5, left5, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)

      val left6 = javaMap(Entry(1, "one"), Entry(2, " two"), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should not contain only (Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))) (/* DOTTY-ONLY using */ after being javaMapTrimmed)
      }
      checkShouldNotContainStackDepth(e6, left6, deep(Array(Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should take specified equality and normalization when 'should contain' is used") {
      
      (List("ONE ", " TWO", "THREE ") should contain only (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      (Set("ONE ", " TWO", "THREE ") should contain only (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("ONE ", " TWO", "THREE ") should contain only (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      (Map(1 -> "ONE ", 2 -> " TWO", 3 -> "THREE ") should contain only (1 -> " one", 2 -> "two ", 3 -> " three")) (decided by mapLowerCaseEquality afterBeing mapTrimmed)

      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("ONE ", " TWO", "THREE ") should contain only (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      (javaMap(Entry(1, "ONE "), Entry(2, " TWO"), Entry(3, "THREE ")) should contain only (Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))) (decided by javaMapLowerCaseEquality afterBeing javaMapTrimmed)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should take specified equality and normalization when 'should not contain' is used") {
      
      (List("one ", " two", "three ") should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by reverseEquality afterBeing trimmed)
      (Set("one ", " two", "three ") should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by reverseEquality afterBeing trimmed)
      (Array("one ", " two", "three ") should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by reverseEquality afterBeing trimmed)
      (Map(1 -> "one ", 2 -> " two", 3 -> "three ") should not contain only (1 -> " one", 2 -> "two ", 3 -> " three")) (/* DOTTY-ONLY using */ decided by mapReverseEquality afterBeing mapTrimmed)

      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("one ", " two", "three ") should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by reverseEquality afterBeing trimmed)
      (javaMap(Entry(1, "one "), Entry(2, " two"), Entry(3, "three ")) should not contain only (Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))) (/* DOTTY-ONLY using */ decided by javaMapReverseEquality afterBeing javaMapTrimmed)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified equality and normalization") {
      
      val left1 = List("one ", " two", "three ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain only (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e1, left1, deep(Array(" one", "two ", " three")), thisLineNumber - 2)
        
      val left2 = Set("one ", " two", "three ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain only (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e2, left2, deep(Array(" one", "two ", " three")), thisLineNumber - 2)
        
      val left3 = Array("one ", " two", "three ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain only (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e3, left3, deep(Array(" one", "two ", " three")), thisLineNumber - 2)
        
      val left4 = Map(1 -> "one ", 2 -> " two", 3 -> "three ")
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain only (1 -> " one", 2 -> "two ", 3 -> " three")) (decided by mapReverseEquality afterBeing mapTrimmed)
      }
      checkShouldContainStackDepth(e4, left4, deep(Array(1 -> " one", 2 -> "two ", 3 -> " three")), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left5 = javaList("one ", " two", "three ")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain only (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e5, left5, deep(Array(" one", "two ", " three")), thisLineNumber - 2)

      val left6 = javaMap(Entry(1, "one "), Entry(2, " two"), Entry(3, "three "))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain only (Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))) (decided by javaMapReverseEquality afterBeing javaMapTrimmed)
      }
      checkShouldContainStackDepth(e6, left6, deep(Array(Entry(1, " one"), Entry(2, "two "), Entry(3, " three"))), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified equality and normalization") {
      
      val left1 = List("ONE ", " TWO", "THREE ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, deep(Array(" one", "two ", " three")), thisLineNumber - 2)
        
      val left2 = Set("ONE ", " TWO", "THREE ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, deep(Array(" one", "two ", " three")), thisLineNumber - 2)
        
      val left3 = Array("ONE ", " TWO", "THREE ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, deep(Array(" one", "two ", " three")), thisLineNumber - 2)
       
      val left4 = Map(1 -> "ONE ", 2 -> " TWO", 3 -> "THREE ")
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should not contain only (1 -> " one ", 2 -> "two ", 3 -> " three")) (/* DOTTY-ONLY using */ decided by mapLowerCaseEquality afterBeing mapTrimmed)
      }
      checkShouldNotContainStackDepth(e4, left4, deep(Array(1 -> " one ", 2 -> "two ", 3 -> " three")), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left5 = javaList("ONE ", " TWO", "THREE ")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should not contain only (" one", "two ", " three")) (/* DOTTY-ONLY using */ decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e5, left5, deep(Array(" one", "two ", " three")), thisLineNumber - 2)

      val left6 = javaMap(Entry(1, "ONE "), Entry(2, " TWO"), Entry(3, "THREE "))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should not contain only (Entry(1, " one "), Entry(2, "two "), Entry(3, " three"))) (/* DOTTY-ONLY using */ decided by javaMapLowerCaseEquality afterBeing javaMapTrimmed)
      }
      checkShouldNotContainStackDepth(e6, left6, deep(Array(Entry(1, " one "), Entry(2, "two "), Entry(3, " three"))), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}
