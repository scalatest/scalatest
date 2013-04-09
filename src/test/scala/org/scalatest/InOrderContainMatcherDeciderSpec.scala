package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import org.scalautils.StringNormalizations
import org.scalautils.Normalization
import collection.GenTraversable

class InOrderContainMatcherDeciderSpec extends Spec with Matchers with Explicitly with StringNormalizations with SharedHelpers {

  val mapTrimmed: Normalization[(Int, String)] =
    new Normalization[(Int, String)] {

      def isInstanceOfA(b: Any) = b.isInstanceOf[(Int, String)]

      def normalized(s: (Int, String)): (Int, String) = (s._1, s._2.trim)
    }
  
  val incremented: Normalization[Int] = 
    new Normalization[Int] {
      var count = 0
      def isInstanceOfA(b: Any) = b.isInstanceOf[Int]
    
      def normalized(s: Int): Int = {
        count += 1
        s + count
      }
    }
  
  val mapIncremented: Normalization[(Int, String)] = 
    new Normalization[(Int, String)] {
      var count = 0
      def isInstanceOfA(b: Any) = b.isInstanceOf[(Int, String)]
    
      def normalized(s: (Int, String)): (Int, String) = {
        count += 1
        (s._1 + count, s._2)
      }
    }
  
  val appended: Normalization[String] = 
    new Normalization[String] {
      var count = 0
      def isInstanceOfA(b: Any) = b.isInstanceOf[String]
    
      def normalized(s: String): String = {
        count += 1
        s + count
      }
    }
  
  class Translated(map: Map[String, String]) extends Normalization[String] {
    def isInstanceOfA(b: Any) = b.isInstanceOf[String]
    def normalized(s: String): String = 
      map.get(s) match {
        case Some(translated) => translated
        case None => s
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
  
  val reverseEquality = 
    new Equality[String] {
      def areEqual(left: String, right: Any) = 
        left.reverse == (right match {
          case s: String => s.toLowerCase
          case other => other
        })
    }
  
  object `inOrder ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain all of (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained all of (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take specified equality when 'should contain' is used` {
      (List("1 ", "2", "3 ") should contain inOrder ("1", "2 ", "3")) (after being trimmed)
      (Array("1 ", "2", "3 ") should contain inOrder ("1", "2 ", "3")) (after being trimmed)
      (javaList("1", "2 ", "3") should contain inOrder ("1", "2 ", "3")) (after being trimmed)
    }
    
    def `should take specified equality when 'should not contain' is used` {
      List("1 ", "2", "3 ") should not contain inOrder ("1", "2 ", "3") (after being appended)
      Array("1 ", "2", "3 ") should not contain inOrder ("1", "2 ", "3") (after being appended)
      javaList("1 ", "2", "3 ") should not contain inOrder ("1", "2 ", "3") (after being appended)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified normalization` {
      
      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrder ("1", "2 ", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e1, left1, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrder ("1", "2 ", "3")) (after being appended)
      }
        checkShouldContainStackDepth(e2, left2, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrder ("1", "2 ", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e3, left3, Array("1", "2 ", "3").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization` {
      val translated = new Translated(Map("eno" -> "one"))
      
      val left1 = List("one", "two", "three")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrder ("eno", "two", "three") (after being translated)
      }
      checkShouldNotContainStackDepth(e1, left1, Array("eno", "two", "three").deep, thisLineNumber - 2)
        
      val left2 = Array("one", "two", "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrder ("eno", "two", "three") (after being translated)
      }
      checkShouldNotContainStackDepth(e2, left2, Array("eno", "two", "three").deep, thisLineNumber - 2)
        
      val left3 = javaList("one", "two", "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrder ("eno", "two", "three") (after being translated)
      }
      checkShouldNotContainStackDepth(e3, left3, Array("eno", "two", "three").deep, thisLineNumber - 2)
    }
    
    def `should take specified equality and normalization when 'should contain' is used` {
      (List("A ", "B", "C ") should contain inOrder ("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("A ", "B", "C ") should contain inOrder ("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (javaList("A ", "B", "C ") should contain inOrder ("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
    }
    
    def `should take specified equality and normalization when 'should not contain' is used` {
      List("one ", "two", "three ") should not contain inOrder ("one", "two ", "three") (decided by reverseEquality afterBeing trimmed)
      Array("one ", "two", "three ") should not contain inOrder ("one", "two ", "three") (decided by reverseEquality afterBeing trimmed)
      javaList("one ", "two", "three ") should not contain inOrder ("one", "two ", "three") (decided by reverseEquality afterBeing trimmed)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified equality and normalization` {
      
      val left1 = List("one ", "two", "three ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrder ("one", "two ", "three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e1, left1, Array("one", "two ", "three").deep, thisLineNumber - 2)
        
      val left2 = Array("one ", "two", "three ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrder ("one", "two ", "three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e2, left2, Array("one", "two ", "three").deep, thisLineNumber - 2)
        
      val left3 = javaList("one ", "two", "three ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrder ("one", "two ", "three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e3, left3, Array("one", "two ", "three").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified equality and normalization` {
      
      val left1 = List("one ", "two", "three ")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrder ("eno ", "owt", "eerht ") (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, Array("eno ", "owt", "eerht ").deep, thisLineNumber - 2)
        
      val left2 = Array("one ", "two", "three ")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrder ("eno ", "owt", "eerht ") (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, Array("eno ", "owt", "eerht ").deep, thisLineNumber - 2)
        
      val left3 = javaList("one ", "two", "three ")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrder ("eno ", "owt", "eerht ") (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, Array("eno ", "owt", "eerht ").deep, thisLineNumber - 2)
    }
    
  }
  
}