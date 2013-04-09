package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import org.scalautils.StringNormalizations
import org.scalautils.Normalization

class TheSameElementsAsContainMatcherDeciderSpec extends Spec with Matchers with Explicitly with StringNormalizations with SharedHelpers {
  
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

  object `theSameElementsAs ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
      e.message should be (Some(leftText + " did not contain the same elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameElementsAsContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
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
      (javaMap(1 -> "one ", 2 -> "two", 3 -> "three ") should contain theSameElementsAs Map(1 -> "one", 2 -> "two ", 3 -> "three")) (after being mapTrimmed)
    }
    
    def `should take specified normalization in scope when 'should not contain' is used` {
      List(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3)) (after being incremented)
      Set(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3)) (after being incremented)
      Array(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3)) (after being incremented)
      javaList(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3)) (after being incremented)
      javaSet(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 3)) (after being incremented)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three")) (after being mapIncremented)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three")) (after being mapIncremented)
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
      
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val right6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain theSameElementsAs right6) (after being mapIncremented)
      }
      checkShouldContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
      
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization in scope` {
      
      val left1 = List("1 ", "2", " 3")
      val right1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameElementsAs (right1) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set("1 ", "2", " 3")
      val right2 = List("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameElementsAs (right2) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array("1 ", "2", " 3")
      val right3 = List("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameElementsAs (right3) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList("1 ", "2", " 3")
      val right4 = List("1", " 2", "3")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain theSameElementsAs (right4) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e4, left4, right4, thisLineNumber - 2)
      
      val left5 = Map(1 -> "one ", 2 -> "two", 3 -> " three")
      val right5 = Map(1 -> "one", 2 -> "two ", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain theSameElementsAs (right5) (after being mapTrimmed)
      }
      checkShouldNotContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = javaMap(1 -> "one ", 2 -> "two", 3 -> " three")
      val right6 = Map(1 -> "one", 2 -> "two ", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain theSameElementsAs (right6) (after being mapTrimmed)
      }
      checkShouldNotContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      
      (List("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Set("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (javaList("A ", "B", " C") should contain theSameElementsAs List("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
       
      (Map(1 -> "ONE ", 2 -> "TWO", 3 -> " THREE") should contain theSameElementsAs Map(1 -> "one", 2 -> " two", 3 -> "three")) (decided by mapLowerCaseEquality afterBeing mapTrimmed)
      (javaMap(1 -> "ONE ", 2 -> "TWO", 3 -> " THREE") should contain theSameElementsAs Map(1 -> "one", 2 -> " two", 3 -> "three")) (decided by mapLowerCaseEquality afterBeing mapTrimmed)
    }
      
    def `should take specified explicit equality and normalization when 'should not contain' is used` {
      
      List("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      Set("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      Array("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      javaList("one ", " two", "three ") should not contain theSameElementsAs (List(" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
        
      Map(1 -> "one ", 2 -> " two", 3 -> "three ") should not contain theSameElementsAs (Map(1 -> " one", 2 -> "two ", 3 -> " three")) (mapReverseEquality)
      javaMap(1 -> "one ", 2 -> " two", 3 -> "three ") should not contain theSameElementsAs (Map(1 -> " one", 2 -> "two ", 3 -> " three")) (mapReverseEquality)
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
        
      val left6 = javaMap(1 -> "one ", 2 -> " two", 3 -> "three ")
      val right6 = Map(1 -> " one", 2 -> "two ", 3 -> " three")
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain theSameElementsAs right6) (decided by mapReverseEquality afterBeing mapTrimmed)
      }
      checkShouldContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
      
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified equality and normalization` {
      
      val left1 = List("ONE ", " TWO", "THREE ")
      val right1 = List(" one", "two ", " three")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameElementsAs (right1) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set("ONE ", " TWO", "THREE ")
      val right2 = List(" one", "two ", " three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameElementsAs (right2) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array("ONE ", " TWO", "THREE ")
      val right3 = List(" one", "two ", " three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameElementsAs (right3) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList("ONE ", " TWO", "THREE ")
      val right4 = List(" one", "two ", " three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain theSameElementsAs (right4) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e4, left4, right4, thisLineNumber - 2)
      
      val left5 = Map(1 -> "ONE ", 2 -> "TWO", 3 -> " THREE ")
      val right5 = Map(1 -> "one", 2 -> " two", 3 -> "three ")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain theSameElementsAs (right5) (decided by mapLowerCaseEquality afterBeing mapTrimmed)
      }
      checkShouldNotContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = javaMap(1 -> "ONE ", 2 -> "TWO", 3 -> " THREE ")
      val right6 = Map(1 -> "one", 2 -> " two", 3 -> "three ")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain theSameElementsAs (right6) (decided by mapLowerCaseEquality afterBeing mapTrimmed)
      }
      checkShouldNotContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
    
  }
  
}