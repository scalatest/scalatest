package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import org.scalautils.StringNormalizations
import org.scalautils.Normalization

class TheSameIteratedElementsAsContainMatcherDeciderSpec extends Spec with Matchers with Explicitly with StringNormalizations with SharedHelpers {
  
  val incremented: Normalization[Int] = 
    new Normalization[Int] {
      var count = 0
      def isInstanceOfA(b: Any) = b.isInstanceOf[Int]
    
      def normalized(s: Int): Int = {
        count += 1
        s + count
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
  
  object `theSameIteratedElementsAs ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
      e.message should be (Some(leftText + " did not contain the same iterated elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
      e.message should be (Some(leftText + " contained the same iterated elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take specified normalization when 'should contain' is used` {
      (List("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")) (after being trimmed)
      (Array("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")) (after being trimmed)
      (javaList("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")) (after being trimmed)
    }
    
    def `should take specified normalization when 'should not contain' is used` {
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (after being incremented)
      Array(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (after being incremented)
      javaList(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (after being incremented)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified normalization` {
      
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameIteratedElementsAs right1) (after being incremented)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameIteratedElementsAs right2) (after being incremented)
      }
        checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList(1, 2, 3)
      val right3 = List(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameIteratedElementsAs right3) (after being incremented)
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization` {
      
      val left1 = List("1 ", " 2", "3 ")
      val right1 = List(" 1", "2 ", " 3")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs (right1) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("1 ", " 2", "3 ")
      val right2 = List(" 1", "2 ", " 3")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs (right2) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("1 ", " 2", "3 ")
      val right3 = List(" 1", "2 ", " 3")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameIteratedElementsAs (right3) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      (List("A ", " B", "C ") should contain theSameIteratedElementsAs List(" a", "b ", " c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("A ", " B", "C ") should contain theSameIteratedElementsAs List(" a", "b ", " c")) (decided by lowerCaseEquality afterBeing trimmed)
      (javaList("A ", " B", "C ") should contain theSameIteratedElementsAs List(" a", "b ", " c")) (decided by lowerCaseEquality afterBeing trimmed)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      List("one ", " two", "three ") should not contain theSameIteratedElementsAs (List(" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      Array("one ", " two", "three ") should not contain theSameIteratedElementsAs (List(" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      javaList("one ", " two", "three ") should not contain theSameIteratedElementsAs (List(" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      
      val left1 = List("one ", " two", "three ")
      val right1 = List(" one", "two ", " three")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameIteratedElementsAs right1) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("one ", " two", "three ")
      val right2 = List(" one", "two ", " three")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameIteratedElementsAs right2) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("one ", " two", "three ")
      val right3 = List(" one", "two ", " three")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameIteratedElementsAs right3) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      
      val left1 = List("ONE ", " TWO", "THREE ")
      val right1 = List("one", "two ", " three")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs (right1) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("ONE ", " TWO", "THREE ")
      val right2 = List("one", "two ", " three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs (right2)  (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("ONE ", " TWO", "THREE ")
      val right3 = List("one", "two ", " three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameIteratedElementsAs (right3)  (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
  }
  
}