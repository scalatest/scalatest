package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import collection.GenTraversable

class InOrderOnlyContainMatcherEqualitySpec extends Spec with Matchers with Explicitly with SharedHelpers {

  class TrimEquality extends Equality[String] {
    def areEqual(left: String, right: Any) = 
      left.trim == (right match {
        case s: String => s.trim
        case other => other
      })
  }
  
  class MapTrimEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any) = 
      right match {
        case t2: Tuple2[_, _] =>  
          left._1 == t2._1 && 
          left._2.trim == (t2._2 match {
            case s: String => s.trim
            case other => other
          })
        case right => left == right
      }
  }
  
  class FalseEquality extends Equality[Int] {
    def areEqual(left: Int, right: Any): Boolean = false
  }
  
  class MapFalseEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any): Boolean = false
  }
  
  object `inOrderOnly ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      implicit val equality = new TrimEquality
      List("1", " 2", "3") should contain inOrderOnly (" 1", "2 ", " 3")
      Array("1", " 2", "3") should contain inOrderOnly (" 1", "2 ", " 3")
      javaList("1", " 2", "3") should contain inOrderOnly (" 1", "2 ", " 3")
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain inOrderOnly (1, 2, 3)
      Array(1, 2, 3) should not contain inOrderOnly (1, 2, 3)
      javaList(1, 2, 3) should not contain inOrderOnly (1, 2, 3)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new FalseEquality
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrderOnly (1, 2, 3)
      }
      checkShouldContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrderOnly (1, 2, 3)
      }
      checkShouldContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = javaList(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrderOnly (1, 2, 3)
      }
      checkShouldContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new TrimEquality
        
      val left1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrderOnly (" 1", "2 ", " 3")
      }
      checkShouldNotContainStackDepth(e1, left1, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
        
      val left2 = Array("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrderOnly (" 1", "2 ", " 3")
      }
      checkShouldNotContainStackDepth(e2, left2, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
        
      val left3 = javaList("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrderOnly (" 1", "2 ", " 3")
      }
      checkShouldNotContainStackDepth(e3, left3, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      implicit val equality = new TrimEquality
      (List("1 ", " 2", "3 ") should contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      (Array("1 ", " 2", "3 ") should contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      (javaList("1 ", " 2", "3 ") should contain inOrderOnly (" 1", "2 ", " 3")) (equality)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain inOrderOnly (1, 2, 3) (equality)
      Array(1, 2, 3) should not contain inOrderOnly (1, 2, 3) (equality)
      javaList(1, 2, 3) should not contain inOrderOnly (1, 2, 3) (equality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new FalseEquality
        
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrderOnly (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrderOnly (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = javaList(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrderOnly (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new TrimEquality
        
      val left1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrderOnly (" 1", "2 ", " 3") (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
        
      val left2 = Array("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrderOnly (" 1", "2 ", " 3") (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
        
      val left3 = javaList("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrderOnly (" 1", "2 ", " 3") (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
    }
  }
  
}