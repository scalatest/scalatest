package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import collection.GenTraversable

class AllOfContainMatcherEqualitySpec extends Spec with Matchers with Explicitly with SharedHelpers {

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
  
  object `allOf ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain all of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("AllOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained all of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("AllOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      
      implicit val trimEquality = new TrimEquality
      implicit val mapTrimEquality = new MapTrimEquality
      
      List("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3")
      Set("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3")
      Array("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3")
      javaList("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3")
      javaSet("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3")
        
      Map(1 -> "one ", 2 -> "two", 3 -> "three ") should contain allOf (1 -> "one", 2 -> "two ", 3 -> "three")
      javaMap(1 -> "one ", 2 -> "two", 3 -> "three ") should contain allOf (1 -> "one", 2 -> "two ", 3 -> "three")
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      
      implicit val trimEquality = new TrimEquality
      implicit val mapTrimEquality = new MapTrimEquality
      
      List("A ", "B", "C ") should not contain allOf ("a ", "b", "c ")
      Set("A ", "B", "C ") should not contain allOf ("a ", "b", "c ")
      Array("A ", "B", "C ") should not contain allOf ("a ", "b", "c ")
      javaList("A ", "B", "C ") should not contain allOf ("a ", "b", "c ")
      javaSet("A ", "B", "C ") should not contain allOf ("a ", "b", "c ")
      
      Map(1 -> "A ", 2 -> "B", 3 -> "C ") should not contain allOf (1 -> "a ", 2 -> "b", 3 -> "c ")
      javaMap(1 -> "A ", 2 -> "B", 3 -> "C ") should not contain allOf (1 -> "a ", 2 -> "b", 3 -> "c ")
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      
      implicit val falseEquality = new FalseEquality
      implicit val mapFalseEquality = new MapFalseEquality
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain allOf (1, 2, 3)
      }
      checkShouldContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain allOf (1, 2, 3)
      }
      checkShouldContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain allOf (1, 2, 3)
      }
        checkShouldContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allOf (1, 2, 3)
      }
      checkShouldContainStackDepth(e4, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkShouldContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
      
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkShouldContainStackDepth(e6, left6, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {
      
      implicit val trimEquality = new TrimEquality
      implicit val mapTrimEquality = new MapTrimEquality
      
      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain allOf ("1", "2 ", "3")
      }
      checkShouldNotContainStackDepth(e1, left1, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left2 = Set("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain allOf ("1", "2 ", "3")
      }
      checkShouldNotContainStackDepth(e2, left2, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left3 = Array("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain allOf ("1", "2 ", "3")
      }
      checkShouldNotContainStackDepth(e3, left3, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left4 = javaList("1 ", "2", "3 ")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allOf ("1", "2 ", "3")
      }
      checkShouldNotContainStackDepth(e4, left4, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left5 = Map(1 -> "one ", 2 -> "two", 3 -> "three ")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain allOf (1 -> "one", 2 -> "two ", 3 -> "three")
      }
      checkShouldNotContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two ", 3 -> "three").deep, thisLineNumber - 2)
        
      val left6 = javaMap(1 -> "one ", 2 -> "two", 3 -> "three ")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain allOf (1 -> "one", 2 -> "two ", 3 -> "three")
      }
      checkShouldNotContainStackDepth(e6, left6, Array(1 -> "one", 2 -> "two ", 3 -> "three").deep, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      val trimEquality = new TrimEquality
      val mapTrimEquality = new MapTrimEquality
      
      (List("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3 ")) (trimEquality)
      (Set("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3 ")) (trimEquality)
      (Array("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3 ")) (trimEquality)
      (javaList("1 ", "2", "3 ") should contain allOf ("1", "2 ", "3 ")) (trimEquality)
       
      (Map(1 -> "one ", 2 -> "two", 3 -> "three ") should contain allOf (1 -> "one", 2 -> "two ", 3 -> "three")) (mapTrimEquality)
      (javaMap(1 -> "one ", 2 -> "two", 3 -> "three ") should contain allOf (1 -> "one", 2 -> "two ", 3 -> "three")) (mapTrimEquality)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      val equality = new FalseEquality
      List(1, 2, 3) should not contain allOf (1, 2, 3) (equality)
      Set(1, 2, 3) should not contain allOf (1, 2, 3) (equality)
      Array(1, 2, 3) should not contain allOf (1, 2, 3) (equality)
      javaList(1, 2, 3) should not contain allOf (1, 2, 3) (equality)
        
      val mapEquality = new MapFalseEquality
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three") (mapEquality)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three") (mapEquality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      val equality = new FalseEquality
        
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain allOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain allOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain allOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain allOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e4, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val mapEquality = new MapFalseEquality
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")) (mapEquality)
      }
      checkShouldContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
        
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")) (mapEquality)
      }
      checkShouldContainStackDepth(e6, left6, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      val trimEquality = new TrimEquality
        
      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain allOf ("1", "2 ", "3") (trimEquality)
      }
      checkShouldNotContainStackDepth(e1, left1, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left2 = Set("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain allOf ("1", "2 ", "3") (trimEquality)
      }
      checkShouldNotContainStackDepth(e2, left2, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left3 = Array("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain allOf ("1", "2 ", "3") (trimEquality)
      }
      checkShouldNotContainStackDepth(e3, left3, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left4 = javaList("1 ", "2", "3 ")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allOf ("1", "2 ", "3") (trimEquality)
      }
      checkShouldNotContainStackDepth(e4, left4, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val mapTrimEquality = new MapTrimEquality
       
      val left5 = Map(1 -> "one ", 2 -> "two", 3 -> "three ")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain allOf (1 -> "one", 2 -> "two ", 3 -> "three") (mapTrimEquality)
      }
      checkShouldNotContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two ", 3 -> "three").deep, thisLineNumber - 2)
        
      val left6 = javaMap(1 -> "one ", 2 -> "two", 3 -> "three ")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain allOf (1 -> "one", 2 -> "two ", 3 -> "three") (mapTrimEquality)
      }
      checkShouldNotContainStackDepth(e6, left6, Array(1 -> "one", 2 -> "two ", 3 -> "three").deep, thisLineNumber - 2)
    }
  }
  
}