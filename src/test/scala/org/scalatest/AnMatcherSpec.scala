package org.scalatest

import org.scalatest._
import matchers._

class AnMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `AnMatcher ` {
    
    object `when used with integer` {
      
      val oddNumber = AnMatcher[Int]("odd number") { _ % 2 != 0 }
      
      def `should work correctly with 'should be a'` {
        1 should be an oddNumber
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          0 should be an oddNumber
        }
        e.message should be (Some("0 was not an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be a'` {
        0 should not be an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          1 should not be an (oddNumber)
        }
        e.message should be (Some("1 was an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with string` {
      
      val oddLengthString = AnMatcher[String]("odd length string") { _.length % 2 != 0 }
      
      def `should work correctly with 'should be'` {
        "hello" should be an oddLengthString
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          "" should be an oddLengthString
        }
        e.message should be (Some("\"\" was not an odd length string"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        "" should not be an (oddLengthString)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          "hai" should not be an (oddLengthString)
        }
        e.message should be (Some("\"hai\" was an odd length string"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with list` {

      val oddLengthList = AnMatcher[List[Int]]("odd length list") { _.length % 2 != 0 }
      
      def `should work correctly with 'should be'` {
        List(1, 2, 3) should be an oddLengthList
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List.empty[Int] should be an oddLengthList
        }
        e.message should be (Some(List.empty[Int] + " was not an odd length list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        List.empty[Int] should not be an (oddLengthList)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 3) should not be an (oddLengthList)
        }
        e.message should be (Some(List(1, 2, 3) + " was an odd length list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val oddNumber = AnMatcher[Int]("odd number") { _ % 2 != 0 }
      
      def `should work correctly with 'should contain'` {
        List(2, 5, 8) should contain an oddNumber
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(2, 6, 8) should contain an oddNumber
        }
        e.message should be (Some(List(2, 6, 8) + " did not contain an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        List(2, 6, 8) should not contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 6) should not contain an (oddNumber)
        }
        e.message should be (Some(List(1, 2, 6) + " contained an odd number: 1 was an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with set` {

      val oddSizeSet = AnMatcher[Set[Int]]("odd size set") { _.size % 2 != 0 }
      
      def `should work correctly with 'should be'` {
        Set(1, 2, 3) should be an oddSizeSet
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set.empty[Int] should be an oddSizeSet
        }
        e.message should be (Some(Set.empty[Int] + " was not an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Set.empty[Int] should not be an (oddSizeSet)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(1, 2, 3) should not be an (oddSizeSet)
        }
        e.message should be (Some(Set(1, 2, 3) + " was an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val oddNumber = AnMatcher[Int]("odd number") { _ % 2 != 0 }
      
      def `should work correctly with 'should contain'` {
        Set(2, 5, 8) should contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(2, 6, 8) should contain an oddNumber
        }
        e.message should be (Some(Set(2, 6, 8) + " did not contain an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        Set(2, 6, 8) should not contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(1, 2, 6) should not contain an (oddNumber)
        }
        e.message should be (Some(Set(1, 2, 6) + " contained an odd number: 1 was an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with array` {

      val oddSizeArray = AnMatcher[Array[Int]]("odd size array") { _.size % 2 != 0 }
      
      def `should work correctly with 'should be'` {
        Array(1, 2, 3) should be an oddSizeArray
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array.empty[Int] should be an oddSizeArray
        }
        e.message should be (Some("Array() was not an odd size array"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Array.empty[Int] should not be an (oddSizeArray)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(1, 2, 3) should not be an (oddSizeArray)
        }
        e.message should be (Some("Array(1, 2, 3) was an odd size array"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val oddNumber = AnMatcher[Int]("odd number") { _ % 2 != 0 }
      
      def `should work correctly with 'should contain'` {
        Array(2, 5, 8) should contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(2, 6, 8) should contain an oddNumber
        }
        e.message should be (Some("Array(2, 6, 8) did not contain an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        Array(2, 6, 8) should not contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(1, 2, 6) should not contain an (oddNumber)
        }
        e.message should be (Some("Array(1, 2, 6) contained an odd number: 1 was an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    
    object `when used with map` {

      val oddSizeMap = AnMatcher[Map[Int, String]]("odd size map") { _.size % 2 != 0 }
      
      def `should work correctly with 'should be'` {
        Map(1 -> "one", 2 -> "two", 3 -> "three") should be an oddSizeMap
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map.empty[Int, String] should be an oddSizeMap
        }
        e.message should be (Some(Map.empty[Int, String] + " was not an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Map.empty[Int, String] should not be an (oddSizeMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(1 -> "one", 2 -> "two", 3 -> "three") should not be an (oddSizeMap)
        }
        e.message should be (Some(Map(1 -> "one", 2 -> "two", 3 -> "three") + " was an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val oddNumberKey = AnMatcher[(Int, String)]("odd number key") { _._1 % 2 != 0 }
      
      def `should work correctly with 'should contain'` {
        Map(-2 -> "minus two", 5 -> "five", -8 -> "minus eight") should contain an (oddNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") should contain an oddNumberKey
        }
        e.message should be (Some(Map(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") + " did not contain an odd number key"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        Map(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") should not contain an (oddNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(1 -> "one", 2 -> "two", -6 -> "minus six") should not contain an (oddNumberKey)
        }
        e.message should be (Some(Map(1 -> "one", 2 -> "two", -6 -> "minus six") + " contained an odd number key: (1,one) was an odd number key"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with java list` {

      val oddSizeList = AnMatcher[java.util.List[Int]]("odd size list") { _.size % 2 != 0 }
      
      def javaList(values: Int*): java.util.List[Int] = {
        val javaList = new java.util.ArrayList[Int]()
        values.foreach(javaList.add(_))
        javaList
      }
      
      def `should work correctly with 'should be'` {
        javaList(1, 2, 3) should be an oddSizeList
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val left = javaList()
        val e = intercept[exceptions.TestFailedException] {
          left should be an oddSizeList
        }
        e.message should be (Some(left  + " was not an odd size list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        javaList() should not be an (oddSizeList)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val left = javaList(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be an (oddSizeList)
        }
        e.message should be (Some(left + " was an odd size list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val oddNumber = AnMatcher[Int]("odd number") { _ % 2 != 0 }
      
      def `should work correctly with 'should contain'` {
        javaList(-2, 5, -8) should contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = javaList(-2, -6, -8)
        val e = intercept[exceptions.TestFailedException] {
          left should contain an oddNumber
        }
        e.message should be (Some(left + " did not contain an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        javaList(-2, -6, -8) should not contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val left = javaList(-1, 2, -6)
        val e = intercept[exceptions.TestFailedException] {
          left should not contain an (oddNumber)
        }
        e.message should be (Some(left + " contained an odd number: -1 was an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with java set` {

      val oddSizeSet = AnMatcher[java.util.Set[Int]]("odd size set") { _.size % 2 != 0 }
      
      def javaSet(values: Int*): java.util.Set[Int] = {
        val javaSet = new java.util.HashSet[Int]()
        values.foreach(javaSet.add(_))
        javaSet
      }
      
      def `should work correctly with 'should be'` {
        javaSet(1, 2, 3) should be an oddSizeSet
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val left = javaSet()
        val e = intercept[exceptions.TestFailedException] {
          left should be an oddSizeSet
        }
        e.message should be (Some(left  + " was not an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        javaSet() should not be an (oddSizeSet)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val left = javaSet(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be an (oddSizeSet)
        }
        e.message should be (Some(left + " was an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val oddNumber = AnMatcher[Int]("odd number") { _ % 2 != 0 }
      
      def `should work correctly with 'should contain'` {
        javaSet(-2, 5, -8) should contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = javaSet(-2, -6, -8)
        val e = intercept[exceptions.TestFailedException] {
          left should contain an oddNumber
        }
        e.message should be (Some(left + " did not contain an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        javaSet(-2, -6, -8) should not contain an (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val left = javaSet(-1, 2, -6)
        val e = intercept[exceptions.TestFailedException] {
          left should not contain an (oddNumber)
        }
        e.message should be (Some(left + " contained an odd number: -1 was an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with java map` {
      
      def javaMap(values: (Int, String)*): java.util.Map[Int, String] = {
        val javaMap = new java.util.LinkedHashMap[Int, String]()
        values.foreach(e => javaMap.put(e._1, e._2))
        javaMap
      }

      val oddSizeMap = AnMatcher[java.util.Map[Int, String]]("odd size map") { _.size  % 2 != 0 }
      
      def `should work correctly with 'should be'` {
        javaMap(1 -> "one", 2 -> "two", 3 -> "three") should be an oddSizeMap
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val left = javaMap()
        val e = intercept[exceptions.TestFailedException] {
          left should be an oddSizeMap
        }
        e.message should be (Some(left + " was not an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        javaMap() should not be an (oddSizeMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val left = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
        val e = intercept[exceptions.TestFailedException] {
          left should not be an (oddSizeMap)
        }
        e.message should be (Some(left + " was an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val oddNumberKey = AnMatcher[(Int, String)]("odd number key") { _._1  % 2 != 0 }
      
      def `should work correctly with 'should contain'` {
        javaMap(-2 -> "minus two", 5 -> "five", -8 -> "minus eight") should contain an (oddNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = javaMap(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight")
        val e = intercept[exceptions.TestFailedException] {
          left should contain an oddNumberKey
        }
        e.message should be (Some(left + " did not contain an odd number key"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        javaMap(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") should not contain an (oddNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val left = javaMap(1 -> "one", 2 -> "two", -6 -> "minus six")
        val e = intercept[exceptions.TestFailedException] {
          left should not contain an (oddNumberKey)
        }
        e.message should be (Some(left + " contained an odd number key: (1,one) was an odd number key"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with custom object` {

      case class Person(name: String, age: Int)
      
      val oldMan = AnMatcher[Person]("old man") { _.age > 59 }
      
      def `should work correctly with 'should be'` {
        Person("Tom", 60) should be an oldMan
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val tom = Person("Tom", 30)
        val e = intercept[exceptions.TestFailedException] {
          tom should be an oldMan
        }
        e.message should be (Some(tom + " was not an old man"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Person("Tom", 30) should not be an (oldMan)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val tom = Person("Tom", 60)
        val e = intercept[exceptions.TestFailedException] {
          tom should not be an (oldMan)
        }
        e.message should be (Some(tom + " was an old man"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should contain'` {
        List(Person("Tom", 30), Person("Donald", 60), Person("Jerry", 35)) should contain an oldMan
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = List(Person("Tom", 30), Person("Donald", 40), Person("Jerry", 35))
        val e = intercept[exceptions.TestFailedException] {
          left should contain an oldMan
        }
        e.message should be (Some(left + " did not contain an old man"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        List(Person("Tom", 30), Person("Donald", 40), Person("Jerry", 35)) should not contain an (oldMan)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val donald = Person("Donald", 60)
        val left = List(Person("Tom", 30), donald, Person("Jerry", 35))
        val e = intercept[exceptions.TestFailedException] {
          left should not contain an (oldMan)
        }
        e.message should be (Some(left + " contained an old man: " + donald + " was an old man"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
  }
  
}