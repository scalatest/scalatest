package org.scalatest

import org.scalatest._
import matchers._

class AMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `AMatcher ` {
    
    object `when used with integer` {
      
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0 }
      
      def `should work correctly with 'should be a'` {
        1 should be a positiveNumber
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          0 should be a positiveNumber
        }
        e.message should be (Some("0 was not a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be a'` {
        0 should not be a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          1 should not be a (positiveNumber)
        }
        e.message should be (Some("1 was a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with string` {
      
      val positiveLengthString = AMatcher[String]("positive length string") { _.length > 0 }
      
      def `should work correctly with 'should be'` {
        "hello" should be a positiveLengthString
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          "" should be a positiveLengthString
        }
        e.message should be (Some("\"\" was not a positive length string"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        "" should not be a (positiveLengthString)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          "hai" should not be a (positiveLengthString)
        }
        e.message should be (Some("\"hai\" was a positive length string"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with list` {

      val positiveLengthList = AMatcher[List[Int]]("positive length list") { _.length > 0 }
      
      def `should work correctly with 'should be'` {
        List(1, 2, 3) should be a positiveLengthList
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List.empty[Int] should be a positiveLengthList
        }
        e.message should be (Some(List.empty[Int] + " was not a positive length list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        List.empty[Int] should not be a (positiveLengthList)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 3) should not be a (positiveLengthList)
        }
        e.message should be (Some(List(1, 2, 3) + " was a positive length list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0}
      
      def `should work correctly with 'should contain'` {
        List(-2, 5, -8) should contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(-2, -6, -8) should contain a positiveNumber
        }
        e.message should be (Some(List(-2, -6, -8) + " did not contain a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        List(-2, -6, -8) should not contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(-1, 2, -6) should not contain a (positiveNumber)
        }
        e.message should be (Some(List(-1, 2, -6) + " contained a positive number: 2 was a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with set` {

      val positiveSizeSet = AMatcher[Set[Int]]("positive size set") { _.size > 0 }
      
      def `should work correctly with 'should be'` {
        Set(1, 2, 3) should be a positiveSizeSet
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set.empty[Int] should be a positiveSizeSet
        }
        e.message should be (Some(Set.empty[Int] + " was not a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Set.empty[Int] should not be a (positiveSizeSet)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(1, 2, 3) should not be a (positiveSizeSet)
        }
        e.message should be (Some(Set(1, 2, 3) + " was a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0}
      
      def `should work correctly with 'should contain'` {
        Set(-2, 5, -8) should contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(-2, -6, -8) should contain a positiveNumber
        }
        e.message should be (Some(Set(-2, -6, -8) + " did not contain a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        Set(-2, -6, -8) should not contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(-1, 2, -6) should not contain a (positiveNumber)
        }
        e.message should be (Some(Set(-1, 2, -6) + " contained a positive number: 2 was a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with array` {

      val positiveSizeArray = AMatcher[Array[Int]]("positive size array") { _.size > 0 }
      
      def `should work correctly with 'should be'` {
        Array(1, 2, 3) should be a positiveSizeArray
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array.empty[Int] should be a positiveSizeArray
        }
        e.message should be (Some("Array() was not a positive size array"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Array.empty[Int] should not be a (positiveSizeArray)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(1, 2, 3) should not be a (positiveSizeArray)
        }
        e.message should be (Some("Array(1, 2, 3) was a positive size array"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0}
      
      def `should work correctly with 'should contain'` {
        Array(-2, 5, -8) should contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(-2, -6, -8) should contain a positiveNumber
        }
        e.message should be (Some("Array(-2, -6, -8) did not contain a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        Array(-2, -6, -8) should not contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(-1, 2, -6) should not contain a (positiveNumber)
        }
        e.message should be (Some("Array(-1, 2, -6) contained a positive number: 2 was a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with map` {

      val positiveSizeMap = AMatcher[Map[Int, String]]("positive size map") { _.size > 0 }
      
      def `should work correctly with 'should be'` {
        Map(1 -> "one", 2 -> "two", 3 -> "three") should be a positiveSizeMap
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map.empty[Int, String] should be a positiveSizeMap
        }
        e.message should be (Some(Map.empty[Int, String] + " was not a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Map.empty[Int, String] should not be a (positiveSizeMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(1 -> "one", 2 -> "two", 3 -> "three") should not be a (positiveSizeMap)
        }
        e.message should be (Some(Map(1 -> "one", 2 -> "two", 3 -> "three") + " was a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val positiveNumberKey = AMatcher[(Int, String)]("positive number key") { _._1 > 0}
      
      def `should work correctly with 'should contain'` {
        Map(-2 -> "minus two", 5 -> "five", -8 -> "minus eight") should contain a (positiveNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") should contain a positiveNumberKey
        }
        e.message should be (Some(Map(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") + " did not contain a positive number key"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        Map(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") should not contain a (positiveNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(-1 -> "minus one", 2 -> "two", -6 -> "minus six") should not contain a (positiveNumberKey)
        }
        e.message should be (Some(Map(-1 -> "minus one", 2 -> "two", -6 -> "minus six") + " contained a positive number key: (2,two) was a positive number key"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with java list` {

      val positiveSizeList = AMatcher[java.util.List[Int]]("positive size list") { _.size > 0 }
      
      def javaList(values: Int*): java.util.List[Int] = {
        val javaList = new java.util.ArrayList[Int]()
        values.foreach(javaList.add(_))
        javaList
      }
      
      def `should work correctly with 'should be'` {
        javaList(1, 2, 3) should be a positiveSizeList
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val left = javaList()
        val e = intercept[exceptions.TestFailedException] {
          left should be a positiveSizeList
        }
        e.message should be (Some(left  + " was not a positive size list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        javaList() should not be a (positiveSizeList)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val left = javaList(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be a (positiveSizeList)
        }
        e.message should be (Some(left + " was a positive size list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0}
      
      def `should work correctly with 'should contain'` {
        javaList(-2, 5, -8) should contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = javaList(-2, -6, -8)
        val e = intercept[exceptions.TestFailedException] {
          left should contain a positiveNumber
        }
        e.message should be (Some(left + " did not contain a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        javaList(-2, -6, -8) should not contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val left = javaList(-1, 2, -6)
        val e = intercept[exceptions.TestFailedException] {
          left should not contain a (positiveNumber)
        }
        e.message should be (Some(left + " contained a positive number: 2 was a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with java set` {

      val positiveSizeSet = AMatcher[java.util.Set[Int]]("positive size set") { _.size > 0 }
      
      def javaSet(values: Int*): java.util.Set[Int] = {
        val javaSet = new java.util.HashSet[Int]()
        values.foreach(javaSet.add(_))
        javaSet
      }
      
      def `should work correctly with 'should be'` {
        javaSet(1, 2, 3) should be a positiveSizeSet
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val left = javaSet()
        val e = intercept[exceptions.TestFailedException] {
          left should be a positiveSizeSet
        }
        e.message should be (Some(left  + " was not a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        javaSet() should not be a (positiveSizeSet)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val left = javaSet(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be a (positiveSizeSet)
        }
        e.message should be (Some(left + " was a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0}
      
      def `should work correctly with 'should contain'` {
        javaSet(-2, 5, -8) should contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = javaSet(-2, -6, -8)
        val e = intercept[exceptions.TestFailedException] {
          left should contain a positiveNumber
        }
        e.message should be (Some(left + " did not contain a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        javaSet(-2, -6, -8) should not contain a (positiveNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val left = javaSet(-1, 2, -6)
        val e = intercept[exceptions.TestFailedException] {
          left should not contain a (positiveNumber)
        }
        e.message should be (Some(left + " contained a positive number: 2 was a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when used with java map` {
      
      def javaMap(values: (Int, String)*): java.util.Map[Int, String] = {
        val javaMap = new java.util.LinkedHashMap[Int, String]()
        values.foreach(e => javaMap.put(e._1, e._2))
        javaMap
      }

      val positiveSizeMap = AMatcher[java.util.Map[Int, String]]("positive size map") { _.size > 0 }
      
      def `should work correctly with 'should be'` {
        javaMap(1 -> "one", 2 -> "two", 3 -> "three") should be a positiveSizeMap
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val left = javaMap()
        val e = intercept[exceptions.TestFailedException] {
          left should be a positiveSizeMap
        }
        e.message should be (Some(left + " was not a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        javaMap() should not be a (positiveSizeMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val left = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
        val e = intercept[exceptions.TestFailedException] {
          left should not be a (positiveSizeMap)
        }
        e.message should be (Some(left + " was a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      val positiveNumberKey = AMatcher[(Int, String)]("positive number key") { _._1 > 0}
      
      def `should work correctly with 'should contain'` {
        javaMap(-2 -> "minus two", 5 -> "five", -8 -> "minus eight") should contain a (positiveNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = javaMap(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight")
        val e = intercept[exceptions.TestFailedException] {
          left should contain a positiveNumberKey
        }
        e.message should be (Some(left + " did not contain a positive number key"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        javaMap(-2 -> "minus two", -6 -> "minus six", -8 -> "minus eight") should not contain a (positiveNumberKey)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val left = javaMap(-1 -> "minus one", 2 -> "two", -6 -> "minus six")
        val e = intercept[exceptions.TestFailedException] {
          left should not contain a (positiveNumberKey)
        }
        e.message should be (Some(left + " contained a positive number key: (2,two) was a positive number key"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with custom object` {

      case class Person(name: String, age: Int)
      
      val youngMan = AMatcher[Person]("young man") { _.age < 60 }
      
      def `should work correctly with 'should be'` {
        Person("Tom", 30) should be a youngMan
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed` {
        val tom = Person("Tom", 60)
        val e = intercept[exceptions.TestFailedException] {
          tom should be a youngMan
        }
        e.message should be (Some(tom + " was not a young man"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Person("Tom", 60) should not be a (youngMan)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed` {
        val tom = Person("Tom", 30)
        val e = intercept[exceptions.TestFailedException] {
          tom should not be a (youngMan)
        }
        e.message should be (Some(tom + " was a young man"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should contain'` {
        List(Person("Tom", 60), Person("Donald", 30), Person("Jerry", 65)) should contain a youngMan
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should contain a' assertion failed` {
        val left = List(Person("Tom", 60), Person("Donald", 70), Person("Jerry", 65))
        val e = intercept[exceptions.TestFailedException] {
          left should contain a youngMan
        }
        e.message should be (Some(left + " did not contain a young man"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not contain'` {
        List(Person("Tom", 60), Person("Donald", 70), Person("Jerry", 65)) should not contain a (youngMan)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not contain a' assertion failed` {
        val donald = Person("Donald", 30)
        val left = List(Person("Tom", 60), donald, Person("Jerry", 65))
        val e = intercept[exceptions.TestFailedException] {
          left should not contain a (youngMan)
        }
        e.message should be (Some(left + " contained a young man: " + donald + " was a young man"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
  }
}