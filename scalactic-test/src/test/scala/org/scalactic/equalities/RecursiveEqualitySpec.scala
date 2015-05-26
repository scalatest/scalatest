/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.equalities

import org.scalatest._
import org.scalactic._

import scala.util.Success

class RecursiveEqualitySpec extends FunSpec with Matchers with NonImplicitAssertions {

  describe("A Chain") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under any policy by default") {
      new UncheckedEquality {
        Chain(Chain("HI")) shouldEqual Chain(Chain("HI"))
        Chain(Chain("HI")) should not equal Chain(Chain("hi"))
        Chain(Chain("HI", "THERE"), Chain("FOO", "BAR")) should not equal Chain(Chain("hi", "there"), Chain("Foo", "Bar"))
      }
    }
    it("should do recursive equality under RecursiveChainEquality") {
      new UncheckedEquality {
        import RecursiveChainEquality._
        Chain(Chain("HI")) shouldEqual Chain(Chain("HI"))
        Chain(Chain("HI")) shouldEqual Chain(Chain("hi"))
        Chain(Chain("HI", "THERE"), Chain("FOO", "BAR")) shouldEqual Chain(Chain("hi", "there"), Chain("Foo", "Bar"))
      }
    }
  }

  describe("An Every") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under any policy by default") {
      new UncheckedEquality {
        One("HI") should not equal One("hi")
        One(One("HI")) should not equal One(One("hi"))

        Many("HI", "THERE") should not equal Many("hi", "there")
        Many(Many("HI", "THERE"), Many("FOO", "BAR")) should not equal Many(Many("hi", "there"), Many("Foo", "Bar"))

        One(Many("HI", "THERE")) should not equal One(Many("hi", "there"))
        Many(One("HI"), One("THERE")) should not equal Many(One("hi"), One("there"))
      }
    }
    it("should do recursive equality under RecursiveEveryEquality") {
      new UncheckedEquality {
        import RecursiveEveryEquality._
        One("HI") shouldEqual One("hi")
        One(One("HI")) shouldEqual One(One("hi"))

        Many("HI", "THERE") shouldEqual Many("hi", "there")
        Many(Many("HI", "THERE"), Many("FOO", "BAR")) shouldEqual Many(Many("hi", "there"), Many("Foo", "Bar"))

        One(Many("HI", "THERE")) shouldEqual One(Many("hi", "there"))
        Many(One("HI"), One("THERE")) shouldEqual Many(One("hi"), One("there"))
      }
    }
  }

  describe("A Try") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    val anException = new IllegalArgumentException("Boo!")

    it("should NOT do recursive equality under any policy by default") {
      new UncheckedEquality {
        Success(Success("HI")) shouldEqual Success(Success("HI"))
        Success(Success("HI")) should not equal Success(Success("hi"))
        Fail(Fail(anException)) shouldEqual Fail(Fail(anException))
      }
    }
    it("should do recursive equality under RecursiveTryEquality") {
      new UncheckedEquality {
        import RecursiveTryEquality._
        Success(Success("HI")) should not equal Success(Success("hi"))
        Fail(Fail(anException)) shouldEqual Fail(Fail(anException))
      }
    }
  }

  describe("An Either") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        Left("HI") should not equal Left("hi")
        Left(Left("HI")) should not equal Left(Left("hi"))

        Right("HI") should not equal Right("hi")
        Right(Right("HI")) should not equal Right(Right("hi"))

        Left(Right("HI")) should not equal Left(Right("hi"))
        Right(Left("HI")) should not equal Right(Left("hi"))
 
        // With Either as left hand type
        (Left("HI"): Either[String, Int]) should not equal Left("hi")
        (Left(Left("HI")): Either[Either[String, Int], Float]) should not equal Left(Left("hi"))

        (Right("HI"): Either[Int, String]) should not equal Right("hi")
        (Right(Right("HI")): Either[Float, Either[Int, String]]) should not equal Right(Right("hi"))

        (Left(Right("HI")): Either[Either[Float, String], Int]) should not equal Left(Right("hi"))
        (Right(Left("HI")): Either[Int, Either[String, Float]]) should not equal Right(Left("hi"))
      }
    }

    it("should do recursive equality under RecursiveEitherEquality") {
      new UncheckedEquality {
        import RecursiveEitherEquality._
        Left("HI") shouldEqual Left("hi")
        Left(Left("HI")) shouldEqual Left(Left("hi"))

        Right("HI") shouldEqual Right("hi")
        Right(Right("HI")) shouldEqual Right(Right("hi"))

        Left(Right("HI")) shouldEqual Left(Right("hi"))
        Left(Right("HI")) should not equal Right(Left("hi"))
 
        // With Either as left hand type
        (Left("HI"): Either[String, Int]) shouldEqual Left("hi")
        (Left(Left("HI")): Either[Either[String, Int], Float]) shouldEqual Left(Left("hi"))

        (Right("HI"): Either[Int, String]) shouldEqual Right("hi")
        (Right(Right("HI")): Either[Float, Either[Int, String]]) shouldEqual Right(Right("hi"))

        (Left(Right("HI")): Either[Either[Float, String], Int]) shouldEqual Left(Right("hi"))
        (Right(Left("HI")): Either[Int, Either[String, Float]]) shouldEqual Right(Left("hi"))
      }
    }
  }

  describe("An Or") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        Good("HI") should not equal Good("hi")
        Good(Good("HI")) should not equal Good(Good("hi"))

        Bad("HI") should not equal Bad("hi")
        Bad(Bad("HI")) should not equal Bad(Bad("hi"))

        Good(Bad("HI")) should not equal Good(Bad("hi"))
        Bad(Good("HI")) should not equal Bad(Good("hi"))

/*
        // With Or as left hand type
        (Good("HI"): Or[String, Int]) should not equal Good("hi")
        (Good(Good("HI")): Or[Or[String, Int], Float]) should not equal Good(Good("hi"))

        (Bad("HI"): Or[Int, String]) should not equal Bad("hi")
        (Bad(Bad("HI")): Or[Float, Or[Int, String]]) should not equal Bad(Bad("hi"))

        (Good(Bad("HI")): Or[Or[Float, String], Int]) should not equal Good(Bad("hi"))
        (Bad(Good("HI")): Or[Int, Or[String, Float]]) should not equal Bad(Good("hi"))
*/
      }
    }
    it("should do recursive equality under with RecursiveOrEquality") {
      new TripleEquals {
        import RecursiveOrEquality._

        Good("HI") shouldEqual Good("hi")
        Good(Good("HI")) shouldEqual Good(Good("hi"))

        Bad("HI") shouldEqual Bad("hi")
        Bad(Bad("HI")) shouldEqual Bad(Bad("hi"))

        Good(Bad("HI")) shouldEqual Good(Bad("hi"))
        Bad(Good("HI")) shouldEqual Bad(Good("hi"))

/*
        // With Or as left hand type
        (Good("HI"): Or[String, Int]) shouldEqual Good("hi")
        (Good(Good("HI")): Or[Or[String, Int], Float]) shouldEqual Good(Good("hi"))

        (Bad("HI"): Or[Int, String]) shouldEqual Bad("hi")
        (Bad(Bad("HI")): Or[Float, Or[Int, String]]) shouldEqual Bad(Bad("hi"))

        (Good(Bad("HI")): Or[Or[Float, String], Int]) shouldEqual Good(Bad("hi"))
        (Bad(Good("HI")): Or[Int, Or[String, Float]]) shouldEqual Bad(Good("hi"))
*/
      }
    }
  }

  describe("A List") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    def jList[E](e: E*): java.util.List[E] = {
      val list = new java.util.LinkedList[E]()
      e.foreach(list.add)
      list
    }

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") should not equal List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF"), "foo")
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("hi", "there"), List("abc", "def"))

        List(List(List("HI", "There"))) should not equal List(List(List("hi", "there")))
        List(List(List("HIGH", "Their"))) should not equal List(List(List("hi", "there")))

        List(Nil) shouldEqual List(Nil)
        List(List(Nil), List(Nil)) shouldEqual List(List(Nil), List(Nil))
        List(List(Nil)) should not equal List(Nil)

        jList("HI", "THERE") should not equal jList("hi", "there")
        jList(jList("HI", "THERE"), jList("FOO", "BAR")) should not equal jList(jList("hi", "there"), jList("foo", "bar"))
        jList(jList("HI", "THERE"), jList[String]()) should not equal jList(jList("hi", "there"), jList[String]())

        Array("HI", "THERE") should not equal Array("hi", "there")
        Array(Array("HI", "THERE"), Array("FOO", "BAR")) should not equal Array(Array("hi", "there"), Array("foo", "bar"))
        Array(Array("HI", "THERE"), Array[String]()) should not equal Array(Array("hi", "there"), Array[String]())
      }

      new CheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") should not equal List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF", "foo"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("hi", "there"), List("abc", "def"), "foo")

        List(List(List("HI", "There"))) should not equal List(List(List("hi", "there")))
        List(List(List("HIGH", "Their"))) should not equal List(List(List("hi", "there")))

        jList("HI", "THERE") should not equal jList("hi", "there")
        jList(jList("HI", "THERE"), jList("FOO", "BAR")) should not equal jList(jList("hi", "there"), jList("foo", "bar"))
        jList(jList("HI", "THERE"), jList[String]()) should not equal jList(jList("hi", "there"), jList[String]())

        Array("HI", "THERE") should not equal Array("hi", "there")
        Array(Array("HI", "THERE"), Array("FOO", "BAR")) should not equal Array(Array("hi", "there"), Array("foo", "bar"))
        Array(Array("HI", "THERE"), Array[String]()) should not equal Array(Array("hi", "there"), Array[String]())
      }
    }

    it("should do recursive equality under any policy under RecursiveSeqEquality") {
      import RecursiveSeqEquality._
      import RecursiveJavaListEquality._
      import RecursiveArrayEquality._

      new UncheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") shouldEqual List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF", "foo"))
        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("hi", "there"), List("abc", "def"))


        List(List(List("HI", "There"))) shouldEqual List(List(List("hi", "there")))
        List(List(List("HIGH", "Their"))) should not equal List(List(List("hi", "there")))

        List(Nil) shouldEqual List(Nil)
        List(List(Nil), List(Nil)) shouldEqual List(List(Nil), List(Nil))
        List(List(Nil)) should not equal List(Nil)

        jList("HI", "THERE") shouldEqual jList("hi", "there")
        jList(jList("HI", "THERE"), jList("FOO", "BAR")) shouldEqual jList(jList("hi", "there"), jList("foo", "bar"))
        jList(jList("HI", "THERE"), jList[String]()) shouldEqual jList(jList("hi", "there"), jList[String]())

        Array("HI", "THERE") shouldEqual Array("hi", "there")
        Array(Array("HI", "THERE"), Array("FOO", "BAR")) shouldEqual Array(Array("hi", "there"), Array("foo", "bar"))
        Array(Array("HI", "THERE"), Array[String]()) shouldEqual Array(Array("hi", "there"), Array[String]())
      }

      new CheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") shouldEqual List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF", "foo"))
        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("hi", "there"), List("abc", "def"))

        jList("HI", "THERE") shouldEqual jList("hi", "there")
        jList(jList("HI", "THERE"), jList("FOO", "BAR")) shouldEqual jList(jList("hi", "there"), jList("foo", "bar"))
        jList(jList("HI", "THERE"), jList[String]()) shouldEqual jList(jList("hi", "there"), jList[String]())

        Array("HI", "THERE") shouldEqual Array("hi", "there")
        Array(Array("HI", "THERE"), Array("FOO", "BAR")) shouldEqual Array(Array("hi", "there"), Array("foo", "bar"))
        Array(Array("HI", "THERE"), Array[String]()) shouldEqual Array(Array("hi", "there"), Array[String]())
      }
    }
  }

  describe("A Set") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    def jSet[E](e: E*): java.util.Set[E] = {
      val set = new java.util.HashSet[E]()
      e.foreach(set.add)
      set
    }

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") should not equal Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("DeF", "aBc"), Set("There", "HI"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("def", "abc"), Set("there", "hi"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) should not equal Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))
        Set(Set(Nil)) should not equal Set(Nil)

        jSet("HI", "THERE") should not equal jSet("hi", "there")
        jSet(jSet("HI", "THERE"), jSet("FOO", "BAR")) should not equal jSet(jSet("foo", "bar"), jSet("hi", "there"))
        jSet(jSet("HI", "THERE"), jSet[String]()) should not equal jSet(jSet("hi", "there"), jSet[String]())
      }

      new CheckedEquality {
        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") should not equal Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("DeF", "aBc"), Set("There", "HI"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("def", "abc"), Set("there", "hi"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) should not equal Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))

        jSet("HI", "THERE") should not equal jSet("hi", "there")
        jSet(jSet("HI", "THERE"), jSet("FOO", "BAR")) should not equal jSet(jSet("foo", "bar"), jSet("hi", "there"))
        jSet(jSet("HI", "THERE"), jSet[String]()) should not equal jSet(jSet("hi", "there"), jSet[String]())
      }
    }

    it("should do recursive equality under any policy under RecursiveSetEquality") {
      import RecursiveSetEquality._
      import RecursiveJavaSetEquality._

      new UncheckedEquality {

        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") shouldEqual Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("There", "HI"), Set("DeF", "aBc"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("there", "hi"), Set("def", "abc"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) shouldEqual Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))

        jSet("HI", "THERE") shouldEqual jSet("hi", "there")
        jSet(jSet("HI", "THERE"), jSet("FOO", "BAR")) shouldEqual jSet(jSet("foo", "bar"), jSet("hi", "there"))
        jSet(jSet("HI", "THERE"), jSet[String]()) shouldEqual jSet(jSet("hi", "there"), jSet[String]())
      }

      new CheckedEquality {

        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") shouldEqual Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("There", "HI"), Set("DeF", "aBc"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("there", "hi"), Set("def", "abc"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) shouldEqual Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))

        jSet("HI", "THERE") shouldEqual jSet("hi", "there")
        jSet(jSet("HI", "THERE"), jSet("FOO", "BAR")) shouldEqual jSet(jSet("foo", "bar"), jSet("hi", "there"))
        jSet(jSet("HI", "THERE"), jSet[String]()) shouldEqual jSet(jSet("hi", "there"), jSet[String]())
      }
    }
  }

  describe("A Map") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    def jMap[K, V](e: (K,V)*): java.util.Map[K,V] = {
      val map = new java.util.HashMap[K,V]()
      e.foreach(kv => map.put(kv._1, kv._2))
      map
    }

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") should not equal Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) should not equal Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) should not equal Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) should not equal Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])

        jMap("Key1" -> jMap("HI" -> "High")) should not equal jMap("key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap("FOO" -> "Bar")) should not equal jMap("key2" -> jMap("foo" -> "bar"), "key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap()) should not equal jMap("key2" -> jMap("foo" -> "bar"), "key1" -> jMap())
      }

      new CheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") should not equal Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) should not equal Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) should not equal Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) should not equal Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])

        jMap("Key1" -> jMap("HI" -> "High")) should not equal jMap("key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap("FOO" -> "Bar")) should not equal jMap("key2" -> jMap("foo" -> "bar"), "key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap()) should not equal jMap("key2" -> jMap("foo" -> "bar"), "key1" -> jMap())
      }
    }

    it("should do recursive equality under any policy under RecursiveSetEquality") {
      import RecursiveMapEquality._
      import RecursiveJavaMapEquality._

      new UncheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) shouldEqual Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) shouldEqual Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])

        jMap("Key1" -> jMap("HI" -> "High")) shouldEqual jMap("key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap("FOO" -> "Bar")) shouldEqual jMap("key2" -> jMap("foo" -> "bar"), "key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap[String,String]()) shouldEqual jMap("key1" -> jMap("hi" -> "high"), "key2" -> jMap[String,String]())
      }

      new CheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) shouldEqual Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) shouldEqual Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])

        jMap("Key1" -> jMap("HI" -> "High")) shouldEqual jMap("key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap("FOO" -> "Bar")) shouldEqual jMap("key2" -> jMap("foo" -> "bar"), "key1" -> jMap("hi" -> "high"))
        jMap("Key1" -> jMap("HI" -> "High"), "Key2" -> jMap[String,String]()) shouldEqual jMap("key1" -> jMap("hi" -> "high"), "key2" -> jMap[String,String]())
      }
    }
  }

  describe("An Option") {

    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under the all policies by default") {

      // New policies
      // Both sides Some
      new UncheckedEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new CheckedEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new EnabledEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      // Both sides Option
      new UncheckedEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new CheckedEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new EnabledEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      // Left side Some, right side Option
      new UncheckedEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new CheckedEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new EnabledEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      // Left side Option, right side Some
      new UncheckedEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new CheckedEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new EnabledEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }

      // Deprecated policies
      // Both sides Some
      new TripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      // Both sides Option
      new TripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      // Left side Some, right side Option
      new TripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      // Left side Option, right side Some
      new TripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
    }

    it("should do recursive equality under any policy under RecursiveOptionEquality") {

      import RecursiveOptionEquality._

      // New policies
      // Both sides Some
      new UncheckedEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new CheckedEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new EnabledEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      // Both sides Option
      new UncheckedEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new CheckedEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new EnabledEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Some, right side Option
      new UncheckedEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new CheckedEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new EnabledEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Option, right side Some
      new UncheckedEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new CheckedEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new EnabledEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }

      // Deprecated policies
      // Both sides Some
      new TripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      // Both sides Option
      new TripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Some, right side Option
      new TripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Option, right side Some
      new TripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
    }
  }
}

