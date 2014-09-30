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
package org.scalactic

import java.text._
import org.scalatest._
import scala.collection.GenTraversable
import scala.collection.immutable.TreeSet
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.SortedSet

class SortedEquaSetSpec extends UnitSpec {
  implicit class HasExactType[T](o: T) {
    def shouldHaveExactType[U](implicit ev: T =:= U): Unit = ()
  }
  def normalHashingEquality[T] =
    new HashingEquality[T] {
      def hashCodeFor(a: T): Int = a.hashCode
      def areEqual(a: T, b: Any): Boolean = a == b
    }
  def normalOrderingEquality[T](implicit ord: Ordering[T]) =
    new OrderingEquality[T] {
      def compare(a: T, b: T): Int = ord.compare(a, b)
      def hashCodeFor(a: T): Int = a.hashCode
      def areEqual(a: T, b: Any): Boolean = a == b
    }
  val intEquality =
    new OrderingEquality[Int] {
      def hashCodeFor(a: Int): Int = a.hashCode
      def areEqual(a: Int, b: Any): Boolean = a == b
      def compare(a: Int, b: Int): Int = a - b
    }
  val plainNumber = EquaSets[Int](intEquality)
  val number = SortedEquaSets[Int](intEquality)
  val sortedNumber = SortedEquaSets[Int](normalOrderingEquality[Int])
  val lower = SortedEquaSets[String](StringNormalizations.lowerCased.toOrderingEquality)
  val plainLower = EquaSets[String](StringNormalizations.lowerCased.toOrderingEquality)
  val sortedLower = SortedEquaSets[String](StringNormalizations.lowerCased.toOrderingEquality)
  val trimmed = SortedEquaSets[String](StringNormalizations.trimmed.toOrderingEquality)
  val intStringEquality =
    new OrderingEquality[(Int, String)] {
      def hashCodeFor(a: (Int, String)): Int = a.hashCode
      def areEqual(a: (Int, String), b: Any): Boolean = a == b
      def compare(a: (Int, String), b: (Int, String)): Int =
        if (a._1 != b._1)
          a._1 - b._1
        else
          a._2 compareTo b._2
    }
  val numberLower = SortedEquaSets(intStringEquality)
  val intStringStringEquality =
    new OrderingEquality[(Int, String, String)] {
      def hashCodeFor(a: (Int, String, String)): Int = a.hashCode
      def areEqual(a: (Int, String, String), b: Any): Boolean = a == b
      def compare(a: (Int, String, String), b: (Int, String, String)): Int =
        if (a._1 != b._1)
          a._1 - b._1
        else if (a._2 != b._2)
          a._2 compareTo b._2
        else
          a._3 compareTo b._3
    }
  val numberLowerTrimmed = SortedEquaSets(intStringStringEquality)

  def numberListEquality[T] =
    new OrderingEquality[List[Int]] {
      def hashCodeFor(a: List[Int]): Int = a.hashCode
      def areEqual(a: List[Int], b: Any): Boolean = a == b
      def compare(a: List[Int], b: List[Int]): Int = a.mkString compareTo b.mkString
    }

  val numberList = SortedEquaSets[List[Int]](numberListEquality)

  val numberNumberEquality =
    new OrderingEquality[number.SortedEquaSet] {
      def hashCodeFor(a: number.SortedEquaSet): Int = a.hashCode
      def areEqual(a: number.SortedEquaSet, b: Any): Boolean = a == b
      def compare(a: number.SortedEquaSet, b: number.SortedEquaSet): Int = a.mkString compareTo b.mkString
    }
  val numberNumber = SortedEquaSets[number.SortedEquaSet](numberNumberEquality)

  "An SortedEquaSet" can "be constructed with empty" in {
    val emptySet = lower.SortedEquaSet.empty
    emptySet shouldBe empty

    val treeEmptySet = lower.TreeEquaSet.empty
    treeEmptySet shouldBe empty

    lower.SortedEquaSet.empty.shouldHaveExactType[lower.SortedEquaSet]
    lower.TreeEquaSet.empty.shouldHaveExactType[lower.TreeEquaSet]
  }
  it can "be constructed with apply" in {
    val result1 = lower.SortedEquaSet("one", "two", "three")
    result1 should have size 3
    result1.shouldHaveExactType[lower.SortedEquaSet]

    val result2 = lower.TreeEquaSet("one", "two", "three")
    result2 should have size 3
    result2.shouldHaveExactType[lower.TreeEquaSet]
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedEquaSet.
  }
  it should "construct only sets with appropriate element types" in {
    "lower.SortedEquaSet(1, 2, 3)" shouldNot compile
    "lower.TreeEquaSet(1, 2, 3)" shouldNot compile
  }
  it should "eliminate 'duplicate' entries passed to the apply factory method" in {
    val result1 = lower.SortedEquaSet("one", "two", "two", "three", "Three")
    result1 should have size 3
    result1.shouldHaveExactType[lower.SortedEquaSet]

    val result2 = lower.TreeEquaSet("one", "two", "two", "three", "Three")
    result2 should have size 3
    result2.shouldHaveExactType[lower.TreeEquaSet]
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedEquaSet.
  }
  it should "have a toString method" in {
    lower.SortedEquaSet("hi", "ho").toString should === ("TreeEquaSet(hi, ho)")
    lower.TreeEquaSet("hi", "ho").toString should === ("TreeEquaSet(hi, ho)")
  }
  it should "have a diff method that takes another SortedEquaSet instance with the same path-dependant type" in {
    val result1 = lower.SortedEquaSet("hi", "ho") diff lower.SortedEquaSet("HI", "HO")
    result1 shouldBe lower.SortedEquaSet()
    result1.shouldHaveExactType[lower.SortedEquaSet]

    val result2 = trimmed.SortedEquaSet("hi", "ho") diff trimmed.SortedEquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.SortedEquaSet()
    result2.shouldHaveExactType[trimmed.SortedEquaSet]

    """lower.SortedEquaSet(" hi ", "hi") diff trimmed.SortedEquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.SortedEquaSet("hi", "ho") diff lower.SortedEquaSet("ho")
    result3 shouldBe lower.SortedEquaSet("hi")
    result3.shouldHaveExactType[lower.SortedEquaSet]

    val result4 = lower.SortedEquaSet("hi", "ho", "let's", "go") diff lower.SortedEquaSet("bo", "no", "go", "ho")
    result4 shouldBe lower.SortedEquaSet("hi", "let's")
    result4.shouldHaveExactType[lower.SortedEquaSet]

    val result5 = lower.TreeEquaSet("hi", "ho") diff lower.TreeEquaSet("HI", "HO")
    result5 shouldBe lower.TreeEquaSet()
    result5.shouldHaveExactType[lower.TreeEquaSet]

    val result6 = trimmed.TreeEquaSet("hi", "ho") diff trimmed.TreeEquaSet(" hi ", " ho ")
    result6 shouldBe trimmed.TreeEquaSet()
    result6.shouldHaveExactType[trimmed.TreeEquaSet]

    """lower.TreeEquaSet(" hi ", "hi") diff trimmed.TreeEquaSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.TreeEquaSet("hi", "ho") diff lower.TreeEquaSet("ho")
    result7 shouldBe lower.TreeEquaSet("hi")
    result7.shouldHaveExactType[lower.TreeEquaSet]

    val result8 = lower.TreeEquaSet("hi", "ho", "let's", "go") diff lower.TreeEquaSet("bo", "no", "go", "ho")
    result8 shouldBe lower.TreeEquaSet("hi", "let's")
    result8.shouldHaveExactType[lower.TreeEquaSet]
  }
  it should "have a &~ method that takes another SortedEquaSet instance with the same path-dependant type" in {
    lower.SortedEquaSet("hi", "ho") &~ lower.SortedEquaSet("HI", "HO") shouldBe lower.SortedEquaSet()
    trimmed.SortedEquaSet("hi", "ho") &~ trimmed.SortedEquaSet(" hi ", " ho ") shouldBe trimmed.SortedEquaSet()
    """lower.SortedEquaSet(" hi ", "hi") &~ trimmed.SortedEquaSet("hi", "HI")""" shouldNot typeCheck
    lower.SortedEquaSet("hi", "ho") &~ lower.SortedEquaSet("ho") shouldBe lower.SortedEquaSet("hi")
    lower.SortedEquaSet("hi", "ho", "let's", "go") &~ lower.SortedEquaSet("bo", "no", "go", "ho") shouldBe lower.SortedEquaSet("hi", "let's")
  }
  it should "have an intersect method that takes another SortedEquaSet instance with the same path-dependant type" in {
    lower.SortedEquaSet("hi", "ho") intersect lower.SortedEquaSet("HI", "HO") shouldBe lower.SortedEquaSet("hi", "ho")
    trimmed.SortedEquaSet("hi", "ho") intersect trimmed.SortedEquaSet(" hi ", " ho ") shouldBe trimmed.SortedEquaSet("hi", "ho")
    """lower.SortedEquaSet(" hi ", "hi") intersect trimmed.SortedEquaSet("hi", "HI")""" shouldNot typeCheck
    lower.SortedEquaSet("hi", "ho") intersect lower.SortedEquaSet("ho") shouldBe lower.SortedEquaSet("ho")
    lower.SortedEquaSet("hi", "ho", "let's", "go") intersect lower.SortedEquaSet("bo", "no", "go", "ho") shouldBe lower.SortedEquaSet("ho", "go")
  }
  it should "have an & method that takes another SortedEquaSet instance with the same path-dependant type" in {
    lower.SortedEquaSet("hi", "ho") & lower.SortedEquaSet("HI", "HO") shouldBe lower.SortedEquaSet("hi", "ho")
    trimmed.SortedEquaSet("hi", "ho") & trimmed.SortedEquaSet(" hi ", " ho ") shouldBe trimmed.SortedEquaSet("hi", "ho")
    """lower.SortedEquaSet(" hi ", "hi") & trimmed.SortedEquaSet("hi", "HI")""" shouldNot typeCheck
    lower.SortedEquaSet("hi", "ho") & lower.SortedEquaSet("ho") shouldBe lower.SortedEquaSet("ho")
    lower.SortedEquaSet("hi", "ho", "let's", "go") & lower.SortedEquaSet("bo", "no", "go", "ho") shouldBe lower.SortedEquaSet("ho", "go")
  }
  it should "have a union method that takes another SortedEquaSet instance with the same path-dependant type" in {
    lower.SortedEquaSet("hi", "ho") union lower.SortedEquaSet("HI", "HO") shouldBe lower.SortedEquaSet("hi", "ho")
    trimmed.SortedEquaSet("hi", "ho") union trimmed.SortedEquaSet(" hi ", " ho ") shouldBe trimmed.SortedEquaSet("hi", "ho")
    """lower.SortedEquaSet(" hi ", "hi") union trimmed.SortedEquaSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a | method that takes another SortedEquaSet instance with the same path-dependant type" in {
    lower.SortedEquaSet("hi", "ho") | lower.SortedEquaSet("HI", "HO") shouldBe lower.SortedEquaSet("hi", "ho")
    trimmed.SortedEquaSet("hi", "ho") | trimmed.SortedEquaSet(" hi ", " ho ") shouldBe trimmed.SortedEquaSet("hi", "ho")
    """lower.SortedEquaSet(" hi ", "hi") | trimmed.SortedEquaSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a toSet method" in {
    lower.SortedEquaSet("hi", "ho").toSet should === (Set(lower.EquaBox("hi"), lower.EquaBox("ho")))
  }
  it should "have a + method that takes one argument" in {
    lower.SortedEquaSet("hi", "ho") + "ha" shouldBe lower.SortedEquaSet("hi", "ho", "ha")
    lower.SortedEquaSet("hi", "ho") + "HO" shouldBe lower.SortedEquaSet("hi", "ho")
  }
  it should "have a + method that takes two or more arguments" in {
    lower.SortedEquaSet("hi", "ho") + ("ha", "hey!") shouldBe lower.SortedEquaSet("hi", "ho", "ha", "hey!")
    lower.SortedEquaSet("hi", "ho") + ("HO", "hoe", "Ho!") shouldBe lower.SortedEquaSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have a - method that takes one argument" in {
    lower.SortedEquaSet("hi", "ho", "ha") - "ha" shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho") - "HO" shouldBe lower.SortedEquaSet("hi")
    lower.SortedEquaSet("hi", "ho") - "who?" shouldBe lower.SortedEquaSet("hi", "ho")
  }
  it should "have a - method that takes two or more arguments" in {
    lower.SortedEquaSet("hi", "ho", "ha") - ("ha", "howdy!") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")  shouldBe lower.SortedEquaSet("hi", "fee", "foe")
    lower.SortedEquaSet("hi", "ho") - ("who", "goes", "thar") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho") - ("HI", "HO") shouldBe lower.SortedEquaSet.empty
  }
  it should "return an iterator that returns elements in sorted order" in {
    lower.SortedEquaSet("hi", "ho", "ha", "he").iterator.toList shouldEqual List("ha", "he", "hi", "ho")
  }
  it should "have a ++ method that takes a GenTraversableOnce" in {
    lower.SortedEquaSet("hi", "ho") ++ List("ha", "hey!") shouldBe lower.SortedEquaSet("hi", "ho", "ha", "hey!")
    lower.SortedEquaSet("hi", "ho") ++ List("HO", "hoe", "Ho!") shouldBe lower.SortedEquaSet("hi", "ho", "hoe", "Ho!")

    lower.SortedEquaSet("hi", "ho") ++ Set("ha", "hey!") shouldBe lower.SortedEquaSet("hi", "ho", "ha", "hey!")
    lower.SortedEquaSet("hi", "ho") ++ Set("HO", "hoe", "Ho!") shouldBe lower.SortedEquaSet("hi", "ho", "hoe", "Ho!")

    lower.SortedEquaSet("hi", "ho") ++ Vector("ha", "hey!") shouldBe lower.SortedEquaSet("hi", "ho", "ha", "hey!")
    lower.SortedEquaSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!") shouldBe lower.SortedEquaSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have a ++ method that takes another EquaSet" in {
    lower.SortedEquaSet("hi", "ho") ++ lower.SortedEquaSet("ha", "hey!") shouldBe lower.SortedEquaSet("hi", "ho", "ha", "hey!")
    lower.SortedEquaSet("hi", "ho") ++ lower.SortedEquaSet("HO", "hoe", "Ho!") shouldBe lower.SortedEquaSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have a -- method that takes a GenTraversableOnce" in {
    lower.SortedEquaSet("hi", "ho", "ha") -- List("ha", "howdy!") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")  shouldBe lower.SortedEquaSet("hi", "fee", "foe")
    lower.SortedEquaSet("hi", "ho") -- List("who", "goes", "thar") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho") -- List("HI", "HO") shouldBe lower.SortedEquaSet.empty

    lower.SortedEquaSet("hi", "ho", "ha") -- Set("ha", "howdy!") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")  shouldBe lower.SortedEquaSet("hi", "fee", "foe")
    lower.SortedEquaSet("hi", "ho") -- Set("who", "goes", "thar") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho") -- Set("HI", "HO") shouldBe lower.SortedEquaSet.empty

    lower.SortedEquaSet("hi", "ho", "ha") -- Vector("ha", "howdy!") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")  shouldBe lower.SortedEquaSet("hi", "fee", "foe")
    lower.SortedEquaSet("hi", "ho") -- Vector("who", "goes", "thar") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho") -- Vector("HI", "HO") shouldBe lower.SortedEquaSet.empty
  }
  it should "have a -- method that takes another EquaSet" in {
    lower.SortedEquaSet("hi", "ho", "ha") -- lower.EquaSet("ha", "howdy!") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.EquaSet("HO", "FIE", "fUm")  shouldBe lower.SortedEquaSet("hi", "fee", "foe")
    lower.SortedEquaSet("hi", "ho") -- lower.EquaSet("who", "goes", "thar") shouldBe lower.SortedEquaSet("hi", "ho")
    lower.SortedEquaSet("hi", "ho") -- lower.EquaSet("HI", "HO") shouldBe lower.SortedEquaSet.empty
  }
  it should "have a /: method" in {
    (0 /: number.SortedEquaSet(1))(_ + _) shouldBe 1
    (1 /: number.SortedEquaSet(1))(_ + _) shouldBe 2
    (0 /: number.SortedEquaSet(1, 2, 3))(_ + _) shouldBe 6
    (1 /: number.SortedEquaSet(1, 2, 3))(_ + _) shouldBe 7
  }
  it should "have a :\\ method" in {
    (number.SortedEquaSet(1) :\ 0)(_ + _) shouldBe 1
    (number.SortedEquaSet(1) :\ 1)(_ + _) shouldBe 2
    (number.SortedEquaSet(1, 2, 3) :\ 0)(_ + _) shouldBe 6
    (number.SortedEquaSet(1, 2, 3) :\ 1)(_ + _) shouldBe 7
  }
  it should "have 3 addString methods" in {
    lower.SortedEquaSet("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    number.SortedEquaSet(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    lower.SortedEquaSet("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    number.SortedEquaSet(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    number.SortedEquaSet(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    lower.SortedEquaSet("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    number.SortedEquaSet(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    number.SortedEquaSet(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have a aggregate method" in {
    lower.SortedEquaSet("hi", "ho", "ha", "hey!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "ha", "hey!")
    lower.SortedEquaSet("hi", "ho", "ha", "hey!").aggregate(lower.SortedEquaSet())(_ + _, _ ++ _) shouldBe lower.SortedEquaSet("hi", "ho", "ha", "hey!")

    lower.SortedEquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "hoe", "Ho!")
    lower.SortedEquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.SortedEquaSet())(_ + _, _ ++ _) shouldBe lower.SortedEquaSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have an apply method" in {
    val a = number.SortedEquaSet(1, 2, 3)
    a(2) shouldEqual true
    a(5) shouldEqual false

    val b = lower.SortedEquaSet("hi")
    b("hi") shouldEqual true
    b("Hi") shouldEqual true
    b("hI") shouldEqual true
    b("HI") shouldEqual true
    b("he") shouldEqual false
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = number.SortedEquaSet(1) andThen (!_)
    pf1(1) shouldEqual false
    pf1(2) shouldEqual true

    val pf2 = number.SortedEquaSet(1, 2, 3) andThen (!_)
    pf2(1) shouldEqual false
    pf2(2) shouldEqual false
    pf2(3) shouldEqual false
    pf2(0) shouldEqual true
  }
  it should "have a canEqual method" in {
    number.SortedEquaSet(1).canEqual(3) shouldBe false
    number.SortedEquaSet(1).canEqual("hi") shouldBe false
    number.SortedEquaSet(1).canEqual(number.EquaSet(1)) shouldBe true
    number.SortedEquaSet(1).canEqual(number.EquaSet(1, 2, 3)) shouldBe true
    number.SortedEquaSet(1).canEqual(lower.EquaSet("hi")) shouldBe false
    number.SortedEquaSet(1).canEqual(number.SortedEquaSet(1)) shouldBe true
    number.SortedEquaSet(1).canEqual(number.SortedEquaSet(1, 2, 3)) shouldBe true
    number.SortedEquaSet(1).canEqual(lower.SortedEquaSet("hi")) shouldBe false
    number.EquaSet(1).canEqual(number.SortedEquaSet(1)) shouldBe true
    number.EquaSet(1).canEqual(number.SortedEquaSet(1, 2, 3)) shouldBe true
    number.EquaSet(1).canEqual(lower.SortedEquaSet("hi")) shouldBe false
  }
  it should "have an into.collect method" in {

    // SortedEquaSet into EquaSets => EquaSet
    val result1 = number.SortedEquaSet(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10).into(plainLower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result1 shouldBe plainLower.EquaSet("4", "8", "12", "16", "20")
    result1.shouldHaveExactType[plainLower.EquaSet]

    // SortedEquaSet into SortedEquaSets => SortedEquaSet
    val result2 = number.SortedEquaSet(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10).into(sortedLower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result2 shouldBe sortedLower.SortedEquaSet("4", "8", "12", "16", "20")
    result2.shouldHaveExactType[sortedLower.SortedEquaSet]

    // TreeEquaSet into EquaSets => EquaSet
    val result3 = number.TreeEquaSet(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10).into(plainLower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result3 shouldBe plainLower.EquaSet("4", "8", "12", "16", "20")
    result3.shouldHaveExactType[plainLower.EquaSet]

    // TreeEquaSet into SortedEquaSets => TreeEquaSet
    val result4 = number.TreeEquaSet(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10).into(sortedLower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result4 shouldBe sortedLower.TreeEquaSet("4", "8", "12", "16", "20")
    result4.shouldHaveExactType[sortedLower.TreeEquaSet]

    // Extra stuff from oldInto tests
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    number.SortedEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower) collect { case i if i % 2 == 0 => (i * 2).toString } shouldBe lower.EquaSet("4", "8", "12", "16", "20")
    number.SortedEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower) collect { case i if i > 10 => (i * 2).toString } shouldBe lower.EquaSet.empty

    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    number.SortedEquaSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).into(sortedLower) collect { case i if i % 2 == 0 => (i * 2).toString } shouldBe sortedLower.SortedEquaSet("4", "8", "12", "16", "20")
    number.SortedEquaSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).into(sortedLower) collect { case i if i > 10 => (i * 2).toString } shouldBe sortedLower.EquaSet.empty
  }
  it should "have a collect method that only accepts functions that result in the path-enclosed type" in {
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    val result1 = number.SortedEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i % 2 == 0 => i * 2 }
    result1 shouldBe number.SortedEquaSet(4, 8, 12, 16, 20)
    result1.shouldHaveExactType[number.SortedEquaSet]
    number.SortedEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i > 10 => i * 2 } shouldBe number.SortedEquaSet.empty
    val result2 = number.TreeEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i % 2 == 0 => i * 2 }
    result2 shouldBe number.TreeEquaSet(4, 8, 12, 16, 20)
    result2.shouldHaveExactType[number.TreeEquaSet]
  }
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Boolean = number.SortedEquaSet(1, 2, 3).compose(_ + 1)
    fn(0) shouldBe true
    fn(1) shouldBe true
    fn(2) shouldBe true
    fn(3) shouldBe false
  }
  it should "have an contains method that does a type check" in {
    val e = number.SortedEquaSet(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    """e.contains("five")""" shouldNot typeCheck
    new CheckedEquality {
      val es = lower.SortedEquaSet("one", "two", "three")
      """es.contains(5)""" shouldNot typeCheck
      es.contains("ONE") shouldBe true;
    }
    abstract class Fruit {
      val name: String
    }
    case class Apple(name: String) extends Fruit
    case class Orange(name: String) extends Fruit
    val mac = Apple("Mcintosh")
    val navel = Orange("Navel") 
    val equalityOfFruit =
      new OrderingEquality[Fruit] {
        private val nameEquality = StringNormalizations.lowerCased.toOrderingEquality
        def areEqual(a: Fruit, b: Any): Boolean =
          b match {
            case bFruit: Fruit => nameEquality.areEqual(a.name, bFruit.name)
            case _ => false
          }
        def hashCodeFor(a: Fruit): Int = nameEquality.hashCodeFor(a.name)
        def compare(a: Fruit, b: Fruit): Int = nameEquality.compare(a.name, b.name)
      }
    val fruitEquaSets = SortedEquaSets(equalityOfFruit)
    val fruits = fruitEquaSets.SortedEquaSet(mac, navel)
    fruits.contains(mac) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(number.EquaBox(-1))
    number.SortedEquaSet(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3), number.EquaBox(4), number.EquaBox(5))

    val arr2 = Array.fill(5)(number.EquaBox(-1))
    number.SortedEquaSet(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(number.EquaBox(-1), number.EquaBox(1), number.EquaBox(2), number.EquaBox(3), number.EquaBox(4))

    val arr3 = Array.fill(5)(number.EquaBox(-1))
    number.SortedEquaSet(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(number.EquaBox(-1), number.EquaBox(1), number.EquaBox(2), number.EquaBox(-1), number.EquaBox(-1))
  }
  it should "have a copyToBuffer method" in {
    val buf = ListBuffer.fill(3)(number.EquaBox(-1))
    number.SortedEquaSet(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(number.EquaBox(-1), number.EquaBox(-1), number.EquaBox(-1), number.EquaBox(1), number.EquaBox(2), number.EquaBox(3), number.EquaBox(4), number.EquaBox(5))
  }
  it should "have a count method" in {
    val set = number.SortedEquaSet(1, 2, 3, 4, 5)
    set.count(_ > 10) shouldBe 0
    set.count(_ % 2 == 0) shouldBe 2
    set.count(_ % 2 == 1) shouldBe 3
  }
  it should "have a drop method" in {
    val set = number.SortedEquaSet(1, 2, 3, 4, 5)
    set.drop(0) shouldBe number.EquaSet(1, 2, 3, 4, 5)
    set.drop(1) shouldBe number.EquaSet(2, 3, 4, 5)
    set.drop(2) shouldBe number.EquaSet(3, 4, 5)
    set.drop(3) shouldBe number.EquaSet(4, 5)
    set.drop(4) shouldBe number.EquaSet(5)
    set.drop(5) shouldBe number.EquaSet()
  }
  it should "have a dropRight method" in {
    val set = number.SortedEquaSet(1, 2, 3, 4, 5)
    set.dropRight(0) shouldBe number.EquaSet(1, 2, 3, 4, 5)
    set.dropRight(1) shouldBe number.EquaSet(1, 2, 3, 4)
    set.dropRight(2) shouldBe number.EquaSet(1, 2, 3)
    set.dropRight(3) shouldBe number.EquaSet(1, 2)
    set.dropRight(4) shouldBe number.EquaSet(1)
    set.dropRight(5) shouldBe number.EquaSet()
  }
  it should "have a dropWhile method" in {
    val set = number.SortedEquaSet(1, 2, 3, 4, 5)
    set.dropWhile(_ < 1) shouldBe number.EquaSet(1, 2, 3, 4, 5)
    set.dropWhile(_ < 2) shouldBe number.EquaSet(2, 3, 4, 5)
    set.dropWhile(_ < 3) shouldBe number.EquaSet(3, 4, 5)
    set.dropWhile(_ < 4) shouldBe number.EquaSet(4, 5)
    set.dropWhile(_ < 5) shouldBe number.EquaSet(5)
    set.dropWhile(_ < 6) shouldBe number.EquaSet()
  }
  it should "have an enclosingEquaSets method" in {
    trimmed.EquaSet("hi").enclosingEquaSets shouldBe trimmed
    trimmed.FastEquaSet("hi").enclosingEquaSets shouldBe trimmed
    trimmed.SortedEquaSet("hi").enclosingEquaSets shouldBe trimmed
    trimmed.TreeEquaSet("hi").enclosingEquaSets shouldBe trimmed
  }
  it should "have an exists method" in {
    number.SortedEquaSet(1, 2, 3).exists(_ == 2) shouldBe true
    number.SortedEquaSet(1, 2, 3).exists(_ == 5) shouldBe false
  }
  it should "have a filter method" in {
    val set = number.SortedEquaSet(1, 2, 3)
    set.filter(_ == 1) shouldBe number.SortedEquaSet(1)
    set.filter(_ == 2) shouldBe number.SortedEquaSet(2)
    set.filter(_ == 3) shouldBe number.SortedEquaSet(3)
  }
  it should "have a filterNot method" in {
    val set = number.SortedEquaSet(1, 2, 3)
    set.filterNot(_ == 1) shouldBe number.SortedEquaSet(2, 3)
    set.filterNot(_ == 2) shouldBe number.SortedEquaSet(1, 3)
    set.filterNot(_ == 3) shouldBe number.SortedEquaSet(1, 2)
  }
  it should "have a find method" in {
    number.SortedEquaSet(1, 2, 3).find(_ == 5) shouldBe None
    number.SortedEquaSet(1, 2, 3).find(_ == 2) shouldBe Some(number.EquaBox(2))
  }
  it should "have an into.flatMap method" in {

    // SortedEquaSet into EquaSets => EquaSet
    val result1 = number.SortedEquaSet(7, 8, 9).into(plainLower).flatMap(i => plainLower.EquaSet(i.toString))
    result1 shouldBe plainLower.EquaSet("7", "8", "9")
    result1.shouldHaveExactType[plainLower.EquaSet]

    // SortedEquaSet into SortedEquaSets => SortedEquaSet
    val result2 = number.SortedEquaSet(7, 8, 9).into(sortedLower).flatMap(i => sortedLower.SortedEquaSet(i.toString))
    result2 shouldBe sortedLower.SortedEquaSet("7", "8", "9")
    result2.shouldHaveExactType[sortedLower.SortedEquaSet]

    // TreeEquaSet into EquaSets => EquaSet
    val result3 = number.TreeEquaSet(7, 8, 9).into(plainLower).flatMap(i => plainLower.EquaSet(i.toString))
    result3 shouldBe plainLower.EquaSet("7", "8", "9")
    result3.shouldHaveExactType[plainLower.EquaSet]

    // TreeEquaSet into SortedEquaSets => TreeEquaSet
    val result4 = number.TreeEquaSet(7, 8, 9).into(sortedLower).flatMap(i => sortedLower.TreeEquaSet(i.toString))
    result4 shouldBe sortedLower.TreeEquaSet("7", "8", "9")
    result4.shouldHaveExactType[sortedLower.TreeEquaSet]

    // Extra stuff from oldInto test
    val nonSortedlower = EquaSets[String](StringNormalizations.lowerCased.toOrderingEquality)
    number.SortedEquaSet(8).into(nonSortedlower).flatMap(i => nonSortedlower.EquaSet(i.toString)) shouldBe nonSortedlower.EquaSet("8")
    number.SortedEquaSet(8).into(sortedLower).flatMap(i => sortedLower.SortedEquaSet(i.toString)) shouldBe sortedLower.SortedEquaSet("8")
  }
  it should "have a flatMap method" in {
    number.SortedEquaSet(1, 2, 3) flatMap (i => number.SortedEquaSet(i + 1)) shouldBe number.SortedEquaSet(2, 3, 4)
    number.SortedEquaSet(5) flatMap (i => number.SortedEquaSet(i + 3)) shouldBe number.SortedEquaSet(8)
    val ss = number.SortedEquaSet(1, 2)
    val is = number.SortedEquaSet(1, 2, 3)
    (for (s <- ss; i <- is) yield s + i) shouldBe number.SortedEquaSet(2, 3, 4, 3, 4, 5)
  }
  it should "have an into.flatten method that works on nested EquaSet" in {

    implicit def plainOrdering: Ordering[plainNumber.EquaSet] =
      new Ordering[plainNumber.EquaSet] {
        def compare(x: plainNumber.EquaSet, y: plainNumber.EquaSet): Int = x.size - y.size
      }
    implicit def plainFastOrdering: Ordering[plainNumber.FastEquaSet] =
      new Ordering[plainNumber.FastEquaSet] {
        def compare(x: plainNumber.FastEquaSet, y: plainNumber.FastEquaSet): Int = x.size - y.size
      }
    implicit def sortedOrdering: Ordering[sortedNumber.EquaSet] =
      new Ordering[sortedNumber.EquaSet] {
        def compare(x: sortedNumber.EquaSet, y: sortedNumber.EquaSet): Int = x.size - y.size
      }
    implicit def sortedSortedOrdering: Ordering[sortedNumber.SortedEquaSet] =
      new Ordering[sortedNumber.SortedEquaSet] {
        def compare(x: sortedNumber.SortedEquaSet, y: sortedNumber.SortedEquaSet): Int = x.size - y.size
      }
    // SortedEquaSet into EquaSets => EquaSet
    val numberNumber1 = SortedEquaSets[plainNumber.EquaSet](normalOrderingEquality[plainNumber.EquaSet])
    val result1 = numberNumber1.SortedEquaSet(plainNumber.EquaSet(1, 2), plainNumber.EquaSet(3)).into(plainNumber).flatten
    result1 shouldBe plainNumber.EquaSet(1, 2, 3)
    result1.shouldHaveExactType[plainNumber.EquaSet]

    // SortedEquaSet into SortedEquaSets => SortedEquaSet
    val numberNumber2 = SortedEquaSets[sortedNumber.SortedEquaSet](normalOrderingEquality[sortedNumber.SortedEquaSet])
    val result2 = numberNumber2.SortedEquaSet(sortedNumber.SortedEquaSet(1, 2), sortedNumber.SortedEquaSet(3)).into(sortedNumber).flatten
    result2 shouldBe sortedNumber.SortedEquaSet(1, 2, 3)
    result2.shouldHaveExactType[sortedNumber.SortedEquaSet]

    // TreeEquaSet into EquaSets => EquaSet
    val numberNumber3 = SortedEquaSets[plainNumber.FastEquaSet](normalOrderingEquality[plainNumber.FastEquaSet])
    val result3 = numberNumber3.TreeEquaSet(plainNumber.FastEquaSet(1, 2), plainNumber.FastEquaSet(3)).into(plainNumber).flatten // What about into EquaSets.EquaSet?
    result3 shouldBe plainNumber.EquaSet(1, 2, 3)
    result3.shouldHaveExactType[plainNumber.EquaSet]

    // TreeEquaSet into SortedEquaSets => TreeEquaSet
    val numberNumber4 = SortedEquaSets[sortedNumber.EquaSet](normalOrderingEquality[sortedNumber.EquaSet])
    val result4 = numberNumber4.TreeEquaSet(sortedNumber.EquaSet(1, 2), sortedNumber.EquaSet(3)).into(sortedNumber).flatten
    result4 shouldBe sortedNumber.TreeEquaSet(1, 2, 3)
    result4.shouldHaveExactType[sortedNumber.TreeEquaSet]

    // Extra stuff from oldInto test
    numberNumber.SortedEquaSet(number.SortedEquaSet(1, 2), number.SortedEquaSet(3)).into(number).flatten shouldBe number.SortedEquaSet(1, 2, 3)
    numberNumber.SortedEquaSet(number.SortedEquaSet(1)).into(number).flatten shouldBe number.SortedEquaSet(1)
  }
  it can "be flattened when in a GenTraversableOnce" in {
    // need to keep this commented out until finish implementing all methods
    Vector(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(1, 2, 3)).flatten shouldBe List(1, 2, 3, 1, 2, 3)
    List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(1, 2, 3)).toIterator.flatten.toStream shouldBe List(1, 2, 3, 1, 2, 3).toIterator.toStream
    List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(1, 2, 3)).par.flatten shouldBe List(1, 2, 3, 1, 2, 3).par
  }
  it should "have a flatten method that works on nested GenTraversable" in {
    numberList.SortedEquaSet(List(1, 2), List(3)).flatten shouldBe List(1, 2, 3)
    numberList.SortedEquaSet(List(1)).flatten shouldBe List(1)
  }
  it should "have a fold method" in {
    number.SortedEquaSet(1).fold(0)(_ + _) shouldBe 1
    number.SortedEquaSet(1).fold(1)(_ * _) shouldBe 1
    number.SortedEquaSet(2).fold(0)(_ + _) shouldBe 2
    number.SortedEquaSet(2).fold(1)(_ * _) shouldBe 2
    number.SortedEquaSet(3).fold(0)(_ + _) shouldBe 3
    number.SortedEquaSet(3).fold(1)(_ * _) shouldBe 3
    number.SortedEquaSet(1, 2, 3).fold(0)(_ + _) shouldBe 6
    number.SortedEquaSet(1, 2, 3).fold(1)(_ * _) shouldBe 6
    number.SortedEquaSet(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    number.SortedEquaSet(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    number.SortedEquaSet(1).foldLeft(0)(_ + _) shouldBe 1
    number.SortedEquaSet(1).foldLeft(1)(_ + _) shouldBe 2
    number.SortedEquaSet(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    number.SortedEquaSet(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    number.SortedEquaSet(1).foldRight(0)(_ + _) shouldBe 1
    number.SortedEquaSet(1).foldRight(1)(_ + _) shouldBe 2
    number.SortedEquaSet(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    number.SortedEquaSet(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    number.SortedEquaSet(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    number.SortedEquaSet(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- number.EquaSet(1, 2, 3))
      num += i
    num shouldBe 12
    number.SortedEquaSet(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).groupBy(_ % 2) shouldBe Map(1 -> number.SortedEquaSet(1, 3, 5), 0 -> number.SortedEquaSet(2, 4))
    number.SortedEquaSet(1, 2, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> number.SortedEquaSet(1, 3, 3, 3), 0 -> number.SortedEquaSet(2))
    number.SortedEquaSet(1, 1, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> number.SortedEquaSet(1, 1, 3, 3, 3))
    number.SortedEquaSet(1, 2, 3, 5, 7).groupBy(_ % 2) shouldBe Map(1 -> number.SortedEquaSet(1, 3, 5, 7), 0 -> number.SortedEquaSet(2))
  }
  it should "have a grouped method" in {
    number.SortedEquaSet(1, 2, 3).grouped(2).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(3))
    number.SortedEquaSet(1, 2, 3).grouped(1).toList shouldBe List(number.SortedEquaSet(1), number.SortedEquaSet(2), number.SortedEquaSet(3))
    an [IllegalArgumentException] should be thrownBy { number.SortedEquaSet(1, 2, 3).grouped(0).toList }
    number.SortedEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(3, 4), number.SortedEquaSet(5, 6), number.SortedEquaSet(7, 8), number.SortedEquaSet(9, 10))
    number.SortedEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toList shouldBe List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(4, 5, 6), number.SortedEquaSet(7, 8, 9), number.SortedEquaSet(10))
    number.SortedEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toList shouldBe List(number.SortedEquaSet(1, 2, 3, 4), number.SortedEquaSet(5, 6, 7, 8), number.SortedEquaSet(9, 10))
    number.SortedEquaSet(1).grouped(2).toList shouldBe List(number.SortedEquaSet(1))
    number.SortedEquaSet(1).grouped(1).toList shouldBe List(number.SortedEquaSet(1))
  }
  it should "have a hasDefiniteSize method" in {
    number.SortedEquaSet(1).hasDefiniteSize shouldBe true
    number.SortedEquaSet(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a head method" in {
    lower.SortedEquaSet("hi").head shouldBe "hi"
    number.SortedEquaSet(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    lower.SortedEquaSet("hi").headOption shouldBe Some("hi")
    number.SortedEquaSet(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have an init method" in {
    number.SortedEquaSet(1, 2, 3).init shouldBe number.SortedEquaSet(1, 2)
  }
  it should "have an inits method" in {
    val inits = number.SortedEquaSet(1, 2, 3).inits
    inits.next shouldBe number.SortedEquaSet(1,2,3)
    inits.next shouldBe number.SortedEquaSet(1,2)
    inits.next shouldBe number.SortedEquaSet(1)
    inits.next shouldBe number.SortedEquaSet()
    inits.hasNext shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    lower.SortedEquaSet("hi").isTraversableAgain shouldBe true
    number.SortedEquaSet(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have a last method" in {
    lower.SortedEquaSet("hi").last shouldBe "hi"
    number.SortedEquaSet(1, 2, 3).last shouldBe 3
  }
  it should "have an lastOption method" in {
    lower.SortedEquaSet("hi").lastOption shouldBe Some("hi")
    number.SortedEquaSet(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an into.map method" in {
    // Can map directly if want to stay in same SortedEquaSets
    number.SortedEquaSet(1, 2, 3).map(_ + 1) shouldBe number.SortedEquaSet(2, 3, 4)
    (for (ele <- number.SortedEquaSet(1, 2, 3)) yield ele * 2) shouldBe number.SortedEquaSet(2, 4, 6)
    number.SortedEquaSet(5) map (_ + 3) shouldBe number.SortedEquaSet(8)

    // Can map into self explicitly too
    number.SortedEquaSet(1, 2, 3).into(number).map(_ + 1) shouldBe number.SortedEquaSet(2, 3, 4)
    number.SortedEquaSet(5).into(number).map(_ + 3) shouldBe number.SortedEquaSet(8)

    // SortedEquaSet into EquaSets => EquaSet
    val result1 = number.SortedEquaSet(7, 8, 9).into(plainLower).map(_.toString)
    result1 shouldBe plainLower.EquaSet("7", "8", "9")
    result1.shouldHaveExactType[plainLower.EquaSet]

    // SortedEquaSet into SortedEquaSets => SortedEquaSet
    val result2 = number.SortedEquaSet(7, 8, 9).into(sortedLower).map(_.toString)
    result2 shouldBe sortedLower.SortedEquaSet("7", "8", "9")
    result2.shouldHaveExactType[sortedLower.SortedEquaSet]

    // TreeEquaSet into EquaSets => EquaSet
    val result3 = number.TreeEquaSet(7, 8, 9).into(plainLower).map(_.toString)
    result3 shouldBe plainLower.EquaSet("7", "8", "9")
    result3.shouldHaveExactType[plainLower.EquaSet]

    // TreeEquaSet into SortedEquaSets => TreeEquaSet
    val result4 = number.TreeEquaSet(7, 8, 9).into(sortedLower).map(_.toString)
    result4 shouldBe sortedLower.TreeEquaSet("7", "8", "9")
    result4.shouldHaveExactType[sortedLower.TreeEquaSet]

    // Extra stuff from oldInto test
    number.SortedEquaSet(1, 2, 3).into(number).map(_ + 1) shouldBe number.SortedEquaSet(2, 3, 4)
    number.SortedEquaSet(5).into(number).map(_ + 3) shouldBe number.SortedEquaSet(8)
    number.SortedEquaSet(8).into(lower).map(_.toString) shouldBe lower.SortedEquaSet("8")
  }
  it should "have a map method" in {
    number.SortedEquaSet(1, 2, 3) .map (_ + 1) shouldBe number.SortedEquaSet(2, 3, 4)
    (for (ele <- number.SortedEquaSet(1, 2, 3)) yield ele * 2) shouldBe number.SortedEquaSet(2, 4, 6)
    number.SortedEquaSet(5) map (_ + 3) shouldBe number.SortedEquaSet(8)
  }
  it should "have a max method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).max shouldBe 5
    number.SortedEquaSet(1).max shouldBe 1
    number.SortedEquaSet(-1).max shouldBe -1
    lower.SortedEquaSet("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    number.SortedEquaSet(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).min shouldBe 1
    number.SortedEquaSet(1).min shouldBe 1
    number.SortedEquaSet(-1).min shouldBe -1
    lower.SortedEquaSet("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    number.SortedEquaSet(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a 3 mkString method" in {

    lower.SortedEquaSet("hi").mkString shouldBe "hi"
    number.SortedEquaSet(1, 2, 3).mkString shouldBe "123"

    lower.SortedEquaSet("hi").mkString("#") shouldBe "hi"
    number.SortedEquaSet(1, 2, 3).mkString("#") shouldBe "1#2#3"
    number.SortedEquaSet(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    lower.SortedEquaSet("hi").mkString("<", "#", ">") shouldBe "<hi>"
    number.SortedEquaSet(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    number.SortedEquaSet(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    lower.SortedEquaSet("hi").nonEmpty shouldBe true
    number.SortedEquaSet(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have a partition method" in {
    number.SortedEquaSet(1, 2, 3, 4).partition(_ < 3) shouldBe (number.SortedEquaSet(1, 2), number.SortedEquaSet(3, 4))
  }
  it should "have a product method" in {
    number.SortedEquaSet(1, 2, 3).product shouldBe 6
    number.SortedEquaSet(3).product shouldBe 3
    number.SortedEquaSet(3, 4, 5).product shouldBe 60
    number.SortedEquaSet(3, 4, 5).product shouldBe 60
  }
  it should "have a reduce method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    number.SortedEquaSet(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    number.SortedEquaSet(5).reduce(_ + _) shouldBe 5
    number.SortedEquaSet(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    number.SortedEquaSet(1).reduceLeft(_ + _) shouldBe 1
    number.SortedEquaSet(1).reduceLeft(_ * _) shouldBe 1
    number.SortedEquaSet(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    number.SortedEquaSet(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    number.SortedEquaSet(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    number.SortedEquaSet(1).reduceLeftOption(_ + _) shouldBe Some(1)
    number.SortedEquaSet(1).reduceLeftOption(_ * _) shouldBe Some(1)
    number.SortedEquaSet(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    number.SortedEquaSet(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    number.SortedEquaSet(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    number.SortedEquaSet(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    number.SortedEquaSet(5).reduceOption(_ + _) shouldBe Some(5)
    number.SortedEquaSet(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    number.SortedEquaSet(1).reduceRight(_ * _) shouldBe 1
    number.SortedEquaSet(1, 2, 3).reduceRight(_ + _) shouldBe 6
    number.SortedEquaSet(1, 2, 3).reduceRight(_ * _) shouldBe 6
    number.SortedEquaSet(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    number.SortedEquaSet(1).reduceRightOption(_ + _) shouldBe Some(1)
    number.SortedEquaSet(1).reduceRightOption(_ * _) shouldBe Some(1)
    number.SortedEquaSet(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    number.SortedEquaSet(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    number.SortedEquaSet(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a repr method" in {
    implicit val numberOrdering = new Ordering[number.EquaBox] {
      def compare(x: number.EquaBox, y: number.EquaBox): Int = x.value - y.value
    }
    number.SortedEquaSet(1, 2, 3).repr shouldBe SortedSet(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a sameElements method that takes a GenIterable" in {
    number.SortedEquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5)) shouldBe true
    number.SortedEquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4)) shouldBe false
    number.SortedEquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5, 6)) shouldBe false
    number.SortedEquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 4)) shouldBe false
    number.SortedEquaSet(3).sameElements(List(1, 2, 3, 4, 5)) shouldBe false
    number.SortedEquaSet(3).sameElements(List(1)) shouldBe false
    number.SortedEquaSet(3).sameElements(List(3)) shouldBe true
  }
  it should "have a scanLeft method" in {
    number.SortedEquaSet(1).scanLeft(0)(_ + _) shouldBe number.SortedEquaSet(0, 1)
    number.SortedEquaSet(1, 2, 3).scanLeft(0)(_ + _) shouldBe number.SortedEquaSet(0, 1, 3, 6)
  }
  it should "have an into.scanLeft method" in {

    // SortedEquaSet into EquaSets => EquaSet
    val result1 = number.SortedEquaSet(7, 8, 9).into(plainLower).scanLeft("z")(_ + _)
    result1 shouldBe plainLower.EquaSet("z", "z7", "z78", "z789")
    result1.shouldHaveExactType[plainLower.EquaSet]

    // SortedEquaSet into SortedEquaSets => SortedEquaSet
    val result2 = number.SortedEquaSet(7, 8, 9).into(sortedLower).scanLeft("z")(_ + _)
    result2 shouldBe sortedLower.SortedEquaSet("z", "z7", "z78", "z789")
    result2.shouldHaveExactType[sortedLower.SortedEquaSet]

    // TreeEquaSet into EquaSets => EquaSet
    val result3 = number.TreeEquaSet(7, 8, 9).into(plainLower).scanLeft("z")(_ + _)
    result3 shouldBe plainLower.EquaSet("z", "z7", "z78", "z789")
    result3.shouldHaveExactType[plainLower.EquaSet]

    // TreeEquaSet into SortedEquaSets => TreeEquaSet
    val result4 = number.TreeEquaSet(7, 8, 9).into(sortedLower).scanLeft("z")(_ + _)
    result4 shouldBe sortedLower.TreeEquaSet("z", "z7", "z78", "z789")
    result4.shouldHaveExactType[sortedLower.TreeEquaSet]

    // Extra stuff from oldInto test
    number.SortedEquaSet(1, 2, 3).into(lower).scanLeft("z")(_ + _) shouldBe lower.SortedEquaSet("z", "z1", "z12", "z123")
    number.SortedEquaSet(0).into(lower).scanLeft("z")(_ + _) shouldBe lower.SortedEquaSet("z", "z0")
  }
  it should "have a scanRight method" in {
    number.SortedEquaSet(1).scanRight(0)(_ + _) shouldBe number.SortedEquaSet(1, 0)
    number.SortedEquaSet(1, 2, 3).scanRight(0)(_ + _) shouldBe number.SortedEquaSet(6, 5, 3, 0)
  }
  it should "have an into.scanRight method" in {

    // SortedEquaSet into EquaSets => EquaSet
    val result1 = number.SortedEquaSet(7, 8, 9).into(plainLower).scanRight("z")(_ + _)
    result1 shouldBe plainLower.EquaSet("789z", "89z", "9z", "z")
    result1.shouldHaveExactType[plainLower.EquaSet]

    // SortedEquaSet into SortedEquaSets => SortedEquaSet
    val result2 = number.SortedEquaSet(7, 8, 9).into(sortedLower).scanRight("z")(_ + _)
    result2 shouldBe sortedLower.SortedEquaSet("789z", "89z", "9z", "z")
    result2.shouldHaveExactType[sortedLower.SortedEquaSet]

    // TreeEquaSet into EquaSets => EquaSet
    val result3 = number.TreeEquaSet(7, 8, 9).into(plainLower).scanRight("z")(_ + _)
    result3 shouldBe plainLower.EquaSet("789z", "89z", "9z", "z")
    result3.shouldHaveExactType[plainLower.EquaSet]

    // TreeEquaSet into SortedEquaSets => TreeEquaSet
    val result4 = number.TreeEquaSet(7, 8, 9).into(sortedLower).scanRight("z")(_ + _)
    result4 shouldBe sortedLower.TreeEquaSet("789z", "89z", "9z", "z")
    result4.shouldHaveExactType[sortedLower.TreeEquaSet]

    // Extra stuff from oldInto test
    number.SortedEquaSet(1, 2, 3).into(lower).scanRight("z")(_ + _) shouldBe lower.SortedEquaSet("123z", "23z", "3z", "z")
    number.SortedEquaSet(0).into(lower).scanRight("z")(_ + _) shouldBe lower.SortedEquaSet("0z", "z")
  }
  it should "have a slice method" in {
    number.SortedEquaSet(3).slice(0, 0) shouldBe number.SortedEquaSet()
    number.SortedEquaSet(1, 2, 3).slice(2, 1) shouldBe number.SortedEquaSet()
    number.SortedEquaSet(1, 2, 3).slice(1, 3) shouldBe number.SortedEquaSet(2, 3)
  }
  it should "have 2 sliding methods" in {

    number.SortedEquaSet(1).sliding(1).toList shouldBe List(number.SortedEquaSet(1))
    number.SortedEquaSet(1).sliding(2).toList shouldBe List(number.SortedEquaSet(1))
    number.SortedEquaSet(1, 2, 3).sliding(2).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(2, 3))
    number.SortedEquaSet(1, 2, 3).sliding(1).toList shouldBe List(number.SortedEquaSet(1), number.EquaSet(2), number.SortedEquaSet(3))
    number.SortedEquaSet(1, 2, 3).sliding(3).toList shouldBe List(number.SortedEquaSet(1, 2, 3))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(3).toList shouldBe List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(2, 3, 4), number.SortedEquaSet(3, 4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(2).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(2, 3), number.SortedEquaSet(3, 4), number.SortedEquaSet(4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(1).toList shouldBe List(number.SortedEquaSet(1), number.SortedEquaSet(2), number.SortedEquaSet(3), number.SortedEquaSet(4), number.SortedEquaSet(5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(4).toList shouldBe List(number.SortedEquaSet(1, 2, 3, 4), number.SortedEquaSet(2, 3, 4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(5).toList shouldBe List(number.SortedEquaSet(1, 2, 3, 4, 5))

    number.SortedEquaSet(1).sliding(1, 1).toList shouldBe List(number.SortedEquaSet(1))
    number.SortedEquaSet(1).sliding(1, 2).toList shouldBe List(number.SortedEquaSet(1))
    number.SortedEquaSet(1, 2, 3).sliding(1, 1).toList shouldBe List(number.SortedEquaSet(1), number.SortedEquaSet(2), number.SortedEquaSet(3))
    number.SortedEquaSet(1, 2, 3).sliding(2, 1).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(2, 3))
    number.SortedEquaSet(1, 2, 3).sliding(2, 2).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(3))
    number.SortedEquaSet(1, 2, 3).sliding(3, 2).toList shouldBe List(number.SortedEquaSet(1, 2, 3))
    number.SortedEquaSet(1, 2, 3).sliding(3, 1).toList shouldBe List(number.SortedEquaSet(1, 2, 3))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(2, 3, 4), number.SortedEquaSet(3, 4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(2, 2).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(3, 4), number.SortedEquaSet(5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(2, 3).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(2, 4).toList shouldBe List(number.SortedEquaSet(1, 2), number.SortedEquaSet(5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(2, 3, 4), number.SortedEquaSet(3, 4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(3, 2).toList shouldBe List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(3, 4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(3, 3).toList shouldBe List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(4, 5))
    number.SortedEquaSet(1, 2, 3, 4, 5).sliding(3, 4).toList shouldBe List(number.SortedEquaSet(1, 2, 3), number.SortedEquaSet(5))
  }
  it should "have a span method" in {
    number.SortedEquaSet(1, 2, 3).span(_ < 3) shouldBe (number.SortedEquaSet(1, 2), number.SortedEquaSet(3))
    number.SortedEquaSet(1, 2, 3).span(_ > 3) shouldBe (number.SortedEquaSet(), number.SortedEquaSet(1, 2, 3))
  }
  it should "have a splitAt method" in {
    number.SortedEquaSet(1, 2, 3).splitAt(0) shouldBe (number.SortedEquaSet(), number.SortedEquaSet(1, 2, 3))
    number.SortedEquaSet(1, 2, 3).splitAt(1) shouldBe (number.SortedEquaSet(1), number.SortedEquaSet(2, 3))
    number.SortedEquaSet(1, 2, 3).splitAt(2) shouldBe (number.SortedEquaSet(1, 2), number.SortedEquaSet(3))
    number.SortedEquaSet(1, 2, 3).splitAt(3) shouldBe (number.SortedEquaSet(1, 2, 3), number.SortedEquaSet())
  }
  it should "have a stringPrefix method" in {
    number.SortedEquaSet(1).stringPrefix shouldBe "TreeEquaSet"
    number.SortedEquaSet(1, 2, 3).stringPrefix shouldBe "TreeEquaSet"
    lower.SortedEquaSet("1").stringPrefix shouldBe "TreeEquaSet"
    lower.SortedEquaSet("1", "2", "3").stringPrefix shouldBe "TreeEquaSet"
  }
  it should "have a subsetOf method" in {
    number.SortedEquaSet(2, 3).subsetOf(number.SortedEquaSet(1, 2, 3, 4, 5)) shouldBe true
    number.SortedEquaSet(2).subsetOf(number.SortedEquaSet(1, 2, 3, 4, 5)) shouldBe true
    number.SortedEquaSet(2, 0).subsetOf(number.SortedEquaSet(1, 2, 3, 4, 5)) shouldBe false
    lower.SortedEquaSet("aa", "bb").subsetOf(lower.SortedEquaSet("aa", "bb", "cc")) shouldBe true
    lower.SortedEquaSet("aA", "Bb").subsetOf(lower.SortedEquaSet("aa", "bb", "cc")) shouldBe true
    lower.SortedEquaSet("aa", "bb").subsetOf(lower.SortedEquaSet("aA", "Bb", "cC")) shouldBe true
    lower.SortedEquaSet("aa", "bc").subsetOf(lower.SortedEquaSet("aa", "bb", "cc")) shouldBe false
  }
  it should "have a subsets method" in {
    val subsets = number.SortedEquaSet(1, 2, 3).subsets.toList
    subsets should have length 8
    subsets should contain (number.SortedEquaSet())
    subsets should contain (number.SortedEquaSet(1))
    subsets should contain (number.SortedEquaSet(2))
    subsets should contain (number.SortedEquaSet(3))
    subsets should contain (number.SortedEquaSet(1, 2))
    subsets should contain (number.SortedEquaSet(1, 3))
    subsets should contain (number.SortedEquaSet(2, 3))
    subsets should contain (number.SortedEquaSet(1, 2, 3))

    val subsets2 = number.SortedEquaSet(1, 2, 3).subsets(2).toList
    subsets2 should have length 3
    subsets2 should contain (number.SortedEquaSet(1, 2))
    subsets2 should contain (number.SortedEquaSet(1, 3))
    subsets2 should contain (number.SortedEquaSet(2, 3))
  }
  it should "have a sum method" in {
    number.SortedEquaSet(1).sum shouldBe 1
    number.SortedEquaSet(5).sum shouldBe 5
    number.SortedEquaSet(1, 2, 3).sum shouldBe 6
    number.SortedEquaSet(1, 2, 3, 4, 5).sum shouldBe 15
  }
  it should "have an tail method" in {
    number.SortedEquaSet(1, 2, 3).tail shouldBe number.SortedEquaSet(2, 3)
  }
  it should "have an tails method" in {
    number.SortedEquaSet(1, 2, 3).tails.toList shouldBe List(number.SortedEquaSet(1,2,3), number.SortedEquaSet(2,3), number.SortedEquaSet(3), number.SortedEquaSet())
  }
  it should "have a take method" in {
    number.SortedEquaSet(1, 2, 3).take(0) shouldBe number.SortedEquaSet()
    number.SortedEquaSet(1, 2, 3).take(1) shouldBe number.SortedEquaSet(1)
    number.SortedEquaSet(1, 2, 3).take(2) shouldBe number.SortedEquaSet(1, 2)
    number.SortedEquaSet(1, 2, 3).take(3) shouldBe number.SortedEquaSet(1, 2, 3)
  }
  it should "have a takeRight method" in {
    number.SortedEquaSet(1, 2, 3).takeRight(0) shouldBe number.SortedEquaSet()
    number.SortedEquaSet(1, 2, 3).takeRight(1) shouldBe number.SortedEquaSet(3)
    number.SortedEquaSet(1, 2, 3).takeRight(2) shouldBe number.SortedEquaSet(2, 3)
    number.SortedEquaSet(1, 2, 3).takeRight(3) shouldBe number.SortedEquaSet(1, 2, 3)
  }
  it should "have a to method" in {
    number.SortedEquaSet(1).to[List] shouldBe List(number.EquaBox(1))
    number.SortedEquaSet(1, 2, 3).to[List] shouldBe List(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
    number.SortedEquaSet(1, 2, 3).to[scala.collection.mutable.ListBuffer] shouldBe ListBuffer(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
    number.SortedEquaSet(1, 2, 3).to[Vector] shouldBe Vector(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a toArray method" in {
    number.SortedEquaSet(1, 2, 3).toArray shouldBe (Array(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toArray shouldBe (Array(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toArray shouldBe (Array(number.EquaBox(1)))
  }
  it should "have a toBuffer method" in {
    number.SortedEquaSet(1, 2, 3).toBuffer shouldBe (Buffer(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toBuffer shouldBe (Buffer(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toBuffer shouldBe (Buffer(number.EquaBox(1)))
  }
  it should "have a toIndexedSeq method" in {
    number.SortedEquaSet(1, 2, 3).toIndexedSeq shouldBe (IndexedSeq(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toIndexedSeq shouldBe (IndexedSeq(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toIndexedSeq shouldBe (IndexedSeq(number.EquaBox(1)))
  }
  it should "have a toIterable method" in {
    number.SortedEquaSet(1, 2, 3).toIterable shouldBe (Set(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toIterable shouldBe (Set(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toIterable shouldBe (Set(number.EquaBox(1)))
  }
  it should "have a toIterator method" in {
    number.SortedEquaSet(1, 2, 3).toIterator.toList shouldBe (Iterator(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)).toList)
    lower.SortedEquaSet("a", "b").toIterator.toList shouldBe (Iterator(lower.EquaBox("a"), lower.EquaBox("b")).toList)
    number.SortedEquaSet(1).toIterator.toList shouldBe (Iterator(number.EquaBox(1)).toList)
    number.SortedEquaSet(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    lower.SortedEquaSet("a", "b").toIterator shouldBe an [Iterator[_]]
    number.SortedEquaSet(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    number.SortedEquaSet(1, 2, 3).toList shouldBe (List(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toList shouldBe (List(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toList shouldBe (List(number.EquaBox(1)))
  }
  it should "have a toSeq method" in {
    number.SortedEquaSet(1, 2, 3).toSeq shouldBe (Seq(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toSeq shouldBe (Seq(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toSeq shouldBe (Seq(number.EquaBox(1)))
  }
  it should "have a toStream method" in {
    number.SortedEquaSet(1, 2, 3).toStream shouldBe (Stream(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toStream shouldBe (Stream(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toStream shouldBe(Stream(number.EquaBox(1)))
  }
  it should "have a toTraversable method" in {
    implicit val numberOrdering = new Ordering[number.EquaBox] {
      def compare(x: number.EquaBox, y: number.EquaBox): Int = x.value - y.value
    }
    implicit val lowerOrdering = new Ordering[lower.EquaBox] {
      def compare(x: lower.EquaBox, y: lower.EquaBox): Int = x.value compareTo y.value
    }
    number.SortedEquaSet(1, 2, 3).toTraversable should === (TreeSet(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toTraversable should === (TreeSet(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toTraversable should === (TreeSet(number.EquaBox(1)))
  }
  it should "have a toVector method" in {
    number.SortedEquaSet(1, 2, 3).toVector should === (Vector(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.SortedEquaSet("a", "b").toVector should === (Vector(lower.EquaBox("a"), lower.EquaBox("b")))
    number.SortedEquaSet(1).toVector should === (Vector(number.EquaBox(1)))
  }
  it should "have a transpose method" in {
    numberList.SortedEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose shouldBe numberList.SortedEquaSet(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    numberList.SortedEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose shouldBe numberList.SortedEquaSet(List(1, 3, 5, 7), List(2, 4, 6, 8))
    numberList.SortedEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose.transpose shouldBe numberList.SortedEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8))
    numberList.SortedEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose.transpose shouldBe numberList.SortedEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  }
  it should "have an unzip method" in {
    numberLower.SortedEquaSet((1, "2")).unzip(number, lower) shouldBe ((number.EquaSet(1), lower.EquaSet("2")))
    numberLower.SortedEquaSet((1, "2"), (3, "4")).unzip(number, lower) shouldBe ((number.EquaSet(1, 3), lower.EquaSet("2", "4")))
    numberLower.SortedEquaSet((1, "2"), (3, "4"), (5, "6")).unzip(number, lower) shouldBe ((number.EquaSet(1, 3, 5), lower.EquaSet("2", "4", "6")))
  }
  it should "have an unzip3 method" in {
    numberLowerTrimmed.SortedEquaSet((1, "2", "3")).unzip3(number, lower, trimmed) shouldBe (number.EquaSet(1), lower.EquaSet("2"), trimmed.EquaSet("3"))
    numberLowerTrimmed.SortedEquaSet((1, "2", "3"), (4, "5", "6")).unzip3(number, lower, trimmed) shouldBe (number.EquaSet(1, 4), lower.EquaSet("2", "5"), trimmed.EquaSet("3", "6"))
    numberLowerTrimmed.SortedEquaSet((1, "2", "3"), (4, "5", "6"), (7, "8", "9")).unzip3(number, lower, trimmed) shouldBe (number.EquaSet(1, 4, 7), lower.EquaSet("2", "5", "8"), trimmed.EquaSet("3", "6", "9"))
  }
  it should "have 2 views method" in {
    number.SortedEquaSet(3).view(0, 0).toList shouldBe List()
    number.SortedEquaSet(1, 2, 3).view(2, 1).toList shouldBe List()
    number.SortedEquaSet(1, 2, 3).view(1, 3).toList shouldBe List(number.EquaBox(2), number.EquaBox(3))
    number.SortedEquaSet(1, 2, 3).view.toList shouldBe List(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a zip method" in {
    number.SortedEquaSet(1, 2, 3).zip(List("4", "5", "6")) shouldBe Set((1, "4"), (2, "5"), (3, "6"))
    number.SortedEquaSet(1, 2, 3).zip(List("4", "5")) shouldBe Set((1, "4"), (2, "5"))
  }
  it should "have a zipAll method" in {
    number.SortedEquaSet(1, 2, 3).zipAll(List("4", "5", "6"), 0, "0") shouldBe Set((1, "4"), (2, "5"), (3, "6"))
    number.SortedEquaSet(1, 2, 3).zipAll(List("4", "5"), 0, "0") shouldBe Set((1, "4"), (2, "5"), (3, "0"))
    number.SortedEquaSet(1, 2).zipAll(List("4", "5", "6"), 0, "0") shouldBe Set((1, "4"), (2, "5"), (0, "6"))
  }
  it should "have a zipWithIndex method" in {
    number.SortedEquaSet(99).zipWithIndex shouldBe Set((99,0))
    number.SortedEquaSet(1, 2, 3).zipWithIndex shouldBe Set((1,0), (2,1), (3,2))
  }
}

