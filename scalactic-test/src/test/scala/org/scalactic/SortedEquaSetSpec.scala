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
import scala.collection.immutable.{TreeSet => StandardTreeSet}
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.{SortedSet => StandardSortedSet}
import scala.collection.parallel.mutable.ParArray
import org.scalactic.iterators.Iterator
import scala.collection.{Iterator => StdIterator}
import views.TreeSetView

class SortedSetSpec extends UnitSpec {
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
  val plainNumber = Collections[Int](intEquality)
  val number = SortedCollections[Int](intEquality)
  val sortedNumber = SortedCollections[Int](normalOrderingEquality[Int])
  val lower = SortedCollections[String](StringNormalizations.lowerCased.toOrderingEquality)
  val plainLower = Collections[String](StringNormalizations.lowerCased.toOrderingEquality)
  val sortedLower = SortedCollections[String](StringNormalizations.lowerCased.toOrderingEquality)
  val trimmed = SortedCollections[String](StringNormalizations.trimmed.toOrderingEquality)
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
  val numberLower = SortedCollections(intStringEquality)
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
  val numberLowerTrimmed = SortedCollections(intStringStringEquality)

  def numberListEquality[T] =
    new OrderingEquality[List[Int]] {
      def hashCodeFor(a: List[Int]): Int = a.hashCode
      def areEqual(a: List[Int], b: Any): Boolean = a == b
      def compare(a: List[Int], b: List[Int]): Int = a.mkString compareTo b.mkString
    }

  val numberList = SortedCollections[List[Int]](numberListEquality)

  val numberNumberEquality =
    new OrderingEquality[number.immutable.SortedSet[Int]] {
      def hashCodeFor(a: number.immutable.SortedSet[Int]): Int = a.hashCode
      def areEqual(a: number.immutable.SortedSet[Int], b: Any): Boolean = a == b
      def compare(a: number.immutable.SortedSet[Int], b: number.immutable.SortedSet[Int]): Int = a.mkString compareTo b.mkString
    }
  val numberNumber = SortedCollections[number.immutable.SortedSet[Int]](numberNumberEquality)

  "An SortedSet" can "be constructed with empty" in {
    val emptySet = lower.immutable.SortedSet.empty
    emptySet shouldBe empty

    val treeEmptySet = lower.immutable.TreeSet.empty
    treeEmptySet shouldBe empty

    lower.immutable.SortedSet.empty.shouldHaveExactType[lower.immutable.SortedSet[String]]
    lower.immutable.TreeSet.empty.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it can "be constructed with apply" in {
    val result1 = lower.immutable.SortedSet("one", "two", "three")
    result1 should have size 3
    result1.shouldHaveExactType[lower.immutable.inhabited.SortedSet[String]]

    val result2 = lower.immutable.TreeSet("one", "two", "three")
    result2 should have size 3
    result2.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedSet.
  }
  it should "have a a membership method that returns a Membership" in {
    val membership = lower.immutable.SortedSet("hi", "ho").membership
    membership("hi") shouldBe true
    membership("ho") shouldBe true
    membership("Hi") shouldBe true
    membership("HO") shouldBe true
    membership(" hi") shouldBe false
    """membership(1) shouldBe false""" shouldNot typeCheck
  }
  it should "construct only sets with appropriate element types" in {
    "lower.immutable.SortedSet(1, 2, 3)" shouldNot compile
    "lower.immutable.TreeSet(1, 2, 3)" shouldNot compile
  }
  it should "eliminate 'duplicate' entries passed to the apply factory method" in {
    val result1 = lower.immutable.SortedSet("one", "two", "two", "three", "Three")
    result1 should have size 3
    result1.shouldHaveExactType[lower.immutable.inhabited.SortedSet[String]]

    val result2 = lower.immutable.TreeSet("one", "two", "two", "three", "Three")
    result2 should have size 3
    result2.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedSet.
  }
  it should "have a toString method" in {
    lower.immutable.SortedSet("hi", "ho").toString should === ("TreeSet(hi, ho)")
    lower.immutable.TreeSet("hi", "ho").toString should === ("TreeSet(hi, ho)")
  }
  it should "have a diff method that takes another SortedSet instance with the same path-dependant type" in {
    val result1 = lower.immutable.SortedSet("hi", "ho") diff lower.immutable.SortedSet("HI", "HO")
    result1 shouldBe lower.immutable.SortedSet()
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = trimmed.immutable.SortedSet("hi", "ho") diff trimmed.immutable.SortedSet(" hi ", " ho ")
    result2 shouldBe trimmed.immutable.SortedSet()
    result2.shouldHaveExactType[trimmed.immutable.SortedSet[String]]

    """lower.immutable.SortedSet(" hi ", "hi") diff trimmed.immutable.SortedSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.immutable.SortedSet("hi", "ho") diff lower.immutable.SortedSet("ho")
    result3 shouldBe lower.immutable.SortedSet("hi")
    result3.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result4 = lower.immutable.SortedSet("hi", "ho", "let's", "go") diff lower.immutable.SortedSet("bo", "no", "go", "ho")
    result4 shouldBe lower.immutable.SortedSet("hi", "let's")
    result4.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result5 = lower.immutable.TreeSet("hi", "ho") diff lower.immutable.TreeSet("HI", "HO")
    result5 shouldBe lower.immutable.TreeSet()
    result5.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result6 = trimmed.immutable.TreeSet("hi", "ho") diff trimmed.immutable.TreeSet(" hi ", " ho ")
    result6 shouldBe trimmed.immutable.TreeSet()
    result6.shouldHaveExactType[trimmed.immutable.TreeSet[String]]

    """lower.immutable.TreeSet(" hi ", "hi") diff trimmed.immutable.TreeSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.immutable.TreeSet("hi", "ho") diff lower.immutable.TreeSet("ho")
    result7 shouldBe lower.immutable.TreeSet("hi")
    result7.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result8 = lower.immutable.TreeSet("hi", "ho", "let's", "go") diff lower.immutable.TreeSet("bo", "no", "go", "ho")
    result8 shouldBe lower.immutable.TreeSet("hi", "let's")
    result8.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "have an intersect method that takes another SortedSet instance with the same path-dependant type" in {
    val result1 = lower.immutable.SortedSet("hi", "ho") intersect lower.immutable.SortedSet("HI", "HO")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = trimmed.immutable.SortedSet("hi", "ho") intersect trimmed.immutable.SortedSet(" hi ", " ho ")
    result2 shouldBe trimmed.immutable.SortedSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.immutable.SortedSet[String]]

    """lower.immutable.SortedSet(" hi ", "hi") intersect trimmed.immutable.SortedSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.immutable.SortedSet("hi", "ho") intersect lower.immutable.SortedSet("ho")
    result3 shouldBe lower.immutable.SortedSet("ho")
    result3.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result4 = lower.immutable.SortedSet("hi", "ho", "let's", "go") intersect lower.immutable.SortedSet("bo", "no", "go", "ho")
    result4 shouldBe lower.immutable.SortedSet("ho", "go")
    result4.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result5 = lower.immutable.TreeSet("hi", "ho") intersect lower.immutable.TreeSet("HI", "HO")
    result5 shouldBe lower.immutable.TreeSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result6 = trimmed.immutable.TreeSet("hi", "ho") intersect trimmed.immutable.TreeSet(" hi ", " ho ")
    result6 shouldBe trimmed.immutable.TreeSet("hi", "ho")
    result6.shouldHaveExactType[trimmed.immutable.TreeSet[String]]

    """lower.immutable.TreeSet(" hi ", "hi") intersect trimmed.immutable.TreeSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.immutable.TreeSet("hi", "ho") intersect lower.immutable.TreeSet("ho")
    result7 shouldBe lower.immutable.TreeSet("ho")
    result7.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result8 = lower.immutable.TreeSet("hi", "ho", "let's", "go") intersect lower.immutable.TreeSet("bo", "no", "go", "ho")
    result8 shouldBe lower.immutable.TreeSet("ho", "go")
    result8.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "have a union method that takes another SortedSet instance with the same path-dependant type" in {
    val result1 = lower.immutable.SortedSet("hi", "ho") union lower.immutable.SortedSet("HI", "HO")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = trimmed.immutable.SortedSet("hi", "ho") union trimmed.immutable.SortedSet(" hi ", " ho ")
    result2 shouldBe trimmed.immutable.SortedSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.immutable.SortedSet[String]]

    """lower.immutable.SortedSet(" hi ", "hi") union trimmed.immutable.SortedSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.immutable.TreeSet("hi", "ho") union lower.immutable.TreeSet("HI", "HO")
    result3 shouldBe lower.immutable.TreeSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result4 = trimmed.immutable.TreeSet("hi", "ho") union trimmed.immutable.TreeSet(" hi ", " ho ")
    result4 shouldBe trimmed.immutable.TreeSet("hi", "ho")
    result4.shouldHaveExactType[trimmed.immutable.TreeSet[String]]

    """lower.immutable.TreeSet(" hi ", "hi") union trimmed.immutable.TreeSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a toSet method" in {
    lower.immutable.SortedSet("hi", "ho").toStandardSet should === (Set("hi", "ho"))
  }
  it should "have a toBoxSet method" in {
    lower.immutable.SortedSet("hi", "ho").toBoxStandardSet should === (Set(lower.Box("hi"), lower.Box("ho")))
    implicit val numberOrdering = new Ordering[number.Box[Int]] {
      def compare(x: number.Box[Int], y: number.Box[Int]): Int = x.value - y.value
    }
    number.immutable.SortedSet(1, 2, 3).toBoxStandardSet shouldBe StandardSortedSet(number.Box[Int](1), number.Box[Int](2), number.Box[Int](3))
  }
  it should "have a + method that takes one argument" in {
    val result1 = lower.immutable.SortedSet("hi", "ho") + "ha"
    result1 shouldBe lower.immutable.SortedSet("hi", "ho", "ha")
    result1.shouldHaveExactType[lower.immutable.inhabited.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho") + "HO"
    result2 shouldBe lower.immutable.SortedSet("hi", "ho")
    result2.shouldHaveExactType[lower.immutable.inhabited.SortedSet[String]]

    val result3 = lower.immutable.TreeSet("hi", "ho") + "ha"
    result3 shouldBe lower.immutable.TreeSet("hi", "ho", "ha")
    result3.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]

    val result4 = lower.immutable.TreeSet("hi", "ho") + "HO"
    result4 shouldBe lower.immutable.TreeSet("hi", "ho")
    result4.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]

    val result5 = lower.immutable.TreeSet.empty[String] + "HO"
    result5 shouldBe lower.immutable.TreeSet("HO")
    result5.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]
  }
  it should "have a + method that takes two or more arguments" in {
    val result1 = lower.immutable.SortedSet("hi", "ho") + ("ha", "hey!")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.inhabited.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho") + ("HO", "hoe", "Ho!")
    result2 shouldBe lower.immutable.SortedSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.inhabited.SortedSet[String]]

    val result3 = lower.immutable.TreeSet("hi", "ho") + ("ha", "hey!")
    result3 shouldBe lower.immutable.TreeSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]

    val result4 = lower.immutable.TreeSet("hi", "ho") + ("HO", "hoe", "Ho!")
    result4 shouldBe lower.immutable.TreeSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]

    val result5 = lower.immutable.TreeSet.empty[String] + ("HI", "HO")
    result5 shouldBe lower.immutable.TreeSet("HI", "HO")
    result5.shouldHaveExactType[lower.immutable.inhabited.TreeSet[String]]
  }
  it should "have a - method that takes one argument" in {
    val result1 = lower.immutable.SortedSet("hi", "ho", "ha") - "ha"
    result1 shouldBe lower.immutable.SortedSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho") - "HO"
    result2 shouldBe lower.immutable.SortedSet("hi")
    result2.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result3 = lower.immutable.SortedSet("hi", "ho") - "who?"
    result3 shouldBe lower.immutable.SortedSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result4 = lower.immutable.TreeSet("hi", "ho", "ha") - "ha"
    result4 shouldBe lower.immutable.TreeSet("hi", "ho")
    result4.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result5 = lower.immutable.TreeSet("hi", "ho") - "HO"
    result5 shouldBe lower.immutable.TreeSet("hi")
    result5.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result6 = lower.immutable.TreeSet("hi", "ho") - "who?"
    result6 shouldBe lower.immutable.TreeSet("hi", "ho")
    result6.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "have a - method that takes two or more arguments" in {
    val result1 = lower.immutable.SortedSet("hi", "ho", "ha") - ("ha", "howdy!")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")
    result2 shouldBe lower.immutable.SortedSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result3 = lower.immutable.SortedSet("hi", "ho") - ("who", "goes", "thar")
    result3 shouldBe lower.immutable.SortedSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result4 = lower.immutable.SortedSet("hi", "ho") - ("HI", "HO")
    result4 shouldBe lower.immutable.SortedSet.empty
    result4.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result5 = lower.immutable.TreeSet("hi", "ho", "ha") - ("ha", "howdy!")
    result5 shouldBe lower.immutable.TreeSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result6 = lower.immutable.TreeSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")
    result6 shouldBe lower.immutable.TreeSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result7 = lower.immutable.TreeSet("hi", "ho") - ("who", "goes", "thar")
    result7 shouldBe lower.immutable.TreeSet("hi", "ho")
    result7.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result8 = lower.immutable.TreeSet("hi", "ho") - ("HI", "HO")
    result8 shouldBe lower.immutable.TreeSet.empty
    result8.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "return an iterator that returns elements in sorted order" in {
    lower.immutable.SortedSet("hi", "ho", "ha", "he").iterator.toStandardList shouldEqual List("ha", "he", "hi", "ho")
  }
  it should "have a ++ method that takes a GenTraversableOnce" in {
    val result1 = lower.immutable.SortedSet("hi", "ho") ++ List("ha", "hey!")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho") ++ List("HO", "hoe", "Ho!")
    result2 shouldBe lower.immutable.SortedSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result3 = lower.immutable.SortedSet("hi", "ho") ++ Set("ha", "hey!")
    result3 shouldBe lower.immutable.SortedSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result4 = lower.immutable.SortedSet("hi", "ho") ++ Set("HO", "hoe", "Ho!")
    result4 shouldBe lower.immutable.SortedSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result5 = lower.immutable.SortedSet("hi", "ho") ++ Vector("ha", "hey!")
    result5 shouldBe lower.immutable.SortedSet("hi", "ho", "ha", "hey!")
    result5.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result6 = lower.immutable.SortedSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!")
    result6 shouldBe lower.immutable.SortedSet("hi", "ho", "hoe", "Ho!")
    result6.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result7 = lower.immutable.TreeSet("hi", "ho") ++ List("ha", "hey!")
    result7 shouldBe lower.immutable.TreeSet("hi", "ho", "ha", "hey!")
    result7.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result8 = lower.immutable.TreeSet("hi", "ho") ++ List("HO", "hoe", "Ho!")
    result8 shouldBe lower.immutable.TreeSet("hi", "ho", "hoe", "Ho!")
    result8.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result9 = lower.immutable.TreeSet("hi", "ho") ++ Set("ha", "hey!")
    result9 shouldBe lower.immutable.TreeSet("hi", "ho", "ha", "hey!")
    result9.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result10 = lower.immutable.TreeSet("hi", "ho") ++ Set("HO", "hoe", "Ho!")
    result10 shouldBe lower.immutable.TreeSet("hi", "ho", "hoe", "Ho!")
    result10.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result11 = lower.immutable.TreeSet("hi", "ho") ++ Vector("ha", "hey!")
    result11 shouldBe lower.immutable.TreeSet("hi", "ho", "ha", "hey!")
    result11.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result12 = lower.immutable.TreeSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!")
    result12 shouldBe lower.immutable.TreeSet("hi", "ho", "hoe", "Ho!")
    result12.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "have a ++ method that takes another Set" in {
    val result1 = lower.immutable.SortedSet("hi", "ho") ++ lower.immutable.SortedSet("ha", "hey!")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho") ++ lower.immutable.SortedSet("HO", "hoe", "Ho!")
    result2 shouldBe lower.immutable.SortedSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result3 = lower.immutable.TreeSet("hi", "ho") ++ lower.immutable.TreeSet("ha", "hey!")
    result3 shouldBe lower.immutable.TreeSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result4 = lower.immutable.TreeSet("hi", "ho") ++ lower.immutable.TreeSet("HO", "hoe", "Ho!")
    result4 shouldBe lower.immutable.TreeSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "have a -- method that takes a GenTraversableOnce" in {
    val result1 = lower.immutable.SortedSet("hi", "ho", "ha") -- List("ha", "howdy!")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")
    result2 shouldBe lower.immutable.SortedSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result3 = lower.immutable.SortedSet("hi", "ho") -- List("who", "goes", "thar")
    result3 shouldBe lower.immutable.SortedSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result4 = lower.immutable.SortedSet("hi", "ho") -- List("HI", "HO")
    result4 shouldBe lower.immutable.SortedSet.empty
    result4.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result5 = lower.immutable.SortedSet("hi", "ho", "ha") -- Set("ha", "howdy!")
    result5 shouldBe lower.immutable.SortedSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result6 = lower.immutable.SortedSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")
    result6 shouldBe lower.immutable.SortedSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result7 = lower.immutable.SortedSet("hi", "ho") -- Set("who", "goes", "thar")
    result7 shouldBe lower.immutable.SortedSet("hi", "ho")
    result7.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result8 = lower.immutable.SortedSet("hi", "ho") -- Set("HI", "HO")
    result8 shouldBe lower.immutable.SortedSet.empty
    result8.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result9 = lower.immutable.SortedSet("hi", "ho", "ha") -- Vector("ha", "howdy!")
    result9 shouldBe lower.immutable.SortedSet("hi", "ho")
    result9.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result10 = lower.immutable.SortedSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")
    result10 shouldBe lower.immutable.SortedSet("hi", "fee", "foe")
    result10.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result11 = lower.immutable.SortedSet("hi", "ho") -- Vector("who", "goes", "thar")
    result11 shouldBe lower.immutable.SortedSet("hi", "ho")
    result11.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result12 = lower.immutable.SortedSet("hi", "ho") -- Vector("HI", "HO")
    result12 shouldBe lower.immutable.SortedSet.empty
    result12.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result13 = lower.immutable.TreeSet("hi", "ho", "ha") -- List("ha", "howdy!")
    result13 shouldBe lower.immutable.TreeSet("hi", "ho")
    result13.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result14 = lower.immutable.TreeSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")
    result14 shouldBe lower.immutable.TreeSet("hi", "fee", "foe")
    result14.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result15 = lower.immutable.TreeSet("hi", "ho") -- List("who", "goes", "thar")
    result15 shouldBe lower.immutable.TreeSet("hi", "ho")
    result15.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result16 = lower.immutable.TreeSet("hi", "ho") -- List("HI", "HO")
    result16 shouldBe lower.immutable.TreeSet.empty
    result16.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result17 = lower.immutable.TreeSet("hi", "ho", "ha") -- Set("ha", "howdy!")
    result17 shouldBe lower.immutable.TreeSet("hi", "ho")
    result17.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result18 = lower.immutable.TreeSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")
    result18 shouldBe lower.immutable.TreeSet("hi", "fee", "foe")
    result18.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result19 = lower.immutable.TreeSet("hi", "ho") -- Set("who", "goes", "thar")
    result19 shouldBe lower.immutable.TreeSet("hi", "ho")
    result19.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result20 = lower.immutable.TreeSet("hi", "ho") -- Set("HI", "HO")
    result20 shouldBe lower.immutable.TreeSet.empty
    result20.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result21 = lower.immutable.TreeSet("hi", "ho", "ha") -- Vector("ha", "howdy!")
    result21 shouldBe lower.immutable.TreeSet("hi", "ho")
    result21.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result22 = lower.immutable.TreeSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")
    result22 shouldBe lower.immutable.TreeSet("hi", "fee", "foe")
    result22.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result23 = lower.immutable.TreeSet("hi", "ho") -- Vector("who", "goes", "thar")
    result23 shouldBe lower.immutable.TreeSet("hi", "ho")
    result23.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result24 = lower.immutable.TreeSet("hi", "ho") -- Vector("HI", "HO")
    result24 shouldBe lower.immutable.TreeSet.empty
    result24.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "have a -- method that takes another Set" in {
    val result1 = lower.immutable.SortedSet("hi", "ho", "ha") -- lower.immutable.Set("ha", "howdy!")
    result1 shouldBe lower.immutable.SortedSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.immutable.Set("HO", "FIE", "fUm")
    result2 shouldBe lower.immutable.SortedSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result3 = lower.immutable.SortedSet("hi", "ho") -- lower.immutable.Set("who", "goes", "thar")
    result3 shouldBe lower.immutable.SortedSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result4 = lower.immutable.SortedSet("hi", "ho") -- lower.immutable.Set("HI", "HO")
    result4 shouldBe lower.immutable.SortedSet.empty
    result4.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result5 = lower.immutable.TreeSet("hi", "ho", "ha") -- lower.immutable.Set("ha", "howdy!")
    result5 shouldBe lower.immutable.TreeSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result6 = lower.immutable.TreeSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.immutable.Set("HO", "FIE", "fUm")
    result6 shouldBe lower.immutable.TreeSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result7 = lower.immutable.TreeSet("hi", "ho") -- lower.immutable.Set("who", "goes", "thar")
    result7 shouldBe lower.immutable.TreeSet("hi", "ho")
    result7.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result8 = lower.immutable.TreeSet("hi", "ho") -- lower.immutable.Set("HI", "HO")
    result8 shouldBe lower.immutable.TreeSet.empty
    result8.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
  it should "have 3 addString methods" in {
    lower.immutable.SortedSet("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    number.immutable.SortedSet(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    lower.immutable.SortedSet("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    number.immutable.SortedSet(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    number.immutable.SortedSet(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    lower.immutable.SortedSet("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    number.immutable.SortedSet(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    number.immutable.SortedSet(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have a aggregate method" in {
    lower.immutable.SortedSet("hi", "ho", "ha", "hey!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "ha", "hey!")
    lower.immutable.SortedSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "hoe", "Ho!")

    val result1 = lower.immutable.SortedSet("hi", "ho", "ha", "hey!").aggregate(lower.immutable.SortedSet[String]())(_ + _, _ ++ _)
    result1 shouldBe lower.immutable.SortedSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result2 = lower.immutable.SortedSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.immutable.SortedSet[String]())(_ + _, _ ++ _)
    result2 shouldBe lower.immutable.SortedSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.SortedSet[String]]

    val result3 = lower.immutable.TreeSet("hi", "ho", "ha", "hey!").aggregate(lower.immutable.TreeSet[String]())(_ + _, _ ++ _)
    result3 shouldBe lower.immutable.TreeSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.TreeSet[String]]

    val result4 = lower.immutable.TreeSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.immutable.TreeSet[String]())(_ + _, _ ++ _)
    result4 shouldBe lower.immutable.TreeSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.TreeSet[String]]
  }
/*
  it should "have an apply method" in {
    val a = number.immutable.SortedSet(1, 2, 3)
    a(2) shouldEqual true
    a(5) shouldEqual false

    val b = lower.immutable.SortedSet("hi")
    b("hi") shouldEqual true
    b("Hi") shouldEqual true
    b("hI") shouldEqual true
    b("HI") shouldEqual true
    b("he") shouldEqual false
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = number.immutable.SortedSet(1) andThen (!_)
    pf1(1) shouldEqual false
    pf1(2) shouldEqual true

    val pf2 = number.immutable.SortedSet(1, 2, 3) andThen (!_)
    pf2(1) shouldEqual false
    pf2(2) shouldEqual false
    pf2(3) shouldEqual false
    pf2(0) shouldEqual true
  }
*/
  it should "have a canEqual method" in {
    number.immutable.SortedSet(1).canEqual(3) shouldBe false
    number.immutable.SortedSet(1).canEqual("hi") shouldBe false
    number.immutable.SortedSet(1).canEqual(number.immutable.Set(1)) shouldBe true
    number.immutable.SortedSet(1).canEqual(number.immutable.Set(1, 2, 3)) shouldBe true
    number.immutable.SortedSet(1).canEqual(lower.immutable.Set("hi")) shouldBe false
    number.immutable.SortedSet(1).canEqual(number.immutable.SortedSet(1)) shouldBe true
    number.immutable.SortedSet(1).canEqual(number.immutable.SortedSet(1, 2, 3)) shouldBe true
    number.immutable.SortedSet(1).canEqual(lower.immutable.SortedSet("hi")) shouldBe false
    number.immutable.Set(1).canEqual(number.immutable.SortedSet(1)) shouldBe true
    number.immutable.Set(1).canEqual(number.immutable.SortedSet(1, 2, 3)) shouldBe true
    number.immutable.Set(1).canEqual(lower.immutable.SortedSet("hi")) shouldBe false
  }
  it should "have an into.collect method" is pending
  it should "have a collect method that only accepts functions that result in the path-enclosed type" is pending
/*
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Boolean = number.immutable.SortedSet(1, 2, 3).compose(_ + 1)
    fn(0) shouldBe true
    fn(1) shouldBe true
    fn(2) shouldBe true
    fn(3) shouldBe false
  }
*/
  it should "have an contains method that does a type check" in {
    val e = number.immutable.SortedSet(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    """e.contains("five")""" shouldNot typeCheck
    // new TypeCheckedTripleEquals {
      val es = lower.immutable.SortedSet("one", "two", "three")
      """es.contains(5)""" shouldNot typeCheck
      es.contains("ONE") shouldBe true;
    // }
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
    val fruitCollections = SortedCollections(equalityOfFruit)
    val fruits = fruitCollections.immutable.SortedSet(mac, navel)
    fruits.contains(mac) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(number.Box(-1))
    number.immutable.SortedSet(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(number.Box(1), number.Box(2), number.Box(3), number.Box(4), number.Box(5))

    val arr2 = Array.fill(5)(number.Box(-1))
    number.immutable.SortedSet(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(number.Box(-1), number.Box(1), number.Box(2), number.Box(3), number.Box(4))

    val arr3 = Array.fill(5)(number.Box(-1))
    number.immutable.SortedSet(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(number.Box(-1), number.Box(1), number.Box(2), number.Box(-1), number.Box(-1))
  }
  it should "have a copyToBuffer method" in {
    val buf = ListBuffer.fill(3)(number.Box(-1))
    number.immutable.SortedSet(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(number.Box(-1), number.Box(-1), number.Box(-1), number.Box(1), number.Box(2), number.Box(3), number.Box(4), number.Box(5))
  }
  it should "have a count method" in {
    val set = number.immutable.SortedSet(1, 2, 3, 4, 5)
    set.count(_ > 10) shouldBe 0
    set.count(_ % 2 == 0) shouldBe 2
    set.count(_ % 2 == 1) shouldBe 3
  }
  it should "have a drop method" in {
    val set = number.immutable.SortedSet(1, 2, 3, 4, 5)
    val treeSet = number.immutable.TreeSet(1, 2, 3, 4, 5)

    val result1 = set.drop(0)
    result1 shouldBe number.immutable.SortedSet(1, 2, 3, 4, 5)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = set.drop(1)
    result2 shouldBe number.immutable.SortedSet(2, 3, 4, 5)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = set.drop(2)
    result3 shouldBe number.immutable.SortedSet(3, 4, 5)
    result3.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result4 = set.drop(3)
    result4 shouldBe number.immutable.SortedSet(4, 5)
    result4.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result5 = set.drop(4)
    result5 shouldBe number.immutable.SortedSet(5)
    result5.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result6 = set.drop(5)
    result6 shouldBe number.immutable.SortedSet()
    result6.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result7 = treeSet.drop(0)
    result7 shouldBe number.immutable.TreeSet(1, 2, 3, 4, 5)
    result7.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result8 = treeSet.drop(1)
    result8 shouldBe number.immutable.TreeSet(2, 3, 4, 5)
    result8.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result9 = treeSet.drop(2)
    result9 shouldBe number.immutable.TreeSet(3, 4, 5)
    result9.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result10 = treeSet.drop(3)
    result10 shouldBe number.immutable.TreeSet(4, 5)
    result10.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result11 = treeSet.drop(4)
    result11 shouldBe number.immutable.TreeSet(5)
    result11.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result12 = treeSet.drop(5)
    result12 shouldBe number.immutable.TreeSet()
    result12.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have a dropRight method" in {
    val set = number.immutable.SortedSet(1, 2, 3, 4, 5)
    val treeSet = number.immutable.TreeSet(1, 2, 3, 4, 5)

    val result1 = set.dropRight(0)
    result1 shouldBe number.immutable.SortedSet(1, 2, 3, 4, 5)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = set.dropRight(1)
    result2 shouldBe number.immutable.SortedSet(1, 2, 3, 4)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = set.dropRight(2)
    result3 shouldBe number.immutable.SortedSet(1, 2, 3)
    result3.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result4 = set.dropRight(3)
    result4 shouldBe number.immutable.SortedSet(1, 2)
    result4.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result5 = set.dropRight(4)
    result5 shouldBe number.immutable.SortedSet(1)
    result5.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result6 = set.dropRight(5)
    result6 shouldBe number.immutable.SortedSet()
    result6.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result7 = treeSet.dropRight(0)
    result7 shouldBe number.immutable.TreeSet(1, 2, 3, 4, 5)
    result7.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result8 = treeSet.dropRight(1)
    result8 shouldBe number.immutable.TreeSet(1, 2, 3, 4)
    result8.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result9 = treeSet.dropRight(2)
    result9 shouldBe number.immutable.TreeSet(1, 2, 3)
    result9.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result10 = treeSet.dropRight(3)
    result10 shouldBe number.immutable.TreeSet(1, 2)
    result10.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result11 = treeSet.dropRight(4)
    result11 shouldBe number.immutable.TreeSet(1)
    result11.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result12 = treeSet.dropRight(5)
    result12 shouldBe number.immutable.TreeSet()
    result12.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have a dropWhile method" in {
    val set = number.immutable.SortedSet(1, 2, 3, 4, 5)
    val treeSet = number.immutable.TreeSet(1, 2, 3, 4, 5)

    val result1 = set.dropWhile(_ < 1)
    result1 shouldBe number.immutable.SortedSet(1, 2, 3, 4, 5)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = set.dropWhile(_ < 2)
    result2 shouldBe number.immutable.SortedSet(2, 3, 4, 5)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = set.dropWhile(_ < 3)
    result3 shouldBe number.immutable.SortedSet(3, 4, 5)
    result3.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result4 = set.dropWhile(_ < 4)
    result4 shouldBe number.immutable.SortedSet(4, 5)
    result4.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result5 = set.dropWhile(_ < 5)
    result5 shouldBe number.immutable.SortedSet(5)
    result5.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result6 = set.dropWhile(_ < 6)
    result6 shouldBe number.immutable.SortedSet()
    result6.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result7 = treeSet.dropWhile(_ < 1)
    result7 shouldBe number.immutable.TreeSet(1, 2, 3, 4, 5)
    result7.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result8 = treeSet.dropWhile(_ < 2)
    result8 shouldBe number.immutable.TreeSet(2, 3, 4, 5)
    result8.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result9 = treeSet.dropWhile(_ < 3)
    result9 shouldBe number.immutable.TreeSet(3, 4, 5)
    result9.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result10 = treeSet.dropWhile(_ < 4)
    result10 shouldBe number.immutable.TreeSet(4, 5)
    result10.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result11 = treeSet.dropWhile(_ < 5)
    result11 shouldBe number.immutable.TreeSet(5)
    result11.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result12 = treeSet.dropWhile(_ < 6)
    result12 shouldBe number.immutable.TreeSet()
    result12.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have an path method" in {
    trimmed.immutable.Set("hi").path shouldBe trimmed
    trimmed.immutable.FastSet("hi").path shouldBe trimmed
    trimmed.immutable.SortedSet("hi").path shouldBe trimmed
    trimmed.immutable.TreeSet("hi").path shouldBe trimmed
  }
  it should "have an exists method" in {
    number.immutable.SortedSet(1, 2, 3).exists(_ == 2) shouldBe true
    number.immutable.SortedSet(1, 2, 3).exists(_ == 5) shouldBe false
  }
  it should "have a filter method" in {
    val set = number.immutable.SortedSet(1, 2, 3)
    val treeSet = number.immutable.TreeSet(1, 2, 3)

    val result1 = set.filter(_ == 1)
    result1 shouldBe number.immutable.SortedSet(1)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = set.filter(_ == 2)
    result2 shouldBe number.immutable.SortedSet(2)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = set.filter(_ == 3)
    result3 shouldBe number.immutable.SortedSet(3)
    result3.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result4 = treeSet.filter(_ == 1)
    result4 shouldBe number.immutable.TreeSet(1)
    result4.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result5 = treeSet.filter(_ == 2)
    result5 shouldBe number.immutable.TreeSet(2)
    result5.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result6 = treeSet.filter(_ == 3)
    result6 shouldBe number.immutable.TreeSet(3)
    result6.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have a filterNot method" in {
    val set = number.immutable.SortedSet(1, 2, 3)
    set.filterNot(_ == 1) shouldBe number.immutable.SortedSet(2, 3)
    set.filterNot(_ == 2) shouldBe number.immutable.SortedSet(1, 3)
    set.filterNot(_ == 3) shouldBe number.immutable.SortedSet(1, 2)
  }
  it should "have a find method" in {
    number.immutable.SortedSet(1, 2, 3).find(_ == 5) shouldBe None
    number.immutable.SortedSet(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have a flatMap method" in {
    val set = trimmed.immutable.SortedSet("1", "2", "01", "3")
    val flatMapped = set.flatMap { (digit: String) =>
      TreeSetView(digit.toInt)
    }
    val strictSet = flatMapped.toSet(number)
    strictSet should equal (number.immutable.SortedSet(1, 2, 3))
  }
  it should "offer a flatMap method on its view" in {
    val lazySet = trimmed.immutable.SortedSet("1", "2", "01", "3").view
    val flatMapped = lazySet.flatMap { (digit: String) =>
      TreeSetView(digit.toInt)
    }
    val strictSet = flatMapped.toSet(number)
    strictSet should equal (number.immutable.SortedSet(1, 2, 3))
  }
  it should "allow chaining of maps and flatMaps on its view" in {
    val lazySet = trimmed.immutable.SortedSet("1", "2", "01", "3").view
    val flatMapped = lazySet.flatMap { (digit: String) =>
      TreeSetView(digit.toInt)
    }
    val mapped = flatMapped.map(_ + 1)
    val strictSet = mapped.toSet(number)
    strictSet should equal (number.immutable.SortedSet(2, 3, 4))
  }
/*
  it can "be flattened when in a GenTraversableOnce" in {
    // need to keep this commented out until finish implementing all methods
    Vector(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(1, 2, 3)).flatten shouldBe List(1, 2, 3, 1, 2, 3)
    // TODO: this is not working 2.10, we may want to enable this back when we understand better how flatten is supported by the implicit in 2.10
    //List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(1, 2, 3)).toStandardIterator.flatten.toStandardStream shouldBe List(1, 2, 3, 1, 2, 3).toStandardIterator.toStream
    List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(1, 2, 3)).par.flatten shouldBe List(1, 2, 3, 1, 2, 3).par
  }
  it should "have a flatten method that works on nested GenTraversable" in {
    numberList.immutable.SortedSet(List(1, 2), List(3)).flatten shouldBe List(1, 2, 3)
    numberList.immutable.SortedSet(List(1)).flatten shouldBe List(1)
  }
*/
  it should "have a fold method" in {
    number.immutable.SortedSet(1).fold(0)(_ + _) shouldBe 1
    number.immutable.SortedSet(1).fold(1)(_ * _) shouldBe 1
    number.immutable.SortedSet(2).fold(0)(_ + _) shouldBe 2
    number.immutable.SortedSet(2).fold(1)(_ * _) shouldBe 2
    number.immutable.SortedSet(3).fold(0)(_ + _) shouldBe 3
    number.immutable.SortedSet(3).fold(1)(_ * _) shouldBe 3
    number.immutable.SortedSet(1, 2, 3).fold(0)(_ + _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3).fold(1)(_ * _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    number.immutable.SortedSet(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    number.immutable.SortedSet(1).foldLeft(0)(_ + _) shouldBe 1
    number.immutable.SortedSet(1).foldLeft(1)(_ + _) shouldBe 2
    number.immutable.SortedSet(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    number.immutable.SortedSet(1).foldRight(0)(_ + _) shouldBe 1
    number.immutable.SortedSet(1).foldRight(1)(_ + _) shouldBe 2
    number.immutable.SortedSet(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    number.immutable.SortedSet(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    number.immutable.SortedSet(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- number.immutable.Set(1, 2, 3))
      num += i
    num shouldBe 12
    number.immutable.SortedSet(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3, 4, 5).groupBy(_ % 2)
    result1 shouldBe Map(1 -> number.immutable.SortedSet(1, 3, 5), 0 -> number.immutable.SortedSet(2, 4))
    result1.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.SortedSet[Int]]]

    val result2 = number.immutable.SortedSet(1, 2, 3, 3, 3).groupBy(_ % 2)
    result2 shouldBe Map(1 -> number.immutable.SortedSet(1, 3, 3, 3), 0 -> number.immutable.SortedSet(2))
    result2.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.SortedSet[Int]]]

    val result3 = number.immutable.SortedSet(1, 1, 3, 3, 3).groupBy(_ % 2)
    result3 shouldBe Map(1 -> number.immutable.SortedSet(1, 1, 3, 3, 3))
    result3.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.SortedSet[Int]]]

    val result4 = number.immutable.SortedSet(1, 2, 3, 5, 7).groupBy(_ % 2)
    result4 shouldBe Map(1 -> number.immutable.SortedSet(1, 3, 5, 7), 0 -> number.immutable.SortedSet(2))
    result4.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.SortedSet[Int]]]

    val result5 = number.immutable.TreeSet(1, 2, 3, 4, 5).groupBy(_ % 2)
    result5 shouldBe Map(1 -> number.immutable.TreeSet(1, 3, 5), 0 -> number.immutable.TreeSet(2, 4))
    result5.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.TreeSet[Int]]]

    val result6 = number.immutable.TreeSet(1, 2, 3, 3, 3).groupBy(_ % 2)
    result6 shouldBe Map(1 -> number.immutable.TreeSet(1, 3, 3, 3), 0 -> number.immutable.TreeSet(2))
    result6.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.TreeSet[Int]]]

    val result7 = number.immutable.TreeSet(1, 1, 3, 3, 3).groupBy(_ % 2)
    result7 shouldBe Map(1 -> number.immutable.TreeSet(1, 1, 3, 3, 3))
    result7.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.TreeSet[Int]]]

    val result8 = number.immutable.TreeSet(1, 2, 3, 5, 7).groupBy(_ % 2)
    result8 shouldBe Map(1 -> number.immutable.TreeSet(1, 3, 5, 7), 0 -> number.immutable.TreeSet(2))
    result8.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.TreeSet[Int]]]
  }
  it should "have a grouped method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).grouped(2).toStandardList
    result1 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(3))
    result1.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result2 = number.immutable.SortedSet(1, 2, 3).grouped(1).toStandardList
    result2 shouldBe List(number.immutable.SortedSet(1), number.immutable.SortedSet(2), number.immutable.SortedSet(3))
    result2.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    an [IllegalArgumentException] should be thrownBy { number.immutable.SortedSet(1, 2, 3).grouped(0).toStandardList }

    val result3 = number.immutable.SortedSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toStandardList
    result3 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(3, 4), number.immutable.SortedSet(5, 6), number.immutable.SortedSet(7, 8), number.immutable.SortedSet(9, 10))
    result3.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result4 = number.immutable.SortedSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toStandardList
    result4 shouldBe List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(4, 5, 6), number.immutable.SortedSet(7, 8, 9), number.immutable.SortedSet(10))
    result4.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result5 = number.immutable.SortedSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toStandardList
    result5 shouldBe List(number.immutable.SortedSet(1, 2, 3, 4), number.immutable.SortedSet(5, 6, 7, 8), number.immutable.SortedSet(9, 10))
    result5.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result6 = number.immutable.SortedSet(1).grouped(2).toStandardList
    result6 shouldBe List(number.immutable.SortedSet(1))
    result6.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result7 = number.immutable.SortedSet(1).grouped(1).toStandardList
    result7 shouldBe List(number.immutable.SortedSet(1))
    result7.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result8 = number.immutable.TreeSet(1, 2, 3).grouped(2).toStandardList
    result8 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(3))
    result8.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result9 = number.immutable.TreeSet(1, 2, 3).grouped(1).toStandardList
    result9 shouldBe List(number.immutable.TreeSet(1), number.immutable.TreeSet(2), number.immutable.TreeSet(3))
    result9.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    an [IllegalArgumentException] should be thrownBy { number.immutable.TreeSet(1, 2, 3).grouped(0).toStandardList }

    val result10 = number.immutable.TreeSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toStandardList
    result10 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(3, 4), number.immutable.TreeSet(5, 6), number.immutable.TreeSet(7, 8), number.immutable.TreeSet(9, 10))
    result10.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result11 = number.immutable.TreeSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toStandardList
    result11 shouldBe List(number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet(4, 5, 6), number.immutable.TreeSet(7, 8, 9), number.immutable.TreeSet(10))
    result11.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result12 = number.immutable.TreeSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toStandardList
    result12 shouldBe List(number.immutable.TreeSet(1, 2, 3, 4), number.immutable.TreeSet(5, 6, 7, 8), number.immutable.TreeSet(9, 10))
    result12.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result13 = number.immutable.TreeSet(1).grouped(2).toStandardList
    result13 shouldBe List(number.immutable.TreeSet(1))
    result13.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result14 = number.immutable.TreeSet(1).grouped(1).toStandardList
    result14 shouldBe List(number.immutable.TreeSet(1))
    result14.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]
  }
  it should "have a hasDefiniteSize method" in {
    number.immutable.SortedSet(1).hasDefiniteSize shouldBe true
    number.immutable.SortedSet(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a head method" in {
    lower.immutable.SortedSet("hi").head shouldBe "hi"
    number.immutable.SortedSet(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    lower.immutable.SortedSet("hi").headOption shouldBe Some("hi")
    number.immutable.SortedSet(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have an init method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).init
    result1 shouldBe number.immutable.SortedSet(1, 2)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = number.immutable.TreeSet(1, 2, 3).init
    result2 shouldBe number.immutable.TreeSet(1, 2)
    result2.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have an inits method" in {
    val inits = number.immutable.SortedSet(1, 2, 3).inits
    inits.shouldHaveExactType[Iterator[number.immutable.SortedSet[Int]]]
    inits.next shouldBe number.immutable.SortedSet(1,2,3)
    inits.next shouldBe number.immutable.SortedSet(1,2)
    inits.next shouldBe number.immutable.SortedSet(1)
    inits.next shouldBe number.immutable.SortedSet()
    inits.hasNext shouldBe false

    val treeInits = number.immutable.TreeSet(1, 2, 3).inits
    treeInits.shouldHaveExactType[Iterator[number.immutable.TreeSet[Int]]]
    treeInits.next shouldBe number.immutable.TreeSet(1,2,3)
    treeInits.next shouldBe number.immutable.TreeSet(1,2)
    treeInits.next shouldBe number.immutable.TreeSet(1)
    treeInits.next shouldBe number.immutable.TreeSet()
    treeInits.hasNext shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    lower.immutable.SortedSet("hi").isTraversableAgain shouldBe true
    number.immutable.SortedSet(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have a last method" in {
    lower.immutable.SortedSet("hi").last shouldBe "hi"
    number.immutable.SortedSet(1, 2, 3).last shouldBe 3
  }
  it should "have an lastOption method" in {
    lower.immutable.SortedSet("hi").lastOption shouldBe Some("hi")
    number.immutable.SortedSet(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an into.map method" is pending
  it should "have a map method" is pending
  it should "have a max method" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).max shouldBe 5
    number.immutable.SortedSet(1).max shouldBe 1
    number.immutable.SortedSet(-1).max shouldBe -1
    lower.immutable.SortedSet("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    number.immutable.SortedSet(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).min shouldBe 1
    number.immutable.SortedSet(1).min shouldBe 1
    number.immutable.SortedSet(-1).min shouldBe -1
    lower.immutable.SortedSet("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    number.immutable.SortedSet(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a 3 mkString method" in {

    lower.immutable.SortedSet("hi").mkString shouldBe "hi"
    number.immutable.SortedSet(1, 2, 3).mkString shouldBe "123"

    lower.immutable.SortedSet("hi").mkString("#") shouldBe "hi"
    number.immutable.SortedSet(1, 2, 3).mkString("#") shouldBe "1#2#3"
    number.immutable.SortedSet(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    lower.immutable.SortedSet("hi").mkString("<", "#", ">") shouldBe "<hi>"
    number.immutable.SortedSet(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    number.immutable.SortedSet(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    lower.immutable.SortedSet("hi").nonEmpty shouldBe true
    number.immutable.SortedSet(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have a partition method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3, 4).partition(_ < 3)
    result1 shouldBe (number.immutable.SortedSet(1, 2), number.immutable.SortedSet(3, 4))
    result1.shouldHaveExactType[(number.immutable.SortedSet[Int], number.immutable.SortedSet[Int])]

    val result2 = number.immutable.TreeSet(1, 2, 3, 4).partition(_ < 3)
    result2 shouldBe (number.immutable.TreeSet(1, 2), number.immutable.TreeSet(3, 4))
    result2.shouldHaveExactType[(number.immutable.TreeSet[Int], number.immutable.TreeSet[Int])]
  }
  it should "have a product method" in {
    number.immutable.SortedSet(1, 2, 3).product shouldBe 6
    number.immutable.SortedSet(3).product shouldBe 3
    number.immutable.SortedSet(3, 4, 5).product shouldBe 60
    number.immutable.SortedSet(3, 4, 5).product shouldBe 60
  }
  it should "have a reduce method" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    number.immutable.SortedSet(5).reduce(_ + _) shouldBe 5
    number.immutable.SortedSet(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    number.immutable.SortedSet(1).reduceLeft(_ + _) shouldBe 1
    number.immutable.SortedSet(1).reduceLeft(_ * _) shouldBe 1
    number.immutable.SortedSet(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    number.immutable.SortedSet(1).reduceLeftOption(_ + _) shouldBe Some(1)
    number.immutable.SortedSet(1).reduceLeftOption(_ * _) shouldBe Some(1)
    number.immutable.SortedSet(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    number.immutable.SortedSet(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    number.immutable.SortedSet(5).reduceOption(_ + _) shouldBe Some(5)
    number.immutable.SortedSet(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    number.immutable.SortedSet(1).reduceRight(_ * _) shouldBe 1
    number.immutable.SortedSet(1, 2, 3).reduceRight(_ + _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3).reduceRight(_ * _) shouldBe 6
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    number.immutable.SortedSet(1).reduceRightOption(_ + _) shouldBe Some(1)
    number.immutable.SortedSet(1).reduceRightOption(_ * _) shouldBe Some(1)
    number.immutable.SortedSet(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    number.immutable.SortedSet(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    number.immutable.SortedSet(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a sameElements method that takes a GenIterable" in {
    number.immutable.SortedSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5)) shouldBe true
    number.immutable.SortedSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4)) shouldBe false
    number.immutable.SortedSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5, 6)) shouldBe false
    number.immutable.SortedSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 4)) shouldBe false
    number.immutable.SortedSet(3).sameElements(List(1, 2, 3, 4, 5)) shouldBe false
    number.immutable.SortedSet(3).sameElements(List(1)) shouldBe false
    number.immutable.SortedSet(3).sameElements(List(3)) shouldBe true
  }
/*
  it should "have a scanLeft method" in {
    val result1 = number.immutable.SortedSet(1).scanLeft(0)(_ + _)
    result1 shouldBe number.immutable.SortedSet(0, 1)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = number.immutable.SortedSet(1, 2, 3).scanLeft(0)(_ + _)
    result2 shouldBe number.immutable.SortedSet(0, 1, 3, 6)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = number.immutable.TreeSet(1).scanLeft(0)(_ + _)
    result3 shouldBe number.immutable.TreeSet(0, 1)
    result3.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result4 = number.immutable.TreeSet(1, 2, 3).scanLeft(0)(_ + _)
    result4 shouldBe number.immutable.TreeSet(0, 1, 3, 6)
    result4.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have an into.scanLeft method" is pending
  it should "have a scanRight method" in {
    val result1 = number.immutable.SortedSet(1).scanRight(0)(_ + _)
    result1 shouldBe number.immutable.SortedSet(1, 0)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = number.immutable.SortedSet(1, 2, 3).scanRight(0)(_ + _)
    result2 shouldBe number.immutable.SortedSet(6, 5, 3, 0)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = number.immutable.TreeSet(1).scanRight(0)(_ + _)
    result3 shouldBe number.immutable.TreeSet(1, 0)
    result3.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result4 = number.immutable.TreeSet(1, 2, 3).scanRight(0)(_ + _)
    result4 shouldBe number.immutable.TreeSet(6, 5, 3, 0)
    result4.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have an into.scanRight method" is pending
*/
  it should "have a slice method" in {
    val result1 = number.immutable.SortedSet(3).slice(0, 0)
    result1 shouldBe number.immutable.SortedSet()
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = number.immutable.SortedSet(1, 2, 3).slice(2, 1)
    result2 shouldBe number.immutable.SortedSet()
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = number.immutable.SortedSet(1, 2, 3).slice(1, 3)
    result3 shouldBe number.immutable.SortedSet(2, 3)
    result3.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result4 = number.immutable.TreeSet(3).slice(0, 0)
    result4 shouldBe number.immutable.TreeSet()
    result4.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result5 = number.immutable.TreeSet(1, 2, 3).slice(2, 1)
    result5 shouldBe number.immutable.TreeSet()
    result5.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result6 = number.immutable.TreeSet(1, 2, 3).slice(1, 3)
    result6 shouldBe number.immutable.TreeSet(2, 3)
    result6.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have 2 sliding methods" in {

    val result1 = number.immutable.SortedSet(1).sliding(1).toStandardList
    result1 shouldBe List(number.immutable.SortedSet(1))
    result1.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result2 = number.immutable.SortedSet(1).sliding(2).toStandardList
    result2 shouldBe List(number.immutable.SortedSet(1))
    result2.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result3 = number.immutable.SortedSet(1, 2, 3).sliding(2).toStandardList
    result3 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(2, 3))
    result3.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result4 = number.immutable.SortedSet(1, 2, 3).sliding(1).toStandardList
    result4 shouldBe List(number.immutable.SortedSet(1), number.immutable.Set(2), number.immutable.SortedSet(3))
    result4.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result5 = number.immutable.SortedSet(1, 2, 3).sliding(3).toStandardList
    result5 shouldBe List(number.immutable.SortedSet(1, 2, 3))
    result5.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result6 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(3).toStandardList
    result6 shouldBe List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(2, 3, 4), number.immutable.SortedSet(3, 4, 5))
    result6.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result7 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(2).toStandardList
    result7 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(2, 3), number.immutable.SortedSet(3, 4), number.immutable.SortedSet(4, 5))
    result7.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result8 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(1).toStandardList
    result8 shouldBe List(number.immutable.SortedSet(1), number.immutable.SortedSet(2), number.immutable.SortedSet(3), number.immutable.SortedSet(4), number.immutable.SortedSet(5))
    result8.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result9 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(4).toStandardList
    result9 shouldBe List(number.immutable.SortedSet(1, 2, 3, 4), number.immutable.SortedSet(2, 3, 4, 5))
    result9.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result10 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(5).toStandardList
    result10 shouldBe List(number.immutable.SortedSet(1, 2, 3, 4, 5))
    result10.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result11 = number.immutable.SortedSet(1).sliding(1, 1).toStandardList
    result11 shouldBe List(number.immutable.SortedSet(1))
    result11.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result12 = number.immutable.SortedSet(1).sliding(1, 2).toStandardList
    result12 shouldBe List(number.immutable.SortedSet(1))
    result12.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result13 = number.immutable.SortedSet(1, 2, 3).sliding(1, 1).toStandardList
    result13 shouldBe List(number.immutable.SortedSet(1), number.immutable.SortedSet(2), number.immutable.SortedSet(3))
    result13.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result14 = number.immutable.SortedSet(1, 2, 3).sliding(2, 1).toStandardList
    result14 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(2, 3))
    result14.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result15 = number.immutable.SortedSet(1, 2, 3).sliding(2, 2).toStandardList
    result15 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(3))
    result15.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result16 = number.immutable.SortedSet(1, 2, 3).sliding(3, 2).toStandardList
    result16 shouldBe List(number.immutable.SortedSet(1, 2, 3))
    result16.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result17 = number.immutable.SortedSet(1, 2, 3).sliding(3, 1).toStandardList
    result17 shouldBe List(number.immutable.SortedSet(1, 2, 3))
    result17.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result18 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(3, 1).toStandardList
    result18 shouldBe List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(2, 3, 4), number.immutable.SortedSet(3, 4, 5))
    result18.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result19 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(2, 2).toStandardList
    result19 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(3, 4), number.immutable.SortedSet(5))
    result19.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result20 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(2, 3).toStandardList
    result20 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(4, 5))
    result20.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result21 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(2, 4).toStandardList
    result21 shouldBe List(number.immutable.SortedSet(1, 2), number.immutable.SortedSet(5))
    result21.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result22 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(3, 1).toStandardList
    result22 shouldBe List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(2, 3, 4), number.immutable.SortedSet(3, 4, 5))
    result22.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result23 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(3, 2).toStandardList
    result23 shouldBe List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(3, 4, 5))
    result23.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result24 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(3, 3).toStandardList
    result24 shouldBe List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(4, 5))
    result24.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result25 = number.immutable.SortedSet(1, 2, 3, 4, 5).sliding(3, 4).toStandardList
    result25 shouldBe List(number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet(5))
    result25.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result26 = number.immutable.TreeSet(1).sliding(1).toStandardList
    result26 shouldBe List(number.immutable.TreeSet(1))
    result26.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result27 = number.immutable.TreeSet(1).sliding(2).toStandardList
    result27 shouldBe List(number.immutable.TreeSet(1))
    result27.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result28 = number.immutable.TreeSet(1, 2, 3).sliding(2).toStandardList
    result28 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(2, 3))
    result28.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result29 = number.immutable.TreeSet(1, 2, 3).sliding(1).toStandardList
    result29 shouldBe List(number.immutable.TreeSet(1), number.immutable.TreeSet(2), number.immutable.TreeSet(3))
    result29.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result30 = number.immutable.TreeSet(1, 2, 3).sliding(3).toStandardList
    result30 shouldBe List(number.immutable.TreeSet(1, 2, 3))
    result30.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result31 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(3).toStandardList
    result31 shouldBe List(number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet(2, 3, 4), number.immutable.TreeSet(3, 4, 5))
    result31.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result32 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(2).toStandardList
    result32 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(2, 3), number.immutable.TreeSet(3, 4), number.immutable.TreeSet(4, 5))
    result32.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result33 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(1).toStandardList
    result33 shouldBe List(number.immutable.TreeSet(1), number.immutable.TreeSet(2), number.immutable.TreeSet(3), number.immutable.TreeSet(4), number.immutable.TreeSet(5))
    result33.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result34 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(4).toStandardList
    result34 shouldBe List(number.immutable.TreeSet(1, 2, 3, 4), number.immutable.TreeSet(2, 3, 4, 5))
    result34.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result35 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(5).toStandardList
    result35 shouldBe List(number.immutable.TreeSet(1, 2, 3, 4, 5))
    result35.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result36 = number.immutable.TreeSet(1).sliding(1, 1).toStandardList
    result36 shouldBe List(number.immutable.TreeSet(1))
    result36.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result37 = number.immutable.TreeSet(1).sliding(1, 2).toStandardList
    result37 shouldBe List(number.immutable.TreeSet(1))
    result37.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result38 = number.immutable.TreeSet(1, 2, 3).sliding(1, 1).toStandardList
    result38 shouldBe List(number.immutable.TreeSet(1), number.immutable.TreeSet(2), number.immutable.TreeSet(3))
    result38.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result39 = number.immutable.TreeSet(1, 2, 3).sliding(2, 1).toStandardList
    result39 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(2, 3))
    result39.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result40 = number.immutable.TreeSet(1, 2, 3).sliding(2, 2).toStandardList
    result40 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(3))
    result40.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result41 = number.immutable.TreeSet(1, 2, 3).sliding(3, 2).toStandardList
    result41 shouldBe List(number.immutable.TreeSet(1, 2, 3))
    result41.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result42 = number.immutable.TreeSet(1, 2, 3).sliding(3, 1).toStandardList
    result42 shouldBe List(number.immutable.TreeSet(1, 2, 3))
    result42.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result43 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(3, 1).toStandardList
    result43 shouldBe List(number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet(2, 3, 4), number.immutable.TreeSet(3, 4, 5))
    result43.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result44 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(2, 2).toStandardList
    result44 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(3, 4), number.immutable.TreeSet(5))
    result44.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result45 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(2, 3).toStandardList
    result45 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(4, 5))
    result45.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result46 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(2, 4).toStandardList
    result46 shouldBe List(number.immutable.TreeSet(1, 2), number.immutable.TreeSet(5))
    result46.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result47 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(3, 1).toStandardList
    result47 shouldBe List(number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet(2, 3, 4), number.immutable.TreeSet(3, 4, 5))
    result47.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result48 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(3, 2).toStandardList
    result48 shouldBe List(number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet(3, 4, 5))
    result48.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result49 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(3, 3).toStandardList
    result49 shouldBe List(number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet(4, 5))
    result49.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]

    val result50 = number.immutable.TreeSet(1, 2, 3, 4, 5).sliding(3, 4).toStandardList
    result50 shouldBe List(number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet(5))
    result50.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]
  }
  it should "have a span method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).span(_ < 3)
    result1 shouldBe (number.immutable.SortedSet(1, 2), number.immutable.SortedSet(3))
    result1.shouldHaveExactType[(number.immutable.SortedSet[Int], number.immutable.SortedSet[Int])]

    val result2 = number.immutable.SortedSet(1, 2, 3).span(_ > 3)
    result2 shouldBe (number.immutable.SortedSet(), number.immutable.SortedSet(1, 2, 3))
    result2.shouldHaveExactType[(number.immutable.SortedSet[Int], number.immutable.SortedSet[Int])]

    val result3 = number.immutable.TreeSet(1, 2, 3).span(_ < 3)
    result3 shouldBe (number.immutable.TreeSet(1, 2), number.immutable.TreeSet(3))
    result3.shouldHaveExactType[(number.immutable.TreeSet[Int], number.immutable.TreeSet[Int])]

    val result4 = number.immutable.TreeSet(1, 2, 3).span(_ > 3)
    result4 shouldBe (number.immutable.TreeSet(), number.immutable.TreeSet(1, 2, 3))
    result4.shouldHaveExactType[(number.immutable.TreeSet[Int], number.immutable.TreeSet[Int])]
  }
  it should "have a splitAt method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).splitAt(0)
    result1 shouldBe (number.immutable.SortedSet(), number.immutable.SortedSet(1, 2, 3))
    result1.shouldHaveExactType[(number.immutable.SortedSet[Int], number.immutable.SortedSet[Int])]

    val result2 = number.immutable.SortedSet(1, 2, 3).splitAt(1)
    result2 shouldBe (number.immutable.SortedSet(1), number.immutable.SortedSet(2, 3))
    result2.shouldHaveExactType[(number.immutable.SortedSet[Int], number.immutable.SortedSet[Int])]

    val result3 = number.immutable.SortedSet(1, 2, 3).splitAt(2)
    result3 shouldBe (number.immutable.SortedSet(1, 2), number.immutable.SortedSet(3))
    result3.shouldHaveExactType[(number.immutable.SortedSet[Int], number.immutable.SortedSet[Int])]

    val result4 = number.immutable.SortedSet(1, 2, 3).splitAt(3)
    result4 shouldBe (number.immutable.SortedSet(1, 2, 3), number.immutable.SortedSet())
    result4.shouldHaveExactType[(number.immutable.SortedSet[Int], number.immutable.SortedSet[Int])]

    val result5 = number.immutable.TreeSet(1, 2, 3).splitAt(0)
    result5 shouldBe (number.immutable.TreeSet(), number.immutable.TreeSet(1, 2, 3))
    result5.shouldHaveExactType[(number.immutable.TreeSet[Int], number.immutable.TreeSet[Int])]

    val result6 = number.immutable.TreeSet(1, 2, 3).splitAt(1)
    result6 shouldBe (number.immutable.TreeSet(1), number.immutable.TreeSet(2, 3))
    result6.shouldHaveExactType[(number.immutable.TreeSet[Int], number.immutable.TreeSet[Int])]

    val result7 = number.immutable.TreeSet(1, 2, 3).splitAt(2)
    result7 shouldBe (number.immutable.TreeSet(1, 2), number.immutable.TreeSet(3))
    result7.shouldHaveExactType[(number.immutable.TreeSet[Int], number.immutable.TreeSet[Int])]

    val result8 = number.immutable.TreeSet(1, 2, 3).splitAt(3)
    result8 shouldBe (number.immutable.TreeSet(1, 2, 3), number.immutable.TreeSet())
    result8.shouldHaveExactType[(number.immutable.TreeSet[Int], number.immutable.TreeSet[Int])]
  }
  it should "have a stringPrefix method" in {
    number.immutable.SortedSet(1).stringPrefix shouldBe "TreeSet"
    number.immutable.SortedSet(1, 2, 3).stringPrefix shouldBe "TreeSet"
    lower.immutable.SortedSet("1").stringPrefix shouldBe "TreeSet"
    lower.immutable.SortedSet("1", "2", "3").stringPrefix shouldBe "TreeSet"
  }
  it should "have a subsetOf method" in {
    number.immutable.SortedSet(2, 3).subsetOf(number.immutable.SortedSet(1, 2, 3, 4, 5)) shouldBe true
    number.immutable.SortedSet(2).subsetOf(number.immutable.SortedSet(1, 2, 3, 4, 5)) shouldBe true
    number.immutable.SortedSet(2, 0).subsetOf(number.immutable.SortedSet(1, 2, 3, 4, 5)) shouldBe false
    lower.immutable.SortedSet("aa", "bb").subsetOf(lower.immutable.SortedSet("aa", "bb", "cc")) shouldBe true
    lower.immutable.SortedSet("aA", "Bb").subsetOf(lower.immutable.SortedSet("aa", "bb", "cc")) shouldBe true
    lower.immutable.SortedSet("aa", "bb").subsetOf(lower.immutable.SortedSet("aA", "Bb", "cC")) shouldBe true
    lower.immutable.SortedSet("aa", "bc").subsetOf(lower.immutable.SortedSet("aa", "bb", "cc")) shouldBe false
  }
  it should "have a subsets method" in {
    val subsets = number.immutable.SortedSet(1, 2, 3).subsets.toStandardList
    subsets should have length 8
    subsets should contain (number.immutable.SortedSet())
    subsets should contain (number.immutable.SortedSet(1))
    subsets should contain (number.immutable.SortedSet(2))
    subsets should contain (number.immutable.SortedSet(3))
    subsets should contain (number.immutable.SortedSet(1, 2))
    subsets should contain (number.immutable.SortedSet(1, 3))
    subsets should contain (number.immutable.SortedSet(2, 3))
    subsets should contain (number.immutable.SortedSet(1, 2, 3))

    val subsets2 = number.immutable.SortedSet(1, 2, 3).subsets(2).toStandardList
    subsets2 should have length 3
    subsets2 should contain (number.immutable.SortedSet(1, 2))
    subsets2 should contain (number.immutable.SortedSet(1, 3))
    subsets2 should contain (number.immutable.SortedSet(2, 3))

    number.immutable.SortedSet(1, 2, 3).subsets.shouldHaveExactType[Iterator[number.immutable.SortedSet[Int]]]
    number.immutable.SortedSet(1, 2, 3).subsets(2).shouldHaveExactType[Iterator[number.immutable.SortedSet[Int]]]

    number.immutable.TreeSet(1, 2, 3).subsets.shouldHaveExactType[Iterator[number.immutable.TreeSet[Int]]]
    number.immutable.TreeSet(1, 2, 3).subsets(2).shouldHaveExactType[Iterator[number.immutable.TreeSet[Int]]]
  }
  it should "have a sum method" in {
    number.immutable.SortedSet(1).sum shouldBe 1
    number.immutable.SortedSet(5).sum shouldBe 5
    number.immutable.SortedSet(1, 2, 3).sum shouldBe 6
    number.immutable.SortedSet(1, 2, 3, 4, 5).sum shouldBe 15
  }
  it should "have an tail method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).tail
    result1 shouldBe number.immutable.SortedSet(2, 3)
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = number.immutable.TreeSet(1, 2, 3).tail
    result2 shouldBe number.immutable.TreeSet(2, 3)
    result2.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have an tails method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).tails.toStandardList
    result1 shouldBe List(number.immutable.SortedSet(1,2,3), number.immutable.SortedSet(2,3), number.immutable.SortedSet(3), number.immutable.SortedSet())
    result1.shouldHaveExactType[List[number.immutable.SortedSet[Int]]]

    val result2 = number.immutable.TreeSet(1, 2, 3).tails.toStandardList
    result2 shouldBe List(number.immutable.TreeSet(1,2,3), number.immutable.TreeSet(2,3), number.immutable.TreeSet(3), number.immutable.TreeSet())
    result2.shouldHaveExactType[List[number.immutable.TreeSet[Int]]]
  }
  it should "have a take method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).take(0)
    result1 shouldBe number.immutable.SortedSet()
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = number.immutable.SortedSet(1, 2, 3).take(1)
    result2 shouldBe number.immutable.SortedSet(1)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = number.immutable.SortedSet(1, 2, 3).take(2)
    result3 shouldBe number.immutable.SortedSet(1, 2)
    result3.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result4 = number.immutable.SortedSet(1, 2, 3).take(3)
    result4 shouldBe number.immutable.SortedSet(1, 2, 3)
    result4.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result5 = number.immutable.TreeSet(1, 2, 3).take(0)
    result5 shouldBe number.immutable.TreeSet()
    result5.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result6 = number.immutable.TreeSet(1, 2, 3).take(1)
    result6 shouldBe number.immutable.TreeSet(1)
    result6.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result7 = number.immutable.TreeSet(1, 2, 3).take(2)
    result7 shouldBe number.immutable.TreeSet(1, 2)
    result7.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result8 = number.immutable.TreeSet(1, 2, 3).take(3)
    result8 shouldBe number.immutable.TreeSet(1, 2, 3)
    result8.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have a takeRight method" in {
    val result1 = number.immutable.SortedSet(1, 2, 3).takeRight(0)
    result1 shouldBe number.immutable.SortedSet()
    result1.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result2 = number.immutable.SortedSet(1, 2, 3).takeRight(1)
    result2 shouldBe number.immutable.SortedSet(3)
    result2.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result3 = number.immutable.SortedSet(1, 2, 3).takeRight(2)
    result3 shouldBe number.immutable.SortedSet(2, 3)
    result3.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result4 = number.immutable.SortedSet(1, 2, 3).takeRight(3)
    result4 shouldBe number.immutable.SortedSet(1, 2, 3)
    result4.shouldHaveExactType[number.immutable.SortedSet[Int]]

    val result5 = number.immutable.TreeSet(1, 2, 3).takeRight(0)
    result5 shouldBe number.immutable.TreeSet()
    result5.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result6 = number.immutable.TreeSet(1, 2, 3).takeRight(1)
    result6 shouldBe number.immutable.TreeSet(3)
    result6.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result7 = number.immutable.TreeSet(1, 2, 3).takeRight(2)
    result7 shouldBe number.immutable.TreeSet(2, 3)
    result7.shouldHaveExactType[number.immutable.TreeSet[Int]]

    val result8 = number.immutable.TreeSet(1, 2, 3).takeRight(3)
    result8 shouldBe number.immutable.TreeSet(1, 2, 3)
    result8.shouldHaveExactType[number.immutable.TreeSet[Int]]
  }
  it should "have a toArray method" in {
    number.immutable.SortedSet(1, 2, 3).toArray shouldBe Array(1, 2, 3)
    lower.immutable.SortedSet("a", "b").toArray shouldBe Array("a", "b")
    number.immutable.SortedSet(1).toArray shouldBe Array(1)
  }
  it should "have a toBoxArray method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxArray shouldBe Array(number.Box[Int](1), number.Box[Int](2), number.Box[Int](3))
    lower.immutable.SortedSet("a", "b").toBoxArray shouldBe Array(lower.Box("a"), lower.Box("b"))
    number.immutable.SortedSet(1).toBoxArray shouldBe Array(number.Box[Int](1))
  }
  it should "have a toBuffer method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardBuffer shouldBe (Buffer(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardBuffer shouldBe (Buffer("a", "b"))
    number.immutable.SortedSet(1).toStandardBuffer shouldBe (Buffer(1))
  }
  it should "have a toBoxBuffer method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardBuffer shouldBe (Buffer(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardBuffer shouldBe (Buffer(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardBuffer shouldBe (Buffer(number.Box(1)))
  }
  it should "have a toIndexedSeq method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardIndexedSeq shouldBe (IndexedSeq(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardIndexedSeq shouldBe (IndexedSeq("a", "b"))
    number.immutable.SortedSet(1).toStandardIndexedSeq shouldBe (IndexedSeq(1))
  }
  it should "have a toBoxIndexedSeq method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardIndexedSeq shouldBe (IndexedSeq(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardIndexedSeq shouldBe (IndexedSeq(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardIndexedSeq shouldBe (IndexedSeq(number.Box(1)))
  }
  it should "have a toIterable method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardIterable shouldBe (Set(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardIterable shouldBe (Set("a", "b"))
    number.immutable.SortedSet(1).toStandardIterable shouldBe (Set(1))
  }
  it should "have a toBoxIterable method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardIterable shouldBe (Set(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardIterable shouldBe (Set(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardIterable shouldBe (Set(number.Box(1)))
  }
  it should "have a toIterator method" is pending
  it should "have a toStandardIterator method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardIterator.toList shouldBe (StdIterator(1, 2, 3).toList)
    lower.immutable.SortedSet("a", "b").toStandardIterator.toList shouldBe (StdIterator("a", "b").toList)
    number.immutable.SortedSet(1).toStandardIterator.toList shouldBe (StdIterator(1).toList)
    number.immutable.SortedSet(1, 2, 3).toStandardIterator shouldBe an [StdIterator[_]]
    lower.immutable.SortedSet("a", "b").toStandardIterator shouldBe an [StdIterator[_]]
    number.immutable.SortedSet(1).toStandardIterator shouldBe an [StdIterator[_]]
  }
  it should "have a toStandardBoxIterator method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardIterator.toList shouldBe (StdIterator(number.Box(1), number.Box(2), number.Box(3)).toList)
    lower.immutable.SortedSet("a", "b").toBoxStandardIterator.toList shouldBe (StdIterator(lower.Box("a"), lower.Box("b")).toList)
    number.immutable.SortedSet(1).toBoxStandardIterator.toList shouldBe (StdIterator(number.Box(1)).toList)
    number.immutable.SortedSet(1, 2, 3).toBoxStandardIterator shouldBe an [StdIterator[_]]
    lower.immutable.SortedSet("a", "b").toBoxStandardIterator shouldBe an [StdIterator[_]]
    number.immutable.SortedSet(1).toBoxStandardIterator shouldBe an [StdIterator[_]]
  }
  it should "have a toList method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardList shouldBe (List(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardList shouldBe (List("a", "b"))
    number.immutable.SortedSet(1).toStandardList shouldBe (List(1))
  }
  it should "have a toBoxList method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardList shouldBe (List(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardList shouldBe (List(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardList shouldBe (List(number.Box(1)))
  }
  it should "have a toMap method" in {
    numberLower.immutable.SortedSet((1, "one"), (2, "two"), (3, "three")).toStandardMap shouldBe Map(1 -> "one", 2 -> "two", 3 -> "three")
  }
  it should "have a toParArray method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardParArray shouldBe ParArray(1, 2, 3)
  }
  it should "have a toBoxParArray method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardParArray shouldBe ParArray(number.Box(1), number.Box(2), number.Box(3))
  }
  it should "have a toSeq method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardSeq shouldBe (Seq(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardSeq shouldBe (Seq("a", "b"))
    number.immutable.SortedSet(1).toStandardSeq shouldBe (Seq(1))
  }
  it should "have a toBoxSeq method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardSeq shouldBe (Seq(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardSeq shouldBe (Seq(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardSeq shouldBe (Seq(number.Box(1)))
  }
  it should "have a toStream method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardStream shouldBe (Stream(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardStream shouldBe (Stream("a", "b"))
    number.immutable.SortedSet(1).toStandardStream shouldBe(Stream(1))
  }
  it should "have a toBoxStream method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardStream shouldBe (Stream(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardStream shouldBe (Stream(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardStream shouldBe(Stream(number.Box(1)))
  }
  it should "have a toTraversable method" in {
    implicit val numberOrdering = new Ordering[number.Box[Int]] {
      def compare(x: number.Box[Int], y: number.Box[Int]): Int = x.value - y.value
    }
    implicit val lowerOrdering = new Ordering[lower.Box[String]] {
      def compare(x: lower.Box[String], y: lower.Box[String]): Int = x.value compareTo y.value
    }
    number.immutable.SortedSet(1, 2, 3).toStandardTraversable should === (StandardTreeSet(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardTraversable should === (StandardTreeSet("a", "b"))
    number.immutable.SortedSet(1).toStandardTraversable should === (StandardTreeSet(1))
  }
  it should "have a toBoxTraversable method" in {
    implicit val numberOrdering = new Ordering[number.Box[Int]] {
      def compare(x: number.Box[Int], y: number.Box[Int]): Int = x.value - y.value
    }
    implicit val lowerOrdering = new Ordering[lower.Box[String]] {
      def compare(x: lower.Box[String], y: lower.Box[String]): Int = x.value compareTo y.value
    }
    number.immutable.SortedSet(1, 2, 3).toBoxStandardTraversable should === (StandardTreeSet(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardTraversable should === (StandardTreeSet(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardTraversable should === (StandardTreeSet(number.Box(1)))
  }
  it should "have a toBoxVector method" in {
    number.immutable.SortedSet(1, 2, 3).toBoxStandardVector should === (Vector(number.Box(1), number.Box(2), number.Box(3)))
    lower.immutable.SortedSet("a", "b").toBoxStandardVector should === (Vector(lower.Box("a"), lower.Box("b")))
    number.immutable.SortedSet(1).toBoxStandardVector should === (Vector(number.Box(1)))
  }
  it should "have a toVector method" in {
    number.immutable.SortedSet(1, 2, 3).toStandardVector should === (Vector(1, 2, 3))
    lower.immutable.SortedSet("a", "b").toStandardVector should === (Vector("a", "b"))
    number.immutable.SortedSet(1).toStandardVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    val result1 = numberList.immutable.SortedSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose
    result1 shouldBe numberList.immutable.SortedSet(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    result1.shouldHaveExactType[numberList.immutable.SortedSet[List[Int]]]

    val result2 = numberList.immutable.SortedSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose
    result2 shouldBe numberList.immutable.SortedSet(List(1, 3, 5, 7), List(2, 4, 6, 8))
    result2.shouldHaveExactType[numberList.immutable.SortedSet[List[Int]]]

    val result3 = numberList.immutable.SortedSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose.transpose
    result3 shouldBe numberList.immutable.SortedSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8))
    result3.shouldHaveExactType[numberList.immutable.SortedSet[List[Int]]]

    val result4 = numberList.immutable.SortedSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose.transpose
    result4 shouldBe numberList.immutable.SortedSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    result4.shouldHaveExactType[numberList.immutable.SortedSet[List[Int]]]

    val result5 = numberList.immutable.TreeSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose
    result5 shouldBe numberList.immutable.TreeSet(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    result5.shouldHaveExactType[numberList.immutable.TreeSet[List[Int]]]

    val result6 = numberList.immutable.TreeSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose
    result6 shouldBe numberList.immutable.TreeSet(List(1, 3, 5, 7), List(2, 4, 6, 8))
    result6.shouldHaveExactType[numberList.immutable.TreeSet[List[Int]]]

    val result7 = numberList.immutable.TreeSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose.transpose
    result7 shouldBe numberList.immutable.TreeSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8))
    result7.shouldHaveExactType[numberList.immutable.TreeSet[List[Int]]]

    val result8 = numberList.immutable.TreeSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose.transpose
    result8 shouldBe numberList.immutable.TreeSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    result8.shouldHaveExactType[numberList.immutable.TreeSet[List[Int]]]
  }

  it should "have a zip method" in {
    """number.immutable.SortedSet(1, 2, 3).zip(List("4", "5", "6"))""" shouldNot typeCheck
    """number.immutable.SortedSet(1, 2, 3).zip(List("4", "5"))""" shouldNot typeCheck

    number.immutable.SortedSet(1, 2, 3).toStandardSet.zip(List("4", "5", "6")) should contain theSameElementsAs Set((1, "4"), (2, "5"), (3, "6"))
    number.immutable.SortedSet(1, 2, 3).toStandardSet.zip(List("4", "5")) should contain theSameElementsAs Set((1, "4"), (2, "5"))
  }
  it should "have a zipAll method" in {
    """number.immutable.SortedSet(1, 2, 3).zipAll(List("4", "5", "6"), 0, "0")""" shouldNot typeCheck
    """number.immutable.SortedSet(1, 2, 3).zipAll(List("4", "5"), 0, "0")""" shouldNot typeCheck
    """number.immutable.SortedSet(1, 2).zipAll(List("4", "5", "6"), 0, "0")""" shouldNot typeCheck

    number.immutable.SortedSet(1, 2, 3).toStandardSet.zipAll(List("4", "5", "6"), 0, "0") should contain theSameElementsAs Set((1, "4"), (2, "5"), (3, "6"))
    number.immutable.SortedSet(1, 2, 3).toStandardSet.zipAll(List("4", "5"), 0, "0") should contain theSameElementsAs Set((1, "4"), (2, "5"), (3, "0"))
    number.immutable.SortedSet(1, 2).toStandardSet.zipAll(List("4", "5", "6"), 0, "0") should contain theSameElementsAs Set((1, "4"), (2, "5"), (0, "6"))
  }
  it should "have a zipWithIndex method" in {
    """number.immutable.SortedSet(99).zipWithIndex""" shouldNot typeCheck
    """number.immutable.SortedSet(1, 2, 3).zipWithIndex""" shouldNot typeCheck

    number.immutable.SortedSet(99).toStandardSet.zipWithIndex should contain theSameElementsAs Set((99,0))
    number.immutable.SortedSet(1, 2, 3).toStandardSet.zipWithIndex should contain theSameElementsAs Set((1,0), (2,1), (3,2))
  }

  it should "have a filter method after it is converted into EquaBridge with into" is pending

  it should "have a withFilter method after it is converted into EquaBridge with into" is pending

  it should "return an equal set if toSortedSet is called after view" in {
    val actual = number.immutable.SortedSet(1, 2, 3).view.toSortedSet(number)
    actual should equal (number.immutable.Set(1, 2, 3))
  }

  "TreeSetView" should "offer a lazy map method" in {
    val lazySeq = trimmed.immutable.SortedSet("1", "2", "01", "3").view
    var performed = false
    val toIntFun = (s: String) => { performed = true; s.toInt }
    val mappedTreeSetView = lazySeq.map(toIntFun).map(_ + 1)
    performed shouldBe false
    val strictSet = mappedTreeSetView.toSortedSet(number)
    performed shouldBe true
    strictSet should equal (number.immutable.SortedSet(2, 3, 4))
  }

  it should "offer an equals method that looks at processed elements" in {
    val trimmedSeq = trimmed.immutable.SortedSet("1", "2", "01", "3").view
    val lowerSeq = trimmed.immutable.SortedSet("2", "3", "02", "4").view
    val modifiedTrimmedSeq = trimmedSeq.map(_.toInt).map(_ + 1)
    val modifiedLowerSeq = lowerSeq.map(_.toInt)
    modifiedTrimmedSeq should equal (modifiedLowerSeq)
    val thirdModifiedSeq = lowerSeq.map(_.toInt).map(_ + 2)
    modifiedTrimmedSeq should not equal thirdModifiedSeq
  }

  it should "offer a hashCode method that looks at processed elements" in {
    val trimmedSeq = trimmed.immutable.SortedSet("1", "2", "01", "3").view
    val lowerSeq = trimmed.immutable.SortedSet("2", "3", "02", "4").view
    val modifiedTrimmedSeq = trimmedSeq.map(_.toInt).map(_ + 1)
    val modifiedLowerSeq = lowerSeq.map(_.toInt)
    modifiedTrimmedSeq.hashCode should equal (modifiedLowerSeq.hashCode)
    val thirdModifiedSeq = lowerSeq.map(_.toInt).map(_ + 2)
    modifiedTrimmedSeq.hashCode should not equal thirdModifiedSeq.hashCode // Overspecified
  }
  "The collections value" should "be a nice reference to a default SortedCollections[Any]" in {

    val sortedIntCollections = SortedCollections[Int]
    import sortedIntCollections._

    Set(1, 2, 3) intersect Set(2, 3, 4) shouldEqual Set(2, 3)
    FastSet(1, 2, 3) intersect FastSet(2, 3, 4) shouldEqual FastSet(2, 3)
    SortedSet(1, 2, 3) intersect SortedSet(2, 3, 4) shouldEqual SortedSet(2, 3)
    TreeSet(1, 2, 3) intersect TreeSet(2, 3, 4) shouldEqual TreeSet(2, 3)

    immutable.Set(1, 2, 3) intersect immutable.Set(2, 3, 4) shouldEqual immutable.Set(2, 3)
    immutable.FastSet(1, 2, 3) intersect immutable.FastSet(2, 3, 4) shouldEqual immutable.FastSet(2, 3)
    immutable.SortedSet(1, 2, 3) intersect immutable.SortedSet(2, 3, 4) shouldEqual immutable.SortedSet(2, 3)
    immutable.TreeSet(1, 2, 3) intersect immutable.TreeSet(2, 3, 4) shouldEqual immutable.TreeSet(2, 3)
  }
}

