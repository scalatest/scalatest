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
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParArray

/*
val t = Collections[String](StringNormalizations.trimmed.toHashingEquality)
val w = SortedCollections[String](StringNormalizations.lowerCased.toOrderingEquality)
val tes = t.EquaSet("tes")
val tfes = t.FastEquaSet("tfes")
val wes = w.EquaSet("les")
val wfes = w.FastEquaSet("lfes")
val wses = w.SortedEquaSet("lses")
val wtes = w.TreeEquaSet("ltes")
*/
class EquaSetSpec extends UnitSpec {
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
  val number = Collections[Int](normalHashingEquality[Int])
  val sortedNumber = SortedCollections[Int](normalOrderingEquality[Int])
  val lower = Collections[String](StringNormalizations.lowerCased.toHashingEquality)
  val trimmed = Collections[String](StringNormalizations.trimmed.toHashingEquality)
  val sortedLower = SortedCollections[String](StringNormalizations.lowerCased.toOrderingEquality)
  val numberList = Collections[List[Int]](normalHashingEquality[List[Int]])
  val numberLower = Collections[(Int, String)](normalHashingEquality[(Int, String)])
  val numberLowerTrimmed = Collections[(Int, String, String)](normalHashingEquality[(Int, String, String)])
  val numberNumber = Collections[number.immutable.EquaSet[Int]](normalHashingEquality[number.immutable.EquaSet[Int]])
  def upperCharHashingEquality =
    new HashingEquality[Char] {
      def hashCodeFor(a: Char): Int = a.toUpper.hashCode
      def areEqual(a: Char, b: Any): Boolean =
        b match {
          case bChar: Char => a.toUpper == bChar.toUpper
          case _ => false
        }
    }
  val upperChar = Collections[Char](upperCharHashingEquality)
  val regularChar = Collections[Char](normalHashingEquality[Char])
  val tuple = Collections[(Int, String)](normalHashingEquality)

  "An EquaSet" can "be constructed with empty" in {
    val emptySet = lower.immutable.EquaSet.empty
    emptySet shouldBe empty
  }
  it can "be constructed with apply" in {
    val nonEmptySet = lower.immutable.EquaSet("one", "two", "three")
    nonEmptySet should have size 3
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for EquaSet.
  }
  it should "construct only sets with appropriate element types" in {
    "lower.immutable.EquaSet(1, 2, 3)" shouldNot compile
  }
  it should "eliminate 'duplicate' entries passed to the apply factory method" in {
    val nonEmptySet = lower.immutable.EquaSet("one", "two", "two", "three", "Three")
    nonEmptySet should have size 3
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for EquaSet.
  }
  it should "have a toString method" in {
    lower.immutable.EquaSet("hi", "ho").toString should === ("EquaSet(hi, ho)")
  }
  it should "have a diff method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.immutable.EquaSet("hi", "ho") diff lower.immutable.EquaSet("HI", "HO")
    result1 shouldBe lower.immutable.EquaSet()
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = trimmed.immutable.EquaSet("hi", "ho") diff trimmed.immutable.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.immutable.EquaSet()
    result2.shouldHaveExactType[trimmed.immutable.EquaSet[String]]

    """lower.immutable.EquaSet(" hi ", "hi") diff trimmed.immutable.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.immutable.EquaSet("hi", "ho") diff lower.immutable.EquaSet("ho")
    result3 shouldBe lower.immutable.EquaSet("hi")
    result3.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result4 = lower.immutable.EquaSet("hi", "ho", "let's", "go") diff lower.immutable.EquaSet("bo", "no", "go", "ho")
    result4 shouldBe lower.immutable.EquaSet("hi", "let's")
    result4.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result5 = lower.immutable.FastEquaSet("hi", "ho") diff lower.immutable.FastEquaSet("HI", "HO")
    result5 shouldBe lower.immutable.FastEquaSet()
    result5.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result6 = trimmed.immutable.FastEquaSet("hi", "ho") diff trimmed.immutable.FastEquaSet(" hi ", " ho ")
    result6 shouldBe trimmed.immutable.FastEquaSet()
    result6.shouldHaveExactType[trimmed.immutable.FastEquaSet[String]]

    """lower.immutable.FastEquaSet(" hi ", "hi") diff trimmed.immutable.FastEquaSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.immutable.FastEquaSet("hi", "ho") diff lower.immutable.FastEquaSet("ho")
    result7 shouldBe lower.immutable.FastEquaSet("hi")
    result7.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result8 = lower.immutable.FastEquaSet("hi", "ho", "let's", "go") diff lower.immutable.FastEquaSet("bo", "no", "go", "ho")
    result8 shouldBe lower.immutable.FastEquaSet("hi", "let's")
    result8.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have an intersect method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.immutable.EquaSet("hi", "ho") intersect lower.immutable.EquaSet("HI", "HO")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = trimmed.immutable.EquaSet("hi", "ho") intersect trimmed.immutable.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.immutable.EquaSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.immutable.EquaSet[String]]

    """lower.immutable.EquaSet(" hi ", "hi") intersect trimmed.immutable.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.immutable.EquaSet("hi", "ho") intersect lower.immutable.EquaSet("ho")
    result3 shouldBe lower.immutable.EquaSet("ho")
    result3.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result4 = lower.immutable.EquaSet("hi", "ho", "let's", "go") intersect lower.immutable.EquaSet("bo", "no", "go", "ho")
    result4 shouldBe lower.immutable.EquaSet("ho", "go")
    result4.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result5 = lower.immutable.FastEquaSet("hi", "ho") intersect lower.immutable.FastEquaSet("HI", "HO")
    result5 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result6 = trimmed.immutable.FastEquaSet("hi", "ho") intersect trimmed.immutable.FastEquaSet(" hi ", " ho ")
    result6 shouldBe trimmed.immutable.FastEquaSet("hi", "ho")
    result6.shouldHaveExactType[trimmed.immutable.FastEquaSet[String]]

    """lower.immutable.FastEquaSet(" hi ", "hi") intersect trimmed.immutable.FastEquaSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.immutable.FastEquaSet("hi", "ho") intersect lower.immutable.FastEquaSet("ho")
    result7 shouldBe lower.immutable.FastEquaSet("ho")
    result7.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result8 = lower.immutable.FastEquaSet("hi", "ho", "let's", "go") intersect lower.immutable.FastEquaSet("bo", "no", "go", "ho")
    result8 shouldBe lower.immutable.FastEquaSet("ho", "go")
    result8.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have a union method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.immutable.EquaSet("hi", "ho") union lower.immutable.EquaSet("HI", "HO")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = trimmed.immutable.EquaSet("hi", "ho") union trimmed.immutable.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.immutable.EquaSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.immutable.EquaSet[String]]

    """lower.immutable.EquaSet(" hi ", "hi") union trimmed.immutable.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.immutable.FastEquaSet("hi", "ho") union lower.immutable.FastEquaSet("HI", "HO")
    result3 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result4 = trimmed.immutable.FastEquaSet("hi", "ho") union trimmed.immutable.FastEquaSet(" hi ", " ho ")
    result4 shouldBe trimmed.immutable.FastEquaSet("hi", "ho")
    result4.shouldHaveExactType[trimmed.immutable.FastEquaSet[String]]

    """lower.immutable.FastEquaSet(" hi ", "hi") union trimmed.immutable.FastEquaSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a toSet method" in {
    lower.immutable.EquaSet("hi", "ho").toSet should === (Set("hi", "ho"))
  }
  it should "have a toEquaBoxSet method" in {
    lower.immutable.EquaSet("hi", "ho").toEquaBoxSet should === (Set(lower.EquaBox("hi"), lower.EquaBox("ho")))
  }
  it should "have a + method that takes one argument" in {
    val result1 = lower.immutable.EquaSet("hi", "ho") + "ha"
    result1 shouldBe lower.immutable.EquaSet("hi", "ho", "ha")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho") + "HO"
    result2 shouldBe lower.immutable.EquaSet("hi", "ho")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.FastEquaSet("hi", "ho") + "ha"
    result3 shouldBe lower.immutable.FastEquaSet("hi", "ho", "ha")
    result3.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result4 = lower.immutable.FastEquaSet("hi", "ho") + "HO"
    result4 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result4.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have a + method that takes two or more arguments" in {
    val result1 = lower.immutable.EquaSet("hi", "ho") + ("ha", "hey!")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho") + ("HO", "hoe", "Ho!")
    result2 shouldBe lower.immutable.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.FastEquaSet("hi", "ho") + ("ha", "hey!")
    result3 shouldBe lower.immutable.FastEquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result4 = lower.immutable.FastEquaSet("hi", "ho") + ("HO", "hoe", "Ho!")
    result4 shouldBe lower.immutable.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have a - method that takes one argument" in {
    val result1 = lower.immutable.EquaSet("hi", "ho", "ha") - "ha"
    result1 shouldBe lower.immutable.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho") - "HO"
    result2 shouldBe lower.immutable.EquaSet("hi")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.EquaSet("hi", "ho") - "who?"
    result3 shouldBe lower.immutable.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result4 = lower.immutable.FastEquaSet("hi", "ho", "ha") - "ha"
    result4 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result4.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result5 = lower.immutable.FastEquaSet("hi", "ho") - "HO"
    result5 shouldBe lower.immutable.FastEquaSet("hi")
    result5.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result6 = lower.immutable.FastEquaSet("hi", "ho") - "who?"
    result6 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result6.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have a - method that takes two or more arguments" in {
    val result1 = lower.immutable.EquaSet("hi", "ho", "ha") - ("ha", "howdy!")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")
    result2 shouldBe lower.immutable.EquaSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.EquaSet("hi", "ho") - ("who", "goes", "thar")
    result3 shouldBe lower.immutable.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result4 = lower.immutable.EquaSet("hi", "ho") - ("HI", "HO")
    result4 shouldBe lower.immutable.EquaSet.empty
    result4.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result5 = lower.immutable.FastEquaSet("hi", "ho", "ha") - ("ha", "howdy!")
    result5 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result6 = lower.immutable.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")
    result6 shouldBe lower.immutable.FastEquaSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result7 = lower.immutable.FastEquaSet("hi", "ho") - ("who", "goes", "thar")
    result7 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result7.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result8 = lower.immutable.FastEquaSet("hi", "ho") - ("HI", "HO")
    result8 shouldBe lower.immutable.FastEquaSet.empty
    result8.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "return an iterator that returns the set's elements" in {
    lower.immutable.EquaSet("hi", "ho", "ha", "he").iterator.toList should contain theSameElementsAs List("ha", "he", "hi", "ho")
  }
  it should "have a ++ method that takes a GenTraversableOnce" in {
    val result1 = lower.immutable.EquaSet("hi", "ho") ++ List("ha", "hey!")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho") ++ List("HO", "hoe", "Ho!")
    result2 shouldBe lower.immutable.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.EquaSet("hi", "ho") ++ Set("ha", "hey!")
    result3 shouldBe lower.immutable.EquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result4 = lower.immutable.EquaSet("hi", "ho") ++ Set("HO", "hoe", "Ho!")
    result4 shouldBe lower.immutable.EquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result5 = lower.immutable.EquaSet("hi", "ho") ++ Vector("ha", "hey!")
    result5 shouldBe lower.immutable.EquaSet("hi", "ho", "ha", "hey!")
    result5.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result6 = lower.immutable.EquaSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!")
    result6 shouldBe lower.immutable.EquaSet("hi", "ho", "hoe", "Ho!")
    result6.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result7 = lower.immutable.FastEquaSet("hi", "ho") ++ List("ha", "hey!")
    result7 shouldBe lower.immutable.FastEquaSet("hi", "ho", "ha", "hey!")
    result7.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result8 = lower.immutable.FastEquaSet("hi", "ho") ++ List("HO", "hoe", "Ho!")
    result8 shouldBe lower.immutable.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result8.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result9 = lower.immutable.FastEquaSet("hi", "ho") ++ Set("ha", "hey!")
    result9 shouldBe lower.immutable.FastEquaSet("hi", "ho", "ha", "hey!")
    result9.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result10 = lower.immutable.FastEquaSet("hi", "ho") ++ Set("HO", "hoe", "Ho!")
    result10 shouldBe lower.immutable.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result10.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result11 = lower.immutable.FastEquaSet("hi", "ho") ++ Vector("ha", "hey!")
    result11 shouldBe lower.immutable.FastEquaSet("hi", "ho", "ha", "hey!")
    result11.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result12 = lower.immutable.FastEquaSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!")
    result12 shouldBe lower.immutable.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result12.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have a ++ method that takes another EquaSet" in {
    val result1 = lower.immutable.EquaSet("hi", "ho") ++ lower.immutable.EquaSet("ha", "hey!")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho") ++ lower.immutable.EquaSet("HO", "hoe", "Ho!")
    result2 shouldBe lower.immutable.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.FastEquaSet("hi", "ho") ++ lower.immutable.FastEquaSet("ha", "hey!")
    result3 shouldBe lower.immutable.FastEquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result4 = lower.immutable.FastEquaSet("hi", "ho") ++ lower.immutable.FastEquaSet("HO", "hoe", "Ho!")
    result4 shouldBe lower.immutable.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have a -- method that takes a GenTraversableOnce" in {
    val result1 = lower.immutable.EquaSet("hi", "ho", "ha") -- List("ha", "howdy!")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")
    result2 shouldBe lower.immutable.EquaSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.EquaSet("hi", "ho") -- List("who", "goes", "thar")
    result3 shouldBe lower.immutable.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result4 = lower.immutable.EquaSet("hi", "ho") -- List("HI", "HO")
    result4 shouldBe lower.immutable.EquaSet.empty
    result4.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result5 = lower.immutable.EquaSet("hi", "ho", "ha") -- Set("ha", "howdy!")
    result5 shouldBe lower.immutable.EquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result6 = lower.immutable.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")
    result6 shouldBe lower.immutable.EquaSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result7 = lower.immutable.EquaSet("hi", "ho") -- Set("who", "goes", "thar")
    result7 shouldBe lower.immutable.EquaSet("hi", "ho")
    result7.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result8 = lower.immutable.EquaSet("hi", "ho") -- Set("HI", "HO")
    result8 shouldBe lower.immutable.EquaSet.empty
    result8.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result9 = lower.immutable.EquaSet("hi", "ho", "ha") -- Vector("ha", "howdy!")
    result9 shouldBe lower.immutable.EquaSet("hi", "ho")
    result9.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result10 = lower.immutable.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")
    result10 shouldBe lower.immutable.EquaSet("hi", "fee", "foe")
    result10.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result11 = lower.immutable.EquaSet("hi", "ho") -- Vector("who", "goes", "thar")
    result11 shouldBe lower.immutable.EquaSet("hi", "ho")
    result11.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result12 = lower.immutable.EquaSet("hi", "ho") -- Vector("HI", "HO")
    result12 shouldBe lower.immutable.EquaSet.empty
    result12.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result13 = lower.immutable.FastEquaSet("hi", "ho", "ha") -- List("ha", "howdy!")
    result13 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result13.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result14 = lower.immutable.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")
    result14 shouldBe lower.immutable.FastEquaSet("hi", "fee", "foe")
    result14.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result15 = lower.immutable.FastEquaSet("hi", "ho") -- List("who", "goes", "thar")
    result15 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result15.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result16 = lower.immutable.FastEquaSet("hi", "ho") -- List("HI", "HO")
    result16 shouldBe lower.immutable.FastEquaSet.empty
    result16.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result17 = lower.immutable.FastEquaSet("hi", "ho", "ha") -- Set("ha", "howdy!")
    result17 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result17.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result18 = lower.immutable.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")
    result18 shouldBe lower.immutable.FastEquaSet("hi", "fee", "foe")
    result18.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result19 = lower.immutable.FastEquaSet("hi", "ho") -- Set("who", "goes", "thar")
    result19 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result19.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result20 = lower.immutable.FastEquaSet("hi", "ho") -- Set("HI", "HO")
    result20 shouldBe lower.immutable.FastEquaSet.empty
    result20.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result21 = lower.immutable.FastEquaSet("hi", "ho", "ha") -- Vector("ha", "howdy!")
    result21 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result21.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result22 = lower.immutable.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")
    result22 shouldBe lower.immutable.FastEquaSet("hi", "fee", "foe")
    result22.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result23 = lower.immutable.FastEquaSet("hi", "ho") -- Vector("who", "goes", "thar")
    result23 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result23.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result24 = lower.immutable.FastEquaSet("hi", "ho") -- Vector("HI", "HO")
    result24 shouldBe lower.immutable.FastEquaSet.empty
    result24.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have a -- method that takes another EquaSet" in {
    val result1 = lower.immutable.EquaSet("hi", "ho", "ha") -- lower.immutable.EquaSet("ha", "howdy!")
    result1 shouldBe lower.immutable.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.immutable.EquaSet("HO", "FIE", "fUm")
    result2 shouldBe lower.immutable.EquaSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.EquaSet("hi", "ho") -- lower.immutable.EquaSet("who", "goes", "thar")
    result3 shouldBe lower.immutable.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result4 = lower.immutable.EquaSet("hi", "ho") -- lower.immutable.EquaSet("HI", "HO")
    result4 shouldBe lower.immutable.EquaSet.empty
    result4.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result5 = lower.immutable.FastEquaSet("hi", "ho", "ha") -- lower.immutable.FastEquaSet("ha", "howdy!")
    result5 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result6 = lower.immutable.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.immutable.FastEquaSet("HO", "FIE", "fUm")
    result6 shouldBe lower.immutable.FastEquaSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result7 = lower.immutable.FastEquaSet("hi", "ho") -- lower.immutable.FastEquaSet("who", "goes", "thar")
    result7 shouldBe lower.immutable.FastEquaSet("hi", "ho")
    result7.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result8 = lower.immutable.FastEquaSet("hi", "ho") -- lower.immutable.FastEquaSet("HI", "HO")
    result8 shouldBe lower.immutable.FastEquaSet.empty
    result8.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
  it should "have 3 addString methods" in {
    lower.immutable.EquaSet("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    number.immutable.EquaSet(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    lower.immutable.EquaSet("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    number.immutable.EquaSet(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    number.immutable.EquaSet(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    lower.immutable.EquaSet("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    number.immutable.EquaSet(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    number.immutable.EquaSet(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have a aggregate method" in {
    lower.immutable.EquaSet("hi", "ho", "ha", "hey!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "ha", "hey!")

    lower.immutable.EquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "hoe", "Ho!")

    val result1 = lower.immutable.EquaSet("hi", "ho", "ha", "hey!").aggregate(lower.immutable.EquaSet[String]())(_ + _, _ ++ _)
    result1 shouldBe lower.immutable.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result2 = lower.immutable.EquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.immutable.EquaSet[String]())(_ + _, _ ++ _)
    result2 shouldBe lower.immutable.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.immutable.EquaSet[String]]

    val result3 = lower.immutable.FastEquaSet("hi", "ho", "ha", "hey!").aggregate(lower.immutable.FastEquaSet[String]())(_ + _, _ ++ _)
    result3 shouldBe lower.immutable.FastEquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.immutable.FastEquaSet[String]]

    val result4 = lower.immutable.FastEquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.immutable.FastEquaSet[String]())(_ + _, _ ++ _)
    result4 shouldBe lower.immutable.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.immutable.FastEquaSet[String]]
  }
/*
  it should "have an apply method" in {
    val a = number.immutable.EquaSet(1, 2, 3)
    a(2) shouldEqual true
    a(5) shouldEqual false

    val b = lower.immutable.EquaSet("hi")
    b("hi") shouldEqual true
    b("Hi") shouldEqual true
    b("hI") shouldEqual true
    b("HI") shouldEqual true
    b("he") shouldEqual false
  }
*/
/*
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = number.immutable.EquaSet(1) andThen (!_)
    pf1(1) shouldEqual false
    pf1(2) shouldEqual true

    val pf2 = number.immutable.EquaSet(1, 2, 3) andThen (!_)
    pf2(1) shouldEqual false
    pf2(2) shouldEqual false
    pf2(3) shouldEqual false
    pf2(0) shouldEqual true
  }
*/
  it should "have a canEqual method" in {
    number.immutable.EquaSet(1).canEqual(3) shouldBe false
    number.immutable.EquaSet(1).canEqual("hi") shouldBe false
    number.immutable.EquaSet(1).canEqual(number.immutable.EquaSet(1)) shouldBe true
    number.immutable.EquaSet(1).canEqual(number.immutable.EquaSet(1, 2, 3)) shouldBe true
    number.immutable.EquaSet(1).canEqual(lower.immutable.EquaSet("hi")) shouldBe false
    val orderingEquality = StringNormalizations.lowerCased.toOrderingEquality
    val equaSets = Collections[String](orderingEquality) // Two different Collections instances
    val sortedCollections = SortedCollections[String](orderingEquality)
    val equaSet = equaSets.immutable.EquaSet("hi", "ho")
    val fastEquaSet = equaSets.immutable.FastEquaSet("Bi", "Bo")
    val sortedEquaSet = sortedCollections.immutable.SortedEquaSet("cI", "cO")
    val treeEquaSet = sortedCollections.immutable.TreeEquaSet("DI", "DO")
    equaSet.canEqual(equaSet) shouldBe true
    equaSet.canEqual(equaSets.immutable.FastEquaSet("Hi", "Ho")) shouldBe true
    equaSets.immutable.FastEquaSet("Hi", "Ho").canEqual(equaSet) shouldBe true
    equaSet.canEqual(fastEquaSet) shouldBe true
    fastEquaSet.canEqual(equaSet) shouldBe true
    equaSet.canEqual(sortedEquaSet) shouldBe true
    sortedEquaSet.canEqual(equaSet) shouldBe true
    equaSet.canEqual(treeEquaSet) shouldBe true
    treeEquaSet.canEqual(equaSet) shouldBe true
    fastEquaSet.canEqual(fastEquaSet) shouldBe true
    fastEquaSet.canEqual(sortedEquaSet) shouldBe true
    sortedEquaSet.canEqual(fastEquaSet) shouldBe true
    fastEquaSet.canEqual(treeEquaSet) shouldBe true
    treeEquaSet.canEqual(fastEquaSet) shouldBe true
    sortedEquaSet.canEqual(sortedEquaSet) shouldBe true
    sortedEquaSet.canEqual(treeEquaSet) shouldBe true
    treeEquaSet.canEqual(sortedEquaSet) shouldBe true
    treeEquaSet.canEqual(treeEquaSet) shouldBe true
  }
  it should "have an into.collect method" is pending

/*
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Boolean = number.immutable.EquaSet(1, 2, 3).compose(_ + 1)
    fn(0) shouldBe true
    fn(1) shouldBe true
    fn(2) shouldBe true
    fn(3) shouldBe false
  }
*/
  it should "have an contains method that does a type check" in {
    val e = number.immutable.EquaSet(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    """e.contains("five")""" shouldNot typeCheck
    // new CheckedEquality {
      val es = lower.immutable.EquaSet("one", "two", "three")
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
      new HashingEquality[Fruit] {
        private val nameEquality = StringNormalizations.lowerCased.toHashingEquality
        def areEqual(a: Fruit, b: Any): Boolean =
          b match {
            case bFruit: Fruit => nameEquality.areEqual(a.name, bFruit.name)
            case _ => false
          }
        def hashCodeFor(a: Fruit): Int = nameEquality.hashCodeFor(a.name)
      }
    val fruitCollections = Collections(equalityOfFruit)
    val fruits = fruitCollections.immutable.EquaSet(mac, navel)
    fruits.contains(mac) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val seq = number.immutable.EquaSet(1, 2, 3, 4, 5).toEquaBoxSet.toSeq

    val arr1 = Array.fill(5)(number.EquaBox(-1))
    number.immutable.EquaSet(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(seq(0), seq(1), seq(2), seq(3), seq(4))

    val arr2 = Array.fill(5)(number.EquaBox(-1))
    number.immutable.EquaSet(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(number.EquaBox(-1), seq(0), seq(1), seq(2), seq(3))

    val arr3 = Array.fill(5)(number.EquaBox(-1))
    number.immutable.EquaSet(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(number.EquaBox(-1), seq(0), seq(1), number.EquaBox(-1), number.EquaBox(-1))
  }
  it should "have a copyToBuffer method" in {
    val seq = number.immutable.EquaSet(1, 2, 3, 4, 5).toEquaBoxSet.toSeq
    val buf = ListBuffer.fill(3)(number.EquaBox(-1))
    number.immutable.EquaSet(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(number.EquaBox(-1), number.EquaBox(-1), number.EquaBox(-1), seq(0), seq(1), seq(2), seq(3), seq(4))
  }
  it should "have a count method" in {
    val set = number.immutable.EquaSet(1, 2, 3, 4, 5)
    set.count(_ > 10) shouldBe 0
    set.count(_ % 2 == 0) shouldBe 2
    set.count(_ % 2 == 1) shouldBe 3
  }
  it should "have a drop method" in {
    val set = number.immutable.EquaSet(1, 2, 3, 4, 5)
    val fastSet = number.immutable.FastEquaSet(1, 2, 3, 4, 5)
    val seq = number.immutable.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = set.drop(0)
    result1 shouldBe number.immutable.EquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = set.drop(1)
    result2 shouldBe number.immutable.EquaSet(seq(1), seq(2), seq(3), seq(4))
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = set.drop(2)
    result3 shouldBe number.immutable.EquaSet(seq(2), seq(3), seq(4))
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = set.drop(3)
    result4 shouldBe number.immutable.EquaSet(seq(3), seq(4))
    result4.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result5 = set.drop(4)
    result5 shouldBe number.immutable.EquaSet(seq(4))
    result5.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result6 = set.drop(5)
    result6 shouldBe number.immutable.EquaSet()
    result6.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result7 = fastSet.drop(0)
    result7 shouldBe number.immutable.FastEquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result7.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result8 = fastSet.drop(1)
    result8 shouldBe number.immutable.FastEquaSet(seq(1), seq(2), seq(3), seq(4))
    result8.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result9 = fastSet.drop(2)
    result9 shouldBe number.immutable.FastEquaSet(seq(2), seq(3), seq(4))
    result9.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result10 = fastSet.drop(3)
    result10 shouldBe number.immutable.FastEquaSet(seq(3), seq(4))
    result10.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result11 = fastSet.drop(4)
    result11 shouldBe number.immutable.FastEquaSet(seq(4))
    result11.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result12 = fastSet.drop(5)
    result12 shouldBe number.immutable.FastEquaSet()
    result12.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have a dropRight method" in {
    val set = number.immutable.EquaSet(1, 2, 3, 4, 5)
    val fastSet = number.immutable.FastEquaSet(1, 2, 3, 4, 5)
    val seq = number.immutable.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = set.dropRight(0)
    result1 shouldBe number.immutable.EquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = set.dropRight(1)
    result2 shouldBe number.immutable.EquaSet(seq(0), seq(1), seq(2), seq(3))
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = set.dropRight(2)
    result3 shouldBe number.immutable.EquaSet(seq(0), seq(1), seq(2))
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = set.dropRight(3)
    result4 shouldBe number.immutable.EquaSet(seq(0), seq(1))
    result4.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result5 = set.dropRight(4)
    result5 shouldBe number.immutable.EquaSet(seq(0))
    result5.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result6 = set.dropRight(5)
    result6 shouldBe number.immutable.EquaSet()
    result6.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result7 = fastSet.dropRight(0)
    result7 shouldBe number.immutable.FastEquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result7.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result8 = fastSet.dropRight(1)
    result8 shouldBe number.immutable.FastEquaSet(seq(0), seq(1), seq(2), seq(3))
    result8.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result9 = fastSet.dropRight(2)
    result9 shouldBe number.immutable.FastEquaSet(seq(0), seq(1), seq(2))
    result9.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result10 = fastSet.dropRight(3)
    result10 shouldBe number.immutable.FastEquaSet(seq(0), seq(1))
    result10.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result11 = fastSet.dropRight(4)
    result11 shouldBe number.immutable.FastEquaSet(seq(0))
    result11.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result12 = fastSet.dropRight(5)
    result12 shouldBe number.immutable.FastEquaSet()
    result12.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have a dropWhile method" in {
    val set = number.immutable.EquaSet(1, 2, 3, 4, 5)
    val fastSet = number.immutable.FastEquaSet(1, 2, 3, 4, 5)
    val seq = number.immutable.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = set.dropWhile(_ < 1)
    result1 shouldBe number.immutable.EquaSet(seq.dropWhile(_ < 1): _*)
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = set.dropWhile(_ < 2)
    result2 shouldBe number.immutable.EquaSet(seq.dropWhile(_ < 2): _*)
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = set.dropWhile(_ < 3)
    result3 shouldBe number.immutable.EquaSet(seq.dropWhile(_ < 3): _*)
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = set.dropWhile(_ < 4)
    result4 shouldBe number.immutable.EquaSet(seq.dropWhile(_ < 4): _*)
    result4.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result5 = set.dropWhile(_ < 5)
    result5 shouldBe number.immutable.EquaSet(seq.dropWhile(_ < 5): _*)
    result5.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result6 = set.dropWhile(_ < 6)
    result6 shouldBe number.immutable.EquaSet()
    result6.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result7 = fastSet.dropWhile(_ < 1)
    result7 shouldBe number.immutable.FastEquaSet(seq.dropWhile(_ < 1): _*)
    result7.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result8 = fastSet.dropWhile(_ < 2)
    result8 shouldBe number.immutable.FastEquaSet(seq.dropWhile(_ < 2): _*)
    result8.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result9 = fastSet.dropWhile(_ < 3)
    result9 shouldBe number.immutable.FastEquaSet(seq.dropWhile(_ < 3): _*)
    result9.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result10 = fastSet.dropWhile(_ < 4)
    result10 shouldBe number.immutable.FastEquaSet(seq.dropWhile(_ < 4): _*)
    result10.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result11 = fastSet.dropWhile(_ < 5)
    result11 shouldBe number.immutable.FastEquaSet(seq.dropWhile(_ < 5): _*)
    result11.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result12 = fastSet.dropWhile(_ < 6)
    result12 shouldBe number.immutable.FastEquaSet()
    result12.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have an path method" in {
    lower.immutable.EquaSet("hi").path shouldBe lower
    lower.immutable.FastEquaSet("hi").path shouldBe lower
  }
  it should "have an equals method" in {
    sortedLower.immutable.SortedEquaSet("one", "two", "three") shouldEqual sortedLower.immutable.EquaSet("Three", "Two", "One")
    sortedLower.immutable.EquaSet("one", "two", "three") shouldEqual sortedLower.immutable.SortedEquaSet("Three", "Two", "One")
    val orderingEquality = StringNormalizations.lowerCased.toOrderingEquality
    val equaSets = Collections[String](orderingEquality) // Two different Collections instances
    val sortedCollections = SortedCollections[String](orderingEquality)
    val equaSet = equaSets.immutable.EquaSet("hi", "ho")
    val fastEquaSet = equaSets.immutable.FastEquaSet("Hi", "Ho")
    val sortedEquaSet = sortedCollections.immutable.SortedEquaSet("hI", "hO")
    val treeEquaSet = sortedCollections.immutable.TreeEquaSet("HI", "HO")
    equaSet shouldEqual equaSet
    equaSet shouldEqual equaSets.immutable.FastEquaSet("Hi", "Ho")
    equaSets.immutable.FastEquaSet("Hi", "Ho") shouldEqual equaSet
    equaSet shouldEqual fastEquaSet
    fastEquaSet shouldEqual equaSet
    equaSet shouldEqual sortedEquaSet
    sortedEquaSet shouldEqual equaSet
    equaSet shouldEqual treeEquaSet
    treeEquaSet shouldEqual equaSet
    fastEquaSet shouldEqual fastEquaSet
    fastEquaSet shouldEqual sortedEquaSet
    sortedEquaSet shouldEqual fastEquaSet
    fastEquaSet shouldEqual treeEquaSet
    treeEquaSet shouldEqual fastEquaSet
    sortedEquaSet shouldEqual sortedEquaSet
    sortedEquaSet shouldEqual treeEquaSet
    treeEquaSet shouldEqual sortedEquaSet
    treeEquaSet shouldEqual treeEquaSet
  }
  it should "have an exists method" in {
    number.immutable.EquaSet(1, 2, 3).exists(_ == 2) shouldBe true
    number.immutable.EquaSet(1, 2, 3).exists(_ == 5) shouldBe false
  }
  it should "have a filter method" in {
    val set = number.immutable.EquaSet(1, 2, 3)
    val fastSet = number.immutable.FastEquaSet(1, 2, 3)

    val result1 = set.filter(_ == 1)
    result1 shouldBe number.immutable.EquaSet(1)
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = set.filter(_ == 2)
    result2 shouldBe number.immutable.EquaSet(2)
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = set.filter(_ == 3)
    result3 shouldBe number.immutable.EquaSet(3)
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = fastSet.filter(_ == 1)
    result4 shouldBe number.immutable.FastEquaSet(1)
    result4.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result5 = fastSet.filter(_ == 2)
    result5 shouldBe number.immutable.FastEquaSet(2)
    result5.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result6 = fastSet.filter(_ == 3)
    result6 shouldBe number.immutable.FastEquaSet(3)
    result6.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have a filterNot method" in {
    val set = number.immutable.EquaSet(1, 2, 3)
    val fastSet = number.immutable.FastEquaSet(1, 2, 3)

    val result1 = set.filterNot(_ == 1)
    result1 shouldBe number.immutable.EquaSet(2, 3)
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = set.filterNot(_ == 2)
    result2 shouldBe number.immutable.EquaSet(1, 3)
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = set.filterNot(_ == 3)
    result3 shouldBe number.immutable.EquaSet(1, 2)
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = fastSet.filterNot(_ == 1)
    result4 shouldBe number.immutable.FastEquaSet(2, 3)
    result4.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result5 = fastSet.filterNot(_ == 2)
    result5 shouldBe number.immutable.FastEquaSet(1, 3)
    result5.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result6 = fastSet.filterNot(_ == 3)
    result6 shouldBe number.immutable.FastEquaSet(1, 2)
    result6.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have a find method" in {
    number.immutable.EquaSet(1, 2, 3).find(_ == 5) shouldBe None
    number.immutable.EquaSet(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have an into.flatMap method" is pending

  it should "have a flatMap method" is pending

  it should "have an into.flatten method that works on nested EquaSet" is pending
/* I don't want these anymore
  it can "be flattened when in a GenTraversableOnce" in {
    // need to keep this commented out until finish implementing all methods
    Vector(number.immutable.EquaSet(1, 2, 3), number.immutable.EquaSet(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    List(number.immutable.EquaSet(1, 2, 3), number.immutable.EquaSet(1, 2, 3)).flatten shouldBe List(1, 2, 3, 1, 2, 3)
    // TODO: this is not working 2.10, we may want to enable this back when we understand better how flatten is supported by the implicit in 2.10
    //List(number.immutable.EquaSet(1, 2, 3), number.immutable.EquaSet(1, 2, 3)).toIterator.flatten.toStream shouldBe List(1, 2, 3, 1, 2, 3).toIterator.toStream
    List(number.immutable.EquaSet(1, 2, 3), number.immutable.EquaSet(1, 2, 3)).par.flatten shouldBe List(1, 2, 3, 1, 2, 3).par
  }
  it should "have a flatten method that works on nested GenTraversable" in {
    numberList.immutable.EquaSet(List(1, 2), List(3)).flatten shouldBe List(1, 2, 3)
    numberList.immutable.EquaSet(List(1)).flatten shouldBe List(1)
  }
*/
  it should "have a fold method" in {
    number.immutable.EquaSet(1).fold(0)(_ + _) shouldBe 1
    number.immutable.EquaSet(1).fold(1)(_ * _) shouldBe 1
    number.immutable.EquaSet(2).fold(0)(_ + _) shouldBe 2
    number.immutable.EquaSet(2).fold(1)(_ * _) shouldBe 2
    number.immutable.EquaSet(3).fold(0)(_ + _) shouldBe 3
    number.immutable.EquaSet(3).fold(1)(_ * _) shouldBe 3
    number.immutable.EquaSet(1, 2, 3).fold(0)(_ + _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3).fold(1)(_ * _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    number.immutable.EquaSet(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    number.immutable.EquaSet(1).foldLeft(0)(_ + _) shouldBe 1
    number.immutable.EquaSet(1).foldLeft(1)(_ + _) shouldBe 2
    number.immutable.EquaSet(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    number.immutable.EquaSet(1).foldRight(0)(_ + _) shouldBe 1
    number.immutable.EquaSet(1).foldRight(1)(_ + _) shouldBe 2
    number.immutable.EquaSet(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    number.immutable.EquaSet(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    number.immutable.EquaSet(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- number.immutable.EquaSet(1, 2, 3))
      num += i
    num shouldBe 12
    number.immutable.EquaSet(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3, 4, 5).groupBy(_ % 2)
    result1 shouldBe Map(1 -> number.immutable.EquaSet(1, 3, 5), 0 -> number.immutable.EquaSet(2, 4))
    result1.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.EquaSet[Int]]]

    val result2 = number.immutable.EquaSet(1, 2, 3, 3, 3).groupBy(_ % 2)
    result2 shouldBe Map(1 -> number.immutable.EquaSet(1, 3, 3, 3), 0 -> number.immutable.EquaSet(2))
    result2.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.EquaSet[Int]]]

    val result3 = number.immutable.EquaSet(1, 1, 3, 3, 3).groupBy(_ % 2)
    result3 shouldBe Map(1 -> number.immutable.EquaSet(1, 1, 3, 3, 3))
    result3.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.EquaSet[Int]]]

    val result4 = number.immutable.EquaSet(1, 2, 3, 5, 7).groupBy(_ % 2)
    result4 shouldBe Map(1 -> number.immutable.EquaSet(1, 3, 5, 7), 0 -> number.immutable.EquaSet(2))
    result4.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.EquaSet[Int]]]

    val result5 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).groupBy(_ % 2)
    result5 shouldBe Map(1 -> number.immutable.FastEquaSet(1, 3, 5), 0 -> number.immutable.FastEquaSet(2, 4))
    result5.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.FastEquaSet[Int]]]

    val result6 = number.immutable.FastEquaSet(1, 2, 3, 3, 3).groupBy(_ % 2)
    result6 shouldBe Map(1 -> number.immutable.FastEquaSet(1, 3, 3, 3), 0 -> number.immutable.FastEquaSet(2))
    result6.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.FastEquaSet[Int]]]

    val result7 = number.immutable.FastEquaSet(1, 1, 3, 3, 3).groupBy(_ % 2)
    result7 shouldBe Map(1 -> number.immutable.FastEquaSet(1, 1, 3, 3, 3))
    result7.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.FastEquaSet[Int]]]

    val result8 = number.immutable.FastEquaSet(1, 2, 3, 5, 7).groupBy(_ % 2)
    result8 shouldBe Map(1 -> number.immutable.FastEquaSet(1, 3, 5, 7), 0 -> number.immutable.FastEquaSet(2))
    result8.shouldHaveExactType[scala.collection.GenMap[Int, number.immutable.FastEquaSet[Int]]]
  }
  it should "have a grouped method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).grouped(2).toList
    result1 shouldBe List(number.immutable.EquaSet(1, 2), number.immutable.EquaSet(3))
    result1.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result2 = number.immutable.EquaSet(1, 2, 3).grouped(1).toList
    result2 shouldBe List(number.immutable.EquaSet(1), number.immutable.EquaSet(2), number.immutable.EquaSet(3))
    result2.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    an [IllegalArgumentException] should be thrownBy { number.immutable.EquaSet(1, 2, 3).grouped(0).toList }

    val set = number.immutable.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val fastSet = number.immutable.FastEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val seq = set.toSet.toSeq

    val result3 = set.grouped(2).toList
    result3 shouldBe List(number.immutable.EquaSet(seq(0), seq(1)), number.immutable.EquaSet(seq(2), seq(3)), number.immutable.EquaSet(seq(4), seq(5)), number.immutable.EquaSet(seq(6), seq(7)), number.immutable.EquaSet(seq(8), seq(9)))
    result3.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result4 = set.grouped(3).toList
    result4 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2)), number.immutable.EquaSet(seq(3), seq(4), seq(5)), number.immutable.EquaSet(seq(6), seq(7), seq(8)), number.immutable.EquaSet(seq(9)))
    result4.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result5 = set.grouped(4).toList
    result5 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2), seq(3)), number.immutable.EquaSet(seq(4), seq(5), seq(6), seq(7)), number.immutable.EquaSet(seq(8), seq(9)))
    result5.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result6 = number.immutable.EquaSet(1).grouped(2).toList
    result6 shouldBe List(number.immutable.EquaSet(1))
    result6.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result7 = number.immutable.EquaSet(1).grouped(1).toList
    result7 shouldBe List(number.immutable.EquaSet(1))
    result7.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result8 = number.immutable.FastEquaSet(1, 2, 3).grouped(2).toList
    result8 shouldBe List(number.immutable.FastEquaSet(1, 2), number.immutable.FastEquaSet(3))
    result8.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result9 = number.immutable.FastEquaSet(1, 2, 3).grouped(1).toList
    result9 shouldBe List(number.immutable.FastEquaSet(1), number.immutable.EquaSet(2), number.immutable.FastEquaSet(3))
    result9.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result10 = fastSet.grouped(2).toList
    result10 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1)), number.immutable.EquaSet(seq(2), seq(3)), number.immutable.FastEquaSet(seq(4), seq(5)), number.immutable.EquaSet(seq(6), seq(7)), number.immutable.FastEquaSet(seq(8), seq(9)))
    result10.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result11 = fastSet.grouped(3).toList
    result11 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2)), number.immutable.FastEquaSet(seq(3), seq(4), seq(5)), number.immutable.FastEquaSet(seq(6), seq(7), seq(8)), number.immutable.FastEquaSet(seq(9)))
    result11.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result12 = fastSet.grouped(4).toList
    result12 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2), seq(3)), number.immutable.FastEquaSet(seq(4), seq(5), seq(6), seq(7)), number.immutable.FastEquaSet(seq(8), seq(9)))
    result12.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result13 = number.immutable.FastEquaSet(1).grouped(2).toList
    result13 shouldBe List(number.immutable.FastEquaSet(1))
    result13.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result14 = number.immutable.FastEquaSet(1).grouped(1).toList
    result14 shouldBe List(number.immutable.FastEquaSet(1))
    result14.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]
  }
  it should "have a hasDefiniteSize method" in {
    number.immutable.EquaSet(1).hasDefiniteSize shouldBe true
    number.immutable.EquaSet(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a head method" in {
    lower.immutable.EquaSet("hi").head shouldBe "hi"
    number.immutable.EquaSet(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    lower.immutable.EquaSet("hi").headOption shouldBe Some("hi")
    number.immutable.EquaSet(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have an init method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).init
    result1 shouldBe number.immutable.EquaSet(1, 2)
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = number.immutable.FastEquaSet(1, 2, 3).init
    result2 shouldBe number.immutable.FastEquaSet(1, 2)
    result2.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have an inits method" in {
    val inits = number.immutable.EquaSet(1, 2, 3).inits
    val result1 = inits.next
    result1 shouldBe number.immutable.EquaSet(1,2,3)
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = inits.next
    result2 shouldBe number.immutable.EquaSet(1,2)
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = inits.next
    result3 shouldBe number.immutable.EquaSet(1)
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = inits.next
    result4 shouldBe number.immutable.EquaSet()
    result4.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result5 = inits.hasNext
    result5 shouldBe false

    val fastInits = number.immutable.FastEquaSet(1, 2, 3).inits
    val result6 = fastInits.next
    result6 shouldBe number.immutable.FastEquaSet(1,2,3)
    result6.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result7 = fastInits.next
    result7 shouldBe number.immutable.FastEquaSet(1,2)
    result7.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result8 = fastInits.next
    result8 shouldBe number.immutable.FastEquaSet(1)
    result8.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result9 = fastInits.next
    result9 shouldBe number.immutable.FastEquaSet()
    result9.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result10 = fastInits.hasNext
    result10 shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    lower.immutable.EquaSet("hi").isTraversableAgain shouldBe true
    number.immutable.EquaSet(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have a last method" in {
    lower.immutable.EquaSet("hi").last shouldBe "hi"
    number.immutable.EquaSet(1, 2, 3).last shouldBe 3
  }
  it should "have an lastOption method" in {
    lower.immutable.EquaSet("hi").lastOption shouldBe Some("hi")
    number.immutable.EquaSet(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an into.map method" is pending
  it should "have a map method" is pending
  it should "have a max method" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).max shouldBe 5
    number.immutable.EquaSet(1).max shouldBe 1
    number.immutable.EquaSet(-1).max shouldBe -1
    lower.immutable.EquaSet("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    number.immutable.EquaSet(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).min shouldBe 1
    number.immutable.EquaSet(1).min shouldBe 1
    number.immutable.EquaSet(-1).min shouldBe -1
    lower.immutable.EquaSet("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    number.immutable.EquaSet(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a 3 mkString method" in {

    lower.immutable.EquaSet("hi").mkString shouldBe "hi"
    number.immutable.EquaSet(1, 2, 3).mkString shouldBe "123"

    lower.immutable.EquaSet("hi").mkString("#") shouldBe "hi"
    number.immutable.EquaSet(1, 2, 3).mkString("#") shouldBe "1#2#3"
    number.immutable.EquaSet(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    lower.immutable.EquaSet("hi").mkString("<", "#", ">") shouldBe "<hi>"
    number.immutable.EquaSet(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    number.immutable.EquaSet(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    lower.immutable.EquaSet("hi").nonEmpty shouldBe true
    number.immutable.EquaSet(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have a partition method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3, 4).partition(_ < 3)
    result1 shouldBe (number.immutable.EquaSet(1, 2), number.immutable.EquaSet(3, 4))
    result1.shouldHaveExactType[(number.immutable.EquaSet[Int], number.immutable.EquaSet[Int])]

    val result2 = number.immutable.FastEquaSet(1, 2, 3, 4).partition(_ < 3)
    result2 shouldBe (number.immutable.FastEquaSet(1, 2), number.immutable.FastEquaSet(3, 4))
    result2.shouldHaveExactType[(number.immutable.FastEquaSet[Int], number.immutable.FastEquaSet[Int])]
  }
  it should "have a product method" in {
    number.immutable.EquaSet(1, 2, 3).product shouldBe 6
    number.immutable.EquaSet(3).product shouldBe 3
    number.immutable.EquaSet(3, 4, 5).product shouldBe 60
    number.immutable.EquaSet(3, 4, 5).product shouldBe 60
  }
  it should "have a reduce method" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    number.immutable.EquaSet(5).reduce(_ + _) shouldBe 5
    number.immutable.EquaSet(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    number.immutable.EquaSet(1).reduceLeft(_ + _) shouldBe 1
    number.immutable.EquaSet(1).reduceLeft(_ * _) shouldBe 1
    number.immutable.EquaSet(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    number.immutable.EquaSet(1).reduceLeftOption(_ + _) shouldBe Some(1)
    number.immutable.EquaSet(1).reduceLeftOption(_ * _) shouldBe Some(1)
    number.immutable.EquaSet(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    number.immutable.EquaSet(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    number.immutable.EquaSet(5).reduceOption(_ + _) shouldBe Some(5)
    number.immutable.EquaSet(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    number.immutable.EquaSet(1).reduceRight(_ * _) shouldBe 1
    number.immutable.EquaSet(1, 2, 3).reduceRight(_ + _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3).reduceRight(_ * _) shouldBe 6
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    number.immutable.EquaSet(1).reduceRightOption(_ + _) shouldBe Some(1)
    number.immutable.EquaSet(1).reduceRightOption(_ * _) shouldBe Some(1)
    number.immutable.EquaSet(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    number.immutable.EquaSet(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    number.immutable.EquaSet(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a sameElements method that takes a GenIterable" in {
    number.immutable.EquaSet(1, 2, 3, 4, 5).sameElements(number.immutable.EquaSet(1, 2, 3, 4, 5).toSet.toSeq) shouldBe true
    number.immutable.EquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4)) shouldBe false
    number.immutable.EquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5, 6)) shouldBe false
    number.immutable.EquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 4)) shouldBe false
    number.immutable.EquaSet(3).sameElements(List(1, 2, 3, 4, 5)) shouldBe false
    number.immutable.EquaSet(3).sameElements(List(1)) shouldBe false
    number.immutable.EquaSet(3).sameElements(List(3)) shouldBe true
  }
  it should "have an into.scanLeft method" is pending
  it should "have an into.scanRight method" is pending
  it should "have a slice method" in {
    val result1 = number.immutable.EquaSet(3).slice(0, 0)
    result1 shouldBe number.immutable.EquaSet()
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = number.immutable.EquaSet(1, 2, 3).slice(2, 1)
    result2 shouldBe number.immutable.EquaSet()
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = number.immutable.EquaSet(1, 2, 3).slice(1, 3)
    result3 shouldBe number.immutable.EquaSet(2, 3)
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = number.immutable.FastEquaSet(3).slice(0, 0)
    result4 shouldBe number.immutable.FastEquaSet()
    result4.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result5 = number.immutable.FastEquaSet(1, 2, 3).slice(2, 1)
    result5 shouldBe number.immutable.FastEquaSet()
    result5.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result6 = number.immutable.FastEquaSet(1, 2, 3).slice(1, 3)
    result6 shouldBe number.immutable.FastEquaSet(2, 3)
    result6.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have 2 sliding methods" in {

    val seq = number.immutable.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = number.immutable.EquaSet(1).sliding(1).toList
    result1 shouldBe List(number.immutable.EquaSet(1))
    result1.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result2 = number.immutable.EquaSet(1).sliding(2).toList
    result2 shouldBe List(number.immutable.EquaSet(1))
    result2.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result3 = number.immutable.EquaSet(1, 2, 3).sliding(2).toList
    result3 shouldBe List(number.immutable.EquaSet(1, 2), number.immutable.EquaSet(2, 3))
    result3.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result4 = number.immutable.EquaSet(1, 2, 3).sliding(1).toList
    result4 shouldBe List(number.immutable.EquaSet(1), number.immutable.EquaSet(2), number.immutable.EquaSet(3))
    result4.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result5 = number.immutable.EquaSet(1, 2, 3).sliding(3).toList
    result5 shouldBe List(number.immutable.EquaSet(1, 2, 3))
    result5.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result6 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(3).toList
    result6 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2)), number.immutable.EquaSet(seq(1), seq(2), seq(3)), number.immutable.EquaSet(seq(2), seq(3), seq(4)))
    result6.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result7 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(2).toList
    result7 shouldBe List(number.immutable.EquaSet(seq(0), seq(1)), number.immutable.EquaSet(seq(1), seq(2)), number.immutable.EquaSet(seq(2), seq(3)), number.immutable.EquaSet(seq(3), seq(4)))
    result7.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result8 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(1).toList
    result8 shouldBe List(number.immutable.EquaSet(seq(0)), number.immutable.EquaSet(seq(1)), number.immutable.EquaSet(seq(2)), number.immutable.EquaSet(seq(3)), number.immutable.EquaSet(seq(4)))
    result8.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result9 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(4).toList
    result9 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2), seq(3)), number.immutable.EquaSet(seq(1), seq(2), seq(3), seq(4)))
    result9.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result10 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(5).toList
    result10 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2), seq(3), seq(4)))
    result10.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result11 = number.immutable.EquaSet(1).sliding(1, 1).toList
    result11 shouldBe List(number.immutable.EquaSet(1))
    result11.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result12 = number.immutable.EquaSet(1).sliding(1, 2).toList
    result12 shouldBe List(number.immutable.EquaSet(1))
    result12.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result13 = number.immutable.EquaSet(1, 2, 3).sliding(1, 1).toList
    result13 shouldBe List(number.immutable.EquaSet(1), number.immutable.EquaSet(2), number.immutable.EquaSet(3))
    result13.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result14 = number.immutable.EquaSet(1, 2, 3).sliding(2, 1).toList
    result14 shouldBe List(number.immutable.EquaSet(1, 2), number.immutable.EquaSet(2, 3))
    result14.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result15 = number.immutable.EquaSet(1, 2, 3).sliding(2, 2).toList
    result15 shouldBe List(number.immutable.EquaSet(1, 2), number.immutable.EquaSet(3))
    result15.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result16 = number.immutable.EquaSet(1, 2, 3).sliding(3, 2).toList
    result16 shouldBe List(number.immutable.EquaSet(1, 2, 3))
    result16.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result17 = number.immutable.EquaSet(1, 2, 3).sliding(3, 1).toList
    result17 shouldBe List(number.immutable.EquaSet(1, 2, 3))
    result17.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result18 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result18 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2)), number.immutable.EquaSet(seq(1), seq(2), seq(3)), number.immutable.EquaSet(seq(2), seq(3), seq(4)))
    result18.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result19 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(2, 2).toList
    result19 shouldBe List(number.immutable.EquaSet(seq(0), seq(1)), number.immutable.EquaSet(seq(2), seq(3)), number.immutable.EquaSet(seq(4)))
    result19.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result20 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(2, 3).toList
    result20 shouldBe List(number.immutable.EquaSet(seq(0), seq(1)), number.immutable.EquaSet(seq(3), seq(4)))
    result20.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result21 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(2, 4).toList
    result21 shouldBe List(number.immutable.EquaSet(seq(0), seq(1)), number.immutable.EquaSet(seq(4)))
    result21.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result22 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result22 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2)), number.immutable.EquaSet(seq(1), seq(2), seq(3)), number.immutable.EquaSet(seq(2), seq(3), seq(4)))
    result22.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result23 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(3, 2).toList
    result23 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2)), number.immutable.EquaSet(seq(2), seq(3), seq(4)))
    result23.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result24 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(3, 3).toList
    result24 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2)), number.immutable.EquaSet(seq(3), seq(4)))
    result24.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result25 = number.immutable.EquaSet(1, 2, 3, 4, 5).sliding(3, 4).toList
    result25 shouldBe List(number.immutable.EquaSet(seq(0), seq(1), seq(2)), number.immutable.EquaSet(seq(4)))
    result25.shouldHaveExactType[List[number.immutable.EquaSet[Int]]]

    val result26 = number.immutable.FastEquaSet(1).sliding(1).toList
    result26 shouldBe List(number.immutable.FastEquaSet(1))
    result26.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result27 = number.immutable.FastEquaSet(1).sliding(2).toList
    result27 shouldBe List(number.immutable.FastEquaSet(1))
    result27.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result28 = number.immutable.FastEquaSet(1, 2, 3).sliding(2).toList
    result28 shouldBe List(number.immutable.FastEquaSet(1, 2), number.immutable.FastEquaSet(2, 3))
    result28.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result29 = number.immutable.FastEquaSet(1, 2, 3).sliding(1).toList
    result29 shouldBe List(number.immutable.FastEquaSet(1), number.immutable.FastEquaSet(2), number.immutable.FastEquaSet(3))
    result29.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result30 = number.immutable.FastEquaSet(1, 2, 3).sliding(3).toList
    result30 shouldBe List(number.immutable.FastEquaSet(1, 2, 3))
    result30.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result31 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(3).toList
    result31 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2)), number.immutable.FastEquaSet(seq(1), seq(2), seq(3)), number.immutable.FastEquaSet(seq(2), seq(3), seq(4)))
    result31.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result32 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(2).toList
    result32 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1)), number.immutable.FastEquaSet(seq(1), seq(2)), number.immutable.FastEquaSet(seq(2), seq(3)), number.immutable.FastEquaSet(seq(3), seq(4)))
    result32.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result33 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(1).toList
    result33 shouldBe List(number.immutable.FastEquaSet(seq(0)), number.immutable.FastEquaSet(seq(1)), number.immutable.FastEquaSet(seq(2)), number.immutable.FastEquaSet(seq(3)), number.immutable.FastEquaSet(seq(4)))
    result33.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result34 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(4).toList
    result34 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2), seq(3)), number.immutable.FastEquaSet(seq(1), seq(2), seq(3), seq(4)))
    result34.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result35 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(5).toList
    result35 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2), seq(3), seq(4)))
    result35.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result36 = number.immutable.FastEquaSet(1).sliding(1, 1).toList
    result36 shouldBe List(number.immutable.FastEquaSet(1))
    result36.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result37 = number.immutable.FastEquaSet(1).sliding(1, 2).toList
    result37 shouldBe List(number.immutable.FastEquaSet(1))
    result37.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result38 = number.immutable.FastEquaSet(1, 2, 3).sliding(1, 1).toList
    result38 shouldBe List(number.immutable.FastEquaSet(1), number.immutable.FastEquaSet(2), number.immutable.FastEquaSet(3))
    result38.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result39 = number.immutable.FastEquaSet(1, 2, 3).sliding(2, 1).toList
    result39 shouldBe List(number.immutable.FastEquaSet(1, 2), number.immutable.FastEquaSet(2, 3))
    result39.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result40 = number.immutable.FastEquaSet(1, 2, 3).sliding(2, 2).toList
    result40 shouldBe List(number.immutable.FastEquaSet(1, 2), number.immutable.FastEquaSet(3))
    result40.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result41 = number.immutable.FastEquaSet(1, 2, 3).sliding(3, 2).toList
    result41 shouldBe List(number.immutable.FastEquaSet(1, 2, 3))
    result41.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result42 = number.immutable.FastEquaSet(1, 2, 3).sliding(3, 1).toList
    result42 shouldBe List(number.immutable.FastEquaSet(1, 2, 3))
    result42.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result43 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result43 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2)), number.immutable.FastEquaSet(seq(1), seq(2), seq(3)), number.immutable.FastEquaSet(seq(2), seq(3), seq(4)))
    result43.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result44 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(2, 2).toList
    result44 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1)), number.immutable.FastEquaSet(seq(2), seq(3)), number.immutable.FastEquaSet(seq(4)))
    result44.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result45 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(2, 3).toList
    result45 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1)), number.immutable.FastEquaSet(seq(3), seq(4)))
    result45.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result46 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(2, 4).toList
    result46 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1)), number.immutable.FastEquaSet(seq(4)))
    result46.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result47 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result47 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2)), number.immutable.FastEquaSet(seq(1), seq(2), seq(3)), number.immutable.FastEquaSet(seq(2), seq(3), seq(4)))
    result47.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result48 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 2).toList
    result48 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2)), number.immutable.FastEquaSet(seq(2), seq(3), seq(4)))
    result48.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result49 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 3).toList
    result49 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2)), number.immutable.FastEquaSet(seq(3), seq(4)))
    result49.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]

    val result50 = number.immutable.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 4).toList
    result50 shouldBe List(number.immutable.FastEquaSet(seq(0), seq(1), seq(2)), number.immutable.FastEquaSet(seq(4)))
    result50.shouldHaveExactType[List[number.immutable.FastEquaSet[Int]]]
  }
  it should "have a span method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).span(_ < 3)
    result1 shouldBe (number.immutable.EquaSet(1, 2), number.immutable.EquaSet(3))
    result1.shouldHaveExactType[(number.immutable.EquaSet[Int], number.immutable.EquaSet[Int])]

    val result2 = number.immutable.EquaSet(1, 2, 3).span(_ > 3)
    result2 shouldBe (number.immutable.EquaSet(), number.immutable.EquaSet(1, 2, 3))
    result2.shouldHaveExactType[(number.immutable.EquaSet[Int], number.immutable.EquaSet[Int])]

    val result3 = number.immutable.FastEquaSet(1, 2, 3).span(_ < 3)
    result3 shouldBe (number.immutable.FastEquaSet(1, 2), number.immutable.FastEquaSet(3))
    result3.shouldHaveExactType[(number.immutable.FastEquaSet[Int], number.immutable.FastEquaSet[Int])]

    val result4 = number.immutable.FastEquaSet(1, 2, 3).span(_ > 3)
    result4 shouldBe (number.immutable.FastEquaSet(), number.immutable.FastEquaSet(1, 2, 3))
    result4.shouldHaveExactType[(number.immutable.FastEquaSet[Int], number.immutable.FastEquaSet[Int])]
  }
  it should "have a splitAt method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).splitAt(0)
    result1 shouldBe (number.immutable.EquaSet(), number.immutable.EquaSet(1, 2, 3))
    result1.shouldHaveExactType[(number.immutable.EquaSet[Int], number.immutable.EquaSet[Int])]

    val result2 = number.immutable.EquaSet(1, 2, 3).splitAt(1)
    result2 shouldBe (number.immutable.EquaSet(1), number.immutable.EquaSet(2, 3))
    result2.shouldHaveExactType[(number.immutable.EquaSet[Int], number.immutable.EquaSet[Int])]

    val result3 = number.immutable.EquaSet(1, 2, 3).splitAt(2)
    result3 shouldBe (number.immutable.EquaSet(1, 2), number.immutable.EquaSet(3))
    result3.shouldHaveExactType[(number.immutable.EquaSet[Int], number.immutable.EquaSet[Int])]

    val result4 = number.immutable.EquaSet(1, 2, 3).splitAt(3)
    result4 shouldBe (number.immutable.EquaSet(1, 2, 3), number.immutable.EquaSet())
    result4.shouldHaveExactType[(number.immutable.EquaSet[Int], number.immutable.EquaSet[Int])]

    val result5 = number.immutable.FastEquaSet(1, 2, 3).splitAt(0)
    result5 shouldBe (number.immutable.FastEquaSet(), number.immutable.FastEquaSet(1, 2, 3))
    result5.shouldHaveExactType[(number.immutable.FastEquaSet[Int], number.immutable.FastEquaSet[Int])]

    val result6 = number.immutable.FastEquaSet(1, 2, 3).splitAt(1)
    result6 shouldBe (number.immutable.FastEquaSet(1), number.immutable.FastEquaSet(2, 3))
    result6.shouldHaveExactType[(number.immutable.FastEquaSet[Int], number.immutable.FastEquaSet[Int])]

    val result7 = number.immutable.FastEquaSet(1, 2, 3).splitAt(2)
    result7 shouldBe (number.immutable.FastEquaSet(1, 2), number.immutable.FastEquaSet(3))
    result7.shouldHaveExactType[(number.immutable.FastEquaSet[Int], number.immutable.FastEquaSet[Int])]

    val result8 = number.immutable.FastEquaSet(1, 2, 3).splitAt(3)
    result8 shouldBe (number.immutable.FastEquaSet(1, 2, 3), number.immutable.FastEquaSet())
    result8.shouldHaveExactType[(number.immutable.FastEquaSet[Int], number.immutable.FastEquaSet[Int])]
  }
  it should "have a stringPrefix method" in {
    number.immutable.EquaSet(1).stringPrefix shouldBe "EquaSet"
    number.immutable.EquaSet(1, 2, 3).stringPrefix shouldBe "EquaSet"
    lower.immutable.EquaSet("1").stringPrefix shouldBe "EquaSet"
    lower.immutable.EquaSet("1", "2", "3").stringPrefix shouldBe "EquaSet"
  }
  it should "have a subsetOf method" in {
    number.immutable.EquaSet(2, 3).subsetOf(number.immutable.EquaSet(1, 2, 3, 4, 5)) shouldBe true
    number.immutable.EquaSet(2).subsetOf(number.immutable.EquaSet(1, 2, 3, 4, 5)) shouldBe true
    number.immutable.EquaSet(2, 0).subsetOf(number.immutable.EquaSet(1, 2, 3, 4, 5)) shouldBe false
    lower.immutable.EquaSet("aa", "bb").subsetOf(lower.immutable.EquaSet("aa", "bb", "cc")) shouldBe true
    lower.immutable.EquaSet("aA", "Bb").subsetOf(lower.immutable.EquaSet("aa", "bb", "cc")) shouldBe true
    lower.immutable.EquaSet("aa", "bb").subsetOf(lower.immutable.EquaSet("aA", "Bb", "cC")) shouldBe true
    lower.immutable.EquaSet("aa", "bc").subsetOf(lower.immutable.EquaSet("aa", "bb", "cc")) shouldBe false
  }
  it should "have a 2 subsets method" in {
    val subsets = number.immutable.EquaSet(1, 2, 3).subsets.toList
    subsets should have length 8
    subsets should contain (number.immutable.EquaSet())
    subsets should contain (number.immutable.EquaSet(1))
    subsets should contain (number.immutable.EquaSet(2))
    subsets should contain (number.immutable.EquaSet(3))
    subsets should contain (number.immutable.EquaSet(1, 2))
    subsets should contain (number.immutable.EquaSet(1, 3))
    subsets should contain (number.immutable.EquaSet(2, 3))
    subsets should contain (number.immutable.EquaSet(1, 2, 3))

    val subsets2 = number.immutable.EquaSet(1, 2, 3).subsets(2).toList
    subsets2 should have length 3
    subsets2 should contain (number.immutable.EquaSet(1, 2))
    subsets2 should contain (number.immutable.EquaSet(1, 3))
    subsets2 should contain (number.immutable.EquaSet(2, 3))

    number.immutable.EquaSet(1, 2, 3).subsets.shouldHaveExactType[Iterator[number.immutable.EquaSet[Int]]]
    number.immutable.EquaSet(1, 2, 3).subsets(2).shouldHaveExactType[Iterator[number.immutable.EquaSet[Int]]]

    number.immutable.FastEquaSet(1, 2, 3).subsets.shouldHaveExactType[Iterator[number.immutable.FastEquaSet[Int]]]
    number.immutable.FastEquaSet(1, 2, 3).subsets(2).shouldHaveExactType[Iterator[number.immutable.FastEquaSet[Int]]]
  }
  it should "have a sum method" in {
    number.immutable.EquaSet(1).sum shouldBe 1
    number.immutable.EquaSet(5).sum shouldBe 5
    number.immutable.EquaSet(1, 2, 3).sum shouldBe 6
    number.immutable.EquaSet(1, 2, 3, 4, 5).sum shouldBe 15
  }
  it should "have an tail method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).tail
    result1 shouldBe number.immutable.EquaSet(2, 3)
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = number.immutable.FastEquaSet(1, 2, 3).tail
    result2 shouldBe number.immutable.FastEquaSet(2, 3)
    result2.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have an tails method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).tails
    result1.toList shouldBe List(number.immutable.EquaSet(1,2,3), number.immutable.EquaSet(2,3), number.immutable.EquaSet(3), number.immutable.EquaSet())
    result1.shouldHaveExactType[Iterator[number.immutable.EquaSet[Int]]]

    val result2 = number.immutable.FastEquaSet(1, 2, 3).tails
    result2.toList shouldBe List(number.immutable.FastEquaSet(1,2,3), number.immutable.FastEquaSet(2,3), number.immutable.FastEquaSet(3), number.immutable.FastEquaSet())
    result2.shouldHaveExactType[Iterator[number.immutable.FastEquaSet[Int]]]
  }
  it should "have a take method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).take(0)
    result1 shouldBe number.immutable.EquaSet()
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = number.immutable.EquaSet(1, 2, 3).take(1)
    result2 shouldBe number.immutable.EquaSet(1)
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = number.immutable.EquaSet(1, 2, 3).take(2)
    result3 shouldBe number.immutable.EquaSet(1, 2)
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = number.immutable.EquaSet(1, 2, 3).take(3)
    result4 shouldBe number.immutable.EquaSet(1, 2, 3)
    result4.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result5 = number.immutable.FastEquaSet(1, 2, 3).take(0)
    result5 shouldBe number.immutable.FastEquaSet()
    result5.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result6 = number.immutable.FastEquaSet(1, 2, 3).take(1)
    result6 shouldBe number.immutable.FastEquaSet(1)
    result6.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result7 = number.immutable.FastEquaSet(1, 2, 3).take(2)
    result7 shouldBe number.immutable.FastEquaSet(1, 2)
    result7.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result8 = number.immutable.FastEquaSet(1, 2, 3).take(3)
    result8 shouldBe number.immutable.FastEquaSet(1, 2, 3)
    result8.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have a takeRight method" in {
    val result1 = number.immutable.EquaSet(1, 2, 3).takeRight(0)
    result1 shouldBe number.immutable.EquaSet()
    result1.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result2 = number.immutable.EquaSet(1, 2, 3).takeRight(1)
    result2 shouldBe number.immutable.EquaSet(3)
    result2.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result3 = number.immutable.EquaSet(1, 2, 3).takeRight(2)
    result3 shouldBe number.immutable.EquaSet(2, 3)
    result3.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result4 = number.immutable.EquaSet(1, 2, 3).takeRight(3)
    result4 shouldBe number.immutable.EquaSet(1, 2, 3)
    result4.shouldHaveExactType[number.immutable.EquaSet[Int]]

    val result5 = number.immutable.FastEquaSet(1, 2, 3).takeRight(0)
    result5 shouldBe number.immutable.FastEquaSet()
    result5.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result6 = number.immutable.FastEquaSet(1, 2, 3).takeRight(1)
    result6 shouldBe number.immutable.FastEquaSet(3)
    result6.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result7 = number.immutable.FastEquaSet(1, 2, 3).takeRight(2)
    result7 shouldBe number.immutable.FastEquaSet(2, 3)
    result7.shouldHaveExactType[number.immutable.FastEquaSet[Int]]

    val result8 = number.immutable.FastEquaSet(1, 2, 3).takeRight(3)
    result8 shouldBe number.immutable.FastEquaSet(1, 2, 3)
    result8.shouldHaveExactType[number.immutable.FastEquaSet[Int]]
  }
  it should "have a toArray method" in {
    number.immutable.EquaSet(1, 2, 3).toArray shouldBe (Array(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toArray shouldBe (Array("a", "b"))
    number.immutable.EquaSet(1).toArray shouldBe (Array(1))
  }
  it should "have a toEquaBoxArray method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxArray shouldBe (Array(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxArray shouldBe (Array(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxArray shouldBe (Array(number.EquaBox(1)))
  }
  it should "have a toBuffer method" in {
    number.immutable.EquaSet(1, 2, 3).toBuffer shouldBe (Buffer(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toBuffer shouldBe (Buffer("a", "b"))
    number.immutable.EquaSet(1).toBuffer shouldBe (Buffer(1))
  }
  it should "have a toEquaBoxBuffer method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxBuffer shouldBe (Buffer(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxBuffer shouldBe (Buffer(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxBuffer shouldBe (Buffer(number.EquaBox(1)))
  }
  it should "have a toIndexedSeq method" in {
    number.immutable.EquaSet(1, 2, 3).toIndexedSeq shouldBe (IndexedSeq(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toIndexedSeq shouldBe (IndexedSeq("a", "b"))
    number.immutable.EquaSet(1).toIndexedSeq shouldBe (IndexedSeq(1))
  }
  it should "have a toEquaBoxIndexedSeq method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxIndexedSeq shouldBe (IndexedSeq(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxIndexedSeq shouldBe (IndexedSeq(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxIndexedSeq shouldBe (IndexedSeq(number.EquaBox(1)))
  }
  it should "have a toIterable method" in {
    number.immutable.EquaSet(1, 2, 3).toIterable shouldBe (Set(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toIterable shouldBe (Set("a", "b"))
    number.immutable.EquaSet(1).toIterable shouldBe (Set(1))
  }
  it should "have a toEquaBoxIterable method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxIterable shouldBe (Set(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxIterable shouldBe (Set(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxIterable shouldBe (Set(number.EquaBox(1)))
  }
  it should "have a toIterator method" in {
    number.immutable.EquaSet(1, 2, 3).toIterator.toList shouldBe (Iterator(1, 2, 3).toList)
    lower.immutable.EquaSet("a", "b").toIterator.toList shouldBe (Iterator("a", "b").toList)
    number.immutable.EquaSet(1).toIterator.toList shouldBe (Iterator(1).toList)
    number.immutable.EquaSet(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    lower.immutable.EquaSet("a", "b").toIterator shouldBe an [Iterator[_]]
    number.immutable.EquaSet(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toEquaBoxIterator method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxIterator.toList shouldBe (Iterator(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)).toList)
    lower.immutable.EquaSet("a", "b").toEquaBoxIterator.toList shouldBe (Iterator(lower.EquaBox("a"), lower.EquaBox("b")).toList)
    number.immutable.EquaSet(1).toEquaBoxIterator.toList shouldBe (Iterator(number.EquaBox(1)).toList)
    number.immutable.EquaSet(1, 2, 3).toEquaBoxIterator shouldBe an [Iterator[_]]
    lower.immutable.EquaSet("a", "b").toEquaBoxIterator shouldBe an [Iterator[_]]
    number.immutable.EquaSet(1).toEquaBoxIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    number.immutable.EquaSet(1, 2, 3).toList shouldBe (List(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toList shouldBe (List("a", "b"))
    number.immutable.EquaSet(1).toList shouldBe (List(1))
  }
  it should "have a toEquaBoxList method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxList shouldBe (List(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxList shouldBe (List(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxList shouldBe (List(number.EquaBox(1)))
  }
  it should "have a toMap method" in {
    tuple.immutable.EquaSet((1, "one"), (2, "two"), (3, "three")).toMap shouldBe Map(1 -> "one", 2 -> "two", 3 -> "three")
  }
  it should "have a toParArray method" in {
    number.immutable.EquaSet(1, 2, 3).toParArray shouldBe ParArray(1, 2, 3)
  }
  it should "have a toEquaBoxParArray method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxParArray shouldBe ParArray(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a toSeq method" in {
    number.immutable.EquaSet(1, 2, 3).toSeq shouldBe (Seq(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toSeq shouldBe (Seq("a", "b"))
    number.immutable.EquaSet(1).toSeq shouldBe (Seq(1))
  }
  it should "have a toEquaBoxSeq method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxSeq shouldBe (Seq(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxSeq shouldBe (Seq(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxSeq shouldBe (Seq(number.EquaBox(1)))
  }
  it should "have a toStream method" in {
    number.immutable.EquaSet(1, 2, 3).toStream shouldBe (Stream(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toStream shouldBe (Stream("a", "b"))
    number.immutable.EquaSet(1).toStream shouldBe(Stream(1))
  }
  it should "have a toEquaBoxStream method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxStream shouldBe (Stream(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxStream shouldBe (Stream(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxStream shouldBe(Stream(number.EquaBox(1)))
  }
  it should "have a toEquaBoxTraversable method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxTraversable shouldBe (Set(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxTraversable shouldBe (Set(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxTraversable shouldBe (Set(number.EquaBox(1)))
  }
  it should "have a toTraversable method" in {
    number.immutable.EquaSet(1, 2, 3).toTraversable shouldBe (Set(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toTraversable shouldBe (Set("a", "b"))
    number.immutable.EquaSet(1).toTraversable shouldBe (Set(1))
  }
  it should "have a toEquaBoxVector method" in {
    number.immutable.EquaSet(1, 2, 3).toEquaBoxVector should === (Vector(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.immutable.EquaSet("a", "b").toEquaBoxVector should === (Vector(lower.EquaBox("a"), lower.EquaBox("b")))
    number.immutable.EquaSet(1).toEquaBoxVector should === (Vector(number.EquaBox(1)))
  }
  it should "have a toVector method" in {
    number.immutable.EquaSet(1, 2, 3).toVector should === (Vector(1, 2, 3))
    lower.immutable.EquaSet("a", "b").toVector should === (Vector("a", "b"))
    number.immutable.EquaSet(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    val result1 = numberList.immutable.EquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose
    result1 shouldBe numberList.immutable.EquaSet(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    result1.shouldHaveExactType[numberList.immutable.EquaSet[List[Int]]]

    val result2 = numberList.immutable.EquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose
    result2 shouldBe numberList.immutable.EquaSet(List(1, 3, 5, 7), List(2, 4, 6, 8))
    result2.shouldHaveExactType[numberList.immutable.EquaSet[List[Int]]]

    val result3 = numberList.immutable.EquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose.transpose
    result3 shouldBe numberList.immutable.EquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8))
    result3.shouldHaveExactType[numberList.immutable.EquaSet[List[Int]]]

    val result4 = numberList.immutable.EquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose.transpose
    result4 shouldBe numberList.immutable.EquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    result4.shouldHaveExactType[numberList.immutable.EquaSet[List[Int]]]

    val result5 = numberList.immutable.FastEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose
    result5 shouldBe numberList.immutable.FastEquaSet(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    result5.shouldHaveExactType[numberList.immutable.FastEquaSet[List[Int]]]

    val result6 = numberList.immutable.FastEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose
    result6 shouldBe numberList.immutable.FastEquaSet(List(1, 3, 5, 7), List(2, 4, 6, 8))
    result6.shouldHaveExactType[numberList.immutable.FastEquaSet[List[Int]]]

    val result7 = numberList.immutable.FastEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose.transpose
    result7 shouldBe numberList.immutable.FastEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8))
    result7.shouldHaveExactType[numberList.immutable.FastEquaSet[List[Int]]]

    val result8 = numberList.immutable.FastEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose.transpose
    result8 shouldBe numberList.immutable.FastEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    result8.shouldHaveExactType[numberList.immutable.FastEquaSet[List[Int]]]
  }
  it should "have a withFilter method" in {
    var a = 0
    var b = 0
    val set = number.immutable.EquaSet(1, 2, 3)
    val withFilter = set.withFilter { e =>
      a += 1
      e > 1
    }
    a shouldBe 0
    withFilter.foreach { e =>
      b += e
    }
    a shouldBe 3  // increase when doing the filter of the elements
    b shouldBe 5

    var c = 0
    withFilter.withFilter(_ > 2).foreach { e =>
      c += e
    }
    a shouldBe 6  // 3 + 3
    c shouldBe 3

/*
    val result1 = withFilter.map(_ * 2)
    result1 shouldBe number.immutable.EquaSet(4, 6)
    a shouldBe 9

    val result2 = withFilter.flatMap(e => number.immutable.EquaSet(e * 2, e * 3))
    result2 shouldBe number.immutable.EquaSet(4, 6, 9)
    a shouldBe 12
*/
  }

  it should "not have a zip method" in {
    """number.immutable.EquaSet(1, 2, 3).zip(List("4", "5", "6"))""" shouldNot typeCheck
    """number.immutable.EquaSet(1, 2, 3).zip(List("4", "5"))""" shouldNot typeCheck

    number.immutable.EquaSet(1, 2, 3).toSet.zip(List("4", "5", "6")) should contain theSameElementsAs Set((1, "4"), (2, "5"), (3, "6"))
    number.immutable.EquaSet(1, 2, 3).toSet.zip(List("4", "5")) should contain theSameElementsAs Set((1, "4"), (2, "5"))
  }
  it should "not have a zipAll method" in {
    """number.immutable.EquaSet(1, 2, 3).zipAll(List("4", "5", "6"), 0, "0")""" shouldNot typeCheck
    """number.immutable.EquaSet(1, 2, 3).zipAll(List("4", "5"), 0, "0")""" shouldNot typeCheck
    """number.immutable.EquaSet(1, 2).zipAll(List("4", "5", "6"), 0, "0")""" shouldNot typeCheck

    number.immutable.EquaSet(1, 2, 3).toSet.zipAll(List("4", "5", "6"), 0, "0") should contain theSameElementsAs Set((1, "4"), (2, "5"), (3, "6"))
    number.immutable.EquaSet(1, 2, 3).toSet.zipAll(List("4", "5"), 0, "0") should contain theSameElementsAs Set((1, "4"), (2, "5"), (3, "0"))
    number.immutable.EquaSet(1, 2).toSet.zipAll(List("4", "5", "6"), 0, "0") should contain theSameElementsAs Set((1, "4"), (2, "5"), (0, "6"))
  }
  it should "not have a zipWithIndex method" in {
    """number.immutable.EquaSet(99).zipWithIndex""" shouldNot typeCheck
    """number.immutable.EquaSet(1, 2, 3).zipWithIndex""" shouldNot typeCheck

    number.immutable.EquaSet(99).toSet.zipWithIndex should contain theSameElementsAs Set((99,0))
    number.immutable.EquaSet(1, 2, 3).toSet.zipWithIndex should contain theSameElementsAs Set((1,0), (2,1), (3,2))
  }

  it should "have a filter method after it is converted into EquaBridge with into" is pending

  it should "have a withFilter method after it is converted into EquaBridge with into" is pending

  it should "return an equal set if toEquaSet is called after view" in {
    val actual = number.immutable.EquaSet(1, 2, 3).view.toEquaSet(number)
    actual should equal (number.immutable.EquaSet(1, 2, 3))
  }

/*
abstract def contains(elem: A): Boolean
abstract def iterator: Iterator[A] 
def &(that: GenSet[A]): Set[A]
def &~(that: GenSet[A]): Set[A]
def ++(elems: GenTraversableOnce[A]): Set[A]
def ++[B](that: GenTraversableOnce[B]): Set[B]
def ++:[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[Set[A], B, That]): That
def ++:[B](that: TraversableOnce[B]): Set[B]
def -(elem1: A, elem2: A, elems: A*): Set[A]
def --(xs: GenTraversableOnce[A]): Set[A]
def /:[B](z: B)(op: (B, A) ⇒ B): B
def :\[B](z: B)(op: (A, B) ⇒ B): B
def addString(b: StringBuilder): StringBuilder
def addString(b: StringBuilder, sep: String): StringBuilder
def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder
def aggregate[B](z: ⇒ B)(seqop: (B, A) ⇒ B, combop: (B, B) ⇒ B): B
def andThen[A](g: (Boolean) ⇒ A): (A) ⇒ A
def apply(elem: A): Boolean
def canEqual(that: Any): Boolean
def collect[B](pf: PartialFunction[A, B]): Set[B]
def collectFirst[B](pf: PartialFunction[A, B]): Option[B]
def companion: GenericCompanion[Set]
def compose[A](g: (A) ⇒ A): (A) ⇒ Boolean
def copyToArray(xs: Array[A], start: Int, len: Int): Unit
def copyToArray(xs: Array[A]): Unit
def copyToArray(xs: Array[A], start: Int): Unit
def copyToBuffer[B >: A](dest: Buffer[B]): Unit
def count(p: (A) ⇒ Boolean): Int
def diff(that: GenSet[A]): Set[A]
def drop(n: Int): Set[A]
def dropRight(n: Int): Set[A]
def dropWhile(p: (A) ⇒ Boolean): Set[A]
def empty: Set[A]
def equals(that: Any): Boolean
def exists(p: (A) ⇒ Boolean): Boolean
def filter(p: (A) ⇒ Boolean): Set[A]
def filterNot(p: (A) ⇒ Boolean): Set[A]
def find(p: (A) ⇒ Boolean): Option[A]
def flatMap[B](f: (A) ⇒ GenTraversableOnce[B]): Set[B]
def flatten[B]: Set[B]
def fold[A1 >: A](z: A1)(op: (A1, A1) ⇒ A1): A1
def foldLeft[B](z: B)(op: (B, A) ⇒ B): B
def foldRight[B](z: B)(op: (A, B) ⇒ B): B
def forall(p: (A) ⇒ Boolean): Boolean
def foreach(f: (A) ⇒ Unit): Unit
def genericBuilder[B]: Builder[B, Set[B]]
def groupBy[K](f: (A) ⇒ K): immutable.Map[K, Set[A]]
def grouped(size: Int): Iterator[Set[A]]
def hasDefiniteSize: Boolean
def hashCode(): Int
def head: A
def headOption: Option[A]
def init: Set[A]
def inits: Iterator[Set[A]]
def intersect(that: GenSet[A]): Set[A]
def isEmpty: Boolean
final def isTraversableAgain: Boolean
def last: A
def lastOption: Option[A]
def map[B](f: (A) ⇒ B): Set[B]
def max: A
def maxBy[B](f: (A) ⇒ B): A
def min: A
def minBy[B](f: (A) ⇒ B): A
def mkString: String
def mkString(sep: String): String
def mkString(start: String, sep: String, end: String): String
def nonEmpty: Boolean
def par: ParSet[A]
def partition(p: (A) ⇒ Boolean): (Set[A], Set[A])
def product: A
def reduce[A1 >: A](op: (A1, A1) ⇒ A1): A1
def reduceLeft[B >: A](op: (B, A) ⇒ B): B
def reduceLeftOption[B >: A](op: (B, A) ⇒ B): Option[B]
def reduceOption[A1 >: A](op: (A1, A1) ⇒ A1): Option[A1]
def reduceRight[B >: A](op: (A, B) ⇒ B): B
def reduceRightOption[B >: A](op: (A, B) ⇒ B): Option[B]
def repr: Set[A]
def sameElements(that: GenIterable[A]): Boolean
def scan[B >: A, That](z: B)(op: (B, B) ⇒ B)(implicit cbf: CanBuildFrom[Set[A], B, That]): That
def scanLeft[B, That](z: B)(op: (B, A) ⇒ B)(implicit bf: CanBuildFrom[Set[A], B, That]): That
def scanRight[B, That](z: B)(op: (A, B) ⇒ B)(implicit bf: CanBuildFrom[Set[A], B, That]): That
def seq: Set[A]
def size: Int
def slice(from: Int, until: Int): Set[A]
def sliding(size: Int, step: Int): Iterator[Set[A]]
def sliding(size: Int): Iterator[Set[A]]
def span(p: (A) ⇒ Boolean): (Set[A], Set[A])
def splitAt(n: Int): (Set[A], Set[A])
def stringPrefix: String
def subsetOf(that: GenSet[A]): Boolean
def subsets: Iterator[Set[A]]
def subsets(len: Int): Iterator[Set[A]]
def sum: A
def tail: Set[A]
def tails: Iterator[Set[A]]
def take(n: Int): Set[A]
def takeRight(n: Int): Set[A]
def takeWhile(p: (A) ⇒ Boolean): Set[A]
def to[Col[_]]: Col[A]
def toArray: Array[A]
def toBuffer[A1 >: A]: Buffer[A1]
def toIndexedSeq: immutable.IndexedSeq[A]
def toIterable: Iterable[A]
def toIterator: Iterator[A]
def toList: List[A]
def toMap[T, U]: Map[T, U]
def toParArray: ParArray[T]
def toSeq: Seq[A]
def toSet[B >: A]: immutable.Set[B]
def toStream: immutable.Stream[A]
def toString(): String
def toTraversable: Traversable[A]
def toVector: Vector[A]
def transpose[B](implicit asTraversable: (A) ⇒ GenTraversableOnce[B]): Set[Set[B]]
def union(that: GenSet[A]): Set[A]
def unzip[A1, A2](implicit asPair: (A) ⇒ (A1, A2)): (Set[A1], Set[A2])
def unzip3[A1, A2, A3](implicit asTriple: (A) ⇒ (A1, A2, A3)): (Set[A1], Set[A2], Set[A3])
def view(from: Int, until: Int): IterableView[A, Set[A]]
def view: IterableView[A, Set[A]]
def withFilter(p: (A) ⇒ Boolean): FilterMonadic[A, Set[A]]
def zip[B](that: GenIterable[B]): Set[(A, B)]
def zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): Set[(A, B)]
def zipWithIndex: Set[(A, Int)]
*/
  "FastEquaSetView" should "offer a lazy map method" in {
    val lazyBag = trimmed.immutable.EquaSet("1", "2", "01", "3").view
    var performed = false
    val toIntFun = (s: String) => { performed = true; s.toInt }
    val mappedFastEquaSetView = lazyBag.map(toIntFun).map(_ + 1)
    performed shouldBe false
    val strictSet = mappedFastEquaSetView.toEquaSet(number)
    performed shouldBe true
    strictSet should equal (number.immutable.EquaSet(2, 3, 4))
  }

  it should "offer an equals method that looks at processed elements" in {
    val trimmedBag = trimmed.immutable.EquaSet("1", "2", "01", "3").view
    val lowerBag = trimmed.immutable.EquaSet("2", "3", "02", "4").view
    val modifiedTrimmedBag = trimmedBag.map(_.toInt).map(_ + 1)
    val modifiedLowerBag = lowerBag.map(_.toInt)
    modifiedTrimmedBag should equal (modifiedLowerBag)
    val thirdModifiedBag = lowerBag.map(_.toInt).map(_ + 2)
    modifiedTrimmedBag should not equal thirdModifiedBag
  }

  it should "offer a hashCode method that looks at processed elements" in {
    val trimmedBag = trimmed.immutable.EquaSet("1", "2", "01", "3").view
    val lowerBag = trimmed.immutable.EquaSet("2", "3", "02", "4").view
    val modifiedTrimmedBag = trimmedBag.map(_.toInt).map(_ + 1)
    val modifiedLowerBag = lowerBag.map(_.toInt)
    modifiedTrimmedBag.hashCode should equal (modifiedLowerBag.hashCode)
    val thirdModifiedBag = lowerBag.map(_.toInt).map(_ + 2)
    modifiedTrimmedBag.hashCode should not equal thirdModifiedBag.hashCode // Overspecified
  }

  it should "offer a flatMap method" in {
    val lazySet = trimmed.immutable.EquaSet("1", "2", "01", "3").view
    val flatMapped = lazySet.flatMap { (digit: String) =>
      FastEquaSetView(digit.toInt)
    }
    val strictSet = flatMapped.toEquaSet(number)
    strictSet should equal (number.immutable.EquaSet(1, 2, 3))
  }
  it should "allow chaining of maps and flatMaps" in {
    val lazySet = trimmed.immutable.EquaSet("1", "2", "01", "3").view
    val flatMapped = lazySet.flatMap { (digit: String) =>
      FastEquaSetView(digit.toInt)
    }
    val mapped = flatMapped.map(_ + 1)
    val strictSet = mapped.toEquaSet(number)
    strictSet should equal (number.immutable.EquaSet(2, 3, 4))
  }
  "The collections value" should "be a nice reference to a default Collections[Any]" in {
    import Collections.native._
    immutable.EquaSet(1, 2, 3) intersect immutable.EquaSet(2, 3, 4) shouldEqual immutable.EquaSet(2, 3)
    immutable.FastEquaSet(1, 2, 3) intersect immutable.FastEquaSet(2, 3, 4) shouldEqual immutable.FastEquaSet(2, 3)
    EquaSet(1, 2, 3) intersect EquaSet(2, 3, 4) shouldEqual EquaSet(2, 3)
    FastEquaSet(1, 2, 3) intersect FastEquaSet(2, 3, 4) shouldEqual FastEquaSet(2, 3)
  }
}

