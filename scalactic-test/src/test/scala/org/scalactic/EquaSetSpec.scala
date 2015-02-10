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
val t = EquaPath[String](StringNormalizations.trimmed.toHashingEquality)
val w = SortedEquaPath[String](StringNormalizations.lowerCased.toOrderingEquality)
val tes = t.EquaSet("tes")
val tfes = t.FastEquaSet("tfes")
val wes = w.EquaSet("les")
val wfes = w.FastEquaSet("lfes")
val wses = w.SortedEquaSet("lses")
val wtes = w.TreeEquaSet("ltes")

EquaBridge[F, EquaPath#EquaSet, EquaPath] // EquaPath#EquaSet                      thatEquaPath.EquaSetBridge
EquaBridge[F, EquaPath#EquaSet, SortedEquaPath] // SortedEquaPath#EquaSet          thatEquaPath.EquaSetBridge

EquaBridge[F, EquaPath#FastEquaSet, EquaPath] // EquaPath#FastEquaSet              thatEquaPath.FastEquaSetBridge
EquaBridge[F, EquaPath#FastEquaSet, SortedEquaPath] // SortedEquaPath#FastEquaSet  thatEquaPath.FastEquaSetBridge

EquaBridge[F, SortedEquaPath#EquaSet, EquaPath] // EquaPath#EquaSet                thatEquaPath.EquaSetBridge
EquaBridge[F, SortedEquaPath#EquaSet, SortedEquaPath] // SortedEquaPath#EquaSet    thatEquaPath.EquaSetBridge

EquaBridge[F, SortedEquaPath#FastEquaSet, EquaPath] // EquaPath#FastEquaSet               thatEquaPath.FastEquaSetBridge
EquaBridge[F, SortedEquaPath#FastEquaSet, SortedEquaPath] // SortedEquaPath#FastEquaSet   thatEquaPath.FastEquaSetBridge

EquaBridge[F, SortedEquaPath#SortedEquaSet, EquaPath] // EquaPath#EquaSet                    thatEquaPath.EquaSetBridge
EquaBridge[F, SortedEquaPath#SortedEquaSet, SortedEquaPath] // SortedEquaPath#SortedEquaSet  thatEquaPath.SortedEquaSetBridge

EquaBridge[F, SortedEquaPath#TreeEquaSet, EquaPath] // EquaPath#EquaSet                      thatEquaPath.EquaSetBridge
EquaBridge[F, SortedEquaPath#TreeEquaSet, SortedEquaPath] // SortedEquaPath#TreeEquaSet      thatEquaPath.TreeEquaSetBridge

// def into[U, EQUASETS[u] <: EquaPath[u]](thatEquaPath: EQUASETS[U]): thatEquaPath.EquaBridgeResult[T]
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
  val number = EquaPath[Int](normalHashingEquality[Int])
  val sortedNumber = SortedEquaPath[Int](normalOrderingEquality[Int])
  val lower = EquaPath[String](StringNormalizations.lowerCased.toHashingEquality)
  val trimmed = EquaPath[String](StringNormalizations.trimmed.toHashingEquality)
  val sortedLower = SortedEquaPath[String](StringNormalizations.lowerCased.toOrderingEquality)
  val numberList = EquaPath[List[Int]](normalHashingEquality[List[Int]])
  val numberLower = EquaPath[(Int, String)](normalHashingEquality[(Int, String)])
  val numberLowerTrimmed = EquaPath[(Int, String, String)](normalHashingEquality[(Int, String, String)])
  val numberNumber = EquaPath[number.EquaSet](normalHashingEquality[number.EquaSet])
  def upperCharHashingEquality =
    new HashingEquality[Char] {
      def hashCodeFor(a: Char): Int = a.toUpper.hashCode
      def areEqual(a: Char, b: Any): Boolean =
        b match {
          case bChar: Char => a.toUpper == bChar.toUpper
          case _ => false
        }
    }
  val upperChar = EquaPath[Char](upperCharHashingEquality)
  val regularChar = EquaPath[Char](normalHashingEquality[Char])
  val tuple = EquaPath[(Int, String)](normalHashingEquality)

  "An EquaSet" can "be constructed with empty" in {
    val emptySet = lower.EquaSet.empty
    emptySet shouldBe empty
  }
  it can "be constructed with apply" in {
    val nonEmptySet = lower.EquaSet("one", "two", "three")
    nonEmptySet should have size 3
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for EquaSet.
  }
  it should "construct only sets with appropriate element types" in {
    "lower.EquaSet(1, 2, 3)" shouldNot compile
  }
  it should "eliminate 'duplicate' entries passed to the apply factory method" in {
    val nonEmptySet = lower.EquaSet("one", "two", "two", "three", "Three")
    nonEmptySet should have size 3
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for EquaSet.
  }
  it should "have a toString method" in {
    lower.EquaSet("hi", "ho").toString should === ("EquaSet(hi, ho)")
  }
  it should "have a diff method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.EquaSet("hi", "ho") diff lower.EquaSet("HI", "HO")
    result1 shouldBe lower.EquaSet()
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = trimmed.EquaSet("hi", "ho") diff trimmed.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.EquaSet()
    result2.shouldHaveExactType[trimmed.EquaSet]

    """lower.EquaSet(" hi ", "hi") diff trimmed.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.EquaSet("hi", "ho") diff lower.EquaSet("ho")
    result3 shouldBe lower.EquaSet("hi")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho", "let's", "go") diff lower.EquaSet("bo", "no", "go", "ho")
    result4 shouldBe lower.EquaSet("hi", "let's")
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.FastEquaSet("hi", "ho") diff lower.FastEquaSet("HI", "HO")
    result5 shouldBe lower.FastEquaSet()
    result5.shouldHaveExactType[lower.FastEquaSet]

    val result6 = trimmed.FastEquaSet("hi", "ho") diff trimmed.FastEquaSet(" hi ", " ho ")
    result6 shouldBe trimmed.FastEquaSet()
    result6.shouldHaveExactType[trimmed.FastEquaSet]

    """lower.FastEquaSet(" hi ", "hi") diff trimmed.FastEquaSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.FastEquaSet("hi", "ho") diff lower.FastEquaSet("ho")
    result7 shouldBe lower.FastEquaSet("hi")
    result7.shouldHaveExactType[lower.FastEquaSet]

    val result8 = lower.FastEquaSet("hi", "ho", "let's", "go") diff lower.FastEquaSet("bo", "no", "go", "ho")
    result8 shouldBe lower.FastEquaSet("hi", "let's")
    result8.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a &~ method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.EquaSet("hi", "ho") &~ lower.EquaSet("HI", "HO")
    result1 shouldBe lower.EquaSet()
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = trimmed.EquaSet("hi", "ho") &~ trimmed.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.EquaSet()
    result2.shouldHaveExactType[trimmed.EquaSet]

    """lower.EquaSet(" hi ", "hi") &~ trimmed.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.EquaSet("hi", "ho") &~ lower.EquaSet("ho")
    result3 shouldBe lower.EquaSet("hi")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho", "let's", "go") &~ lower.EquaSet("bo", "no", "go", "ho")
    result4 shouldBe lower.EquaSet("hi", "let's")
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.FastEquaSet("hi", "ho") &~ lower.FastEquaSet("HI", "HO")
    result5 shouldBe lower.FastEquaSet()
    result5.shouldHaveExactType[lower.FastEquaSet]

    val result6 = trimmed.FastEquaSet("hi", "ho") &~ trimmed.FastEquaSet(" hi ", " ho ")
    result6 shouldBe trimmed.FastEquaSet()
    result6.shouldHaveExactType[trimmed.FastEquaSet]

    """lower.FastEquaSet(" hi ", "hi") &~ trimmed.FastEquaSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.FastEquaSet("hi", "ho") &~ lower.FastEquaSet("ho")
    result7 shouldBe lower.FastEquaSet("hi")
    result7.shouldHaveExactType[lower.FastEquaSet]

    val result8 = lower.FastEquaSet("hi", "ho", "let's", "go") &~ lower.FastEquaSet("bo", "no", "go", "ho")
    result8 shouldBe lower.FastEquaSet("hi", "let's")
    result8.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have an intersect method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.EquaSet("hi", "ho") intersect lower.EquaSet("HI", "HO")
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = trimmed.EquaSet("hi", "ho") intersect trimmed.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.EquaSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.EquaSet]

    """lower.EquaSet(" hi ", "hi") intersect trimmed.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.EquaSet("hi", "ho") intersect lower.EquaSet("ho")
    result3 shouldBe lower.EquaSet("ho")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho", "let's", "go") intersect lower.EquaSet("bo", "no", "go", "ho")
    result4 shouldBe lower.EquaSet("ho", "go")
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.FastEquaSet("hi", "ho") intersect lower.FastEquaSet("HI", "HO")
    result5 shouldBe lower.FastEquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.FastEquaSet]

    val result6 = trimmed.FastEquaSet("hi", "ho") intersect trimmed.FastEquaSet(" hi ", " ho ")
    result6 shouldBe trimmed.FastEquaSet("hi", "ho")
    result6.shouldHaveExactType[trimmed.FastEquaSet]

    """lower.FastEquaSet(" hi ", "hi") intersect trimmed.FastEquaSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.FastEquaSet("hi", "ho") intersect lower.FastEquaSet("ho")
    result7 shouldBe lower.FastEquaSet("ho")
    result7.shouldHaveExactType[lower.FastEquaSet]

    val result8 = lower.FastEquaSet("hi", "ho", "let's", "go") intersect lower.FastEquaSet("bo", "no", "go", "ho")
    result8 shouldBe lower.FastEquaSet("ho", "go")
    result8.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have an & method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.EquaSet("hi", "ho") & lower.EquaSet("HI", "HO")
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = trimmed.EquaSet("hi", "ho") & trimmed.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.EquaSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.EquaSet]

    """lower.EquaSet(" hi ", "hi") & trimmed.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.EquaSet("hi", "ho") & lower.EquaSet("ho")
    result3 shouldBe lower.EquaSet("ho")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho", "let's", "go") & lower.EquaSet("bo", "no", "go", "ho")
    result4 shouldBe lower.EquaSet("ho", "go")
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.FastEquaSet("hi", "ho") & lower.FastEquaSet("HI", "HO")
    result5 shouldBe lower.FastEquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.FastEquaSet]

    val result6 = trimmed.FastEquaSet("hi", "ho") & trimmed.FastEquaSet(" hi ", " ho ")
    result6 shouldBe trimmed.FastEquaSet("hi", "ho")
    result6.shouldHaveExactType[trimmed.FastEquaSet]

    """lower.FastEquaSet(" hi ", "hi") & trimmed.FastEquaSet("hi", "HI")""" shouldNot typeCheck

    val result7 = lower.FastEquaSet("hi", "ho") & lower.FastEquaSet("ho")
    result7 shouldBe lower.FastEquaSet("ho")
    result7.shouldHaveExactType[lower.FastEquaSet]

    val result8 = lower.FastEquaSet("hi", "ho", "let's", "go") & lower.FastEquaSet("bo", "no", "go", "ho")
    result8 shouldBe lower.FastEquaSet("ho", "go")
    result8.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a union method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.EquaSet("hi", "ho") union lower.EquaSet("HI", "HO")
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = trimmed.EquaSet("hi", "ho") union trimmed.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.EquaSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.EquaSet]

    """lower.EquaSet(" hi ", "hi") union trimmed.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.FastEquaSet("hi", "ho") union lower.FastEquaSet("HI", "HO")
    result3 shouldBe lower.FastEquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.FastEquaSet]

    val result4 = trimmed.FastEquaSet("hi", "ho") union trimmed.FastEquaSet(" hi ", " ho ")
    result4 shouldBe trimmed.FastEquaSet("hi", "ho")
    result4.shouldHaveExactType[trimmed.FastEquaSet]

    """lower.FastEquaSet(" hi ", "hi") union trimmed.FastEquaSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a | method that takes another EquaSet instance with the same path-dependant type" in {
    val result1 = lower.EquaSet("hi", "ho") | lower.EquaSet("HI", "HO")
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = trimmed.EquaSet("hi", "ho") | trimmed.EquaSet(" hi ", " ho ")
    result2 shouldBe trimmed.EquaSet("hi", "ho")
    result2.shouldHaveExactType[trimmed.EquaSet]

    """lower.EquaSet(" hi ", "hi") | trimmed.EquaSet("hi", "HI")""" shouldNot typeCheck

    val result3 = lower.FastEquaSet("hi", "ho") | lower.FastEquaSet("HI", "HO")
    result3 shouldBe lower.FastEquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.FastEquaSet]

    val result4 = trimmed.FastEquaSet("hi", "ho") | trimmed.FastEquaSet(" hi ", " ho ")
    result4 shouldBe trimmed.FastEquaSet("hi", "ho")
    result4.shouldHaveExactType[trimmed.FastEquaSet]

    """lower.FastEquaSet(" hi ", "hi") | trimmed.FastEquaSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a toSet method" in {
    lower.EquaSet("hi", "ho").toSet should === (Set("hi", "ho"))
  }
  it should "have a toEquaBoxSet method" in {
    lower.EquaSet("hi", "ho").toEquaBoxSet should === (Set(lower.EquaBox("hi"), lower.EquaBox("ho")))
  }
  it should "have a + method that takes one argument" in {
    val result1 = lower.EquaSet("hi", "ho") + "ha"
    result1 shouldBe lower.EquaSet("hi", "ho", "ha")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho") + "HO"
    result2 shouldBe lower.EquaSet("hi", "ho")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.FastEquaSet("hi", "ho") + "ha"
    result3 shouldBe lower.FastEquaSet("hi", "ho", "ha")
    result3.shouldHaveExactType[lower.FastEquaSet]

    val result4 = lower.FastEquaSet("hi", "ho") + "HO"
    result4 shouldBe lower.FastEquaSet("hi", "ho")
    result4.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a + method that takes two or more arguments" in {
    val result1 = lower.EquaSet("hi", "ho") + ("ha", "hey!")
    result1 shouldBe lower.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho") + ("HO", "hoe", "Ho!")
    result2 shouldBe lower.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.FastEquaSet("hi", "ho") + ("ha", "hey!")
    result3 shouldBe lower.FastEquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.FastEquaSet]

    val result4 = lower.FastEquaSet("hi", "ho") + ("HO", "hoe", "Ho!")
    result4 shouldBe lower.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a - method that takes one argument" in {
    val result1 = lower.EquaSet("hi", "ho", "ha") - "ha"
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho") - "HO"
    result2 shouldBe lower.EquaSet("hi")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.EquaSet("hi", "ho") - "who?"
    result3 shouldBe lower.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.FastEquaSet("hi", "ho", "ha") - "ha"
    result4 shouldBe lower.FastEquaSet("hi", "ho")
    result4.shouldHaveExactType[lower.FastEquaSet]

    val result5 = lower.FastEquaSet("hi", "ho") - "HO"
    result5 shouldBe lower.FastEquaSet("hi")
    result5.shouldHaveExactType[lower.FastEquaSet]

    val result6 = lower.FastEquaSet("hi", "ho") - "who?"
    result6 shouldBe lower.FastEquaSet("hi", "ho")
    result6.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a - method that takes two or more arguments" in {
    val result1 = lower.EquaSet("hi", "ho", "ha") - ("ha", "howdy!")
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")
    result2 shouldBe lower.EquaSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.EquaSet("hi", "ho") - ("who", "goes", "thar")
    result3 shouldBe lower.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho") - ("HI", "HO")
    result4 shouldBe lower.EquaSet.empty
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.FastEquaSet("hi", "ho", "ha") - ("ha", "howdy!")
    result5 shouldBe lower.FastEquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.FastEquaSet]

    val result6 = lower.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")
    result6 shouldBe lower.FastEquaSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.FastEquaSet]

    val result7 = lower.FastEquaSet("hi", "ho") - ("who", "goes", "thar")
    result7 shouldBe lower.FastEquaSet("hi", "ho")
    result7.shouldHaveExactType[lower.FastEquaSet]

    val result8 = lower.FastEquaSet("hi", "ho") - ("HI", "HO")
    result8 shouldBe lower.FastEquaSet.empty
    result8.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "return an iterator that returns the set's elements" in {
    lower.EquaSet("hi", "ho", "ha", "he").iterator.toList should contain theSameElementsAs List("ha", "he", "hi", "ho")
  }
  it should "have a ++ method that takes a GenTraversableOnce" in {
    val result1 = lower.EquaSet("hi", "ho") ++ List("ha", "hey!")
    result1 shouldBe lower.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho") ++ List("HO", "hoe", "Ho!")
    result2 shouldBe lower.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.EquaSet("hi", "ho") ++ Set("ha", "hey!")
    result3 shouldBe lower.EquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho") ++ Set("HO", "hoe", "Ho!")
    result4 shouldBe lower.EquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.EquaSet("hi", "ho") ++ Vector("ha", "hey!")
    result5 shouldBe lower.EquaSet("hi", "ho", "ha", "hey!")
    result5.shouldHaveExactType[lower.EquaSet]

    val result6 = lower.EquaSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!")
    result6 shouldBe lower.EquaSet("hi", "ho", "hoe", "Ho!")
    result6.shouldHaveExactType[lower.EquaSet]

    val result7 = lower.FastEquaSet("hi", "ho") ++ List("ha", "hey!")
    result7 shouldBe lower.FastEquaSet("hi", "ho", "ha", "hey!")
    result7.shouldHaveExactType[lower.FastEquaSet]

    val result8 = lower.FastEquaSet("hi", "ho") ++ List("HO", "hoe", "Ho!")
    result8 shouldBe lower.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result8.shouldHaveExactType[lower.FastEquaSet]

    val result9 = lower.FastEquaSet("hi", "ho") ++ Set("ha", "hey!")
    result9 shouldBe lower.FastEquaSet("hi", "ho", "ha", "hey!")
    result9.shouldHaveExactType[lower.FastEquaSet]

    val result10 = lower.FastEquaSet("hi", "ho") ++ Set("HO", "hoe", "Ho!")
    result10 shouldBe lower.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result10.shouldHaveExactType[lower.FastEquaSet]

    val result11 = lower.FastEquaSet("hi", "ho") ++ Vector("ha", "hey!")
    result11 shouldBe lower.FastEquaSet("hi", "ho", "ha", "hey!")
    result11.shouldHaveExactType[lower.FastEquaSet]

    val result12 = lower.FastEquaSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!")
    result12 shouldBe lower.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result12.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a ++ method that takes another EquaSet" in {
    val result1 = lower.EquaSet("hi", "ho") ++ lower.EquaSet("ha", "hey!")
    result1 shouldBe lower.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho") ++ lower.EquaSet("HO", "hoe", "Ho!")
    result2 shouldBe lower.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.FastEquaSet("hi", "ho") ++ lower.FastEquaSet("ha", "hey!")
    result3 shouldBe lower.FastEquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.FastEquaSet]

    val result4 = lower.FastEquaSet("hi", "ho") ++ lower.FastEquaSet("HO", "hoe", "Ho!")
    result4 shouldBe lower.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a -- method that takes a GenTraversableOnce" in {
    val result1 = lower.EquaSet("hi", "ho", "ha") -- List("ha", "howdy!")
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")
    result2 shouldBe lower.EquaSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.EquaSet("hi", "ho") -- List("who", "goes", "thar")
    result3 shouldBe lower.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho") -- List("HI", "HO")
    result4 shouldBe lower.EquaSet.empty
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.EquaSet("hi", "ho", "ha") -- Set("ha", "howdy!")
    result5 shouldBe lower.EquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.EquaSet]

    val result6 = lower.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")
    result6 shouldBe lower.EquaSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.EquaSet]

    val result7 = lower.EquaSet("hi", "ho") -- Set("who", "goes", "thar")
    result7 shouldBe lower.EquaSet("hi", "ho")
    result7.shouldHaveExactType[lower.EquaSet]

    val result8 = lower.EquaSet("hi", "ho") -- Set("HI", "HO")
    result8 shouldBe lower.EquaSet.empty
    result8.shouldHaveExactType[lower.EquaSet]

    val result9 = lower.EquaSet("hi", "ho", "ha") -- Vector("ha", "howdy!")
    result9 shouldBe lower.EquaSet("hi", "ho")
    result9.shouldHaveExactType[lower.EquaSet]

    val result10 = lower.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")
    result10 shouldBe lower.EquaSet("hi", "fee", "foe")
    result10.shouldHaveExactType[lower.EquaSet]

    val result11 = lower.EquaSet("hi", "ho") -- Vector("who", "goes", "thar")
    result11 shouldBe lower.EquaSet("hi", "ho")
    result11.shouldHaveExactType[lower.EquaSet]

    val result12 = lower.EquaSet("hi", "ho") -- Vector("HI", "HO")
    result12 shouldBe lower.EquaSet.empty
    result12.shouldHaveExactType[lower.EquaSet]

    val result13 = lower.FastEquaSet("hi", "ho", "ha") -- List("ha", "howdy!")
    result13 shouldBe lower.FastEquaSet("hi", "ho")
    result13.shouldHaveExactType[lower.FastEquaSet]

    val result14 = lower.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")
    result14 shouldBe lower.FastEquaSet("hi", "fee", "foe")
    result14.shouldHaveExactType[lower.FastEquaSet]

    val result15 = lower.FastEquaSet("hi", "ho") -- List("who", "goes", "thar")
    result15 shouldBe lower.FastEquaSet("hi", "ho")
    result15.shouldHaveExactType[lower.FastEquaSet]

    val result16 = lower.FastEquaSet("hi", "ho") -- List("HI", "HO")
    result16 shouldBe lower.FastEquaSet.empty
    result16.shouldHaveExactType[lower.FastEquaSet]

    val result17 = lower.FastEquaSet("hi", "ho", "ha") -- Set("ha", "howdy!")
    result17 shouldBe lower.FastEquaSet("hi", "ho")
    result17.shouldHaveExactType[lower.FastEquaSet]

    val result18 = lower.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")
    result18 shouldBe lower.FastEquaSet("hi", "fee", "foe")
    result18.shouldHaveExactType[lower.FastEquaSet]

    val result19 = lower.FastEquaSet("hi", "ho") -- Set("who", "goes", "thar")
    result19 shouldBe lower.FastEquaSet("hi", "ho")
    result19.shouldHaveExactType[lower.FastEquaSet]

    val result20 = lower.FastEquaSet("hi", "ho") -- Set("HI", "HO")
    result20 shouldBe lower.FastEquaSet.empty
    result20.shouldHaveExactType[lower.FastEquaSet]

    val result21 = lower.FastEquaSet("hi", "ho", "ha") -- Vector("ha", "howdy!")
    result21 shouldBe lower.FastEquaSet("hi", "ho")
    result21.shouldHaveExactType[lower.FastEquaSet]

    val result22 = lower.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")
    result22 shouldBe lower.FastEquaSet("hi", "fee", "foe")
    result22.shouldHaveExactType[lower.FastEquaSet]

    val result23 = lower.FastEquaSet("hi", "ho") -- Vector("who", "goes", "thar")
    result23 shouldBe lower.FastEquaSet("hi", "ho")
    result23.shouldHaveExactType[lower.FastEquaSet]

    val result24 = lower.FastEquaSet("hi", "ho") -- Vector("HI", "HO")
    result24 shouldBe lower.FastEquaSet.empty
    result24.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a -- method that takes another EquaSet" in {
    val result1 = lower.EquaSet("hi", "ho", "ha") -- lower.EquaSet("ha", "howdy!")
    result1 shouldBe lower.EquaSet("hi", "ho")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.EquaSet("HO", "FIE", "fUm")
    result2 shouldBe lower.EquaSet("hi", "fee", "foe")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.EquaSet("hi", "ho") -- lower.EquaSet("who", "goes", "thar")
    result3 shouldBe lower.EquaSet("hi", "ho")
    result3.shouldHaveExactType[lower.EquaSet]

    val result4 = lower.EquaSet("hi", "ho") -- lower.EquaSet("HI", "HO")
    result4 shouldBe lower.EquaSet.empty
    result4.shouldHaveExactType[lower.EquaSet]

    val result5 = lower.FastEquaSet("hi", "ho", "ha") -- lower.FastEquaSet("ha", "howdy!")
    result5 shouldBe lower.FastEquaSet("hi", "ho")
    result5.shouldHaveExactType[lower.FastEquaSet]

    val result6 = lower.FastEquaSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.FastEquaSet("HO", "FIE", "fUm")
    result6 shouldBe lower.FastEquaSet("hi", "fee", "foe")
    result6.shouldHaveExactType[lower.FastEquaSet]

    val result7 = lower.FastEquaSet("hi", "ho") -- lower.FastEquaSet("who", "goes", "thar")
    result7 shouldBe lower.FastEquaSet("hi", "ho")
    result7.shouldHaveExactType[lower.FastEquaSet]

    val result8 = lower.FastEquaSet("hi", "ho") -- lower.FastEquaSet("HI", "HO")
    result8 shouldBe lower.FastEquaSet.empty
    result8.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a /: method" in {
    (0 /: number.EquaSet(1))(_ + _) shouldBe 1
    (1 /: number.EquaSet(1))(_ + _) shouldBe 2
    (0 /: number.EquaSet(1, 2, 3))(_ + _) shouldBe 6
    (1 /: number.EquaSet(1, 2, 3))(_ + _) shouldBe 7
  }
  it should "have a :\\ method" in {
    (number.EquaSet(1) :\ 0)(_ + _) shouldBe 1
    (number.EquaSet(1) :\ 1)(_ + _) shouldBe 2
    (number.EquaSet(1, 2, 3) :\ 0)(_ + _) shouldBe 6
    (number.EquaSet(1, 2, 3) :\ 1)(_ + _) shouldBe 7
  }
  it should "have 3 addString methods" in {
    lower.EquaSet("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    number.EquaSet(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    lower.EquaSet("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    number.EquaSet(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    number.EquaSet(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    lower.EquaSet("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    number.EquaSet(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    number.EquaSet(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have a aggregate method" in {
    lower.EquaSet("hi", "ho", "ha", "hey!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "ha", "hey!")

    lower.EquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "hoe", "Ho!")

    val result1 = lower.EquaSet("hi", "ho", "ha", "hey!").aggregate(lower.EquaSet())(_ + _, _ ++ _)
    result1 shouldBe lower.EquaSet("hi", "ho", "ha", "hey!")
    result1.shouldHaveExactType[lower.EquaSet]

    val result2 = lower.EquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.EquaSet())(_ + _, _ ++ _)
    result2 shouldBe lower.EquaSet("hi", "ho", "hoe", "Ho!")
    result2.shouldHaveExactType[lower.EquaSet]

    val result3 = lower.FastEquaSet("hi", "ho", "ha", "hey!").aggregate(lower.FastEquaSet())(_ + _, _ ++ _)
    result3 shouldBe lower.FastEquaSet("hi", "ho", "ha", "hey!")
    result3.shouldHaveExactType[lower.FastEquaSet]

    val result4 = lower.FastEquaSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.FastEquaSet())(_ + _, _ ++ _)
    result4 shouldBe lower.FastEquaSet("hi", "ho", "hoe", "Ho!")
    result4.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have an apply method" in {
    val a = number.EquaSet(1, 2, 3)
    a(2) shouldEqual true
    a(5) shouldEqual false

    val b = lower.EquaSet("hi")
    b("hi") shouldEqual true
    b("Hi") shouldEqual true
    b("hI") shouldEqual true
    b("HI") shouldEqual true
    b("he") shouldEqual false
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = number.EquaSet(1) andThen (!_)
    pf1(1) shouldEqual false
    pf1(2) shouldEqual true

    val pf2 = number.EquaSet(1, 2, 3) andThen (!_)
    pf2(1) shouldEqual false
    pf2(2) shouldEqual false
    pf2(3) shouldEqual false
    pf2(0) shouldEqual true
  }
  it should "have a canEqual method" in {
    number.EquaSet(1).canEqual(3) shouldBe false
    number.EquaSet(1).canEqual("hi") shouldBe false
    number.EquaSet(1).canEqual(number.EquaSet(1)) shouldBe true
    number.EquaSet(1).canEqual(number.EquaSet(1, 2, 3)) shouldBe true
    number.EquaSet(1).canEqual(lower.EquaSet("hi")) shouldBe false
    val orderingEquality = StringNormalizations.lowerCased.toOrderingEquality
    val equaSets = EquaPath[String](orderingEquality) // Two different EquaPath instances
    val sortedEquaPath = SortedEquaPath[String](orderingEquality)
    val equaSet = equaSets.EquaSet("hi", "ho")
    val fastEquaSet = equaSets.FastEquaSet("Bi", "Bo")
    val sortedEquaSet = sortedEquaPath.SortedEquaSet("cI", "cO")
    val treeEquaSet = sortedEquaPath.TreeEquaSet("DI", "DO")
    equaSet.canEqual(equaSet) shouldBe true
    equaSet.canEqual(equaSets.FastEquaSet("Hi", "Ho")) shouldBe true
    equaSets.FastEquaSet("Hi", "Ho").canEqual(equaSet) shouldBe true
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
  it should "have an into.collect method" in {
    // Can map into self explicitly too
    number.EquaSet(1, 2, 3).into(number).map(_ + 1) shouldBe number.EquaSet(2, 3, 4)
    number.EquaSet(5).into(number).map(_ + 3) shouldBe number.EquaSet(8)

    // EquaSet into EquaPath => EquaSet
    val result1 = number.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result1 shouldBe lower.EquaSet("4", "8", "12", "16", "20")
    result1.shouldHaveExactType[lower.EquaSet]

    // EquaSet into SortedEquaPath => EquaSet
    val result2 = number.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(sortedLower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result2 shouldBe sortedLower.EquaSet("4", "8", "12", "16", "20")
    result2.shouldHaveExactType[sortedLower.EquaSet]

    // FastEquaSet into EquaPath => FastEquaSet
    val result3 = number.FastEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result3 shouldBe lower.FastEquaSet("4", "8", "12", "16", "20")
    result3.shouldHaveExactType[lower.FastEquaSet]

    // FastEquaSet into SortedEquaPath => FastEquaSet
    val result4 = number.FastEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(sortedLower).collect { case i if i % 2 == 0 => (i * 2).toString }
    result4 shouldBe sortedLower.FastEquaSet("4", "8", "12", "16", "20")
    result4.shouldHaveExactType[sortedLower.FastEquaSet]

    // Extra stuff from oldInto tests
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    val result = number.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower) collect { case i if i % 2 == 0 => (i * 2).toString }
    result shouldBe lower.EquaSet("4", "8", "12", "16", "20")
    // result.shouldHaveExactType[lower.EquaSet]
    number.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower) collect { case i if i > 10 => (i * 2).toString } shouldBe lower.EquaSet.empty
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    val resultB = number.EquaSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).into(sortedLower) collect { case i if i % 2 == 0 => (i * 2).toString }
    resultB shouldBe sortedLower.EquaSet("4", "8", "12", "16", "20")
    // result.shouldHaveExactType[sortedLower.EquaSet]
    number.EquaSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).into(sortedLower) collect { case i if i > 10 => (i * 2).toString } shouldBe sortedLower.EquaSet.empty
  }
  it should "have a collect method that only accepts functions that result in the path-enclosed type" in {
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    val result1 = number.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i % 2 == 0 => i * 2 }
    result1 shouldBe number.EquaSet(4, 8, 12, 16, 20)
    result1.shouldHaveExactType[number.EquaSet]
    number.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i > 10 => i * 2 } shouldBe number.EquaSet.empty
    val result2 = number.FastEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i % 2 == 0 => i * 2 }
    result2 shouldBe number.FastEquaSet(4, 8, 12, 16, 20)
    result2.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Boolean = number.EquaSet(1, 2, 3).compose(_ + 1)
    fn(0) shouldBe true
    fn(1) shouldBe true
    fn(2) shouldBe true
    fn(3) shouldBe false
  }
  it should "have an contains method that does a type check" in {
    val e = number.EquaSet(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    """e.contains("five")""" shouldNot typeCheck
    new CheckedEquality {
      val es = lower.EquaSet("one", "two", "three")
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
      new HashingEquality[Fruit] {
        private val nameEquality = StringNormalizations.lowerCased.toHashingEquality
        def areEqual(a: Fruit, b: Any): Boolean =
          b match {
            case bFruit: Fruit => nameEquality.areEqual(a.name, bFruit.name)
            case _ => false
          }
        def hashCodeFor(a: Fruit): Int = nameEquality.hashCodeFor(a.name)
      }
    val fruitEquaPath = EquaPath(equalityOfFruit)
    val fruits = fruitEquaPath.EquaSet(mac, navel)
    fruits.contains(mac) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val seq = number.EquaSet(1, 2, 3, 4, 5).toEquaBoxSet.toSeq

    val arr1 = Array.fill(5)(number.EquaBox(-1))
    number.EquaSet(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(seq(0), seq(1), seq(2), seq(3), seq(4))

    val arr2 = Array.fill(5)(number.EquaBox(-1))
    number.EquaSet(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(number.EquaBox(-1), seq(0), seq(1), seq(2), seq(3))

    val arr3 = Array.fill(5)(number.EquaBox(-1))
    number.EquaSet(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(number.EquaBox(-1), seq(0), seq(1), number.EquaBox(-1), number.EquaBox(-1))
  }
  it should "have a copyToBuffer method" in {
    val seq = number.EquaSet(1, 2, 3, 4, 5).toEquaBoxSet.toSeq
    val buf = ListBuffer.fill(3)(number.EquaBox(-1))
    number.EquaSet(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(number.EquaBox(-1), number.EquaBox(-1), number.EquaBox(-1), seq(0), seq(1), seq(2), seq(3), seq(4))
  }
  it should "have a count method" in {
    val set = number.EquaSet(1, 2, 3, 4, 5)
    set.count(_ > 10) shouldBe 0
    set.count(_ % 2 == 0) shouldBe 2
    set.count(_ % 2 == 1) shouldBe 3
  }
  it should "have a drop method" in {
    val set = number.EquaSet(1, 2, 3, 4, 5)
    val fastSet = number.FastEquaSet(1, 2, 3, 4, 5)
    val seq = number.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = set.drop(0)
    result1 shouldBe number.EquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = set.drop(1)
    result2 shouldBe number.EquaSet(seq(1), seq(2), seq(3), seq(4))
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = set.drop(2)
    result3 shouldBe number.EquaSet(seq(2), seq(3), seq(4))
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = set.drop(3)
    result4 shouldBe number.EquaSet(seq(3), seq(4))
    result4.shouldHaveExactType[number.EquaSet]

    val result5 = set.drop(4)
    result5 shouldBe number.EquaSet(seq(4))
    result5.shouldHaveExactType[number.EquaSet]

    val result6 = set.drop(5)
    result6 shouldBe number.EquaSet()
    result6.shouldHaveExactType[number.EquaSet]

    val result7 = fastSet.drop(0)
    result7 shouldBe number.FastEquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result7.shouldHaveExactType[number.FastEquaSet]

    val result8 = fastSet.drop(1)
    result8 shouldBe number.FastEquaSet(seq(1), seq(2), seq(3), seq(4))
    result8.shouldHaveExactType[number.FastEquaSet]

    val result9 = fastSet.drop(2)
    result9 shouldBe number.FastEquaSet(seq(2), seq(3), seq(4))
    result9.shouldHaveExactType[number.FastEquaSet]

    val result10 = fastSet.drop(3)
    result10 shouldBe number.FastEquaSet(seq(3), seq(4))
    result10.shouldHaveExactType[number.FastEquaSet]

    val result11 = fastSet.drop(4)
    result11 shouldBe number.FastEquaSet(seq(4))
    result11.shouldHaveExactType[number.FastEquaSet]

    val result12 = fastSet.drop(5)
    result12 shouldBe number.FastEquaSet()
    result12.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a dropRight method" in {
    val set = number.EquaSet(1, 2, 3, 4, 5)
    val fastSet = number.FastEquaSet(1, 2, 3, 4, 5)
    val seq = number.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = set.dropRight(0)
    result1 shouldBe number.EquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = set.dropRight(1)
    result2 shouldBe number.EquaSet(seq(0), seq(1), seq(2), seq(3))
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = set.dropRight(2)
    result3 shouldBe number.EquaSet(seq(0), seq(1), seq(2))
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = set.dropRight(3)
    result4 shouldBe number.EquaSet(seq(0), seq(1))
    result4.shouldHaveExactType[number.EquaSet]

    val result5 = set.dropRight(4)
    result5 shouldBe number.EquaSet(seq(0))
    result5.shouldHaveExactType[number.EquaSet]

    val result6 = set.dropRight(5)
    result6 shouldBe number.EquaSet()
    result6.shouldHaveExactType[number.EquaSet]

    val result7 = fastSet.dropRight(0)
    result7 shouldBe number.FastEquaSet(seq(0), seq(1), seq(2), seq(3), seq(4))
    result7.shouldHaveExactType[number.FastEquaSet]

    val result8 = fastSet.dropRight(1)
    result8 shouldBe number.FastEquaSet(seq(0), seq(1), seq(2), seq(3))
    result8.shouldHaveExactType[number.FastEquaSet]

    val result9 = fastSet.dropRight(2)
    result9 shouldBe number.FastEquaSet(seq(0), seq(1), seq(2))
    result9.shouldHaveExactType[number.FastEquaSet]

    val result10 = fastSet.dropRight(3)
    result10 shouldBe number.FastEquaSet(seq(0), seq(1))
    result10.shouldHaveExactType[number.FastEquaSet]

    val result11 = fastSet.dropRight(4)
    result11 shouldBe number.FastEquaSet(seq(0))
    result11.shouldHaveExactType[number.FastEquaSet]

    val result12 = fastSet.dropRight(5)
    result12 shouldBe number.FastEquaSet()
    result12.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a dropWhile method" in {
    val set = number.EquaSet(1, 2, 3, 4, 5)
    val fastSet = number.FastEquaSet(1, 2, 3, 4, 5)
    val seq = number.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = set.dropWhile(_ < 1)
    result1 shouldBe number.EquaSet(seq.dropWhile(_ < 1): _*)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = set.dropWhile(_ < 2)
    result2 shouldBe number.EquaSet(seq.dropWhile(_ < 2): _*)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = set.dropWhile(_ < 3)
    result3 shouldBe number.EquaSet(seq.dropWhile(_ < 3): _*)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = set.dropWhile(_ < 4)
    result4 shouldBe number.EquaSet(seq.dropWhile(_ < 4): _*)
    result4.shouldHaveExactType[number.EquaSet]

    val result5 = set.dropWhile(_ < 5)
    result5 shouldBe number.EquaSet(seq.dropWhile(_ < 5): _*)
    result5.shouldHaveExactType[number.EquaSet]

    val result6 = set.dropWhile(_ < 6)
    result6 shouldBe number.EquaSet()
    result6.shouldHaveExactType[number.EquaSet]

    val result7 = fastSet.dropWhile(_ < 1)
    result7 shouldBe number.FastEquaSet(seq.dropWhile(_ < 1): _*)
    result7.shouldHaveExactType[number.FastEquaSet]

    val result8 = fastSet.dropWhile(_ < 2)
    result8 shouldBe number.FastEquaSet(seq.dropWhile(_ < 2): _*)
    result8.shouldHaveExactType[number.FastEquaSet]

    val result9 = fastSet.dropWhile(_ < 3)
    result9 shouldBe number.FastEquaSet(seq.dropWhile(_ < 3): _*)
    result9.shouldHaveExactType[number.FastEquaSet]

    val result10 = fastSet.dropWhile(_ < 4)
    result10 shouldBe number.FastEquaSet(seq.dropWhile(_ < 4): _*)
    result10.shouldHaveExactType[number.FastEquaSet]

    val result11 = fastSet.dropWhile(_ < 5)
    result11 shouldBe number.FastEquaSet(seq.dropWhile(_ < 5): _*)
    result11.shouldHaveExactType[number.FastEquaSet]

    val result12 = fastSet.dropWhile(_ < 6)
    result12 shouldBe number.FastEquaSet()
    result12.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have an path method" in {
    lower.EquaSet("hi").path shouldBe lower
    lower.FastEquaSet("hi").path shouldBe lower
  }
  it should "have an equals method" in {
    sortedLower.SortedEquaSet("one", "two", "three") shouldEqual sortedLower.EquaSet("Three", "Two", "One")
    sortedLower.EquaSet("one", "two", "three") shouldEqual sortedLower.SortedEquaSet("Three", "Two", "One")
    val orderingEquality = StringNormalizations.lowerCased.toOrderingEquality
    val equaSets = EquaPath[String](orderingEquality) // Two different EquaPath instances
    val sortedEquaPath = SortedEquaPath[String](orderingEquality)
    val equaSet = equaSets.EquaSet("hi", "ho")
    val fastEquaSet = equaSets.FastEquaSet("Hi", "Ho")
    val sortedEquaSet = sortedEquaPath.SortedEquaSet("hI", "hO")
    val treeEquaSet = sortedEquaPath.TreeEquaSet("HI", "HO")
    equaSet shouldEqual equaSet
    equaSet shouldEqual equaSets.FastEquaSet("Hi", "Ho")
    equaSets.FastEquaSet("Hi", "Ho") shouldEqual equaSet
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
    number.EquaSet(1, 2, 3).exists(_ == 2) shouldBe true
    number.EquaSet(1, 2, 3).exists(_ == 5) shouldBe false
  }
  it should "have a filter method" in {
    val set = number.EquaSet(1, 2, 3)
    val fastSet = number.FastEquaSet(1, 2, 3)

    val result1 = set.filter(_ == 1)
    result1 shouldBe number.EquaSet(1)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = set.filter(_ == 2)
    result2 shouldBe number.EquaSet(2)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = set.filter(_ == 3)
    result3 shouldBe number.EquaSet(3)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = fastSet.filter(_ == 1)
    result4 shouldBe number.FastEquaSet(1)
    result4.shouldHaveExactType[number.FastEquaSet]

    val result5 = fastSet.filter(_ == 2)
    result5 shouldBe number.FastEquaSet(2)
    result5.shouldHaveExactType[number.FastEquaSet]

    val result6 = fastSet.filter(_ == 3)
    result6 shouldBe number.FastEquaSet(3)
    result6.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a filterNot method" in {
    val set = number.EquaSet(1, 2, 3)
    val fastSet = number.FastEquaSet(1, 2, 3)

    val result1 = set.filterNot(_ == 1)
    result1 shouldBe number.EquaSet(2, 3)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = set.filterNot(_ == 2)
    result2 shouldBe number.EquaSet(1, 3)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = set.filterNot(_ == 3)
    result3 shouldBe number.EquaSet(1, 2)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = fastSet.filterNot(_ == 1)
    result4 shouldBe number.FastEquaSet(2, 3)
    result4.shouldHaveExactType[number.FastEquaSet]

    val result5 = fastSet.filterNot(_ == 2)
    result5 shouldBe number.FastEquaSet(1, 3)
    result5.shouldHaveExactType[number.FastEquaSet]

    val result6 = fastSet.filterNot(_ == 3)
    result6 shouldBe number.FastEquaSet(1, 2)
    result6.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a find method" in {
    number.EquaSet(1, 2, 3).find(_ == 5) shouldBe None
    number.EquaSet(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have an into.flatMap method" in {

    // EquaSet into EquaPath => EquaSet
    val result1 = number.EquaSet(7, 8, 9).into(lower).flatMap(i => lower.EquaSet(i.toString))
    result1 shouldBe lower.EquaSet("7", "8", "9")
    result1.shouldHaveExactType[lower.EquaSet]

    // EquaSet into SortedEquaPath => EquaSet
    val result2 = number.EquaSet(7, 8, 9).into(sortedLower).flatMap(i => sortedLower.EquaSet(i.toString))
    result2 shouldBe sortedLower.EquaSet("7", "8", "9")
    result2.shouldHaveExactType[sortedLower.EquaSet]

    // FastEquaSet into EquaPath => FastEquaSet
    val result3 = number.FastEquaSet(7, 8, 9).into(lower).flatMap(i => lower.FastEquaSet(i.toString))
    result3 shouldBe lower.FastEquaSet("7", "8", "9")
    result3.shouldHaveExactType[lower.FastEquaSet]

    // FastEquaSet into SortedEquaPath => FastEquaSet
    val result4 = number.FastEquaSet(7, 8, 9).into(sortedLower).flatMap(i => sortedLower.FastEquaSet(i.toString))
    result4 shouldBe sortedLower.FastEquaSet("7", "8", "9")
    result4.shouldHaveExactType[sortedLower.FastEquaSet]

    // Extra stuff from oldInto test
    number.EquaSet(8).into(lower).flatMap(i => lower.EquaSet(i.toString)) shouldBe lower.EquaSet("8")
    number.EquaSet(8).into(sortedLower).flatMap(i => sortedLower.SortedEquaSet(i.toString)) shouldBe sortedLower.SortedEquaSet("8")

    number.EquaSet(9, 8, 7).into(lower).flatMap(i => lower.EquaSet(i.toString)) shouldBe lower.EquaSet("9", "8", "7")
    number.EquaSet(9, 8, 7).into(sortedLower).flatMap(i => sortedLower.SortedEquaSet(i.toString)) shouldBe sortedLower.EquaSet("9", "8", "7")

    val cis = number.EquaSet('c'.toInt, 'C'.toInt, 'b'.toInt, 'B'.toInt, 'a'.toInt, 'A'.toInt)
    cis.into(regularChar).map(i => i.toChar) shouldBe regularChar.EquaSet('A', 'a', 'b', 'B', 'C', 'c')
    (for (i <- cis.into(regularChar)) yield i.toChar) shouldBe regularChar.EquaSet('A', 'a', 'b', 'B', 'C', 'c')

    val regChars = cis.into(regularChar).flatMap(i => regularChar.EquaSet(i.toChar))
    regChars shouldBe regularChar.EquaSet('A', 'a', 'b', 'B', 'C', 'c')
    regChars.into(upperChar).flatMap(c => upperChar.EquaSet(c)) shouldBe upperChar.EquaSet('A', 'b', 'C')
    val regCharsFromFor =
      for {
        u <- (
          for (c <- cis into regularChar) yield c.toChar
        ) into upperChar
      } yield u
    regCharsFromFor shouldBe upperChar.EquaSet('A', 'B', 'C')
  }
  it should "have a flatMap method" in {
    val result1 = number.EquaSet(1, 2, 3) flatMap (i => number.EquaSet(i + 1))
    result1 shouldBe number.EquaSet(2, 3, 4)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.EquaSet(5) flatMap (i => number.EquaSet(i + 3))
    result2 shouldBe number.EquaSet(8)
    result2.shouldHaveExactType[number.EquaSet]

    val ss = number.EquaSet(1, 2)
    val is = number.EquaSet(1, 2, 3)

    val result3 = (for (s <- ss; i <- is) yield s + i)
    result3 shouldBe number.EquaSet(2, 3, 4, 3, 4, 5)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = number.FastEquaSet(1, 2, 3) flatMap (i => number.FastEquaSet(i + 1))
    result4 shouldBe number.FastEquaSet(2, 3, 4)
    result4.shouldHaveExactType[number.FastEquaSet]

    val result5 = number.FastEquaSet(5) flatMap (i => number.FastEquaSet(i + 3))
    result5 shouldBe number.FastEquaSet(8)
    result5.shouldHaveExactType[number.FastEquaSet]

    val fss = number.FastEquaSet(1, 2)
    val fis = number.FastEquaSet(1, 2, 3)

    val result6 = (for (s <- fss; i <- fis) yield s + i)
    result6 shouldBe number.FastEquaSet(2, 3, 4, 3, 4, 5)
    result6.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have an into.flatten method that works on nested EquaSet" in {
/*
    implicit def nestedOrdering: Ordering[number.SortedEquaSet] =
      new Ordering[number.SortedEquaSet] {
        def compare(x: number.SortedEquaSet, y: number.SortedEquaSet): Int = x.size - y.size
      }
*/

    // EquaSet into EquaPath => EquaSet
    val numberNumber1 = EquaPath[number.EquaSet](normalHashingEquality[number.EquaSet])
    val result1 = numberNumber1.EquaSet(number.EquaSet(1, 2), number.EquaSet(3)).into(number).flatten
    result1 shouldBe number.EquaSet(1, 2, 3)
    result1.shouldHaveExactType[number.EquaSet]

    // EquaSet into SortedEquaPath => EquaSet
    val numberNumber2 = EquaPath[sortedNumber.SortedEquaSet](normalHashingEquality[sortedNumber.SortedEquaSet])
    val result2 = numberNumber2.EquaSet(sortedNumber.SortedEquaSet(1, 2), sortedNumber.SortedEquaSet(3)).into(sortedNumber).flatten
    result2 shouldBe sortedNumber.EquaSet(1, 2, 3)
    result2.shouldHaveExactType[sortedNumber.EquaSet]

    // FastEquaSet into EquaPath => FastEquaSet
    val numberNumber3 = EquaPath[number.FastEquaSet](normalHashingEquality[number.FastEquaSet])
    val result3 = numberNumber3.FastEquaSet(number.FastEquaSet(1, 2), number.FastEquaSet(3)).into(number).flatten // I think also true for into EquaPath.EquaSet
    result3 shouldBe number.FastEquaSet(1, 2, 3)
    result3.shouldHaveExactType[number.FastEquaSet]

    // FastEquaSet into SortedEquaPath => FastEquaSet
    val numberNumber4 = EquaPath[sortedNumber.EquaSet](normalHashingEquality[sortedNumber.EquaSet])
    val result4 = numberNumber4.FastEquaSet(sortedNumber.EquaSet(1, 2), sortedNumber.EquaSet(3)).into(sortedNumber).flatten
    result4 shouldBe sortedNumber.FastEquaSet(1, 2, 3)
    result4.shouldHaveExactType[sortedNumber.FastEquaSet]

    // Extra stuff from oldInto test
    numberNumber.EquaSet(number.EquaSet(1, 2), number.EquaSet(3)).into(number).flatten shouldBe number.EquaSet(1, 2, 3)
    numberNumber.EquaSet(number.EquaSet(1)).into(number).flatten shouldBe number.EquaSet(1)
  }
  it can "be flattened when in a GenTraversableOnce" in {
    // need to keep this commented out until finish implementing all methods
    Vector(number.EquaSet(1, 2, 3), number.EquaSet(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    List(number.EquaSet(1, 2, 3), number.EquaSet(1, 2, 3)).flatten shouldBe List(1, 2, 3, 1, 2, 3)
    // TODO: this is not working 2.10, we may want to enable this back when we understand better how flatten is supported by the implicit in 2.10
    //List(number.EquaSet(1, 2, 3), number.EquaSet(1, 2, 3)).toIterator.flatten.toStream shouldBe List(1, 2, 3, 1, 2, 3).toIterator.toStream
    List(number.EquaSet(1, 2, 3), number.EquaSet(1, 2, 3)).par.flatten shouldBe List(1, 2, 3, 1, 2, 3).par
  }
  it should "have a flatten method that works on nested GenTraversable" in {
    numberList.EquaSet(List(1, 2), List(3)).flatten shouldBe List(1, 2, 3)
    numberList.EquaSet(List(1)).flatten shouldBe List(1)
  }
  it should "have a fold method" in {
    number.EquaSet(1).fold(0)(_ + _) shouldBe 1
    number.EquaSet(1).fold(1)(_ * _) shouldBe 1
    number.EquaSet(2).fold(0)(_ + _) shouldBe 2
    number.EquaSet(2).fold(1)(_ * _) shouldBe 2
    number.EquaSet(3).fold(0)(_ + _) shouldBe 3
    number.EquaSet(3).fold(1)(_ * _) shouldBe 3
    number.EquaSet(1, 2, 3).fold(0)(_ + _) shouldBe 6
    number.EquaSet(1, 2, 3).fold(1)(_ * _) shouldBe 6
    number.EquaSet(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    number.EquaSet(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    number.EquaSet(1).foldLeft(0)(_ + _) shouldBe 1
    number.EquaSet(1).foldLeft(1)(_ + _) shouldBe 2
    number.EquaSet(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    number.EquaSet(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    number.EquaSet(1).foldRight(0)(_ + _) shouldBe 1
    number.EquaSet(1).foldRight(1)(_ + _) shouldBe 2
    number.EquaSet(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    number.EquaSet(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    number.EquaSet(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    number.EquaSet(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    number.EquaSet(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- number.EquaSet(1, 2, 3))
      num += i
    num shouldBe 12
    number.EquaSet(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    val result1 = number.EquaSet(1, 2, 3, 4, 5).groupBy(_ % 2)
    result1 shouldBe Map(1 -> number.EquaSet(1, 3, 5), 0 -> number.EquaSet(2, 4))
    result1.shouldHaveExactType[scala.collection.GenMap[Int, number.EquaSet]]

    val result2 = number.EquaSet(1, 2, 3, 3, 3).groupBy(_ % 2)
    result2 shouldBe Map(1 -> number.EquaSet(1, 3, 3, 3), 0 -> number.EquaSet(2))
    result2.shouldHaveExactType[scala.collection.GenMap[Int, number.EquaSet]]

    val result3 = number.EquaSet(1, 1, 3, 3, 3).groupBy(_ % 2)
    result3 shouldBe Map(1 -> number.EquaSet(1, 1, 3, 3, 3))
    result3.shouldHaveExactType[scala.collection.GenMap[Int, number.EquaSet]]

    val result4 = number.EquaSet(1, 2, 3, 5, 7).groupBy(_ % 2)
    result4 shouldBe Map(1 -> number.EquaSet(1, 3, 5, 7), 0 -> number.EquaSet(2))
    result4.shouldHaveExactType[scala.collection.GenMap[Int, number.EquaSet]]

    val result5 = number.FastEquaSet(1, 2, 3, 4, 5).groupBy(_ % 2)
    result5 shouldBe Map(1 -> number.FastEquaSet(1, 3, 5), 0 -> number.FastEquaSet(2, 4))
    result5.shouldHaveExactType[scala.collection.GenMap[Int, number.FastEquaSet]]

    val result6 = number.FastEquaSet(1, 2, 3, 3, 3).groupBy(_ % 2)
    result6 shouldBe Map(1 -> number.FastEquaSet(1, 3, 3, 3), 0 -> number.FastEquaSet(2))
    result6.shouldHaveExactType[scala.collection.GenMap[Int, number.FastEquaSet]]

    val result7 = number.FastEquaSet(1, 1, 3, 3, 3).groupBy(_ % 2)
    result7 shouldBe Map(1 -> number.FastEquaSet(1, 1, 3, 3, 3))
    result7.shouldHaveExactType[scala.collection.GenMap[Int, number.FastEquaSet]]

    val result8 = number.FastEquaSet(1, 2, 3, 5, 7).groupBy(_ % 2)
    result8 shouldBe Map(1 -> number.FastEquaSet(1, 3, 5, 7), 0 -> number.FastEquaSet(2))
    result8.shouldHaveExactType[scala.collection.GenMap[Int, number.FastEquaSet]]
  }
  it should "have a grouped method" in {
    val result1 = number.EquaSet(1, 2, 3).grouped(2).toList
    result1 shouldBe List(number.EquaSet(1, 2), number.EquaSet(3))
    result1.shouldHaveExactType[List[number.EquaSet]]

    val result2 = number.EquaSet(1, 2, 3).grouped(1).toList
    result2 shouldBe List(number.EquaSet(1), number.EquaSet(2), number.EquaSet(3))
    result2.shouldHaveExactType[List[number.EquaSet]]

    an [IllegalArgumentException] should be thrownBy { number.EquaSet(1, 2, 3).grouped(0).toList }

    val set = number.EquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val fastSet = number.FastEquaSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val seq = set.toSet.toSeq

    val result3 = set.grouped(2).toList
    result3 shouldBe List(number.EquaSet(seq(0), seq(1)), number.EquaSet(seq(2), seq(3)), number.EquaSet(seq(4), seq(5)), number.EquaSet(seq(6), seq(7)), number.EquaSet(seq(8), seq(9)))
    result3.shouldHaveExactType[List[number.EquaSet]]

    val result4 = set.grouped(3).toList
    result4 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2)), number.EquaSet(seq(3), seq(4), seq(5)), number.EquaSet(seq(6), seq(7), seq(8)), number.EquaSet(seq(9)))
    result4.shouldHaveExactType[List[number.EquaSet]]

    val result5 = set.grouped(4).toList
    result5 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2), seq(3)), number.EquaSet(seq(4), seq(5), seq(6), seq(7)), number.EquaSet(seq(8), seq(9)))
    result5.shouldHaveExactType[List[number.EquaSet]]

    val result6 = number.EquaSet(1).grouped(2).toList
    result6 shouldBe List(number.EquaSet(1))
    result6.shouldHaveExactType[List[number.EquaSet]]

    val result7 = number.EquaSet(1).grouped(1).toList
    result7 shouldBe List(number.EquaSet(1))
    result7.shouldHaveExactType[List[number.EquaSet]]

    val result8 = number.FastEquaSet(1, 2, 3).grouped(2).toList
    result8 shouldBe List(number.FastEquaSet(1, 2), number.FastEquaSet(3))
    result8.shouldHaveExactType[List[number.FastEquaSet]]

    val result9 = number.FastEquaSet(1, 2, 3).grouped(1).toList
    result9 shouldBe List(number.FastEquaSet(1), number.EquaSet(2), number.FastEquaSet(3))
    result9.shouldHaveExactType[List[number.FastEquaSet]]

    val result10 = fastSet.grouped(2).toList
    result10 shouldBe List(number.FastEquaSet(seq(0), seq(1)), number.EquaSet(seq(2), seq(3)), number.FastEquaSet(seq(4), seq(5)), number.EquaSet(seq(6), seq(7)), number.FastEquaSet(seq(8), seq(9)))
    result10.shouldHaveExactType[List[number.FastEquaSet]]

    val result11 = fastSet.grouped(3).toList
    result11 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2)), number.FastEquaSet(seq(3), seq(4), seq(5)), number.FastEquaSet(seq(6), seq(7), seq(8)), number.FastEquaSet(seq(9)))
    result11.shouldHaveExactType[List[number.FastEquaSet]]

    val result12 = fastSet.grouped(4).toList
    result12 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2), seq(3)), number.FastEquaSet(seq(4), seq(5), seq(6), seq(7)), number.FastEquaSet(seq(8), seq(9)))
    result12.shouldHaveExactType[List[number.FastEquaSet]]

    val result13 = number.FastEquaSet(1).grouped(2).toList
    result13 shouldBe List(number.FastEquaSet(1))
    result13.shouldHaveExactType[List[number.FastEquaSet]]

    val result14 = number.FastEquaSet(1).grouped(1).toList
    result14 shouldBe List(number.FastEquaSet(1))
    result14.shouldHaveExactType[List[number.FastEquaSet]]
  }
  it should "have a hasDefiniteSize method" in {
    number.EquaSet(1).hasDefiniteSize shouldBe true
    number.EquaSet(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a head method" in {
    lower.EquaSet("hi").head shouldBe "hi"
    number.EquaSet(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    lower.EquaSet("hi").headOption shouldBe Some("hi")
    number.EquaSet(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have an init method" in {
    val result1 = number.EquaSet(1, 2, 3).init
    result1 shouldBe number.EquaSet(1, 2)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.FastEquaSet(1, 2, 3).init
    result2 shouldBe number.FastEquaSet(1, 2)
    result2.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have an inits method" in {
    val inits = number.EquaSet(1, 2, 3).inits
    val result1 = inits.next
    result1 shouldBe number.EquaSet(1,2,3)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = inits.next
    result2 shouldBe number.EquaSet(1,2)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = inits.next
    result3 shouldBe number.EquaSet(1)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = inits.next
    result4 shouldBe number.EquaSet()
    result4.shouldHaveExactType[number.EquaSet]

    val result5 = inits.hasNext
    result5 shouldBe false

    val fastInits = number.FastEquaSet(1, 2, 3).inits
    val result6 = fastInits.next
    result6 shouldBe number.FastEquaSet(1,2,3)
    result6.shouldHaveExactType[number.FastEquaSet]

    val result7 = fastInits.next
    result7 shouldBe number.FastEquaSet(1,2)
    result7.shouldHaveExactType[number.FastEquaSet]

    val result8 = fastInits.next
    result8 shouldBe number.FastEquaSet(1)
    result8.shouldHaveExactType[number.FastEquaSet]

    val result9 = fastInits.next
    result9 shouldBe number.FastEquaSet()
    result9.shouldHaveExactType[number.FastEquaSet]

    val result10 = fastInits.hasNext
    result10 shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    lower.EquaSet("hi").isTraversableAgain shouldBe true
    number.EquaSet(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have a last method" in {
    lower.EquaSet("hi").last shouldBe "hi"
    number.EquaSet(1, 2, 3).last shouldBe 3
  }
  it should "have an lastOption method" in {
    lower.EquaSet("hi").lastOption shouldBe Some("hi")
    number.EquaSet(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an into.map method" in {
    // Can map directly if want to stay in same EquaPath
    number.EquaSet(1, 2, 3).map(_ + 1) shouldBe number.EquaSet(2, 3, 4)
    (for (ele <- number.EquaSet(1, 2, 3)) yield ele * 2) shouldBe number.EquaSet(2, 4, 6)
    number.EquaSet(5) map (_ + 3) shouldBe number.EquaSet(8)

    // Can map into self explicitly too
    number.EquaSet(1, 2, 3).into(number).map(_ + 1) shouldBe number.EquaSet(2, 3, 4)
    number.EquaSet(5).into(number).map(_ + 3) shouldBe number.EquaSet(8)

    // EquaSet into EquaPath => EquaSet
    val result1 = number.EquaSet(7, 8, 9).into(lower).map(_.toString)
    result1 shouldBe lower.EquaSet("7", "8", "9")
    result1.shouldHaveExactType[lower.EquaSet]

    // EquaSet into SortedEquaPath => EquaSet
    val result2 = number.EquaSet(7, 8, 9).into(sortedLower).map(_.toString)
    result2 shouldBe sortedLower.EquaSet("7", "8", "9")
    result2.shouldHaveExactType[sortedLower.EquaSet]

    // FastEquaSet into EquaPath => FastEquaSet
    val result3 = number.FastEquaSet(7, 8, 9).into(lower).map(_.toString)
    result3 shouldBe lower.FastEquaSet("7", "8", "9")
    result3.shouldHaveExactType[lower.FastEquaSet]

    // FastEquaSet into SortedEquaPath => FastEquaSet
    val result4 = number.FastEquaSet(7, 8, 9).into(sortedLower).map(_.toString)
    result4 shouldBe sortedLower.FastEquaSet("7", "8", "9")
    result4.shouldHaveExactType[sortedLower.FastEquaSet]

    // Extra stuff from oldInto test
    number.EquaSet(1, 2, 3).into(number).map(_ + 1) shouldBe number.EquaSet(2, 3, 4)
    number.EquaSet(5).into(number).map(_ + 3) shouldBe number.EquaSet(8)
    number.EquaSet(8).into(lower).map(_.toString) shouldBe lower.EquaSet("8")
  }
  it should "have a map method" in {
    val result1 = number.EquaSet(1, 2, 3).map (_ + 1)
    result1 shouldBe number.EquaSet(2, 3, 4)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = (for (ele <- number.EquaSet(1, 2, 3)) yield ele * 2)
    result2 shouldBe number.EquaSet(2, 4, 6)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = number.EquaSet(5) map (_ + 3)
    result3 shouldBe number.EquaSet(8)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = number.FastEquaSet(1, 2, 3).map (_ + 1)
    result4 shouldBe number.FastEquaSet(2, 3, 4)
    result4.shouldHaveExactType[number.FastEquaSet]

    val result5 = (for (ele <- number.FastEquaSet(1, 2, 3)) yield ele * 2)
    result5 shouldBe number.FastEquaSet(2, 4, 6)
    result5.shouldHaveExactType[number.FastEquaSet]

    val result6 = number.FastEquaSet(5) map (_ + 3)
    result6 shouldBe number.FastEquaSet(8)
    result6.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a max method" in {
    number.EquaSet(1, 2, 3, 4, 5).max shouldBe 5
    number.EquaSet(1).max shouldBe 1
    number.EquaSet(-1).max shouldBe -1
    lower.EquaSet("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    number.EquaSet(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    number.EquaSet(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    number.EquaSet(1, 2, 3, 4, 5).min shouldBe 1
    number.EquaSet(1).min shouldBe 1
    number.EquaSet(-1).min shouldBe -1
    lower.EquaSet("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    number.EquaSet(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    number.EquaSet(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a 3 mkString method" in {

    lower.EquaSet("hi").mkString shouldBe "hi"
    number.EquaSet(1, 2, 3).mkString shouldBe "123"

    lower.EquaSet("hi").mkString("#") shouldBe "hi"
    number.EquaSet(1, 2, 3).mkString("#") shouldBe "1#2#3"
    number.EquaSet(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    lower.EquaSet("hi").mkString("<", "#", ">") shouldBe "<hi>"
    number.EquaSet(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    number.EquaSet(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    lower.EquaSet("hi").nonEmpty shouldBe true
    number.EquaSet(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have a partition method" in {
    val result1 = number.EquaSet(1, 2, 3, 4).partition(_ < 3)
    result1 shouldBe (number.EquaSet(1, 2), number.EquaSet(3, 4))
    result1.shouldHaveExactType[(number.EquaSet, number.EquaSet)]

    val result2 = number.FastEquaSet(1, 2, 3, 4).partition(_ < 3)
    result2 shouldBe (number.FastEquaSet(1, 2), number.FastEquaSet(3, 4))
    result2.shouldHaveExactType[(number.FastEquaSet, number.FastEquaSet)]
  }
  it should "have a product method" in {
    number.EquaSet(1, 2, 3).product shouldBe 6
    number.EquaSet(3).product shouldBe 3
    number.EquaSet(3, 4, 5).product shouldBe 60
    number.EquaSet(3, 4, 5).product shouldBe 60
  }
  it should "have a reduce method" in {
    number.EquaSet(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    number.EquaSet(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    number.EquaSet(5).reduce(_ + _) shouldBe 5
    number.EquaSet(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    number.EquaSet(1).reduceLeft(_ + _) shouldBe 1
    number.EquaSet(1).reduceLeft(_ * _) shouldBe 1
    number.EquaSet(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    number.EquaSet(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    number.EquaSet(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    number.EquaSet(1).reduceLeftOption(_ + _) shouldBe Some(1)
    number.EquaSet(1).reduceLeftOption(_ * _) shouldBe Some(1)
    number.EquaSet(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    number.EquaSet(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    number.EquaSet(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    number.EquaSet(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    number.EquaSet(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    number.EquaSet(5).reduceOption(_ + _) shouldBe Some(5)
    number.EquaSet(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    number.EquaSet(1).reduceRight(_ * _) shouldBe 1
    number.EquaSet(1, 2, 3).reduceRight(_ + _) shouldBe 6
    number.EquaSet(1, 2, 3).reduceRight(_ * _) shouldBe 6
    number.EquaSet(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    number.EquaSet(1).reduceRightOption(_ + _) shouldBe Some(1)
    number.EquaSet(1).reduceRightOption(_ * _) shouldBe Some(1)
    number.EquaSet(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    number.EquaSet(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    number.EquaSet(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a repr method" in {
    number.EquaSet(1, 2, 3).repr shouldBe Set(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a sameElements method that takes a GenIterable" in {
    number.EquaSet(1, 2, 3, 4, 5).sameElements(number.EquaSet(1, 2, 3, 4, 5).toSet.toSeq) shouldBe true
    number.EquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4)) shouldBe false
    number.EquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5, 6)) shouldBe false
    number.EquaSet(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 4)) shouldBe false
    number.EquaSet(3).sameElements(List(1, 2, 3, 4, 5)) shouldBe false
    number.EquaSet(3).sameElements(List(1)) shouldBe false
    number.EquaSet(3).sameElements(List(3)) shouldBe true
  }
  it should "have an into.scanLeft method" in {

    // EquaSet into EquaPath => EquaSet
    val result1 = number.EquaSet(7, 8, 9).into(lower).scanLeft("z")(_ + _)
    result1 shouldBe lower.EquaSet("z", "z7", "z78", "z789")
    result1.shouldHaveExactType[lower.EquaSet]

    // EquaSet into SortedEquaPath => EquaSet
    val result2 = number.EquaSet(7, 8, 9).into(sortedLower).scanLeft("z")(_ + _)
    result2 shouldBe sortedLower.EquaSet("z", "z7", "z78", "z789")
    result2.shouldHaveExactType[sortedLower.EquaSet]

    // FastEquaSet into EquaPath => FastEquaSet
    val result3 = number.FastEquaSet(7, 8, 9).into(lower).scanLeft("z")(_ + _)
    result3 shouldBe lower.FastEquaSet("z", "z7", "z78", "z789")
    result3.shouldHaveExactType[lower.FastEquaSet]

    // FastEquaSet into SortedEquaPath => FastEquaSet
    val result4 = number.FastEquaSet(7, 8, 9).into(sortedLower).scanLeft("z")(_ + _)
    result4 shouldBe sortedLower.FastEquaSet("z", "z7", "z78", "z789")
    result4.shouldHaveExactType[sortedLower.FastEquaSet]

    // Extra stuff from oldInto test
    number.EquaSet(1, 2, 3).into(lower).scanLeft("z")(_ + _) shouldBe lower.EquaSet("z", "z1", "z12", "z123")
    number.EquaSet(0).into(lower).scanLeft("z")(_ + _) shouldBe lower.EquaSet("z", "z0")
  }
  it should "have a scanLeft method" in {
    val result1 = number.EquaSet(1).scanLeft(0)(_ + _)
    result1 shouldBe number.EquaSet(0, 1)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.EquaSet(1, 2, 3).scanLeft(0)(_ + _)
    result2 shouldBe number.EquaSet(0, 1, 3, 6)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = number.FastEquaSet(1).scanLeft(0)(_ + _)
    result3 shouldBe number.FastEquaSet(0, 1)
    result3.shouldHaveExactType[number.FastEquaSet]

    val result4 = number.FastEquaSet(1, 2, 3).scanLeft(0)(_ + _)
    result4 shouldBe number.FastEquaSet(0, 1, 3, 6)
    result4.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a scanRight method" in {
    val result1 = number.EquaSet(1).scanRight(0)(_ + _)
    result1 shouldBe number.EquaSet(1, 0)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.EquaSet(1, 2, 3).scanRight(0)(_ + _)
    result2 shouldBe number.EquaSet(6, 5, 3, 0)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = number.FastEquaSet(1).scanRight(0)(_ + _)
    result3 shouldBe number.FastEquaSet(1, 0)
    result3.shouldHaveExactType[number.FastEquaSet]

    val result4 = number.FastEquaSet(1, 2, 3).scanRight(0)(_ + _)
    result4 shouldBe number.FastEquaSet(6, 5, 3, 0)
    result4.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have an into.scanRight method" in {

    // EquaSet into EquaPath => EquaSet
    val result1 = number.EquaSet(7, 8, 9).into(lower).scanRight("z")(_ + _)
    result1 shouldBe lower.EquaSet("789z", "89z", "9z", "z")
    result1.shouldHaveExactType[lower.EquaSet]

    // EquaSet into SortedEquaPath => EquaSet
    val result2 = number.EquaSet(7, 8, 9).into(sortedLower).scanRight("z")(_ + _)
    result2 shouldBe sortedLower.EquaSet("789z", "89z", "9z", "z")
    result2.shouldHaveExactType[sortedLower.EquaSet]

    // FastEquaSet into EquaPath => FastEquaSet
    val result3 = number.FastEquaSet(7, 8, 9).into(lower).scanRight("z")(_ + _)
    result3 shouldBe lower.FastEquaSet("789z", "89z", "9z", "z")
    result3.shouldHaveExactType[lower.FastEquaSet]

    // FastEquaSet into SortedEquaPath => FastEquaSet
    val result4 = number.FastEquaSet(7, 8, 9).into(sortedLower).scanRight("z")(_ + _)
    result4 shouldBe sortedLower.FastEquaSet("789z", "89z", "9z", "z")
    result4.shouldHaveExactType[sortedLower.FastEquaSet]

    // Extra stuff from oldInto test
    number.EquaSet(1, 2, 3).into(lower).scanRight("z")(_ + _) shouldBe lower.EquaSet("123z", "23z", "3z", "z")
    number.EquaSet(0).into(lower).scanRight("z")(_ + _) shouldBe lower.EquaSet("0z", "z")
  }
  it should "have a slice method" in {
    val result1 = number.EquaSet(3).slice(0, 0)
    result1 shouldBe number.EquaSet()
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.EquaSet(1, 2, 3).slice(2, 1)
    result2 shouldBe number.EquaSet()
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = number.EquaSet(1, 2, 3).slice(1, 3)
    result3 shouldBe number.EquaSet(2, 3)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = number.FastEquaSet(3).slice(0, 0)
    result4 shouldBe number.FastEquaSet()
    result4.shouldHaveExactType[number.FastEquaSet]

    val result5 = number.FastEquaSet(1, 2, 3).slice(2, 1)
    result5 shouldBe number.FastEquaSet()
    result5.shouldHaveExactType[number.FastEquaSet]

    val result6 = number.FastEquaSet(1, 2, 3).slice(1, 3)
    result6 shouldBe number.FastEquaSet(2, 3)
    result6.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have 2 sliding methods" in {

    val seq = number.EquaSet(1, 2, 3, 4, 5).toSet.toSeq

    val result1 = number.EquaSet(1).sliding(1).toList
    result1 shouldBe List(number.EquaSet(1))
    result1.shouldHaveExactType[List[number.EquaSet]]

    val result2 = number.EquaSet(1).sliding(2).toList
    result2 shouldBe List(number.EquaSet(1))
    result2.shouldHaveExactType[List[number.EquaSet]]

    val result3 = number.EquaSet(1, 2, 3).sliding(2).toList
    result3 shouldBe List(number.EquaSet(1, 2), number.EquaSet(2, 3))
    result3.shouldHaveExactType[List[number.EquaSet]]

    val result4 = number.EquaSet(1, 2, 3).sliding(1).toList
    result4 shouldBe List(number.EquaSet(1), number.EquaSet(2), number.EquaSet(3))
    result4.shouldHaveExactType[List[number.EquaSet]]

    val result5 = number.EquaSet(1, 2, 3).sliding(3).toList
    result5 shouldBe List(number.EquaSet(1, 2, 3))
    result5.shouldHaveExactType[List[number.EquaSet]]

    val result6 = number.EquaSet(1, 2, 3, 4, 5).sliding(3).toList
    result6 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2)), number.EquaSet(seq(1), seq(2), seq(3)), number.EquaSet(seq(2), seq(3), seq(4)))
    result6.shouldHaveExactType[List[number.EquaSet]]

    val result7 = number.EquaSet(1, 2, 3, 4, 5).sliding(2).toList
    result7 shouldBe List(number.EquaSet(seq(0), seq(1)), number.EquaSet(seq(1), seq(2)), number.EquaSet(seq(2), seq(3)), number.EquaSet(seq(3), seq(4)))
    result7.shouldHaveExactType[List[number.EquaSet]]

    val result8 = number.EquaSet(1, 2, 3, 4, 5).sliding(1).toList
    result8 shouldBe List(number.EquaSet(seq(0)), number.EquaSet(seq(1)), number.EquaSet(seq(2)), number.EquaSet(seq(3)), number.EquaSet(seq(4)))
    result8.shouldHaveExactType[List[number.EquaSet]]

    val result9 = number.EquaSet(1, 2, 3, 4, 5).sliding(4).toList
    result9 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2), seq(3)), number.EquaSet(seq(1), seq(2), seq(3), seq(4)))
    result9.shouldHaveExactType[List[number.EquaSet]]

    val result10 = number.EquaSet(1, 2, 3, 4, 5).sliding(5).toList
    result10 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2), seq(3), seq(4)))
    result10.shouldHaveExactType[List[number.EquaSet]]

    val result11 = number.EquaSet(1).sliding(1, 1).toList
    result11 shouldBe List(number.EquaSet(1))
    result11.shouldHaveExactType[List[number.EquaSet]]

    val result12 = number.EquaSet(1).sliding(1, 2).toList
    result12 shouldBe List(number.EquaSet(1))
    result12.shouldHaveExactType[List[number.EquaSet]]

    val result13 = number.EquaSet(1, 2, 3).sliding(1, 1).toList
    result13 shouldBe List(number.EquaSet(1), number.EquaSet(2), number.EquaSet(3))
    result13.shouldHaveExactType[List[number.EquaSet]]

    val result14 = number.EquaSet(1, 2, 3).sliding(2, 1).toList
    result14 shouldBe List(number.EquaSet(1, 2), number.EquaSet(2, 3))
    result14.shouldHaveExactType[List[number.EquaSet]]

    val result15 = number.EquaSet(1, 2, 3).sliding(2, 2).toList
    result15 shouldBe List(number.EquaSet(1, 2), number.EquaSet(3))
    result15.shouldHaveExactType[List[number.EquaSet]]

    val result16 = number.EquaSet(1, 2, 3).sliding(3, 2).toList
    result16 shouldBe List(number.EquaSet(1, 2, 3))
    result16.shouldHaveExactType[List[number.EquaSet]]

    val result17 = number.EquaSet(1, 2, 3).sliding(3, 1).toList
    result17 shouldBe List(number.EquaSet(1, 2, 3))
    result17.shouldHaveExactType[List[number.EquaSet]]

    val result18 = number.EquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result18 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2)), number.EquaSet(seq(1), seq(2), seq(3)), number.EquaSet(seq(2), seq(3), seq(4)))
    result18.shouldHaveExactType[List[number.EquaSet]]

    val result19 = number.EquaSet(1, 2, 3, 4, 5).sliding(2, 2).toList
    result19 shouldBe List(number.EquaSet(seq(0), seq(1)), number.EquaSet(seq(2), seq(3)), number.EquaSet(seq(4)))
    result19.shouldHaveExactType[List[number.EquaSet]]

    val result20 = number.EquaSet(1, 2, 3, 4, 5).sliding(2, 3).toList
    result20 shouldBe List(number.EquaSet(seq(0), seq(1)), number.EquaSet(seq(3), seq(4)))
    result20.shouldHaveExactType[List[number.EquaSet]]

    val result21 = number.EquaSet(1, 2, 3, 4, 5).sliding(2, 4).toList
    result21 shouldBe List(number.EquaSet(seq(0), seq(1)), number.EquaSet(seq(4)))
    result21.shouldHaveExactType[List[number.EquaSet]]

    val result22 = number.EquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result22 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2)), number.EquaSet(seq(1), seq(2), seq(3)), number.EquaSet(seq(2), seq(3), seq(4)))
    result22.shouldHaveExactType[List[number.EquaSet]]

    val result23 = number.EquaSet(1, 2, 3, 4, 5).sliding(3, 2).toList
    result23 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2)), number.EquaSet(seq(2), seq(3), seq(4)))
    result23.shouldHaveExactType[List[number.EquaSet]]

    val result24 = number.EquaSet(1, 2, 3, 4, 5).sliding(3, 3).toList
    result24 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2)), number.EquaSet(seq(3), seq(4)))
    result24.shouldHaveExactType[List[number.EquaSet]]

    val result25 = number.EquaSet(1, 2, 3, 4, 5).sliding(3, 4).toList
    result25 shouldBe List(number.EquaSet(seq(0), seq(1), seq(2)), number.EquaSet(seq(4)))
    result25.shouldHaveExactType[List[number.EquaSet]]

    val result26 = number.FastEquaSet(1).sliding(1).toList
    result26 shouldBe List(number.FastEquaSet(1))
    result26.shouldHaveExactType[List[number.FastEquaSet]]

    val result27 = number.FastEquaSet(1).sliding(2).toList
    result27 shouldBe List(number.FastEquaSet(1))
    result27.shouldHaveExactType[List[number.FastEquaSet]]

    val result28 = number.FastEquaSet(1, 2, 3).sliding(2).toList
    result28 shouldBe List(number.FastEquaSet(1, 2), number.FastEquaSet(2, 3))
    result28.shouldHaveExactType[List[number.FastEquaSet]]

    val result29 = number.FastEquaSet(1, 2, 3).sliding(1).toList
    result29 shouldBe List(number.FastEquaSet(1), number.FastEquaSet(2), number.FastEquaSet(3))
    result29.shouldHaveExactType[List[number.FastEquaSet]]

    val result30 = number.FastEquaSet(1, 2, 3).sliding(3).toList
    result30 shouldBe List(number.FastEquaSet(1, 2, 3))
    result30.shouldHaveExactType[List[number.FastEquaSet]]

    val result31 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(3).toList
    result31 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2)), number.FastEquaSet(seq(1), seq(2), seq(3)), number.FastEquaSet(seq(2), seq(3), seq(4)))
    result31.shouldHaveExactType[List[number.FastEquaSet]]

    val result32 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(2).toList
    result32 shouldBe List(number.FastEquaSet(seq(0), seq(1)), number.FastEquaSet(seq(1), seq(2)), number.FastEquaSet(seq(2), seq(3)), number.FastEquaSet(seq(3), seq(4)))
    result32.shouldHaveExactType[List[number.FastEquaSet]]

    val result33 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(1).toList
    result33 shouldBe List(number.FastEquaSet(seq(0)), number.FastEquaSet(seq(1)), number.FastEquaSet(seq(2)), number.FastEquaSet(seq(3)), number.FastEquaSet(seq(4)))
    result33.shouldHaveExactType[List[number.FastEquaSet]]

    val result34 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(4).toList
    result34 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2), seq(3)), number.FastEquaSet(seq(1), seq(2), seq(3), seq(4)))
    result34.shouldHaveExactType[List[number.FastEquaSet]]

    val result35 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(5).toList
    result35 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2), seq(3), seq(4)))
    result35.shouldHaveExactType[List[number.FastEquaSet]]

    val result36 = number.FastEquaSet(1).sliding(1, 1).toList
    result36 shouldBe List(number.FastEquaSet(1))
    result36.shouldHaveExactType[List[number.FastEquaSet]]

    val result37 = number.FastEquaSet(1).sliding(1, 2).toList
    result37 shouldBe List(number.FastEquaSet(1))
    result37.shouldHaveExactType[List[number.FastEquaSet]]

    val result38 = number.FastEquaSet(1, 2, 3).sliding(1, 1).toList
    result38 shouldBe List(number.FastEquaSet(1), number.FastEquaSet(2), number.FastEquaSet(3))
    result38.shouldHaveExactType[List[number.FastEquaSet]]

    val result39 = number.FastEquaSet(1, 2, 3).sliding(2, 1).toList
    result39 shouldBe List(number.FastEquaSet(1, 2), number.FastEquaSet(2, 3))
    result39.shouldHaveExactType[List[number.FastEquaSet]]

    val result40 = number.FastEquaSet(1, 2, 3).sliding(2, 2).toList
    result40 shouldBe List(number.FastEquaSet(1, 2), number.FastEquaSet(3))
    result40.shouldHaveExactType[List[number.FastEquaSet]]

    val result41 = number.FastEquaSet(1, 2, 3).sliding(3, 2).toList
    result41 shouldBe List(number.FastEquaSet(1, 2, 3))
    result41.shouldHaveExactType[List[number.FastEquaSet]]

    val result42 = number.FastEquaSet(1, 2, 3).sliding(3, 1).toList
    result42 shouldBe List(number.FastEquaSet(1, 2, 3))
    result42.shouldHaveExactType[List[number.FastEquaSet]]

    val result43 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result43 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2)), number.FastEquaSet(seq(1), seq(2), seq(3)), number.FastEquaSet(seq(2), seq(3), seq(4)))
    result43.shouldHaveExactType[List[number.FastEquaSet]]

    val result44 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(2, 2).toList
    result44 shouldBe List(number.FastEquaSet(seq(0), seq(1)), number.FastEquaSet(seq(2), seq(3)), number.FastEquaSet(seq(4)))
    result44.shouldHaveExactType[List[number.FastEquaSet]]

    val result45 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(2, 3).toList
    result45 shouldBe List(number.FastEquaSet(seq(0), seq(1)), number.FastEquaSet(seq(3), seq(4)))
    result45.shouldHaveExactType[List[number.FastEquaSet]]

    val result46 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(2, 4).toList
    result46 shouldBe List(number.FastEquaSet(seq(0), seq(1)), number.FastEquaSet(seq(4)))
    result46.shouldHaveExactType[List[number.FastEquaSet]]

    val result47 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 1).toList
    result47 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2)), number.FastEquaSet(seq(1), seq(2), seq(3)), number.FastEquaSet(seq(2), seq(3), seq(4)))
    result47.shouldHaveExactType[List[number.FastEquaSet]]

    val result48 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 2).toList
    result48 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2)), number.FastEquaSet(seq(2), seq(3), seq(4)))
    result48.shouldHaveExactType[List[number.FastEquaSet]]

    val result49 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 3).toList
    result49 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2)), number.FastEquaSet(seq(3), seq(4)))
    result49.shouldHaveExactType[List[number.FastEquaSet]]

    val result50 = number.FastEquaSet(1, 2, 3, 4, 5).sliding(3, 4).toList
    result50 shouldBe List(number.FastEquaSet(seq(0), seq(1), seq(2)), number.FastEquaSet(seq(4)))
    result50.shouldHaveExactType[List[number.FastEquaSet]]
  }
  it should "have a span method" in {
    val result1 = number.EquaSet(1, 2, 3).span(_ < 3)
    result1 shouldBe (number.EquaSet(1, 2), number.EquaSet(3))
    result1.shouldHaveExactType[(number.EquaSet, number.EquaSet)]

    val result2 = number.EquaSet(1, 2, 3).span(_ > 3)
    result2 shouldBe (number.EquaSet(), number.EquaSet(1, 2, 3))
    result2.shouldHaveExactType[(number.EquaSet, number.EquaSet)]

    val result3 = number.FastEquaSet(1, 2, 3).span(_ < 3)
    result3 shouldBe (number.FastEquaSet(1, 2), number.FastEquaSet(3))
    result3.shouldHaveExactType[(number.FastEquaSet, number.FastEquaSet)]

    val result4 = number.FastEquaSet(1, 2, 3).span(_ > 3)
    result4 shouldBe (number.FastEquaSet(), number.FastEquaSet(1, 2, 3))
    result4.shouldHaveExactType[(number.FastEquaSet, number.FastEquaSet)]
  }
  it should "have a splitAt method" in {
    val result1 = number.EquaSet(1, 2, 3).splitAt(0)
    result1 shouldBe (number.EquaSet(), number.EquaSet(1, 2, 3))
    result1.shouldHaveExactType[(number.EquaSet, number.EquaSet)]

    val result2 = number.EquaSet(1, 2, 3).splitAt(1)
    result2 shouldBe (number.EquaSet(1), number.EquaSet(2, 3))
    result2.shouldHaveExactType[(number.EquaSet, number.EquaSet)]

    val result3 = number.EquaSet(1, 2, 3).splitAt(2)
    result3 shouldBe (number.EquaSet(1, 2), number.EquaSet(3))
    result3.shouldHaveExactType[(number.EquaSet, number.EquaSet)]

    val result4 = number.EquaSet(1, 2, 3).splitAt(3)
    result4 shouldBe (number.EquaSet(1, 2, 3), number.EquaSet())
    result4.shouldHaveExactType[(number.EquaSet, number.EquaSet)]

    val result5 = number.FastEquaSet(1, 2, 3).splitAt(0)
    result5 shouldBe (number.FastEquaSet(), number.FastEquaSet(1, 2, 3))
    result5.shouldHaveExactType[(number.FastEquaSet, number.FastEquaSet)]

    val result6 = number.FastEquaSet(1, 2, 3).splitAt(1)
    result6 shouldBe (number.FastEquaSet(1), number.FastEquaSet(2, 3))
    result6.shouldHaveExactType[(number.FastEquaSet, number.FastEquaSet)]

    val result7 = number.FastEquaSet(1, 2, 3).splitAt(2)
    result7 shouldBe (number.FastEquaSet(1, 2), number.FastEquaSet(3))
    result7.shouldHaveExactType[(number.FastEquaSet, number.FastEquaSet)]

    val result8 = number.FastEquaSet(1, 2, 3).splitAt(3)
    result8 shouldBe (number.FastEquaSet(1, 2, 3), number.FastEquaSet())
    result8.shouldHaveExactType[(number.FastEquaSet, number.FastEquaSet)]
  }
  it should "have a stringPrefix method" in {
    number.EquaSet(1).stringPrefix shouldBe "EquaSet"
    number.EquaSet(1, 2, 3).stringPrefix shouldBe "EquaSet"
    lower.EquaSet("1").stringPrefix shouldBe "EquaSet"
    lower.EquaSet("1", "2", "3").stringPrefix shouldBe "EquaSet"
  }
  it should "have a subsetOf method" in {
    number.EquaSet(2, 3).subsetOf(number.EquaSet(1, 2, 3, 4, 5)) shouldBe true
    number.EquaSet(2).subsetOf(number.EquaSet(1, 2, 3, 4, 5)) shouldBe true
    number.EquaSet(2, 0).subsetOf(number.EquaSet(1, 2, 3, 4, 5)) shouldBe false
    lower.EquaSet("aa", "bb").subsetOf(lower.EquaSet("aa", "bb", "cc")) shouldBe true
    lower.EquaSet("aA", "Bb").subsetOf(lower.EquaSet("aa", "bb", "cc")) shouldBe true
    lower.EquaSet("aa", "bb").subsetOf(lower.EquaSet("aA", "Bb", "cC")) shouldBe true
    lower.EquaSet("aa", "bc").subsetOf(lower.EquaSet("aa", "bb", "cc")) shouldBe false
  }
  it should "have a 2 subsets method" in {
    val subsets = number.EquaSet(1, 2, 3).subsets.toList
    subsets should have length 8
    subsets should contain (number.EquaSet())
    subsets should contain (number.EquaSet(1))
    subsets should contain (number.EquaSet(2))
    subsets should contain (number.EquaSet(3))
    subsets should contain (number.EquaSet(1, 2))
    subsets should contain (number.EquaSet(1, 3))
    subsets should contain (number.EquaSet(2, 3))
    subsets should contain (number.EquaSet(1, 2, 3))

    val subsets2 = number.EquaSet(1, 2, 3).subsets(2).toList
    subsets2 should have length 3
    subsets2 should contain (number.EquaSet(1, 2))
    subsets2 should contain (number.EquaSet(1, 3))
    subsets2 should contain (number.EquaSet(2, 3))

    number.EquaSet(1, 2, 3).subsets.shouldHaveExactType[Iterator[number.EquaSet]]
    number.EquaSet(1, 2, 3).subsets(2).shouldHaveExactType[Iterator[number.EquaSet]]

    number.FastEquaSet(1, 2, 3).subsets.shouldHaveExactType[Iterator[number.FastEquaSet]]
    number.FastEquaSet(1, 2, 3).subsets(2).shouldHaveExactType[Iterator[number.FastEquaSet]]
  }
  it should "have a sum method" in {
    number.EquaSet(1).sum shouldBe 1
    number.EquaSet(5).sum shouldBe 5
    number.EquaSet(1, 2, 3).sum shouldBe 6
    number.EquaSet(1, 2, 3, 4, 5).sum shouldBe 15
  }
  it should "have an tail method" in {
    val result1 = number.EquaSet(1, 2, 3).tail
    result1 shouldBe number.EquaSet(2, 3)
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.FastEquaSet(1, 2, 3).tail
    result2 shouldBe number.FastEquaSet(2, 3)
    result2.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have an tails method" in {
    val result1 = number.EquaSet(1, 2, 3).tails
    result1.toList shouldBe List(number.EquaSet(1,2,3), number.EquaSet(2,3), number.EquaSet(3), number.EquaSet())
    result1.shouldHaveExactType[Iterator[number.EquaSet]]

    val result2 = number.FastEquaSet(1, 2, 3).tails
    result2.toList shouldBe List(number.FastEquaSet(1,2,3), number.FastEquaSet(2,3), number.FastEquaSet(3), number.FastEquaSet())
    result2.shouldHaveExactType[Iterator[number.FastEquaSet]]
  }
  it should "have a take method" in {
    val result1 = number.EquaSet(1, 2, 3).take(0)
    result1 shouldBe number.EquaSet()
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.EquaSet(1, 2, 3).take(1)
    result2 shouldBe number.EquaSet(1)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = number.EquaSet(1, 2, 3).take(2)
    result3 shouldBe number.EquaSet(1, 2)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = number.EquaSet(1, 2, 3).take(3)
    result4 shouldBe number.EquaSet(1, 2, 3)
    result4.shouldHaveExactType[number.EquaSet]

    val result5 = number.FastEquaSet(1, 2, 3).take(0)
    result5 shouldBe number.FastEquaSet()
    result5.shouldHaveExactType[number.FastEquaSet]

    val result6 = number.FastEquaSet(1, 2, 3).take(1)
    result6 shouldBe number.FastEquaSet(1)
    result6.shouldHaveExactType[number.FastEquaSet]

    val result7 = number.FastEquaSet(1, 2, 3).take(2)
    result7 shouldBe number.FastEquaSet(1, 2)
    result7.shouldHaveExactType[number.FastEquaSet]

    val result8 = number.FastEquaSet(1, 2, 3).take(3)
    result8 shouldBe number.FastEquaSet(1, 2, 3)
    result8.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a takeRight method" in {
    val result1 = number.EquaSet(1, 2, 3).takeRight(0)
    result1 shouldBe number.EquaSet()
    result1.shouldHaveExactType[number.EquaSet]

    val result2 = number.EquaSet(1, 2, 3).takeRight(1)
    result2 shouldBe number.EquaSet(3)
    result2.shouldHaveExactType[number.EquaSet]

    val result3 = number.EquaSet(1, 2, 3).takeRight(2)
    result3 shouldBe number.EquaSet(2, 3)
    result3.shouldHaveExactType[number.EquaSet]

    val result4 = number.EquaSet(1, 2, 3).takeRight(3)
    result4 shouldBe number.EquaSet(1, 2, 3)
    result4.shouldHaveExactType[number.EquaSet]

    val result5 = number.FastEquaSet(1, 2, 3).takeRight(0)
    result5 shouldBe number.FastEquaSet()
    result5.shouldHaveExactType[number.FastEquaSet]

    val result6 = number.FastEquaSet(1, 2, 3).takeRight(1)
    result6 shouldBe number.FastEquaSet(3)
    result6.shouldHaveExactType[number.FastEquaSet]

    val result7 = number.FastEquaSet(1, 2, 3).takeRight(2)
    result7 shouldBe number.FastEquaSet(2, 3)
    result7.shouldHaveExactType[number.FastEquaSet]

    val result8 = number.FastEquaSet(1, 2, 3).takeRight(3)
    result8 shouldBe number.FastEquaSet(1, 2, 3)
    result8.shouldHaveExactType[number.FastEquaSet]
  }
  it should "have a to method" in {
    number.EquaSet(1).to[List] shouldBe List(number.EquaBox(1))
    number.EquaSet(1, 2, 3).to[List] shouldBe List(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
    number.EquaSet(1, 2, 3).to[scala.collection.mutable.ListBuffer] shouldBe ListBuffer(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
    number.EquaSet(1, 2, 3).to[Vector] shouldBe Vector(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a toArray method" in {
    number.EquaSet(1, 2, 3).toArray shouldBe (Array(1, 2, 3))
    lower.EquaSet("a", "b").toArray shouldBe (Array("a", "b"))
    number.EquaSet(1).toArray shouldBe (Array(1))
  }
  it should "have a toEquaBoxArray method" in {
    number.EquaSet(1, 2, 3).toEquaBoxArray shouldBe (Array(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxArray shouldBe (Array(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxArray shouldBe (Array(number.EquaBox(1)))
  }
  it should "have a toBuffer method" in {
    number.EquaSet(1, 2, 3).toBuffer shouldBe (Buffer(1, 2, 3))
    lower.EquaSet("a", "b").toBuffer shouldBe (Buffer("a", "b"))
    number.EquaSet(1).toBuffer shouldBe (Buffer(1))
  }
  it should "have a toEquaBoxBuffer method" in {
    number.EquaSet(1, 2, 3).toEquaBoxBuffer shouldBe (Buffer(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxBuffer shouldBe (Buffer(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxBuffer shouldBe (Buffer(number.EquaBox(1)))
  }
  it should "have a toIndexedSeq method" in {
    number.EquaSet(1, 2, 3).toIndexedSeq shouldBe (IndexedSeq(1, 2, 3))
    lower.EquaSet("a", "b").toIndexedSeq shouldBe (IndexedSeq("a", "b"))
    number.EquaSet(1).toIndexedSeq shouldBe (IndexedSeq(1))
  }
  it should "have a toEquaBoxIndexedSeq method" in {
    number.EquaSet(1, 2, 3).toEquaBoxIndexedSeq shouldBe (IndexedSeq(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxIndexedSeq shouldBe (IndexedSeq(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxIndexedSeq shouldBe (IndexedSeq(number.EquaBox(1)))
  }
  it should "have a toIterable method" in {
    number.EquaSet(1, 2, 3).toIterable shouldBe (Set(1, 2, 3))
    lower.EquaSet("a", "b").toIterable shouldBe (Set("a", "b"))
    number.EquaSet(1).toIterable shouldBe (Set(1))
  }
  it should "have a toEquaBoxIterable method" in {
    number.EquaSet(1, 2, 3).toEquaBoxIterable shouldBe (Set(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxIterable shouldBe (Set(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxIterable shouldBe (Set(number.EquaBox(1)))
  }
  it should "have a toIterator method" in {
    number.EquaSet(1, 2, 3).toIterator.toList shouldBe (Iterator(1, 2, 3).toList)
    lower.EquaSet("a", "b").toIterator.toList shouldBe (Iterator("a", "b").toList)
    number.EquaSet(1).toIterator.toList shouldBe (Iterator(1).toList)
    number.EquaSet(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    lower.EquaSet("a", "b").toIterator shouldBe an [Iterator[_]]
    number.EquaSet(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toEquaBoxIterator method" in {
    number.EquaSet(1, 2, 3).toEquaBoxIterator.toList shouldBe (Iterator(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)).toList)
    lower.EquaSet("a", "b").toEquaBoxIterator.toList shouldBe (Iterator(lower.EquaBox("a"), lower.EquaBox("b")).toList)
    number.EquaSet(1).toEquaBoxIterator.toList shouldBe (Iterator(number.EquaBox(1)).toList)
    number.EquaSet(1, 2, 3).toEquaBoxIterator shouldBe an [Iterator[_]]
    lower.EquaSet("a", "b").toEquaBoxIterator shouldBe an [Iterator[_]]
    number.EquaSet(1).toEquaBoxIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    number.EquaSet(1, 2, 3).toList shouldBe (List(1, 2, 3))
    lower.EquaSet("a", "b").toList shouldBe (List("a", "b"))
    number.EquaSet(1).toList shouldBe (List(1))
  }
  it should "have a toEquaBoxList method" in {
    number.EquaSet(1, 2, 3).toEquaBoxList shouldBe (List(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxList shouldBe (List(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxList shouldBe (List(number.EquaBox(1)))
  }
  it should "have a toMap method" in {
    tuple.EquaSet((1, "one"), (2, "two"), (3, "three")).toMap shouldBe Map(1 -> "one", 2 -> "two", 3 -> "three")
  }
  it should "have a toParArray method" in {
    number.EquaSet(1, 2, 3).toParArray shouldBe ParArray(1, 2, 3)
  }
  it should "have a toEquaBoxParArray method" in {
    number.EquaSet(1, 2, 3).toEquaBoxParArray shouldBe ParArray(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a toSeq method" in {
    number.EquaSet(1, 2, 3).toSeq shouldBe (Seq(1, 2, 3))
    lower.EquaSet("a", "b").toSeq shouldBe (Seq("a", "b"))
    number.EquaSet(1).toSeq shouldBe (Seq(1))
  }
  it should "have a toEquaBoxSeq method" in {
    number.EquaSet(1, 2, 3).toEquaBoxSeq shouldBe (Seq(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxSeq shouldBe (Seq(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxSeq shouldBe (Seq(number.EquaBox(1)))
  }
  it should "have a toStream method" in {
    number.EquaSet(1, 2, 3).toStream shouldBe (Stream(1, 2, 3))
    lower.EquaSet("a", "b").toStream shouldBe (Stream("a", "b"))
    number.EquaSet(1).toStream shouldBe(Stream(1))
  }
  it should "have a toEquaBoxStream method" in {
    number.EquaSet(1, 2, 3).toEquaBoxStream shouldBe (Stream(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxStream shouldBe (Stream(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxStream shouldBe(Stream(number.EquaBox(1)))
  }
  it should "have a toEquaBoxTraversable method" in {
    number.EquaSet(1, 2, 3).toEquaBoxTraversable shouldBe (Set(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxTraversable shouldBe (Set(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxTraversable shouldBe (Set(number.EquaBox(1)))
  }
  it should "have a toTraversable method" in {
    number.EquaSet(1, 2, 3).toTraversable shouldBe (Set(1, 2, 3))
    lower.EquaSet("a", "b").toTraversable shouldBe (Set("a", "b"))
    number.EquaSet(1).toTraversable shouldBe (Set(1))
  }
  it should "have a toEquaBoxVector method" in {
    number.EquaSet(1, 2, 3).toEquaBoxVector should === (Vector(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3)))
    lower.EquaSet("a", "b").toEquaBoxVector should === (Vector(lower.EquaBox("a"), lower.EquaBox("b")))
    number.EquaSet(1).toEquaBoxVector should === (Vector(number.EquaBox(1)))
  }
  it should "have a toVector method" in {
    number.EquaSet(1, 2, 3).toVector should === (Vector(1, 2, 3))
    lower.EquaSet("a", "b").toVector should === (Vector("a", "b"))
    number.EquaSet(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    val result1 = numberList.EquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose
    result1 shouldBe numberList.EquaSet(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    result1.shouldHaveExactType[numberList.EquaSet]

    val result2 = numberList.EquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose
    result2 shouldBe numberList.EquaSet(List(1, 3, 5, 7), List(2, 4, 6, 8))
    result2.shouldHaveExactType[numberList.EquaSet]

    val result3 = numberList.EquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose.transpose
    result3 shouldBe numberList.EquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8))
    result3.shouldHaveExactType[numberList.EquaSet]

    val result4 = numberList.EquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose.transpose
    result4 shouldBe numberList.EquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    result4.shouldHaveExactType[numberList.EquaSet]

    val result5 = numberList.FastEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose
    result5 shouldBe numberList.FastEquaSet(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    result5.shouldHaveExactType[numberList.FastEquaSet]

    val result6 = numberList.FastEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose
    result6 shouldBe numberList.FastEquaSet(List(1, 3, 5, 7), List(2, 4, 6, 8))
    result6.shouldHaveExactType[numberList.FastEquaSet]

    val result7 = numberList.FastEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8)).transpose.transpose
    result7 shouldBe numberList.FastEquaSet(List(1, 2), List(3, 4), List(5, 6), List(7, 8))
    result7.shouldHaveExactType[numberList.FastEquaSet]

    val result8 = numberList.FastEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose.transpose
    result8 shouldBe numberList.FastEquaSet(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    result8.shouldHaveExactType[numberList.FastEquaSet]
  }
  it should "have an unzip method" in {
    val result1 = numberLower.EquaSet((1, "2")).unzip(number, lower)
    result1 shouldBe ((number.EquaSet(1), lower.EquaSet("2")))
    result1.shouldHaveExactType[(number.EquaSet, lower.EquaSet)]

    val result2 = numberLower.EquaSet((1, "2"), (3, "4")).unzip(number, lower)
    result2 shouldBe ((number.EquaSet(1, 3), lower.EquaSet("2", "4")))
    result2.shouldHaveExactType[(number.EquaSet, lower.EquaSet)]

    val result3 = numberLower.EquaSet((1, "2"), (3, "4"), (5, "6")).unzip(number, lower)
    result3 shouldBe ((number.EquaSet(1, 3, 5), lower.EquaSet("2", "4", "6")))
    result3.shouldHaveExactType[(number.EquaSet, lower.EquaSet)]

    val result4 = numberLower.FastEquaSet((1, "2")).unzip(number, lower)
    result4 shouldBe ((number.FastEquaSet(1), lower.FastEquaSet("2")))
    result4.shouldHaveExactType[(number.FastEquaSet, lower.FastEquaSet)]

    val result5 = numberLower.FastEquaSet((1, "2"), (3, "4")).unzip(number, lower)
    result5 shouldBe ((number.FastEquaSet(1, 3), lower.FastEquaSet("2", "4")))
    result5.shouldHaveExactType[(number.FastEquaSet, lower.FastEquaSet)]

    val result6 = numberLower.FastEquaSet((1, "2"), (3, "4"), (5, "6")).unzip(number, lower)
    result6 shouldBe ((number.FastEquaSet(1, 3, 5), lower.FastEquaSet("2", "4", "6")))
    result6.shouldHaveExactType[(number.FastEquaSet, lower.FastEquaSet)]
  }
  it should "have an unzip3 method" in {
    val result1 = numberLowerTrimmed.EquaSet((1, "2", "3")).unzip3(number, lower, trimmed)
    result1 shouldBe (number.EquaSet(1), lower.EquaSet("2"), trimmed.EquaSet("3"))
    result1.shouldHaveExactType[(number.EquaSet, lower.EquaSet, trimmed.EquaSet)]

    val result2 = numberLowerTrimmed.EquaSet((1, "2", "3"), (4, "5", "6")).unzip3(number, lower, trimmed)
    result2 shouldBe (number.EquaSet(1, 4), lower.EquaSet("2", "5"), trimmed.EquaSet("3", "6"))
    result2.shouldHaveExactType[(number.EquaSet, lower.EquaSet, trimmed.EquaSet)]

    val result3 = numberLowerTrimmed.EquaSet((1, "2", "3"), (4, "5", "6"), (7, "8", "9")).unzip3(number, lower, trimmed)
    result3 shouldBe (number.EquaSet(1, 4, 7), lower.EquaSet("2", "5", "8"), trimmed.EquaSet("3", "6", "9"))
    result3.shouldHaveExactType[(number.EquaSet, lower.EquaSet, trimmed.EquaSet)]

    val result4 = numberLowerTrimmed.FastEquaSet((1, "2", "3")).unzip3(number, lower, trimmed)
    result4 shouldBe (number.FastEquaSet(1), lower.FastEquaSet("2"), trimmed.FastEquaSet("3"))
    result4.shouldHaveExactType[(number.FastEquaSet, lower.FastEquaSet, trimmed.FastEquaSet)]

    val result5 = numberLowerTrimmed.FastEquaSet((1, "2", "3"), (4, "5", "6")).unzip3(number, lower, trimmed)
    result5 shouldBe (number.FastEquaSet(1, 4), lower.FastEquaSet("2", "5"), trimmed.FastEquaSet("3", "6"))
    result5.shouldHaveExactType[(number.FastEquaSet, lower.FastEquaSet, trimmed.FastEquaSet)]

    val result6 = numberLowerTrimmed.FastEquaSet((1, "2", "3"), (4, "5", "6"), (7, "8", "9")).unzip3(number, lower, trimmed)
    result6 shouldBe (number.FastEquaSet(1, 4, 7), lower.FastEquaSet("2", "5", "8"), trimmed.FastEquaSet("3", "6", "9"))
    result6.shouldHaveExactType[(number.FastEquaSet, lower.FastEquaSet, trimmed.FastEquaSet)]
  }
  it should "have 2 views method" in {
    number.EquaSet(3).view(0, 0).toList shouldBe List()
    number.EquaSet(1, 2, 3).view(2, 1).toList shouldBe List()
    number.EquaSet(1, 2, 3).view(1, 3).toList shouldBe List(number.EquaBox(2), number.EquaBox(3))
    number.EquaSet(1, 2, 3).view.toList shouldBe List(number.EquaBox(1), number.EquaBox(2), number.EquaBox(3))
  }
  it should "have a withFilter method" in {
    var a = 0
    var b = 0
    val set = number.EquaSet(1, 2, 3)
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

    val result1 = withFilter.map(_ * 2)
    result1 shouldBe number.EquaSet(4, 6)
    a shouldBe 9

    val result2 = withFilter.flatMap(e => number.EquaSet(e * 2, e * 3))
    result2 shouldBe number.EquaSet(4, 6, 9)
    a shouldBe 12
  }
  it should "have a zip method" in {
    number.EquaSet(1, 2, 3).zip(List("4", "5", "6")) shouldBe Set((1, "4"), (2, "5"), (3, "6"))
    number.EquaSet(1, 2, 3).zip(List("4", "5")) shouldBe Set((1, "4"), (2, "5"))
  }
  it should "have a zipAll method" in {
    number.EquaSet(1, 2, 3).zipAll(List("4", "5", "6"), 0, "0") shouldBe Set((1, "4"), (2, "5"), (3, "6"))
    number.EquaSet(1, 2, 3).zipAll(List("4", "5"), 0, "0") shouldBe Set((1, "4"), (2, "5"), (3, "0"))
    number.EquaSet(1, 2).zipAll(List("4", "5", "6"), 0, "0") shouldBe Set((1, "4"), (2, "5"), (0, "6"))
  }
  it should "have a zipWithIndex method" in {
    number.EquaSet(99).zipWithIndex shouldBe Set((99,0))
    number.EquaSet(1, 2, 3).zipWithIndex shouldBe Set((1,0), (2,1), (3,2))
  }
  it should "have an copyInto method" in {
    val equaSet = number.EquaSet(1, 2, 3)
    equaSet.copyInto(sortedNumber) shouldEqual sortedNumber.EquaSet(1, 2, 3)
    equaSet.copyInto(number) should be theSameInstanceAs equaSet
  }
  it should "have a filter method after it is converted into EquaBridge with into" in {
    val set = number.EquaSet(1, 2, 3)
    val fastSet = number.FastEquaSet(1, 2, 3)

    val bridge1 = set.into(lower)

    val result1 = bridge1.filter(_ == 1)
    result1.map(_.toString) shouldBe lower.EquaSet("1")
    result1.shouldHaveExactType[lower.EquaBridge[Int]]

    val result2 = for (i <- bridge1 if i == 1) yield i.toString
    result2 shouldBe lower.EquaSet("1")
    result2.shouldHaveExactType[lower.EquaSet]

    val bridge2 = fastSet.into(lower)

    val result3 = bridge2.filter(_ == 2)
    result3.map(_.toString) shouldBe lower.FastEquaSet("2")
    result3.shouldHaveExactType[lower.FastEquaBridge[Int]]

    val result4 = for (i <- bridge2 if i == 2) yield i.toString
    result4 shouldBe lower.FastEquaSet("2")
    result4.shouldHaveExactType[lower.FastEquaSet]
  }
  it should "have a withFilter method after it is converted into EquaBridge with into" in {
    val set = number.EquaSet(1, 2, 3)
    val fastSet = number.FastEquaSet(1, 2, 3)

    val bridge1 = set.into(lower)

    var count = 0
    val result1 = bridge1.withFilter { i =>
      count += 1
      i == 1
    }
    count shouldBe 0
    result1.map(_.toString) shouldBe lower.EquaSet("1")
    count shouldBe 3
    result1.shouldHaveExactType[bridge1.WithFilter]

    val bridge2 = fastSet.into(lower)

    val result2 = bridge2.withFilter { i =>
      count += 1
      i == 2
    }
    count shouldBe 3
    result2.map(_.toString) shouldBe lower.FastEquaSet("2")
    count shouldBe 6
    result2.shouldHaveExactType[bridge2.FastWithFilter]
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
}

