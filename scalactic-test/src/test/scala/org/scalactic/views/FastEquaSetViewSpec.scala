/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalactic.views

import org.scalactic.UnitSpec
import org.scalactic.Collections
import org.scalactic.StringNormalizations
import org.scalactic.HashingEquality

class FastSetViewSpec extends UnitSpec {

  def normalHashingEquality[T] =
    new HashingEquality[T] {
      def hashCodeFor(a: T): Int = a.hashCode
      def areEqual(a: T, b: Any): Boolean = a == b
    }
  val trimmed = Collections[String](StringNormalizations.trimmed.toHashingEquality)
  val number = Collections[Int](normalHashingEquality[Int])

  "FastSetView" should "offer a size method" in {
    FastSetView(1, 2, 3).size shouldBe 3
    FastSetView(1, 1, 3, 2).size shouldBe 4
    FastSetView(1, 1, 1, 1).size shouldBe 4
  }
  it should "have a pretty toString" in {
    def assertPretty[T](equaSetView: SetView[T]) = {
      val lbs = equaSetView.toString
      lbs should startWith ("FastSetView(")
      lbs should endWith (")")
      /*
      scala> lbs.replaceAll(""".*\((.*)\).*""", "$1")
      res0: String = 1,2,3

      scala> res0.split(',')
      res1: Array[String] = Array(1, 2, 3)

      scala> val lbs = "FastSetView()"
      lbs: String = FastSetView()

      scala> lbs.replaceAll(""".*\((.*)\).*""", "$1")
      res2: String = ""

      scala> res2.split(',')
      res3: Array[String] = Array("")
      */
      val elemStrings = lbs.replaceAll(""".*\((.*)\).*""", "$1")
      val elemStrArr = if (elemStrings.size != 0) elemStrings.split(',') else Array.empty[String]
      elemStrArr.size should equal (equaSetView.size)

      elemStrArr should contain theSameElementsAs equaSetView.toStandardList.map(_.toString)
    }

    // Test BasicFastSetView
    assertPretty(FastSetView(1, 2, 3))
    assertPretty(FastSetView(1, 2, 3, 4))
    assertPretty(FastSetView(1))
    assertPretty(FastSetView())
    assertPretty(FastSetView("one", "two", "three", "four", "five"))

    // Test FlatMappedFastSetView
    val trimmed = Collections[String](StringNormalizations.trimmed.toHashingEquality)
    val equaSetView = trimmed.immutable.Set("1", "2", "01", "3").view
    val flatMapped = equaSetView.flatMap { (digit: String) =>
      FastSetView(digit.toInt)
    }
    assertPretty(flatMapped)
    val mapped = flatMapped.map(_ + 1)
    assertPretty(mapped)
  }

  it should "have an unzip method" in {
    val zipped = FastSetView(3, 1, 2, -3, 3).zip(FastSetView("z", "a", "b", "c", "z"))
    val (intBag, stringBag) = zipped.unzip
    intBag.toStandardList should contain theSameElementsAs FastSetView(3, -3, 3, 2, 1).toStandardList
    stringBag.toStandardList should contain theSameElementsAs FastSetView("z", "z", "a", "b", "c").toStandardList
  }

  it should "have an unzip3 method" in {
    val tuples = List(
      ("a", 0.0, 3),
      ("b", 1.1, -3),
      ("c", 2.2, 0),
      ("z", -2.2, 0)
    )
    val (stringBag, doubleBag, intBag) = FastSetView(tuples: _*).unzip3
    stringBag.toStandardList should contain theSameElementsAs FastSetView("z", "a", "b", "c").toStandardList
    doubleBag.toStandardList should contain theSameElementsAs FastSetView(-2.2, 0.0, 1.1, 2.2).toStandardList
    intBag.toStandardList should contain theSameElementsAs FastSetView(0, 3, -3, 0).toStandardList
  }

  it should "have a zip method" in {
    val bag1 = FastSetView(1,2,3)
    val bag2 = FastSetView("a", "b", "c")
    val zipped = bag1.zip(bag2)
    val (b1, b2) = zipped.toStandardList.unzip
    b1 should contain theSameElementsAs bag1.toStandardList
    b2 should contain theSameElementsAs bag2.toStandardList
  }

  it should "have a zipAll method" in {
    val shortBag1 = FastSetView(1,2,3)
    val longBag1 = FastSetView(1,2,3,4)
    val shortBag2 = FastSetView("a", "b", "c")
    val longBag2 = FastSetView("a", "b", "c", "d")

    def assertSameElements(thisBag: FastSetView[_], thatBag: FastSetView[_]): Unit = {
      val zipped = thisBag.zipAll(thatBag, 4, "d")
      val (unzip1, unzip2) = zipped.toStandardList.unzip
      unzip1 should contain theSameElementsAs longBag1.toStandardList
      unzip2 should contain theSameElementsAs longBag2.toStandardList
    }
    assertSameElements(shortBag1, longBag2)
    assertSameElements(longBag1, shortBag2)
    assertSameElements(longBag1, longBag2)
  }

  it should "have a zipWithIndex method" in {
    val bag = FastSetView("a", "b", "c")
    val zipped = bag.zipWithIndex
    val (b1, b2) = zipped.toStandardList.unzip
    b1 should contain theSameElementsAs bag.toStandardList
    b2 should contain theSameElementsAs List(0, 1, 2)
  }

  it should "have a collect method" in {
    val bag = FastSetView(1, 2, 3, 4, 5)
    val doubledOdds = bag.collect {
      case n: Int if n % 2 == 1 => n * 2
    }
    doubledOdds.toStandardList should contain theSameElementsAs FastSetView(2, 6, 10).toStandardList
    val noMatch = bag.collect { case n: Int if n < 0 => n }
    noMatch.toStandardList shouldBe empty
  }

  it should "have a scan method" in {
    val bag = FastSetView(1, 2, 3, 4, 5)
    val scanned = bag.scan(0)(_+_)
    scanned.toStandardList should contain theSameElementsAs FastSetView(0, 1, 3, 6, 10, 15).toStandardList
  }

  it should "have a scanLeft method" in {
    val bag = FastSetView(1, 2, 3, 4, 5)
    val scanned = bag.scanLeft(0)(_+_)
    scanned.toStandardList should contain theSameElementsAs FastSetView(0, 1, 3, 6, 10, 15).toStandardList
  }

  it should "have a scanRight method" in {
    val bag = FastSetView(1, 2, 3, 4, 5)
    val scanned = bag.scanRight(0)(_+_)
    scanned.toStandardList should contain theSameElementsAs FastSetView(0, 5, 9, 12, 14, 15).toStandardList
  }

  it should "offer a forceInto method that returns a FastSet" in {
    val setView = trimmed.immutable.FastSet("1", "2", "01", "3").view
    val flatMapped = setView.flatMap { (digit: String) =>
      FastSetView(digit.toInt)
    }
    val strictSet = flatMapped.forceInto(number)
    strictSet should equal (number.immutable.FastSet(1, 2, 3))
  }
}

