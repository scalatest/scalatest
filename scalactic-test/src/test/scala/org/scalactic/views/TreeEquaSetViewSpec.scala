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
import org.scalactic.SortedCollections
import org.scalactic.StringNormalizations
import org.scalactic.HashingEquality
import org.scalactic.OrderingEquality

class TreeSetViewSpec extends UnitSpec {

  def normalHashingEquality[T] =
    new HashingEquality[T] {
      def hashCodeFor(a: T): Int = a.hashCode
      def areEqual(a: T, b: Any): Boolean = a == b
    }
  val intEquality =
    new OrderingEquality[Int] {
      def hashCodeFor(a: Int): Int = a.hashCode
      def areEqual(a: Int, b: Any): Boolean = a == b
      def compare(a: Int, b: Int): Int = a - b
    }
  val trimmed = SortedCollections[String](StringNormalizations.trimmed.toOrderingEquality)
  val number = SortedCollections[Int](intEquality)

  "TreeSetView" should "offer a size method" in {
    TreeSetView(1, 2, 3).size shouldBe 3
    TreeSetView(1, 1, 3, 2).size shouldBe 4
    TreeSetView(1, 1, 1, 1).size shouldBe 4
  }
  it should "have a pretty toString" in {
    def assertPretty[T](sortedSetView: SortedSetView[T]) = {
      val lss = sortedSetView.toString
      lss should startWith ("TreeSetView(")
      lss should endWith (")")
      /*
      scala> val lss = "TreeSetView(1, 2, 3)"
      lss: String = TreeSetView()

      scala> lss.replaceAll(""".*\((.*)\).*""", "$1")
      res0: String = 1,2,3

      scala> res0.split(',')
      res1: Array[String] = Array(1, 2, 3)

      scala> val lss = "TreeSetView()"
      lss: String = TreeSetView()

      scala> lss.replaceAll(""".*\((.*)\).*""", "$1")
      res2: String = ""

      scala> res2.split(',')
      res3: Array[String] = Array("")
      */
      val elemStrings = lss.replaceAll(""".*\((.*)\).*""", "$1")
      val elemStrArr = if (elemStrings.size != 0) elemStrings.split(',') else Array.empty[String]
      elemStrArr.size should equal (sortedSetView.size)

      elemStrArr should contain theSameElementsAs sortedSetView.toStandardList.map(_.toString)
    }

    // Test BasicTreeSetView
    assertPretty(TreeSetView(1, 2, 3))
    assertPretty(TreeSetView(1, 2, 3, 4))
    assertPretty(TreeSetView(1))
    assertPretty(TreeSetView())
    assertPretty(TreeSetView("one", "two", "three", "four", "five"))

    // Test FlatMappedTreeSetView
    val trimmed = SortedCollections[String](StringNormalizations.trimmed.toOrderingEquality)
    val sortedSetView = trimmed.immutable.SortedSet("1", "2", "01", "3").view
    val flatMapped = sortedSetView.flatMap { (digit: String) =>
      TreeSetView(digit.toInt)
    }
    assertPretty(flatMapped)
    val mapped = flatMapped.map(_ + 1)
    assertPretty(mapped)
  }

  it should "have an unzip method" in {
    val zipped = TreeSetView(3, 1, 2, -3, 3).zip(TreeSetView("z", "a", "b", "c", "z"))
    val (intSeq, stringSeq) = zipped.unzip
    intSeq.toStandardList shouldBe TreeSetView(3, 1, 2, -3, 3).toStandardList
    stringSeq.toStandardList shouldBe TreeSetView("z", "a", "b", "c", "z").toStandardList
  }

  it should "have an unzip3 method" in {
    val tuples = List(
      ("a", 0.0, 3),
      ("b", 1.1, -3),
      ("c", 2.2, 0),
      ("z", -2.2, 0)
    )
    val (stringSeq, doubleSeq, intSeq) = TreeSetView(tuples: _*).unzip3
    stringSeq.toStandardList shouldBe TreeSetView("a", "b", "c", "z").toStandardList
    doubleSeq.toStandardList shouldBe TreeSetView(0.0, 1.1, 2.2, -2.2).toStandardList
    intSeq.toStandardList shouldBe TreeSetView(3, -3, 0, 0).toStandardList
  }

  it should "have a zip method" in {
    val seq1 = TreeSetView(1,2,3)
    val seq2 = TreeSetView("a", "b", "c")
    val zipped = seq1.zip(seq2)
    val (b1, b2) = zipped.toStandardList.unzip
    b1 shouldBe seq1.toStandardList
    b2 shouldBe seq2.toStandardList
  }

  it should "have a zipAll method" in {
    val shortSeq1 = TreeSetView(1,2,3)
    val longSeq1 = TreeSetView(1,2,3,4)
    val shortSeq2 = TreeSetView("a", "b", "c")
    val longSeq2 = TreeSetView("a", "b", "c", "d")

    def assertSameElements(thisSeq: TreeSetView[_], thatSeq: TreeSetView[_]): Unit = {
      val zipped = thisSeq.zipAll(thatSeq, 4, "d")
      val (unzip1, unzip2) = zipped.toStandardList.unzip
      unzip1 shouldBe longSeq1.toStandardList
      unzip2 shouldBe longSeq2.toStandardList
    }
    assertSameElements(shortSeq1, longSeq2)
    assertSameElements(longSeq1, shortSeq2)
    assertSameElements(longSeq1, longSeq2)
  }

  it should "have a zipWithIndex method" in {
    val bag = TreeSetView("a", "b", "c")
    val zipped = bag.zipWithIndex
    val (b1, b2) = zipped.toStandardList.unzip
    b1 shouldBe bag.toStandardList
    b2 shouldBe List(0, 1, 2)
  }

  it should "have a collect method" in {
    val seq = TreeSetView(1, 2, 3, 4, 5)
    val doubledOdds = seq.collect {
      case n: Int if n % 2 == 1 => n * 2
    }
    doubledOdds.toStandardList shouldBe TreeSetView(2, 6, 10).toStandardList
    val noMatch = seq.collect { case n: Int if n < 0 => n }
    noMatch.toStandardList shouldBe empty
  }

  it should "have a scan method" in {
    val seq = TreeSetView(1, 2, 3, 4, 5)
    val scanned = seq.scan(0)(_+_)
    scanned.toStandardList shouldBe TreeSetView(0, 1, 3, 6, 10, 15).toStandardList
  }

  it should "have a scanLeft method" in {
    val seq = TreeSetView(1, 2, 3, 4, 5)
    val scanned = seq.scanLeft(0)(_+_)
    scanned.toStandardList shouldBe TreeSetView(0, 1, 3, 6, 10, 15).toStandardList
  }

  it should "have a scanRight method" in {
    val seq = TreeSetView(1, 2, 3, 4, 5)
    val scanned = seq.scanRight(0)(_+_)
    scanned.toStandardList shouldBe TreeSetView(15, 14, 12, 9, 5, 0).toStandardList
  }

  it should "offer a force method that returns a SortedSet" in {
    val setView = trimmed.immutable.TreeSet("1", "2", "01", "3").view
    val flatMapped = setView.flatMap { (digit: String) =>
      FastSetView(digit.toInt)
    }
    val strictSet = flatMapped.force(number)
    strictSet should equal (number.immutable.TreeSet(1, 2, 3))
  }
}

