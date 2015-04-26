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
package org.scalactic

class TreeEquaSetViewSpec extends UnitSpec {

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
  val trimmed = SortedEquaPath[String](StringNormalizations.trimmed.toOrderingEquality)
  val number = SortedEquaPath[Int](intEquality)

  "TreeEquaSetView" should "offer a size method" in {
    TreeEquaSetView(1, 2, 3).size shouldBe 3
    TreeEquaSetView(1, 1, 3, 2).size shouldBe 4
    TreeEquaSetView(1, 1, 1, 1).size shouldBe 4
  }
  it should "have a pretty toString" in {
    def assertPretty[T](lazySeq: SortedEquaSetView[T]) = {
      val lss = lazySeq.toString
      lss should startWith ("TreeEquaSetView(")
      lss should endWith (")")
      /*
      scala> val lss = "TreeEquaSetView(1, 2, 3)"
      lss: String = TreeEquaSetView()

      scala> lss.replaceAll(""".*\((.*)\).*""", "$1")
      res0: String = 1,2,3

      scala> res0.split(',')
      res1: Array[String] = Array(1, 2, 3)

      scala> val lss = "TreeEquaSetView()"
      lss: String = TreeEquaSetView()

      scala> lss.replaceAll(""".*\((.*)\).*""", "$1")
      res2: String = ""

      scala> res2.split(',')
      res3: Array[String] = Array("")
      */
      val elemStrings = lss.replaceAll(""".*\((.*)\).*""", "$1")
      val elemStrArr = if (elemStrings.size != 0) elemStrings.split(',') else Array.empty[String]
      elemStrArr.size should equal (lazySeq.size)

      elemStrArr should contain theSameElementsAs lazySeq.toList.map(_.toString)
    }

    // Test BasicTreeEquaSetView
    assertPretty(TreeEquaSetView(1, 2, 3))
    assertPretty(TreeEquaSetView(1, 2, 3, 4))
    assertPretty(TreeEquaSetView(1))
    assertPretty(TreeEquaSetView())
    assertPretty(TreeEquaSetView("one", "two", "three", "four", "five"))

    // Test FlatMappedTreeEquaSetView
    val trimmed = SortedEquaPath[String](StringNormalizations.trimmed.toOrderingEquality)
    val lazySeq = trimmed.SortedEquaSet("1", "2", "01", "3").view
    val flatMapped = lazySeq.flatMap { (digit: String) =>
      TreeEquaSetView(digit.toInt)
    }
    assertPretty(flatMapped)
    val mapped = flatMapped.map(_ + 1)
    assertPretty(mapped)
  }

  it should "have an unzip method" in {
    val zipped = TreeEquaSetView(3, 1, 2, -3, 3).zip(TreeEquaSetView("z", "a", "b", "c", "z"))
    val (intSeq, stringSeq) = zipped.unzip
    intSeq.toList shouldBe TreeEquaSetView(3, 1, 2, -3, 3).toList
    stringSeq.toList shouldBe TreeEquaSetView("z", "a", "b", "c", "z").toList
  }

  it should "have an unzip3 method" in {
    val tuples = List(
      ("a", 0.0, 3),
      ("b", 1.1, -3),
      ("c", 2.2, 0),
      ("z", -2.2, 0)
    )
    val (stringSeq, doubleSeq, intSeq) = TreeEquaSetView(tuples: _*).unzip3
    stringSeq.toList shouldBe TreeEquaSetView("a", "b", "c", "z").toList
    doubleSeq.toList shouldBe TreeEquaSetView(0.0, 1.1, 2.2, -2.2).toList
    intSeq.toList shouldBe TreeEquaSetView(3, -3, 0, 0).toList
  }

  it should "have a zip method" in {
    val seq1 = TreeEquaSetView(1,2,3)
    val seq2 = TreeEquaSetView("a", "b", "c")
    val zipped = seq1.zip(seq2)
    val (b1, b2) = zipped.toList.unzip
    b1 shouldBe seq1.toList
    b2 shouldBe seq2.toList
  }

  it should "have a zipAll method" in {
    val shortSeq1 = TreeEquaSetView(1,2,3)
    val longSeq1 = TreeEquaSetView(1,2,3,4)
    val shortSeq2 = TreeEquaSetView("a", "b", "c")
    val longSeq2 = TreeEquaSetView("a", "b", "c", "d")

    def assertSameElements(thisSeq: TreeEquaSetView[_], thatSeq: TreeEquaSetView[_]): Unit = {
      val zipped = thisSeq.zipAll(thatSeq, 4, "d")
      val (unzip1, unzip2) = zipped.toList.unzip
      unzip1 shouldBe longSeq1.toList
      unzip2 shouldBe longSeq2.toList
    }
    assertSameElements(shortSeq1, longSeq2)
    assertSameElements(longSeq1, shortSeq2)
    assertSameElements(longSeq1, longSeq2)
  }

  it should "have a zipWithIndex method" in {
    val bag = TreeEquaSetView("a", "b", "c")
    val zipped = bag.zipWithIndex
    val (b1, b2) = zipped.toList.unzip
    b1 shouldBe bag.toList
    b2 shouldBe List(0, 1, 2)
  }

  it should "have a collect method" in {
    val seq = TreeEquaSetView(1, 2, 3, 4, 5)
    val doubledOdds = seq.collect {
      case n: Int if n % 2 == 1 => n * 2
    }
    doubledOdds.toList shouldBe TreeEquaSetView(2, 6, 10).toList
    val noMatch = seq.collect { case n: Int if n < 0 => n }
    noMatch.toList shouldBe empty
  }

  it should "have a scan method" in {
    val seq = TreeEquaSetView(1, 2, 3, 4, 5)
    val scanned = seq.scan(0)(_+_)
    scanned.toList shouldBe TreeEquaSetView(0, 1, 3, 6, 10, 15).toList
  }

  it should "have a scanLeft method" in {
    val seq = TreeEquaSetView(1, 2, 3, 4, 5)
    val scanned = seq.scanLeft(0)(_+_)
    scanned.toList shouldBe TreeEquaSetView(0, 1, 3, 6, 10, 15).toList
  }

  it should "have a scanRight method" in {
    val seq = TreeEquaSetView(1, 2, 3, 4, 5)
    val scanned = seq.scanRight(0)(_+_)
    scanned.toList shouldBe TreeEquaSetView(15, 14, 12, 9, 5, 0).toList
  }

  it should "offer a force method that returns a SortedEquaSet" in {
    val lazySet = trimmed.TreeEquaSet("1", "2", "01", "3").view
    val flatMapped = lazySet.flatMap { (digit: String) =>
      FastEquaSetView(digit.toInt)
    }
    val strictSet = flatMapped.force(number)
    strictSet should equal (number.TreeEquaSet(1, 2, 3))
  }
}

