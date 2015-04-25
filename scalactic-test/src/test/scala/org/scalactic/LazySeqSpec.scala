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

class LazySeqSpec extends UnitSpec {
  "LazySeq" should "offer a size method" in {
    LazySeq(1, 2, 3).size shouldBe 3
    LazySeq(1, 1, 3, 2).size shouldBe 4
    LazySeq(1, 1, 1, 1).size shouldBe 4
  }
  it should "have a pretty toString" in {
    def assertPretty[T](lazySeq: LazySeq[T]) = {
      val lss = lazySeq.toString
      lss should startWith ("LazySeq(")
      lss should endWith (")")
      /*
      scala> val lss = "LazySeq(1, 2, 3)"
      lss: String = LazySeq()

      scala> lss.replaceAll(""".*\((.*)\).*""", "$1")
      res0: String = 1,2,3

      scala> res0.split(',')
      res1: Array[String] = Array(1, 2, 3)

      scala> val lss = "LazySeq()"
      lss: String = LazySeq()

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

    // Test BasicLazySeq
    assertPretty(LazySeq(1, 2, 3))
    assertPretty(LazySeq(1, 2, 3, 4))
    assertPretty(LazySeq(1))
    assertPretty(LazySeq())
    assertPretty(LazySeq("one", "two", "three", "four", "five"))

    // Test FlatMappedLazySeq
    val trimmed = SortedEquaPath[String](StringNormalizations.trimmed.toOrderingEquality)
    val lazySeq = trimmed.SortedEquaSet("1", "2", "01", "3").toLazy
    val flatMapped = lazySeq.flatMap { (digit: String) =>
      LazySeq(digit.toInt)
    }
    assertPretty(flatMapped)
    val mapped = flatMapped.map(_ + 1)
    assertPretty(mapped)
  }

  it should "have an unzip method" in {
    val zipped = LazySeq(3, 1, 2, -3, 3).zip(LazySeq("z", "a", "b", "c", "z"))
    val (intSeq, stringSeq) = zipped.unzip
    intSeq.toList shouldBe LazySeq(3, 1, 2, -3, 3).toList
    stringSeq.toList shouldBe LazySeq("z", "a", "b", "c", "z").toList
  }

  it should "have an unzip3 method" in {
    val tuples = List(
      ("a", 0.0, 3),
      ("b", 1.1, -3),
      ("c", 2.2, 0),
      ("z", -2.2, 0)
    )
    val (stringSeq, doubleSeq, intSeq) = LazySeq(tuples: _*).unzip3
    stringSeq.toList shouldBe LazySeq("a", "b", "c", "z").toList
    doubleSeq.toList shouldBe LazySeq(0.0, 1.1, 2.2, -2.2).toList
    intSeq.toList shouldBe LazySeq(3, -3, 0, 0).toList
    val (stringEmpty, doubleEmpty, intEmpty) = LazySeq[(String,Double,Int)]().unzip3
    stringEmpty.toList shouldBe empty
    doubleEmpty.toList shouldBe empty
    intEmpty.toList shouldBe empty
  }

  it should "have a zip method" in {
    val seq1 = LazySeq(1,2,3)
    val seq2 = LazySeq("a", "b", "c")
    val zipped = seq1.zip(seq2)
    val (b1, b2) = zipped.toList.unzip
    b1 shouldBe seq1.toList
    b2 shouldBe seq2.toList
  }

  it should "have a zipAll method" in {
    val shortSeq1 = LazySeq(1,2,3)
    val longSeq1 = LazySeq(1,2,3,4)
    val shortSeq2 = LazySeq("a", "b", "c")
    val longSeq2 = LazySeq("a", "b", "c", "d")

    def assertSameElements(thisSeq: LazySeq[_], thatSeq: LazySeq[_]): Unit = {
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
    val bag = LazySeq("a", "b", "c")
    val zipped = bag.zipWithIndex
    val (b1, b2) = zipped.toList.unzip
    b1 shouldBe bag.toList
    b2 shouldBe List(0, 1, 2)
    LazySeq[String]().toList.zipWithIndex shouldBe List.empty[(List[String], List[Int])]
  }

  it should "have a collect method" in {
    val doubledOdds: PartialFunction[Int, Int] = { case n: Int if n % 2 == 1 => n * 2 }
    LazySeq[Int]().collect(doubledOdds).toList shouldBe empty
    LazySeq(1).collect(doubledOdds).toList shouldBe List(2)
    LazySeq(1, 2).collect(doubledOdds).toList shouldBe List(2)
    LazySeq(1, 2, 3).collect(doubledOdds).toList shouldBe List(2, 6)
    LazySeq(1, 2, 3, 4).collect(doubledOdds).toList shouldBe List(2, 6)
    LazySeq(1, 2, 3, 4, 5).collect(doubledOdds).toList shouldBe List(2, 6, 10)
  }

  it should "have a scan method" in {
    LazySeq[Int]().scan(0)(_+_).toList should contain theSameElementsAs List(0)
    LazySeq(1).scan(0)(_ + _).toList should contain theSameElementsAs List(0, 1)
    LazySeq(1, 2).scan(0)(_ + _).toList should contain theSameElementsAs List(0, 1, 3)
    LazySeq(1, 2, 3).scan(0)(_ + _).toList should contain theSameElementsAs List(0, 1, 3, 6)
    LazySeq(1, 2, 3, 4).scan(0)(_+_).toList should contain theSameElementsAs List(0, 1, 3, 6, 10)
    LazySeq(1, 2, 3, 4, 5).scan(0)(_+_).toList should contain theSameElementsAs List(0, 1, 3, 6, 10, 15)
  }

  it should "have a scanLeft method" in {
    LazySeq[Int]().scanLeft(0)(_+_).toList shouldBe List(0)
    LazySeq(1).scanLeft(0)(_ + _).toList shouldBe List(0, 1)
    LazySeq(1, 2).scanLeft(0)(_ + _).toList shouldBe List(0, 1, 3)
    LazySeq(1, 2, 3).scanLeft(0)(_ + _).toList shouldBe List(0, 1, 3, 6)
    LazySeq(1, 2, 3, 4).scanLeft(0)(_+_).toList shouldBe List(0, 1, 3, 6, 10)
    LazySeq(1, 2, 3, 4, 5).scanLeft(0)(_+_).toList shouldBe List(0, 1, 3, 6, 10, 15)
  }

  it should "have a scanRight method" in {
    LazySeq[Int]().scanRight(0)(_+_).toList shouldBe List(0)
    LazySeq(1).scanRight(0)(_ + _).toList shouldBe List(1, 0)
    LazySeq(1, 2).scanRight(0)(_ + _).toList shouldBe List(3, 2, 0)
    LazySeq(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
    LazySeq(1, 2, 3, 4).scanRight(0)(_+_).toList shouldBe List(10, 9, 7, 4, 0)
    LazySeq(1, 2, 3, 4, 5).scanRight(0)(_+_).toList shouldBe List(15, 14, 12, 9, 5, 0)
  }
}

