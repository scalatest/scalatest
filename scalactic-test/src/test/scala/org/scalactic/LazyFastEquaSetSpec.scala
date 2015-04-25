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

class LazyFastEquaSetSpec extends UnitSpec {

  def normalHashingEquality[T] =
    new HashingEquality[T] {
      def hashCodeFor(a: T): Int = a.hashCode
      def areEqual(a: T, b: Any): Boolean = a == b
    }
  val trimmed = EquaPath[String](StringNormalizations.trimmed.toHashingEquality)
  val number = EquaPath[Int](normalHashingEquality[Int])

  "LazyFastEquaSet" should "offer a size method" in {
    LazyFastEquaSet(1, 2, 3).size shouldBe 3
    LazyFastEquaSet(1, 1, 3, 2).size shouldBe 4
    LazyFastEquaSet(1, 1, 1, 1).size shouldBe 4
  }
  it should "have a pretty toString" in {
    def assertPretty[T](lazyBag: LazyEquaSet[T]) = {
      val lbs = lazyBag.toString
      lbs should startWith ("LazyFastEquaSet(")
      lbs should endWith (")")
      /*
      scala> lbs.replaceAll(""".*\((.*)\).*""", "$1")
      res0: String = 1,2,3

      scala> res0.split(',')
      res1: Array[String] = Array(1, 2, 3)

      scala> val lbs = "LazyFastEquaSet()"
      lbs: String = LazyFastEquaSet()

      scala> lbs.replaceAll(""".*\((.*)\).*""", "$1")
      res2: String = ""

      scala> res2.split(',')
      res3: Array[String] = Array("")
      */
      val elemStrings = lbs.replaceAll(""".*\((.*)\).*""", "$1")
      val elemStrArr = if (elemStrings.size != 0) elemStrings.split(',') else Array.empty[String]
      elemStrArr.size should equal (lazyBag.size)

      elemStrArr should contain theSameElementsAs lazyBag.toList.map(_.toString)
    }

    // Test BasicLazyFastEquaSet
    assertPretty(LazyFastEquaSet(1, 2, 3))
    assertPretty(LazyFastEquaSet(1, 2, 3, 4))
    assertPretty(LazyFastEquaSet(1))
    assertPretty(LazyFastEquaSet())
    assertPretty(LazyFastEquaSet("one", "two", "three", "four", "five"))

    // Test FlatMappedLazyFastEquaSet
    val trimmed = EquaPath[String](StringNormalizations.trimmed.toHashingEquality)
    val lazyBag = trimmed.EquaSet("1", "2", "01", "3").toLazy
    val flatMapped = lazyBag.flatMap { (digit: String) =>
      LazyFastEquaSet(digit.toInt)
    }
    assertPretty(flatMapped)
    val mapped = flatMapped.map(_ + 1)
    assertPretty(mapped)
  }

  it should "have an unzip method" in {
    val zipped = LazyFastEquaSet(3, 1, 2, -3, 3).zip(LazyFastEquaSet("z", "a", "b", "c", "z"))
    val (intBag, stringBag) = zipped.unzip
    intBag.toList should contain theSameElementsAs LazyFastEquaSet(3, -3, 3, 2, 1).toList
    stringBag.toList should contain theSameElementsAs LazyFastEquaSet("z", "z", "a", "b", "c").toList
  }

  it should "have an unzip3 method" in {
    val tuples = List(
      ("a", 0.0, 3),
      ("b", 1.1, -3),
      ("c", 2.2, 0),
      ("z", -2.2, 0)
    )
    val (stringBag, doubleBag, intBag) = LazyFastEquaSet(tuples: _*).unzip3
    stringBag.toList should contain theSameElementsAs LazyFastEquaSet("z", "a", "b", "c").toList
    doubleBag.toList should contain theSameElementsAs LazyFastEquaSet(-2.2, 0.0, 1.1, 2.2).toList
    intBag.toList should contain theSameElementsAs LazyFastEquaSet(0, 3, -3, 0).toList
  }

  it should "have a zip method" in {
    val bag1 = LazyFastEquaSet(1,2,3)
    val bag2 = LazyFastEquaSet("a", "b", "c")
    val zipped = bag1.zip(bag2)
    val (b1, b2) = zipped.toList.unzip
    b1 should contain theSameElementsAs bag1.toList
    b2 should contain theSameElementsAs bag2.toList
  }

  it should "have a zipAll method" in {
    val shortBag1 = LazyFastEquaSet(1,2,3)
    val longBag1 = LazyFastEquaSet(1,2,3,4)
    val shortBag2 = LazyFastEquaSet("a", "b", "c")
    val longBag2 = LazyFastEquaSet("a", "b", "c", "d")

    def assertSameElements(thisBag: LazyFastEquaSet[_], thatBag: LazyFastEquaSet[_]): Unit = {
      val zipped = thisBag.zipAll(thatBag, 4, "d")
      val (unzip1, unzip2) = zipped.toList.unzip
      unzip1 should contain theSameElementsAs longBag1.toList
      unzip2 should contain theSameElementsAs longBag2.toList
    }
    assertSameElements(shortBag1, longBag2)
    assertSameElements(longBag1, shortBag2)
    assertSameElements(longBag1, longBag2)
  }

  it should "have a zipWithIndex method" in {
    val bag = LazyFastEquaSet("a", "b", "c")
    val zipped = bag.zipWithIndex
    val (b1, b2) = zipped.toList.unzip
    b1 should contain theSameElementsAs bag.toList
    b2 should contain theSameElementsAs List(0, 1, 2)
  }

  it should "have a collect method" in {
    val bag = LazyFastEquaSet(1, 2, 3, 4, 5)
    val doubledOdds = bag.collect {
      case n: Int if n % 2 == 1 => n * 2
    }
    doubledOdds.toList should contain theSameElementsAs LazyFastEquaSet(2, 6, 10).toList
    val noMatch = bag.collect { case n: Int if n < 0 => n }
    noMatch.toList shouldBe empty
  }

  it should "have a scan method" in {
    val bag = LazyFastEquaSet(1, 2, 3, 4, 5)
    val scanned = bag.scan(0)(_+_)
    scanned.toList should contain theSameElementsAs LazyFastEquaSet(0, 1, 3, 6, 10, 15).toList
  }

  it should "have a scanLeft method" in {
    val bag = LazyFastEquaSet(1, 2, 3, 4, 5)
    val scanned = bag.scanLeft(0)(_+_)
    scanned.toList should contain theSameElementsAs LazyFastEquaSet(0, 1, 3, 6, 10, 15).toList
  }

  it should "have a scanRight method" in {
    val bag = LazyFastEquaSet(1, 2, 3, 4, 5)
    val scanned = bag.scanRight(0)(_+_)
    scanned.toList should contain theSameElementsAs LazyFastEquaSet(0, 5, 9, 12, 14, 15).toList
  }

  it should "offer a toStrict method that returns a FastEquaSet" in {
    val lazySet = trimmed.FastEquaSet("1", "2", "01", "3").toLazy
    val flatMapped = lazySet.flatMap { (digit: String) =>
      LazyFastEquaSet(digit.toInt)
    }
    val strictSet = flatMapped.toStrict(number)
    strictSet should equal (number.FastEquaSet(1, 2, 3))
  }
}

