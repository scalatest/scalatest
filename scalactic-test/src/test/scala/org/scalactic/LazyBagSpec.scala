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

class LazyBagSpec extends UnitSpec {
  "LazyBag" should "offer a size method" in {
    LazyBag(1, 2, 3).size shouldBe 3
    LazyBag(1, 1, 3, 2).size shouldBe 4
    LazyBag(1, 1, 1, 1).size shouldBe 4
  }
  it should "have a pretty toString" in {
    def assertPretty[T](lazyBag: LazyBag[T]) = {
      val lbs = lazyBag.toString
      lbs should startWith ("LazyBag(")
      lbs should endWith (")")
      /*
      scala> lbs.replaceAll(""".*\((.*)\).*""", "$1")
      res0: String = 1,2,3

      scala> res0.split(',')
      res1: Array[String] = Array(1, 2, 3)

      scala> val lbs = "LazyBag()"
      lbs: String = LazyBag()

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

    // Test BasicLazyBag
    assertPretty(LazyBag(1, 2, 3))
    assertPretty(LazyBag(1, 2, 3, 4))
    assertPretty(LazyBag(1))
    assertPretty(LazyBag())
    assertPretty(LazyBag("one", "two", "three", "four", "five"))

    // Test FlatMappedLazyBag
    val trimmed = EquaPath[String](StringNormalizations.trimmed.toHashingEquality)
    val lazyBag = trimmed.EquaSet("1", "2", "01", "3").toLazy
    val flatMapped = lazyBag.flatMap { (digit: String) =>
      LazyBag(digit.toInt)
    }
    assertPretty(flatMapped)
    val mapped = flatMapped.map(_ + 1)
    assertPretty(mapped)
  }

  it should "have a zip method" in {
    val bag1 = LazyBag(1,2,3)
    val bag2 = LazyBag("a", "b", "c")
    val zipped = bag1.zip(bag2)
    val (b1, b2) = zipped.toList.unzip
    b1 should contain theSameElementsAs bag1.toList
    b2 should contain theSameElementsAs bag2.toList
  }

  it should "have a zipAll method" in {
    val shortBag1 = LazyBag(1,2,3)
    val longBag1 = LazyBag(1,2,3,4)
    val shortBag2 = LazyBag("a", "b", "c")
    val longBag2 = LazyBag("a", "b", "c", "d")

    def assertSameElements(thisBag: LazyBag[_], thatBag: LazyBag[_]): Unit = {
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
    val bag = LazyBag("a", "b", "c")
    val zipped = bag.zipWithIndex
    val (b1, b2) = zipped.toList.unzip
    b1 should contain theSameElementsAs bag.toList
    b2 should contain theSameElementsAs List(0, 1, 2)
  }
}

