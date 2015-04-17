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
}

