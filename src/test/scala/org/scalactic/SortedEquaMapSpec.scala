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

class SortedEquaMapSpec extends UnitSpec {

  implicit class HasExactType[T](o: T) {
    def shouldHaveExactType[U](implicit ev: T =:= U): Unit = ()
  }

  val lower = SortedEquaPath[String](StringNormalizations.lowerCased.toOrderingEquality)

  "An SortedEquaMap" can "be constructed with empty" in {
    val emptyMap = lower.SortedEquaMap.empty
    emptyMap shouldBe empty

    val treeEmptyMap = lower.TreeEquaMap.empty
    treeEmptyMap shouldBe empty

    lower.SortedEquaSet.empty.shouldHaveExactType[lower.SortedEquaSet]
    lower.TreeEquaSet.empty.shouldHaveExactType[lower.TreeEquaSet]
  }

  it can "be constructed with apply" in {
    val result1 = lower.SortedEquaMap("one" -> 1, "two" -> 2, "three" -> 3)
    result1 should have size 3
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.TreeEquaMap("one" -> 1, "two" -> 2, "three" -> 3)
    result2 should have size 3
    result2.shouldHaveExactType[lower.TreeEquaMap[Int]]
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedEquaSet.
  }

  it should "construct only entries with appropriate key types" in {
    "lower.SortedEquaMap(1 -> \"one\", 2 -> \"two\", 3 -> \"three\")" shouldNot compile
    "lower.TreeEquaMap(1 -> \"one\", 2 -> \"two\", 3 -> \"three\")" shouldNot compile
  }

  it should "eliminate 'duplicate' entries passed to the apply factory method" in {
    val result1 = lower.SortedEquaMap("one" -> 1, "two" -> 22, "two" -> 2, "three" -> 33, "Three" -> 3)
    result1 should have size 3
    result1 shouldBe lower.SortedEquaMap("one" -> 1, "two" -> 2, "Three" -> 3)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.TreeEquaMap("one" -> 1, "two" -> 22, "two" -> 2, "three" -> 33, "Three" -> 3)
    result2 should have size 3
    result2 shouldBe lower.TreeEquaMap("one" -> 1, "two" -> 2, "Three" -> 3)
    result2.shouldHaveExactType[lower.TreeEquaMap[Int]]
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedEquaSet.
  }

  it should "have a toString method" in {
    lower.SortedEquaMap("hi" -> 1, "ho" -> 2).toString shouldBe "TreeEquaMap(hi -> 1, ho -> 2)"
    lower.TreeEquaMap("hi" -> 1, "ho" -> 2).toString shouldBe "TreeEquaMap(hi -> 1, ho -> 2)"
  }

  it should "have a toMap method" in {
    lower.SortedEquaMap("hi" -> 1, "ho" -> 2).toMap shouldBe (Map("hi" -> 1, "ho" -> 2))
  }

  it should "have a toEquaBoxMap method" in {
    lower.SortedEquaMap("hi" -> 1, "ho" -> 2).toEquaBoxMap shouldBe (Map(lower.EquaBox("hi") -> 1, lower.EquaBox("ho") -> 2))
  }

  it should "have a + method that takes one argument" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3)
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3)
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "HO" -> 3)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3)
    result3 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3)
    result3.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result4 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3)
    result4 shouldBe lower.TreeEquaMap("hi" -> 1, "HO" -> 3)
    result4.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a + method that takes two or more arguments" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3, "hey!" -> 4)
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3, "hey!" -> 4)
    result3 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result3.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result4 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4 shouldBe lower.TreeEquaMap("hi" -> 1, "HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a - method that takes one argument" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - "ha"
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) - "HO"
    result2 shouldBe lower.SortedEquaMap("hi" -> 1)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) - "who?"
    result3 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result4 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - "ha"
    result4 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result4.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result5 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) - "HO"
    result5 shouldBe lower.TreeEquaMap("hi" -> 1)
    result5.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result6 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) - "who?"
    result6 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result6.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }
}