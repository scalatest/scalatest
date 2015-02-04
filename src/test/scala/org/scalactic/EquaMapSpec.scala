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

class EquaMapSpec extends UnitSpec {
  implicit class HasExactType[T](o: T) {
    def shouldHaveExactType[U](implicit ev: T =:= U): Unit = ()
  }
  val lower = EquaPath[String](StringNormalizations.lowerCased.toHashingEquality)

  "An EquaMap" can "be constructed with empty" in {
    val emptyMap = lower.EquaMap.empty
    emptyMap shouldBe empty

    val fastEmptyMap = lower.FastEquaMap.empty
    fastEmptyMap shouldBe empty
  }

  it can "be constructed with apply" in {
    val nonEmptyMap = lower.EquaMap("one" -> 1, "two" -> 2, "three" -> 3)
    nonEmptyMap should have size 3
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for EquaMap.
  }

  it should "construct only sets with appropriate element types" in {
    "lower.EquaMap(1 -> \"one\", 2 -> \"two\", 3 -> \"three\")" shouldNot compile
  }

  it should "eliminate 'duplicate' entries passed to the apply factory method" in {
    val nonEmptyMap = lower.EquaMap("one" -> 1, "two" -> 22, "two" -> 2, "three" -> 33, "Three" -> 3)
    nonEmptyMap should have size 3
    nonEmptyMap shouldBe lower.EquaMap("one" -> 1, "two" -> 2, "Three" -> 3)
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for EquaSet.
  }

  it should "have a toString method" in {
    lower.EquaMap("hi" -> 1, "ho" -> 2).toString should === ("EquaMap(hi -> 1, ho -> 2)")
  }

  it should "have a + method that takes one argument" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3)
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3)
    result2 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 3)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3)
    result3 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3)
    result3.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result4 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3)
    result4 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 3)
    result4.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

}