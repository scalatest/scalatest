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

/*
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
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 3)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3)
    result3 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3)
    result3.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result4 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3)
    result4 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 3)
    result4.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a + method that takes two or more arguments" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3, "hey!" -> 4)
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("ha" -> 3, "hey!" -> 4)
    result3 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result3.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result4 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) + ("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
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

  it should "have a - method that takes two or more arguments" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - ("ha", "howdy!")
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) - ("HO", "FIE", "fUm")
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) - ("who", "goes", "thar")
    result3 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result4 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) - ("HI", "HO")
    result4 shouldBe lower.SortedEquaMap.empty
    result4.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result5 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - ("ha", "howdy!")
    result5 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result5.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result6 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) - ("HO", "FIE", "fUm")
    result6 shouldBe lower.TreeEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result6.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result7 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) - ("who", "goes", "thar")
    result7 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result7.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result8 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) - ("HI", "HO")
    result8 shouldBe lower.TreeEquaMap.empty[Int]
    result8.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a keysIterator method that returns keys in sorted order" in {
    lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "he" -> 4).keysIterator.toList shouldEqual List("ha", "he", "hi", "ho")
  }

  it should "have a valuesIterator method that returns keys in sorted order" in {
    lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "he" -> 4).valuesIterator.toList shouldEqual List(3, 4, 1, 2)  // sorted by the order of key
  }

  it should "have a ++ method that takes a GenTraversableOnce" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ List("ha" -> 3, "hey!" -> 4)
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ List("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ Set("ha" -> 3, "hey!" -> 4)
    result3 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result3.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result4 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ Set("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result5 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ Vector("ha" -> 3, "hey!" -> 4)
    result5 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result5.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result6 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ Vector("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result6 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result6.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result7 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ List("ha" -> 3, "hey!" -> 4)
    result7 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result7.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result8 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ List("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result8 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result8.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result9 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ Set("ha" -> 3, "hey!" -> 4)
    result9 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result9.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result10 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ Set("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result10 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result10.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result11 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ Vector("ha" -> 3, "hey!" -> 4)
    result11 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result11.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result12 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ Vector("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result12 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result12.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a ++ method that takes another EquaSet" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ lower.SortedEquaMap("ha" -> 3, "hey!" -> 4)
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) ++ lower.SortedEquaMap("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ lower.TreeEquaMap("ha" -> 3, "hey!" -> 4)
    result3 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result3.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result4 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) ++ lower.TreeEquaMap("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 3, "Ho!" -> 5, "hoe" -> 4)
    result4.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a -- method that takes a GenTraversableOnce" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- List("ha", "howdy!")
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- List("HO", "FIE", "fUm")
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- List("who", "goes", "thar")
    result3 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result4 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- List("HI", "HO")
    result4 shouldBe lower.SortedEquaMap.empty[Int]
    result4.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result5 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Set("ha", "howdy!")
    result5 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result5.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result6 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Set("HO", "FIE", "fUm")
    result6 shouldBe lower.SortedEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result6.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result7 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- Set("who", "goes", "thar")
    result7 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result7.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result8 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- Set("HI", "HO")
    result8 shouldBe lower.SortedEquaMap.empty[Int]
    result8.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result9 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Vector("ha", "howdy!")
    result9 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result9.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result10 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Vector("HO", "FIE", "fUm")
    result10 shouldBe lower.SortedEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result10.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result11 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- Vector("who", "goes", "thar")
    result11 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result11.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result12 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- Vector("HI", "HO")
    result12 shouldBe lower.SortedEquaMap.empty[Int]
    result12.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result13 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- List("ha", "howdy!")
    result13 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result13.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result14 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- List("HO", "FIE", "fUm")
    result14 shouldBe lower.TreeEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result14.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result15 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- List("who", "goes", "thar")
    result15 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result15.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result16 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- List("HI", "HO")
    result16 shouldBe lower.TreeEquaMap.empty[Int]
    result16.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result17 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Set("ha", "howdy!")
    result17 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result17.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result18 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Set("HO", "FIE", "fUm")
    result18 shouldBe lower.TreeEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result18.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result19 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- Set("who", "goes", "thar")
    result19 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result19.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result20 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- Set("HI", "HO")
    result20 shouldBe lower.TreeEquaMap.empty[Int]
    result20.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result21 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Vector("ha", "howdy!")
    result21 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result21.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result22 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Vector("HO", "FIE", "fUm")
    result22 shouldBe lower.TreeEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result22.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result23 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- Vector("who", "goes", "thar")
    result23 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result23.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result24 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- Vector("HI", "HO")
    result24 shouldBe lower.TreeEquaMap.empty[Int]
    result24.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a -- method that takes EquaSet" in {
    val result1 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- lower.EquaSet("ha", "howdy!")
    result1 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result2 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 5) -- lower.EquaSet("HO", "FIE", "fUm")
    result2 shouldBe lower.SortedEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result2.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result3 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- lower.EquaSet("who", "goes", "thar")
    result3 shouldBe lower.SortedEquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result4 = lower.SortedEquaMap("hi" -> 1, "ho" -> 2) -- lower.EquaSet("HI", "HO")
    result4 shouldBe lower.SortedEquaMap.empty[Int]
    result4.shouldHaveExactType[lower.SortedEquaMap[Int]]

    val result5 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- lower.EquaSet("ha", "howdy!")
    result5 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result5.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result6 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- lower.EquaSet("HO", "FIE", "fUm")
    result6 shouldBe lower.TreeEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result6.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result7 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- lower.EquaSet("who", "goes", "thar")
    result7 shouldBe lower.TreeEquaMap("hi" -> 1, "ho" -> 2)
    result7.shouldHaveExactType[lower.TreeEquaMap[Int]]

    val result8 = lower.TreeEquaMap("hi" -> 1, "ho" -> 2) -- lower.EquaSet("HI", "HO")
    result8 shouldBe lower.TreeEquaMap.empty[Int]
    result8.shouldHaveExactType[lower.TreeEquaMap[Int]]
  }

  it should "have a /: method" in {
    (0 /: lower.SortedEquaMap("one" -> 1))(_ + _._2) shouldBe 1
    (1 /: lower.SortedEquaMap("one" -> 1))(_ + _._2) shouldBe 2
    (0 /: lower.SortedEquaMap("one" -> 1, "two" -> 2, "three" -> 3))(_ + _._2) shouldBe 6
    (1 /: lower.SortedEquaMap("one" -> 1, "two" -> 2, "three" -> 3))(_ + _._2) shouldBe 7
  }
}
*/
