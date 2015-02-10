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
    lower.EquaMap("hi" -> 1, "ho" -> 2).toString should ===("EquaMap(hi -> 1, ho -> 2)")
  }

  it should "have a toMap method" in {
    lower.EquaMap("hi" -> 1, "ho" -> 2).toMap shouldBe (Map("hi" -> 1, "ho" -> 2))
  }

  it should "have a toEquaBoxMap method" in {
    lower.EquaMap("hi" -> 1, "ho" -> 2).toEquaBoxMap shouldBe (Map(lower.EquaBox("hi") -> 1, lower.EquaBox("ho") -> 2))
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

  it should "have a + method that takes two or more arguments" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2) +("ha" -> 3, "hey!" -> 4)
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2) +("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) +("ha" -> 3, "hey!" -> 4)
    result3 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result3.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result4 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) +("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

  it should "have a - method that takes one argument" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - "ha"
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2) - "HO"
    result2 shouldBe lower.EquaMap("hi" -> 1)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.EquaMap("hi" -> 1, "ho" -> 2) - "who?"
    result3 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.EquaMap[Int]]

    val result4 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - "ha"
    result4 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result4.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result5 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) - "HO"
    result5 shouldBe lower.FastEquaMap("hi" -> 1)
    result5.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result6 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) - "who?"
    result6 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result6.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

  it should "have a - method that takes two or more arguments" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - ("ha", "howdy!")
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) - ("HO", "FIE", "fUm")
    result2 shouldBe lower.EquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.EquaMap("hi" -> 1, "ho" -> 2) - ("who", "goes", "thar")
    result3 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.EquaMap[Int]]

    val result4 = lower.EquaMap("hi" -> 1, "ho" -> 2) - ("HI", "HO")
    result4 shouldBe lower.EquaMap.empty[Int]
    result4.shouldHaveExactType[lower.EquaMap[Int]]

    val result5 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) - ("ha", "howdy!")
    result5 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result5.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result6 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) - ("HO", "FIE", "fUm")
    result6 shouldBe lower.FastEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result6.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result7 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) - ("who", "goes", "thar")
    result7 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result7.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result8 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) - ("HI", "HO")
    result8 shouldBe lower.FastEquaMap.empty[Int]
    result8.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

  it should "have a keysIterator method that returns the map's keys" in {
    lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "he" -> 4).keysIterator.toList should contain theSameElementsAs List("ha", "he", "hi", "ho")
  }

  it should "have a valuesIterator method that returns the map's values" in {
    lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "he" -> 4).valuesIterator.toList should contain theSameElementsAs List(1, 2, 3, 4)
  }

  it should "have a ++ method that takes a GenTraversableOnce" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ List("ha" -> 3, "hey!" -> 4)
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ List("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ Set("ha" -> 3, "hey!" -> 4)
    result3 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result3.shouldHaveExactType[lower.EquaMap[Int]]

    val result4 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ Set("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4.shouldHaveExactType[lower.EquaMap[Int]]

    val result5 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ Vector("ha" -> 3, "hey!" -> 4)
    result5 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result5.shouldHaveExactType[lower.EquaMap[Int]]

    val result6 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ Vector("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result6 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result6.shouldHaveExactType[lower.EquaMap[Int]]

    val result7 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ List("ha" -> 3, "hey!" -> 4)
    result7 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result7.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result8 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ List("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result8 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result8.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result9 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ Set("ha" -> 3, "hey!" -> 4)
    result9 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result9.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result10 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ Set("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result10 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result10.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result11 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ Vector("ha" -> 3, "hey!" -> 4)
    result11 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result11.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result12 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ Vector("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result12 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result12.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

  it should "have a ++ method that takes another EquaMap" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ lower.EquaMap("ha" -> 3, "hey!" -> 4)
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2) ++ lower.EquaMap("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ lower.FastEquaMap("ha" -> 3, "hey!" -> 4)
    result3 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3, "hey!" -> 4)
    result3.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result4 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) ++ lower.FastEquaMap("HO" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 3, "hoe" -> 4, "Ho!" -> 5)
    result4.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

  it should "have a -- method that takes a GenTraversableOnce" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- List("ha", "howdy!")
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- List("HO", "FIE", "fUm")
    result2 shouldBe lower.EquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- List("who", "goes", "thar")
    result3 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.EquaMap[Int]]

    val result4 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- List("HI", "HO")
    result4 shouldBe lower.EquaMap.empty[Int]
    result4.shouldHaveExactType[lower.EquaMap[Int]]

    val result5 = lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Set("ha", "howdy!")
    result5 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result5.shouldHaveExactType[lower.EquaMap[Int]]

    val result6 = lower.EquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Set("HO", "FIE", "fUm")
    result6 shouldBe lower.EquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result6.shouldHaveExactType[lower.EquaMap[Int]]

    val result7 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- Set("who", "goes", "thar")
    result7 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result7.shouldHaveExactType[lower.EquaMap[Int]]

    val result8 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- Set("HI", "HO")
    result8 shouldBe lower.EquaMap.empty[Int]
    result8.shouldHaveExactType[lower.EquaMap[Int]]

    val result9 = lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Vector("ha", "howdy!")
    result9 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result9.shouldHaveExactType[lower.EquaMap[Int]]

    val result10 = lower.EquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Vector("HO", "FIE", "fUm")
    result10 shouldBe lower.EquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result10.shouldHaveExactType[lower.EquaMap[Int]]

    val result11 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- Vector("who", "goes", "thar")
    result11 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result11.shouldHaveExactType[lower.EquaMap[Int]]

    val result12 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- Vector("HI", "HO")
    result12 shouldBe lower.EquaMap.empty[Int]
    result12.shouldHaveExactType[lower.EquaMap[Int]]

    val result13 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- List("ha", "howdy!")
    result13 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result13.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result14 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- List("HO", "FIE", "fUm")
    result14 shouldBe lower.FastEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result14.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result15 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- List("who", "goes", "thar")
    result15 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result15.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result16 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- List("HI", "HO")
    result16 shouldBe lower.FastEquaMap.empty[Int]
    result16.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result17 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Set("ha", "howdy!")
    result17 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result17.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result18 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Set("HO", "FIE", "fUm")
    result18 shouldBe lower.FastEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result18.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result19 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- Set("who", "goes", "thar")
    result19 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result19.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result20 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- Set("HI", "HO")
    result20 shouldBe lower.FastEquaMap.empty[Int]
    result20.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result21 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- Vector("ha", "howdy!")
    result21 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result21.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result22 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- Vector("HO", "FIE", "fUm")
    result22 shouldBe lower.FastEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result22.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result23 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- Vector("who", "goes", "thar")
    result23 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result23.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result24 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- Vector("HI", "HO")
    result24 shouldBe lower.FastEquaMap.empty[Int]
    result24.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

  it should "have a -- method that takes EquaSet" in {
    val result1 = lower.EquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- lower.EquaSet("ha", "howdy!")
    result1 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result1.shouldHaveExactType[lower.EquaMap[Int]]

    val result2 = lower.EquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- lower.EquaSet("HO", "FIE", "fUm")
    result2 shouldBe lower.EquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result2.shouldHaveExactType[lower.EquaMap[Int]]

    val result3 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- lower.EquaSet("who", "goes", "thar")
    result3 shouldBe lower.EquaMap("hi" -> 1, "ho" -> 2)
    result3.shouldHaveExactType[lower.EquaMap[Int]]

    val result4 = lower.EquaMap("hi" -> 1, "ho" -> 2) -- lower.EquaSet("HI", "HO")
    result4 shouldBe lower.EquaMap.empty[Int]
    result4.shouldHaveExactType[lower.EquaMap[Int]]

    val result5 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "ha" -> 3) -- lower.FastEquaSet("ha", "howdy!")
    result5 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result5.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result6 = lower.FastEquaMap("hi" -> 1, "ho" -> 2, "fee" -> 3, "fie" -> 4, "foe" -> 5, "fum" -> 6) -- lower.FastEquaSet("HO", "FIE", "fUm")
    result6 shouldBe lower.FastEquaMap("hi" -> 1, "fee" -> 3, "foe" -> 5)
    result6.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result7 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- lower.FastEquaSet("who", "goes", "thar")
    result7 shouldBe lower.FastEquaMap("hi" -> 1, "ho" -> 2)
    result7.shouldHaveExactType[lower.FastEquaMap[Int]]

    val result8 = lower.FastEquaMap("hi" -> 1, "ho" -> 2) -- lower.FastEquaSet("HI", "HO")
    result8 shouldBe lower.FastEquaMap.empty[Int]
    result8.shouldHaveExactType[lower.FastEquaMap[Int]]
  }

  it should "have a /: method" in {
    (0 /: lower.EquaMap("one" -> 1))(_ + _._2) shouldBe 1
    (1 /: lower.EquaMap("one" -> 1))(_ + _._2) shouldBe 2
    (0 /: lower.EquaMap("one" -> 1, "two" -> 2, "three" -> 3))(_ + _._2) shouldBe 6
    (1 /: lower.EquaMap("one" -> 1, "two" -> 2, "three" -> 3))(_ + _._2) shouldBe 7
  }
}