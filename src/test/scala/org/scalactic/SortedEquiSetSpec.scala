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

import java.text._
import org.scalatest._
import scala.collection.GenTraversable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

class SortedEquiSetSpec extends UnitSpec {
  val intEquality =
    new OrderingEquality[Int] {
      def hashCodeFor(a: Int): Int = a.hashCode
      def areEqual(a: Int, b: Any): Boolean = a == b
      def compare(a: Int, b: Int): Int = a - b
    }
  val number = SortedEquiSets[Int](intEquality)
  val lower = SortedEquiSets[String](StringNormalizations.lowerCased.toOrderingEquality)
  val sortedLower = SortedEquiSets[String](StringNormalizations.lowerCased.toOrderingEquality)
  val trimmed = SortedEquiSets[String](StringNormalizations.trimmed.toOrderingEquality)
  "An SortedEquiSet" can "be constructed with empty" in {
    val emptySet = lower.SortedEquiSet.empty
    emptySet shouldBe empty
  }
  it can "be constructed with apply" in {
    val nonEmptySet = lower.SortedEquiSet("one", "two", "three")
    nonEmptySet should have size 3
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedEquiSet.
  }
  it should "construct only sets with appropriate element types" in {
    "lower.SortedEquiSet(1, 2, 3)" shouldNot compile
  }
  it should "eliminate 'duplicate' entries passed to the apply factory method" in {
    val nonEmptySet = lower.SortedEquiSet("one", "two", "two", "three", "Three")
    nonEmptySet should have size 3
    // TODO: After moving enablers to scalactic, make a nominal typeclass
    // instance for Size and Length for SortedEquiSet.
  }
  it should "have a toString method" in {
    lower.SortedEquiSet("hi", "ho").toString should === ("TreeEquiSet(hi, ho)")
  }
  it should "have a diff method that takes another SortedEquiSet instance with the same path-dependant type" in {
    lower.SortedEquiSet("hi", "ho") diff lower.SortedEquiSet("HI", "HO") shouldBe lower.SortedEquiSet()
    trimmed.SortedEquiSet("hi", "ho") diff trimmed.SortedEquiSet(" hi ", " ho ") shouldBe trimmed.SortedEquiSet()
    """lower.SortedEquiSet(" hi ", "hi") diff trimmed.SortedEquiSet("hi", "HI")""" shouldNot typeCheck
    lower.SortedEquiSet("hi", "ho") diff lower.SortedEquiSet("ho") shouldBe lower.SortedEquiSet("hi")
    lower.SortedEquiSet("hi", "ho", "let's", "go") diff lower.SortedEquiSet("bo", "no", "go", "ho") shouldBe lower.SortedEquiSet("hi", "let's")
  }
  it should "have a &~ method that takes another SortedEquiSet instance with the same path-dependant type" in {
    lower.SortedEquiSet("hi", "ho") &~ lower.SortedEquiSet("HI", "HO") shouldBe lower.SortedEquiSet()
    trimmed.SortedEquiSet("hi", "ho") &~ trimmed.SortedEquiSet(" hi ", " ho ") shouldBe trimmed.SortedEquiSet()
    """lower.SortedEquiSet(" hi ", "hi") &~ trimmed.SortedEquiSet("hi", "HI")""" shouldNot typeCheck
    lower.SortedEquiSet("hi", "ho") &~ lower.SortedEquiSet("ho") shouldBe lower.SortedEquiSet("hi")
    lower.SortedEquiSet("hi", "ho", "let's", "go") &~ lower.SortedEquiSet("bo", "no", "go", "ho") shouldBe lower.SortedEquiSet("hi", "let's")
  }
  it should "have an intersect method that takes another SortedEquiSet instance with the same path-dependant type" in {
    lower.SortedEquiSet("hi", "ho") intersect lower.SortedEquiSet("HI", "HO") shouldBe lower.SortedEquiSet("hi", "ho")
    trimmed.SortedEquiSet("hi", "ho") intersect trimmed.SortedEquiSet(" hi ", " ho ") shouldBe trimmed.SortedEquiSet("hi", "ho")
    """lower.SortedEquiSet(" hi ", "hi") intersect trimmed.SortedEquiSet("hi", "HI")""" shouldNot typeCheck
    lower.SortedEquiSet("hi", "ho") intersect lower.SortedEquiSet("ho") shouldBe lower.SortedEquiSet("ho")
    lower.SortedEquiSet("hi", "ho", "let's", "go") intersect lower.SortedEquiSet("bo", "no", "go", "ho") shouldBe lower.SortedEquiSet("ho", "go")
  }
  it should "have an & method that takes another SortedEquiSet instance with the same path-dependant type" in {
    lower.SortedEquiSet("hi", "ho") & lower.SortedEquiSet("HI", "HO") shouldBe lower.SortedEquiSet("hi", "ho")
    trimmed.SortedEquiSet("hi", "ho") & trimmed.SortedEquiSet(" hi ", " ho ") shouldBe trimmed.SortedEquiSet("hi", "ho")
    """lower.SortedEquiSet(" hi ", "hi") & trimmed.SortedEquiSet("hi", "HI")""" shouldNot typeCheck
    lower.SortedEquiSet("hi", "ho") & lower.SortedEquiSet("ho") shouldBe lower.SortedEquiSet("ho")
    lower.SortedEquiSet("hi", "ho", "let's", "go") & lower.SortedEquiSet("bo", "no", "go", "ho") shouldBe lower.SortedEquiSet("ho", "go")
  }
  it should "have a union method that takes another SortedEquiSet instance with the same path-dependant type" in {
    lower.SortedEquiSet("hi", "ho") union lower.SortedEquiSet("HI", "HO") shouldBe lower.SortedEquiSet("hi", "ho")
    trimmed.SortedEquiSet("hi", "ho") union trimmed.SortedEquiSet(" hi ", " ho ") shouldBe trimmed.SortedEquiSet("hi", "ho")
    """lower.SortedEquiSet(" hi ", "hi") union trimmed.SortedEquiSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a | method that takes another SortedEquiSet instance with the same path-dependant type" in {
    lower.SortedEquiSet("hi", "ho") | lower.SortedEquiSet("HI", "HO") shouldBe lower.SortedEquiSet("hi", "ho")
    trimmed.SortedEquiSet("hi", "ho") | trimmed.SortedEquiSet(" hi ", " ho ") shouldBe trimmed.SortedEquiSet("hi", "ho")
    """lower.SortedEquiSet(" hi ", "hi") | trimmed.SortedEquiSet("hi", "HI")""" shouldNot typeCheck
  }
  it should "have a toSet method" in {
    lower.SortedEquiSet("hi", "ho").toSet should === (Set(lower.EquiBox("hi"), lower.EquiBox("ho")))
  }
  it should "have a + method that takes one argument" in {
    lower.SortedEquiSet("hi", "ho") + "ha" shouldBe lower.SortedEquiSet("hi", "ho", "ha")
    lower.SortedEquiSet("hi", "ho") + "HO" shouldBe lower.SortedEquiSet("hi", "ho")
  }
  it should "have a + method that takes two or more arguments" in {
    lower.SortedEquiSet("hi", "ho") + ("ha", "hey!") shouldBe lower.SortedEquiSet("hi", "ho", "ha", "hey!")
    lower.SortedEquiSet("hi", "ho") + ("HO", "hoe", "Ho!") shouldBe lower.SortedEquiSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have a - method that takes one argument" in {
    lower.SortedEquiSet("hi", "ho", "ha") - "ha" shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho") - "HO" shouldBe lower.SortedEquiSet("hi")
    lower.SortedEquiSet("hi", "ho") - "who?" shouldBe lower.SortedEquiSet("hi", "ho")
  }
  it should "have a - method that takes two or more arguments" in {
    lower.SortedEquiSet("hi", "ho", "ha") - ("ha", "howdy!") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho", "fee", "fie", "foe", "fum") - ("HO", "FIE", "fUm")  shouldBe lower.SortedEquiSet("hi", "fee", "foe")
    lower.SortedEquiSet("hi", "ho") - ("who", "goes", "thar") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho") - ("HI", "HO") shouldBe lower.SortedEquiSet.empty
  }
  it should "return an iterator that returns elements in sorted order" in {
    lower.SortedEquiSet("hi", "ho", "ha", "he").iterator.toList shouldEqual List("ha", "he", "hi", "ho")
  }
  it should "have a ++ method that takes a GenTraversableOnce" in {
    lower.SortedEquiSet("hi", "ho") ++ List("ha", "hey!") shouldBe lower.SortedEquiSet("hi", "ho", "ha", "hey!")
    lower.SortedEquiSet("hi", "ho") ++ List("HO", "hoe", "Ho!") shouldBe lower.SortedEquiSet("hi", "ho", "hoe", "Ho!")

    lower.SortedEquiSet("hi", "ho") ++ Set("ha", "hey!") shouldBe lower.SortedEquiSet("hi", "ho", "ha", "hey!")
    lower.SortedEquiSet("hi", "ho") ++ Set("HO", "hoe", "Ho!") shouldBe lower.SortedEquiSet("hi", "ho", "hoe", "Ho!")

    lower.SortedEquiSet("hi", "ho") ++ Vector("ha", "hey!") shouldBe lower.SortedEquiSet("hi", "ho", "ha", "hey!")
    lower.SortedEquiSet("hi", "ho") ++ Vector("HO", "hoe", "Ho!") shouldBe lower.SortedEquiSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have a ++ method that takes another EquiSet" in {
    lower.SortedEquiSet("hi", "ho") ++ lower.SortedEquiSet("ha", "hey!") shouldBe lower.SortedEquiSet("hi", "ho", "ha", "hey!")
    lower.SortedEquiSet("hi", "ho") ++ lower.SortedEquiSet("HO", "hoe", "Ho!") shouldBe lower.SortedEquiSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have a -- method that takes a GenTraversableOnce" in {
    lower.SortedEquiSet("hi", "ho", "ha") -- List("ha", "howdy!") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho", "fee", "fie", "foe", "fum") -- List("HO", "FIE", "fUm")  shouldBe lower.SortedEquiSet("hi", "fee", "foe")
    lower.SortedEquiSet("hi", "ho") -- List("who", "goes", "thar") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho") -- List("HI", "HO") shouldBe lower.SortedEquiSet.empty

    lower.SortedEquiSet("hi", "ho", "ha") -- Set("ha", "howdy!") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho", "fee", "fie", "foe", "fum") -- Set("HO", "FIE", "fUm")  shouldBe lower.SortedEquiSet("hi", "fee", "foe")
    lower.SortedEquiSet("hi", "ho") -- Set("who", "goes", "thar") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho") -- Set("HI", "HO") shouldBe lower.SortedEquiSet.empty

    lower.SortedEquiSet("hi", "ho", "ha") -- Vector("ha", "howdy!") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho", "fee", "fie", "foe", "fum") -- Vector("HO", "FIE", "fUm")  shouldBe lower.SortedEquiSet("hi", "fee", "foe")
    lower.SortedEquiSet("hi", "ho") -- Vector("who", "goes", "thar") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho") -- Vector("HI", "HO") shouldBe lower.SortedEquiSet.empty
  }
  it should "have a -- method that takes another EquiSet" in {
    lower.SortedEquiSet("hi", "ho", "ha") -- lower.EquiSet("ha", "howdy!") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho", "fee", "fie", "foe", "fum") -- lower.EquiSet("HO", "FIE", "fUm")  shouldBe lower.SortedEquiSet("hi", "fee", "foe")
    lower.SortedEquiSet("hi", "ho") -- lower.EquiSet("who", "goes", "thar") shouldBe lower.SortedEquiSet("hi", "ho")
    lower.SortedEquiSet("hi", "ho") -- lower.EquiSet("HI", "HO") shouldBe lower.SortedEquiSet.empty
  }
  it should "have a /: method" in {
    (0 /: number.SortedEquiSet(1))(_ + _) shouldBe 1
    (1 /: number.SortedEquiSet(1))(_ + _) shouldBe 2
    (0 /: number.SortedEquiSet(1, 2, 3))(_ + _) shouldBe 6
    (1 /: number.SortedEquiSet(1, 2, 3))(_ + _) shouldBe 7
  }
  it should "have a :\\ method" in {
    (number.SortedEquiSet(1) :\ 0)(_ + _) shouldBe 1
    (number.SortedEquiSet(1) :\ 1)(_ + _) shouldBe 2
    (number.SortedEquiSet(1, 2, 3) :\ 0)(_ + _) shouldBe 6
    (number.SortedEquiSet(1, 2, 3) :\ 1)(_ + _) shouldBe 7
  }
  it should "have 3 addString methods" in {
    lower.SortedEquiSet("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    number.SortedEquiSet(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    lower.SortedEquiSet("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    number.SortedEquiSet(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    number.SortedEquiSet(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    lower.SortedEquiSet("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    number.SortedEquiSet(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    number.SortedEquiSet(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have a aggregate method" in {
    lower.SortedEquiSet("hi", "ho", "ha", "hey!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "ha", "hey!")
    lower.SortedEquiSet("hi", "ho", "ha", "hey!").aggregate(lower.SortedEquiSet())(_ + _, _ ++ _) shouldBe lower.SortedEquiSet("hi", "ho", "ha", "hey!")

    lower.SortedEquiSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(Set[String]())(_ + _, _ ++ _) shouldBe Set("hi", "ho", "hoe", "Ho!")
    lower.SortedEquiSet("hi", "ho", "HO", "hoe", "Ho!").aggregate(lower.SortedEquiSet())(_ + _, _ ++ _) shouldBe lower.SortedEquiSet("hi", "ho", "hoe", "Ho!")
  }
  it should "have an apply method" in {
    val a = number.SortedEquiSet(1, 2, 3)
    a(2) shouldEqual true
    a(5) shouldEqual false

    val b = lower.SortedEquiSet("hi")
    b("hi") shouldEqual true
    b("Hi") shouldEqual true
    b("hI") shouldEqual true
    b("HI") shouldEqual true
    b("he") shouldEqual false
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = number.SortedEquiSet(1) andThen (!_)
    pf1(1) shouldEqual false
    pf1(2) shouldEqual true

    val pf2 = number.SortedEquiSet(1, 2, 3) andThen (!_)
    pf2(1) shouldEqual false
    pf2(2) shouldEqual false
    pf2(3) shouldEqual false
    pf2(0) shouldEqual true
  }
  it should "have a canEqual method" in {
    number.SortedEquiSet(1).canEqual(3) shouldBe false
    number.SortedEquiSet(1).canEqual("hi") shouldBe false
    number.SortedEquiSet(1).canEqual(number.EquiSet(1)) shouldBe true
    number.SortedEquiSet(1).canEqual(number.EquiSet(1, 2, 3)) shouldBe true
    number.SortedEquiSet(1).canEqual(lower.EquiSet("hi")) shouldBe false
    number.SortedEquiSet(1).canEqual(number.SortedEquiSet(1)) shouldBe true
    number.SortedEquiSet(1).canEqual(number.SortedEquiSet(1, 2, 3)) shouldBe true
    number.SortedEquiSet(1).canEqual(lower.SortedEquiSet("hi")) shouldBe false
  }
  it should "have a collect method that only accepts functions that result in the path-enclosed type" in {
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    number.SortedEquiSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i % 2 == 0 => i * 2 } shouldBe number.SortedEquiSet(4, 8, 12, 16, 20)
    number.SortedEquiSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collect { case i if i > 10 => i * 2 } shouldBe number.SortedEquiSet.empty
  }
  it should "have a collectInto method that accepts a EquiSets and functions that result in other than the path-enclosed type" in {
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    number.SortedEquiSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collectInto(lower) { case i if i % 2 == 0 => (i * 2).toString } shouldBe lower.EquiSet("4", "8", "12", "16", "20")
    number.SortedEquiSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collectInto(lower) { case i if i > 10 => (i * 2).toString } shouldBe lower.EquiSet.empty
    // number.SortedEquiSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower) collect { case i if i % 2 == 0 => (i * 2).toString } shouldBe lower.EquiSet("4", "8", "12", "16", "20")
    // number.SortedEquiSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).into(lower) collect { case i if i > 10 => (i * 2).toString } shouldBe lower.EquiSet.empty
  }
  it should "have a collectInto method that accepts a SortedEquiSets and functions that result in a HashEquiSet other than the path-enclosed type" in {
    /*
    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i % 2 == 0 => i * 2 }
    res3: List[Int] = List(4, 8, 12, 16, 20)

    scala> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i * 2 }
    res4: List[Int] = List()
    */
    number.SortedEquiSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).collectInto(sortedLower) { case i if i % 2 == 0 => (i * 2).toString } shouldBe sortedLower.SortedEquiSet("4", "8", "12", "16", "20")
    number.SortedEquiSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).collectInto(sortedLower) { case i if i > 10 => (i * 2).toString } shouldBe sortedLower.EquiSet.empty
    // number.SortedEquiSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).into(sortedLower) collect { case i if i % 2 == 0 => (i * 2).toString } shouldBe sortedLower.SortedEquiSet("4", "8", "12", "16", "20")
    // number.SortedEquiSet(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).into(sortedLower) collect { case i if i > 10 => (i * 2).toString } shouldBe sortedLower.EquiSet.empty
  }
/*
  it can "be constructed from a GenTraversable via the from method on Every singleton" in {
    Every.from(List.empty[String]) shouldBe None
    Every.from(List("1")) shouldBe Some(One("1"))
    Every.from(List(1, 2, 3)) shouldBe Some(Many(1, 2, 3))
    Every.from(List.empty[String].par) shouldBe None
    Every.from(List("1").par) shouldBe Some(One("1"))
    Every.from(List(1, 2, 3).par) shouldBe Some(Many(1, 2, 3))
  }
  it can "not be constructed with any null elements" is pending
  it can "be deconstructed with One" in {
    One(1) match {
      case One(x) => x shouldEqual 1
      case _ => fail()
    }
    One("hi") match {
      case One(s) => s shouldEqual "hi"
      case _ => fail()
    }
  }
  it can "be deconstructed with Many" in {
    Many(1, 2, 3) match {
      case Many(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    Many("hi", "there") match {
      case Many(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    Many(1, 2, 3) match {
      case Many(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Many(1, 2, 3, 4, 5) match {
      case Many(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
  }
  it can "be deconstructed with Every" in {
    Many(1, 2, 3) match {
      case Every(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    Many("hi", "there") match {
      case Every(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    Many(1, 2, 3) match {
      case Every(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Many(1, 2, 3, 4, 5) match {
      case Every(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Many(1, 2, 3) match {
      case Every(x, _*) => x shouldEqual 1
      case _ => fail()
    }
    One("hi") match {
      case Every(s) => s shouldEqual "hi"
      case _ => fail()
    }
    Every(1, 2, 3) match {
      case Every(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    Every("hi", "there") match {
      case Every(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    Every(1, 2, 3) match {
      case Every(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Every(1, 2, 3, 4, 5) match {
      case Every(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Every(1, 2, 3) match {
      case Every(x, _*) => x shouldEqual 1
      case _ => fail()
    }
  }
  it should "have an apply method" in {
    Every(1, 2, 3)(0) shouldEqual 1 
    Every(1, 2, 3)(1) shouldEqual 2 
    One("hi")(0) shouldEqual "hi"
    Many(7, 8, 9)(2) shouldEqual 9
    the [IndexOutOfBoundsException] thrownBy {
      Every(1, 2, 3)(3)
    } should have message "3"
  }
  it should "have an length method" in {
    One(1).length shouldBe 1
    Many(1, 2).length shouldBe 2
    Many(1, 2, 3, 4, 5).length shouldBe 5
    Every(1).length shouldBe 1
    Every(1, 2).length shouldBe 2
    Every(1, 2, 3, 4, 5).length shouldBe 5
  }
  it should "have a ++ method that takes another Every" in {
    Every(1, 2, 3) ++ One(4) shouldEqual Every(1, 2, 3, 4)
    Every(1, 2, 3) ++ Every(4) shouldEqual Every(1, 2, 3, 4)
    Many(1, 2, 3) ++ Every(4, 5, 6) shouldEqual Every(1, 2, 3, 4, 5, 6)
    Many(1, 2, 3) ++ One(4) shouldEqual Many(1, 2, 3, 4)
    Every(1, 2, 3) ++ One(4) shouldEqual Many(1, 2, 3, 4)
    Every(1, 2, 3) ++ Every(4) shouldEqual Many(1, 2, 3, 4)
    Many(1, 2, 3) ++ Every(4) shouldEqual Many(1, 2, 3, 4)
    Many(1, 2, 3) ++ One(4) shouldEqual Many(1, 2, 3, 4)
  }
  it should "have a ++ method that takes a GenTraversableOnce" in {
    Every(1, 2, 3) ++ List(4) shouldEqual Every(1, 2, 3, 4)
    Every(1, 2, 3) ++ Vector(4, 5, 6) shouldEqual Every(1, 2, 3, 4, 5, 6)
    Many(1, 2, 3) ++ GenTraversable(4) shouldEqual Every(1, 2, 3, 4)
    Many(1, 2, 3) ++ Set(4, 5) shouldEqual Many(1, 2, 3, 4, 5)
    Many(1, 2, 3) ++ Set(4, 5).iterator shouldEqual Many(1, 2, 3, 4, 5)
  }
  it should "have a +: method" in {
    0 +: One(1) shouldBe Many(0, 1)
    0 +: Many(1, 2) shouldBe Many(0, 1, 2)
    "zero" +: Every("one", "two") shouldBe Every("zero", "one", "two")
  }
  it should "implement PartialFunction[Int, T]" in {
    val pf1: PartialFunction[Int, Int] = Every(1)
    pf1.isDefinedAt(0) shouldBe true
    pf1.isDefinedAt(1) shouldBe false
  }
  it should "have a /: method" in {
    (0 /: One(1))(_ + _) shouldBe 1
    (1 /: One(1))(_ + _) shouldBe 2
    (0 /: Many(1, 2, 3))(_ + _) shouldBe 6
    (1 /: Many(1, 2, 3))(_ + _) shouldBe 7
  }
  it should "have a :+ method" in {
    One(1) :+ 2 shouldBe Many(1, 2)
    Many(1, 2) :+ 3 shouldBe Many(1, 2, 3)
  }
  it should "have a :\\ method" in {
    (One(1) :\ 0)(_ + _) shouldBe 1
    (One(1) :\ 1)(_ + _) shouldBe 2
    (Many(1, 2, 3) :\ 0)(_ + _) shouldBe 6
    (Many(1, 2, 3) :\ 1)(_ + _) shouldBe 7
  }
  it should "have 3 addString methods" in {
    One("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    Many(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    One("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    Many(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    Many(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    One("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    Many(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    Many(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = One(1) andThen (_ + 1)
    pf1(0) shouldEqual 2
    val pf2 = Many(1, 2, 3) andThen (_ + 1)
    pf2(0) shouldEqual 2
    pf2(1) shouldEqual 3
    pf2(2) shouldEqual 4
  }
  it should "have an applyOrElse method (inherited from PartialFunction)" in {
    Every(1, 2, 3).applyOrElse(0, (_: Int) * -1) shouldEqual 1
    Every(1, 2, 3).applyOrElse(1, (_: Int) * -1) shouldEqual 2
    Every(1, 2, 3).applyOrElse(2, (_: Int) * -1) shouldEqual 3
    Every(1, 2, 3).applyOrElse(3, (_: Int) * -1) shouldEqual -3
    Every(1, 2, 3).applyOrElse(4, (_: Int) * -1) shouldEqual -4
  }
  it should "have an canEqual method" is pending
  // it should "have an charAt method" is pending
  // Could have an implicit conversion from Every[Char] to CharSequence like
  // there is for Seq in Predef.
  /*
  scala> Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i / 2 }  
  res1: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an collectFirst method" in {
    Every(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collectFirst { case i if i > 10 => i / 2 } shouldBe None
    Every(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) collectFirst { case i if i > 10 => i / 2 } shouldBe Some(5)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Int = Every(1, 2, 3).compose((_: Int) + 1)
    fn(-1) shouldBe 1
    fn(0) shouldBe 2
    fn(1) shouldBe 3
  }
  it should "have an contains method" in {
    val e = Every(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
  }
  // Decided to just overload one for GenSeq and one for Every. Could have done
  // what that has a Slicing nature, but that's a bit too fancy pants.
  it should "have a containsSlice method that takes GenSeq" in {
    val every = Every(1, 2, 3, 4, 5)
    every.containsSlice(List(2, 3)) shouldBe true
    every.containsSlice(List(2, 3, 5)) shouldBe false
    every.containsSlice(List.empty) shouldBe true
    every.containsSlice(Vector(2, 3)) shouldBe true
    every.containsSlice(Vector(2, 3, 5)) shouldBe false
    every.containsSlice(Vector.empty) shouldBe true
    every.containsSlice(ListBuffer(2, 3)) shouldBe true
    every.containsSlice(ListBuffer(2, 3, 5)) shouldBe false
    every.containsSlice(ListBuffer.empty) shouldBe true
  }
  it should "have a containsSlice method that takes an Every" in {
    val every = Every(1, 2, 3, 4, 5)
    every.containsSlice(Every(2, 3)) shouldBe true
    every.containsSlice(Every(2, 3, 5)) shouldBe false
    every.containsSlice(Every(3)) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(-1)
    Many(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(1, 2, 3, 4, 5)

    val arr2 = Array.fill(5)(-1)
    Many(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(-1, 1, 2, 3, 4)

    val arr3 = Array.fill(5)(-1)
    Many(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(-1, 1, 2, -1, -1)
  }
  it should "have a copyToBuffer method" in {

    val buf = ListBuffer.fill(3)(-1)
    Many(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(-1, -1, -1, 1, 2, 3, 4, 5)
  }
  it should "have a corresponds method that takes a GenSeq" in {
    val every = Every(1, 2, 3, 4, 5)
    every.corresponds(List(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    every.corresponds(List(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    every.corresponds(List(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    every.corresponds(List(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes an Every" in {
    val every = Every(1, 2, 3, 4, 5)
    every.corresponds(Many(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    every.corresponds(Many(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    every.corresponds(Many(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    every.corresponds(Many(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have an count method" in {
    val every = Every(1, 2, 3, 4, 5)
    every.count(_ > 10) shouldBe 0
    every.count(_ % 2 == 0) shouldBe 2
    every.count(_ % 2 == 1) shouldBe 3
  }
  /*
  it should not have a diff method
    scala> Vector(1, 2, 3).diff(Vector(1, 2, 3))
    res0: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an distinct method" in {
    Every(1, 2, 3).distinct shouldBe Every(1, 2, 3)
    Every(1).distinct shouldBe Every(1)
    Every(1, 2, 1, 1).distinct shouldBe Every(1, 2)
    Every(1, 1, 1).distinct shouldBe Every(1)
  }

  /*
  it should not have an drop method 
    scala> Vector(1, 2, 3).drop(3)
    res1: scala.collection.immutable.Vector[Int] = Vector()

  it should not have an dropRight method
    scala> Vector(1, 2, 3).dropRight(3)
    res0: scala.collection.immutable.Vector[Int] = Vector()

  it should not have an dropWhile method
    scala> Vector(1, 2, 3).dropWhile(_ < 10)
    res2: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an endsWith method that takes a GenSeq" in {
    Every(1).endsWith(List(1)) shouldBe true
    Every(1).endsWith(List(1, 2)) shouldBe false
    Every(1, 2).endsWith(List(1, 2)) shouldBe true
    Every(1, 2, 3, 4, 5).endsWith(List(1, 2)) shouldBe false
    Every(1, 2, 3, 4, 5).endsWith(List(5)) shouldBe true
    Every(1, 2, 3, 4, 5).endsWith(List(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes an Every" in {
    Every(1).endsWith(Every(1)) shouldBe true
    Every(1).endsWith(Every(1, 2)) shouldBe false
    Every(1, 2).endsWith(Every(1, 2)) shouldBe true
    Every(1, 2, 3, 4, 5).endsWith(Every(1, 2)) shouldBe false
    Every(1, 2, 3, 4, 5).endsWith(Every(5)) shouldBe true
    Every(1, 2, 3, 4, 5).endsWith(Every(3, 4, 5)) shouldBe true
  }
  it should "have an equals method" in {
    // This already worked, because the only concrete classes
    // are case classes.
    One(1) should not equal One(2)
    Many(1, 2) should not equal Many(2, 3)
  }
  it should "have an exists method" in {
    Every(1, 2, 3).exists(_ == 2) shouldBe true
    Every(1, 2, 3).exists(_ == 5) shouldBe false
  }
  /*
  it should not have a filter method
    scala> Vector(1, 2, 3).filter(_ > 10)
    res12: scala.collection.immutable.Vector[Int] = Vector()

  it should not have a filterNot method
    scala> Vector(1, 2, 3).filterNot(_ < 10)
    res13: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a find method" in {
    Every(1, 2, 3).find(_ == 5) shouldBe None
    Every(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have a flatMap method" in {
    Every(1, 2, 3) flatMap (i => One(i + 1)) shouldBe Every(2, 3, 4)
    val ss = Every("hi", "ho")
    val is = Every(1, 2, 3)
    (for (s <- ss; i <- is) yield (s, i)) shouldBe
      Every(
        ("hi",1), ("hi",2), ("hi",3), ("ho",1), ("ho",2), ("ho",3)
      )
    One(5) flatMap (i => One(i + 3)) shouldBe One(8)
    One(8) flatMap (i => Every(i.toString)) shouldBe One("8")
  }
  /*
    Can only flatten Everys
    scala> Vector(Set.empty[Int], Set.empty[Int]).flatten
    res17: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a flatten method that works on nested Everys" in {
    Every(Every(1, 2, 3), Every(1, 2, 3)).flatten shouldBe Every(1, 2, 3, 1, 2, 3)
    Every(Every(1)).flatten shouldBe Every(1)
  }
  it can "be flattened when in a GenTraversableOnce" in {
  // need to keep this commented out until finish implementing all methods
    Vector(Every(1, 2, 3), Every(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    List(Every(1, 2, 3), Every(1, 2, 3)).flatten shouldBe List(1, 2, 3, 1, 2, 3)
    List(Every(1, 2, 3), Every(1, 2, 3)).toIterator.flatten.toStream shouldBe List(1, 2, 3, 1, 2, 3).toIterator.toStream
    List(Every(1, 2, 3), Every(1, 2, 3)).par.flatten shouldBe List(1, 2, 3, 1, 2, 3).par
  }
  it should "have a fold method" in {
    One(1).fold(0)(_ + _) shouldBe 1
    One(1).fold(1)(_ * _) shouldBe 1
    One(2).fold(0)(_ + _) shouldBe 2
    One(2).fold(1)(_ * _) shouldBe 2
    One(3).fold(0)(_ + _) shouldBe 3
    One(3).fold(1)(_ * _) shouldBe 3
    Many(1, 2, 3).fold(0)(_ + _) shouldBe 6
    Many(1, 2, 3).fold(1)(_ * _) shouldBe 6
    Many(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    Many(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    One(1).foldLeft(0)(_ + _) shouldBe 1
    One(1).foldLeft(1)(_ + _) shouldBe 2
    Many(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    Many(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    One(1).foldRight(0)(_ + _) shouldBe 1
    One(1).foldRight(1)(_ + _) shouldBe 2
    Many(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    Many(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    Every(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    Every(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    Every(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- Every(1, 2, 3))
      num += i
    num shouldBe 12
    One(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    Every(1, 2, 3, 4, 5).groupBy(_ % 2) shouldBe Map(1 -> Every(1, 3, 5), 0 -> Every(2, 4))
    Every(1, 2, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> Every(1, 3, 3, 3), 0 -> Every(2))
    Every(1, 1, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> Every(1, 1, 3, 3, 3))
    Every(1, 2, 3, 5, 7).groupBy(_ % 2) shouldBe Map(1 -> Every(1, 3, 5, 7), 0 -> Every(2))
  }
  it should "have a grouped method" in {
    Every(1, 2, 3).grouped(2).toList shouldBe List(Every(1, 2), Every(3))
    Every(1, 2, 3).grouped(1).toList shouldBe List(Every(1), Every(2), Every(3))
    an [IllegalArgumentException] should be thrownBy { Every(1, 2, 3).grouped(0).toList }
    Every(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toList shouldBe List(Every(1, 2), Every(3, 4), Every(5, 6), Every(7, 8), Every(9, 10))
    Every(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toList shouldBe List(Every(1, 2, 3), Every(4, 5, 6), Every(7, 8, 9), Every(10))
    Every(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toList shouldBe List(Every(1, 2, 3, 4), Every(5, 6, 7, 8), Every(9, 10))
    Every(1).grouped(2).toList shouldBe List(Every(1))
    Every(1).grouped(1).toList shouldBe List(Every(1))
  }
  it should "have a hasDefiniteSize method" in {
    Every(1).hasDefiniteSize shouldBe true
    Every(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    One(1).hashCode shouldEqual Every(1).hashCode
    Many(1, 2).hashCode shouldEqual Every(1, 2).hashCode
  }
  it should "have a head method" in {
    One("hi").head shouldBe "hi"
    Many(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    One("hi").headOption shouldBe Some("hi")
    Many(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have 2 indexOf methods" in {
    Every(1, 2, 3, 4, 5).indexOf(3) shouldBe 2
    Every(1, 2, 3, 4, 5).indexOf(1) shouldBe 0
    Every(1, 2, 3, 4, 5).indexOf(1, 2) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOf(6) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOf(5, 3) shouldBe 4
  }
  it should "have 2 indexOfSlice methods that take a GenSeq" in {
    Every(1, 2, 3, 4, 5).indexOfSlice(List(2, 3)) shouldBe 1
    Every(1, 2, 3, 4, 5).indexOfSlice(List(2, 3), 3) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5), 3) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5)) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(List(5)) shouldBe 4
    Every(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
    Every(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
    Every(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe 0
    Every(1, 2, 3, 4, 5).indexOfSlice(List.empty) shouldBe 0
    Every(1, 2, 3, 4, 5).indexOfSlice(List.empty, 6) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(List.empty, 4) shouldBe 4
  }
  it should "have 2 indexOfSlice methods that take an Every" in {
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3)) shouldBe 1
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3), 3) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5)) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(5)) shouldBe 4
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe -1
    Every(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe 0
  }
  it should "have 2 indexWhere methods" in {
    Every(1, 2, 3, 4, 5).indexWhere(_ == 3) shouldBe 2
    Every(1, 2, 3, 4, 5).indexWhere(_ == 1) shouldBe 0
    Every(1, 2, 3, 4, 5).indexWhere(_ == 1, 2) shouldBe -1
    Every(1, 2, 3, 4, 5).indexWhere(_ == 6) shouldBe -1
    Every(1, 2, 3, 4, 5).indexWhere(_ == 5, 3) shouldBe 4
  }
  it should "have an indices method" in {
    Every(1).indices shouldBe Vector(1).indices
    Every(1, 2, 3).indices shouldBe (0 to 2)
    Every(1, 2, 3, 4, 5).indices shouldBe (0 to 4)
  }
  /*
  it should not have an init method
    scala> Vector(1).init
    res30: scala.collection.immutable.Vector[Int] = Vector()

  it should "have an inits method" is pending
    scala> Vector(1).inits.toList
    res32: List[scala.collection.immutable.Vector[Int]] = List(Vector(1), Vector())

  it should "have an intersect method" is pending
    scala> Vector(1, 2, 3) intersect Vector(4, 5)
    res33: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an isDefinedAt method, inherited from PartialFunction" in {
    Every(1).isDefinedAt(0) shouldBe true
    Every(1).isDefinedAt(1) shouldBe false
    Every(1, 2, 3).isDefinedAt(1) shouldBe true
    Every(1, 2, 3).isDefinedAt(2) shouldBe true
    Every(1, 2, 3).isDefinedAt(3) shouldBe false
    Every(1, 2, 3).isDefinedAt(0) shouldBe true
    Every(1, 2, 3).isDefinedAt(-1) shouldBe false
  }
  it should "have an isEmpty method" in {
    One("hi").isEmpty shouldBe false
    Many(1, 2, 3).isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    One("hi").isTraversableAgain shouldBe true
    Many(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    One("hi").iterator.toList shouldBe List("hi")
    Many(1, 2, 3).iterator.toList shouldBe List(1, 2, 3)
  }
  it should "have a last method" in {
    One("hi").last shouldBe "hi"
    Many(1, 2, 3).last shouldBe 3
  }
  it should "have 2 lastIndexOf methods" in {
    Every(1, 2, 3, 4, 5).lastIndexOf(2) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexOf(0) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexOf(5) shouldBe 4
    Every(1, 2, 3, 3, 5).lastIndexOf(3) shouldBe 3
    Every(1).lastIndexOf(1) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexOf(2, 3) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexOf(2, 0) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexOf(2, 1) shouldBe 1
  }
  it should "have 2 lastIndexOfSlice methods that take a GenSeq" in {
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3)) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3), 3) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3, 5)) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(5)) shouldBe 4
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty) shouldBe 5
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty, 6) shouldBe 5
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty, 4) shouldBe 4
  }
  it should "have 2 lastIndexOfSlice methods that take an Every" in {
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3)) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3), 3) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5)) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(5)) shouldBe 4
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe -1
  }
  it should "have 2 lastIndexWhere methods" in {
    Every(1, 2, 3, 4, 5).lastIndexWhere(_ == 2) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexWhere(_ == 0) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexWhere(_ == 5) shouldBe 4
    Every(1, 2, 3, 3, 5).lastIndexWhere(_ == 3) shouldBe 3
    Every(1).lastIndexWhere(_ == 1) shouldBe 0
    Every(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 3) shouldBe 1
    Every(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 0) shouldBe -1
    Every(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 1) shouldBe 1
  }
  it should "have an lastOption method" in {
    One("hi").lastOption shouldBe Some("hi")
    Many(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an lengthCompare method" in {
    One("hi").lengthCompare(0) should be > 0
    One("hi").lengthCompare(1) shouldEqual 0
    One("hi").lengthCompare(2) should be < 0
    Many(1, 2, 3).lengthCompare(0) should be > 0
    Many(1, 2, 3).lengthCompare(1) should be > 0
    Many(1, 2, 3).lengthCompare(2) should be > 0
    Many(1, 2, 3).lengthCompare(3) shouldEqual 0
    Many(1, 2, 3).lengthCompare(4) should be < 0
  }
  it should "have an inherited lift method" in {
    val liftedOne = One("hi").lift
    liftedOne(0) shouldBe Some("hi")
    liftedOne(1) shouldBe None
    liftedOne(-1) shouldBe None
    val liftedMany = Many(1, 2, 3).lift
    liftedMany(0) shouldBe Some(1)
    liftedMany(1) shouldBe Some(2)
    liftedMany(2) shouldBe Some(3)
    liftedMany(3) shouldBe None
    liftedMany(-1) shouldBe None
  }
  it should "have a map method" in {
    Every(1, 2, 3) map (_ + 1) shouldBe Every(2, 3, 4)
    (for (ele <- Every(1, 2, 3)) yield ele * 2) shouldBe Every(2, 4, 6)
    One(5) map (_ + 3) shouldBe One(8)
    One(8) map (_.toString) shouldBe One("8")
  }
  it should "have a max method" in {
    Every(1, 2, 3, 4, 5).max shouldBe 5
    Every(1).max shouldBe 1
    Every(-1).max shouldBe -1
    Every("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    Every(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    Every(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    Every(1, 2, 3, 4, 5).min shouldBe 1
    Every(1).min shouldBe 1
    Every(-1).min shouldBe -1
    Every("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    Every(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    Every(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a mkString method" in {

    One("hi").mkString shouldBe "hi"
    Many(1, 2, 3).mkString shouldBe "123"

    One("hi").mkString("#") shouldBe "hi"
    Many(1, 2, 3).mkString("#") shouldBe "1#2#3"
    Many(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    One("hi").mkString("<", "#", ">") shouldBe "<hi>"
    Many(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    Many(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    One("hi").nonEmpty shouldBe true
    Many(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have an orElse method, inherited from PartialFunction" in {
    val pf: PartialFunction[Int, Int] = { case i => -i }
    val f = Every(1, 2, 3) orElse pf
    f(0) shouldBe 1
    f(1) shouldBe 2
    f(2) shouldBe 3
    f(3) shouldBe -3
    f(-1) shouldBe 1
  }
  it should "have a padTo method" in {
    Every(1).padTo(0, -1) shouldBe Every(1)
    Every(1).padTo(1, -1) shouldBe Every(1)
    Every(1).padTo(2, -1) shouldBe Every(1, -1)
    Every(1).padTo(3, -1) shouldBe Every(1, -1, -1)
    Every(1, 2, 3).padTo(3, -1) shouldBe Every(1, 2, 3)
    Every(1, 2, 3).padTo(4, -1) shouldBe Every(1, 2, 3, -1)
    Every(1, 2, 3).padTo(5, -1) shouldBe Every(1, 2, 3, -1, -1)
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: every.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a patch method" in {
    Every(1, 2, 3, 4, 5).patch(2, Every(-3, -4), 2) shouldBe Every(1, 2, -3, -4, 5)
    Every(1, 2, 3, 4, 5).patch(2, Every(-3, -4), 5) shouldBe Every(1, 2, -3, -4)
    Every(1, 2, 3, 4, 5).patch(2, Every(-3, -4), 1) shouldBe Every(1, 2, -3, -4, 4, 5)
    Every(1, 2, 3, 4, 5).patch(4, Every(-3, -4), 2) shouldBe Every(1, 2, 3, 4, -3, -4)
    Every(1, 2, 3, 4, 5).patch(5, Every(-3, -4), 2) shouldBe Every(1, 2, 3, 4, 5, -3, -4)
    Every(1, 2, 3, 4, 5).patch(6, Every(-3, -4), 2) shouldBe Every(1, 2, 3, 4, 5, -3, -4)
    Every(1, 2, 3, 4, 5).patch(0, Every(-3, -4), 2) shouldBe Every(-3, -4, 3, 4, 5)
    Every(1, 2, 3, 4, 5).patch(0, Every(-3, -4), 3) shouldBe Every(-3, -4, 4, 5)
  }
  it should "have a permutations method" in {
    Every(1, 2, 3).permutations.toStream shouldBe Stream(Every(1, 2, 3), Every(1, 3, 2), Every(2, 1, 3), Every(2, 3, 1), Every(3, 1, 2), Every(3, 2, 1))
    Every(1).permutations.toStream shouldBe Stream(Every(1))
    Every(1, 2).permutations.toStream shouldBe Stream(Every(1, 2), Every(2, 1))
  }
  it should "have a prefixLength method" in {
    Every(1, 2, 3, 4, 5).prefixLength(_ == 1) shouldBe 1
    Every(1, 2, 3, 4, 5).prefixLength(_ == 2) shouldBe 0
    Every(1, 2, 3, 4, 5).prefixLength(_ <= 2) shouldBe 2
    Every(1, 2, 3, 4, 5).prefixLength(_ <= 10) shouldBe 5
    Every(1, 2, 3, 4, 5).prefixLength(_ <= 4) shouldBe 4
  }
  it should "have a product method" in {
    Every(1, 2, 3).product shouldBe 6
    Every(3).product shouldBe 3
    Every(3, 4, 5).product shouldBe 60
    Every(3, 4, 5).product shouldBe 60
    Every(3.1, 4.2, 5.3).product shouldBe 69.006
  }
  it should "have a reduce method" in {
    Every(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    Every(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    Every(5).reduce(_ + _) shouldBe 5
    Every(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    One(1).reduceLeft(_ + _) shouldBe 1
    One(1).reduceLeft(_ * _) shouldBe 1
    Many(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    Many(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    Every(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    One(1).reduceLeftOption(_ + _) shouldBe Some(1)
    One(1).reduceLeftOption(_ * _) shouldBe Some(1)
    Many(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    Many(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    Every(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    Every(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    Every(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    Every(5).reduceOption(_ + _) shouldBe Some(5)
    Every(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    One(1).reduceRight(_ * _) shouldBe 1
    Many(1, 2, 3).reduceRight(_ + _) shouldBe 6
    Many(1, 2, 3).reduceRight(_ * _) shouldBe 6
    Every(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    One(1).reduceRightOption(_ + _) shouldBe Some(1)
    One(1).reduceRightOption(_ * _) shouldBe Some(1)
    Many(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    Many(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    Every(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a reverse method" in {
    Every(33).reverse shouldBe Every(33)
    Every(33, 34, 35).reverse shouldBe Every(35, 34, 33)
  }
  it should "have a reverseIterator method" in {
    Every(3).reverseIterator.toStream shouldBe Stream(3)
    Every(1, 2, 3).reverseIterator.toList shouldBe Stream(3, 2, 1)
  }
  it should "have a reverseMap method" in {
    Every(3).reverseMap(_ + 1) shouldBe Every(4)
    Every(1, 2, 3).reverseMap(_ + 1) shouldBe Every(4, 3, 2)
  }
  it should "have a runWith method, inherited from PartialFunction" in {

    var x = 0
    val f = Vector(1, 2, 3).runWith(x += _)

    f(0) shouldBe true
    x shouldBe 1

    f(1) shouldBe true
    x shouldBe 3

    f(2) shouldBe true
    x shouldBe 6

    f(3) shouldBe false

    var y = 0
    val g = Vector(3).runWith(y += _)

    g(0) shouldBe true
    y shouldBe 3

    g(0) shouldBe true
    y shouldBe 6
  }
  it should "have a sameElements method that takes a GenIterable" in {
    Every(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5)) shouldBe true
    Every(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4)) shouldBe false
    Every(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5, 6)) shouldBe false
    Every(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 4)) shouldBe false
    Every(3).sameElements(List(1, 2, 3, 4, 5)) shouldBe false
    Every(3).sameElements(List(1)) shouldBe false
    Every(3).sameElements(List(3)) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    Every(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5)) shouldBe true
    Every(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4)) shouldBe false
    Every(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5, 6)) shouldBe false
    Every(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 4)) shouldBe false
    Every(3).sameElements(Every(1, 2, 3, 4, 5)) shouldBe false
    Every(3).sameElements(Every(1)) shouldBe false
    Every(3).sameElements(Every(3)) shouldBe true
  }
  it should "have a scan method" in {
    Every(1).scan(0)(_ + _) shouldBe Every(0, 1)
    Every(1, 2, 3).scan(0)(_ + _) shouldBe Every(0, 1, 3, 6)
    Every(1, 2, 3).scan("z")(_ + _.toString) shouldBe Every("z", "z1", "z12", "z123")
    Every(0).scan("z")(_ + _.toString) shouldBe Every("z", "z0")
  }
  it should "have a scanLeft method" in {
    Every(1).scanLeft(0)(_ + _) shouldBe Every(0, 1)
    Every(1, 2, 3).scanLeft(0)(_ + _) shouldBe Every(0, 1, 3, 6)
    Every(1, 2, 3).scanLeft("z")(_ + _) shouldBe Every("z", "z1", "z12", "z123")
    Every(0).scanLeft("z")(_ + _) shouldBe Every("z", "z0")
  }
  it should "have a scanRight method" in {
    Every(1).scanRight(0)(_ + _) shouldBe Every(1, 0)
    Every(1, 2, 3).scanRight(0)(_ + _) shouldBe Every(6, 5, 3, 0)
    Every(1, 2, 3).scanRight("z")(_ + _) shouldBe Every("123z", "23z", "3z", "z")
    Every(0).scanRight("z")(_ + _) shouldBe Every("0z", "z")
  }
  it should "have a segmentLength method" in {
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 7, 0) shouldBe 0
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ == 7, 0) shouldBe 0
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 0) shouldBe 10
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 1, 0) shouldBe 0
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 10) shouldBe 0
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 8) shouldBe 2
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 3, 0) shouldBe 2
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 5, 0) shouldBe 4
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 0) shouldBe 0
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 5) shouldBe 5
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 4) shouldBe 0
    Every(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 6) shouldBe 4
  }
  // it should "have an seq method" is pending
  it should "have a size method" in {
    Every(5).size shouldBe 1
    Every(1, 2, 3).size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    Every(1).sliding(1).toList shouldBe List(Every(1))
    Every(1).sliding(2).toList shouldBe List(Every(1))
    Every(1, 2, 3).sliding(2).toList shouldBe List(Every(1, 2), Every(2, 3))
    Every(1, 2, 3).sliding(1).toList shouldBe List(Every(1), Every(2), Every(3))
    Every(1, 2, 3).sliding(3).toList shouldBe List(Every(1, 2, 3))
    Every(1, 2, 3, 4, 5).sliding(3).toList shouldBe List(Every(1, 2, 3), Every(2, 3, 4), Every(3, 4, 5))
    Every(1, 2, 3, 4, 5).sliding(2).toList shouldBe List(Every(1, 2), Every(2, 3), Every(3, 4), Every(4, 5))
    Every(1, 2, 3, 4, 5).sliding(1).toList shouldBe List(Every(1), Every(2), Every(3), Every(4), Every(5))
    Every(1, 2, 3, 4, 5).sliding(4).toList shouldBe List(Every(1, 2, 3, 4), Every(2, 3, 4, 5))
    Every(1, 2, 3, 4, 5).sliding(5).toList shouldBe List(Every(1, 2, 3, 4, 5))

    Every(1).sliding(1, 1).toList shouldBe List(Every(1))
    Every(1).sliding(1, 2).toList shouldBe List(Every(1))
    Every(1, 2, 3).sliding(1, 1).toList shouldBe List(Every(1), Every(2), Every(3))
    Every(1, 2, 3).sliding(2, 1).toList shouldBe List(Every(1, 2), Every(2, 3))
    Every(1, 2, 3).sliding(2, 2).toList shouldBe List(Every(1, 2), Every(3))
    Every(1, 2, 3).sliding(3, 2).toList shouldBe List(Every(1, 2, 3))
    Every(1, 2, 3).sliding(3, 1).toList shouldBe List(Every(1, 2, 3))
    Every(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(Every(1, 2, 3), Every(2, 3, 4), Every(3, 4, 5))
    Every(1, 2, 3, 4, 5).sliding(2, 2).toList shouldBe List(Every(1, 2), Every(3, 4), Every(5))
    Every(1, 2, 3, 4, 5).sliding(2, 3).toList shouldBe List(Every(1, 2), Every(4, 5))
    Every(1, 2, 3, 4, 5).sliding(2, 4).toList shouldBe List(Every(1, 2), Every(5))
    Every(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(Every(1, 2, 3), Every(2, 3, 4), Every(3, 4, 5))
    Every(1, 2, 3, 4, 5).sliding(3, 2).toList shouldBe List(Every(1, 2, 3), Every(3, 4, 5))
    Every(1, 2, 3, 4, 5).sliding(3, 3).toList shouldBe List(Every(1, 2, 3), Every(4, 5))
    Every(1, 2, 3, 4, 5).sliding(3, 4).toList shouldBe List(Every(1, 2, 3), Every(5))
  }
  it should "have a sortBy method" in {
    val regFun: String => Int = {
      case "one" => 1
      case "two" => 2
      case "three" => 3
      case "four" => 4
      case "five" => 5
      case "-one" => -1
      case "-two" => -2
      case "-three" => -3
      case "-four" => -4
      case "-five" => -5
    }
    val absFun: String => Int = {
      case "one" => 1
      case "two" => 2
      case "three" => 3
      case "four" => 4
      case "five" => 5
      case "-one" => 1
      case "-two" => 2
      case "-three" => 3
      case "-four" => 4
      case "-five" => 5
    }
    Every("five", "four", "three", "two", "one").sortBy(regFun) shouldBe Every("one", "two", "three", "four", "five")
    Every("two", "one", "four", "five", "three").sortBy(regFun) shouldBe Every("one", "two", "three", "four", "five")
    Every("two", "-one", "four", "-five", "-three").sortBy(regFun) shouldBe Every("-five", "-three", "-one", "two", "four")
    Every("two", "-one", "four", "-five", "-three").sortBy(absFun) shouldBe Every("-one", "two", "-three", "four", "-five")
  }
  it should "have a sortWith method" in {
    Every(1, 2, 3, 4, 5).sortWith(_ > _) shouldBe Every(5, 4, 3, 2, 1)
    Every(2, 1, 4, 5, 3).sortWith(_ > _) shouldBe Every(5, 4, 3, 2, 1)
    Every(2, -1, 4, -5, -3).sortWith(_.abs > _.abs) shouldBe Every(-5, 4, -3, 2, -1)
    Every(2, -1, 4, -5, -3).sortWith(_.abs < _.abs) shouldBe Every(-1, 2, -3, 4, -5)
  }
  it should "have a sorted method" in {
    Every(1, 2, 3, 4, 5).sorted shouldBe Every(1, 2, 3, 4, 5)
    Every(5, 4, 3, 2, 1).sorted shouldBe Every(1, 2, 3, 4, 5)
    Every(2, 1, 4, 5, 3).sorted shouldBe Every(1, 2, 3, 4, 5)
  }
  /*
  it should not have a span method
    scala> Vector(1, 2, 3, 4, 5).span(_ > 10)
    res105: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  it should not have a splitAt method
    scala> Vector(1, 2, 3, 4, 5).splitAt(0)
    res106: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have 2 startsWith methods that take a GenSeq" in {
    Every(1, 2, 3).startsWith(List(1)) shouldBe true
    Every(1, 2, 3).startsWith(List(1, 2)) shouldBe true
    Every(1, 2, 3).startsWith(List(1, 2, 3)) shouldBe true
    Every(1, 2, 3).startsWith(List(1, 2, 3, 4)) shouldBe false
    Every(1).startsWith(List(1, 2, 3, 4)) shouldBe false
    Every(1).startsWith(List(1)) shouldBe true
    Every(1).startsWith(List(2)) shouldBe false

    Every(1).startsWith(List(1), 0) shouldBe true
    Every(1).startsWith(List(1), 1) shouldBe false
    Every(1, 2, 3).startsWith(List(1), 1) shouldBe false
    Every(1, 2, 3).startsWith(List(1), 2) shouldBe false
    Every(1, 2, 3).startsWith(List(2), 2) shouldBe false
    Every(1, 2, 3).startsWith(List(2), 1) shouldBe true
    Every(1, 2, 3).startsWith(List(2, 3), 1) shouldBe true
    Every(1, 2, 3).startsWith(List(1, 2, 3), 1) shouldBe false
    Every(1, 2, 3).startsWith(List(1, 2, 3), 0) shouldBe true
    Every(1, 2, 3, 4, 5).startsWith(List(3, 4), 2) shouldBe true
    Every(1, 2, 3, 4, 5).startsWith(List(3, 4, 5), 2) shouldBe true
    Every(1, 2, 3, 4, 5).startsWith(List(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take an Every" in {
    Every(1, 2, 3).startsWith(Every(1)) shouldBe true
    Every(1, 2, 3).startsWith(Every(1, 2)) shouldBe true
    Every(1, 2, 3).startsWith(Every(1, 2, 3)) shouldBe true
    Every(1, 2, 3).startsWith(Every(1, 2, 3, 4)) shouldBe false
    Every(1).startsWith(Every(1, 2, 3, 4)) shouldBe false
    Every(1).startsWith(Every(1)) shouldBe true
    Every(1).startsWith(Every(2)) shouldBe false

    Every(1).startsWith(Every(1), 0) shouldBe true
    Every(1).startsWith(Every(1), 1) shouldBe false
    Every(1, 2, 3).startsWith(Every(1), 1) shouldBe false
    Every(1, 2, 3).startsWith(Every(1), 2) shouldBe false
    Every(1, 2, 3).startsWith(Every(2), 2) shouldBe false
    Every(1, 2, 3).startsWith(Every(2), 1) shouldBe true
    Every(1, 2, 3).startsWith(Every(2, 3), 1) shouldBe true
    Every(1, 2, 3).startsWith(Every(1, 2, 3), 1) shouldBe false
    Every(1, 2, 3).startsWith(Every(1, 2, 3), 0) shouldBe true
    Every(1, 2, 3, 4, 5).startsWith(Every(3, 4), 2) shouldBe true
    Every(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5), 2) shouldBe true
    Every(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have a stringPrefix method" in {
    Every(1).stringPrefix shouldBe "One"
    Every(1, 2, 3).stringPrefix shouldBe "Many"
  }
  it should "have a sum method" in {
    Every(1).sum shouldBe 1
    Every(5).sum shouldBe 5
    Every(1, 2, 3).sum shouldBe 6
    Every(1, 2, 3, 4, 5).sum shouldBe 15
    Every(1.1, 2.2, 3.3).sum shouldBe 6.6
  }
/*
  it should not have a tail method
    scala> Vector(1).tail
    res7: scala.collection.immutable.Vector[Int] = Vector()

  it should not have a tails method
    scala> Vector(1).tails.toList
    res8: List[scala.collection.immutable.Vector[Int]] = List(Vector(1), Vector())

  it should not have a take method
    scala> Vector(1).take(0)
    res10: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3).take(0)
    res11: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3).take(-1)
    res12: scala.collection.immutable.Vector[Int] = Vector()

  it should not have a takeRight method 
    scala> Vector(1).takeRight(1)
    res13: scala.collection.immutable.Vector[Int] = Vector(1)
    scala> Vector(1).takeRight(0)
    res14: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3).takeRight(0)
    res15: scala.collection.immutable.Vector[Int] = Vector()

  it should not have a takeWhile method
    scala> Vector(1, 2, 3).takeWhile(_ > 10)
    res17: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1).takeWhile(_ > 10)
    res18: scala.collection.immutable.Vector[Int] = Vector()
*/
  it should "have a to method" in {
    Every(1).to[List] shouldBe List(1)
    Every(1, 2, 3).to[List] shouldBe List(1, 2, 3)
    Every(1, 2, 3).to[scala.collection.mutable.ListBuffer] shouldBe ListBuffer(1, 2, 3)
    Every(1, 2, 3).to[Vector] shouldBe Vector(1, 2, 3)
  }
  it should "have a toArray method" in {
    Every(1, 2, 3).toArray should === (Array(1, 2, 3))
    Many("a", "b").toArray should === (Array("a", "b"))
    One(1).toArray should === (Array(1))
  }
  it should "have a toBuffer method" in {
    Every(1, 2, 3).toBuffer should === (Buffer(1, 2, 3))
    Many("a", "b").toBuffer should === (Buffer("a", "b"))
    One(1).toBuffer should === (Buffer(1))
  }
  it should "have a toIndexedSeq method" in {
    Every(1, 2, 3).toIndexedSeq should === (IndexedSeq(1, 2, 3))
    Many("a", "b").toIndexedSeq should === (IndexedSeq("a", "b"))
    One(1).toIndexedSeq should === (IndexedSeq(1))
  }
  it should "have a toIterable method" in {
    Every(1, 2, 3).toIterable should === (Iterable(1, 2, 3))
    Many("a", "b").toIterable should === (Iterable("a", "b"))
    One(1).toIterable should === (Iterable(1))
  }
  it should "have a toIterator method" in {
    Every(1, 2, 3).toIterator.toList should === (Iterator(1, 2, 3).toList)
    Many("a", "b").toIterator.toList should === (Iterator("a", "b").toList)
    One(1).toIterator.toList should === (Iterator(1).toList)
    Every(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    Many("a", "b").toIterator shouldBe an [Iterator[_]]
    One(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    Every(1, 2, 3).toList should === (List(1, 2, 3))
    Many("a", "b").toList should === (List("a", "b"))
    One(1).toList should === (List(1))
  }
  it should "have a toMap method" in {
    Every("1" -> 1, "2" -> 2, "3" -> 3).toMap should === (Map("1" -> 1, "2" -> 2, "3" -> 3))
    Many('A' -> "a", 'B' -> "b").toMap should === (Map('A' -> "a", 'B' -> "b"))
    One("1" -> 1).toMap should === (Map("1" -> 1))
  }
  it should "have a toSeq method" in {
    Every(1, 2, 3).toSeq should === (Seq(1, 2, 3))
    Many("a", "b").toSeq should === (Seq("a", "b"))
    One(1).toSeq should === (Seq(1))
  }
  it should "have a toStream method" in {
    Every(1, 2, 3).toStream should === (Stream(1, 2, 3))
    Many("a", "b").toStream should === (Stream("a", "b"))
    One(1).toStream should === (Stream(1))
  }
  it should "have a toTraversable method" in {
    Every(1, 2, 3).toTraversable should === (Traversable(1, 2, 3))
    Many("a", "b").toTraversable should === (Traversable("a", "b"))
    One(1).toTraversable should === (Traversable(1))
  }
  it should "have a toVector method" in {
    Every(1, 2, 3).toVector should === (Vector(1, 2, 3))
    Many("a", "b").toVector should === (Vector("a", "b"))
    One(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    Every(Every(1, 2, 3), Every(4, 5, 6), Every(7, 8, 9)).transpose shouldBe Every(Every(1, 4, 7), Every(2, 5, 8), Every(3, 6, 9))
    Every(Every(1, 2), Every(3, 4), Every(5, 6), Every(7, 8)).transpose shouldBe Every(Every(1, 3, 5, 7), Every(2, 4, 6, 8))
    Every(Every(1, 2), Every(3, 4), Every(5, 6), Every(7, 8)).transpose.transpose shouldBe Every(Every(1, 2), Every(3, 4), Every(5, 6), Every(7, 8))
    Every(Every(1, 2, 3), Every(4, 5, 6), Every(7, 8, 9)).transpose.transpose shouldBe Every(Every(1, 2, 3), Every(4, 5, 6), Every(7, 8, 9))
  }
  it should "have an unzip method" in {
    Every((1, 2)).unzip shouldBe (Every(1),Every(2))
    Every((1, 2), (3, 4)).unzip shouldBe (Every(1, 3), Every(2, 4))
    Every((1, 2), (3, 4), (5, 6)).unzip shouldBe (Every(1, 3, 5), Every(2, 4, 6))
  }
  it should "have an unzip3 method" in {
    Every((1, 2, 3)).unzip3 shouldBe (Every(1), Every(2), Every(3))
    Every((1, 2, 3), (4, 5, 6)).unzip3 shouldBe (Every(1, 4), Every(2, 5), Every(3, 6))
    Every((1, 2, 3), (4, 5, 6), (7, 8, 9)).unzip3 shouldBe (Every(1, 4, 7), Every(2, 5, 8), Every(3, 6, 9))
  }
  it should "have an updated method" in {
    Every(1).updated(0, 2) shouldBe Every(2)
    an [IndexOutOfBoundsException] should be thrownBy { Every(1).updated(1, 2) }
    Every(1, 1, 1).updated(1, 2) shouldBe Every(1, 2, 1)
    Every(1, 1, 1).updated(2, 2) shouldBe Every(1, 1, 2)
    Every(1, 1, 1).updated(0, 2) shouldBe Every(2, 1, 1)
  }
  /*
  it should not have 2 view methods, because I don't want to support views in Every
  */
/*
  it should not have a zip method
    scala> List(1) zip Nil
    res0: List[(Int, Nothing)] = List()
*/
  it should "have a zipAll method that takes an Iterable" in {

    // Empty on right
    One(1).zipAll(Nil, -1, -2) shouldBe One((1, -2))
    Many(1, 2).zipAll(Nil, -1, -2) shouldBe Many((1, -2), (2, -2))

    // Same length
    One(1).zipAll(List(1), -1, -2) shouldBe One((1, 1))
    Many(1, 2).zipAll(List(1, 2), -1, -2) shouldBe Many((1, 1), (2, 2))

    // Non-empty, longer on right
    One(1).zipAll(List(10, 20), -1, -2) shouldBe Many((1,10), (-1,20))
    Many(1, 2).zipAll(List(10, 20, 30), -1, -2) shouldBe Many((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    Many(1, 2, 3).zipAll(List(10, 20), -1, -2) shouldBe Many((1,10), (2,20), (3,-2))
    Many(1, 2, 3, 4).zipAll(List(10, 20, 30), -1, -2) shouldBe Many((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    One(1).zipAll(Every(1), -1, -2) shouldBe One((1, 1))
    Many(1, 2).zipAll(Every(1, 2), -1, -2) shouldBe Many((1, 1), (2, 2))

    // Non-empty, longer on right
    One(1).zipAll(Every(10, 20), -1, -2) shouldBe Many((1,10), (-1,20))
    Many(1, 2).zipAll(Every(10, 20, 30), -1, -2) shouldBe Many((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    Many(1, 2, 3).zipAll(Every(10, 20), -1, -2) shouldBe Many((1,10), (2,20), (3,-2))
    Many(1, 2, 3, 4).zipAll(Every(10, 20, 30), -1, -2) shouldBe Many((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipWithIndex method" in {
    Every(99).zipWithIndex shouldBe Every((99,0))
    Every(1, 2, 3, 4, 5).zipWithIndex shouldBe Every((1,0), (2,1), (3,2), (4,3), (5,4))
  }
  "A One" can "be widened to an Every type via .asEvery" in {
    One(1).asEvery shouldBe One(1)
  }
  "A Many" can "be widened to an Every type via .asEvery" in {
    Many(1, 2, 3).asEvery shouldBe Many(1, 2, 3)
  }
*/
}

