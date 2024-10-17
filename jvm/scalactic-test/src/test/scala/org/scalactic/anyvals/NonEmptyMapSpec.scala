/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalactic.anyvals

import org.scalactic.ColCompatHelper.Iterable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

import org.scalactic.{Every, One, Many, StringNormalizations}
import org.scalactic.UnitSpec
import org.scalactic.NormalizingEquality

import org.scalatest.CompatParColls.Converters._

class NonEmptyMapSpec extends UnitSpec {
  "A NonEmptyMap" can "be constructed with one element" in {
    val onesie = NonEmptyMap(3 -> "three")
    onesie.size shouldBe 1
    onesie(3) shouldBe "three"
  }
  it can "be constructed with many elements" in {
    val twosie = NonEmptyMap(2 -> "two", 3 -> "three")
    twosie.size shouldBe 2
    twosie(2) shouldBe "two"
    twosie(3) shouldBe "three"
    val threesie = NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")
    threesie.size shouldBe 3
    threesie(1) shouldBe "one"
    threesie(2) shouldBe "two"
    threesie(3) shouldBe "three"
  }
  it can "be constructed from a Iterable via the from method on NonEmptyMap singleton" in {
    NonEmptyMap.from(Map.empty[Int, String]) shouldBe None
    NonEmptyMap.from(Map(1 -> "one")) shouldBe Some(NonEmptyMap(1 -> "one"))
    NonEmptyMap.from(Map(1 -> "one", 2 -> "two", 3 -> "three")) shouldBe Some(NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three"))
    // SKIP-SCALATESTJS,NATIVE-START
    NonEmptyMap.from(Map.empty[Int, String].par) shouldBe None
    NonEmptyMap.from(Map(1 -> "one").par) shouldBe Some(NonEmptyMap(1 -> "one"))
    NonEmptyMap.from(Map(1 -> "one", 2 -> "two", 3 -> "three").par) shouldBe Some(NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three"))
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it can "be deconstructed with NonEmptyMap" in {
    NonEmptyMap(1 -> "one") match {
      case NonEmptyMap((x, y)) =>
        x shouldEqual 1
        y shouldEqual "one"
      case _ => fail()
    }
    NonEmptyMap("hi" -> "hello") match {
      case NonEmptyMap((hi, hello)) =>
        hi shouldEqual "hi"
        hello shouldEqual "hello"
      case _ => fail()
    }
  }
  it can "be deconstructed with Many" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap((x1, x2), (y1, y2), (z1, z2)) => ((x1, x2), (y1, y2), (z1, z2)) shouldEqual (2 -> "two", 3 -> "three", 1 -> "one")
      case _ => fail()
    }
    NonEmptyMap("hi" -> "hello", "there" -> "here") match {
      case NonEmptyMap((s1, s2), (t1, t2)) =>
        (s1, s2) shouldEqual ("there", "here")
        (t1, t2) shouldEqual ("hi", "hello")

      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap((x1, x2), (y1, y2), _) =>
        (x1, x2) shouldEqual (2, "two")
        (y1, y2) shouldEqual (3, "three")

      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") match {
      case NonEmptyMap((x1, x2), (y1, y2), _*) =>
        (x1, x2) shouldEqual (5, "five")
        (y1, y2) shouldEqual (1, "one")

      case _ => fail()
    }
  }
  it can "be deconstructed with Every" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap((x1, x2), (y1, y2), (z1, z2)) =>
        ((x1, x2), (y1, y2), (z1, z2)) shouldEqual ((2, "two"), (3, "three"), (1, "one"))
      case _ => fail()
    }
    NonEmptyMap("hi" -> "hello", "there" -> "here") match {
      case NonEmptyMap((s1, s2), (t1, t2)) =>
        ((s1, s2), (t1, t2)) shouldEqual (("there", "here"), ("hi", "hello"))
      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap(x, y, _) => (x, y) shouldEqual ((2, "two"), (3, "three"))
      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") match {
      case NonEmptyMap((x1, x2), (y1, y2), _*) => ((x1, x2), (y1, y2)) shouldEqual ((5, "five"), (1, "one"))
      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap((x1, x2), _*) => (x1, x2) shouldEqual (2, "two")
      case _ => fail()
    }
    NonEmptyMap("hi" -> "hello") match {
      case NonEmptyMap((hi, hello)) => (hi, hello) shouldEqual ("hi", "hello")
      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap((x1, x2), (y1, y2), (z1, z2)) => ((x1, x2), (y1, y2), (z1, z2)) shouldEqual ((2, "two"), (3, "three"), (1, "one"))
      case _ => fail()
    }
    NonEmptyMap("hi" -> "hello", "there" -> "here") match {
      case NonEmptyMap((s1, s2), (t1, t2)) => ((s1, s2), (t1, t2)) shouldEqual (("there", "here"), ("hi", "hello"))
      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap((x1, x2), (y1, y2), _) => ((x1, x2), (y1, y2)) shouldEqual ((2, "two"), (3, "three"))
      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") match {
      case NonEmptyMap((x1, x2), (y1, y2), _*) => ((x1, x2), (y1, y2)) shouldEqual ((5, "five"), (1, "one"))
      case _ => fail()
    }
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") match {
      case NonEmptyMap((x1, x2), _*) => (x1, x2) shouldEqual (2, "two")
      case _ => fail()
    }
  }
  it should "have an apply method" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")(1) shouldEqual "one"
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")(2) shouldEqual "two"
    NonEmptyMap("hi" -> "hello")("hi") shouldEqual "hello"
    NonEmptyMap(7 -> "seven", 8 -> "eight", 9 -> "nine")(9) shouldEqual "nine"
    the [NoSuchElementException] thrownBy {
      NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")(0)
    } should have message "key not found: 0"
  }
  it should "have a ++ method that takes another NonEmptyMap" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ NonEmptyMap(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ NonEmptyMap(4 -> "four", 5 -> "five") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ NonEmptyMap(4 -> "four", 5 -> "five", 6 -> "six") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six")
  }
  it should "have a ++ method that takes an Every" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ One(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Every(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Every(4 -> "four", 5 -> "five", 6 -> "six") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ One(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ One(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Every(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Every(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ One(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
  }
  it should "have a ++ method that takes a IterableOnce" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Map(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Vector(4 -> "four", 5 -> "five", 6 -> "six") shouldEqual NonEmptyMap(5 -> "five", 6 -> "six", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Iterable(4 -> "four") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Set(4 -> "four", 5 -> "five") shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") ++ Set(4 -> "four", 5 -> "five").iterator shouldEqual NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
  }
  it should "have a +: method" in {
    (0, "zero") +: NonEmptyMap(1 -> "one") shouldBe NonEmptyMap(0 -> "zero", 1 -> "one")
    (0, "zero") +: NonEmptyMap(1 -> "one", 2 -> "two") shouldBe NonEmptyMap(0 -> "zero", 1 -> "one", 2 -> "two")
    ("zero", 0) +: NonEmptyMap("one" -> 1, "two" -> 2) shouldBe NonEmptyMap("zero" -> 0, "one" -> 1, "two" -> 2)
  }
  it should "implement PartialFunction[K, V]" in {
    val pf1: PartialFunction[Int, String] = NonEmptyMap(1 -> "one")
    pf1.isDefinedAt(1) shouldBe true
    pf1.isDefinedAt(0) shouldBe false
  }
  it should "have 3 addString methods" in {
    NonEmptyMap("hi" -> "hello").addString(new StringBuilder) shouldBe new StringBuilder("hi -> hello")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").addString(new StringBuilder) shouldBe new StringBuilder("2 -> two3 -> three1 -> one")

    NonEmptyMap("hi" -> "hello").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi -> hello")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").addString(new StringBuilder, "#") shouldBe new StringBuilder("2 -> two#3 -> three#1 -> one")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").addString(new StringBuilder, ", ") shouldBe new StringBuilder("2 -> two, 3 -> three, 1 -> one")

    NonEmptyMap("hi" -> "hello").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi -> hello>")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<2 -> two#3 -> three#1 -> one>")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 2 -> two, 3 -> three, 1 -> one ) ")
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = NonEmptyMap(1 -> "one") andThen (_ + 1)
    pf1(1) shouldEqual "one1"
    val pf2 = NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three") andThen (_ + 1)
    pf2(1) shouldEqual "one1"
    pf2(2) shouldEqual "two1"
    pf2(3) shouldEqual "three1"
  }
  it should "have an applyOrElse method (inherited from PartialFunction)" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").applyOrElse(0, (_: Int) + " not found") shouldEqual "0 not found"
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").applyOrElse(1, (_: Int) + " not found") shouldEqual "one"
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").applyOrElse(2, (_: Int) + " not found") shouldEqual "two"
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").applyOrElse(3, (_: Int) + " not found") shouldEqual "three"
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").applyOrElse(4, (_: Int) + " not found") shouldEqual "4 not found"
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
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6", 7 -> "7", 8 -> "8", 9 -> "9", 10 -> "10") collectFirst { case (i, _) if i > 10 => i / 2 } shouldBe None
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6", 7 -> "7", 8 -> "8", 9 -> "9", 10 -> "10", 11 -> "11", 12 -> "12") collectFirst { case (i, _) if i > 10 => i / 2 } shouldBe Some(6)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => String = NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").compose(i => i + 1)
    fn(0) shouldBe "one"
    fn(1) shouldBe "two"
    fn(2) shouldBe "three"
  }
  it should "have a contains method" in {
    val e = NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    val es = NonEmptyMap("one" -> 1, "two" -> 2, "three" -> 3)
    es.contains("one") shouldBe true
    es.contains("ONE") shouldBe false

    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.contains("one") shouldBe true
    es.contains("ONE") shouldBe false
    // SKIP-DOTTY-END
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(-1 -> "negative 1")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five").copyToArray(arr1)
    arr1 shouldEqual Array(5 -> "five", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")

    val arr2 = Array.fill(5)(-1 -> "negative 1")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five").copyToArray(arr2, 1)
    arr2 shouldEqual Array(-1 -> "negative 1", 5 -> "five", 1 -> "one", 2 -> "two", 3 -> "three")

    val arr3 = Array.fill(5)(-1 -> "negative 1")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five").copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(-1 -> "negative 1", 5 -> "five", 1 -> "one", -1 -> "negative 1", -1 -> "negative 1")
  }
  it should "have a copyToBuffer method" in {

    val buf = ListBuffer.fill(3)(-1 -> "negative 1")
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five").copyToBuffer(buf)
    buf shouldEqual Buffer(-1 -> "negative 1", -1 -> "negative 1", -1 -> "negative 1", 5 -> "five", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
  }
  it should "have a count method" in {
    val nonEmptyMap = NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
    nonEmptyMap.count(_._1 > 10) shouldBe 0
    nonEmptyMap.count(_._1 % 2 == 0) shouldBe 2
    nonEmptyMap.count(_._1 % 2 == 1) shouldBe 3
  }
  /*
  it should not have a diff method
    scala> Vector(1, 2, 3).diff(Vector(1, 2, 3))
    res0: scala.collection.immutable.Vector[Int] = Vector()
  */

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
  it should "have an equals method" in {
    NonEmptyMap(1 -> "one") shouldEqual NonEmptyMap(1 -> "one")
    NonEmptyMap(1 -> "one") should not equal NonEmptyMap(2 -> "two")
    NonEmptyMap(1 -> "one", 2 -> "two") should not equal NonEmptyMap(2 -> "two", 3 -> "three")
  }
  it should "have an exists method" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").exists(_._1 == 2) shouldBe true
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").exists(_._1 == 5) shouldBe false
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
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").find(_._1 == 5) shouldBe None
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").find(_._1 == 2) shouldBe Some(2 -> "two")
  }
  it should "have a flatMap method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3") flatMap (i => NonEmptyMap(i._1 + 1 -> (i._1 + 1).toString)) shouldBe NonEmptyMap(2 -> "2", 3 -> "3", 4 -> "4")
    val ss = NonEmptyMap("hi" -> "hihi", "ho" -> "hoho")
    val is = NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")
    (for (s <- ss; i <- is) yield (s._1 + i._1, i._1)) shouldBe
      NonEmptyMap(
        ("hi1",1), ("hi2",2), ("hi3",3), ("ho1",1), ("ho2",2), ("ho3",3)
      )
    NonEmptyMap(5 -> "five") flatMap (i => NonEmptyMap(i._1 + 3 -> i._2)) shouldBe NonEmptyMap(8 -> "five")
    NonEmptyMap(8 -> "eight") flatMap (i => NonEmptyMap(i._1.toString -> i._2)) shouldBe NonEmptyMap("8" -> "eight")
  }
  /*
    Can only flatten NonEmptyMaps
    scala> Vector(Set.empty[Int], Set.empty[Int]).flatten
    res17: scala.collection.immutable.Vector[Int] = Vector()
  */
  // TODO: Actually it would make sense to flatten Everys too
  it should "have a fold method" in {
    NonEmptyMap(1 -> "1").fold(0 -> "0"){ case (t1, t2) => (t1._1 + t2._1, t1._2 + t2._2)} shouldBe (1, "01")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").fold(0 -> "0"){ case (t1, t2) => (t1._1 + t2._1, t1._2 + t2._2)} shouldBe (6, "0231")
  }
  it should "have a foldLeft method" in {
    NonEmptyMap(1 -> "1").foldLeft(0)(_ + _._1) shouldBe 1
    NonEmptyMap(1 -> "1").foldLeft(1)(_ + _._1) shouldBe 2
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").foldLeft(0)(_ + _._1) shouldBe 6
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").foldLeft(1)(_ + _._1) shouldBe 7
  }
  it should "have a foldRight method" in {
    NonEmptyMap(1 -> "1").foldRight(0)(_._1 + _) shouldBe 1
    NonEmptyMap(1 -> "1").foldRight(1)(_._1 + _) shouldBe 2
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").foldRight(0)(_._1 + _) shouldBe 6
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").foldRight(1)(_._1 + _) shouldBe 7
  }
  it should "have a forall method" in {
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five").forall(_._1 > 0) shouldBe true
    NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five").forall(_._1 < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3") foreach (num += _._1)
    num shouldBe 6
    for (i <- NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3"))
      num += i._1
    num shouldBe 12
    NonEmptyMap(5 -> "5") foreach (num *= _._1)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").groupBy(_._1 % 2) shouldBe Map(1 -> NonEmptyMap(1 -> "1", 3 -> "3", 5 -> "5"), 0 -> NonEmptyMap(2 -> "2", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 3 -> "3", 3 -> "3").groupBy(_._1 % 2) shouldBe Map(1 -> NonEmptyMap(1 -> "1", 3 -> "3", 3 -> "3", 3 -> "3"), 0 -> NonEmptyMap(2 -> "2"))
    NonEmptyMap(1 -> "1", 1 -> "1", 3 -> "3", 3 -> "3", 3 -> "3").groupBy(_._1 % 2) shouldBe Map(1 -> NonEmptyMap(1 -> "1", 1 -> "1", 3 -> "3", 3 -> "3", 3 -> "3"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 5 -> "5", 7 -> "7").groupBy(_._1 % 2) shouldBe Map(1 -> NonEmptyMap(1 -> "1", 3 -> "3", 5 -> "5", 7 -> "7"), 0 -> NonEmptyMap(2 -> "2"))
  }
  it should "have a grouped method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").grouped(2).toList shouldBe List(NonEmptyMap(2 -> "2", 3 -> "3"), NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").grouped(1).toList shouldBe List(NonEmptyMap(2 -> "2"), NonEmptyMap(3 -> "3"), NonEmptyMap(1 -> "1"))
    an [IllegalArgumentException] should be thrownBy { NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").grouped(0) }
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6", 7 -> "7", 8 -> "8", 9 -> "9", 10 -> "10").grouped(2).toList shouldBe List(NonEmptyMap(5 -> "5", 10 -> "10"), NonEmptyMap(1 -> "1", 6 -> "6"), NonEmptyMap(9 -> "9", 2 -> "2"), NonEmptyMap(7 -> "7", 3 -> "3"), NonEmptyMap(8 -> "8", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6", 7 -> "7", 8 -> "8", 9 -> "9", 10 -> "10").grouped(3).toList shouldBe List(NonEmptyMap(5 -> "5", 10 -> "10", 1 -> "1"), NonEmptyMap(6 -> "6", 9 -> "9", 2 -> "2"), NonEmptyMap(7 -> "7", 3 -> "3", 8 -> "8"), NonEmptyMap(4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6", 7 -> "7", 8 -> "8", 9 -> "9", 10 -> "10").grouped(4).toList shouldBe List(NonEmptyMap(5 -> "5", 10 -> "10", 1 -> "1", 6 -> "6"), NonEmptyMap(9 -> "9", 2 -> "2", 7 -> "7", 3 -> "3"), NonEmptyMap(8 -> "8", 4 -> "4"))
    NonEmptyMap(1 -> "1").grouped(2).toList shouldBe List(NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1").grouped(1).toList shouldBe List(NonEmptyMap(1 -> "1"))
  }
  it should "have a hasDefiniteSize method" in {
    NonEmptyMap(1 -> "1").hasDefiniteSize shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2").hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    NonEmptyMap(1 -> "1").hashCode shouldEqual NonEmptyMap(1 -> "1").hashCode
    NonEmptyMap(1 -> "1", 2 -> "2").hashCode shouldEqual NonEmptyMap(1 -> "1", 2 -> "2").hashCode
  }
  it should "have a head method" in {
    NonEmptyMap("hi" -> "ho").head shouldBe ("hi", "ho")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").head shouldBe (2, "2")
  }
  it should "have a headOption method" in {
    NonEmptyMap("hi" -> "ho").headOption shouldBe Some(("hi", "ho"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").headOption shouldBe Some((2, "2"))
  }
  /*
  it should not have an init method
    scala> Vector(1).init
    res30: scala.collection.immutable.Vector[Int] = Vector()

  it should "have an inits method" is pending
    scala> Vector(1).inits.toMap
    res32: Map[scala.collection.immutable.Vector[Int]] = Map(Vector(1), Vector())

  it should "have an intersect method" is pending
    scala> Vector(1, 2, 3) intersect Vector(4, 5)
    res33: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an isDefinedAt method, inherited from PartialFunction" in {
    NonEmptyMap(1 -> "1").isDefinedAt(0) shouldBe false
    NonEmptyMap(1 -> "1").isDefinedAt(1) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").isDefinedAt(1) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").isDefinedAt(2) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").isDefinedAt(3) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").isDefinedAt(0) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").isDefinedAt(-1) shouldBe false
  }
  it should "have an isEmpty method" in {
    NonEmptyMap("hi" -> "ho").isEmpty shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    NonEmptyMap("hi" -> "ho").isTraversableAgain shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    NonEmptyMap("hi" -> "ho").iterator.toMap shouldBe Map("hi" -> "ho")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").iterator.toMap shouldBe Map(1 -> "1", 2 -> "2", 3 -> "3")
  }
  it should "have a last method" in {
    NonEmptyMap("hi" -> "ho").last shouldBe ("hi", "ho")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").last shouldBe (1, "1")
  }
  it should "have an lastOption method" in {
    NonEmptyMap("hi" -> "ho").lastOption shouldBe Some("hi" -> "ho")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").lastOption shouldBe Some((1 -> "1"))
  }
  it should "have an inherited lift method" in {
    val liftedOne = NonEmptyMap("hi" -> "ho").lift
    liftedOne("hi") shouldBe Some("ho")
    liftedOne("other") shouldBe None
    liftedOne("hello") shouldBe None
    val liftedMany = NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").lift
    liftedMany(1) shouldBe Some("1")
    liftedMany(2) shouldBe Some("2")
    liftedMany(3) shouldBe Some("3")
    liftedMany(0) shouldBe None
    liftedMany(-1) shouldBe None
  }
  it should "have a map method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3") map (e => (e._1 + 1, e._2)) shouldBe NonEmptyMap(3 -> "2", 4 -> "3", 2 -> "1")
    (for (ele <- NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3")) yield ((ele._1 * 2) -> ele._2)) shouldBe NonEmptyMap(2 -> "1", 4 -> "2", 6 -> "3")
    NonEmptyMap(5 -> "5") map (e => (e._1 + 1, e._2)) shouldBe NonEmptyMap(6 -> "5")
    NonEmptyMap(8 -> "8") map (e => (e._1.toString, e._2)) shouldBe NonEmptyMap("8" -> "8")
  }
  it should "have a max method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").max shouldBe (5, "5")
    NonEmptyMap(1 -> "1").max shouldBe (1, "1")
    NonEmptyMap(-1 -> "-1").max shouldBe (-1, "-1")
    NonEmptyMap("aaa" -> "AAA", "ccc" -> "CCC", "bbb" -> "BBB").max shouldBe ("ccc", "CCC")
  }
  it should "have a maxBy method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").maxBy(_._1.abs) shouldBe (5, "5")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", -5 -> "-5").maxBy(_._1.abs) shouldBe (-5, "-5")
  }
  it should "have a min method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").min shouldBe (1, "1")
    NonEmptyMap(1 -> "1").min shouldBe (1, "1")
    NonEmptyMap(-1 -> "-1").min shouldBe (-1, "-1")
    NonEmptyMap("aaa" -> "AAA", "ccc" -> "CCC", "bbb" -> "BBB").min shouldBe ("aaa", "AAA")
  }
  it should "have a minBy method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").minBy(_._1.abs) shouldBe (1 -> "1")
    NonEmptyMap(-1 -> "-1", -2 -> "-2", 3 -> "-3", 4 -> "4", 5 -> "5").minBy(_._1.abs) shouldBe (-1 -> "-1")
  }
  it should "have a mkString method" in {
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    NonEmptyMap("hi" -> "ho").mkString shouldBe "hi -> ho"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").mkString shouldBe "2 -> 23 -> 31 -> 1"
    // SKIP-DOTTY-END

    NonEmptyMap("hi" -> "ho").mkString("#") shouldBe "hi -> ho"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").mkString("#") shouldBe "2 -> 2#3 -> 3#1 -> 1"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").mkString(", ") shouldBe "2 -> 2, 3 -> 3, 1 -> 1"

    NonEmptyMap("hi" -> "ho").mkString("<", "#", ">") shouldBe "<hi -> ho>"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").mkString("<", "#", ">") shouldBe "<2 -> 2#3 -> 3#1 -> 1>"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").mkString(" ( ", ", ", " ) ") shouldBe " ( 2 -> 2, 3 -> 3, 1 -> 1 ) "
  }
  it should "have an nonEmpty method" in {
    NonEmptyMap("hi" -> "ho").nonEmpty shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").nonEmpty shouldBe true
  }
  it should "have an orElse method, inherited from PartialFunction" in {
    val pf: PartialFunction[Int, Int] = { case i => -i }
    val f = NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3") orElse pf
    f(0) shouldBe 0
    f(1) shouldBe "1"
    f(2) shouldBe "2"
    f(3) shouldBe "3"
    f(-1) shouldBe 1
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: nonEmptyMap.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a reduce method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduce { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe (15, "51234")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduce { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe (120, "51234")
    NonEmptyMap(5 -> "5").reduce { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe (5, "5")
    NonEmptyMap(5 -> "5").reduce { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe (5, "5")
  }
  it should "have a reduceLeft method" in {
    NonEmptyMap(1 -> "1").reduceLeft { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe 1 -> "1"
    NonEmptyMap(1 -> "1").reduceLeft { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe 1 -> "1"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceLeft { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe 6 -> "231"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceLeft { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe 6 -> "231"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduceLeft { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe 120 -> "51234"
  }
  it should "have a reduceLeftOption method" in {
    NonEmptyMap(1 -> "1").reduceLeftOption { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe Some(1 -> "1")
    NonEmptyMap(1 -> "1").reduceLeftOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(1 -> "1")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceLeftOption { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe Some(6 -> "231")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceLeftOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(6 -> "231")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduceLeftOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(120 -> "51234")
  }
  it should "have a reduceOption method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduceOption { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe Some(15 -> "51234")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduceOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(120 -> "51234")
    NonEmptyMap(5 -> "5").reduceOption { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe Some(5 -> "5")
    NonEmptyMap(5 -> "5").reduceOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(5 -> "5")
  }
  it should "have a reduceRight method" in {
    NonEmptyMap(1 -> "1").reduceRight { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe 1 -> "1"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceRight { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe 6 -> "231"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceRight { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe 6 -> "231"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduceRight { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe 120 -> "51234"
  }
  it should "have a reduceRightOption method" in {
    NonEmptyMap(1 -> "1").reduceRightOption { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe Some(1 -> "1")
    NonEmptyMap(1 -> "1").reduceRightOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(1 -> "1")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceRightOption { (e1, e2) => (e1._1 + e2._1, e1._2 + e2._2) } shouldBe Some(6 -> "231")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").reduceRightOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(6 -> "231")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").reduceRightOption { (e1, e2) => (e1._1 * e2._1, e1._2 + e2._2) } shouldBe Some(120 -> "51234")
  }
  it should "have a runWith method, inherited from PartialFunction" in {
    // TODO: What is this? Seems to be testing Vector or Map instead of Every or NonEmptyMap.
    var x = 0
    val f = NonEmptyMap("1" -> 1, "2" -> 2, "3" -> 3).runWith(x += _)

    f("0") shouldBe false
    x shouldBe 0

    f("1") shouldBe true
    x shouldBe 1

    f("2") shouldBe true
    x shouldBe 3

    f("3") shouldBe true
    x shouldBe 6

    var y = 0
    val g = NonEmptyMap("3" -> 3).runWith(y += _)

    g("3") shouldBe true
    y shouldBe 3

    g("3") shouldBe true
    y shouldBe 6
  }
  it should "have a sameElements method that takes a GenIterable" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(List(5 -> "5", 1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4")) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(List(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(List(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(List(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(List(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 4 -> "4")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(List(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(List(1 -> "1")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(List(3 -> "3")) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(Every(5 -> "5", 1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4")) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(Every(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(Every(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(Every(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(Every(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 4 -> "4")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(Every(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(Every(1 -> "1")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(Every(3 -> "3")) shouldBe true
  }
  it should "have a sameElements method that takes a NonEmptyMap" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4")) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5")) shouldBe true
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6")) shouldBe false
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sameElements(NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 4 -> "4")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(NonEmptyMap(1 -> "1")) shouldBe false
    NonEmptyMap(3 -> "3").sameElements(NonEmptyMap(3 -> "3")) shouldBe true
  }
  it should "have a scan method" in {
    NonEmptyMap(1 -> "1").scan(0 -> "0")((e1, e2) => (e1._1 + e2._1, e1._2 + e2._2)) shouldBe NonEmptyMap(0 -> "0", 1 -> "01")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").scan(0 -> "0")((e1, e2) => (e1._1 + e2._1, e1._2 + e2._2)) shouldBe NonEmptyMap(0 -> "0", 2 -> "02", 5 -> "023", 6 -> "0231")
  }
  it should "have a scanLeft method" in {
    NonEmptyMap(1 -> "1").scanLeft(0)(_ + _._1) shouldBe List(0, 1)
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").scanLeft(0)(_ + _._1) shouldBe List(0, 2, 5, 6)
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").scanLeft("z")(_.toString + _._2.toString) shouldBe List("z", "z2", "z23", "z231")
    NonEmptyMap(0 -> "0").scanLeft("z")(_ .toString+ _._2.toString) shouldBe List("z", "z0")
  }
  it should "have a scanRight method" in {
    NonEmptyMap(1 -> "1").scanRight(0)(_._1 + _) shouldBe List(1, 0)
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").scanRight(0)(_._1 + _) shouldBe List(6, 4, 1, 0)
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").scanRight("z")(_._2.toString + _.toString) shouldBe List("231z", "31z", "1z", "z")
    NonEmptyMap(0 -> "0").scanRight("z")(_._2.toString + _.toString) shouldBe List("0z", "z")
  }
  // it should "have a seq method" is pending
  it should "have a size method" in {
    NonEmptyMap(5 -> "5").size shouldBe 1
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    NonEmptyMap(1 -> "1").sliding(1).toList shouldBe List(NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1").sliding(2).toList shouldBe List(NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(2).toList shouldBe List(NonEmptyMap(2 -> "2", 3 -> "3"), NonEmptyMap(3 -> "3", 1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(1).toList shouldBe List(NonEmptyMap(2 -> "2"), NonEmptyMap(3 -> "3"), NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(3).toList shouldBe List(NonEmptyMap(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(3).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2"), NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3"), NonEmptyMap(2 -> "2", 3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(2).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1"), NonEmptyMap(1 -> "1", 2 -> "2"), NonEmptyMap(2 -> "2", 3 -> "3"), NonEmptyMap(3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(1).toList shouldBe List(NonEmptyMap(5 -> "5"), NonEmptyMap(1 -> "1"), NonEmptyMap(2 -> "2"), NonEmptyMap(3 -> "3"), NonEmptyMap(4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(4).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2", 3 -> "3"), NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(5).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4"))

    NonEmptyMap(1 -> "1").sliding(1, 1).toList shouldBe List(NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1").sliding(1, 2).toList shouldBe List(NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(1, 1).toList shouldBe List(NonEmptyMap(2 -> "2"), NonEmptyMap(3 -> "3"), NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(2, 1).toList shouldBe List(NonEmptyMap(2 -> "2", 3 -> "3"), NonEmptyMap(3 -> "3", 1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(2, 2).toList shouldBe List(NonEmptyMap(2 -> "2", 3 -> "3"), NonEmptyMap(1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(3, 2).toList shouldBe List(NonEmptyMap(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").sliding(3, 1).toList shouldBe List(NonEmptyMap(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(3, 1).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2"), NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3"), NonEmptyMap(2 -> "2", 3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(2, 2).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1"), NonEmptyMap(2 -> "2", 3 -> "3"), NonEmptyMap(4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(2, 3).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1"), NonEmptyMap(3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(2, 4).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1"), NonEmptyMap(4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(3, 1).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2"), NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3"), NonEmptyMap(2 -> "2", 3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(3, 2).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2"), NonEmptyMap(2 -> "2", 3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(3, 3).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2"), NonEmptyMap(3 -> "3", 4 -> "4"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").sliding(3, 4).toList shouldBe List(NonEmptyMap(5 -> "5", 1 -> "1", 2 -> "2"), NonEmptyMap(4 -> "4"))
  }
  /*
  it should not have a span method
    scala> Vector(1, 2, 3, 4, 5).span(_ > 10)
    res105: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  it should not have a splitAt method
    scala> Vector(1, 2, 3, 4, 5).splitAt(0)
    res106: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a stringPrefix method" in {
    NonEmptyMap(1 -> "1").stringPrefix shouldBe "NonEmptyMap"
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").stringPrefix shouldBe "NonEmptyMap"
  }
  it should "have a sum method" in {

    implicit object TestNumeric extends Numeric[(Int, Int)] {
      def plus(x: (Int, Int), y: (Int, Int)): (Int, Int) = (x._1 + y._1, x._2 + y._2)
      def minus(x: (Int, Int), y: (Int, Int)): (Int, Int) = (x._1 - y._1, x._2 - y._2)
      def times(x: (Int, Int), y: (Int, Int)): (Int, Int) = (x._1 * y._1, x._2 * y._2)
      def negate(x: (Int, Int)): (Int, Int) = (-x._1, -x._2)
      def fromInt(x: Int): (Int, Int) = (x, x)
      def toInt(x: (Int, Int)): Int = x._1
      def toLong(x: (Int, Int)): Long = x._1.toLong
      def toFloat(x: (Int, Int)): Float = x._1.toFloat
      def toDouble(x: (Int, Int)): Double = x._1.toDouble

      def compare(x: (Int, Int), y: (Int, Int)): Int = x._1 compare y._1
      def parseString(str: String): Option[(Int, Int)] = ???  // For scala 2.13, for testing purpose we do not need to implement it here.
    }

    NonEmptyMap(1 -> 1).sum shouldBe 1 -> 1
    NonEmptyMap(5 -> 5).sum shouldBe 5 -> 5
    NonEmptyMap(1 -> 1, 2 -> 2, 3 -> 3).sum shouldBe 6 -> 6
    NonEmptyMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5).sum shouldBe 15 -> 15
  }
  /*
    it should not have a tail method
      scala> Vector(1).tail
      res7: scala.collection.immutable.Vector[Int] = Vector()

    it should not have a tails method
      scala> Vector(1).tails.toMap
      res8: Map[scala.collection.immutable.Vector[Int]] = Map(Vector(1), Vector())

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
    import org.scalactic.ColCompatHelper.Factory._
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").to(scala.collection.mutable.ListBuffer) shouldBe ListBuffer(2 -> "2", 3 -> "3", 1 -> "1")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").to(Vector) shouldBe Vector(2 -> "2", 3 -> "3", 1 -> "1")
  }
  it should "have a toArray method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toArray should === (Array(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toArray should === (Array("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toArray should === (Array(1 -> "1"))
  }
  it should "have a toBuffer method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toBuffer should === (Buffer(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toBuffer should === (Buffer("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toBuffer should === (Buffer(1 -> "1"))
  }
  it should "have a toIndexedSeq method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toIndexedSeq should === (IndexedSeq(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toIndexedSeq should === (IndexedSeq("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toIndexedSeq should === (IndexedSeq(1 -> "1"))
  }
  it should "have a toIterable method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toIterable should === (Map(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toIterable should === (Map("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toIterable should === (Map(1 -> "1"))
  }
  it should "have a toIterator method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toIterator.toMap should === (Iterator(2 -> "2", 3 -> "3", 1 -> "1").toMap)
    NonEmptyMap("a" -> "A", "b" -> "B").toIterator.toMap should === (Iterator("b" -> "B", "a" -> "A").toMap)
    NonEmptyMap(1 -> "1").toIterator.toMap should === (Iterator(1 -> "1").toMap)
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toIterator shouldBe an [Iterator[_]]
    NonEmptyMap("a" -> "A", "b" -> "B").toIterator shouldBe an [Iterator[_]]
    NonEmptyMap(1 -> "1").toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toList should === (List(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toList should === (List("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toList should === (List(1 -> "1"))
  }
  it should "have a toMap method" in {
    NonEmptyMap("1" -> 1, "2" -> 2, "3" -> 3).toMap should === (Map("1" -> 1, "2" -> 2, "3" -> 3))
    NonEmptyMap('A' -> "a", 'B' -> "b").toMap should === (Map('A' -> "a", 'B' -> "b"))
    NonEmptyMap("1" -> 1).toMap should === (Map("1" -> 1))
  }
  it should "have a toSeq method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toSeq should === (Seq(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toSeq should === (Seq("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toSeq should === (Seq(1 -> "1"))
  }
  it should "have a toSet method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toSet should === (Set(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toSet should === (Set("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toSet should === (Set(1 -> "1"))
  }
  it should "have a toStream method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toStream should === (Stream(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toStream should === (Stream("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toStream should === (Stream(1 -> "1"))
  }
  it should "have a toString method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toString should === ("NonEmptyMap(2 -> 2, 3 -> 3, 1 -> 1)")
    NonEmptyMap(1 -> "1").toString should === ("NonEmptyMap(1 -> 1)")
  }
  it should "have a toVector method" in {
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").toVector should === (Vector(2 -> "2", 3 -> "3", 1 -> "1"))
    NonEmptyMap("a" -> "A", "b" -> "B").toVector should === (Vector("b" -> "B", "a" -> "A"))
    NonEmptyMap(1 -> "1").toVector should === (Vector(1 -> "1"))
  }
  it should "have an unzip method" in {
    NonEmptyMap(1 -> "1", 2 -> "2").unzip shouldBe (List(2, 1), List("2", "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4").unzip shouldBe (List(2, 3, 4, 1), List("2", "3", "4", "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6").unzip shouldBe (List(5, 1, 6, 2, 3, 4), List("5", "1", "6", "2", "3", "4"))
  }
  it should "have an unzip3 method" in {
    import scala.language.implicitConversions
    implicit def convertEntryToTuple3(e: (Int, String)): (Int, Int, String) = (e._1, e._1 * 2, e._2)
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").unzip3 shouldBe (List(2, 3, 1), List(4, 6, 2),  List("2", "3", "1"))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5", 6 -> "6").unzip3 shouldBe (List(5, 1, 6, 2, 3, 4), List(10, 2, 12, 4, 6, 8), List("5", "1", "6", "2", "3", "4"))
  }
  it should "have an updated method" in {
    NonEmptyMap(1 -> "1").updated(1, "one") shouldBe NonEmptyMap(1 -> "one")
    NonEmptyMap(1 -> "1").updated(2, "two") shouldBe NonEmptyMap(1 -> "1", 2 -> "two")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").updated(1, "one") shouldBe NonEmptyMap(1 -> "one", 2 -> "2", 3 -> "3")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").updated(2, "two") shouldBe NonEmptyMap(1 -> "1", 2 -> "two", 3 -> "3")
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").updated(3, "three") shouldBe NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "three")
  }
  /*
  it should not have 2 view methods, because I don't want to support views in Every
  */
  /*
    it should not have a zip method
      scala> Map(1) zip Nil
      res0: Map[(Int, Nothing)] = Map()
  */
  it should "have a zipAll method that takes an Iterable" in {

    // Empty on right
    NonEmptyMap(1 -> "1").zipAll(Nil, -1 -> "-1", -2) shouldBe NonEmptyMap((1, "1") -> -2)
    NonEmptyMap(1 -> "1", 2 -> "2").zipAll(Nil, -1 -> "-1", -2) shouldBe NonEmptyMap((1, "1") -> -2, (2, "2") -> -2)

    // Same length
    NonEmptyMap(1 -> "1").zipAll(List(1), -1 -> "-1", -2) shouldBe NonEmptyMap((1, "1") -> 1)
    NonEmptyMap(1 -> "1", 2 -> "2").zipAll(List(1, 2), -1 -> "-1", -2) shouldBe NonEmptyMap((2, "2") -> 1, (1, "1") -> 2)

    // Non-empty, longer on right
    NonEmptyMap(1 -> "1").zipAll(List(10, 20), -1 -> "-1", -2) shouldBe NonEmptyMap((1 -> "1", 10), (-1 -> "-1", 20))
    NonEmptyMap(1 -> "1", 2 -> "2").zipAll(List(10, 20, 30), -1 -> "-1", -2) shouldBe NonEmptyMap((2 -> "2", 10), (1 -> "1", 20), (-1 -> "-1", 30))

    // Non-empty, shorter on right
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").zipAll(List(10, 20), -1 -> "-1", -2) shouldBe NonEmptyMap((2 -> "2", 10), (3 -> "3", 20), (1 -> "1", -2))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4").zipAll(List(10, 20, 30), -1 -> "-1", -2) shouldBe NonEmptyMap((2 -> "2", 10), (3 -> "3", 20), (4 -> "4", 30), (1 -> "1", -2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    NonEmptyMap(1 -> "1").zipAll(Every(1), -1 -> "-1", -2) shouldBe NonEmptyMap((1, "1") -> 1)
    NonEmptyMap(1 -> "1", 2 -> "2").zipAll(Every(1, 2), -1 -> "-1", -2) shouldBe NonEmptyMap((2, "2") -> 1, (1, "1") -> 2)

    // Non-empty, longer on right
    NonEmptyMap(1 -> "1").zipAll(Every(10, 20), -1 -> "-1", -2) shouldBe NonEmptyMap((1 -> "1", 10), (-1 -> "-1", 20))
    NonEmptyMap(1 -> "1", 2 -> "2").zipAll(Every(10, 20, 30), -1 -> "-1", -2) shouldBe NonEmptyMap((2 -> "2", 10), (1 -> "1", 20), (-1 -> "-1", 30))

    // Non-empty, shorter on right
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").zipAll(Every(10, 20), -1 -> "-1", -2) shouldBe NonEmptyMap((2 -> "2", 10), (3 -> "3", 20), (1 -> "1", -2))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4").zipAll(Every(10, 20, 30), -1 -> "-1", -2) shouldBe NonEmptyMap((2 -> "2", 10), (3 -> "3", 20), (4 -> "4", 30), (1 -> "1", -2))
  }
  it should "have a zipAll method that takes a NonEmptyMap" in {

    // Same length
    NonEmptyMap(1 -> "1").zipAll(NonEmptyMap(1 -> "one"), -1 -> "-1", -2 -> "-two") shouldBe NonEmptyMap((1, "1") -> (1, "one"))
    NonEmptyMap(1 -> "1", 2 -> "2").zipAll(NonEmptyMap(1 -> "one", 2 -> "two"), -1 -> "-1", -2 -> "-two") shouldBe NonEmptyMap((2, "2") -> (2, "two"), (1, "1") -> (1, "one"))

    // Non-empty, longer on right
    NonEmptyMap(1 -> "1").zipAll(NonEmptyMap(10 -> "ten", 20 -> "twenty"), -1 -> "-1", -2 -> "-two") shouldBe NonEmptyMap((1 -> "1", (20, "twenty")), (-1 -> "-1", (10, "ten")))
    NonEmptyMap(1 -> "1", 2 -> "2").zipAll(NonEmptyMap(10 -> "ten", 20 -> "twenty", 30 -> "thirty"), -1 -> "-1", -2 -> "-two") shouldBe NonEmptyMap((2 -> "2", (20, "twenty")), (1 -> "1", (30, "thirty")), (-1 -> "-1", (10, "ten")))

    // Non-empty, shorter on right
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3").zipAll(NonEmptyMap(10 -> "ten", 20 -> "twenty"), -1 -> "-1", -2 -> "-two") shouldBe NonEmptyMap((2 -> "2", (20, "twenty")), (3 -> "3", (10, "ten")), (1 -> "1", (-2, "-two")))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4").zipAll(NonEmptyMap(10 -> "ten", 20 -> "twenty", 30 -> "thirty"), -1 -> "-1", -2 -> "-two") shouldBe NonEmptyMap((2 -> "2", (20, "twenty")), (3 -> "3", (30, "thirty")), (4 -> "4", (10, "ten")), (1 -> "1", (-2, "-two")))
  }
  it should "have a zipWithIndex method" in {
    NonEmptyMap(99 -> "99").zipWithIndex shouldBe NonEmptyMap((99 -> "99", 0))
    NonEmptyMap(1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5 -> "5").zipWithIndex shouldBe NonEmptyMap((5 -> "5", 0), (1 -> "1", 1), (2 -> "2", 2), (3 -> "3", 3), (4 -> "4", 4))
  }
}
