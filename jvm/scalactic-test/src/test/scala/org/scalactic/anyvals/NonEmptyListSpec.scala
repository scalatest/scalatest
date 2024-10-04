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

class NonEmptyListSpec extends UnitSpec {
  "A NonEmptyList" can "be constructed with one element" in {
    val onesie = NonEmptyList(3)
    onesie.length shouldBe 1
    onesie(0) shouldBe 3
  }
  it can "be constructed with many elements" in {
    val twosie = NonEmptyList(2, 3)
    twosie.length shouldBe 2
    twosie(0) shouldBe 2
    twosie(1) shouldBe 3
    val threesie = NonEmptyList(1, 2, 3)
    threesie.length shouldBe 3
    threesie(0) shouldBe 1
    threesie(1) shouldBe 2
    threesie(2) shouldBe 3
  }
  it can "be constructed from a Iterable via the from method on NonEmptyList singleton" in {
    NonEmptyList.from(List.empty[String]) shouldBe None
    NonEmptyList.from(List("1")) shouldBe Some(NonEmptyList("1"))
    NonEmptyList.from(List(1, 2, 3)) shouldBe Some(NonEmptyList(1, 2, 3))
    // SKIP-SCALATESTJS,NATIVE-START
    NonEmptyList.from(List.empty[String].par) shouldBe None
    NonEmptyList.from(List("1").par) shouldBe Some(NonEmptyList("1"))
    NonEmptyList.from(List(1, 2, 3).par) shouldBe Some(NonEmptyList(1, 2, 3))
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it can "be constructed with null elements" in {
    noException should be thrownBy NonEmptyList("hi", null, "ho")
    noException should be thrownBy NonEmptyList(null)
    noException should be thrownBy NonEmptyList("ho", null)
  }
  it can "be constructed using cons-End style" in {
    0 :: 1 :: End shouldBe NonEmptyList(0, 1)
    0 :: 1 ::  2 :: End shouldBe NonEmptyList(0, 1, 2)
    "zero" :: "one" ::  "two" :: End shouldBe NonEmptyList("zero", "one", "two")
  }
  it can "be deconstructed with NonEmptyList" in {
    NonEmptyList(1) match {
      case NonEmptyList(x) => x shouldEqual 1
      case _ => fail()
    }
    NonEmptyList("hi") match {
      case NonEmptyList(s) => s shouldEqual "hi"
      case _ => fail()
    }
  }
  it can "be deconstructed with Many" in {
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyList("hi", "there") match {
      case NonEmptyList(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyList(1, 2, 3, 4, 5) match {
      case NonEmptyList(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
  }
  it can "be deconstructed with Every" in {
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyList("hi", "there") match {
      case NonEmptyList(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyList(1, 2, 3, 4, 5) match {
      case NonEmptyList(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, _*) => x shouldEqual 1
      case _ => fail()
    }
    NonEmptyList("hi") match {
      case NonEmptyList(s) => s shouldEqual "hi"
      case _ => fail()
    }
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyList("hi", "there") match {
      case NonEmptyList(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyList(1, 2, 3, 4, 5) match {
      case NonEmptyList(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyList(1, 2, 3) match {
      case NonEmptyList(x, _*) => x shouldEqual 1
      case _ => fail()
    }
  }
  it should "have an apply method" in {
    NonEmptyList(1, 2, 3)(0) shouldEqual 1
    NonEmptyList(1, 2, 3)(1) shouldEqual 2
    NonEmptyList("hi")(0) shouldEqual "hi"
    NonEmptyList(7, 8, 9)(2) shouldEqual 9
    the [IndexOutOfBoundsException] thrownBy {
      NonEmptyList(1, 2, 3)(3)
    } should have message "3"
  }
  it should "have a length method" in {
    NonEmptyList(1).length shouldBe 1
    NonEmptyList(1, 2).length shouldBe 2
    NonEmptyList(1, 2, 3, 4, 5).length shouldBe 5
  }
  it should "have a ++ method that takes another NonEmptyList" in {
    NonEmptyList(1, 2, 3) ++ NonEmptyList(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ NonEmptyList(4, 5) shouldEqual NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(1, 2, 3) ++ NonEmptyList(4, 5, 6) shouldEqual NonEmptyList(1, 2, 3, 4, 5, 6)
  }
  it should "have a ++ method that takes an Every" in {
    NonEmptyList(1, 2, 3) ++ One(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ Every(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ Every(4, 5, 6) shouldEqual NonEmptyList(1, 2, 3, 4, 5, 6)
    NonEmptyList(1, 2, 3) ++ One(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ One(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ Every(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ Every(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ One(4) shouldEqual NonEmptyList(1, 2, 3, 4)
  }
  it should "have a ++ method that takes a IterableOnce" in {
    NonEmptyList(1, 2, 3) ++ List(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ Vector(4, 5, 6) shouldEqual NonEmptyList(1, 2, 3, 4, 5, 6)
    NonEmptyList(1, 2, 3) ++ Iterable(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ++ Set(4, 5) shouldEqual NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(1, 2, 3) ++ Set(4, 5).iterator shouldEqual NonEmptyList(1, 2, 3, 4, 5)
  }
  it should "have a +: method" in {
    0 +: NonEmptyList(1) shouldBe NonEmptyList(0, 1)
    0 +: NonEmptyList(1, 2) shouldBe NonEmptyList(0, 1, 2)
    "zero" +: NonEmptyList("one", "two") shouldBe NonEmptyList("zero", "one", "two")
  }
  it should "have a :: method" in {

    0 :: NonEmptyList(1) shouldBe NonEmptyList(0, 1)
    0 :: NonEmptyList(1, 2) shouldBe NonEmptyList(0, 1, 2)
    "zero" :: NonEmptyList("one", "two") shouldBe NonEmptyList("zero", "one", "two")
  }
  it should "have a ::: method that takes another NonEmptyList" in {
    NonEmptyList(1, 2, 3) ::: NonEmptyList(4) shouldEqual NonEmptyList(1, 2, 3, 4)
    NonEmptyList(1, 2, 3) ::: NonEmptyList(4, 5) shouldEqual NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(1, 2, 3) ::: NonEmptyList(4, 5, 6) shouldEqual NonEmptyList(1, 2, 3, 4, 5, 6)
  }
  it should "have a ::: method that takes an Every" in {
    One(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    Every(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    Every(1, 2, 3) ::: NonEmptyList(4, 5, 6) shouldEqual NonEmptyList(1, 2, 3, 4, 5, 6)
    One(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    One(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    Every(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    Every(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    One(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
  }
  it should "have a ::: method that takes a IterableOnce" in {
    List(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    Vector(1, 2, 3) ::: NonEmptyList(4, 5, 6) shouldEqual NonEmptyList(1, 2, 3, 4, 5, 6)
    Iterable(1) ::: NonEmptyList(2, 3, 4) shouldEqual NonEmptyList(1, 2, 3, 4)
    Set(1, 2) ::: NonEmptyList(3, 4, 5) shouldEqual NonEmptyList(1, 2, 3, 4, 5)
    Set(1, 2).iterator ::: NonEmptyList(3, 4, 5) shouldEqual NonEmptyList(1, 2, 3, 4, 5)
  }

  it should "implement PartialFunction[Int, T]" in {
    val pf1: PartialFunction[Int, Int] = NonEmptyList(1)
    pf1.isDefinedAt(0) shouldBe true
    pf1.isDefinedAt(1) shouldBe false
  }
  it should "have a /: method" in {
    (0 /: NonEmptyList(1))(_ + _) shouldBe 1
    (1 /: NonEmptyList(1))(_ + _) shouldBe 2
    (0 /: NonEmptyList(1, 2, 3))(_ + _) shouldBe 6
    (1 /: NonEmptyList(1, 2, 3))(_ + _) shouldBe 7
  }
  it should "have a :+ method" in {
    NonEmptyList(1) :+ 2 shouldBe NonEmptyList(1, 2)
    NonEmptyList(1, 2) :+ 3 shouldBe NonEmptyList(1, 2, 3)
  }
  it should "have a :\\ method" in {
    (NonEmptyList(1) :\ 0)(_ + _) shouldBe 1
    (NonEmptyList(1) :\ 1)(_ + _) shouldBe 2
    (NonEmptyList(1, 2, 3) :\ 0)(_ + _) shouldBe 6
    (NonEmptyList(1, 2, 3) :\ 1)(_ + _) shouldBe 7
  }
  it should "have 3 addString methods" in {
    NonEmptyList("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    NonEmptyList(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    NonEmptyList("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    NonEmptyList(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    NonEmptyList(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    NonEmptyList("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    NonEmptyList(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    NonEmptyList(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = NonEmptyList(1) andThen (_ + 1)
    pf1(0) shouldEqual 2
    val pf2 = NonEmptyList(1, 2, 3) andThen (_ + 1)
    pf2(0) shouldEqual 2
    pf2(1) shouldEqual 3
    pf2(2) shouldEqual 4
  }
  it should "have an applyOrElse method (inherited from PartialFunction)" in {
    NonEmptyList(1, 2, 3).applyOrElse(0, (_: Int) * -1) shouldEqual 1
    NonEmptyList(1, 2, 3).applyOrElse(1, (_: Int) * -1) shouldEqual 2
    NonEmptyList(1, 2, 3).applyOrElse(2, (_: Int) * -1) shouldEqual 3
    NonEmptyList(1, 2, 3).applyOrElse(3, (_: Int) * -1) shouldEqual -3
    NonEmptyList(1, 2, 3).applyOrElse(4, (_: Int) * -1) shouldEqual -4
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
    NonEmptyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collectFirst { case i if i > 10 => i / 2 } shouldBe None
    NonEmptyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) collectFirst { case i if i > 10 => i / 2 } shouldBe Some(5)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Int = NonEmptyList(1, 2, 3).compose((_: Int) + 1)
    fn(-1) shouldBe 1
    fn(0) shouldBe 2
    fn(1) shouldBe 3
  }
  it should "have a contains method" in {
    val e = NonEmptyList(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    val es = NonEmptyList("one", "two", "three")
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
  // Decided to just overload one for GenSeq and one for Every. Could have done
  // what that has a Slicing nature, but that's a bit too fancy pants.
  it should "have a containsSlice method that takes GenSeq" in {
    val nonEmptyList = NonEmptyList(1, 2, 3, 4, 5)
    nonEmptyList.containsSlice(List(2, 3)) shouldBe true
    nonEmptyList.containsSlice(List(2, 3, 5)) shouldBe false
    nonEmptyList.containsSlice(List.empty) shouldBe true
    nonEmptyList.containsSlice(Vector(2, 3)) shouldBe true
    nonEmptyList.containsSlice(Vector(2, 3, 5)) shouldBe false
    nonEmptyList.containsSlice(Vector.empty) shouldBe true
    nonEmptyList.containsSlice(ListBuffer(2, 3)) shouldBe true
    nonEmptyList.containsSlice(ListBuffer(2, 3, 5)) shouldBe false
    nonEmptyList.containsSlice(ListBuffer.empty) shouldBe true
  }
  it should "have a containsSlice method that takes an Every" in {
    val nonEmptyList = NonEmptyList(1, 2, 3, 4, 5)
    nonEmptyList.containsSlice(Every(2, 3)) shouldBe true
    nonEmptyList.containsSlice(Every(2, 3, 5)) shouldBe false
    nonEmptyList.containsSlice(Every(3)) shouldBe true
  }
  it should "have a containsSlice method that takes a NonEmptyList" in {
    val nonEmptyList = NonEmptyList(1, 2, 3, 4, 5)
    nonEmptyList.containsSlice(NonEmptyList(2, 3)) shouldBe true
    nonEmptyList.containsSlice(NonEmptyList(2, 3, 5)) shouldBe false
    nonEmptyList.containsSlice(NonEmptyList(3)) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(-1)
    NonEmptyList(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(1, 2, 3, 4, 5)

    val arr2 = Array.fill(5)(-1)
    NonEmptyList(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(-1, 1, 2, 3, 4)

    val arr3 = Array.fill(5)(-1)
    NonEmptyList(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(-1, 1, 2, -1, -1)
  }
  it should "have a copyToBuffer method" in {

    val buf = ListBuffer.fill(3)(-1)
    NonEmptyList(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(-1, -1, -1, 1, 2, 3, 4, 5)
  }
  it should "have a corresponds method that takes a GenSeq" in {
    val nonEmptyList = NonEmptyList(1, 2, 3, 4, 5)
    nonEmptyList.corresponds(List(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyList.corresponds(List(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyList.corresponds(List(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyList.corresponds(List(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes an Every" in {
    val nonEmptyList = NonEmptyList(1, 2, 3, 4, 5)
    nonEmptyList.corresponds(Many(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyList.corresponds(Many(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyList.corresponds(Many(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyList.corresponds(Many(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes a NonEmptyList" in {
    val nonEmptyList = NonEmptyList(1, 2, 3, 4, 5)
    nonEmptyList.corresponds(NonEmptyList(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyList.corresponds(NonEmptyList(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyList.corresponds(NonEmptyList(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyList.corresponds(NonEmptyList(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a count method" in {
    val nonEmptyList = NonEmptyList(1, 2, 3, 4, 5)
    nonEmptyList.count(_ > 10) shouldBe 0
    nonEmptyList.count(_ % 2 == 0) shouldBe 2
    nonEmptyList.count(_ % 2 == 1) shouldBe 3
  }
  /*
  it should not have a diff method
    scala> Vector(1, 2, 3).diff(Vector(1, 2, 3))
    res0: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a distinct method" in {
    NonEmptyList(1, 2, 3).distinct shouldBe NonEmptyList(1, 2, 3)
    NonEmptyList(1).distinct shouldBe NonEmptyList(1)
    NonEmptyList(1, 2, 1, 1).distinct shouldBe NonEmptyList(1, 2)
    NonEmptyList(1, 1, 1).distinct shouldBe NonEmptyList(1)
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
    NonEmptyList(1).endsWith(List(1)) shouldBe true
    NonEmptyList(1).endsWith(List(1, 2)) shouldBe false
    NonEmptyList(1, 2).endsWith(List(1, 2)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).endsWith(List(1, 2)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).endsWith(List(5)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).endsWith(List(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes an Every" in {
    NonEmptyList(1).endsWith(Every(1)) shouldBe true
    NonEmptyList(1).endsWith(Every(1, 2)) shouldBe false
    NonEmptyList(1, 2).endsWith(Every(1, 2)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).endsWith(Every(1, 2)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).endsWith(Every(5)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).endsWith(Every(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes a NonEmptyList" in {
    NonEmptyList(1).endsWith(NonEmptyList(1)) shouldBe true
    NonEmptyList(1).endsWith(NonEmptyList(1, 2)) shouldBe false
    NonEmptyList(1, 2).endsWith(NonEmptyList(1, 2)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).endsWith(NonEmptyList(1, 2)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).endsWith(NonEmptyList(5)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).endsWith(NonEmptyList(3, 4, 5)) shouldBe true
  }
  it should "have an equals method" in {
    NonEmptyList(1) shouldEqual NonEmptyList(1)
    NonEmptyList(1) should not equal NonEmptyList(2)
    NonEmptyList(1, 2) should not equal NonEmptyList(2, 3)
  }
  it should "have an exists method" in {
    NonEmptyList(1, 2, 3).exists(_ == 2) shouldBe true
    NonEmptyList(1, 2, 3).exists(_ == 5) shouldBe false
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
    NonEmptyList(1, 2, 3).find(_ == 5) shouldBe None
    NonEmptyList(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have a flatMap method" in {
    NonEmptyList(1, 2, 3) flatMap (i => NonEmptyList(i + 1)) shouldBe NonEmptyList(2, 3, 4)
    val ss = NonEmptyList("hi", "ho")
    val is = NonEmptyList(1, 2, 3)
    (for (s <- ss; i <- is) yield (s, i)) shouldBe
      NonEmptyList(
        ("hi",1), ("hi",2), ("hi",3), ("ho",1), ("ho",2), ("ho",3)
      )
    NonEmptyList(5) flatMap (i => NonEmptyList(i + 3)) shouldBe NonEmptyList(8)
    NonEmptyList(8) flatMap (i => NonEmptyList(i.toString)) shouldBe NonEmptyList("8")
  }
  /*
    Can only flatten NonEmptyLists
    scala> Vector(Set.empty[Int], Set.empty[Int]).flatten
    res17: scala.collection.immutable.Vector[Int] = Vector()
  */
  // TODO: Actually it would make sense to flatten Everys too
  it should "have a flatten method that works on nested NonEmptyLists" in {
    NonEmptyList(NonEmptyList(1, 2, 3), NonEmptyList(1, 2, 3)).flatten shouldBe NonEmptyList(1, 2, 3, 1, 2, 3)
    NonEmptyList(NonEmptyList(1)).flatten shouldBe NonEmptyList(1)
  }
  it can "be flattened when in a IterableOnce" in {
    Vector(NonEmptyList(1, 2, 3), NonEmptyList(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    List(NonEmptyList(1, 2, 3), NonEmptyList(1, 2, 3)).flatten shouldBe List(1, 2, 3, 1, 2, 3)
    List(NonEmptyList(1, 2, 3), NonEmptyList(1, 2, 3)).toIterator.flatten.toStream shouldBe List(1, 2, 3, 1, 2, 3).toIterator.toStream
    // SKIP-SCALATESTJS,NATIVE-START
    List(NonEmptyList(1, 2, 3), NonEmptyList(1, 2, 3)).par.flatten shouldBe List(1, 2, 3, 1, 2, 3).par
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it should "have a fold method" in {
    NonEmptyList(1).fold(0)(_ + _) shouldBe 1
    NonEmptyList(1).fold(1)(_ * _) shouldBe 1
    NonEmptyList(2).fold(0)(_ + _) shouldBe 2
    NonEmptyList(2).fold(1)(_ * _) shouldBe 2
    NonEmptyList(3).fold(0)(_ + _) shouldBe 3
    NonEmptyList(3).fold(1)(_ * _) shouldBe 3
    NonEmptyList(1, 2, 3).fold(0)(_ + _) shouldBe 6
    NonEmptyList(1, 2, 3).fold(1)(_ * _) shouldBe 6
    NonEmptyList(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    NonEmptyList(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    NonEmptyList(1).foldLeft(0)(_ + _) shouldBe 1
    NonEmptyList(1).foldLeft(1)(_ + _) shouldBe 2
    NonEmptyList(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    NonEmptyList(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    NonEmptyList(1).foldRight(0)(_ + _) shouldBe 1
    NonEmptyList(1).foldRight(1)(_ + _) shouldBe 2
    NonEmptyList(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    NonEmptyList(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    NonEmptyList(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    NonEmptyList(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- NonEmptyList(1, 2, 3))
      num += i
    num shouldBe 12
    NonEmptyList(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    NonEmptyList(1, 2, 3, 4, 5).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyList(1, 3, 5), 0 -> NonEmptyList(2, 4))
    NonEmptyList(1, 2, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyList(1, 3, 3, 3), 0 -> NonEmptyList(2))
    NonEmptyList(1, 1, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyList(1, 1, 3, 3, 3))
    NonEmptyList(1, 2, 3, 5, 7).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyList(1, 3, 5, 7), 0 -> NonEmptyList(2))
  }
  it should "have a grouped method" in {
    NonEmptyList(1, 2, 3).grouped(2).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(3))
    NonEmptyList(1, 2, 3).grouped(1).toList shouldBe List(NonEmptyList(1), NonEmptyList(2), NonEmptyList(3))
    an [IllegalArgumentException] should be thrownBy { NonEmptyList(1, 2, 3).grouped(0).toList }
    NonEmptyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(3, 4), NonEmptyList(5, 6), NonEmptyList(7, 8), NonEmptyList(9, 10))
    NonEmptyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toList shouldBe List(NonEmptyList(1, 2, 3), NonEmptyList(4, 5, 6), NonEmptyList(7, 8, 9), NonEmptyList(10))
    NonEmptyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toList shouldBe List(NonEmptyList(1, 2, 3, 4), NonEmptyList(5, 6, 7, 8), NonEmptyList(9, 10))
    NonEmptyList(1).grouped(2).toList shouldBe List(NonEmptyList(1))
    NonEmptyList(1).grouped(1).toList shouldBe List(NonEmptyList(1))
  }
  it should "have a hasDefiniteSize method" in {
    NonEmptyList(1).hasDefiniteSize shouldBe true
    NonEmptyList(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    NonEmptyList(1).hashCode shouldEqual NonEmptyList(1).hashCode
    NonEmptyList(1, 2).hashCode shouldEqual NonEmptyList(1, 2).hashCode
  }
  it should "have a head method" in {
    NonEmptyList("hi").head shouldBe "hi"
    NonEmptyList(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    NonEmptyList("hi").headOption shouldBe Some("hi")
    NonEmptyList(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have 2 indexOf methods" in {
    NonEmptyList(1, 2, 3, 4, 5).indexOf(3) shouldBe 2
    NonEmptyList(1, 2, 3, 4, 5).indexOf(1) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).indexOf(1, 2) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).indexOf(6) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).indexOf(5, 3) shouldBe 4

    val es = NonEmptyList("one", "two", "three")
    es.indexOf("one") shouldBe 0
    es.indexOf("one", 1) shouldBe -1
    es.indexOf("ONE") shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOf("one") shouldBe 0
    es.indexOf("ONE") shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take a GenSeq" in {
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(2, 3)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(2, 3))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(2, 3), 3) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(2, 3), 3)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5), 3) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5), 3)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 0)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 1)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), -1)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List.empty) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List.empty)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List.empty, 6) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List.empty, 6)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(List.empty, 4) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(List.empty, 4)

    val es = NonEmptyList("one", "two", "three", "four", "five")
    val el = List("one", "two", "three", "four", "five")
    es.indexOfSlice(List("one", "two")) shouldBe el.indexOfSlice(List("one", "two"))
    es.indexOfSlice(List("one", "two"), 1) shouldBe el.indexOfSlice(List("one", "two"), 1)
    es.indexOfSlice(List("ONE", "TWO")) shouldBe el.indexOfSlice(List("ONE", "TWO"))
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(List("one", "two")) shouldBe 0
    es.indexOfSlice(List("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take an Every" in {
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3), 3) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3), 3)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5), 3) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5), 3)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 0)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 1)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), -1)

    val es = NonEmptyList("one", "two", "three", "four", "five")
    val el = List("one", "two", "three", "four", "five")
    es.indexOfSlice(Every("one", "two")) shouldBe el.indexOfSlice(Every("one", "two"))
    es.indexOfSlice(Every("one", "two"), 1) shouldBe el.indexOfSlice(Every("one", "two"), 1)
    es.indexOfSlice(Every("ONE", "TWO")) shouldBe el.indexOfSlice(Every("ONE", "TWO"))
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(Every("one", "two")) shouldBe 0
    es.indexOfSlice(Every("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take a NonEmptyList" in {
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3), 3) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3), 3)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3, 5), 3) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3, 5), 3)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3, 5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(2, 3, 5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5), 0) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5), 0)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5), 1) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5), 1)
    NonEmptyList(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5), -1) shouldBe List(1, 2, 3, 4, 5).indexOfSlice(NonEmptyList(1, 2, 3, 4, 5), -1)

    val es = NonEmptyList("one", "two", "three", "four", "five")
    val el = List("one", "two", "three", "four", "five")
    es.indexOfSlice(NonEmptyList("one", "two")) shouldBe el.indexOfSlice(NonEmptyList("one", "two"))
    es.indexOfSlice(NonEmptyList("one", "two"), 1) shouldBe el.indexOfSlice(NonEmptyList("one", "two"), 1)
    es.indexOfSlice(NonEmptyList("ONE", "TWO")) shouldBe el.indexOfSlice(NonEmptyList("ONE", "TWO"))
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(NonEmptyList("one", "two")) shouldBe 0
    es.indexOfSlice(NonEmptyList("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexWhere methods" in {
    NonEmptyList(1, 2, 3, 4, 5).indexWhere(_ == 3) shouldBe 2
    NonEmptyList(1, 2, 3, 4, 5).indexWhere(_ == 1) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).indexWhere(_ == 1, 2) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).indexWhere(_ == 6) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).indexWhere(_ == 5, 3) shouldBe 4
  }
  it should "have an indices method" in {
    NonEmptyList(1).indices shouldBe List(1).indices
    NonEmptyList(1, 2, 3).indices shouldBe (0 to 2)
    NonEmptyList(1, 2, 3, 4, 5).indices shouldBe (0 to 4)
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
    NonEmptyList(1).isDefinedAt(0) shouldBe true
    NonEmptyList(1).isDefinedAt(1) shouldBe false
    NonEmptyList(1, 2, 3).isDefinedAt(1) shouldBe true
    NonEmptyList(1, 2, 3).isDefinedAt(2) shouldBe true
    NonEmptyList(1, 2, 3).isDefinedAt(3) shouldBe false
    NonEmptyList(1, 2, 3).isDefinedAt(0) shouldBe true
    NonEmptyList(1, 2, 3).isDefinedAt(-1) shouldBe false
  }
  it should "have an isEmpty method" in {
    NonEmptyList("hi").isEmpty shouldBe false
    NonEmptyList(1, 2, 3).isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    NonEmptyList("hi").isTraversableAgain shouldBe true
    NonEmptyList(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    NonEmptyList("hi").iterator.toList shouldBe List("hi")
    NonEmptyList(1, 2, 3).iterator.toList shouldBe List(1, 2, 3)
  }
  it should "have a last method" in {
    NonEmptyList("hi").last shouldBe "hi"
    NonEmptyList(1, 2, 3).last shouldBe 3
  }
  it should "have 2 lastIndexOf methods" in {
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOf(2) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5, 1).lastIndexOf(1) shouldBe 5
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOf(0) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOf(5) shouldBe 4
    NonEmptyList(1, 2, 3, 3, 5).lastIndexOf(3) shouldBe 3
    NonEmptyList(1).lastIndexOf(1) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOf(2, 3) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOf(2, 0) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOf(2, 1) shouldBe 1

    val es = Every("one", "two", "three")
    es.lastIndexOf("one") shouldBe 0
    es.lastIndexOf("two") shouldBe 1
    es.lastIndexOf("three") shouldBe 2
    es.lastIndexOf("three", 1) shouldBe -1
    es.lastIndexOf("ONE") shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOf("one") shouldBe 0
    es.lastIndexOf("ONE") shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take a GenSeq" in {
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3)) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3), 3) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3, 5)) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(5)) shouldBe 4
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty) shouldBe 5
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty, 6) shouldBe 5
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty, 4) shouldBe 4

    val es = NonEmptyList("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(List("one", "two")) shouldBe 0
    es.lastIndexOfSlice(List("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(List("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(List("one", "two")) shouldBe 0
    es.lastIndexOfSlice(List("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take an Every" in {
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3)) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3), 3) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5)) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(5)) shouldBe 4
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = NonEmptyList("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(Every("one", "two")) shouldBe 0
    es.lastIndexOfSlice(Every("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(Every("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(Every("one", "two")) shouldBe 0
    es.lastIndexOfSlice(Every("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take a NonEmptyList" in {
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(2, 3)) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(2, 3), 3) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(2, 3, 5), 3) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(2, 3, 5)) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(5)) shouldBe 4
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyList(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = NonEmptyList("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(NonEmptyList("one", "two")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyList("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(NonEmptyList("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(NonEmptyList("one", "two")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyList("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexWhere methods" in {
    NonEmptyList(1, 2, 3, 4, 5).lastIndexWhere(_ == 2) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexWhere(_ == 0) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexWhere(_ == 5) shouldBe 4
    NonEmptyList(1, 2, 3, 3, 5).lastIndexWhere(_ == 3) shouldBe 3
    NonEmptyList(1).lastIndexWhere(_ == 1) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 3) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 0) shouldBe -1
    NonEmptyList(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 1) shouldBe 1
  }
  it should "have an lastOption method" in {
    NonEmptyList("hi").lastOption shouldBe Some("hi")
    NonEmptyList(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an lengthCompare method" in {
    NonEmptyList("hi").lengthCompare(0) should be > 0
    NonEmptyList("hi").lengthCompare(1) shouldEqual 0
    NonEmptyList("hi").lengthCompare(2) should be < 0
    NonEmptyList(1, 2, 3).lengthCompare(0) should be > 0
    NonEmptyList(1, 2, 3).lengthCompare(1) should be > 0
    NonEmptyList(1, 2, 3).lengthCompare(2) should be > 0
    NonEmptyList(1, 2, 3).lengthCompare(3) shouldEqual 0
    NonEmptyList(1, 2, 3).lengthCompare(4) should be < 0
  }
  it should "have an inherited lift method" in {
    val liftedOne = NonEmptyList("hi").lift
    liftedOne(0) shouldBe Some("hi")
    liftedOne(1) shouldBe None
    liftedOne(-1) shouldBe None
    val liftedMany = NonEmptyList(1, 2, 3).lift
    liftedMany(0) shouldBe Some(1)
    liftedMany(1) shouldBe Some(2)
    liftedMany(2) shouldBe Some(3)
    liftedMany(3) shouldBe None
    liftedMany(-1) shouldBe None
  }
  it should "have a map method" in {
    NonEmptyList(1, 2, 3) map (_ + 1) shouldBe NonEmptyList(2, 3, 4)
    (for (ele <- NonEmptyList(1, 2, 3)) yield ele * 2) shouldBe NonEmptyList(2, 4, 6)
    NonEmptyList(5) map (_ + 3) shouldBe NonEmptyList(8)
    NonEmptyList(8) map (_.toString) shouldBe NonEmptyList("8")
  }
  it should "have a max method" in {
    NonEmptyList(1, 2, 3, 4, 5).max shouldBe 5
    NonEmptyList(1).max shouldBe 1
    NonEmptyList(-1).max shouldBe -1
    NonEmptyList("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    NonEmptyList(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    NonEmptyList(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    NonEmptyList(1, 2, 3, 4, 5).min shouldBe 1
    NonEmptyList(1).min shouldBe 1
    NonEmptyList(-1).min shouldBe -1
    NonEmptyList("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    NonEmptyList(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    NonEmptyList(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a mkString method" in {
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    NonEmptyList("hi").mkString shouldBe "hi"
    NonEmptyList(1, 2, 3).mkString shouldBe "123"
    // SKIP-DOTTY-END

    NonEmptyList("hi").mkString("#") shouldBe "hi"
    NonEmptyList(1, 2, 3).mkString("#") shouldBe "1#2#3"
    NonEmptyList(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    NonEmptyList("hi").mkString("<", "#", ">") shouldBe "<hi>"
    NonEmptyList(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    NonEmptyList(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    NonEmptyList("hi").nonEmpty shouldBe true
    NonEmptyList(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have an orElse method, inherited from PartialFunction" in {
    val pf: PartialFunction[Int, Int] = { case i => -i }
    val f = NonEmptyList(1, 2, 3) orElse pf
    f(0) shouldBe 1
    f(1) shouldBe 2
    f(2) shouldBe 3
    f(3) shouldBe -3
    f(-1) shouldBe 1
  }
  it should "have a padTo method" in {
    NonEmptyList(1).padTo(0, -1) shouldBe NonEmptyList(1)
    NonEmptyList(1).padTo(1, -1) shouldBe NonEmptyList(1)
    NonEmptyList(1).padTo(2, -1) shouldBe NonEmptyList(1, -1)
    NonEmptyList(1).padTo(3, -1) shouldBe NonEmptyList(1, -1, -1)
    NonEmptyList(1, 2, 3).padTo(3, -1) shouldBe NonEmptyList(1, 2, 3)
    NonEmptyList(1, 2, 3).padTo(4, -1) shouldBe NonEmptyList(1, 2, 3, -1)
    NonEmptyList(1, 2, 3).padTo(5, -1) shouldBe NonEmptyList(1, 2, 3, -1, -1)
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: nonEmptyList.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a patch method" in {
    NonEmptyList(1, 2, 3, 4, 5).patch(2, NonEmptyList(-3, -4), 2) shouldBe NonEmptyList(1, 2, -3, -4, 5)
    NonEmptyList(1, 2, 3, 4, 5).patch(2, NonEmptyList(-3, -4), 5) shouldBe NonEmptyList(1, 2, -3, -4)
    NonEmptyList(1, 2, 3, 4, 5).patch(2, NonEmptyList(-3, -4), 1) shouldBe NonEmptyList(1, 2, -3, -4, 4, 5)
    NonEmptyList(1, 2, 3, 4, 5).patch(4, NonEmptyList(-3, -4), 2) shouldBe NonEmptyList(1, 2, 3, 4, -3, -4)
    NonEmptyList(1, 2, 3, 4, 5).patch(5, NonEmptyList(-3, -4), 2) shouldBe NonEmptyList(1, 2, 3, 4, 5, -3, -4)
    NonEmptyList(1, 2, 3, 4, 5).patch(6, NonEmptyList(-3, -4), 2) shouldBe NonEmptyList(1, 2, 3, 4, 5, -3, -4)
    NonEmptyList(1, 2, 3, 4, 5).patch(0, NonEmptyList(-3, -4), 2) shouldBe NonEmptyList(-3, -4, 3, 4, 5)
    NonEmptyList(1, 2, 3, 4, 5).patch(0, NonEmptyList(-3, -4), 3) shouldBe NonEmptyList(-3, -4, 4, 5)
  }
  it should "have a permutations method" in {
    NonEmptyList(1, 2, 3).permutations.toStream shouldBe Stream(NonEmptyList(1, 2, 3), NonEmptyList(1, 3, 2), NonEmptyList(2, 1, 3), NonEmptyList(2, 3, 1), NonEmptyList(3, 1, 2), NonEmptyList(3, 2, 1))
    NonEmptyList(1).permutations.toStream shouldBe Stream(NonEmptyList(1))
    NonEmptyList(1, 2).permutations.toStream shouldBe Stream(NonEmptyList(1, 2), NonEmptyList(2, 1))
  }
  it should "have a prefixLength method" in {
    NonEmptyList(1, 2, 3, 4, 5).prefixLength(_ == 1) shouldBe 1
    NonEmptyList(1, 2, 3, 4, 5).prefixLength(_ == 2) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5).prefixLength(_ <= 2) shouldBe 2
    NonEmptyList(1, 2, 3, 4, 5).prefixLength(_ <= 10) shouldBe 5
    NonEmptyList(1, 2, 3, 4, 5).prefixLength(_ <= 4) shouldBe 4
  }
  it should "have a product method" in {
    NonEmptyList(1, 2, 3).product shouldBe 6
    NonEmptyList(3).product shouldBe 3
    NonEmptyList(3, 4, 5).product shouldBe 60
    NonEmptyList(3, 4, 5).product shouldBe 60
    NonEmptyList(3.1, 4.2, 5.3).product shouldBe 69.006
  }
  it should "have a reduce method" in {
    NonEmptyList(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    NonEmptyList(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    NonEmptyList(5).reduce(_ + _) shouldBe 5
    NonEmptyList(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    NonEmptyList(1).reduceLeft(_ + _) shouldBe 1
    NonEmptyList(1).reduceLeft(_ * _) shouldBe 1
    NonEmptyList(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    NonEmptyList(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    NonEmptyList(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    NonEmptyList(1).reduceLeftOption(_ + _) shouldBe Some(1)
    NonEmptyList(1).reduceLeftOption(_ * _) shouldBe Some(1)
    NonEmptyList(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    NonEmptyList(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    NonEmptyList(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    NonEmptyList(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    NonEmptyList(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    NonEmptyList(5).reduceOption(_ + _) shouldBe Some(5)
    NonEmptyList(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    NonEmptyList(1).reduceRight(_ * _) shouldBe 1
    NonEmptyList(1, 2, 3).reduceRight(_ + _) shouldBe 6
    NonEmptyList(1, 2, 3).reduceRight(_ * _) shouldBe 6
    NonEmptyList(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    NonEmptyList(1).reduceRightOption(_ + _) shouldBe Some(1)
    NonEmptyList(1).reduceRightOption(_ * _) shouldBe Some(1)
    NonEmptyList(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    NonEmptyList(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    NonEmptyList(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a reverse method" in {
    NonEmptyList(33).reverse shouldBe NonEmptyList(33)
    NonEmptyList(33, 34, 35).reverse shouldBe NonEmptyList(35, 34, 33)
  }
  it should "have a reverseIterator method" in {
    NonEmptyList(3).reverseIterator.toStream shouldBe Stream(3)
    NonEmptyList(1, 2, 3).reverseIterator.toList shouldBe Stream(3, 2, 1)
  }
  it should "have a reverseMap method" in {
    NonEmptyList(3).reverseMap(_ + 1) shouldBe NonEmptyList(4)
    NonEmptyList(1, 2, 3).reverseMap(_ + 1) shouldBe NonEmptyList(4, 3, 2)
  }
  it should "have a runWith method, inherited from PartialFunction" in {
 // TODO: What is this? Seems to be testing Vector or List instead of Every or NonEmptyList.
    var x = 0
    val f = List(1, 2, 3).runWith(x += _)

    f(0) shouldBe true
    x shouldBe 1

    f(1) shouldBe true
    x shouldBe 3

    f(2) shouldBe true
    x shouldBe 6

    f(3) shouldBe false

    var y = 0
    val g = List(3).runWith(y += _)

    g(0) shouldBe true
    y shouldBe 3

    g(0) shouldBe true
    y shouldBe 6
  }
  it should "have a sameElements method that takes a GenIterable" in {
    NonEmptyList(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyList(3).sameElements(List(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyList(3).sameElements(List(1)) shouldBe false
    NonEmptyList(3).sameElements(List(3)) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    NonEmptyList(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyList(3).sameElements(Every(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyList(3).sameElements(Every(1)) shouldBe false
    NonEmptyList(3).sameElements(Every(3)) shouldBe true
  }
  it should "have a sameElements method that takes a NonEmptyList" in {
    NonEmptyList(1, 2, 3, 4, 5).sameElements(NonEmptyList(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).sameElements(NonEmptyList(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).sameElements(NonEmptyList(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyList(1, 2, 3, 4, 5).sameElements(NonEmptyList(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyList(3).sameElements(NonEmptyList(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyList(3).sameElements(NonEmptyList(1)) shouldBe false
    NonEmptyList(3).sameElements(NonEmptyList(3)) shouldBe true
  }
  it should "have a scan method" in {
    NonEmptyList(1).scan(0)(_ + _) shouldBe NonEmptyList(0, 1)
    NonEmptyList(1, 2, 3).scan(0)(_ + _) shouldBe NonEmptyList(0, 1, 3, 6)
    NonEmptyList(1, 2, 3).scan("z")(_.toString + _.toString) shouldBe NonEmptyList("z", "z1", "z12", "z123")
    NonEmptyList(0).scan("z")(_.toString + _.toString) shouldBe NonEmptyList("z", "z0")
  }
  it should "have a scanLeft method" in {
    NonEmptyList(1).scanLeft(0)(_ + _) shouldBe NonEmptyList(0, 1)
    NonEmptyList(1, 2, 3).scanLeft(0)(_ + _) shouldBe NonEmptyList(0, 1, 3, 6)
    NonEmptyList(1, 2, 3).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptyList("z", "z1", "z12", "z123")
    NonEmptyList(0).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptyList("z", "z0")
  }
  it should "have a scanRight method" in {
    NonEmptyList(1).scanRight(0)(_ + _) shouldBe NonEmptyList(1, 0)
    NonEmptyList(1, 2, 3).scanRight(0)(_ + _) shouldBe NonEmptyList(6, 5, 3, 0)
    NonEmptyList(1, 2, 3).scanRight("z")(_.toString + _.toString) shouldBe NonEmptyList("123z", "23z", "3z", "z")
    NonEmptyList(0).scanRight("z")(_.toString + _.toString) shouldBe NonEmptyList("0z", "z")
  }
  it should "have a segmentLength method" in {
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 7, 0) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ == 7, 0) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 0) shouldBe 10
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 1, 0) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 10) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 8) shouldBe 2
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 3, 0) shouldBe 2
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 5, 0) shouldBe 4
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 0) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 5) shouldBe 5
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 4) shouldBe 0
    NonEmptyList(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 6) shouldBe 4
  }
  // it should "have a seq method" is pending
  it should "have a size method" in {
    NonEmptyList(5).size shouldBe 1
    NonEmptyList(1, 2, 3).size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    NonEmptyList(1).sliding(1).toList shouldBe List(NonEmptyList(1))
    NonEmptyList(1).sliding(2).toList shouldBe List(NonEmptyList(1))
    NonEmptyList(1, 2, 3).sliding(2).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(2, 3))
    NonEmptyList(1, 2, 3).sliding(1).toList shouldBe List(NonEmptyList(1), NonEmptyList(2), NonEmptyList(3))
    NonEmptyList(1, 2, 3).sliding(3).toList shouldBe List(NonEmptyList(1, 2, 3))
    NonEmptyList(1, 2, 3, 4, 5).sliding(3).toList shouldBe List(NonEmptyList(1, 2, 3), NonEmptyList(2, 3, 4), NonEmptyList(3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(2).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(2, 3), NonEmptyList(3, 4), NonEmptyList(4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(1).toList shouldBe List(NonEmptyList(1), NonEmptyList(2), NonEmptyList(3), NonEmptyList(4), NonEmptyList(5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(4).toList shouldBe List(NonEmptyList(1, 2, 3, 4), NonEmptyList(2, 3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(5).toList shouldBe List(NonEmptyList(1, 2, 3, 4, 5))

    NonEmptyList(1).sliding(1, 1).toList shouldBe List(NonEmptyList(1))
    NonEmptyList(1).sliding(1, 2).toList shouldBe List(NonEmptyList(1))
    NonEmptyList(1, 2, 3).sliding(1, 1).toList shouldBe List(NonEmptyList(1), NonEmptyList(2), NonEmptyList(3))
    NonEmptyList(1, 2, 3).sliding(2, 1).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(2, 3))
    NonEmptyList(1, 2, 3).sliding(2, 2).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(3))
    NonEmptyList(1, 2, 3).sliding(3, 2).toList shouldBe List(NonEmptyList(1, 2, 3))
    NonEmptyList(1, 2, 3).sliding(3, 1).toList shouldBe List(NonEmptyList(1, 2, 3))
    NonEmptyList(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(NonEmptyList(1, 2, 3), NonEmptyList(2, 3, 4), NonEmptyList(3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(2, 2).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(3, 4), NonEmptyList(5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(2, 3).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(2, 4).toList shouldBe List(NonEmptyList(1, 2), NonEmptyList(5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(NonEmptyList(1, 2, 3), NonEmptyList(2, 3, 4), NonEmptyList(3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(3, 2).toList shouldBe List(NonEmptyList(1, 2, 3), NonEmptyList(3, 4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(3, 3).toList shouldBe List(NonEmptyList(1, 2, 3), NonEmptyList(4, 5))
    NonEmptyList(1, 2, 3, 4, 5).sliding(3, 4).toList shouldBe List(NonEmptyList(1, 2, 3), NonEmptyList(5))
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
    NonEmptyList("five", "four", "three", "two", "one").sortBy(regFun) shouldBe NonEmptyList("one", "two", "three", "four", "five")
    NonEmptyList("two", "one", "four", "five", "three").sortBy(regFun) shouldBe NonEmptyList("one", "two", "three", "four", "five")
    NonEmptyList("two", "-one", "four", "-five", "-three").sortBy(regFun) shouldBe NonEmptyList("-five", "-three", "-one", "two", "four")
    NonEmptyList("two", "-one", "four", "-five", "-three").sortBy(absFun) shouldBe NonEmptyList("-one", "two", "-three", "four", "-five")
  }
  it should "have a sortWith method" in {
    NonEmptyList(1, 2, 3, 4, 5).sortWith(_ > _) shouldBe NonEmptyList(5, 4, 3, 2, 1)
    NonEmptyList(2, 1, 4, 5, 3).sortWith(_ > _) shouldBe NonEmptyList(5, 4, 3, 2, 1)
    NonEmptyList(2, -1, 4, -5, -3).sortWith(_.abs > _.abs) shouldBe NonEmptyList(-5, 4, -3, 2, -1)
    NonEmptyList(2, -1, 4, -5, -3).sortWith(_.abs < _.abs) shouldBe NonEmptyList(-1, 2, -3, 4, -5)
  }
  it should "have a sorted method" in {
    NonEmptyList(1, 2, 3, 4, 5).sorted shouldBe NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(5, 4, 3, 2, 1).sorted shouldBe NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(2, 1, 4, 5, 3).sorted shouldBe NonEmptyList(1, 2, 3, 4, 5)
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
    NonEmptyList(1, 2, 3).startsWith(List(1)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(List(1, 2)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(List(1, 2, 3)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(List(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1).startsWith(List(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1).startsWith(List(1)) shouldBe true
    NonEmptyList(1).startsWith(List(2)) shouldBe false

    NonEmptyList(1).startsWith(List(1), 0) shouldBe true
    NonEmptyList(1).startsWith(List(1), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(List(1), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(List(1), 2) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(List(2), 2) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(List(2), 1) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(List(2, 3), 1) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(List(1, 2, 3), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(List(1, 2, 3), 0) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(List(3, 4), 2) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(List(3, 4, 5), 2) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(List(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take an Every" in {
    NonEmptyList(1, 2, 3).startsWith(Every(1)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(Every(1, 2)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(Every(1, 2, 3)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1).startsWith(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1).startsWith(Every(1)) shouldBe true
    NonEmptyList(1).startsWith(Every(2)) shouldBe false

    NonEmptyList(1).startsWith(Every(1), 0) shouldBe true
    NonEmptyList(1).startsWith(Every(1), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(Every(1), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(Every(1), 2) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(Every(2), 2) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(Every(2), 1) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(Every(2, 3), 1) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(Every(1, 2, 3), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(Every(1, 2, 3), 0) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(Every(3, 4), 2) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5), 2) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take a NonEmptyList" in {
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1, 2)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1, 2, 3)) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1).startsWith(NonEmptyList(1, 2, 3, 4)) shouldBe false
    NonEmptyList(1).startsWith(NonEmptyList(1)) shouldBe true
    NonEmptyList(1).startsWith(NonEmptyList(2)) shouldBe false

    NonEmptyList(1).startsWith(NonEmptyList(1), 0) shouldBe true
    NonEmptyList(1).startsWith(NonEmptyList(1), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1), 2) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(2), 2) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(2), 1) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(2, 3), 1) shouldBe true
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1, 2, 3), 1) shouldBe false
    NonEmptyList(1, 2, 3).startsWith(NonEmptyList(1, 2, 3), 0) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(NonEmptyList(3, 4), 2) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(NonEmptyList(3, 4, 5), 2) shouldBe true
    NonEmptyList(1, 2, 3, 4, 5).startsWith(NonEmptyList(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have a stringPrefix method" in {
    NonEmptyList(1).stringPrefix shouldBe "NonEmptyList"
    NonEmptyList(1, 2, 3).stringPrefix shouldBe "NonEmptyList"
  }
  it should "have a sum method" in {
    NonEmptyList(1).sum shouldBe 1
    NonEmptyList(5).sum shouldBe 5
    NonEmptyList(1, 2, 3).sum shouldBe 6
    NonEmptyList(1, 2, 3, 4, 5).sum shouldBe 15
    NonEmptyList(1.1, 2.2, 3.3).sum shouldBe 6.6
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
    import org.scalactic.ColCompatHelper.Factory._
    NonEmptyList(1).to(List) shouldBe List(1)
    NonEmptyList(1, 2, 3).to(List) shouldBe List(1, 2, 3)
    NonEmptyList(1, 2, 3).to(scala.collection.mutable.ListBuffer) shouldBe ListBuffer(1, 2, 3)
    NonEmptyList(1, 2, 3).to(Vector) shouldBe Vector(1, 2, 3)
  }
  it should "have a toArray method" in {
    NonEmptyList(1, 2, 3).toArray should === (Array(1, 2, 3))
    NonEmptyList("a", "b").toArray should === (Array("a", "b"))
    NonEmptyList(1).toArray should === (Array(1))
  }
  it should "have a toBuffer method" in {
    NonEmptyList(1, 2, 3).toBuffer should === (Buffer(1, 2, 3))
    NonEmptyList("a", "b").toBuffer should === (Buffer("a", "b"))
    NonEmptyList(1).toBuffer should === (Buffer(1))
  }
  it should "have a toIndexedSeq method" in {
    NonEmptyList(1, 2, 3).toIndexedSeq should === (IndexedSeq(1, 2, 3))
    NonEmptyList("a", "b").toIndexedSeq should === (IndexedSeq("a", "b"))
    NonEmptyList(1).toIndexedSeq should === (IndexedSeq(1))
  }
  it should "have a toIterable method" in {
    NonEmptyList(1, 2, 3).toIterable should === (Iterable(1, 2, 3))
    NonEmptyList("a", "b").toIterable should === (Iterable("a", "b"))
    NonEmptyList(1).toIterable should === (Iterable(1))
  }
  it should "have a toIterator method" in {
    NonEmptyList(1, 2, 3).toIterator.toList should === (Iterator(1, 2, 3).toList)
    NonEmptyList("a", "b").toIterator.toList should === (Iterator("a", "b").toList)
    NonEmptyList(1).toIterator.toList should === (Iterator(1).toList)
    NonEmptyList(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    NonEmptyList("a", "b").toIterator shouldBe an [Iterator[_]]
    NonEmptyList(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    NonEmptyList(1, 2, 3).toList should === (List(1, 2, 3))
    NonEmptyList("a", "b").toList should === (List("a", "b"))
    NonEmptyList(1).toList should === (List(1))
  }
  it should "have a toMap method" in {
    NonEmptyList("1" -> 1, "2" -> 2, "3" -> 3).toMap should === (Map("1" -> 1, "2" -> 2, "3" -> 3))
    NonEmptyList('A' -> "a", 'B' -> "b").toMap should === (Map('A' -> "a", 'B' -> "b"))
    NonEmptyList("1" -> 1).toMap should === (Map("1" -> 1))
  }
  it should "have a toSeq method" in {
    NonEmptyList(1, 2, 3).toSeq should === (Seq(1, 2, 3))
    NonEmptyList("a", "b").toSeq should === (Seq("a", "b"))
    NonEmptyList(1).toSeq should === (Seq(1))
  }
  it should "have a toSet method" in {
    NonEmptyList(1, 2, 3).toSet should === (Set(1, 2, 3))
    NonEmptyList("a", "b").toSet should === (Set("a", "b"))
    NonEmptyList(1).toSet should === (Set(1))
  }
  it should "have a toStream method" in {
    NonEmptyList(1, 2, 3).toStream should === (Stream(1, 2, 3))
    NonEmptyList("a", "b").toStream should === (Stream("a", "b"))
    NonEmptyList(1).toStream should === (Stream(1))
  }
  it should "have a toString method" in {
    NonEmptyList(1, 2, 3).toString should === ("NonEmptyList(1, 2, 3)")
    NonEmptyList(1, 2, 3).toString should === ("NonEmptyList(1, 2, 3)")
    NonEmptyList(1).toString should === ("NonEmptyList(1)")
  }
  it should "have a toVector method" in {
    NonEmptyList(1, 2, 3).toVector should === (Vector(1, 2, 3))
    NonEmptyList("a", "b").toVector should === (Vector("a", "b"))
    NonEmptyList(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    NonEmptyList(NonEmptyList(1, 2, 3), NonEmptyList(4, 5, 6), NonEmptyList(7, 8, 9)).transpose shouldBe NonEmptyList(NonEmptyList(1, 4, 7), NonEmptyList(2, 5, 8), NonEmptyList(3, 6, 9))
    NonEmptyList(NonEmptyList(1, 2), NonEmptyList(3, 4), NonEmptyList(5, 6), NonEmptyList(7, 8)).transpose shouldBe NonEmptyList(NonEmptyList(1, 3, 5, 7), NonEmptyList(2, 4, 6, 8))
    NonEmptyList(NonEmptyList(1, 2), NonEmptyList(3, 4), NonEmptyList(5, 6), NonEmptyList(7, 8)).transpose.transpose shouldBe NonEmptyList(NonEmptyList(1, 2), NonEmptyList(3, 4), NonEmptyList(5, 6), NonEmptyList(7, 8))
    NonEmptyList(NonEmptyList(1, 2, 3), NonEmptyList(4, 5, 6), NonEmptyList(7, 8, 9)).transpose.transpose shouldBe NonEmptyList(NonEmptyList(1, 2, 3), NonEmptyList(4, 5, 6), NonEmptyList(7, 8, 9))
  }
  it should "have a union method that takes a GenSeq" in {
    NonEmptyList(1) union List(1) shouldBe NonEmptyList(1, 1)
    NonEmptyList(1) union List(1, 2) shouldBe NonEmptyList(1, 1, 2)
    NonEmptyList(1, 2) union List(1, 2) shouldBe NonEmptyList(1, 2, 1, 2)
    NonEmptyList(1, 2) union List(1) shouldBe NonEmptyList(1, 2, 1)
    NonEmptyList(1, 2) union List(3, 4, 5) shouldBe NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(1, 2, 3) union List(3, 4, 5) shouldBe NonEmptyList(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes an Every" in {
    NonEmptyList(1) union Every(1) shouldBe NonEmptyList(1, 1)
    NonEmptyList(1) union Every(1, 2) shouldBe NonEmptyList(1, 1, 2)
    NonEmptyList(1, 2) union Every(1, 2) shouldBe NonEmptyList(1, 2, 1, 2)
    NonEmptyList(1, 2) union Every(1) shouldBe NonEmptyList(1, 2, 1)
    NonEmptyList(1, 2) union Every(3, 4, 5) shouldBe NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(1, 2, 3) union Every(3, 4, 5) shouldBe NonEmptyList(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes a NonEmptyList" in {
    NonEmptyList(1) union NonEmptyList(1) shouldBe NonEmptyList(1, 1)
    NonEmptyList(1) union NonEmptyList(1, 2) shouldBe NonEmptyList(1, 1, 2)
    NonEmptyList(1, 2) union NonEmptyList(1, 2) shouldBe NonEmptyList(1, 2, 1, 2)
    NonEmptyList(1, 2) union NonEmptyList(1) shouldBe NonEmptyList(1, 2, 1)
    NonEmptyList(1, 2) union NonEmptyList(3, 4, 5) shouldBe NonEmptyList(1, 2, 3, 4, 5)
    NonEmptyList(1, 2, 3) union NonEmptyList(3, 4, 5) shouldBe NonEmptyList(1, 2, 3, 3, 4, 5)
  }
  it should "have an unzip method" in {
    NonEmptyList((1, 2)).unzip shouldBe (NonEmptyList(1),NonEmptyList(2))
    NonEmptyList((1, 2), (3, 4)).unzip shouldBe (NonEmptyList(1, 3), NonEmptyList(2, 4))
    NonEmptyList((1, 2), (3, 4), (5, 6)).unzip shouldBe (NonEmptyList(1, 3, 5), NonEmptyList(2, 4, 6))
  }
  it should "have an unzip3 method" in {
    NonEmptyList((1, 2, 3)).unzip3 shouldBe (NonEmptyList(1), NonEmptyList(2), NonEmptyList(3))
    NonEmptyList((1, 2, 3), (4, 5, 6)).unzip3 shouldBe (NonEmptyList(1, 4), NonEmptyList(2, 5), NonEmptyList(3, 6))
    NonEmptyList((1, 2, 3), (4, 5, 6), (7, 8, 9)).unzip3 shouldBe (NonEmptyList(1, 4, 7), NonEmptyList(2, 5, 8), NonEmptyList(3, 6, 9))
  }
  it should "have an updated method" in {
    NonEmptyList(1).updated(0, 2) shouldBe NonEmptyList(2)
    an [IndexOutOfBoundsException] should be thrownBy { NonEmptyList(1).updated(1, 2) }
    NonEmptyList(1, 1, 1).updated(1, 2) shouldBe NonEmptyList(1, 2, 1)
    NonEmptyList(1, 1, 1).updated(2, 2) shouldBe NonEmptyList(1, 1, 2)
    NonEmptyList(1, 1, 1).updated(0, 2) shouldBe NonEmptyList(2, 1, 1)
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
    NonEmptyList(1).zipAll(Nil, -1, -2) shouldBe NonEmptyList((1, -2))
    NonEmptyList(1, 2).zipAll(Nil, -1, -2) shouldBe NonEmptyList((1, -2), (2, -2))

    // Same length
    NonEmptyList(1).zipAll(List(1), -1, -2) shouldBe NonEmptyList((1, 1))
    NonEmptyList(1, 2).zipAll(List(1, 2), -1, -2) shouldBe NonEmptyList((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyList(1).zipAll(List(10, 20), -1, -2) shouldBe NonEmptyList((1,10), (-1,20))
    NonEmptyList(1, 2).zipAll(List(10, 20, 30), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyList(1, 2, 3).zipAll(List(10, 20), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (3,-2))
    NonEmptyList(1, 2, 3, 4).zipAll(List(10, 20, 30), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    NonEmptyList(1).zipAll(Every(1), -1, -2) shouldBe NonEmptyList((1, 1))
    NonEmptyList(1, 2).zipAll(Every(1, 2), -1, -2) shouldBe NonEmptyList((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyList(1).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptyList((1,10), (-1,20))
    NonEmptyList(1, 2).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyList(1, 2, 3).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (3,-2))
    NonEmptyList(1, 2, 3, 4).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes a NonEmptyList" in {

    // Same length
    NonEmptyList(1).zipAll(NonEmptyList(1), -1, -2) shouldBe NonEmptyList((1, 1))
    NonEmptyList(1, 2).zipAll(NonEmptyList(1, 2), -1, -2) shouldBe NonEmptyList((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyList(1).zipAll(NonEmptyList(10, 20), -1, -2) shouldBe NonEmptyList((1,10), (-1,20))
    NonEmptyList(1, 2).zipAll(NonEmptyList(10, 20, 30), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyList(1, 2, 3).zipAll(NonEmptyList(10, 20), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (3,-2))
    NonEmptyList(1, 2, 3, 4).zipAll(NonEmptyList(10, 20, 30), -1, -2) shouldBe NonEmptyList((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipWithIndex method" in {
    NonEmptyList(99).zipWithIndex shouldBe NonEmptyList((99,0))
    NonEmptyList(1, 2, 3, 4, 5).zipWithIndex shouldBe NonEmptyList((1,0), (2,1), (3,2), (4,3), (5,4))
  }
  "End" should "have a pretty toString" in {
    End.toString shouldBe "End"
  }
}

