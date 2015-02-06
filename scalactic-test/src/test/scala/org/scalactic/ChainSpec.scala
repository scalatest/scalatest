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

class ChainSpec extends UnitSpec {
  "A Chain" can "be constructed with one element" in {
    val onesie = Chain(3)
    onesie.length shouldBe 1
    onesie(0) shouldBe 3
  }
  it can "be constructed with many elements" in {
    val twosie = Chain(2, 3)
    twosie.length shouldBe 2
    twosie(0) shouldBe 2
    twosie(1) shouldBe 3
    val threesie = Chain(1, 2, 3)
    threesie.length shouldBe 3
    threesie(0) shouldBe 1
    threesie(1) shouldBe 2
    threesie(2) shouldBe 3
  }
  it can "be constructed from a GenTraversable via the from method on Chain singleton" in {
    Chain.from(List.empty[String]) shouldBe None
    Chain.from(List("1")) shouldBe Some(Chain("1"))
    Chain.from(List(1, 2, 3)) shouldBe Some(Chain(1, 2, 3))
    Chain.from(List.empty[String].par) shouldBe None
    Chain.from(List("1").par) shouldBe Some(Chain("1"))
    Chain.from(List(1, 2, 3).par) shouldBe Some(Chain(1, 2, 3))
  }
  it can "be constructed with null elements" in {
    noException should be thrownBy Chain("hi", null, "ho")
    noException should be thrownBy Chain(null)
    noException should be thrownBy Chain("ho", null)
  }
  it can "be constructed using cons-End style" in {
    0 :: 1 :: End shouldBe Chain(0, 1)
    0 :: 1 ::  2 :: End shouldBe Chain(0, 1, 2)
    "zero" :: "one" ::  "two" :: End shouldBe Chain("zero", "one", "two")
  }
  it can "be deconstructed with Chain" in {
    Chain(1) match {
      case Chain(x) => x shouldEqual 1
      case _ => fail()
    }
    Chain("hi") match {
      case Chain(s) => s shouldEqual "hi"
      case _ => fail()
    }
  }
  it can "be deconstructed with Many" in {
    Chain(1, 2, 3) match {
      case Chain(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    Chain("hi", "there") match {
      case Chain(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    Chain(1, 2, 3) match {
      case Chain(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Chain(1, 2, 3, 4, 5) match {
      case Chain(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
  }
  it can "be deconstructed with Every" in {
    Chain(1, 2, 3) match {
      case Chain(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    Chain("hi", "there") match {
      case Chain(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    Chain(1, 2, 3) match {
      case Chain(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Chain(1, 2, 3, 4, 5) match {
      case Chain(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Chain(1, 2, 3) match {
      case Chain(x, _*) => x shouldEqual 1
      case _ => fail()
    }
    Chain("hi") match {
      case Chain(s) => s shouldEqual "hi"
      case _ => fail()
    }
    Chain(1, 2, 3) match {
      case Chain(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    Chain("hi", "there") match {
      case Chain(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    Chain(1, 2, 3) match {
      case Chain(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Chain(1, 2, 3, 4, 5) match {
      case Chain(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    Chain(1, 2, 3) match {
      case Chain(x, _*) => x shouldEqual 1
      case _ => fail()
    }
  }
  it should "have an apply method" in {
    Chain(1, 2, 3)(0) shouldEqual 1 
    Chain(1, 2, 3)(1) shouldEqual 2 
    Chain("hi")(0) shouldEqual "hi"
    Chain(7, 8, 9)(2) shouldEqual 9
    the [IndexOutOfBoundsException] thrownBy {
      Chain(1, 2, 3)(3)
    } should have message "3"
  }
  it should "have a length method" in {
    Chain(1).length shouldBe 1
    Chain(1, 2).length shouldBe 2
    Chain(1, 2, 3, 4, 5).length shouldBe 5
  }
  it should "have a ++ method that takes another Chain" in {
    Chain(1, 2, 3) ++ Chain(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ Chain(4, 5) shouldEqual Chain(1, 2, 3, 4, 5)
    Chain(1, 2, 3) ++ Chain(4, 5, 6) shouldEqual Chain(1, 2, 3, 4, 5, 6)
  }
  it should "have a ++ method that takes an Every" in {
    Chain(1, 2, 3) ++ One(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ Every(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ Every(4, 5, 6) shouldEqual Chain(1, 2, 3, 4, 5, 6)
    Chain(1, 2, 3) ++ One(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ One(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ Every(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ Every(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ One(4) shouldEqual Chain(1, 2, 3, 4)
  }
  it should "have a ++ method that takes a GenTraversableOnce" in {
    Chain(1, 2, 3) ++ List(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ Vector(4, 5, 6) shouldEqual Chain(1, 2, 3, 4, 5, 6)
    Chain(1, 2, 3) ++ GenTraversable(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ++ Set(4, 5) shouldEqual Chain(1, 2, 3, 4, 5)
    Chain(1, 2, 3) ++ Set(4, 5).iterator shouldEqual Chain(1, 2, 3, 4, 5)
  }
  it should "have a +: method" in {
    0 +: Chain(1) shouldBe Chain(0, 1)
    0 +: Chain(1, 2) shouldBe Chain(0, 1, 2)
    "zero" +: Chain("one", "two") shouldBe Chain("zero", "one", "two")
  }
  it should "have a :: method" in {

    0 :: Chain(1) shouldBe Chain(0, 1)
    0 :: Chain(1, 2) shouldBe Chain(0, 1, 2)
    "zero" :: Chain("one", "two") shouldBe Chain("zero", "one", "two")
  }
  it should "have a ::: method that takes another Chain" in {
    Chain(1, 2, 3) ::: Chain(4) shouldEqual Chain(1, 2, 3, 4)
    Chain(1, 2, 3) ::: Chain(4, 5) shouldEqual Chain(1, 2, 3, 4, 5)
    Chain(1, 2, 3) ::: Chain(4, 5, 6) shouldEqual Chain(1, 2, 3, 4, 5, 6)
  }
  it should "have a ::: method that takes an Every" in {
    One(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    Every(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    Every(1, 2, 3) ::: Chain(4, 5, 6) shouldEqual Chain(1, 2, 3, 4, 5, 6)
    One(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    One(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    Every(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    Every(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    One(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
  }
  it should "have a ::: method that takes a GenTraversableOnce" in {
    List(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    Vector(1, 2, 3) ::: Chain(4, 5, 6) shouldEqual Chain(1, 2, 3, 4, 5, 6)
    GenTraversable(1) ::: Chain(2, 3, 4) shouldEqual Chain(1, 2, 3, 4)
    Set(1, 2) ::: Chain(3, 4, 5) shouldEqual Chain(1, 2, 3, 4, 5)
    Set(1, 2).iterator ::: Chain(3, 4, 5) shouldEqual Chain(1, 2, 3, 4, 5)
  }

  it should "implement PartialFunction[Int, T]" in {
    val pf1: PartialFunction[Int, Int] = Chain(1)
    pf1.isDefinedAt(0) shouldBe true
    pf1.isDefinedAt(1) shouldBe false
  }
  it should "have a /: method" in {
    (0 /: Chain(1))(_ + _) shouldBe 1
    (1 /: Chain(1))(_ + _) shouldBe 2
    (0 /: Chain(1, 2, 3))(_ + _) shouldBe 6
    (1 /: Chain(1, 2, 3))(_ + _) shouldBe 7
  }
  it should "have a :+ method" in {
    Chain(1) :+ 2 shouldBe Chain(1, 2)
    Chain(1, 2) :+ 3 shouldBe Chain(1, 2, 3)
  }
  it should "have a :\\ method" in {
    (Chain(1) :\ 0)(_ + _) shouldBe 1
    (Chain(1) :\ 1)(_ + _) shouldBe 2
    (Chain(1, 2, 3) :\ 0)(_ + _) shouldBe 6
    (Chain(1, 2, 3) :\ 1)(_ + _) shouldBe 7
  }
  it should "have 3 addString methods" in {
    Chain("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    Chain(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    Chain("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    Chain(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    Chain(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    Chain("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    Chain(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    Chain(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = Chain(1) andThen (_ + 1)
    pf1(0) shouldEqual 2
    val pf2 = Chain(1, 2, 3) andThen (_ + 1)
    pf2(0) shouldEqual 2
    pf2(1) shouldEqual 3
    pf2(2) shouldEqual 4
  }
  it should "have an applyOrElse method (inherited from PartialFunction)" in {
    Chain(1, 2, 3).applyOrElse(0, (_: Int) * -1) shouldEqual 1
    Chain(1, 2, 3).applyOrElse(1, (_: Int) * -1) shouldEqual 2
    Chain(1, 2, 3).applyOrElse(2, (_: Int) * -1) shouldEqual 3
    Chain(1, 2, 3).applyOrElse(3, (_: Int) * -1) shouldEqual -3
    Chain(1, 2, 3).applyOrElse(4, (_: Int) * -1) shouldEqual -4
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
    Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collectFirst { case i if i > 10 => i / 2 } shouldBe None
    Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) collectFirst { case i if i > 10 => i / 2 } shouldBe Some(5)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Int = Chain(1, 2, 3).compose((_: Int) + 1)
    fn(-1) shouldBe 1
    fn(0) shouldBe 2
    fn(1) shouldBe 3
  }
  it should "have a contains method" in {
    val e = Chain(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    val es = Chain("one", "two", "three")
    es.contains("one") shouldBe true;
    es.contains("ONE") shouldBe false;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.contains("one") shouldBe true;
      es.contains("ONE") shouldBe false
    }
  }
  // Decided to just overload one for GenSeq and one for Every. Could have done
  // what that has a Slicing nature, but that's a bit too fancy pants.
  it should "have a containsSlice method that takes GenSeq" in {
    val chain = Chain(1, 2, 3, 4, 5)
    chain.containsSlice(List(2, 3)) shouldBe true
    chain.containsSlice(List(2, 3, 5)) shouldBe false
    chain.containsSlice(List.empty) shouldBe true
    chain.containsSlice(Vector(2, 3)) shouldBe true
    chain.containsSlice(Vector(2, 3, 5)) shouldBe false
    chain.containsSlice(Vector.empty) shouldBe true
    chain.containsSlice(ListBuffer(2, 3)) shouldBe true
    chain.containsSlice(ListBuffer(2, 3, 5)) shouldBe false
    chain.containsSlice(ListBuffer.empty) shouldBe true
  }
  it should "have a containsSlice method that takes an Every" in {
    val chain = Chain(1, 2, 3, 4, 5)
    chain.containsSlice(Every(2, 3)) shouldBe true
    chain.containsSlice(Every(2, 3, 5)) shouldBe false
    chain.containsSlice(Every(3)) shouldBe true
  }
  it should "have a containsSlice method that takes a Chain" in {
    val chain = Chain(1, 2, 3, 4, 5)
    chain.containsSlice(Chain(2, 3)) shouldBe true
    chain.containsSlice(Chain(2, 3, 5)) shouldBe false
    chain.containsSlice(Chain(3)) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(-1)
    Chain(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(1, 2, 3, 4, 5)

    val arr2 = Array.fill(5)(-1)
    Chain(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(-1, 1, 2, 3, 4)

    val arr3 = Array.fill(5)(-1)
    Chain(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(-1, 1, 2, -1, -1)
  }
  it should "have a copyToBuffer method" in {

    val buf = ListBuffer.fill(3)(-1)
    Chain(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(-1, -1, -1, 1, 2, 3, 4, 5)
  }
  it should "have a corresponds method that takes a GenSeq" in {
    val chain = Chain(1, 2, 3, 4, 5)
    chain.corresponds(List(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    chain.corresponds(List(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    chain.corresponds(List(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    chain.corresponds(List(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes an Every" in {
    val chain = Chain(1, 2, 3, 4, 5)
    chain.corresponds(Many(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    chain.corresponds(Many(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    chain.corresponds(Many(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    chain.corresponds(Many(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes a Chain" in {
    val chain = Chain(1, 2, 3, 4, 5)
    chain.corresponds(Chain(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    chain.corresponds(Chain(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    chain.corresponds(Chain(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    chain.corresponds(Chain(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a count method" in {
    val chain = Chain(1, 2, 3, 4, 5)
    chain.count(_ > 10) shouldBe 0
    chain.count(_ % 2 == 0) shouldBe 2
    chain.count(_ % 2 == 1) shouldBe 3
  }
  /*
  it should not have a diff method
    scala> Vector(1, 2, 3).diff(Vector(1, 2, 3))
    res0: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a distinct method" in {
    Chain(1, 2, 3).distinct shouldBe Chain(1, 2, 3)
    Chain(1).distinct shouldBe Chain(1)
    Chain(1, 2, 1, 1).distinct shouldBe Chain(1, 2)
    Chain(1, 1, 1).distinct shouldBe Chain(1)
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
    Chain(1).endsWith(List(1)) shouldBe true
    Chain(1).endsWith(List(1, 2)) shouldBe false
    Chain(1, 2).endsWith(List(1, 2)) shouldBe true
    Chain(1, 2, 3, 4, 5).endsWith(List(1, 2)) shouldBe false
    Chain(1, 2, 3, 4, 5).endsWith(List(5)) shouldBe true
    Chain(1, 2, 3, 4, 5).endsWith(List(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes an Every" in {
    Chain(1).endsWith(Every(1)) shouldBe true
    Chain(1).endsWith(Every(1, 2)) shouldBe false
    Chain(1, 2).endsWith(Every(1, 2)) shouldBe true
    Chain(1, 2, 3, 4, 5).endsWith(Every(1, 2)) shouldBe false
    Chain(1, 2, 3, 4, 5).endsWith(Every(5)) shouldBe true
    Chain(1, 2, 3, 4, 5).endsWith(Every(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes a Chain" in {
    Chain(1).endsWith(Chain(1)) shouldBe true
    Chain(1).endsWith(Chain(1, 2)) shouldBe false
    Chain(1, 2).endsWith(Chain(1, 2)) shouldBe true
    Chain(1, 2, 3, 4, 5).endsWith(Chain(1, 2)) shouldBe false
    Chain(1, 2, 3, 4, 5).endsWith(Chain(5)) shouldBe true
    Chain(1, 2, 3, 4, 5).endsWith(Chain(3, 4, 5)) shouldBe true
  }
  it should "have an equals method" in {
    Chain(1) shouldEqual Chain(1)
    Chain(1) should not equal Chain(2)
    Chain(1, 2) should not equal Chain(2, 3)
  }
  it should "have an exists method" in {
    Chain(1, 2, 3).exists(_ == 2) shouldBe true
    Chain(1, 2, 3).exists(_ == 5) shouldBe false
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
    Chain(1, 2, 3).find(_ == 5) shouldBe None
    Chain(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have a flatMap method" in {
    Chain(1, 2, 3) flatMap (i => Chain(i + 1)) shouldBe Chain(2, 3, 4)
    val ss = Chain("hi", "ho")
    val is = Chain(1, 2, 3)
    (for (s <- ss; i <- is) yield (s, i)) shouldBe
      Chain(
        ("hi",1), ("hi",2), ("hi",3), ("ho",1), ("ho",2), ("ho",3)
      )
    Chain(5) flatMap (i => Chain(i + 3)) shouldBe Chain(8)
    Chain(8) flatMap (i => Chain(i.toString)) shouldBe Chain("8")
  }
  /*
    Can only flatten Chains
    scala> Vector(Set.empty[Int], Set.empty[Int]).flatten
    res17: scala.collection.immutable.Vector[Int] = Vector()
  */
  // TODO: Actually it would make sense to flatten Everys too
  it should "have a flatten method that works on nested Chains" in {
    Chain(Chain(1, 2, 3), Chain(1, 2, 3)).flatten shouldBe Chain(1, 2, 3, 1, 2, 3)
    Chain(Chain(1)).flatten shouldBe Chain(1)
  }
  it can "be flattened when in a GenTraversableOnce" in {
    Vector(Chain(1, 2, 3), Chain(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    List(Chain(1, 2, 3), Chain(1, 2, 3)).flatten shouldBe List(1, 2, 3, 1, 2, 3)
    List(Chain(1, 2, 3), Chain(1, 2, 3)).toIterator.flatten.toStream shouldBe List(1, 2, 3, 1, 2, 3).toIterator.toStream
    List(Chain(1, 2, 3), Chain(1, 2, 3)).par.flatten shouldBe List(1, 2, 3, 1, 2, 3).par
  }
  it should "have a fold method" in {
    Chain(1).fold(0)(_ + _) shouldBe 1
    Chain(1).fold(1)(_ * _) shouldBe 1
    Chain(2).fold(0)(_ + _) shouldBe 2
    Chain(2).fold(1)(_ * _) shouldBe 2
    Chain(3).fold(0)(_ + _) shouldBe 3
    Chain(3).fold(1)(_ * _) shouldBe 3
    Chain(1, 2, 3).fold(0)(_ + _) shouldBe 6
    Chain(1, 2, 3).fold(1)(_ * _) shouldBe 6
    Chain(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    Chain(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    Chain(1).foldLeft(0)(_ + _) shouldBe 1
    Chain(1).foldLeft(1)(_ + _) shouldBe 2
    Chain(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    Chain(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    Chain(1).foldRight(0)(_ + _) shouldBe 1
    Chain(1).foldRight(1)(_ + _) shouldBe 2
    Chain(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    Chain(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    Chain(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    Chain(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    Chain(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- Chain(1, 2, 3))
      num += i
    num shouldBe 12
    Chain(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    Chain(1, 2, 3, 4, 5).groupBy(_ % 2) shouldBe Map(1 -> Chain(1, 3, 5), 0 -> Chain(2, 4))
    Chain(1, 2, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> Chain(1, 3, 3, 3), 0 -> Chain(2))
    Chain(1, 1, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> Chain(1, 1, 3, 3, 3))
    Chain(1, 2, 3, 5, 7).groupBy(_ % 2) shouldBe Map(1 -> Chain(1, 3, 5, 7), 0 -> Chain(2))
  }
  it should "have a grouped method" in {
    Chain(1, 2, 3).grouped(2).toList shouldBe List(Chain(1, 2), Chain(3))
    Chain(1, 2, 3).grouped(1).toList shouldBe List(Chain(1), Chain(2), Chain(3))
    an [IllegalArgumentException] should be thrownBy { Chain(1, 2, 3).grouped(0).toList }
    Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toList shouldBe List(Chain(1, 2), Chain(3, 4), Chain(5, 6), Chain(7, 8), Chain(9, 10))
    Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toList shouldBe List(Chain(1, 2, 3), Chain(4, 5, 6), Chain(7, 8, 9), Chain(10))
    Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toList shouldBe List(Chain(1, 2, 3, 4), Chain(5, 6, 7, 8), Chain(9, 10))
    Chain(1).grouped(2).toList shouldBe List(Chain(1))
    Chain(1).grouped(1).toList shouldBe List(Chain(1))
  }
  it should "have a hasDefiniteSize method" in {
    Chain(1).hasDefiniteSize shouldBe true
    Chain(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    Chain(1).hashCode shouldEqual Chain(1).hashCode
    Chain(1, 2).hashCode shouldEqual Chain(1, 2).hashCode
  }
  it should "have a head method" in {
    Chain("hi").head shouldBe "hi"
    Chain(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    Chain("hi").headOption shouldBe Some("hi")
    Chain(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have 2 indexOf methods" in {
    Chain(1, 2, 3, 4, 5).indexOf(3) shouldBe 2
    Chain(1, 2, 3, 4, 5).indexOf(1) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOf(1, 2) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOf(6) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOf(5, 3) shouldBe 4

    val es = Chain("one", "two", "three")
    es.indexOf("one") shouldBe 0;
    es.indexOf("one", 1) shouldBe -1
    es.indexOf("ONE") shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.indexOf("one") shouldBe 0;
      es.indexOf("ONE") shouldBe -1
    }
  }
  it should "have 2 indexOfSlice methods that take a GenSeq" in {
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(2, 3)) shouldBe 1
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(2, 3), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(2, 3, 5)) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(5)) shouldBe 4
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(List.empty) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(List.empty, 6) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(List.empty, 4) shouldBe 4

    val es = Chain("one", "two", "three", "four", "five")
    es.indexOfSlice(List("one", "two")) shouldBe 0;
    es.indexOfSlice(List("one", "two"), 1) shouldBe -1
    es.indexOfSlice(List("ONE", "TWO")) shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.indexOfSlice(List("one", "two")) shouldBe 0;
      es.indexOfSlice(List("ONE", "TWO")) shouldBe -1
    }
  }
  it should "have 2 indexOfSlice methods that take an Every" in {
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3)) shouldBe 1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5)) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(5)) shouldBe 4
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe 0

    val es = Chain("one", "two", "three", "four", "five")
    es.indexOfSlice(Every("one", "two")) shouldBe 0;
    es.indexOfSlice(Every("one", "two"), 1) shouldBe -1
    es.indexOfSlice(Every("ONE", "TWO")) shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.indexOfSlice(Every("one", "two")) shouldBe 0;
      es.indexOfSlice(Every("ONE", "TWO")) shouldBe -1
    }
  }
  it should "have 2 indexOfSlice methods that take a Chain" in {
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(2, 3)) shouldBe 1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(2, 3), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(2, 3, 5), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(2, 3, 5)) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(5)) shouldBe 4
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(1, 2, 3, 4, 5)) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(1, 2, 3, 4, 5), 0) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(1, 2, 3, 4, 5), 1) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexOfSlice(Chain(1, 2, 3, 4, 5), -1) shouldBe 0

    val es = Chain("one", "two", "three", "four", "five")
    es.indexOfSlice(Chain("one", "two")) shouldBe 0;
    es.indexOfSlice(Chain("one", "two"), 1) shouldBe -1
    es.indexOfSlice(Chain("ONE", "TWO")) shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.indexOfSlice(Chain("one", "two")) shouldBe 0;
      es.indexOfSlice(Chain("ONE", "TWO")) shouldBe -1
    }
  }
  it should "have 2 indexWhere methods" in {
    Chain(1, 2, 3, 4, 5).indexWhere(_ == 3) shouldBe 2
    Chain(1, 2, 3, 4, 5).indexWhere(_ == 1) shouldBe 0
    Chain(1, 2, 3, 4, 5).indexWhere(_ == 1, 2) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexWhere(_ == 6) shouldBe -1
    Chain(1, 2, 3, 4, 5).indexWhere(_ == 5, 3) shouldBe 4
  }
  it should "have an indices method" in {
    Chain(1).indices shouldBe List(1).indices
    Chain(1, 2, 3).indices shouldBe (0 to 2)
    Chain(1, 2, 3, 4, 5).indices shouldBe (0 to 4)
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
    Chain(1).isDefinedAt(0) shouldBe true
    Chain(1).isDefinedAt(1) shouldBe false
    Chain(1, 2, 3).isDefinedAt(1) shouldBe true
    Chain(1, 2, 3).isDefinedAt(2) shouldBe true
    Chain(1, 2, 3).isDefinedAt(3) shouldBe false
    Chain(1, 2, 3).isDefinedAt(0) shouldBe true
    Chain(1, 2, 3).isDefinedAt(-1) shouldBe false
  }
  it should "have an isEmpty method" in {
    Chain("hi").isEmpty shouldBe false
    Chain(1, 2, 3).isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    Chain("hi").isTraversableAgain shouldBe true
    Chain(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    Chain("hi").iterator.toList shouldBe List("hi")
    Chain(1, 2, 3).iterator.toList shouldBe List(1, 2, 3)
  }
  it should "have a last method" in {
    Chain("hi").last shouldBe "hi"
    Chain(1, 2, 3).last shouldBe 3
  }
  it should "have 2 lastIndexOf methods" in {
    Chain(1, 2, 3, 4, 5).lastIndexOf(2) shouldBe 1
    Chain(1, 2, 3, 4, 5, 1).lastIndexOf(1) shouldBe 5
    Chain(1, 2, 3, 4, 5).lastIndexOf(0) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOf(5) shouldBe 4
    Chain(1, 2, 3, 3, 5).lastIndexOf(3) shouldBe 3
    Chain(1).lastIndexOf(1) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOf(2, 3) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexOf(2, 0) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOf(2, 1) shouldBe 1

    val es = Every("one", "two", "three")
    es.lastIndexOf("one") shouldBe 0
    es.lastIndexOf("two") shouldBe 1
    es.lastIndexOf("three") shouldBe 2
    es.lastIndexOf("three", 1) shouldBe -1
    es.lastIndexOf("ONE") shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.lastIndexOf("one") shouldBe 0;
      es.lastIndexOf("ONE") shouldBe -1
    }
  }
  it should "have 2 lastIndexOfSlice methods that take a GenSeq" in {
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3)) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3), 3) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(2, 3, 5)) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(5)) shouldBe 4
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty) shouldBe 5
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty, 6) shouldBe 5
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(List.empty, 4) shouldBe 4

    val es = Chain("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(List("one", "two")) shouldBe 0;
    es.lastIndexOfSlice(List("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(List("ONE", "TWO")) shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.lastIndexOfSlice(List("one", "two")) shouldBe 0;
      es.lastIndexOfSlice(List("ONE", "TWO")) shouldBe -1
    }
  }
  it should "have 2 lastIndexOfSlice methods that take an Every" in {
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3)) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3), 3) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5)) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(5)) shouldBe 4
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = Chain("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(Every("one", "two")) shouldBe 0;
    es.lastIndexOfSlice(Every("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(Every("ONE", "TWO")) shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.lastIndexOfSlice(Every("one", "two")) shouldBe 0;
      es.lastIndexOfSlice(Every("ONE", "TWO")) shouldBe -1
    }
  }
  it should "have 2 lastIndexOfSlice methods that take a Chain" in {
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(2, 3)) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(2, 3), 3) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(2, 3, 5), 3) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(2, 3, 5)) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(5)) shouldBe 4
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(1, 2, 3, 4, 5)) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(1, 2, 3, 4, 5), 0) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(1, 2, 3, 4, 5), 1) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexOfSlice(Chain(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = Chain("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(Chain("one", "two")) shouldBe 0;
    es.lastIndexOfSlice(Chain("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(Chain("ONE", "TWO")) shouldBe -1;
    {
      implicit val strEq = StringNormalizations.lowerCased.toEquality
      es.lastIndexOfSlice(Chain("one", "two")) shouldBe 0;
      es.lastIndexOfSlice(Chain("ONE", "TWO")) shouldBe -1
    }
  }
  it should "have 2 lastIndexWhere methods" in {
    Chain(1, 2, 3, 4, 5).lastIndexWhere(_ == 2) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexWhere(_ == 0) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexWhere(_ == 5) shouldBe 4
    Chain(1, 2, 3, 3, 5).lastIndexWhere(_ == 3) shouldBe 3
    Chain(1).lastIndexWhere(_ == 1) shouldBe 0
    Chain(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 3) shouldBe 1
    Chain(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 0) shouldBe -1
    Chain(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 1) shouldBe 1
  }
  it should "have an lastOption method" in {
    Chain("hi").lastOption shouldBe Some("hi")
    Chain(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an lengthCompare method" in {
    Chain("hi").lengthCompare(0) should be > 0
    Chain("hi").lengthCompare(1) shouldEqual 0
    Chain("hi").lengthCompare(2) should be < 0
    Chain(1, 2, 3).lengthCompare(0) should be > 0
    Chain(1, 2, 3).lengthCompare(1) should be > 0
    Chain(1, 2, 3).lengthCompare(2) should be > 0
    Chain(1, 2, 3).lengthCompare(3) shouldEqual 0
    Chain(1, 2, 3).lengthCompare(4) should be < 0
  }
  it should "have an inherited lift method" in {
    val liftedOne = Chain("hi").lift
    liftedOne(0) shouldBe Some("hi")
    liftedOne(1) shouldBe None
    liftedOne(-1) shouldBe None
    val liftedMany = Chain(1, 2, 3).lift
    liftedMany(0) shouldBe Some(1)
    liftedMany(1) shouldBe Some(2)
    liftedMany(2) shouldBe Some(3)
    liftedMany(3) shouldBe None
    liftedMany(-1) shouldBe None
  }
  it should "have a map method" in {
    Chain(1, 2, 3) map (_ + 1) shouldBe Chain(2, 3, 4)
    (for (ele <- Chain(1, 2, 3)) yield ele * 2) shouldBe Chain(2, 4, 6)
    Chain(5) map (_ + 3) shouldBe Chain(8)
    Chain(8) map (_.toString) shouldBe Chain("8")
  }
  it should "have a max method" in {
    Chain(1, 2, 3, 4, 5).max shouldBe 5
    Chain(1).max shouldBe 1
    Chain(-1).max shouldBe -1
    Chain("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    Chain(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    Chain(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    Chain(1, 2, 3, 4, 5).min shouldBe 1
    Chain(1).min shouldBe 1
    Chain(-1).min shouldBe -1
    Chain("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    Chain(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    Chain(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a mkString method" in {

    Chain("hi").mkString shouldBe "hi"
    Chain(1, 2, 3).mkString shouldBe "123"

    Chain("hi").mkString("#") shouldBe "hi"
    Chain(1, 2, 3).mkString("#") shouldBe "1#2#3"
    Chain(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    Chain("hi").mkString("<", "#", ">") shouldBe "<hi>"
    Chain(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    Chain(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    Chain("hi").nonEmpty shouldBe true
    Chain(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have an orElse method, inherited from PartialFunction" in {
    val pf: PartialFunction[Int, Int] = { case i => -i }
    val f = Chain(1, 2, 3) orElse pf
    f(0) shouldBe 1
    f(1) shouldBe 2
    f(2) shouldBe 3
    f(3) shouldBe -3
    f(-1) shouldBe 1
  }
  it should "have a padTo method" in {
    Chain(1).padTo(0, -1) shouldBe Chain(1)
    Chain(1).padTo(1, -1) shouldBe Chain(1)
    Chain(1).padTo(2, -1) shouldBe Chain(1, -1)
    Chain(1).padTo(3, -1) shouldBe Chain(1, -1, -1)
    Chain(1, 2, 3).padTo(3, -1) shouldBe Chain(1, 2, 3)
    Chain(1, 2, 3).padTo(4, -1) shouldBe Chain(1, 2, 3, -1)
    Chain(1, 2, 3).padTo(5, -1) shouldBe Chain(1, 2, 3, -1, -1)
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: chain.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a patch method" in {
    Chain(1, 2, 3, 4, 5).patch(2, Chain(-3, -4), 2) shouldBe Chain(1, 2, -3, -4, 5)
    Chain(1, 2, 3, 4, 5).patch(2, Chain(-3, -4), 5) shouldBe Chain(1, 2, -3, -4)
    Chain(1, 2, 3, 4, 5).patch(2, Chain(-3, -4), 1) shouldBe Chain(1, 2, -3, -4, 4, 5)
    Chain(1, 2, 3, 4, 5).patch(4, Chain(-3, -4), 2) shouldBe Chain(1, 2, 3, 4, -3, -4)
    Chain(1, 2, 3, 4, 5).patch(5, Chain(-3, -4), 2) shouldBe Chain(1, 2, 3, 4, 5, -3, -4)
    Chain(1, 2, 3, 4, 5).patch(6, Chain(-3, -4), 2) shouldBe Chain(1, 2, 3, 4, 5, -3, -4)
    Chain(1, 2, 3, 4, 5).patch(0, Chain(-3, -4), 2) shouldBe Chain(-3, -4, 3, 4, 5)
    Chain(1, 2, 3, 4, 5).patch(0, Chain(-3, -4), 3) shouldBe Chain(-3, -4, 4, 5)
  }
  it should "have a permutations method" in {
    Chain(1, 2, 3).permutations.toStream shouldBe Stream(Chain(1, 2, 3), Chain(1, 3, 2), Chain(2, 1, 3), Chain(2, 3, 1), Chain(3, 1, 2), Chain(3, 2, 1))
    Chain(1).permutations.toStream shouldBe Stream(Chain(1))
    Chain(1, 2).permutations.toStream shouldBe Stream(Chain(1, 2), Chain(2, 1))
  }
  it should "have a prefixLength method" in {
    Chain(1, 2, 3, 4, 5).prefixLength(_ == 1) shouldBe 1
    Chain(1, 2, 3, 4, 5).prefixLength(_ == 2) shouldBe 0
    Chain(1, 2, 3, 4, 5).prefixLength(_ <= 2) shouldBe 2
    Chain(1, 2, 3, 4, 5).prefixLength(_ <= 10) shouldBe 5
    Chain(1, 2, 3, 4, 5).prefixLength(_ <= 4) shouldBe 4
  }
  it should "have a product method" in {
    Chain(1, 2, 3).product shouldBe 6
    Chain(3).product shouldBe 3
    Chain(3, 4, 5).product shouldBe 60
    Chain(3, 4, 5).product shouldBe 60
    Chain(3.1, 4.2, 5.3).product shouldBe 69.006
  }
  it should "have a reduce method" in {
    Chain(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    Chain(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    Chain(5).reduce(_ + _) shouldBe 5
    Chain(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    Chain(1).reduceLeft(_ + _) shouldBe 1
    Chain(1).reduceLeft(_ * _) shouldBe 1
    Chain(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    Chain(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    Chain(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    Chain(1).reduceLeftOption(_ + _) shouldBe Some(1)
    Chain(1).reduceLeftOption(_ * _) shouldBe Some(1)
    Chain(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    Chain(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    Chain(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    Chain(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    Chain(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    Chain(5).reduceOption(_ + _) shouldBe Some(5)
    Chain(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    Chain(1).reduceRight(_ * _) shouldBe 1
    Chain(1, 2, 3).reduceRight(_ + _) shouldBe 6
    Chain(1, 2, 3).reduceRight(_ * _) shouldBe 6
    Chain(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    Chain(1).reduceRightOption(_ + _) shouldBe Some(1)
    Chain(1).reduceRightOption(_ * _) shouldBe Some(1)
    Chain(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    Chain(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    Chain(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a reverse method" in {
    Chain(33).reverse shouldBe Chain(33)
    Chain(33, 34, 35).reverse shouldBe Chain(35, 34, 33)
  }
  it should "have a reverseIterator method" in {
    Chain(3).reverseIterator.toStream shouldBe Stream(3)
    Chain(1, 2, 3).reverseIterator.toList shouldBe Stream(3, 2, 1)
  }
  it should "have a reverseMap method" in {
    Chain(3).reverseMap(_ + 1) shouldBe Chain(4)
    Chain(1, 2, 3).reverseMap(_ + 1) shouldBe Chain(4, 3, 2)
  }
  it should "have a runWith method, inherited from PartialFunction" in {
 // TODO: What is this? Seems to be testing Vector or List instead of Every or Chain.
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
    Chain(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5)) shouldBe true
    Chain(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4)) shouldBe false
    Chain(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 5, 6)) shouldBe false
    Chain(1, 2, 3, 4, 5).sameElements(List(1, 2, 3, 4, 4)) shouldBe false
    Chain(3).sameElements(List(1, 2, 3, 4, 5)) shouldBe false
    Chain(3).sameElements(List(1)) shouldBe false
    Chain(3).sameElements(List(3)) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    Chain(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5)) shouldBe true
    Chain(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4)) shouldBe false
    Chain(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5, 6)) shouldBe false
    Chain(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 4)) shouldBe false
    Chain(3).sameElements(Every(1, 2, 3, 4, 5)) shouldBe false
    Chain(3).sameElements(Every(1)) shouldBe false
    Chain(3).sameElements(Every(3)) shouldBe true
  }
  it should "have a sameElements method that takes a Chain" in {
    Chain(1, 2, 3, 4, 5).sameElements(Chain(1, 2, 3, 4, 5)) shouldBe true
    Chain(1, 2, 3, 4, 5).sameElements(Chain(1, 2, 3, 4)) shouldBe false
    Chain(1, 2, 3, 4, 5).sameElements(Chain(1, 2, 3, 4, 5, 6)) shouldBe false
    Chain(1, 2, 3, 4, 5).sameElements(Chain(1, 2, 3, 4, 4)) shouldBe false
    Chain(3).sameElements(Chain(1, 2, 3, 4, 5)) shouldBe false
    Chain(3).sameElements(Chain(1)) shouldBe false
    Chain(3).sameElements(Chain(3)) shouldBe true
  }
  it should "have a scan method" in {
    Chain(1).scan(0)(_ + _) shouldBe Chain(0, 1)
    Chain(1, 2, 3).scan(0)(_ + _) shouldBe Chain(0, 1, 3, 6)
    Chain(1, 2, 3).scan("z")(_ + _.toString) shouldBe Chain("z", "z1", "z12", "z123")
    Chain(0).scan("z")(_ + _.toString) shouldBe Chain("z", "z0")
  }
  it should "have a scanLeft method" in {
    Chain(1).scanLeft(0)(_ + _) shouldBe Chain(0, 1)
    Chain(1, 2, 3).scanLeft(0)(_ + _) shouldBe Chain(0, 1, 3, 6)
    Chain(1, 2, 3).scanLeft("z")(_ + _) shouldBe Chain("z", "z1", "z12", "z123")
    Chain(0).scanLeft("z")(_ + _) shouldBe Chain("z", "z0")
  }
  it should "have a scanRight method" in {
    Chain(1).scanRight(0)(_ + _) shouldBe Chain(1, 0)
    Chain(1, 2, 3).scanRight(0)(_ + _) shouldBe Chain(6, 5, 3, 0)
    Chain(1, 2, 3).scanRight("z")(_ + _) shouldBe Chain("123z", "23z", "3z", "z")
    Chain(0).scanRight("z")(_ + _) shouldBe Chain("0z", "z")
  }
  it should "have a segmentLength method" in {
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 7, 0) shouldBe 0
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ == 7, 0) shouldBe 0
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 0) shouldBe 10
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 1, 0) shouldBe 0
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 10) shouldBe 0
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 8) shouldBe 2
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 3, 0) shouldBe 2
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 5, 0) shouldBe 4
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 0) shouldBe 0
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 5) shouldBe 5
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 4) shouldBe 0
    Chain(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 6) shouldBe 4
  }
  // it should "have a seq method" is pending
  it should "have a size method" in {
    Chain(5).size shouldBe 1
    Chain(1, 2, 3).size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    Chain(1).sliding(1).toList shouldBe List(Chain(1))
    Chain(1).sliding(2).toList shouldBe List(Chain(1))
    Chain(1, 2, 3).sliding(2).toList shouldBe List(Chain(1, 2), Chain(2, 3))
    Chain(1, 2, 3).sliding(1).toList shouldBe List(Chain(1), Chain(2), Chain(3))
    Chain(1, 2, 3).sliding(3).toList shouldBe List(Chain(1, 2, 3))
    Chain(1, 2, 3, 4, 5).sliding(3).toList shouldBe List(Chain(1, 2, 3), Chain(2, 3, 4), Chain(3, 4, 5))
    Chain(1, 2, 3, 4, 5).sliding(2).toList shouldBe List(Chain(1, 2), Chain(2, 3), Chain(3, 4), Chain(4, 5))
    Chain(1, 2, 3, 4, 5).sliding(1).toList shouldBe List(Chain(1), Chain(2), Chain(3), Chain(4), Chain(5))
    Chain(1, 2, 3, 4, 5).sliding(4).toList shouldBe List(Chain(1, 2, 3, 4), Chain(2, 3, 4, 5))
    Chain(1, 2, 3, 4, 5).sliding(5).toList shouldBe List(Chain(1, 2, 3, 4, 5))

    Chain(1).sliding(1, 1).toList shouldBe List(Chain(1))
    Chain(1).sliding(1, 2).toList shouldBe List(Chain(1))
    Chain(1, 2, 3).sliding(1, 1).toList shouldBe List(Chain(1), Chain(2), Chain(3))
    Chain(1, 2, 3).sliding(2, 1).toList shouldBe List(Chain(1, 2), Chain(2, 3))
    Chain(1, 2, 3).sliding(2, 2).toList shouldBe List(Chain(1, 2), Chain(3))
    Chain(1, 2, 3).sliding(3, 2).toList shouldBe List(Chain(1, 2, 3))
    Chain(1, 2, 3).sliding(3, 1).toList shouldBe List(Chain(1, 2, 3))
    Chain(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(Chain(1, 2, 3), Chain(2, 3, 4), Chain(3, 4, 5))
    Chain(1, 2, 3, 4, 5).sliding(2, 2).toList shouldBe List(Chain(1, 2), Chain(3, 4), Chain(5))
    Chain(1, 2, 3, 4, 5).sliding(2, 3).toList shouldBe List(Chain(1, 2), Chain(4, 5))
    Chain(1, 2, 3, 4, 5).sliding(2, 4).toList shouldBe List(Chain(1, 2), Chain(5))
    Chain(1, 2, 3, 4, 5).sliding(3, 1).toList shouldBe List(Chain(1, 2, 3), Chain(2, 3, 4), Chain(3, 4, 5))
    Chain(1, 2, 3, 4, 5).sliding(3, 2).toList shouldBe List(Chain(1, 2, 3), Chain(3, 4, 5))
    Chain(1, 2, 3, 4, 5).sliding(3, 3).toList shouldBe List(Chain(1, 2, 3), Chain(4, 5))
    Chain(1, 2, 3, 4, 5).sliding(3, 4).toList shouldBe List(Chain(1, 2, 3), Chain(5))
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
    Chain("five", "four", "three", "two", "one").sortBy(regFun) shouldBe Chain("one", "two", "three", "four", "five")
    Chain("two", "one", "four", "five", "three").sortBy(regFun) shouldBe Chain("one", "two", "three", "four", "five")
    Chain("two", "-one", "four", "-five", "-three").sortBy(regFun) shouldBe Chain("-five", "-three", "-one", "two", "four")
    Chain("two", "-one", "four", "-five", "-three").sortBy(absFun) shouldBe Chain("-one", "two", "-three", "four", "-five")
  }
  it should "have a sortWith method" in {
    Chain(1, 2, 3, 4, 5).sortWith(_ > _) shouldBe Chain(5, 4, 3, 2, 1)
    Chain(2, 1, 4, 5, 3).sortWith(_ > _) shouldBe Chain(5, 4, 3, 2, 1)
    Chain(2, -1, 4, -5, -3).sortWith(_.abs > _.abs) shouldBe Chain(-5, 4, -3, 2, -1)
    Chain(2, -1, 4, -5, -3).sortWith(_.abs < _.abs) shouldBe Chain(-1, 2, -3, 4, -5)
  }
  it should "have a sorted method" in {
    Chain(1, 2, 3, 4, 5).sorted shouldBe Chain(1, 2, 3, 4, 5)
    Chain(5, 4, 3, 2, 1).sorted shouldBe Chain(1, 2, 3, 4, 5)
    Chain(2, 1, 4, 5, 3).sorted shouldBe Chain(1, 2, 3, 4, 5)
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
    Chain(1, 2, 3).startsWith(List(1)) shouldBe true
    Chain(1, 2, 3).startsWith(List(1, 2)) shouldBe true
    Chain(1, 2, 3).startsWith(List(1, 2, 3)) shouldBe true
    Chain(1, 2, 3).startsWith(List(1, 2, 3, 4)) shouldBe false
    Chain(1).startsWith(List(1, 2, 3, 4)) shouldBe false
    Chain(1).startsWith(List(1)) shouldBe true
    Chain(1).startsWith(List(2)) shouldBe false

    Chain(1).startsWith(List(1), 0) shouldBe true
    Chain(1).startsWith(List(1), 1) shouldBe false
    Chain(1, 2, 3).startsWith(List(1), 1) shouldBe false
    Chain(1, 2, 3).startsWith(List(1), 2) shouldBe false
    Chain(1, 2, 3).startsWith(List(2), 2) shouldBe false
    Chain(1, 2, 3).startsWith(List(2), 1) shouldBe true
    Chain(1, 2, 3).startsWith(List(2, 3), 1) shouldBe true
    Chain(1, 2, 3).startsWith(List(1, 2, 3), 1) shouldBe false
    Chain(1, 2, 3).startsWith(List(1, 2, 3), 0) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(List(3, 4), 2) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(List(3, 4, 5), 2) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(List(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take an Every" in {
    Chain(1, 2, 3).startsWith(Every(1)) shouldBe true
    Chain(1, 2, 3).startsWith(Every(1, 2)) shouldBe true
    Chain(1, 2, 3).startsWith(Every(1, 2, 3)) shouldBe true
    Chain(1, 2, 3).startsWith(Every(1, 2, 3, 4)) shouldBe false
    Chain(1).startsWith(Every(1, 2, 3, 4)) shouldBe false
    Chain(1).startsWith(Every(1)) shouldBe true
    Chain(1).startsWith(Every(2)) shouldBe false

    Chain(1).startsWith(Every(1), 0) shouldBe true
    Chain(1).startsWith(Every(1), 1) shouldBe false
    Chain(1, 2, 3).startsWith(Every(1), 1) shouldBe false
    Chain(1, 2, 3).startsWith(Every(1), 2) shouldBe false
    Chain(1, 2, 3).startsWith(Every(2), 2) shouldBe false
    Chain(1, 2, 3).startsWith(Every(2), 1) shouldBe true
    Chain(1, 2, 3).startsWith(Every(2, 3), 1) shouldBe true
    Chain(1, 2, 3).startsWith(Every(1, 2, 3), 1) shouldBe false
    Chain(1, 2, 3).startsWith(Every(1, 2, 3), 0) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(Every(3, 4), 2) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5), 2) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take a Chain" in {
    Chain(1, 2, 3).startsWith(Chain(1)) shouldBe true
    Chain(1, 2, 3).startsWith(Chain(1, 2)) shouldBe true
    Chain(1, 2, 3).startsWith(Chain(1, 2, 3)) shouldBe true
    Chain(1, 2, 3).startsWith(Chain(1, 2, 3, 4)) shouldBe false
    Chain(1).startsWith(Chain(1, 2, 3, 4)) shouldBe false
    Chain(1).startsWith(Chain(1)) shouldBe true
    Chain(1).startsWith(Chain(2)) shouldBe false

    Chain(1).startsWith(Chain(1), 0) shouldBe true
    Chain(1).startsWith(Chain(1), 1) shouldBe false
    Chain(1, 2, 3).startsWith(Chain(1), 1) shouldBe false
    Chain(1, 2, 3).startsWith(Chain(1), 2) shouldBe false
    Chain(1, 2, 3).startsWith(Chain(2), 2) shouldBe false
    Chain(1, 2, 3).startsWith(Chain(2), 1) shouldBe true
    Chain(1, 2, 3).startsWith(Chain(2, 3), 1) shouldBe true
    Chain(1, 2, 3).startsWith(Chain(1, 2, 3), 1) shouldBe false
    Chain(1, 2, 3).startsWith(Chain(1, 2, 3), 0) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(Chain(3, 4), 2) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(Chain(3, 4, 5), 2) shouldBe true
    Chain(1, 2, 3, 4, 5).startsWith(Chain(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have a stringPrefix method" in {
    Chain(1).stringPrefix shouldBe "Chain"
    Chain(1, 2, 3).stringPrefix shouldBe "Chain"
  }
  it should "have a sum method" in {
    Chain(1).sum shouldBe 1
    Chain(5).sum shouldBe 5
    Chain(1, 2, 3).sum shouldBe 6
    Chain(1, 2, 3, 4, 5).sum shouldBe 15
    Chain(1.1, 2.2, 3.3).sum shouldBe 6.6
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
    Chain(1).to[List] shouldBe List(1)
    Chain(1, 2, 3).to[List] shouldBe List(1, 2, 3)
    Chain(1, 2, 3).to[scala.collection.mutable.ListBuffer] shouldBe ListBuffer(1, 2, 3)
    Chain(1, 2, 3).to[Vector] shouldBe Vector(1, 2, 3)
  }
  it should "have a toArray method" in {
    Chain(1, 2, 3).toArray should === (Array(1, 2, 3))
    Chain("a", "b").toArray should === (Array("a", "b"))
    Chain(1).toArray should === (Array(1))
  }
  it should "have a toBuffer method" in {
    Chain(1, 2, 3).toBuffer should === (Buffer(1, 2, 3))
    Chain("a", "b").toBuffer should === (Buffer("a", "b"))
    Chain(1).toBuffer should === (Buffer(1))
  }
  it should "have a toIndexedSeq method" in {
    Chain(1, 2, 3).toIndexedSeq should === (IndexedSeq(1, 2, 3))
    Chain("a", "b").toIndexedSeq should === (IndexedSeq("a", "b"))
    Chain(1).toIndexedSeq should === (IndexedSeq(1))
  }
  it should "have a toIterable method" in {
    Chain(1, 2, 3).toIterable should === (Iterable(1, 2, 3))
    Chain("a", "b").toIterable should === (Iterable("a", "b"))
    Chain(1).toIterable should === (Iterable(1))
  }
  it should "have a toIterator method" in {
    Chain(1, 2, 3).toIterator.toList should === (Iterator(1, 2, 3).toList)
    Chain("a", "b").toIterator.toList should === (Iterator("a", "b").toList)
    Chain(1).toIterator.toList should === (Iterator(1).toList)
    Chain(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    Chain("a", "b").toIterator shouldBe an [Iterator[_]]
    Chain(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    Chain(1, 2, 3).toList should === (List(1, 2, 3))
    Chain("a", "b").toList should === (List("a", "b"))
    Chain(1).toList should === (List(1))
  }
  it should "have a toMap method" in {
    Chain("1" -> 1, "2" -> 2, "3" -> 3).toMap should === (Map("1" -> 1, "2" -> 2, "3" -> 3))
    Chain('A' -> "a", 'B' -> "b").toMap should === (Map('A' -> "a", 'B' -> "b"))
    Chain("1" -> 1).toMap should === (Map("1" -> 1))
  }
  it should "have a toSeq method" in {
    Chain(1, 2, 3).toSeq should === (Seq(1, 2, 3))
    Chain("a", "b").toSeq should === (Seq("a", "b"))
    Chain(1).toSeq should === (Seq(1))
  }
  it should "have a toSet method" in {
    Chain(1, 2, 3).toSet should === (Set(1, 2, 3))
    Chain("a", "b").toSet should === (Set("a", "b"))
    Chain(1).toSet should === (Set(1))
  }
  it should "have a toStream method" in {
    Chain(1, 2, 3).toStream should === (Stream(1, 2, 3))
    Chain("a", "b").toStream should === (Stream("a", "b"))
    Chain(1).toStream should === (Stream(1))
  }
  it should "have a toString method" in {
    Chain(1, 2, 3).toString should === ("Chain(1, 2, 3)")
    Chain(1, 2, 3).toString should === ("Chain(1, 2, 3)")
    Chain(1).toString should === ("Chain(1)")
  }
  it should "have a toTraversable method" in {
    Chain(1, 2, 3).toTraversable should === (Traversable(1, 2, 3))
    Chain("a", "b").toTraversable should === (Traversable("a", "b"))
    Chain(1).toTraversable should === (Traversable(1))
  }
  it should "have a toVector method" in {
    Chain(1, 2, 3).toVector should === (Vector(1, 2, 3))
    Chain("a", "b").toVector should === (Vector("a", "b"))
    Chain(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    Chain(Chain(1, 2, 3), Chain(4, 5, 6), Chain(7, 8, 9)).transpose shouldBe Chain(Chain(1, 4, 7), Chain(2, 5, 8), Chain(3, 6, 9))
    Chain(Chain(1, 2), Chain(3, 4), Chain(5, 6), Chain(7, 8)).transpose shouldBe Chain(Chain(1, 3, 5, 7), Chain(2, 4, 6, 8))
    Chain(Chain(1, 2), Chain(3, 4), Chain(5, 6), Chain(7, 8)).transpose.transpose shouldBe Chain(Chain(1, 2), Chain(3, 4), Chain(5, 6), Chain(7, 8))
    Chain(Chain(1, 2, 3), Chain(4, 5, 6), Chain(7, 8, 9)).transpose.transpose shouldBe Chain(Chain(1, 2, 3), Chain(4, 5, 6), Chain(7, 8, 9))
  }
  it should "have a union method that takes a GenSeq" in {
    Chain(1) union List(1) shouldBe Chain(1, 1)
    Chain(1) union List(1, 2) shouldBe Chain(1, 1, 2)
    Chain(1, 2) union List(1, 2) shouldBe Chain(1, 2, 1, 2)
    Chain(1, 2) union List(1) shouldBe Chain(1, 2, 1)
    Chain(1, 2) union List(3, 4, 5) shouldBe Chain(1, 2, 3, 4, 5)
    Chain(1, 2, 3) union List(3, 4, 5) shouldBe Chain(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes an Every" in {
    Chain(1) union Every(1) shouldBe Chain(1, 1)
    Chain(1) union Every(1, 2) shouldBe Chain(1, 1, 2)
    Chain(1, 2) union Every(1, 2) shouldBe Chain(1, 2, 1, 2)
    Chain(1, 2) union Every(1) shouldBe Chain(1, 2, 1)
    Chain(1, 2) union Every(3, 4, 5) shouldBe Chain(1, 2, 3, 4, 5)
    Chain(1, 2, 3) union Every(3, 4, 5) shouldBe Chain(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes a Chain" in {
    Chain(1) union Chain(1) shouldBe Chain(1, 1)
    Chain(1) union Chain(1, 2) shouldBe Chain(1, 1, 2)
    Chain(1, 2) union Chain(1, 2) shouldBe Chain(1, 2, 1, 2)
    Chain(1, 2) union Chain(1) shouldBe Chain(1, 2, 1)
    Chain(1, 2) union Chain(3, 4, 5) shouldBe Chain(1, 2, 3, 4, 5)
    Chain(1, 2, 3) union Chain(3, 4, 5) shouldBe Chain(1, 2, 3, 3, 4, 5)
  }
  it should "have an unzip method" in {
    Chain((1, 2)).unzip shouldBe (Chain(1),Chain(2))
    Chain((1, 2), (3, 4)).unzip shouldBe (Chain(1, 3), Chain(2, 4))
    Chain((1, 2), (3, 4), (5, 6)).unzip shouldBe (Chain(1, 3, 5), Chain(2, 4, 6))
  }
  it should "have an unzip3 method" in {
    Chain((1, 2, 3)).unzip3 shouldBe (Chain(1), Chain(2), Chain(3))
    Chain((1, 2, 3), (4, 5, 6)).unzip3 shouldBe (Chain(1, 4), Chain(2, 5), Chain(3, 6))
    Chain((1, 2, 3), (4, 5, 6), (7, 8, 9)).unzip3 shouldBe (Chain(1, 4, 7), Chain(2, 5, 8), Chain(3, 6, 9))
  }
  it should "have an updated method" in {
    Chain(1).updated(0, 2) shouldBe Chain(2)
    an [IndexOutOfBoundsException] should be thrownBy { Chain(1).updated(1, 2) }
    Chain(1, 1, 1).updated(1, 2) shouldBe Chain(1, 2, 1)
    Chain(1, 1, 1).updated(2, 2) shouldBe Chain(1, 1, 2)
    Chain(1, 1, 1).updated(0, 2) shouldBe Chain(2, 1, 1)
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
    Chain(1).zipAll(Nil, -1, -2) shouldBe Chain((1, -2))
    Chain(1, 2).zipAll(Nil, -1, -2) shouldBe Chain((1, -2), (2, -2))

    // Same length
    Chain(1).zipAll(List(1), -1, -2) shouldBe Chain((1, 1))
    Chain(1, 2).zipAll(List(1, 2), -1, -2) shouldBe Chain((1, 1), (2, 2))

    // Non-empty, longer on right
    Chain(1).zipAll(List(10, 20), -1, -2) shouldBe Chain((1,10), (-1,20))
    Chain(1, 2).zipAll(List(10, 20, 30), -1, -2) shouldBe Chain((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    Chain(1, 2, 3).zipAll(List(10, 20), -1, -2) shouldBe Chain((1,10), (2,20), (3,-2))
    Chain(1, 2, 3, 4).zipAll(List(10, 20, 30), -1, -2) shouldBe Chain((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    Chain(1).zipAll(Every(1), -1, -2) shouldBe Chain((1, 1))
    Chain(1, 2).zipAll(Every(1, 2), -1, -2) shouldBe Chain((1, 1), (2, 2))

    // Non-empty, longer on right
    Chain(1).zipAll(Every(10, 20), -1, -2) shouldBe Chain((1,10), (-1,20))
    Chain(1, 2).zipAll(Every(10, 20, 30), -1, -2) shouldBe Chain((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    Chain(1, 2, 3).zipAll(Every(10, 20), -1, -2) shouldBe Chain((1,10), (2,20), (3,-2))
    Chain(1, 2, 3, 4).zipAll(Every(10, 20, 30), -1, -2) shouldBe Chain((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes a Chain" in {

    // Same length
    Chain(1).zipAll(Chain(1), -1, -2) shouldBe Chain((1, 1))
    Chain(1, 2).zipAll(Chain(1, 2), -1, -2) shouldBe Chain((1, 1), (2, 2))

    // Non-empty, longer on right
    Chain(1).zipAll(Chain(10, 20), -1, -2) shouldBe Chain((1,10), (-1,20))
    Chain(1, 2).zipAll(Chain(10, 20, 30), -1, -2) shouldBe Chain((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    Chain(1, 2, 3).zipAll(Chain(10, 20), -1, -2) shouldBe Chain((1,10), (2,20), (3,-2))
    Chain(1, 2, 3, 4).zipAll(Chain(10, 20, 30), -1, -2) shouldBe Chain((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipWithIndex method" in {
    Chain(99).zipWithIndex shouldBe Chain((99,0))
    Chain(1, 2, 3, 4, 5).zipWithIndex shouldBe Chain((1,0), (2,1), (3,2), (4,3), (5,4))
  }
  "End" should "have a pretty toString" in {
    End.toString shouldBe "End"
  }
}

