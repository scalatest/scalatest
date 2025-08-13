/*
 * Copyright 2001-2025 Artima, Inc.
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

class NonEmptyVectorSpec extends UnitSpec {
  "A NonEmptyVector" can "be constructed with one element" in {
    val onesie = NonEmptyVector(3)
    onesie.length shouldBe 1
    onesie(0) shouldBe 3
  }
  it can "be constructed with many elements" in {
    val twosie = NonEmptyVector(2, 3)
    twosie.length shouldBe 2
    twosie(0) shouldBe 2
    twosie(1) shouldBe 3
    val threesie = NonEmptyVector(1, 2, 3)
    threesie.length shouldBe 3
    threesie(0) shouldBe 1
    threesie(1) shouldBe 2
    threesie(2) shouldBe 3
  }
  it can "be constructed from a Iterable via the from method on NonEmptyVector singleton" in {
    NonEmptyVector.from(Vector.empty[String]) shouldBe None
    NonEmptyVector.from(Vector("1")) shouldBe Some(NonEmptyVector("1"))
    NonEmptyVector.from(Vector(1, 2, 3)) shouldBe Some(NonEmptyVector(1, 2, 3))
    // SKIP-SCALATESTJS,NATIVE-START
    NonEmptyVector.from(Vector.empty[String].par) shouldBe None
    NonEmptyVector.from(Vector("1").par) shouldBe Some(NonEmptyVector("1"))
    NonEmptyVector.from(Vector(1, 2, 3).par) shouldBe Some(NonEmptyVector(1, 2, 3))
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it can "be constructed with null elements" in {
    noException should be thrownBy NonEmptyVector("hi", null, "ho")
    noException should be thrownBy NonEmptyVector(null)
    noException should be thrownBy NonEmptyVector("ho", null)
  }
  it can "be deconstructed with NonEmptyVector" in {
    NonEmptyVector(1) match {
      case NonEmptyVector(x) => x shouldEqual 1
      case _ => fail()
    }
    NonEmptyVector("hi") match {
      case NonEmptyVector(s) => s shouldEqual "hi"
      case _ => fail()
    }
  }
  it can "be deconstructed with Many" in {
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyVector("hi", "there") match {
      case NonEmptyVector(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3, 4, 5) match {
      case NonEmptyVector(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
  }
  it can "be deconstructed with Every" in {
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyVector("hi", "there") match {
      case NonEmptyVector(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3, 4, 5) match {
      case NonEmptyVector(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, _*) => x shouldEqual 1
      case _ => fail()
    }
    NonEmptyVector("hi") match {
      case NonEmptyVector(s) => s shouldEqual "hi"
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyVector("hi", "there") match {
      case NonEmptyVector(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3, 4, 5) match {
      case NonEmptyVector(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyVector(1, 2, 3) match {
      case NonEmptyVector(x, _*) => x shouldEqual 1
      case _ => fail()
    }
  }
  it should "have an apply method" in {
    NonEmptyVector(1, 2, 3)(0) shouldEqual 1
    NonEmptyVector(1, 2, 3)(1) shouldEqual 2
    NonEmptyVector("hi")(0) shouldEqual "hi"
    NonEmptyVector(7, 8, 9)(2) shouldEqual 9
    val vectorOutOfBoundException = intercept[IndexOutOfBoundsException] {
      Vector(1, 2, 3)(3)
    }
    the [IndexOutOfBoundsException] thrownBy {
      NonEmptyVector(1, 2, 3)(3)
    } should have message vectorOutOfBoundException.getMessage
  }
  it should "have a length method" in {
    NonEmptyVector(1).length shouldBe 1
    NonEmptyVector(1, 2).length shouldBe 2
    NonEmptyVector(1, 2, 3, 4, 5).length shouldBe 5
  }
  it should "have a ++ method that takes another NonEmptyVector" in {
    NonEmptyVector(1, 2, 3) ++ NonEmptyVector(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ NonEmptyVector(4, 5) shouldEqual NonEmptyVector(1, 2, 3, 4, 5)
    NonEmptyVector(1, 2, 3) ++ NonEmptyVector(4, 5, 6) shouldEqual NonEmptyVector(1, 2, 3, 4, 5, 6)
  }
  it should "have a ++ method that takes an Every" in {
    NonEmptyVector(1, 2, 3) ++ One(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ Every(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ Every(4, 5, 6) shouldEqual NonEmptyVector(1, 2, 3, 4, 5, 6)
    NonEmptyVector(1, 2, 3) ++ One(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ One(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ Every(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ Every(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ One(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
  }
  it should "have a ++ method that takes a IterableOnce" in {
    NonEmptyVector(1, 2, 3) ++ Vector(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ Vector(4, 5, 6) shouldEqual NonEmptyVector(1, 2, 3, 4, 5, 6)
    NonEmptyVector(1, 2, 3) ++ Iterable(4) shouldEqual NonEmptyVector(1, 2, 3, 4)
    NonEmptyVector(1, 2, 3) ++ Set(4, 5) shouldEqual NonEmptyVector(1, 2, 3, 4, 5)
    NonEmptyVector(1, 2, 3) ++ Set(4, 5).iterator shouldEqual NonEmptyVector(1, 2, 3, 4, 5)
  }
  it should "have a +: method" in {
    0 +: NonEmptyVector(1) shouldBe NonEmptyVector(0, 1)
    0 +: NonEmptyVector(1, 2) shouldBe NonEmptyVector(0, 1, 2)
    "zero" +: NonEmptyVector("one", "two") shouldBe NonEmptyVector("zero", "one", "two")
  }
  it should "have a :: method" in {

    0 :: NonEmptyVector(1) shouldBe NonEmptyVector(0, 1)
    0 :: NonEmptyVector(1, 2) shouldBe NonEmptyVector(0, 1, 2)
    "zero" :: NonEmptyVector("one", "two") shouldBe NonEmptyVector("zero", "one", "two")
  }

  it should "implement PartialFunction[Int, T]" in {
    val pf1: PartialFunction[Int, Int] = NonEmptyVector(1)
    pf1.isDefinedAt(0) shouldBe true
    pf1.isDefinedAt(1) shouldBe false
  }
  it should "have a :+ method" in {
    NonEmptyVector(1) :+ 2 shouldBe NonEmptyVector(1, 2)
    NonEmptyVector(1, 2) :+ 3 shouldBe NonEmptyVector(1, 2, 3)
  }
  it should "have 3 addString methods" in {
    NonEmptyVector("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    NonEmptyVector(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    NonEmptyVector("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    NonEmptyVector(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    NonEmptyVector(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    NonEmptyVector("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    NonEmptyVector(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    NonEmptyVector(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = NonEmptyVector(1) andThen (_ + 1)
    pf1(0) shouldEqual 2
    val pf2 = NonEmptyVector(1, 2, 3) andThen (_ + 1)
    pf2(0) shouldEqual 2
    pf2(1) shouldEqual 3
    pf2(2) shouldEqual 4
  }
  it should "have an applyOrElse method (inherited from PartialFunction)" in {
    NonEmptyVector(1, 2, 3).applyOrElse(0, (_: Int) * -1) shouldEqual 1
    NonEmptyVector(1, 2, 3).applyOrElse(1, (_: Int) * -1) shouldEqual 2
    NonEmptyVector(1, 2, 3).applyOrElse(2, (_: Int) * -1) shouldEqual 3
    NonEmptyVector(1, 2, 3).applyOrElse(3, (_: Int) * -1) shouldEqual -3
    NonEmptyVector(1, 2, 3).applyOrElse(4, (_: Int) * -1) shouldEqual -4
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
    NonEmptyVector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collectFirst { case i if i > 10 => i / 2 } shouldBe None
    NonEmptyVector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) collectFirst { case i if i > 10 => i / 2 } shouldBe Some(5)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Int = NonEmptyVector(1, 2, 3).compose((_: Int) + 1)
    fn(-1) shouldBe 1
    fn(0) shouldBe 2
    fn(1) shouldBe 3
  }
  it should "have a contains method" in {
    val e = NonEmptyVector(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    val es = NonEmptyVector("one", "two", "three")
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
    val nonEmptyVector = NonEmptyVector(1, 2, 3, 4, 5)
    nonEmptyVector.containsSlice(Vector(2, 3)) shouldBe true
    nonEmptyVector.containsSlice(Vector(2, 3, 5)) shouldBe false
    nonEmptyVector.containsSlice(Vector.empty) shouldBe true
    nonEmptyVector.containsSlice(Vector(2, 3)) shouldBe true
    nonEmptyVector.containsSlice(Vector(2, 3, 5)) shouldBe false
    nonEmptyVector.containsSlice(Vector.empty) shouldBe true
    nonEmptyVector.containsSlice(ListBuffer(2, 3)) shouldBe true
    nonEmptyVector.containsSlice(ListBuffer(2, 3, 5)) shouldBe false
    nonEmptyVector.containsSlice(ListBuffer.empty) shouldBe true
  }
  it should "have a containsSlice method that takes an Every" in {
    val nonEmptyVector = NonEmptyVector(1, 2, 3, 4, 5)
    nonEmptyVector.containsSlice(Every(2, 3)) shouldBe true
    nonEmptyVector.containsSlice(Every(2, 3, 5)) shouldBe false
    nonEmptyVector.containsSlice(Every(3)) shouldBe true
  }
  it should "have a containsSlice method that takes a NonEmptyVector" in {
    val nonEmptyVector = NonEmptyVector(1, 2, 3, 4, 5)
    nonEmptyVector.containsSlice(NonEmptyVector(2, 3)) shouldBe true
    nonEmptyVector.containsSlice(NonEmptyVector(2, 3, 5)) shouldBe false
    nonEmptyVector.containsSlice(NonEmptyVector(3)) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(-1)
    NonEmptyVector(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(1, 2, 3, 4, 5)

    val arr2 = Array.fill(5)(-1)
    NonEmptyVector(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(-1, 1, 2, 3, 4)

    val arr3 = Array.fill(5)(-1)
    NonEmptyVector(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(-1, 1, 2, -1, -1)
  }
  it should "have a copyToBuffer method" in {

    val buf = ListBuffer.fill(3)(-1)
    NonEmptyVector(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(-1, -1, -1, 1, 2, 3, 4, 5)
  }
  it should "have a corresponds method that takes a GenSeq" in {
    val nonEmptyVector = NonEmptyVector(1, 2, 3, 4, 5)
    nonEmptyVector.corresponds(Vector(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyVector.corresponds(Vector(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyVector.corresponds(Vector(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyVector.corresponds(Vector(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes an Every" in {
    val nonEmptyVector = NonEmptyVector(1, 2, 3, 4, 5)
    nonEmptyVector.corresponds(Many(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyVector.corresponds(Many(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyVector.corresponds(Many(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyVector.corresponds(Many(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes a NonEmptyVector" in {
    val nonEmptyVector = NonEmptyVector(1, 2, 3, 4, 5)
    nonEmptyVector.corresponds(NonEmptyVector(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyVector.corresponds(NonEmptyVector(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyVector.corresponds(NonEmptyVector(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyVector.corresponds(NonEmptyVector(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a count method" in {
    val nonEmptyVector = NonEmptyVector(1, 2, 3, 4, 5)
    nonEmptyVector.count(_ > 10) shouldBe 0
    nonEmptyVector.count(_ % 2 == 0) shouldBe 2
    nonEmptyVector.count(_ % 2 == 1) shouldBe 3
  }
  /*
  it should not have a diff method
    scala> Vector(1, 2, 3).diff(Vector(1, 2, 3))
    res0: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a distinct method" in {
    NonEmptyVector(1, 2, 3).distinct shouldBe NonEmptyVector(1, 2, 3)
    NonEmptyVector(1).distinct shouldBe NonEmptyVector(1)
    NonEmptyVector(1, 2, 1, 1).distinct shouldBe NonEmptyVector(1, 2)
    NonEmptyVector(1, 1, 1).distinct shouldBe NonEmptyVector(1)
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
    NonEmptyVector(1).endsWith(Vector(1)) shouldBe true
    NonEmptyVector(1).endsWith(Vector(1, 2)) shouldBe false
    NonEmptyVector(1, 2).endsWith(Vector(1, 2)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(Vector(1, 2)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(Vector(5)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(Vector(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes an Every" in {
    NonEmptyVector(1).endsWith(Every(1)) shouldBe true
    NonEmptyVector(1).endsWith(Every(1, 2)) shouldBe false
    NonEmptyVector(1, 2).endsWith(Every(1, 2)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(Every(1, 2)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(Every(5)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(Every(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes a NonEmptyVector" in {
    NonEmptyVector(1).endsWith(NonEmptyVector(1)) shouldBe true
    NonEmptyVector(1).endsWith(NonEmptyVector(1, 2)) shouldBe false
    NonEmptyVector(1, 2).endsWith(NonEmptyVector(1, 2)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(NonEmptyVector(1, 2)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(NonEmptyVector(5)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).endsWith(NonEmptyVector(3, 4, 5)) shouldBe true
  }
  it should "have an equals method" in {
    NonEmptyVector(1) shouldEqual NonEmptyVector(1)
    NonEmptyVector(1) should not equal NonEmptyVector(2)
    NonEmptyVector(1, 2) should not equal NonEmptyVector(2, 3)
  }
  it should "have an exists method" in {
    NonEmptyVector(1, 2, 3).exists(_ == 2) shouldBe true
    NonEmptyVector(1, 2, 3).exists(_ == 5) shouldBe false
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
    NonEmptyVector(1, 2, 3).find(_ == 5) shouldBe None
    NonEmptyVector(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have a flatMap method" in {
    NonEmptyVector(1, 2, 3) flatMap (i => NonEmptyVector(i + 1)) shouldBe NonEmptyVector(2, 3, 4)
    val ss = NonEmptyVector("hi", "ho")
    val is = NonEmptyVector(1, 2, 3)
    (for (s <- ss; i <- is) yield (s, i)) shouldBe
      NonEmptyVector(
        ("hi",1), ("hi",2), ("hi",3), ("ho",1), ("ho",2), ("ho",3)
      )
    NonEmptyVector(5) flatMap (i => NonEmptyVector(i + 3)) shouldBe NonEmptyVector(8)
    NonEmptyVector(8) flatMap (i => NonEmptyVector(i.toString)) shouldBe NonEmptyVector("8")
  }
  /*
    Can only flatten NonEmptyVectors
    scala> Vector(Set.empty[Int], Set.empty[Int]).flatten
    res17: scala.collection.immutable.Vector[Int] = Vector()
  */
  // TODO: Actually it would make sense to flatten Everys too
  it should "have a flatten method that works on nested NonEmptyVectors" in {
    NonEmptyVector(NonEmptyVector(1, 2, 3), NonEmptyVector(1, 2, 3)).flatten shouldBe NonEmptyVector(1, 2, 3, 1, 2, 3)
    NonEmptyVector(NonEmptyVector(1)).flatten shouldBe NonEmptyVector(1)
  }
  it can "be flattened when in a IterableOnce" in {
    Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(1, 2, 3)).toIterator.flatten.toStream shouldBe Vector(1, 2, 3, 1, 2, 3).toIterator.toStream
    // SKIP-SCALATESTJS,NATIVE-START
    Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(1, 2, 3)).par.flatten shouldBe Vector(1, 2, 3, 1, 2, 3).par
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it should "have a fold method" in {
    NonEmptyVector(1).fold(0)(_ + _) shouldBe 1
    NonEmptyVector(1).fold(1)(_ * _) shouldBe 1
    NonEmptyVector(2).fold(0)(_ + _) shouldBe 2
    NonEmptyVector(2).fold(1)(_ * _) shouldBe 2
    NonEmptyVector(3).fold(0)(_ + _) shouldBe 3
    NonEmptyVector(3).fold(1)(_ * _) shouldBe 3
    NonEmptyVector(1, 2, 3).fold(0)(_ + _) shouldBe 6
    NonEmptyVector(1, 2, 3).fold(1)(_ * _) shouldBe 6
    NonEmptyVector(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    NonEmptyVector(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    NonEmptyVector(1).foldLeft(0)(_ + _) shouldBe 1
    NonEmptyVector(1).foldLeft(1)(_ + _) shouldBe 2
    NonEmptyVector(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    NonEmptyVector(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    NonEmptyVector(1).foldRight(0)(_ + _) shouldBe 1
    NonEmptyVector(1).foldRight(1)(_ + _) shouldBe 2
    NonEmptyVector(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    NonEmptyVector(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    NonEmptyVector(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    NonEmptyVector(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- NonEmptyVector(1, 2, 3))
      num += i
    num shouldBe 12
    NonEmptyVector(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    NonEmptyVector(1, 2, 3, 4, 5).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyVector(1, 3, 5), 0 -> NonEmptyVector(2, 4))
    NonEmptyVector(1, 2, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyVector(1, 3, 3, 3), 0 -> NonEmptyVector(2))
    NonEmptyVector(1, 1, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyVector(1, 1, 3, 3, 3))
    NonEmptyVector(1, 2, 3, 5, 7).groupBy(_ % 2) shouldBe Map(1 -> NonEmptyVector(1, 3, 5, 7), 0 -> NonEmptyVector(2))
  }
  it should "have a grouped method" in {
    NonEmptyVector(1, 2, 3).grouped(2).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(3))
    NonEmptyVector(1, 2, 3).grouped(1).toVector shouldBe Vector(NonEmptyVector(1), NonEmptyVector(2), NonEmptyVector(3))
    an [IllegalArgumentException] should be thrownBy { NonEmptyVector(1, 2, 3).grouped(0).toVector }
    NonEmptyVector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(3, 4), NonEmptyVector(5, 6), NonEmptyVector(7, 8), NonEmptyVector(9, 10))
    NonEmptyVector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toVector shouldBe Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(4, 5, 6), NonEmptyVector(7, 8, 9), NonEmptyVector(10))
    NonEmptyVector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toVector shouldBe Vector(NonEmptyVector(1, 2, 3, 4), NonEmptyVector(5, 6, 7, 8), NonEmptyVector(9, 10))
    NonEmptyVector(1).grouped(2).toVector shouldBe Vector(NonEmptyVector(1))
    NonEmptyVector(1).grouped(1).toVector shouldBe Vector(NonEmptyVector(1))
  }
  it should "have a hasDefiniteSize method" in {
    NonEmptyVector(1).hasDefiniteSize shouldBe true
    NonEmptyVector(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    NonEmptyVector(1).hashCode shouldEqual NonEmptyVector(1).hashCode
    NonEmptyVector(1, 2).hashCode shouldEqual NonEmptyVector(1, 2).hashCode
  }
  it should "have a head method" in {
    NonEmptyVector("hi").head shouldBe "hi"
    NonEmptyVector(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    NonEmptyVector("hi").headOption shouldBe Some("hi")
    NonEmptyVector(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have 2 indexOf methods" in {
    NonEmptyVector(1, 2, 3, 4, 5).indexOf(3) shouldBe 2
    NonEmptyVector(1, 2, 3, 4, 5).indexOf(1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOf(1, 2) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOf(6) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOf(5, 3) shouldBe 4

    val es = NonEmptyVector("one", "two", "three")
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
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(2, 3)) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(2, 3), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(2, 3, 5), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(2, 3, 5)) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(5)) shouldBe 4
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(1, 2, 3, 4, 5), 1) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector(1, 2, 3, 4, 5), -1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector.empty) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector.empty, 6) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Vector.empty, 4) shouldBe 4

    val es = NonEmptyVector("one", "two", "three", "four", "five")
    es.indexOfSlice(Vector("one", "two")) shouldBe 0
    es.indexOfSlice(Vector("one", "two"), 1) shouldBe -1
    es.indexOfSlice(Vector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(Vector("one", "two")) shouldBe 0
    es.indexOfSlice(Vector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take an Every" in {
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3)) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5)) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(5)) shouldBe 4
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe 0

    val es = NonEmptyVector("one", "two", "three", "four", "five")
    es.indexOfSlice(Every("one", "two")) shouldBe 0
    es.indexOfSlice(Every("one", "two"), 1) shouldBe -1
    es.indexOfSlice(Every("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(Every("one", "two")) shouldBe 0
    es.indexOfSlice(Every("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take a NonEmptyVector" in {
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(2, 3)) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(2, 3), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(2, 3, 5), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(2, 3, 5)) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(5)) shouldBe 4
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(1, 2, 3, 4, 5), 1) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexOfSlice(NonEmptyVector(1, 2, 3, 4, 5), -1) shouldBe 0

    val es = NonEmptyVector("one", "two", "three", "four", "five")
    es.indexOfSlice(NonEmptyVector("one", "two")) shouldBe 0
    es.indexOfSlice(NonEmptyVector("one", "two"), 1) shouldBe -1
    es.indexOfSlice(NonEmptyVector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(NonEmptyVector("one", "two")) shouldBe 0
    es.indexOfSlice(NonEmptyVector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexWhere methods" in {
    NonEmptyVector(1, 2, 3, 4, 5).indexWhere(_ == 3) shouldBe 2
    NonEmptyVector(1, 2, 3, 4, 5).indexWhere(_ == 1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).indexWhere(_ == 1, 2) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexWhere(_ == 6) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).indexWhere(_ == 5, 3) shouldBe 4
  }
  it should "have an indices method" in {
    NonEmptyVector(1).indices shouldBe Vector(1).indices
    NonEmptyVector(1, 2, 3).indices shouldBe (0 to 2)
    NonEmptyVector(1, 2, 3, 4, 5).indices shouldBe (0 to 4)
  }
  /*
  it should not have an init method
    scala> Vector(1).init
    res30: scala.collection.immutable.Vector[Int] = Vector()

  it should "have an inits method" is pending
    scala> Vector(1).inits.toVector
    res32: Vector[scala.collection.immutable.Vector[Int]] = Vector(Vector(1), Vector())

  it should "have an intersect method" is pending
    scala> Vector(1, 2, 3) intersect Vector(4, 5)
    res33: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an isDefinedAt method, inherited from PartialFunction" in {
    NonEmptyVector(1).isDefinedAt(0) shouldBe true
    NonEmptyVector(1).isDefinedAt(1) shouldBe false
    NonEmptyVector(1, 2, 3).isDefinedAt(1) shouldBe true
    NonEmptyVector(1, 2, 3).isDefinedAt(2) shouldBe true
    NonEmptyVector(1, 2, 3).isDefinedAt(3) shouldBe false
    NonEmptyVector(1, 2, 3).isDefinedAt(0) shouldBe true
    NonEmptyVector(1, 2, 3).isDefinedAt(-1) shouldBe false
  }
  it should "have an isEmpty method" in {
    NonEmptyVector("hi").isEmpty shouldBe false
    NonEmptyVector(1, 2, 3).isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    NonEmptyVector("hi").isTraversableAgain shouldBe true
    NonEmptyVector(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    NonEmptyVector("hi").iterator.toVector shouldBe Vector("hi")
    NonEmptyVector(1, 2, 3).iterator.toVector shouldBe Vector(1, 2, 3)
  }
  it should "have a last method" in {
    NonEmptyVector("hi").last shouldBe "hi"
    NonEmptyVector(1, 2, 3).last shouldBe 3
  }
  it should "have 2 lastIndexOf methods" in {
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOf(2) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5, 1).lastIndexOf(1) shouldBe 5
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOf(0) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOf(5) shouldBe 4
    NonEmptyVector(1, 2, 3, 3, 5).lastIndexOf(3) shouldBe 3
    NonEmptyVector(1).lastIndexOf(1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOf(2, 3) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOf(2, 0) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOf(2, 1) shouldBe 1

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
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(2, 3)) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(2, 3), 3) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(2, 3, 5), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(2, 3, 5)) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(5)) shouldBe 4
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector(1, 2, 3, 4, 5), -1) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector.empty) shouldBe 5
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector.empty, 6) shouldBe 5
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Vector.empty, 4) shouldBe 4

    val es = NonEmptyVector("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(Vector("one", "two")) shouldBe 0
    es.lastIndexOfSlice(Vector("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(Vector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(Vector("one", "two")) shouldBe 0
    es.lastIndexOfSlice(Vector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take an Every" in {
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3)) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3), 3) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5)) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(5)) shouldBe 4
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = NonEmptyVector("one", "two", "three", "four", "five")
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
  it should "have 2 lastIndexOfSlice methods that take a NonEmptyVector" in {
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(2, 3)) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(2, 3), 3) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(2, 3, 5), 3) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(2, 3, 5)) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(5)) shouldBe 4
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyVector(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = NonEmptyVector("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(NonEmptyVector("one", "two")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyVector("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(NonEmptyVector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(NonEmptyVector("one", "two")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyVector("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexWhere methods" in {
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexWhere(_ == 2) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexWhere(_ == 0) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexWhere(_ == 5) shouldBe 4
    NonEmptyVector(1, 2, 3, 3, 5).lastIndexWhere(_ == 3) shouldBe 3
    NonEmptyVector(1).lastIndexWhere(_ == 1) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 3) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 0) shouldBe -1
    NonEmptyVector(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 1) shouldBe 1
  }
  it should "have an lastOption method" in {
    NonEmptyVector("hi").lastOption shouldBe Some("hi")
    NonEmptyVector(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an lengthCompare method" in {
    NonEmptyVector("hi").lengthCompare(0) should be > 0
    NonEmptyVector("hi").lengthCompare(1) shouldEqual 0
    NonEmptyVector("hi").lengthCompare(2) should be < 0
    NonEmptyVector(1, 2, 3).lengthCompare(0) should be > 0
    NonEmptyVector(1, 2, 3).lengthCompare(1) should be > 0
    NonEmptyVector(1, 2, 3).lengthCompare(2) should be > 0
    NonEmptyVector(1, 2, 3).lengthCompare(3) shouldEqual 0
    NonEmptyVector(1, 2, 3).lengthCompare(4) should be < 0
  }
  it should "have an inherited lift method" in {
    val liftedOne = NonEmptyVector("hi").lift
    liftedOne(0) shouldBe Some("hi")
    liftedOne(1) shouldBe None
    liftedOne(-1) shouldBe None
    val liftedMany = NonEmptyVector(1, 2, 3).lift
    liftedMany(0) shouldBe Some(1)
    liftedMany(1) shouldBe Some(2)
    liftedMany(2) shouldBe Some(3)
    liftedMany(3) shouldBe None
    liftedMany(-1) shouldBe None
  }
  it should "have a map method" in {
    NonEmptyVector(1, 2, 3) map (_ + 1) shouldBe NonEmptyVector(2, 3, 4)
    (for (ele <- NonEmptyVector(1, 2, 3)) yield ele * 2) shouldBe NonEmptyVector(2, 4, 6)
    NonEmptyVector(5) map (_ + 3) shouldBe NonEmptyVector(8)
    NonEmptyVector(8) map (_.toString) shouldBe NonEmptyVector("8")
  }
  it should "have a max method" in {
    NonEmptyVector(1, 2, 3, 4, 5).max shouldBe 5
    NonEmptyVector(1).max shouldBe 1
    NonEmptyVector(-1).max shouldBe -1
    NonEmptyVector("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    NonEmptyVector(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    NonEmptyVector(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    NonEmptyVector(1, 2, 3, 4, 5).min shouldBe 1
    NonEmptyVector(1).min shouldBe 1
    NonEmptyVector(-1).min shouldBe -1
    NonEmptyVector("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    NonEmptyVector(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    NonEmptyVector(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a mkString method" in {
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    NonEmptyVector("hi").mkString shouldBe "hi"
    NonEmptyVector(1, 2, 3).mkString shouldBe "123"
    // SKIP-DOTTY-END

    NonEmptyVector("hi").mkString("#") shouldBe "hi"
    NonEmptyVector(1, 2, 3).mkString("#") shouldBe "1#2#3"
    NonEmptyVector(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    NonEmptyVector("hi").mkString("<", "#", ">") shouldBe "<hi>"
    NonEmptyVector(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    NonEmptyVector(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    NonEmptyVector("hi").nonEmpty shouldBe true
    NonEmptyVector(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have an orElse method, inherited from PartialFunction" in {
    val pf: PartialFunction[Int, Int] = { case i => -i }
    val f = NonEmptyVector(1, 2, 3) orElse pf
    f(0) shouldBe 1
    f(1) shouldBe 2
    f(2) shouldBe 3
    f(3) shouldBe -3
    f(-1) shouldBe 1
  }
  it should "have a padTo method" in {
    NonEmptyVector(1).padTo(0, -1) shouldBe NonEmptyVector(1)
    NonEmptyVector(1).padTo(1, -1) shouldBe NonEmptyVector(1)
    NonEmptyVector(1).padTo(2, -1) shouldBe NonEmptyVector(1, -1)
    NonEmptyVector(1).padTo(3, -1) shouldBe NonEmptyVector(1, -1, -1)
    NonEmptyVector(1, 2, 3).padTo(3, -1) shouldBe NonEmptyVector(1, 2, 3)
    NonEmptyVector(1, 2, 3).padTo(4, -1) shouldBe NonEmptyVector(1, 2, 3, -1)
    NonEmptyVector(1, 2, 3).padTo(5, -1) shouldBe NonEmptyVector(1, 2, 3, -1, -1)
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: nonEmptyVector.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a patch method" in {
    NonEmptyVector(1, 2, 3, 4, 5).patch(2, NonEmptyVector(-3, -4), 2) shouldBe NonEmptyVector(1, 2, -3, -4, 5)
    NonEmptyVector(1, 2, 3, 4, 5).patch(2, NonEmptyVector(-3, -4), 5) shouldBe NonEmptyVector(1, 2, -3, -4)
    NonEmptyVector(1, 2, 3, 4, 5).patch(2, NonEmptyVector(-3, -4), 1) shouldBe NonEmptyVector(1, 2, -3, -4, 4, 5)
    NonEmptyVector(1, 2, 3, 4, 5).patch(4, NonEmptyVector(-3, -4), 2) shouldBe NonEmptyVector(1, 2, 3, 4, -3, -4)
    NonEmptyVector(1, 2, 3, 4, 5).patch(5, NonEmptyVector(-3, -4), 2) shouldBe NonEmptyVector(1, 2, 3, 4, 5, -3, -4)
    NonEmptyVector(1, 2, 3, 4, 5).patch(6, NonEmptyVector(-3, -4), 2) shouldBe NonEmptyVector(1, 2, 3, 4, 5, -3, -4)
    NonEmptyVector(1, 2, 3, 4, 5).patch(0, NonEmptyVector(-3, -4), 2) shouldBe NonEmptyVector(-3, -4, 3, 4, 5)
    NonEmptyVector(1, 2, 3, 4, 5).patch(0, NonEmptyVector(-3, -4), 3) shouldBe NonEmptyVector(-3, -4, 4, 5)
  }
  it should "have a permutations method" in {
    NonEmptyVector(1, 2, 3).permutations.toStream shouldBe Stream(NonEmptyVector(1, 2, 3), NonEmptyVector(1, 3, 2), NonEmptyVector(2, 1, 3), NonEmptyVector(2, 3, 1), NonEmptyVector(3, 1, 2), NonEmptyVector(3, 2, 1))
    NonEmptyVector(1).permutations.toStream shouldBe Stream(NonEmptyVector(1))
    NonEmptyVector(1, 2).permutations.toStream shouldBe Stream(NonEmptyVector(1, 2), NonEmptyVector(2, 1))
  }
  it should "have a prefixLength method" in {
    NonEmptyVector(1, 2, 3, 4, 5).prefixLength(_ == 1) shouldBe 1
    NonEmptyVector(1, 2, 3, 4, 5).prefixLength(_ == 2) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5).prefixLength(_ <= 2) shouldBe 2
    NonEmptyVector(1, 2, 3, 4, 5).prefixLength(_ <= 10) shouldBe 5
    NonEmptyVector(1, 2, 3, 4, 5).prefixLength(_ <= 4) shouldBe 4
  }
  it should "have a product method" in {
    NonEmptyVector(1, 2, 3).product shouldBe 6
    NonEmptyVector(3).product shouldBe 3
    NonEmptyVector(3, 4, 5).product shouldBe 60
    NonEmptyVector(3, 4, 5).product shouldBe 60
    NonEmptyVector(3.1, 4.2, 5.3).product shouldBe 69.006
  }
  it should "have a reduce method" in {
    NonEmptyVector(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    NonEmptyVector(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    NonEmptyVector(5).reduce(_ + _) shouldBe 5
    NonEmptyVector(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    NonEmptyVector(1).reduceLeft(_ + _) shouldBe 1
    NonEmptyVector(1).reduceLeft(_ * _) shouldBe 1
    NonEmptyVector(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    NonEmptyVector(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    NonEmptyVector(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    NonEmptyVector(1).reduceLeftOption(_ + _) shouldBe Some(1)
    NonEmptyVector(1).reduceLeftOption(_ * _) shouldBe Some(1)
    NonEmptyVector(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    NonEmptyVector(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    NonEmptyVector(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    NonEmptyVector(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    NonEmptyVector(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    NonEmptyVector(5).reduceOption(_ + _) shouldBe Some(5)
    NonEmptyVector(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    NonEmptyVector(1).reduceRight(_ * _) shouldBe 1
    NonEmptyVector(1, 2, 3).reduceRight(_ + _) shouldBe 6
    NonEmptyVector(1, 2, 3).reduceRight(_ * _) shouldBe 6
    NonEmptyVector(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    NonEmptyVector(1).reduceRightOption(_ + _) shouldBe Some(1)
    NonEmptyVector(1).reduceRightOption(_ * _) shouldBe Some(1)
    NonEmptyVector(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    NonEmptyVector(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    NonEmptyVector(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a reverse method" in {
    NonEmptyVector(33).reverse shouldBe NonEmptyVector(33)
    NonEmptyVector(33, 34, 35).reverse shouldBe NonEmptyVector(35, 34, 33)
  }
  it should "have a reverseIterator method" in {
    NonEmptyVector(3).reverseIterator.toStream shouldBe Stream(3)
    NonEmptyVector(1, 2, 3).reverseIterator.toVector shouldBe Stream(3, 2, 1)
  }
  it should "have a reverseMap method" in {
    NonEmptyVector(3).reverseMap(_ + 1) shouldBe NonEmptyVector(4)
    NonEmptyVector(1, 2, 3).reverseMap(_ + 1) shouldBe NonEmptyVector(4, 3, 2)
  }
  it should "have a runWith method, inherited from PartialFunction" in {
    // TODO: What is this? Seems to be testing Vector or Vector instead of Every or NonEmptyVector.
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
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Vector(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Vector(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Vector(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Vector(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyVector(3).sameElements(Vector(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyVector(3).sameElements(Vector(1)) shouldBe false
    NonEmptyVector(3).sameElements(Vector(3)) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyVector(3).sameElements(Every(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyVector(3).sameElements(Every(1)) shouldBe false
    NonEmptyVector(3).sameElements(Every(3)) shouldBe true
  }
  it should "have a sameElements method that takes a NonEmptyVector" in {
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(NonEmptyVector(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(NonEmptyVector(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(NonEmptyVector(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyVector(1, 2, 3, 4, 5).sameElements(NonEmptyVector(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyVector(3).sameElements(NonEmptyVector(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyVector(3).sameElements(NonEmptyVector(1)) shouldBe false
    NonEmptyVector(3).sameElements(NonEmptyVector(3)) shouldBe true
  }
  it should "have a scan method" in {
    NonEmptyVector(1).scan(0)(_ + _) shouldBe NonEmptyVector(0, 1)
    NonEmptyVector(1, 2, 3).scan(0)(_ + _) shouldBe NonEmptyVector(0, 1, 3, 6)
    NonEmptyVector(1, 2, 3).scan("z")(_.toString + _.toString) shouldBe NonEmptyVector("z", "z1", "z12", "z123")
    NonEmptyVector(0).scan("z")(_.toString + _.toString) shouldBe NonEmptyVector("z", "z0")
  }
  it should "have a scanLeft method" in {
    NonEmptyVector(1).scanLeft(0)(_ + _) shouldBe NonEmptyVector(0, 1)
    NonEmptyVector(1, 2, 3).scanLeft(0)(_ + _) shouldBe NonEmptyVector(0, 1, 3, 6)
    NonEmptyVector(1, 2, 3).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptyVector("z", "z1", "z12", "z123")
    NonEmptyVector(0).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptyVector("z", "z0")
  }
  it should "have a scanRight method" in {
    NonEmptyVector(1).scanRight(0)(_ + _) shouldBe NonEmptyVector(1, 0)
    NonEmptyVector(1, 2, 3).scanRight(0)(_ + _) shouldBe NonEmptyVector(6, 5, 3, 0)
    NonEmptyVector(1, 2, 3).scanRight("z")(_.toString + _.toString) shouldBe NonEmptyVector("123z", "23z", "3z", "z")
    NonEmptyVector(0).scanRight("z")(_.toString + _.toString) shouldBe NonEmptyVector("0z", "z")
  }
  it should "have a segmentLength method" in {
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 7, 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ == 7, 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 0) shouldBe 10
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 1, 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 10) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 8) shouldBe 2
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 3, 0) shouldBe 2
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 5, 0) shouldBe 4
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 0) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 5) shouldBe 5
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 4) shouldBe 0
    NonEmptyVector(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 6) shouldBe 4
  }
  // it should "have a seq method" is pending
  it should "have a size method" in {
    NonEmptyVector(5).size shouldBe 1
    NonEmptyVector(1, 2, 3).size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    NonEmptyVector(1).sliding(1).toVector shouldBe Vector(NonEmptyVector(1))
    NonEmptyVector(1).sliding(2).toVector shouldBe Vector(NonEmptyVector(1))
    NonEmptyVector(1, 2, 3).sliding(2).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(2, 3))
    NonEmptyVector(1, 2, 3).sliding(1).toVector shouldBe Vector(NonEmptyVector(1), NonEmptyVector(2), NonEmptyVector(3))
    NonEmptyVector(1, 2, 3).sliding(3).toVector shouldBe Vector(NonEmptyVector(1, 2, 3))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(3).toVector shouldBe Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(2, 3, 4), NonEmptyVector(3, 4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(2).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(2, 3), NonEmptyVector(3, 4), NonEmptyVector(4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(1).toVector shouldBe Vector(NonEmptyVector(1), NonEmptyVector(2), NonEmptyVector(3), NonEmptyVector(4), NonEmptyVector(5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(4).toVector shouldBe Vector(NonEmptyVector(1, 2, 3, 4), NonEmptyVector(2, 3, 4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(5).toVector shouldBe Vector(NonEmptyVector(1, 2, 3, 4, 5))

    NonEmptyVector(1).sliding(1, 1).toVector shouldBe Vector(NonEmptyVector(1))
    NonEmptyVector(1).sliding(1, 2).toVector shouldBe Vector(NonEmptyVector(1))
    NonEmptyVector(1, 2, 3).sliding(1, 1).toVector shouldBe Vector(NonEmptyVector(1), NonEmptyVector(2), NonEmptyVector(3))
    NonEmptyVector(1, 2, 3).sliding(2, 1).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(2, 3))
    NonEmptyVector(1, 2, 3).sliding(2, 2).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(3))
    NonEmptyVector(1, 2, 3).sliding(3, 2).toVector shouldBe Vector(NonEmptyVector(1, 2, 3))
    NonEmptyVector(1, 2, 3).sliding(3, 1).toVector shouldBe Vector(NonEmptyVector(1, 2, 3))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(3, 1).toVector shouldBe Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(2, 3, 4), NonEmptyVector(3, 4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(2, 2).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(3, 4), NonEmptyVector(5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(2, 3).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(2, 4).toVector shouldBe Vector(NonEmptyVector(1, 2), NonEmptyVector(5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(3, 1).toVector shouldBe Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(2, 3, 4), NonEmptyVector(3, 4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(3, 2).toVector shouldBe Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(3, 4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(3, 3).toVector shouldBe Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(4, 5))
    NonEmptyVector(1, 2, 3, 4, 5).sliding(3, 4).toVector shouldBe Vector(NonEmptyVector(1, 2, 3), NonEmptyVector(5))
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
    NonEmptyVector("five", "four", "three", "two", "one").sortBy(regFun) shouldBe NonEmptyVector("one", "two", "three", "four", "five")
    NonEmptyVector("two", "one", "four", "five", "three").sortBy(regFun) shouldBe NonEmptyVector("one", "two", "three", "four", "five")
    NonEmptyVector("two", "-one", "four", "-five", "-three").sortBy(regFun) shouldBe NonEmptyVector("-five", "-three", "-one", "two", "four")
    NonEmptyVector("two", "-one", "four", "-five", "-three").sortBy(absFun) shouldBe NonEmptyVector("-one", "two", "-three", "four", "-five")
  }
  it should "have a sortWith method" in {
    NonEmptyVector(1, 2, 3, 4, 5).sortWith(_ > _) shouldBe NonEmptyVector(5, 4, 3, 2, 1)
    NonEmptyVector(2, 1, 4, 5, 3).sortWith(_ > _) shouldBe NonEmptyVector(5, 4, 3, 2, 1)
    NonEmptyVector(2, -1, 4, -5, -3).sortWith(_.abs > _.abs) shouldBe NonEmptyVector(-5, 4, -3, 2, -1)
    NonEmptyVector(2, -1, 4, -5, -3).sortWith(_.abs < _.abs) shouldBe NonEmptyVector(-1, 2, -3, 4, -5)
  }
  it should "have a sorted method" in {
    NonEmptyVector(1, 2, 3, 4, 5).sorted shouldBe NonEmptyVector(1, 2, 3, 4, 5)
    NonEmptyVector(5, 4, 3, 2, 1).sorted shouldBe NonEmptyVector(1, 2, 3, 4, 5)
    NonEmptyVector(2, 1, 4, 5, 3).sorted shouldBe NonEmptyVector(1, 2, 3, 4, 5)
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
    NonEmptyVector(1, 2, 3).startsWith(Vector(1)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Vector(1, 2)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Vector(1, 2, 3)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Vector(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1).startsWith(Vector(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1).startsWith(Vector(1)) shouldBe true
    NonEmptyVector(1).startsWith(Vector(2)) shouldBe false

    NonEmptyVector(1).startsWith(Vector(1), 0) shouldBe true
    NonEmptyVector(1).startsWith(Vector(1), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Vector(1), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Vector(1), 2) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Vector(2), 2) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Vector(2), 1) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Vector(2, 3), 1) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Vector(1, 2, 3), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Vector(1, 2, 3), 0) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(Vector(3, 4), 2) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(Vector(3, 4, 5), 2) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(Vector(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take an Every" in {
    NonEmptyVector(1, 2, 3).startsWith(Every(1)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Every(1, 2)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Every(1, 2, 3)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1).startsWith(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1).startsWith(Every(1)) shouldBe true
    NonEmptyVector(1).startsWith(Every(2)) shouldBe false

    NonEmptyVector(1).startsWith(Every(1), 0) shouldBe true
    NonEmptyVector(1).startsWith(Every(1), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Every(1), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Every(1), 2) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Every(2), 2) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Every(2), 1) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Every(2, 3), 1) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(Every(1, 2, 3), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(Every(1, 2, 3), 0) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(Every(3, 4), 2) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5), 2) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take a NonEmptyVector" in {
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1, 2)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1, 2, 3)) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1).startsWith(NonEmptyVector(1, 2, 3, 4)) shouldBe false
    NonEmptyVector(1).startsWith(NonEmptyVector(1)) shouldBe true
    NonEmptyVector(1).startsWith(NonEmptyVector(2)) shouldBe false

    NonEmptyVector(1).startsWith(NonEmptyVector(1), 0) shouldBe true
    NonEmptyVector(1).startsWith(NonEmptyVector(1), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1), 2) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(2), 2) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(2), 1) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(2, 3), 1) shouldBe true
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1, 2, 3), 1) shouldBe false
    NonEmptyVector(1, 2, 3).startsWith(NonEmptyVector(1, 2, 3), 0) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(NonEmptyVector(3, 4), 2) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(NonEmptyVector(3, 4, 5), 2) shouldBe true
    NonEmptyVector(1, 2, 3, 4, 5).startsWith(NonEmptyVector(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have a stringPrefix method" in {
    NonEmptyVector(1).stringPrefix shouldBe "NonEmptyVector"
    NonEmptyVector(1, 2, 3).stringPrefix shouldBe "NonEmptyVector"
  }
  it should "have a sum method" in {
    NonEmptyVector(1).sum shouldBe 1
    NonEmptyVector(5).sum shouldBe 5
    NonEmptyVector(1, 2, 3).sum shouldBe 6
    NonEmptyVector(1, 2, 3, 4, 5).sum shouldBe 15
    NonEmptyVector(1.1, 2.2, 3.3).sum shouldBe 6.6
  }
  /*
    it should not have a tail method
      scala> Vector(1).tail
      res7: scala.collection.immutable.Vector[Int] = Vector()

    it should not have a tails method
      scala> Vector(1).tails.toVector
      res8: Vector[scala.collection.immutable.Vector[Int]] = Vector(Vector(1), Vector())

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
    NonEmptyVector(1).to(Vector) shouldBe Vector(1)
    NonEmptyVector(1, 2, 3).to(Vector) shouldBe Vector(1, 2, 3)
    NonEmptyVector(1, 2, 3).to(scala.collection.mutable.ListBuffer) shouldBe ListBuffer(1, 2, 3)
    NonEmptyVector(1, 2, 3).to(Vector) shouldBe Vector(1, 2, 3)
  }
  it should "have a toArray method" in {
    NonEmptyVector(1, 2, 3).toArray should === (Array(1, 2, 3))
    NonEmptyVector("a", "b").toArray should === (Array("a", "b"))
    NonEmptyVector(1).toArray should === (Array(1))
  }
  it should "have a toBuffer method" in {
    NonEmptyVector(1, 2, 3).toBuffer should === (Buffer(1, 2, 3))
    NonEmptyVector("a", "b").toBuffer should === (Buffer("a", "b"))
    NonEmptyVector(1).toBuffer should === (Buffer(1))
  }
  it should "have a toIndexedSeq method" in {
    NonEmptyVector(1, 2, 3).toIndexedSeq should === (IndexedSeq(1, 2, 3))
    NonEmptyVector("a", "b").toIndexedSeq should === (IndexedSeq("a", "b"))
    NonEmptyVector(1).toIndexedSeq should === (IndexedSeq(1))
  }
  it should "have a toIterable method" in {
    NonEmptyVector(1, 2, 3).toIterable should === (Iterable(1, 2, 3))
    NonEmptyVector("a", "b").toIterable should === (Iterable("a", "b"))
    NonEmptyVector(1).toIterable should === (Iterable(1))
  }
  it should "have a toIterator method" in {
    NonEmptyVector(1, 2, 3).toIterator.toVector should === (Iterator(1, 2, 3).toVector)
    NonEmptyVector("a", "b").toIterator.toVector should === (Iterator("a", "b").toVector)
    NonEmptyVector(1).toIterator.toVector should === (Iterator(1).toVector)
    NonEmptyVector(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    NonEmptyVector("a", "b").toIterator shouldBe an [Iterator[_]]
    NonEmptyVector(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    NonEmptyVector(1, 2, 3).toList should === (List(1, 2, 3))
    NonEmptyVector("a", "b").toList should === (List("a", "b"))
    NonEmptyVector(1).toList should === (List(1))
  }
  it should "have a toMap method" in {
    NonEmptyVector("1" -> 1, "2" -> 2, "3" -> 3).toMap should === (Map("1" -> 1, "2" -> 2, "3" -> 3))
    NonEmptyVector('A' -> "a", 'B' -> "b").toMap should === (Map('A' -> "a", 'B' -> "b"))
    NonEmptyVector("1" -> 1).toMap should === (Map("1" -> 1))
  }
  it should "have a toSeq method" in {
    NonEmptyVector(1, 2, 3).toSeq should === (Seq(1, 2, 3))
    NonEmptyVector("a", "b").toSeq should === (Seq("a", "b"))
    NonEmptyVector(1).toSeq should === (Seq(1))
  }
  it should "have a toSet method" in {
    NonEmptyVector(1, 2, 3).toSet should === (Set(1, 2, 3))
    NonEmptyVector("a", "b").toSet should === (Set("a", "b"))
    NonEmptyVector(1).toSet should === (Set(1))
  }
  it should "have a toStream method" in {
    NonEmptyVector(1, 2, 3).toStream should === (Stream(1, 2, 3))
    NonEmptyVector("a", "b").toStream should === (Stream("a", "b"))
    NonEmptyVector(1).toStream should === (Stream(1))
  }
  it should "have a toString method" in {
    NonEmptyVector(1, 2, 3).toString should === ("NonEmptyVector(1, 2, 3)")
    NonEmptyVector(1, 2, 3).toString should === ("NonEmptyVector(1, 2, 3)")
    NonEmptyVector(1).toString should === ("NonEmptyVector(1)")
  }
  it should "have a toVector method" in {
    NonEmptyVector(1, 2, 3).toVector should === (Vector(1, 2, 3))
    NonEmptyVector("a", "b").toVector should === (Vector("a", "b"))
    NonEmptyVector(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    NonEmptyVector(NonEmptyVector(1, 2, 3), NonEmptyVector(4, 5, 6), NonEmptyVector(7, 8, 9)).transpose shouldBe NonEmptyVector(NonEmptyVector(1, 4, 7), NonEmptyVector(2, 5, 8), NonEmptyVector(3, 6, 9))
    NonEmptyVector(NonEmptyVector(1, 2), NonEmptyVector(3, 4), NonEmptyVector(5, 6), NonEmptyVector(7, 8)).transpose shouldBe NonEmptyVector(NonEmptyVector(1, 3, 5, 7), NonEmptyVector(2, 4, 6, 8))
    NonEmptyVector(NonEmptyVector(1, 2), NonEmptyVector(3, 4), NonEmptyVector(5, 6), NonEmptyVector(7, 8)).transpose.transpose shouldBe NonEmptyVector(NonEmptyVector(1, 2), NonEmptyVector(3, 4), NonEmptyVector(5, 6), NonEmptyVector(7, 8))
    NonEmptyVector(NonEmptyVector(1, 2, 3), NonEmptyVector(4, 5, 6), NonEmptyVector(7, 8, 9)).transpose.transpose shouldBe NonEmptyVector(NonEmptyVector(1, 2, 3), NonEmptyVector(4, 5, 6), NonEmptyVector(7, 8, 9))
  }
  it should "have a union method that takes a GenSeq" in {
    NonEmptyVector(1) union Vector(1) shouldBe NonEmptyVector(1, 1)
    NonEmptyVector(1) union Vector(1, 2) shouldBe NonEmptyVector(1, 1, 2)
    NonEmptyVector(1, 2) union Vector(1, 2) shouldBe NonEmptyVector(1, 2, 1, 2)
    NonEmptyVector(1, 2) union Vector(1) shouldBe NonEmptyVector(1, 2, 1)
    NonEmptyVector(1, 2) union Vector(3, 4, 5) shouldBe NonEmptyVector(1, 2, 3, 4, 5)
    NonEmptyVector(1, 2, 3) union Vector(3, 4, 5) shouldBe NonEmptyVector(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes an Every" in {
    NonEmptyVector(1) union Every(1) shouldBe NonEmptyVector(1, 1)
    NonEmptyVector(1) union Every(1, 2) shouldBe NonEmptyVector(1, 1, 2)
    NonEmptyVector(1, 2) union Every(1, 2) shouldBe NonEmptyVector(1, 2, 1, 2)
    NonEmptyVector(1, 2) union Every(1) shouldBe NonEmptyVector(1, 2, 1)
    NonEmptyVector(1, 2) union Every(3, 4, 5) shouldBe NonEmptyVector(1, 2, 3, 4, 5)
    NonEmptyVector(1, 2, 3) union Every(3, 4, 5) shouldBe NonEmptyVector(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes a NonEmptyVector" in {
    NonEmptyVector(1) union NonEmptyVector(1) shouldBe NonEmptyVector(1, 1)
    NonEmptyVector(1) union NonEmptyVector(1, 2) shouldBe NonEmptyVector(1, 1, 2)
    NonEmptyVector(1, 2) union NonEmptyVector(1, 2) shouldBe NonEmptyVector(1, 2, 1, 2)
    NonEmptyVector(1, 2) union NonEmptyVector(1) shouldBe NonEmptyVector(1, 2, 1)
    NonEmptyVector(1, 2) union NonEmptyVector(3, 4, 5) shouldBe NonEmptyVector(1, 2, 3, 4, 5)
    NonEmptyVector(1, 2, 3) union NonEmptyVector(3, 4, 5) shouldBe NonEmptyVector(1, 2, 3, 3, 4, 5)
  }
  it should "have an unzip method" in {
    NonEmptyVector((1, 2)).unzip shouldBe (NonEmptyVector(1),NonEmptyVector(2))
    NonEmptyVector((1, 2), (3, 4)).unzip shouldBe (NonEmptyVector(1, 3), NonEmptyVector(2, 4))
    NonEmptyVector((1, 2), (3, 4), (5, 6)).unzip shouldBe (NonEmptyVector(1, 3, 5), NonEmptyVector(2, 4, 6))
  }
  it should "have an unzip3 method" in {
    NonEmptyVector((1, 2, 3)).unzip3 shouldBe (NonEmptyVector(1), NonEmptyVector(2), NonEmptyVector(3))
    NonEmptyVector((1, 2, 3), (4, 5, 6)).unzip3 shouldBe (NonEmptyVector(1, 4), NonEmptyVector(2, 5), NonEmptyVector(3, 6))
    NonEmptyVector((1, 2, 3), (4, 5, 6), (7, 8, 9)).unzip3 shouldBe (NonEmptyVector(1, 4, 7), NonEmptyVector(2, 5, 8), NonEmptyVector(3, 6, 9))
  }
  it should "have an updated method" in {
    NonEmptyVector(1).updated(0, 2) shouldBe NonEmptyVector(2)
    an [IndexOutOfBoundsException] should be thrownBy { NonEmptyVector(1).updated(1, 2) }
    NonEmptyVector(1, 1, 1).updated(1, 2) shouldBe NonEmptyVector(1, 2, 1)
    NonEmptyVector(1, 1, 1).updated(2, 2) shouldBe NonEmptyVector(1, 1, 2)
    NonEmptyVector(1, 1, 1).updated(0, 2) shouldBe NonEmptyVector(2, 1, 1)
  }
  /*
  it should not have 2 view methods, because I don't want to support views in Every
  */
  /*
    it should not have a zip method
      scala> Vector(1) zip Nil
      res0: Vector[(Int, Nothing)] = Vector()
  */
  it should "have a zipAll method that takes an Iterable" in {

    // Empty on right
    NonEmptyVector(1).zipAll(Nil, -1, -2) shouldBe NonEmptyVector((1, -2))
    NonEmptyVector(1, 2).zipAll(Nil, -1, -2) shouldBe NonEmptyVector((1, -2), (2, -2))

    // Same length
    NonEmptyVector(1).zipAll(Vector(1), -1, -2) shouldBe NonEmptyVector((1, 1))
    NonEmptyVector(1, 2).zipAll(Vector(1, 2), -1, -2) shouldBe NonEmptyVector((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyVector(1).zipAll(Vector(10, 20), -1, -2) shouldBe NonEmptyVector((1,10), (-1,20))
    NonEmptyVector(1, 2).zipAll(Vector(10, 20, 30), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyVector(1, 2, 3).zipAll(Vector(10, 20), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (3,-2))
    NonEmptyVector(1, 2, 3, 4).zipAll(Vector(10, 20, 30), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    NonEmptyVector(1).zipAll(Every(1), -1, -2) shouldBe NonEmptyVector((1, 1))
    NonEmptyVector(1, 2).zipAll(Every(1, 2), -1, -2) shouldBe NonEmptyVector((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyVector(1).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptyVector((1,10), (-1,20))
    NonEmptyVector(1, 2).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyVector(1, 2, 3).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (3,-2))
    NonEmptyVector(1, 2, 3, 4).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes a NonEmptyVector" in {

    // Same length
    NonEmptyVector(1).zipAll(NonEmptyVector(1), -1, -2) shouldBe NonEmptyVector((1, 1))
    NonEmptyVector(1, 2).zipAll(NonEmptyVector(1, 2), -1, -2) shouldBe NonEmptyVector((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyVector(1).zipAll(NonEmptyVector(10, 20), -1, -2) shouldBe NonEmptyVector((1,10), (-1,20))
    NonEmptyVector(1, 2).zipAll(NonEmptyVector(10, 20, 30), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyVector(1, 2, 3).zipAll(NonEmptyVector(10, 20), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (3,-2))
    NonEmptyVector(1, 2, 3, 4).zipAll(NonEmptyVector(10, 20, 30), -1, -2) shouldBe NonEmptyVector((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipWithIndex method" in {
    NonEmptyVector(99).zipWithIndex shouldBe NonEmptyVector((99,0))
    NonEmptyVector(1, 2, 3, 4, 5).zipWithIndex shouldBe NonEmptyVector((1,0), (2,1), (3,2), (4,3), (5,4))
  }
  "End" should "have a pretty toString" in {
    End.toString shouldBe "End"
  }
}

