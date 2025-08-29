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
import scala.collection.mutable.ArrayBuffer

import org.scalactic.{Every, One, Many, StringNormalizations}
import org.scalactic.UnitSpec
import org.scalactic.NormalizingEquality

import org.scalatest.CompatParColls.Converters._

class NonEmptyArraySpec extends UnitSpec {
  "A NonEmptyArray" can "be constructed with one element" in {
    val onesie = NonEmptyArray(3)
    onesie.length shouldBe 1
    onesie(0) shouldBe 3
  }
  it can "be constructed with many elements" in {
    val twosie = NonEmptyArray(2, 3)
    twosie.length shouldBe 2
    twosie(0) shouldBe 2
    twosie(1) shouldBe 3
    val threesie = NonEmptyArray(1, 2, 3)
    threesie.length shouldBe 3
    threesie(0) shouldBe 1
    threesie(1) shouldBe 2
    threesie(2) shouldBe 3
  }
  it can "be constructed from a Iterable via the from method on NonEmptyArray singleton" in {
    NonEmptyArray.from(Array.empty[String]) shouldBe None
    NonEmptyArray.from(Array("1")).get shouldBe NonEmptyArray("1")
    NonEmptyArray.from(Array(1, 2, 3)).get shouldBe NonEmptyArray(1, 2, 3)
    // SKIP-SCALATESTJS,NATIVE-START
    NonEmptyArray.from(Array.empty[String].par) shouldBe None
    NonEmptyArray.from(Array("1").par).get shouldBe NonEmptyArray("1")
    NonEmptyArray.from(Array(1, 2, 3).par).get shouldBe NonEmptyArray(1, 2, 3)
    // SKIP-SCALATESTJS,NATIVE-END
  }
  // This does not compile with scala 2.10
  /*it can "be constructed with null elements" in {
    noException should be thrownBy NonEmptyArray[String]("hi", null, "ho")
    noException should be thrownBy NonEmptyArray[String](null)
    noException should be thrownBy NonEmptyArray[String]("ho", null)
  }*/
  it can "be deconstructed with NonEmptyArray" in {
    NonEmptyArray(1) match {
      case NonEmptyArray(x) => x shouldEqual 1
      case _ => fail()
    }
    NonEmptyArray("hi") match {
      case NonEmptyArray(s) => s shouldEqual "hi"
      case _ => fail()
    }
  }
  it can "be deconstructed with Many" in {
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyArray("hi", "there") match {
      case NonEmptyArray(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3, 4, 5) match {
      case NonEmptyArray(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
  }
  it can "be deconstructed with Every" in {
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyArray("hi", "there") match {
      case NonEmptyArray(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3, 4, 5) match {
      case NonEmptyArray(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, _*) => x shouldEqual 1
      case _ => fail()
    }
    NonEmptyArray("hi") match {
      case NonEmptyArray(s) => s shouldEqual "hi"
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, y, z) => (x, y, z) shouldEqual (1, 2, 3)
      case _ => fail()
    }
    NonEmptyArray("hi", "there") match {
      case NonEmptyArray(s, t) => (s, t) shouldEqual ("hi", "there")
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, y, _) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3, 4, 5) match {
      case NonEmptyArray(x, y, _*) => (x, y) shouldEqual (1, 2)
      case _ => fail()
    }
    NonEmptyArray(1, 2, 3) match {
      case NonEmptyArray(x, _*) => x shouldEqual 1
      case _ => fail()
    }
  }
  it should "have an apply method" in {
    val arr1 = NonEmptyArray(1, 2, 3)
    arr1(0) shouldEqual 1
    val arr2 = NonEmptyArray(1, 2, 3)
    arr2(1) shouldEqual 2
    val arr3 = NonEmptyArray("hi")
    arr3(0) shouldEqual "hi"
    val arr4 = NonEmptyArray(7, 8, 9)
    arr4(2) shouldEqual 9
    // SKIP-SCALATESTJS,NATIVE-START
    the [IndexOutOfBoundsException] thrownBy { // In ScalaJs, this throws scala.scalajs.runtime.UndefinedBehaviorError
      val arr5 = NonEmptyArray(1, 2, 3)        // TODO, might be nice to check for that exception on ScalaJS instead of just skipping the check
      arr5(3)
    } should (have message "3" or have message "Index 3 out of bounds for length 3")  // message 3 is in jdk8 and older, the new message is used by newer version of jdk.
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it should "have a length method" in {
    NonEmptyArray(1).length shouldBe 1
    NonEmptyArray(1, 2).length shouldBe 2
    NonEmptyArray(1, 2, 3, 4, 5).length shouldBe 5
  }
  it should "have a ++ method that takes another NonEmptyArray" in {
    NonEmptyArray(1, 2, 3) ++ NonEmptyArray(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ NonEmptyArray(4, 5) shouldEqual NonEmptyArray(1, 2, 3, 4, 5)
    NonEmptyArray(1, 2, 3) ++ NonEmptyArray(4, 5, 6) shouldEqual NonEmptyArray(1, 2, 3, 4, 5, 6)
  }
  it should "have a ++ method that takes an Every" in {
    NonEmptyArray(1, 2, 3) ++ One(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ Every(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ Every(4, 5, 6) shouldEqual NonEmptyArray(1, 2, 3, 4, 5, 6)
    NonEmptyArray(1, 2, 3) ++ One(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ One(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ Every(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ Every(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ One(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
  }
  it should "have a ++ method that takes a IterableOnce" in {
    NonEmptyArray(1, 2, 3) ++ Array(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ Vector(4, 5, 6) shouldEqual NonEmptyArray(1, 2, 3, 4, 5, 6)
    NonEmptyArray(1, 2, 3) ++ Iterable(4) shouldEqual NonEmptyArray(1, 2, 3, 4)
    NonEmptyArray(1, 2, 3) ++ Set(4, 5) shouldEqual NonEmptyArray(1, 2, 3, 4, 5)
    NonEmptyArray(1, 2, 3) ++ Set(4, 5).iterator shouldEqual NonEmptyArray(1, 2, 3, 4, 5)
  }
  it should "have a +: method" in {
    0 +: NonEmptyArray(1) shouldBe NonEmptyArray(0, 1)
    0 +: NonEmptyArray(1, 2) shouldBe NonEmptyArray(0, 1, 2)
    "zero" +: NonEmptyArray("one", "two") shouldBe NonEmptyArray("zero", "one", "two")
  }
  it should "implement PartialFunction[Int, T]" in {
    val pf1: PartialFunction[Int, Int] = NonEmptyArray(1)
    pf1.isDefinedAt(0) shouldBe true
    pf1.isDefinedAt(1) shouldBe false
  }
  it should "have a :+ method" in {
    NonEmptyArray(1) :+ 2 shouldBe NonEmptyArray(1, 2)
    NonEmptyArray(1, 2) :+ 3 shouldBe NonEmptyArray(1, 2, 3)
  }
  it should "have 3 addString methods" in {
    NonEmptyArray("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    NonEmptyArray(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("123")

    NonEmptyArray("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    NonEmptyArray(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    NonEmptyArray(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    NonEmptyArray("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    NonEmptyArray(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    NonEmptyArray(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = NonEmptyArray(1) andThen (_ + 1)
    pf1(0) shouldEqual 2
    val pf2 = NonEmptyArray(1, 2, 3) andThen (_ + 1)
    pf2(0) shouldEqual 2
    pf2(1) shouldEqual 3
    pf2(2) shouldEqual 4
  }
  it should "have an applyOrElse method (inherited from PartialFunction)" in {
    NonEmptyArray(1, 2, 3).applyOrElse(0, (_: Int) * -1) shouldEqual 1
    NonEmptyArray(1, 2, 3).applyOrElse(1, (_: Int) * -1) shouldEqual 2
    NonEmptyArray(1, 2, 3).applyOrElse(2, (_: Int) * -1) shouldEqual 3
    NonEmptyArray(1, 2, 3).applyOrElse(3, (_: Int) * -1) shouldEqual -3
    NonEmptyArray(1, 2, 3).applyOrElse(4, (_: Int) * -1) shouldEqual -4
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
    NonEmptyArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collectFirst { case i if i > 10 => i / 2 } shouldBe None
    NonEmptyArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) collectFirst { case i if i > 10 => i / 2 } shouldBe Some(5)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Int = NonEmptyArray(1, 2, 3).compose((_: Int) + 1)
    fn(-1) shouldBe 1
    fn(0) shouldBe 2
    fn(1) shouldBe 3
  }
  it should "have a contains method" in {
    val e = NonEmptyArray(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    val es = NonEmptyArray("one", "two", "three")
    es.contains("one") shouldBe true;
    es.contains("ONE") shouldBe false;
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.contains("one") shouldBe true;
    es.contains("ONE") shouldBe false
    // SKIP-DOTTY-END
  }
  // Decided to just overload one for GenSeq and one for Every. Could have done
  // what that has a Slicing nature, but that's a bit too fancy pants.
  it should "have a containsSlice method that takes GenSeq" in {
    val nonEmptyArray = NonEmptyArray(1, 2, 3, 4, 5)
    nonEmptyArray.containsSlice(Array(2, 3)) shouldBe true
    nonEmptyArray.containsSlice(Array(2, 3, 5)) shouldBe false
    nonEmptyArray.containsSlice(Array.empty[Int]) shouldBe true
    nonEmptyArray.containsSlice(Vector(2, 3)) shouldBe true
    nonEmptyArray.containsSlice(Vector(2, 3, 5)) shouldBe false
    nonEmptyArray.containsSlice(Vector.empty) shouldBe true
    nonEmptyArray.containsSlice(ArrayBuffer(2, 3)) shouldBe true
    nonEmptyArray.containsSlice(ArrayBuffer(2, 3, 5)) shouldBe false
    nonEmptyArray.containsSlice(ArrayBuffer.empty) shouldBe true
  }
  it should "have a containsSlice method that takes an Every" in {
    val nonEmptyArray = NonEmptyArray(1, 2, 3, 4, 5)
    nonEmptyArray.containsSlice(Every(2, 3)) shouldBe true
    nonEmptyArray.containsSlice(Every(2, 3, 5)) shouldBe false
    nonEmptyArray.containsSlice(Every(3)) shouldBe true
  }
  it should "have a containsSlice method that takes a NonEmptyArray" in {
    val nonEmptyArray = NonEmptyArray(1, 2, 3, 4, 5)
    nonEmptyArray.containsSlice(NonEmptyArray(2, 3)) shouldBe true
    nonEmptyArray.containsSlice(NonEmptyArray(2, 3, 5)) shouldBe false
    nonEmptyArray.containsSlice(NonEmptyArray(3)) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)(-1)
    NonEmptyArray(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(1, 2, 3, 4, 5)

    val arr2 = Array.fill(5)(-1)
    NonEmptyArray(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(-1, 1, 2, 3, 4)

    val arr3 = Array.fill(5)(-1)
    NonEmptyArray(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(-1, 1, 2, -1, -1)
  }
  it should "have a copyToBuffer method" in {

    val buf = ArrayBuffer.fill(3)(-1)
    NonEmptyArray(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(-1, -1, -1, 1, 2, 3, 4, 5)
  }
  it should "have a corresponds method that takes a GenSeq" in {
    val nonEmptyArray = NonEmptyArray(1, 2, 3, 4, 5)
    nonEmptyArray.corresponds(Array(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyArray.corresponds(Array(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyArray.corresponds(Array(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyArray.corresponds(Array(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes an Every" in {
    val nonEmptyArray = NonEmptyArray(1, 2, 3, 4, 5)
    nonEmptyArray.corresponds(Many(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyArray.corresponds(Many(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyArray.corresponds(Many(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyArray.corresponds(Many(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes a NonEmptyArray" in {
    val nonEmptyArray = NonEmptyArray(1, 2, 3, 4, 5)
    nonEmptyArray.corresponds(NonEmptyArray(2, 4, 6, 8, 10))(_ * 2 == _) shouldBe true
    nonEmptyArray.corresponds(NonEmptyArray(2, 4, 6, 8, 11))(_ * 2 == _) shouldBe false
    nonEmptyArray.corresponds(NonEmptyArray(2, 4, 6, 8))(_ * 2 == _) shouldBe false
    nonEmptyArray.corresponds(NonEmptyArray(2, 4, 6, 8, 10, 12))(_ * 2 == _) shouldBe false
  }
  it should "have a count method" in {
    val nonEmptyArray = NonEmptyArray(1, 2, 3, 4, 5)
    nonEmptyArray.count(_ > 10) shouldBe 0
    nonEmptyArray.count(_ % 2 == 0) shouldBe 2
    nonEmptyArray.count(_ % 2 == 1) shouldBe 3
  }
  /*
  it should not have a diff method
    scala> Vector(1, 2, 3).diff(Vector(1, 2, 3))
    res0: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a distinct method" in {
    NonEmptyArray(1, 2, 3).distinct shouldBe NonEmptyArray(1, 2, 3)
    NonEmptyArray(1).distinct shouldBe NonEmptyArray(1)
    NonEmptyArray(1, 2, 1, 1).distinct shouldBe NonEmptyArray(1, 2)
    NonEmptyArray(1, 1, 1).distinct shouldBe NonEmptyArray(1)
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
    NonEmptyArray(1).endsWith(Array(1)) shouldBe true
    NonEmptyArray(1).endsWith(Array(1, 2)) shouldBe false
    NonEmptyArray(1, 2).endsWith(Array(1, 2)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(Array(1, 2)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(Array(5)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(Array(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes an Every" in {
    NonEmptyArray(1).endsWith(Every(1)) shouldBe true
    NonEmptyArray(1).endsWith(Every(1, 2)) shouldBe false
    NonEmptyArray(1, 2).endsWith(Every(1, 2)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(Every(1, 2)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(Every(5)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(Every(3, 4, 5)) shouldBe true
  }
  it should "have an endsWith method that takes a NonEmptyArray" in {
    NonEmptyArray(1).endsWith(NonEmptyArray(1)) shouldBe true
    NonEmptyArray(1).endsWith(NonEmptyArray(1, 2)) shouldBe false
    NonEmptyArray(1, 2).endsWith(NonEmptyArray(1, 2)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(NonEmptyArray(1, 2)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(NonEmptyArray(5)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).endsWith(NonEmptyArray(3, 4, 5)) shouldBe true
  }
  it should "have an equals method" in {
    NonEmptyArray(1) shouldEqual NonEmptyArray(1)
    NonEmptyArray(1) should not equal NonEmptyArray(2)
    NonEmptyArray(1, 2) should not equal NonEmptyArray(2, 3)
  }
  it should "have an exists method" in {
    NonEmptyArray(1, 2, 3).exists(_ == 2) shouldBe true
    NonEmptyArray(1, 2, 3).exists(_ == 5) shouldBe false
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
    NonEmptyArray(1, 2, 3).find(_ == 5) shouldBe None
    NonEmptyArray(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have a flatMap method" in {
    NonEmptyArray(1, 2, 3) flatMap (i => NonEmptyArray(i + 1)) shouldBe NonEmptyArray(2, 3, 4)
    val ss = NonEmptyArray("hi", "ho")
    val is = NonEmptyArray(1, 2, 3)
    (for (s <- ss; i <- is) yield (s, i)) shouldBe
      NonEmptyArray(
        ("hi",1), ("hi",2), ("hi",3), ("ho",1), ("ho",2), ("ho",3)
      )
    NonEmptyArray(5) flatMap (i => NonEmptyArray(i + 3)) shouldBe NonEmptyArray(8)
    NonEmptyArray(8) flatMap (i => NonEmptyArray(i.toString)) shouldBe NonEmptyArray("8")
  }
  /*
    Can only flatten NonEmptyArrays
    scala> Vector(Set.empty[Int], Set.empty[Int]).flatten
    res17: scala.collection.immutable.Vector[Int] = Vector()
  */
  // TODO: Actually it would make sense to flatten Everys too
  it should "have a flatten method that works on nested NonEmptyArrays" in {
    NonEmptyArray(NonEmptyArray(1, 2, 3), NonEmptyArray(1, 2, 3)).flatten shouldBe NonEmptyArray(1, 2, 3, 1, 2, 3)
    NonEmptyArray(NonEmptyArray(1)).flatten shouldBe NonEmptyArray(1)
  }
  it can "be flattened when in a IterableOnce" in {
    Vector(NonEmptyArray(1, 2, 3), NonEmptyArray(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
    Array(NonEmptyArray(1, 2, 3), NonEmptyArray(1, 2, 3)).flatten shouldBe Array(1, 2, 3, 1, 2, 3)
    Array(NonEmptyArray(1, 2, 3), NonEmptyArray(1, 2, 3)).toIterator.flatten.toStream shouldBe Array(1, 2, 3, 1, 2, 3).toIterator.toStream
    // SKIP-SCALATESTJS,NATIVE-START
    Array(NonEmptyArray(1, 2, 3), NonEmptyArray(1, 2, 3)).par.flatten shouldBe Array(1, 2, 3, 1, 2, 3).par
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it should "have a fold method" in {
    NonEmptyArray(1).fold(0)(_ + _) shouldBe 1
    NonEmptyArray(1).fold(1)(_ * _) shouldBe 1
    NonEmptyArray(2).fold(0)(_ + _) shouldBe 2
    NonEmptyArray(2).fold(1)(_ * _) shouldBe 2
    NonEmptyArray(3).fold(0)(_ + _) shouldBe 3
    NonEmptyArray(3).fold(1)(_ * _) shouldBe 3
    NonEmptyArray(1, 2, 3).fold(0)(_ + _) shouldBe 6
    NonEmptyArray(1, 2, 3).fold(1)(_ * _) shouldBe 6
    NonEmptyArray(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    NonEmptyArray(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    NonEmptyArray(1).foldLeft(0)(_ + _) shouldBe 1
    NonEmptyArray(1).foldLeft(1)(_ + _) shouldBe 2
    NonEmptyArray(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    NonEmptyArray(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    NonEmptyArray(1).foldRight(0)(_ + _) shouldBe 1
    NonEmptyArray(1).foldRight(1)(_ + _) shouldBe 2
    NonEmptyArray(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    NonEmptyArray(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    NonEmptyArray(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    NonEmptyArray(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- NonEmptyArray(1, 2, 3))
      num += i
    num shouldBe 12
    NonEmptyArray(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    val r1 = NonEmptyArray(1, 2, 3, 4, 5).groupBy(_ % 2)
    r1.size shouldBe 2
    r1(0) shouldBe NonEmptyArray(2, 4)
    r1(1) shouldBe NonEmptyArray(1, 3, 5)

    val r2 = NonEmptyArray(1, 2, 3, 3, 3).groupBy(_ % 2)
    r2.size shouldBe 2
    r2(0) shouldBe NonEmptyArray(2)
    r2(1) shouldBe NonEmptyArray(1, 3, 3, 3)

    val r3 = NonEmptyArray(1, 1, 3, 3, 3).groupBy(_ % 2)
    r3.size shouldBe 1
    r3(1) shouldBe NonEmptyArray(1, 1, 3, 3, 3)

    val r4 = NonEmptyArray(1, 2, 3, 5, 7).groupBy(_ % 2)
    r4.size shouldBe 2
    r4(0) shouldBe NonEmptyArray(2)
    r4(1) shouldBe NonEmptyArray(1, 3, 5, 7)
  }
  it should "have a grouped method" in {
    val r1 = NonEmptyArray(1, 2, 3).grouped(2)
    r1.next shouldBe NonEmptyArray(1, 2)
    r1.next shouldBe NonEmptyArray(3)
    r1.hasNext shouldBe false

    val r2 = NonEmptyArray(1, 2, 3).grouped(1)
    r2.next shouldBe NonEmptyArray(1)
    r2.next shouldBe NonEmptyArray(2)
    r2.next shouldBe NonEmptyArray(3)
    r2.hasNext shouldBe false

    an [IllegalArgumentException] should be thrownBy { NonEmptyArray(1, 2, 3).grouped(0) }

    val r3 = NonEmptyArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2)
    r3.next shouldBe NonEmptyArray(1, 2)
    r3.next shouldBe NonEmptyArray(3, 4)
    r3.next shouldBe NonEmptyArray(5, 6)
    r3.next shouldBe NonEmptyArray(7, 8)
    r3.next shouldBe NonEmptyArray(9, 10)
    r3.hasNext shouldBe false

    val r4 = NonEmptyArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3)
    r4.next shouldBe NonEmptyArray(1, 2, 3)
    r4.next shouldBe NonEmptyArray(4, 5, 6)
    r4.next shouldBe NonEmptyArray(7, 8, 9)
    r4.next shouldBe NonEmptyArray(10)
    r4.hasNext shouldBe false

    val r5 = NonEmptyArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4)
    r5.next shouldBe NonEmptyArray(1, 2, 3, 4)
    r5.next shouldBe NonEmptyArray(5, 6, 7, 8)
    r5.next shouldBe NonEmptyArray(9, 10)
    r5.hasNext shouldBe false

    val r6 = NonEmptyArray(1).grouped(2)
    r6.next shouldBe NonEmptyArray(1)
    r6.hasNext shouldBe false

    val r7 = NonEmptyArray(1).grouped(1)
    r7.next shouldBe NonEmptyArray(1)
    r7.hasNext shouldBe false
  }
  it should "have a hasDefiniteSize method" in {
    NonEmptyArray(1).hasDefiniteSize shouldBe true
    NonEmptyArray(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    NonEmptyArray(1).hashCode should not equal NonEmptyArray(1).hashCode
    NonEmptyArray(1, 2).hashCode should not equal NonEmptyArray(1, 2).hashCode
  }
  it should "have a head method" in {
    NonEmptyArray("hi").head shouldBe "hi"
    NonEmptyArray(1, 2, 3).head shouldBe 1
  }
  it should "have a headOption method" in {
    NonEmptyArray("hi").headOption shouldBe Some("hi")
    NonEmptyArray(1, 2, 3).headOption shouldBe Some(1)
  }
  it should "have 2 indexOf methods" in {
    NonEmptyArray(1, 2, 3, 4, 5).indexOf(3) shouldBe 2
    NonEmptyArray(1, 2, 3, 4, 5).indexOf(1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOf(1, 2) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOf(6) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOf(5, 3) shouldBe 4

    val es = NonEmptyArray("one", "two", "three")
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
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(2, 3)) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(2, 3), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(2, 3, 5), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(2, 3, 5)) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(5)) shouldBe 4
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(1, 2, 3, 4, 5), 1) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array(1, 2, 3, 4, 5), -1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array.empty[Int]) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array.empty[Int], 6) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Array.empty[Int], 4) shouldBe 4

    val es = NonEmptyArray("one", "two", "three", "four", "five")
    es.indexOfSlice(Array("one", "two")) shouldBe 0;
    es.indexOfSlice(Array("one", "two"), 1) shouldBe -1
    es.indexOfSlice(Array("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(Array("one", "two")) shouldBe 0
    es.indexOfSlice(Array("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take an Every" in {
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3)) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(2, 3, 5)) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(5)) shouldBe 4
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe 0

    val es = NonEmptyArray("one", "two", "three", "four", "five")
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
  it should "have 2 indexOfSlice methods that take a NonEmptyArray" in {
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(2, 3)) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(2, 3), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(2, 3, 5), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(2, 3, 5)) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(5)) shouldBe 4
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(1, 2, 3, 4, 5), 1) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexOfSlice(NonEmptyArray(1, 2, 3, 4, 5), -1) shouldBe 0

    val es = NonEmptyArray("one", "two", "three", "four", "five")
    es.indexOfSlice(NonEmptyArray("one", "two")) shouldBe 0
    es.indexOfSlice(NonEmptyArray("one", "two"), 1) shouldBe -1
    es.indexOfSlice(NonEmptyArray("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(NonEmptyArray("one", "two")) shouldBe 0
    es.indexOfSlice(NonEmptyArray("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexWhere methods" in {
    NonEmptyArray(1, 2, 3, 4, 5).indexWhere(_ == 3) shouldBe 2
    NonEmptyArray(1, 2, 3, 4, 5).indexWhere(_ == 1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).indexWhere(_ == 1, 2) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexWhere(_ == 6) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).indexWhere(_ == 5, 3) shouldBe 4
  }
  it should "have an indices method" in {
    NonEmptyArray(1).indices shouldBe Array(1).indices
    NonEmptyArray(1, 2, 3).indices shouldBe (0 to 2)
    NonEmptyArray(1, 2, 3, 4, 5).indices shouldBe (0 to 4)
  }
  /*
  it should not have an init method
    scala> Vector(1).init
    res30: scala.collection.immutable.Vector[Int] = Vector()

  it should "have an inits method" is pending
    scala> Vector(1).inits.toArray
    res32: Array[scala.collection.immutable.Vector[Int]] = Array(Vector(1), Vector())

  it should "have an intersect method" is pending
    scala> Vector(1, 2, 3) intersect Vector(4, 5)
    res33: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an isDefinedAt method, inherited from PartialFunction" in {
    NonEmptyArray(1).isDefinedAt(0) shouldBe true
    NonEmptyArray(1).isDefinedAt(1) shouldBe false
    NonEmptyArray(1, 2, 3).isDefinedAt(1) shouldBe true
    NonEmptyArray(1, 2, 3).isDefinedAt(2) shouldBe true
    NonEmptyArray(1, 2, 3).isDefinedAt(3) shouldBe false
    NonEmptyArray(1, 2, 3).isDefinedAt(0) shouldBe true
    NonEmptyArray(1, 2, 3).isDefinedAt(-1) shouldBe false
  }
  it should "have an isEmpty method" in {
    NonEmptyArray("hi").isEmpty shouldBe false
    NonEmptyArray(1, 2, 3).isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    NonEmptyArray("hi").isTraversableAgain shouldBe true
    NonEmptyArray(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    NonEmptyArray("hi").iterator.toArray shouldBe Array("hi")
    NonEmptyArray(1, 2, 3).iterator.toArray shouldBe Array(1, 2, 3)
  }
  it should "have a last method" in {
    NonEmptyArray("hi").last shouldBe "hi"
    NonEmptyArray(1, 2, 3).last shouldBe 3
  }
  it should "have 2 lastIndexOf methods" in {
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOf(2) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5, 1).lastIndexOf(1) shouldBe 5
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOf(0) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOf(5) shouldBe 4
    NonEmptyArray(1, 2, 3, 3, 5).lastIndexOf(3) shouldBe 3
    NonEmptyArray(1).lastIndexOf(1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOf(2, 3) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOf(2, 0) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOf(2, 1) shouldBe 1

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
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(2, 3)) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(2, 3), 3) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(2, 3, 5), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(2, 3, 5)) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(5)) shouldBe 4
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array(1, 2, 3, 4, 5), -1) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array.empty[Int]) shouldBe 5
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array.empty[Int], 6) shouldBe 5
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Array.empty[Int], 4) shouldBe 4

    val es = NonEmptyArray("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(Array("one", "two")) shouldBe 0;
    es.lastIndexOfSlice(Array("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(Array("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(Array("one", "two")) shouldBe 0
    es.lastIndexOfSlice(Array("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take an Every" in {
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3)) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3), 3) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(2, 3, 5)) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(5)) shouldBe 4
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(Every(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = NonEmptyArray("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(Every("one", "two")) shouldBe 0;
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
  it should "have 2 lastIndexOfSlice methods that take a NonEmptyArray" in {
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(2, 3)) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(2, 3), 3) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(2, 3, 5), 3) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(2, 3, 5)) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(5)) shouldBe 4
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(1, 2, 3, 4, 5)) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(1, 2, 3, 4, 5), 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(1, 2, 3, 4, 5), 1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexOfSlice(NonEmptyArray(1, 2, 3, 4, 5), -1) shouldBe -1

    val es = NonEmptyArray("one", "two", "three", "four", "five")
    es.lastIndexOfSlice(NonEmptyArray("one", "two")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyArray("two", "three"), 0) shouldBe -1
    es.lastIndexOfSlice(NonEmptyArray("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(NonEmptyArray("one", "two")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyArray("ONE", "TWO")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexWhere methods" in {
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexWhere(_ == 2) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexWhere(_ == 0) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexWhere(_ == 5) shouldBe 4
    NonEmptyArray(1, 2, 3, 3, 5).lastIndexWhere(_ == 3) shouldBe 3
    NonEmptyArray(1).lastIndexWhere(_ == 1) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 3) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 0) shouldBe -1
    NonEmptyArray(1, 2, 3, 4, 5).lastIndexWhere(_ == 2, 1) shouldBe 1
  }
  it should "have an lastOption method" in {
    NonEmptyArray("hi").lastOption shouldBe Some("hi")
    NonEmptyArray(1, 2, 3).lastOption shouldBe Some(3)
  }
  it should "have an lengthCompare method" in {
    NonEmptyArray("hi").lengthCompare(0) should be > 0
    NonEmptyArray("hi").lengthCompare(1) shouldEqual 0
    NonEmptyArray("hi").lengthCompare(2) should be < 0
    NonEmptyArray(1, 2, 3).lengthCompare(0) should be > 0
    NonEmptyArray(1, 2, 3).lengthCompare(1) should be > 0
    NonEmptyArray(1, 2, 3).lengthCompare(2) should be > 0
    NonEmptyArray(1, 2, 3).lengthCompare(3) shouldEqual 0
    NonEmptyArray(1, 2, 3).lengthCompare(4) should be < 0
  }
  it should "have an inherited lift method" in {
    val liftedOne = NonEmptyArray("hi").lift
    liftedOne(0) shouldBe Some("hi")
    liftedOne(1) shouldBe None
    liftedOne(-1) shouldBe None
    val liftedMany = NonEmptyArray(1, 2, 3).lift
    liftedMany(0) shouldBe Some(1)
    liftedMany(1) shouldBe Some(2)
    liftedMany(2) shouldBe Some(3)
    liftedMany(3) shouldBe None
    liftedMany(-1) shouldBe None
  }
  it should "have a map method" in {
    NonEmptyArray(1, 2, 3) map (_ + 1) shouldBe NonEmptyArray(2, 3, 4)
    (for (ele <- NonEmptyArray(1, 2, 3)) yield ele * 2) shouldBe NonEmptyArray(2, 4, 6)
    NonEmptyArray(5) map (_ + 3) shouldBe NonEmptyArray(8)
    NonEmptyArray(8) map (_.toString) shouldBe NonEmptyArray("8")
  }
  it should "have a max method" in {
    NonEmptyArray(1, 2, 3, 4, 5).max shouldBe 5
    NonEmptyArray(1).max shouldBe 1
    NonEmptyArray(-1).max shouldBe -1
    NonEmptyArray("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    NonEmptyArray(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    NonEmptyArray(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    NonEmptyArray(1, 2, 3, 4, 5).min shouldBe 1
    NonEmptyArray(1).min shouldBe 1
    NonEmptyArray(-1).min shouldBe -1
    NonEmptyArray("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    NonEmptyArray(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    NonEmptyArray(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a mkString method" in {
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    NonEmptyArray("hi").mkString shouldBe "hi"
    NonEmptyArray(1, 2, 3).mkString shouldBe "123"
    // SKIP-DOTTY-END

    NonEmptyArray("hi").mkString("#") shouldBe "hi"
    NonEmptyArray(1, 2, 3).mkString("#") shouldBe "1#2#3"
    NonEmptyArray(1, 2, 3).mkString(", ") shouldBe "1, 2, 3"

    NonEmptyArray("hi").mkString("<", "#", ">") shouldBe "<hi>"
    NonEmptyArray(1, 2, 3).mkString("<", "#", ">") shouldBe "<1#2#3>"
    NonEmptyArray(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    NonEmptyArray("hi").nonEmpty shouldBe true
    NonEmptyArray(1, 2, 3).nonEmpty shouldBe true
  }
  it should "have an orElse method, inherited from PartialFunction" in {
    val pf: PartialFunction[Int, Int] = { case i => -i }
    val f = NonEmptyArray(1, 2, 3) orElse pf
    f(0) shouldBe 1
    f(1) shouldBe 2
    f(2) shouldBe 3
    f(3) shouldBe -3
    f(-1) shouldBe 1
  }
  it should "have a padTo method" in {
    NonEmptyArray(1).padTo(0, -1) shouldBe NonEmptyArray(1)
    NonEmptyArray(1).padTo(1, -1) shouldBe NonEmptyArray(1)
    NonEmptyArray(1).padTo(2, -1) shouldBe NonEmptyArray(1, -1)
    NonEmptyArray(1).padTo(3, -1) shouldBe NonEmptyArray(1, -1, -1)
    NonEmptyArray(1, 2, 3).padTo(3, -1) shouldBe NonEmptyArray(1, 2, 3)
    NonEmptyArray(1, 2, 3).padTo(4, -1) shouldBe NonEmptyArray(1, 2, 3, -1)
    NonEmptyArray(1, 2, 3).padTo(5, -1) shouldBe NonEmptyArray(1, 2, 3, -1, -1)
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: nonEmptyArray.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a patch method" in {
    NonEmptyArray(1, 2, 3, 4, 5).patch(2, NonEmptyArray(-3, -4), 2) shouldBe NonEmptyArray(1, 2, -3, -4, 5)
    NonEmptyArray(1, 2, 3, 4, 5).patch(2, NonEmptyArray(-3, -4), 5) shouldBe NonEmptyArray(1, 2, -3, -4)
    NonEmptyArray(1, 2, 3, 4, 5).patch(2, NonEmptyArray(-3, -4), 1) shouldBe NonEmptyArray(1, 2, -3, -4, 4, 5)
    NonEmptyArray(1, 2, 3, 4, 5).patch(4, NonEmptyArray(-3, -4), 2) shouldBe NonEmptyArray(1, 2, 3, 4, -3, -4)
    NonEmptyArray(1, 2, 3, 4, 5).patch(5, NonEmptyArray(-3, -4), 2) shouldBe NonEmptyArray(1, 2, 3, 4, 5, -3, -4)
    NonEmptyArray(1, 2, 3, 4, 5).patch(6, NonEmptyArray(-3, -4), 2) shouldBe NonEmptyArray(1, 2, 3, 4, 5, -3, -4)
    NonEmptyArray(1, 2, 3, 4, 5).patch(0, NonEmptyArray(-3, -4), 2) shouldBe NonEmptyArray(-3, -4, 3, 4, 5)
    NonEmptyArray(1, 2, 3, 4, 5).patch(0, NonEmptyArray(-3, -4), 3) shouldBe NonEmptyArray(-3, -4, 4, 5)
  }
  it should "have a permutations method" in {
    val r1 = NonEmptyArray(1, 2, 3).permutations
    r1.next shouldBe NonEmptyArray(1, 2, 3)
    r1.next shouldBe NonEmptyArray(1, 3, 2)
    r1.next shouldBe NonEmptyArray(2, 1, 3)
    r1.next shouldBe NonEmptyArray(2, 3, 1)
    r1.next shouldBe NonEmptyArray(3, 1, 2)
    r1.next shouldBe NonEmptyArray(3, 2, 1)
    r1.hasNext shouldBe false

    val r2 = NonEmptyArray(1).permutations
    r2.next shouldBe NonEmptyArray(1)
    r2.hasNext shouldBe false

    val r3 = NonEmptyArray(1, 2).permutations
    r3.next shouldBe NonEmptyArray(1, 2)
    r3.next shouldBe NonEmptyArray(2, 1)
    r3.hasNext shouldBe false
  }
  it should "have a prefixLength method" in {
    NonEmptyArray(1, 2, 3, 4, 5).prefixLength(_ == 1) shouldBe 1
    NonEmptyArray(1, 2, 3, 4, 5).prefixLength(_ == 2) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5).prefixLength(_ <= 2) shouldBe 2
    NonEmptyArray(1, 2, 3, 4, 5).prefixLength(_ <= 10) shouldBe 5
    NonEmptyArray(1, 2, 3, 4, 5).prefixLength(_ <= 4) shouldBe 4
  }
  it should "have a product method" in {
    NonEmptyArray(1, 2, 3).product shouldBe 6
    NonEmptyArray(3).product shouldBe 3
    NonEmptyArray(3, 4, 5).product shouldBe 60
    NonEmptyArray(3, 4, 5).product shouldBe 60
    NonEmptyArray(3.1, 4.2, 5.3).product shouldBe 69.006
  }
  it should "have a reduce method" in {
    NonEmptyArray(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    NonEmptyArray(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    NonEmptyArray(5).reduce(_ + _) shouldBe 5
    NonEmptyArray(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    NonEmptyArray(1).reduceLeft(_ + _) shouldBe 1
    NonEmptyArray(1).reduceLeft(_ * _) shouldBe 1
    NonEmptyArray(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    NonEmptyArray(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    NonEmptyArray(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    NonEmptyArray(1).reduceLeftOption(_ + _) shouldBe Some(1)
    NonEmptyArray(1).reduceLeftOption(_ * _) shouldBe Some(1)
    NonEmptyArray(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    NonEmptyArray(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    NonEmptyArray(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    NonEmptyArray(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    NonEmptyArray(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    NonEmptyArray(5).reduceOption(_ + _) shouldBe Some(5)
    NonEmptyArray(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    NonEmptyArray(1).reduceRight(_ * _) shouldBe 1
    NonEmptyArray(1, 2, 3).reduceRight(_ + _) shouldBe 6
    NonEmptyArray(1, 2, 3).reduceRight(_ * _) shouldBe 6
    NonEmptyArray(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    NonEmptyArray(1).reduceRightOption(_ + _) shouldBe Some(1)
    NonEmptyArray(1).reduceRightOption(_ * _) shouldBe Some(1)
    NonEmptyArray(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    NonEmptyArray(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    NonEmptyArray(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a reverse method" in {
    NonEmptyArray(33).reverse shouldBe NonEmptyArray(33)
    NonEmptyArray(33, 34, 35).reverse shouldBe NonEmptyArray(35, 34, 33)
  }
  it should "have a reverseIterator method" in {
    NonEmptyArray(3).reverseIterator.toStream shouldBe Stream(3)
    NonEmptyArray(1, 2, 3).reverseIterator.toStream shouldBe Stream(3, 2, 1)
  }
  it should "have a reverseMap method" in {
    NonEmptyArray(3).reverseMap(_ + 1) shouldBe NonEmptyArray(4)
    NonEmptyArray(1, 2, 3).reverseMap(_ + 1) shouldBe NonEmptyArray(4, 3, 2)
  }
  it should "have a runWith method, inherited from PartialFunction" in {
    // TODO: What is this? Seems to be testing Vector or Array instead of Every or NonEmptyArray.
    var x = 0
    val f = Array(1, 2, 3).runWith(x += _)

    f(0) shouldBe true
    x shouldBe 1

    f(1) shouldBe true
    x shouldBe 3

    f(2) shouldBe true
    x shouldBe 6

    f(3) shouldBe false

    var y = 0
    val g = Array(3).runWith(y += _)

    g(0) shouldBe true
    y shouldBe 3

    g(0) shouldBe true
    y shouldBe 6
  }
  it should "have a sameElements method that takes a GenIterable" in {
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Array(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Array(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Array(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Array(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyArray(3).sameElements(Array(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyArray(3).sameElements(Array(1)) shouldBe false
    NonEmptyArray(3).sameElements(Array(3)) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyArray(3).sameElements(Every(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyArray(3).sameElements(Every(1)) shouldBe false
    NonEmptyArray(3).sameElements(Every(3)) shouldBe true
  }
  it should "have a sameElements method that takes a NonEmptyArray" in {
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(NonEmptyArray(1, 2, 3, 4, 5)) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(NonEmptyArray(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(NonEmptyArray(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptyArray(1, 2, 3, 4, 5).sameElements(NonEmptyArray(1, 2, 3, 4, 4)) shouldBe false
    NonEmptyArray(3).sameElements(NonEmptyArray(1, 2, 3, 4, 5)) shouldBe false
    NonEmptyArray(3).sameElements(NonEmptyArray(1)) shouldBe false
    NonEmptyArray(3).sameElements(NonEmptyArray(3)) shouldBe true
  }
  it should "have a scan method" in {
    NonEmptyArray(1).scan(0)(_ + _) shouldBe NonEmptyArray(0, 1)
    NonEmptyArray(1, 2, 3).scan(0)(_ + _) shouldBe NonEmptyArray(0, 1, 3, 6)
    NonEmptyArray(1, 2, 3).scan("z")(_.toString + _.toString) shouldBe NonEmptyArray("z", "z1", "z12", "z123")
    NonEmptyArray(0).scan("z")(_.toString + _.toString) shouldBe NonEmptyArray("z", "z0")
  }
  it should "have a scanLeft method" in {
    NonEmptyArray(1).scanLeft(0)(_ + _) shouldBe NonEmptyArray(0, 1)
    NonEmptyArray(1, 2, 3).scanLeft(0)(_ + _) shouldBe NonEmptyArray(0, 1, 3, 6)
    NonEmptyArray(1, 2, 3).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptyArray("z", "z1", "z12", "z123")
    NonEmptyArray(0).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptyArray("z", "z0")
  }
  it should "have a scanRight method" in {
    NonEmptyArray(1).scanRight(0)(_ + _) shouldBe NonEmptyArray(1, 0)
    NonEmptyArray(1, 2, 3).scanRight(0)(_ + _) shouldBe NonEmptyArray(6, 5, 3, 0)
    NonEmptyArray(1, 2, 3).scanRight("z")(_.toString + _.toString) shouldBe NonEmptyArray("123z", "23z", "3z", "z")
    NonEmptyArray(0).scanRight("z")(_.toString + _.toString) shouldBe NonEmptyArray("0z", "z")
  }
  it should "have a segmentLength method" in {
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 7, 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ == 7, 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 0) shouldBe 10
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 1, 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 10) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 0, 8) shouldBe 2
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 3, 0) shouldBe 2
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ < 5, 0) shouldBe 4
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 0) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 5) shouldBe 5
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 4) shouldBe 0
    NonEmptyArray(1, 2, 3, 4, 5, 6, 6, 7, 8, 10).segmentLength(_ > 5, 6) shouldBe 4
  }
  // it should "have a seq method" is pending
  it should "have a size method" in {
    NonEmptyArray(5).size shouldBe 1
    NonEmptyArray(1, 2, 3).size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    val r1 = NonEmptyArray(1).sliding(1)
    r1.next shouldBe NonEmptyArray(1)
    r1.hasNext shouldBe false

    val r2 = NonEmptyArray(1).sliding(2)
    r2.next shouldBe NonEmptyArray(1)
    r2.hasNext shouldBe false

    val r3 = NonEmptyArray(1, 2, 3).sliding(2)
    r3.next shouldBe NonEmptyArray(1, 2)
    r3.next shouldBe NonEmptyArray(2, 3)
    r3.hasNext shouldBe false

    val r4 = NonEmptyArray(1, 2, 3).sliding(1)
    r4.next shouldBe NonEmptyArray(1)
    r4.next shouldBe NonEmptyArray(2)
    r4.next shouldBe NonEmptyArray(3)
    r4.hasNext shouldBe false

    val r5 = NonEmptyArray(1, 2, 3).sliding(3)
    r5.next shouldBe NonEmptyArray(1, 2, 3)
    r5.hasNext shouldBe false

    val r6 = NonEmptyArray(1, 2, 3, 4, 5).sliding(3)
    r6.next shouldBe NonEmptyArray(1, 2, 3)
    r6.next shouldBe NonEmptyArray(2, 3, 4)
    r6.next shouldBe NonEmptyArray(3, 4, 5)
    r6.hasNext shouldBe false

    val r7 = NonEmptyArray(1, 2, 3, 4, 5).sliding(2)
    r7.next shouldBe NonEmptyArray(1, 2)
    r7.next shouldBe NonEmptyArray(2, 3)
    r7.next shouldBe NonEmptyArray(3, 4)
    r7.next shouldBe NonEmptyArray(4, 5)
    r7.hasNext shouldBe false

    val r8 = NonEmptyArray(1, 2, 3, 4, 5).sliding(1)
    r8.next shouldBe NonEmptyArray(1)
    r8.next shouldBe NonEmptyArray(2)
    r8.next shouldBe NonEmptyArray(3)
    r8.next shouldBe NonEmptyArray(4)
    r8.next shouldBe NonEmptyArray(5)
    r8.hasNext shouldBe false

    val r9 = NonEmptyArray(1, 2, 3, 4, 5).sliding(4)
    r9.next shouldBe NonEmptyArray(1, 2, 3, 4)
    r9.next shouldBe NonEmptyArray(2, 3, 4, 5)
    r9.hasNext shouldBe false

    val r10 = NonEmptyArray(1, 2, 3, 4, 5).sliding(5)
    r10.next shouldBe NonEmptyArray(1, 2, 3, 4, 5)
    r10.hasNext shouldBe false

    val r11 = NonEmptyArray(1).sliding(1, 1)
    r11.next shouldBe NonEmptyArray(1)

    val r12 = NonEmptyArray(1).sliding(1, 2)
    r12.next shouldBe NonEmptyArray(1)
    r12.hasNext shouldBe false

    val r13 = NonEmptyArray(1, 2, 3).sliding(1, 1)
    r13.next shouldBe NonEmptyArray(1)
    r13.next shouldBe NonEmptyArray(2)
    r13.next shouldBe NonEmptyArray(3)
    r13.hasNext shouldBe false

    val r14 = NonEmptyArray(1, 2, 3).sliding(2, 1)
    r14.next shouldBe NonEmptyArray(1, 2)
    r14.next shouldBe NonEmptyArray(2, 3)
    r14.hasNext shouldBe false

    val r15 = NonEmptyArray(1, 2, 3).sliding(2, 2)
    r15.next shouldBe NonEmptyArray(1, 2)
    r15.next shouldBe NonEmptyArray(3)
    r15.hasNext shouldBe false

    val r16 = NonEmptyArray(1, 2, 3).sliding(3, 2)
    r16.next shouldBe NonEmptyArray(1, 2, 3)
    r16.hasNext shouldBe false

    val r17 = NonEmptyArray(1, 2, 3).sliding(3, 1)
    r17.next shouldBe NonEmptyArray(1, 2, 3)
    r17.hasNext shouldBe false

    val r18 = NonEmptyArray(1, 2, 3, 4, 5).sliding(3, 1)
    r18.next shouldBe NonEmptyArray(1, 2, 3)
    r18.next shouldBe NonEmptyArray(2, 3, 4)
    r18.next shouldBe NonEmptyArray(3, 4, 5)
    r18.hasNext shouldBe false

    val r19 = NonEmptyArray(1, 2, 3, 4, 5).sliding(2, 2)
    r19.next shouldBe NonEmptyArray(1, 2)
    r19.next shouldBe NonEmptyArray(3, 4)
    r19.next shouldBe NonEmptyArray(5)
    r19.hasNext shouldBe false

    val r20 = NonEmptyArray(1, 2, 3, 4, 5).sliding(2, 3)
    r20.next shouldBe NonEmptyArray(1, 2)
    r20.next shouldBe NonEmptyArray(4, 5)
    r20.hasNext shouldBe false

    val r21 = NonEmptyArray(1, 2, 3, 4, 5).sliding(2, 4)
    r21.next shouldBe NonEmptyArray(1, 2)
    r21.next shouldBe NonEmptyArray(5)
    r21.hasNext shouldBe false

    val r22 = NonEmptyArray(1, 2, 3, 4, 5).sliding(3, 1)
    r22.next shouldBe NonEmptyArray(1, 2, 3)
    r22.next shouldBe NonEmptyArray(2, 3, 4)
    r22.next shouldBe NonEmptyArray(3, 4, 5)
    r22.hasNext shouldBe false

    val r23 = NonEmptyArray(1, 2, 3, 4, 5).sliding(3, 2)
    r23.next shouldBe NonEmptyArray(1, 2, 3)
    r23.next shouldBe NonEmptyArray(3, 4, 5)
    r23.hasNext shouldBe false

    val r24 = NonEmptyArray(1, 2, 3, 4, 5).sliding(3, 3)
    r24.next shouldBe NonEmptyArray(1, 2, 3)
    r24.next shouldBe NonEmptyArray(4, 5)
    r24.hasNext shouldBe false

    val r25 = NonEmptyArray(1, 2, 3, 4, 5).sliding(3, 4)
    r25.next shouldBe NonEmptyArray(1, 2, 3)
    r25.next shouldBe NonEmptyArray(5)
    r25.hasNext shouldBe false
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
    NonEmptyArray("five", "four", "three", "two", "one").sortBy(regFun) shouldBe NonEmptyArray("one", "two", "three", "four", "five")
    NonEmptyArray("two", "one", "four", "five", "three").sortBy(regFun) shouldBe NonEmptyArray("one", "two", "three", "four", "five")
    NonEmptyArray("two", "-one", "four", "-five", "-three").sortBy(regFun) shouldBe NonEmptyArray("-five", "-three", "-one", "two", "four")
    NonEmptyArray("two", "-one", "four", "-five", "-three").sortBy(absFun) shouldBe NonEmptyArray("-one", "two", "-three", "four", "-five")
  }
  it should "have a sortWith method" in {
    NonEmptyArray(1, 2, 3, 4, 5).sortWith(_ > _) shouldBe NonEmptyArray(5, 4, 3, 2, 1)
    NonEmptyArray(2, 1, 4, 5, 3).sortWith(_ > _) shouldBe NonEmptyArray(5, 4, 3, 2, 1)
    NonEmptyArray(2, -1, 4, -5, -3).sortWith(_.abs > _.abs) shouldBe NonEmptyArray(-5, 4, -3, 2, -1)
    NonEmptyArray(2, -1, 4, -5, -3).sortWith(_.abs < _.abs) shouldBe NonEmptyArray(-1, 2, -3, 4, -5)
  }
  it should "have a sorted method" in {
    NonEmptyArray(1, 2, 3, 4, 5).sorted shouldBe NonEmptyArray(1, 2, 3, 4, 5)
    NonEmptyArray(5, 4, 3, 2, 1).sorted shouldBe NonEmptyArray(1, 2, 3, 4, 5)
    NonEmptyArray(2, 1, 4, 5, 3).sorted shouldBe NonEmptyArray(1, 2, 3, 4, 5)
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
    NonEmptyArray(1, 2, 3).startsWith(Array(1)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Array(1, 2)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Array(1, 2, 3)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Array(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1).startsWith(Array(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1).startsWith(Array(1)) shouldBe true
    NonEmptyArray(1).startsWith(Array(2)) shouldBe false

    NonEmptyArray(1).startsWith(Array(1), 0) shouldBe true
    NonEmptyArray(1).startsWith(Array(1), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Array(1), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Array(1), 2) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Array(2), 2) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Array(2), 1) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Array(2, 3), 1) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Array(1, 2, 3), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Array(1, 2, 3), 0) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(Array(3, 4), 2) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(Array(3, 4, 5), 2) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(Array(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take an Every" in {
    NonEmptyArray(1, 2, 3).startsWith(Every(1)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Every(1, 2)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Every(1, 2, 3)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1).startsWith(Every(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1).startsWith(Every(1)) shouldBe true
    NonEmptyArray(1).startsWith(Every(2)) shouldBe false

    NonEmptyArray(1).startsWith(Every(1), 0) shouldBe true
    NonEmptyArray(1).startsWith(Every(1), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Every(1), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Every(1), 2) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Every(2), 2) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Every(2), 1) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Every(2, 3), 1) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(Every(1, 2, 3), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(Every(1, 2, 3), 0) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(Every(3, 4), 2) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5), 2) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(Every(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take a NonEmptyArray" in {
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1, 2)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1, 2, 3)) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1).startsWith(NonEmptyArray(1, 2, 3, 4)) shouldBe false
    NonEmptyArray(1).startsWith(NonEmptyArray(1)) shouldBe true
    NonEmptyArray(1).startsWith(NonEmptyArray(2)) shouldBe false

    NonEmptyArray(1).startsWith(NonEmptyArray(1), 0) shouldBe true
    NonEmptyArray(1).startsWith(NonEmptyArray(1), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1), 2) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(2), 2) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(2), 1) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(2, 3), 1) shouldBe true
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1, 2, 3), 1) shouldBe false
    NonEmptyArray(1, 2, 3).startsWith(NonEmptyArray(1, 2, 3), 0) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(NonEmptyArray(3, 4), 2) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(NonEmptyArray(3, 4, 5), 2) shouldBe true
    NonEmptyArray(1, 2, 3, 4, 5).startsWith(NonEmptyArray(3, 4, 5, 6), 2) shouldBe false
  }
  it should "have a stringPrefix method" in {
    NonEmptyArray(1).stringPrefix shouldBe "NonEmptyArray"
    NonEmptyArray(1, 2, 3).stringPrefix shouldBe "NonEmptyArray"
  }
  it should "have a sum method" in {
    NonEmptyArray(1).sum shouldBe 1
    NonEmptyArray(5).sum shouldBe 5
    NonEmptyArray(1, 2, 3).sum shouldBe 6
    NonEmptyArray(1, 2, 3, 4, 5).sum shouldBe 15
    NonEmptyArray(1.1, 2.2, 3.3).sum shouldBe 6.6
  }
  /*
    it should not have a tail method
      scala> Vector(1).tail
      res7: scala.collection.immutable.Vector[Int] = Vector()

    it should not have a tails method
      scala> Vector(1).tails.toArray
      res8: Array[scala.collection.immutable.Vector[Int]] = Array(Vector(1), Vector())

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
    NonEmptyArray(1).to(Array) shouldBe Array(1)
    NonEmptyArray(1, 2, 3).to(Array) shouldBe Array(1, 2, 3)
    NonEmptyArray(1, 2, 3).to(scala.collection.mutable.ArrayBuffer) shouldBe ArrayBuffer(1, 2, 3)
    NonEmptyArray(1, 2, 3).to(Vector) shouldBe Vector(1, 2, 3)
  }
  it should "have a toList method" in {
    NonEmptyArray(1, 2, 3).toList should === (List(1, 2, 3))
    NonEmptyArray("a", "b").toList should === (List("a", "b"))
    NonEmptyArray(1).toList should === (List(1))
  }
  it should "have a toBuffer method" in {
    NonEmptyArray(1, 2, 3).toBuffer should === (Buffer(1, 2, 3))
    NonEmptyArray("a", "b").toBuffer should === (Buffer("a", "b"))
    NonEmptyArray(1).toBuffer should === (Buffer(1))
  }
  it should "have a toIndexedSeq method" in {
    NonEmptyArray(1, 2, 3).toIndexedSeq should === (IndexedSeq(1, 2, 3))
    NonEmptyArray("a", "b").toIndexedSeq should === (IndexedSeq("a", "b"))
    NonEmptyArray(1).toIndexedSeq should === (IndexedSeq(1))
  }
  it should "have a toIterable method" in {
    NonEmptyArray(1, 2, 3).toIterable should === (Iterable(1, 2, 3))
    NonEmptyArray("a", "b").toIterable should === (Iterable("a", "b"))
    NonEmptyArray(1).toIterable should === (Iterable(1))
  }
  it should "have a toIterator method" in {
    NonEmptyArray(1, 2, 3).toIterator.toArray should === (Iterator(1, 2, 3).toArray)
    NonEmptyArray("a", "b").toIterator.toArray should === (Iterator("a", "b").toArray)
    NonEmptyArray(1).toIterator.toArray should === (Iterator(1).toArray)
    NonEmptyArray(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    NonEmptyArray("a", "b").toIterator shouldBe an [Iterator[_]]
    NonEmptyArray(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toArray method" in {
    NonEmptyArray(1, 2, 3).toArray should === (Array(1, 2, 3))
    NonEmptyArray("a", "b").toArray should === (Array("a", "b"))
    NonEmptyArray(1).toArray should === (Array(1))
  }
  it should "have a toMap method" in {
    NonEmptyArray("1" -> 1, "2" -> 2, "3" -> 3).toMap should === (Map("1" -> 1, "2" -> 2, "3" -> 3))
    NonEmptyArray('A' -> "a", 'B' -> "b").toMap should === (Map('A' -> "a", 'B' -> "b"))
    NonEmptyArray("1" -> 1).toMap should === (Map("1" -> 1))
  }
  it should "have a toSeq method" in {
    NonEmptyArray(1, 2, 3).toSeq should === (Seq(1, 2, 3))
    NonEmptyArray("a", "b").toSeq should === (Seq("a", "b"))
    NonEmptyArray(1).toSeq should === (Seq(1))
  }
  it should "have a toSet method" in {
    NonEmptyArray(1, 2, 3).toSet should === (Set(1, 2, 3))
    NonEmptyArray("a", "b").toSet should === (Set("a", "b"))
    NonEmptyArray(1).toSet should === (Set(1))
  }
  it should "have a toStream method" in {
    NonEmptyArray(1, 2, 3).toStream should === (Stream(1, 2, 3))
    NonEmptyArray("a", "b").toStream should === (Stream("a", "b"))
    NonEmptyArray(1).toStream should === (Stream(1))
  }
  it should "have a toString method" in {
    NonEmptyArray(1, 2, 3).toString should === ("NonEmptyArray(1, 2, 3)")
    NonEmptyArray(1, 2, 3).toString should === ("NonEmptyArray(1, 2, 3)")
    NonEmptyArray(1).toString should === ("NonEmptyArray(1)")
  }
  it should "have a toVector method" in {
    NonEmptyArray(1, 2, 3).toVector should === (Vector(1, 2, 3))
    NonEmptyArray("a", "b").toVector should === (Vector("a", "b"))
    NonEmptyArray(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    //NonEmptyArray(NonEmptyArray(1, 2, 3), NonEmptyArray(4, 5, 6), NonEmptyArray(7, 8, 9)).transpose shouldBe NonEmptyArray(NonEmptyArray(1, 4, 7), NonEmptyArray(2, 5, 8), NonEmptyArray(3, 6, 9))
    //NonEmptyArray(NonEmptyArray(1, 2), NonEmptyArray(3, 4), NonEmptyArray(5, 6), NonEmptyArray(7, 8)).transpose shouldBe NonEmptyArray(NonEmptyArray(1, 3, 5, 7), NonEmptyArray(2, 4, 6, 8))
    //NonEmptyArray(NonEmptyArray(1, 2), NonEmptyArray(3, 4), NonEmptyArray(5, 6), NonEmptyArray(7, 8)).transpose.transpose shouldBe NonEmptyArray(NonEmptyArray(1, 2), NonEmptyArray(3, 4), NonEmptyArray(5, 6), NonEmptyArray(7, 8))
    //NonEmptyArray(NonEmptyArray(1, 2, 3), NonEmptyArray(4, 5, 6), NonEmptyArray(7, 8, 9)).transpose.transpose shouldBe NonEmptyArray(NonEmptyArray(1, 2, 3), NonEmptyArray(4, 5, 6), NonEmptyArray(7, 8, 9))
  }
  it should "have an unzip method" in {
    val r1 = NonEmptyArray((1, 2)).unzip
    r1._1 shouldBe NonEmptyArray(1)
    r1._2 shouldBe NonEmptyArray(2)

    val r2 = NonEmptyArray((1, 2), (3, 4)).unzip
    r2._1 shouldBe NonEmptyArray(1, 3)
    r2._2 shouldBe NonEmptyArray(2, 4)

    val r3 = NonEmptyArray((1, 2), (3, 4), (5, 6)).unzip
    r3._1 shouldBe NonEmptyArray(1, 3, 5)
    r3._2 shouldBe NonEmptyArray(2, 4, 6)
  }
  it should "have an unzip3 method" in {
    val r1 = NonEmptyArray((1, 2, 3)).unzip3
    r1._1 shouldBe NonEmptyArray(1)
    r1._2 shouldBe NonEmptyArray(2)
    r1._3 shouldBe NonEmptyArray(3)

    val r2 = NonEmptyArray((1, 2, 3), (4, 5, 6)).unzip3
    r2._1 shouldBe NonEmptyArray(1, 4)
    r2._2 shouldBe NonEmptyArray(2, 5)
    r2._3 shouldBe NonEmptyArray(3, 6)

    val r3 = NonEmptyArray((1, 2, 3), (4, 5, 6), (7, 8, 9)).unzip3
    r3._1 shouldBe NonEmptyArray(1, 4, 7)
    r3._2 shouldBe NonEmptyArray(2, 5, 8)
    r3._3 shouldBe NonEmptyArray(3, 6, 9)
  }
  it should "have an updated method" in {
    NonEmptyArray(1).updated(0, 2) shouldBe NonEmptyArray(2)
    def willThrowIndexOutOfBoundsException(): Unit = {
      NonEmptyArray(1).updated(1, 2)
      ()
    }
    an [IndexOutOfBoundsException] should be thrownBy {
      willThrowIndexOutOfBoundsException()
    }
    NonEmptyArray(1, 1, 1).updated(1, 2) shouldBe NonEmptyArray(1, 2, 1)
    NonEmptyArray(1, 1, 1).updated(2, 2) shouldBe NonEmptyArray(1, 1, 2)
    NonEmptyArray(1, 1, 1).updated(0, 2) shouldBe NonEmptyArray(2, 1, 1)
  }
  /*
  it should not have 2 view methods, because I don't want to support views in Every
  */
  /*
    it should not have a zip method
      scala> Array(1) zip Nil
      res0: Array[(Int, Nothing)] = Array()
  */
  it should "have a zipAll method that takes an Iterable" in {

    // Empty on right
    NonEmptyArray(1).zipAll(Nil, -1, -2) shouldBe NonEmptyArray((1, -2))
    NonEmptyArray(1, 2).zipAll(Nil, -1, -2) shouldBe NonEmptyArray((1, -2), (2, -2))

    // Same length
    NonEmptyArray(1).zipAll(Array(1), -1, -2) shouldBe NonEmptyArray((1, 1))
    NonEmptyArray(1, 2).zipAll(Array(1, 2), -1, -2) shouldBe NonEmptyArray((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyArray(1).zipAll(Array(10, 20), -1, -2) shouldBe NonEmptyArray((1,10), (-1,20))
    NonEmptyArray(1, 2).zipAll(Array(10, 20, 30), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyArray(1, 2, 3).zipAll(Array(10, 20), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (3,-2))
    NonEmptyArray(1, 2, 3, 4).zipAll(Array(10, 20, 30), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    NonEmptyArray(1).zipAll(Every(1), -1, -2) shouldBe NonEmptyArray((1, 1))
    NonEmptyArray(1, 2).zipAll(Every(1, 2), -1, -2) shouldBe NonEmptyArray((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyArray(1).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptyArray((1,10), (-1,20))
    NonEmptyArray(1, 2).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyArray(1, 2, 3).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (3,-2))
    NonEmptyArray(1, 2, 3, 4).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipAll method that takes a NonEmptyArray" in {

    // Same length
    NonEmptyArray(1).zipAll(NonEmptyArray(1), -1, -2) shouldBe NonEmptyArray((1, 1))
    NonEmptyArray(1, 2).zipAll(NonEmptyArray(1, 2), -1, -2) shouldBe NonEmptyArray((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptyArray(1).zipAll(NonEmptyArray(10, 20), -1, -2) shouldBe NonEmptyArray((1,10), (-1,20))
    NonEmptyArray(1, 2).zipAll(NonEmptyArray(10, 20, 30), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptyArray(1, 2, 3).zipAll(NonEmptyArray(10, 20), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (3,-2))
    NonEmptyArray(1, 2, 3, 4).zipAll(NonEmptyArray(10, 20, 30), -1, -2) shouldBe NonEmptyArray((1,10), (2,20), (3,30), (4,-2))
  }
  it should "have a zipWithIndex method" in {
    NonEmptyArray(99).zipWithIndex shouldBe NonEmptyArray((99,0))
    NonEmptyArray(1, 2, 3, 4, 5).zipWithIndex shouldBe NonEmptyArray((1,0), (2,1), (3,2), (4,3), (5,4))
  }
  "End" should "have a pretty toString" in {
    End.toString shouldBe "End"
  }
}

