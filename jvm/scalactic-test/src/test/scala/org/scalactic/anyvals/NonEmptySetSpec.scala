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

class NonEmptySetSpec extends UnitSpec {
  "A NonEmptySet" can "be constructed with one element" in {
    val onesie = NonEmptySet(3)
    onesie.size shouldBe 1
    onesie(3) shouldBe true
    onesie(2) shouldBe false
  }
  it can "be constructed with many elements" in {
    val twosie = NonEmptySet(2, 3)
    twosie.size shouldBe 2
    twosie(2) shouldBe true
    twosie(3) shouldBe true
    val threesie = NonEmptySet(1, 2, 3)
    threesie.size shouldBe 3
    threesie(1) shouldBe true
    threesie(2) shouldBe true
    threesie(3) shouldBe true
    threesie(4) shouldBe false
  }
  it can "be constructed from a Iterable via the from method on NonEmptySet singleton" in {
    NonEmptySet.from(Set.empty[String]) shouldBe None
    NonEmptySet.from(Set("1")) shouldBe Some(NonEmptySet("1"))
    NonEmptySet.from(Set(1, 2, 3)) shouldBe Some(NonEmptySet(1, 2, 3))
    // SKIP-SCALATESTJS,NATIVE-START
    NonEmptySet.from(Set.empty[String].par) shouldBe None
    NonEmptySet.from(Set("1").par) shouldBe Some(NonEmptySet("1"))
    NonEmptySet.from(Set(1, 2, 3).par) shouldBe Some(NonEmptySet(1, 2, 3))
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it can "be constructed with null elements" in {
    noException should be thrownBy NonEmptySet("hi", null, "ho")
    noException should be thrownBy NonEmptySet(null)
    noException should be thrownBy NonEmptySet("ho", null)
  }
  it can "be deconstructed with NonEmptySet" in {
    NonEmptySet(1) match {
      case NonEmptySet(x) => x shouldEqual 1
      case _ => fail()
    }
    NonEmptySet("hi") match {
      case NonEmptySet(s) => s shouldEqual "hi"
      case _ => fail()
    }
  }
  it can "be deconstructed with Many" in {
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, y, z) => (x, y, z) shouldEqual (2, 3, 1)
      case _ => fail()
    }
    NonEmptySet("hi", "there") match {
      case NonEmptySet(s, t) => (s, t) shouldEqual ("there", "hi")
      case _ => fail()
    }
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, y, _) => (x, y) shouldEqual (2, 3)
      case _ => fail()
    }
    NonEmptySet(1, 2, 3, 4, 5) match {
      case NonEmptySet(x, y, _*) => (x, y) shouldEqual (5, 1)
      case _ => fail()
    }
  }
  it can "be deconstructed with Every" in {
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, y, z) => (x, y, z) shouldEqual (2, 3, 1)
      case _ => fail()
    }
    NonEmptySet("hi", "there") match {
      case NonEmptySet(s, t) => (s, t) shouldEqual ("there", "hi")
      case _ => fail()
    }
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, y, _) => (x, y) shouldEqual (2, 3)
      case _ => fail()
    }
    NonEmptySet(1, 2, 3, 4, 5) match {
      case NonEmptySet(x, y, _*) => (x, y) shouldEqual (5, 1)
      case _ => fail()
    }
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, _*) => x shouldEqual 2
      case _ => fail()
    }
    NonEmptySet("hi") match {
      case NonEmptySet(s) => s shouldEqual "hi"
      case _ => fail()
    }
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, y, z) => (x, y, z) shouldEqual (2, 3, 1)
      case _ => fail()
    }
    NonEmptySet("hi", "there") match {
      case NonEmptySet(s, t) => (s, t) shouldEqual ("there", "hi")
      case _ => fail()
    }
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, y, _) => (x, y) shouldEqual (2, 3)
      case _ => fail()
    }
    NonEmptySet(1, 2, 3, 4, 5) match {
      case NonEmptySet(x, y, _*) => (x, y) shouldEqual (5, 1)
      case _ => fail()
    }
    NonEmptySet(1, 2, 3) match {
      case NonEmptySet(x, _*) => x shouldEqual 2
      case _ => fail()
    }
  }
  it should "have a ++ method that takes another NonEmptySet" in {
    NonEmptySet(1, 2, 3) ++ NonEmptySet(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ NonEmptySet(4, 5) shouldEqual NonEmptySet(1, 2, 3, 4, 5)
    NonEmptySet(1, 2, 3) ++ NonEmptySet(4, 5, 6) shouldEqual NonEmptySet(1, 2, 3, 4, 5, 6)
  }
  it should "have a ++ method that takes an Every" in {
    NonEmptySet(1, 2, 3) ++ One(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ Every(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ Every(4, 5, 6) shouldEqual NonEmptySet(1, 2, 3, 4, 5, 6)
    NonEmptySet(1, 2, 3) ++ One(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ One(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ Every(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ Every(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ One(4) shouldEqual NonEmptySet(1, 2, 3, 4)
  }
  it should "have a ++ method that takes a IterableOnce" in {
    NonEmptySet(1, 2, 3) ++ Set(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ Vector(4, 5, 6) shouldEqual NonEmptySet(1, 2, 3, 4, 5, 6)
    NonEmptySet(1, 2, 3) ++ Iterable(4) shouldEqual NonEmptySet(1, 2, 3, 4)
    NonEmptySet(1, 2, 3) ++ Set(4, 5) shouldEqual NonEmptySet(1, 2, 3, 4, 5)
    NonEmptySet(1, 2, 3) ++ Set(4, 5).iterator shouldEqual NonEmptySet(1, 2, 3, 4, 5)
  }
  it should "have a + method" in {
    NonEmptySet(1) + 0 shouldBe NonEmptySet(0, 1)
    NonEmptySet(1, 2) + 0 shouldBe NonEmptySet(0, 1, 2)
    NonEmptySet("one", "two") + "zero" shouldBe NonEmptySet("zero", "one", "two")
  }
  it should "have 3 addString methods" in {
    NonEmptySet("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    NonEmptySet(1, 2, 3).addString(new StringBuilder) shouldBe new StringBuilder("231")

    NonEmptySet("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("hi")
    NonEmptySet(1, 2, 3).addString(new StringBuilder, "#") shouldBe new StringBuilder("2#3#1")
    NonEmptySet(1, 2, 3).addString(new StringBuilder, ", ") shouldBe new StringBuilder("2, 3, 1")

    NonEmptySet("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<hi>")
    NonEmptySet(1, 2, 3).addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<2#3#1>")
    NonEmptySet(1, 2, 3).addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 2, 3, 1 ) ")
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
    NonEmptySet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) collectFirst { case i if i > 10 => i / 2 } shouldBe None
    NonEmptySet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) collectFirst { case i if i > 10 => i / 2 } shouldBe Some(6)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  it should "have a contains method" in {
    val e = NonEmptySet(1, 2, 3)
    e.contains(-1) shouldBe false
    e.contains(0) shouldBe false
    e.contains(1) shouldBe true
    e.contains(2) shouldBe true
    e.contains(3) shouldBe true
    e.contains(4) shouldBe false
    val es = NonEmptySet("one", "two", "three")
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

    val arr1 = Array.fill(5)(-1)
    NonEmptySet(1, 2, 3, 4, 5).copyToArray(arr1)
    arr1 shouldEqual Array(5, 1, 2, 3, 4)

    val arr2 = Array.fill(5)(-1)
    NonEmptySet(1, 2, 3, 4, 5).copyToArray(arr2, 1)
    arr2 shouldEqual Array(-1, 5, 1, 2, 3)

    val arr3 = Array.fill(5)(-1)
    NonEmptySet(1, 2, 3, 4, 5).copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array(-1, 5, 1, -1, -1)
  }
  it should "have a copyToBuffer method" in {

    val buf = ListBuffer.fill(3)(-1)
    NonEmptySet(1, 2, 3, 4, 5).copyToBuffer(buf)
    buf shouldEqual Buffer(-1, -1, -1, 5, 1, 2, 3, 4)
  }
  it should "have a count method" in {
    val nonEmptySet = NonEmptySet(1, 2, 3, 4, 5)
    nonEmptySet.count(_ > 10) shouldBe 0
    nonEmptySet.count(_ % 2 == 0) shouldBe 2
    nonEmptySet.count(_ % 2 == 1) shouldBe 3
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
    NonEmptySet(1) shouldEqual NonEmptySet(1)
    NonEmptySet(1) should not equal NonEmptySet(2)
    NonEmptySet(1, 2) should not equal NonEmptySet(2, 3)
  }
  it should "have an exists method" in {
    NonEmptySet(1, 2, 3).exists(_ == 2) shouldBe true
    NonEmptySet(1, 2, 3).exists(_ == 5) shouldBe false
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
    NonEmptySet(1, 2, 3).find(_ == 5) shouldBe None
    NonEmptySet(1, 2, 3).find(_ == 2) shouldBe Some(2)
  }
  it should "have a flatMap method" in {
    NonEmptySet(1, 2, 3) flatMap (i => NonEmptySet(i + 1)) shouldBe NonEmptySet(2, 3, 4)
    val ss = NonEmptySet("hi", "ho")
    val is = NonEmptySet(1, 2, 3)
    (for (s <- ss; i <- is) yield (s, i)) shouldBe
      NonEmptySet(
        ("hi",1), ("hi",2), ("hi",3), ("ho",1), ("ho",2), ("ho",3)
      )
    NonEmptySet(5) flatMap (i => NonEmptySet(i + 3)) shouldBe NonEmptySet(8)
    NonEmptySet(8) flatMap (i => NonEmptySet(i.toString)) shouldBe NonEmptySet("8")
  }
  /*
    Can only flatten NonEmptySets
    scala> Vector(Set.empty[Int], Set.empty[Int]).flatten
    res17: scala.collection.immutable.Vector[Int] = Vector()
  */
  // TODO: Actually it would make sense to flatten Everys too
  it should "have a flatten method that works on nested NonEmptySets" in {
    NonEmptySet(NonEmptySet(1, 2, 3), NonEmptySet(1, 2, 3)).flatten shouldBe NonEmptySet(1, 2, 3, 1, 2, 3)
    NonEmptySet(NonEmptySet(1)).flatten shouldBe NonEmptySet(1)
  }
  it can "be flattened when in a IterableOnce" in {
    Vector(NonEmptySet(1, 2, 3), NonEmptySet(1, 2, 3)).flatten shouldBe Vector(2, 3, 1, 2, 3, 1)
    Set(NonEmptySet(1, 2, 3), NonEmptySet(1, 2, 3)).flatten shouldBe Set(1, 2, 3, 1, 2, 3)
    Set(NonEmptySet(1, 2, 3), NonEmptySet(1, 2, 3)).toIterator.flatten.toStream shouldBe Set(2, 3, 1, 2, 3, 1).toIterator.toStream
    // SKIP-SCALATESTJS,NATIVE-START
    Set(NonEmptySet(1, 2, 3), NonEmptySet(1, 2, 3)).par.flatten shouldBe Set(1, 2, 3, 1, 2, 3).par
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it should "have a fold method" in {
    NonEmptySet(1).fold(0)(_ + _) shouldBe 1
    NonEmptySet(1).fold(1)(_ * _) shouldBe 1
    NonEmptySet(2).fold(0)(_ + _) shouldBe 2
    NonEmptySet(2).fold(1)(_ * _) shouldBe 2
    NonEmptySet(3).fold(0)(_ + _) shouldBe 3
    NonEmptySet(3).fold(1)(_ * _) shouldBe 3
    NonEmptySet(1, 2, 3).fold(0)(_ + _) shouldBe 6
    NonEmptySet(1, 2, 3).fold(1)(_ * _) shouldBe 6
    NonEmptySet(1, 2, 3, 4, 5).fold(0)(_ + _) shouldBe 15
    NonEmptySet(1, 2, 3, 4, 5).fold(1)(_ * _) shouldBe 120
  }
  it should "have a foldLeft method" in {
    NonEmptySet(1).foldLeft(0)(_ + _) shouldBe 1
    NonEmptySet(1).foldLeft(1)(_ + _) shouldBe 2
    NonEmptySet(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    NonEmptySet(1, 2, 3).foldLeft(1)(_ + _) shouldBe 7
  }
  it should "have a foldRight method" in {
    NonEmptySet(1).foldRight(0)(_ + _) shouldBe 1
    NonEmptySet(1).foldRight(1)(_ + _) shouldBe 2
    NonEmptySet(1, 2, 3).foldRight(0)(_ + _) shouldBe 6
    NonEmptySet(1, 2, 3).foldRight(1)(_ + _) shouldBe 7
  }
  it should "have a forall method" in {
    NonEmptySet(1, 2, 3, 4, 5).forall(_ > 0) shouldBe true
    NonEmptySet(1, 2, 3, 4, 5).forall(_ < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    NonEmptySet(1, 2, 3) foreach (num += _)
    num shouldBe 6
    for (i <- NonEmptySet(1, 2, 3))
      num += i
    num shouldBe 12
    NonEmptySet(5) foreach (num *= _)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    NonEmptySet(1, 2, 3, 4, 5).groupBy(_ % 2) shouldBe Map(1 -> NonEmptySet(1, 3, 5), 0 -> NonEmptySet(2, 4))
    NonEmptySet(1, 2, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> NonEmptySet(1, 3, 3, 3), 0 -> NonEmptySet(2))
    NonEmptySet(1, 1, 3, 3, 3).groupBy(_ % 2) shouldBe Map(1 -> NonEmptySet(1, 1, 3, 3, 3))
    NonEmptySet(1, 2, 3, 5, 7).groupBy(_ % 2) shouldBe Map(1 -> NonEmptySet(1, 3, 5, 7), 0 -> NonEmptySet(2))
  }
  it should "have a grouped method" in {
    NonEmptySet(1, 2, 3).grouped(2).toSet shouldBe Set(NonEmptySet(2, 3), NonEmptySet(1))
    NonEmptySet(1, 2, 3).grouped(1).toSet shouldBe Set(NonEmptySet(2), NonEmptySet(3), NonEmptySet(1))
    an [IllegalArgumentException] should be thrownBy { NonEmptySet(1, 2, 3).grouped(0).toSet }
    NonEmptySet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(2).toSet shouldBe Set(NonEmptySet(1, 6), NonEmptySet(7, 3), NonEmptySet(8, 4), NonEmptySet(9, 2), NonEmptySet(5, 10))
    NonEmptySet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(3).toSet shouldBe Set(NonEmptySet(5, 10, 1), NonEmptySet(6, 9, 2), NonEmptySet(7, 3, 8), NonEmptySet(4))
    NonEmptySet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).grouped(4).toSet shouldBe Set(NonEmptySet(5, 10, 1, 6), NonEmptySet(9, 2, 7, 3), NonEmptySet(8, 4))
    NonEmptySet(1).grouped(2).toSet shouldBe Set(NonEmptySet(1))
    NonEmptySet(1).grouped(1).toSet shouldBe Set(NonEmptySet(1))
  }
  it should "have a hasDefiniteSize method" in {
    NonEmptySet(1).hasDefiniteSize shouldBe true
    NonEmptySet(1, 2).hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    NonEmptySet(1).hashCode shouldEqual NonEmptySet(1).hashCode
    NonEmptySet(1, 2).hashCode shouldEqual NonEmptySet(1, 2).hashCode
  }
  it should "have a head method" in {
    NonEmptySet("hi").head shouldBe "hi"
    NonEmptySet(1, 2, 3).head shouldBe 2
  }
  it should "have a headOption method" in {
    NonEmptySet("hi").headOption shouldBe Some("hi")
    NonEmptySet(1, 2, 3).headOption shouldBe Some(2)
  }
  /*
  it should not have an init method
    scala> Vector(1).init
    res30: scala.collection.immutable.Vector[Int] = Vector()

  it should "have an inits method" is pending
    scala> Vector(1).inits.toSet
    res32: Set[scala.collection.immutable.Vector[Int]] = Set(Vector(1), Vector())

  it should "have an intersect method" is pending
    scala> Vector(1, 2, 3) intersect Vector(4, 5)
    res33: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an isEmpty method" in {
    NonEmptySet("hi").isEmpty shouldBe false
    NonEmptySet(1, 2, 3).isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    NonEmptySet("hi").isTraversableAgain shouldBe true
    NonEmptySet(1, 2, 3).isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    NonEmptySet("hi").iterator.toSet shouldBe Set("hi")
    NonEmptySet(1, 2, 3).iterator.toSet shouldBe Set(1, 2, 3)
  }
  it should "have a last method" in {
    NonEmptySet("hi").last shouldBe "hi"
    NonEmptySet(1, 2, 3).last shouldBe 1
  }
  it should "have an lastOption method" in {
    NonEmptySet("hi").lastOption shouldBe Some("hi")
    NonEmptySet(1, 2, 3).lastOption shouldBe Some(1)
  }
  it should "have a map method" in {
    NonEmptySet(1, 2, 3) map (_ + 1) shouldBe NonEmptySet(2, 3, 4)
    (for (ele <- NonEmptySet(1, 2, 3)) yield ele * 2) shouldBe NonEmptySet(2, 4, 6)
    NonEmptySet(5) map (_ + 3) shouldBe NonEmptySet(8)
    NonEmptySet(8) map (_.toString) shouldBe NonEmptySet("8")
  }
  it should "have a max method" in {
    NonEmptySet(1, 2, 3, 4, 5).max shouldBe 5
    NonEmptySet(1).max shouldBe 1
    NonEmptySet(-1).max shouldBe -1
    NonEmptySet("aaa", "ccc", "bbb").max shouldBe "ccc"
  }
  it should "have a maxBy method" in {
    NonEmptySet(1, 2, 3, 4, 5).maxBy(_.abs) shouldBe 5
    NonEmptySet(1, 2, 3, 4, -5).maxBy(_.abs) shouldBe -5
  }
  it should "have a min method" in {
    NonEmptySet(1, 2, 3, 4, 5).min shouldBe 1
    NonEmptySet(1).min shouldBe 1
    NonEmptySet(-1).min shouldBe -1
    NonEmptySet("aaa", "ccc", "bbb").min shouldBe "aaa"
  }
  it should "have a minBy method" in {
    NonEmptySet(1, 2, 3, 4, 5).minBy(_.abs) shouldBe 1
    NonEmptySet(-1, -2, 3, 4, 5).minBy(_.abs) shouldBe -1
  }
  it should "have a mkString method" in {
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    NonEmptySet("hi").mkString shouldBe "hi"
    NonEmptySet(1, 2, 3).mkString shouldBe "231"
    // SKIP-DOTTY-END

    NonEmptySet("hi").mkString("#") shouldBe "hi"
    NonEmptySet(1, 2, 3).mkString("#") shouldBe "2#3#1"
    NonEmptySet(1, 2, 3).mkString(", ") shouldBe "2, 3, 1"

    NonEmptySet("hi").mkString("<", "#", ">") shouldBe "<hi>"
    NonEmptySet(1, 2, 3).mkString("<", "#", ">") shouldBe "<2#3#1>"
    NonEmptySet(1, 2, 3).mkString(" ( ", ", ", " ) ") shouldBe " ( 2, 3, 1 ) "
  }
  it should "have an nonEmpty method" in {
    NonEmptySet("hi").nonEmpty shouldBe true
    NonEmptySet(1, 2, 3).nonEmpty shouldBe true
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: nonEmptySet.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a product method" in {
    NonEmptySet(1, 2, 3).product shouldBe 6
    NonEmptySet(3).product shouldBe 3
    NonEmptySet(3, 4, 5).product shouldBe 60
    NonEmptySet(3, 4, 5).product shouldBe 60
    NonEmptySet(3.1, 4.2, 5.3).product shouldBe 69.006
  }
  it should "have a reduce method" in {
    NonEmptySet(1, 2, 3, 4, 5).reduce(_ + _) shouldBe 15
    NonEmptySet(1, 2, 3, 4, 5).reduce(_ * _) shouldBe 120
    NonEmptySet(5).reduce(_ + _) shouldBe 5
    NonEmptySet(5).reduce(_ * _) shouldBe 5
  }
  it should "have a reduceLeft method" in {
    NonEmptySet(1).reduceLeft(_ + _) shouldBe 1
    NonEmptySet(1).reduceLeft(_ * _) shouldBe 1
    NonEmptySet(1, 2, 3).reduceLeft(_ + _) shouldBe 6
    NonEmptySet(1, 2, 3).reduceLeft(_ * _) shouldBe 6
    NonEmptySet(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldBe 120
  }
  it should "have a reduceLeftOption method" in {
    NonEmptySet(1).reduceLeftOption(_ + _) shouldBe Some(1)
    NonEmptySet(1).reduceLeftOption(_ * _) shouldBe Some(1)
    NonEmptySet(1, 2, 3).reduceLeftOption(_ + _) shouldBe Some(6)
    NonEmptySet(1, 2, 3).reduceLeftOption(_ * _) shouldBe Some(6)
    NonEmptySet(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldBe Some(120)
  }
  it should "have a reduceOption method" in {
    NonEmptySet(1, 2, 3, 4, 5).reduceOption(_ + _) shouldBe Some(15)
    NonEmptySet(1, 2, 3, 4, 5).reduceOption(_ * _) shouldBe Some(120)
    NonEmptySet(5).reduceOption(_ + _) shouldBe Some(5)
    NonEmptySet(5).reduceOption(_ * _) shouldBe Some(5)
  }
  it should "have a reduceRight method" in { One(1).reduceRight(_ + _) shouldBe 1
    NonEmptySet(1).reduceRight(_ * _) shouldBe 1
    NonEmptySet(1, 2, 3).reduceRight(_ + _) shouldBe 6
    NonEmptySet(1, 2, 3).reduceRight(_ * _) shouldBe 6
    NonEmptySet(1, 2, 3, 4, 5).reduceRight(_ * _) shouldBe 120
  }
  it should "have a reduceRightOption method" in {
    NonEmptySet(1).reduceRightOption(_ + _) shouldBe Some(1)
    NonEmptySet(1).reduceRightOption(_ * _) shouldBe Some(1)
    NonEmptySet(1, 2, 3).reduceRightOption(_ + _) shouldBe Some(6)
    NonEmptySet(1, 2, 3).reduceRightOption(_ * _) shouldBe Some(6)
    NonEmptySet(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldBe Some(120)
  }
  it should "have a sameElements method that takes a GenIterable" in {
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Set(1, 2, 3, 4, 5)) shouldBe true
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Set(1, 2, 3, 4)) shouldBe false
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Set(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Set(1, 2, 3, 4, 4)) shouldBe false
    NonEmptySet(3).sameElements(Set(1, 2, 3, 4, 5)) shouldBe false
    NonEmptySet(3).sameElements(Set(1)) shouldBe false
    NonEmptySet(3).sameElements(Set(3)) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Every(5, 1, 2, 3, 4)) shouldBe true
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4)) shouldBe false
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptySet(1, 2, 3, 4, 5).sameElements(Every(1, 2, 3, 4, 4)) shouldBe false
    NonEmptySet(3).sameElements(Every(1, 2, 3, 4, 5)) shouldBe false
    NonEmptySet(3).sameElements(Every(1)) shouldBe false
    NonEmptySet(3).sameElements(Every(3)) shouldBe true
  }
  it should "have a sameElements method that takes a NonEmptySet" in {
    NonEmptySet(1, 2, 3, 4, 5).sameElements(NonEmptySet(1, 2, 3, 4, 5)) shouldBe true
    NonEmptySet(1, 2, 3, 4, 5).sameElements(NonEmptySet(1, 2, 3, 4)) shouldBe false
    NonEmptySet(1, 2, 3, 4, 5).sameElements(NonEmptySet(1, 2, 3, 4, 5, 6)) shouldBe false
    NonEmptySet(1, 2, 3, 4, 5).sameElements(NonEmptySet(1, 2, 3, 4, 4)) shouldBe false
    NonEmptySet(3).sameElements(NonEmptySet(1, 2, 3, 4, 5)) shouldBe false
    NonEmptySet(3).sameElements(NonEmptySet(1)) shouldBe false
    NonEmptySet(3).sameElements(NonEmptySet(3)) shouldBe true
  }
  it should "have a scan method" in {
    NonEmptySet(1).scan(0)(_ + _) shouldBe NonEmptySet(0, 1)
    NonEmptySet(1, 2, 3).scan(0)(_ + _) shouldBe NonEmptySet(0, 2, 5, 6)
    NonEmptySet(1, 2, 3).scan("z")(_.toString + _.toString) shouldBe NonEmptySet("z", "z2", "z23", "z231")
    NonEmptySet(0).scan("z")(_.toString + _.toString) shouldBe NonEmptySet("z", "z0")
  }
  it should "have a scanLeft method" in {
    NonEmptySet(1).scanLeft(0)(_ + _) shouldBe NonEmptySet(0, 1)
    NonEmptySet(1, 2, 3).scanLeft(0)(_ + _) shouldBe NonEmptySet(0, 2, 5, 6)
    NonEmptySet(1, 2, 3).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptySet("z", "z2", "z23", "z231")
    NonEmptySet(0).scanLeft("z")(_.toString + _.toString) shouldBe NonEmptySet("z", "z0")
  }
  it should "have a scanRight method" in {
    NonEmptySet(1).scanRight(0)(_ + _) shouldBe NonEmptySet(1, 0)
    NonEmptySet(1, 2, 3).scanRight(0)(_ + _) shouldBe NonEmptySet(6, 4, 1, 0)
    NonEmptySet(1, 2, 3).scanRight("z")(_.toString + _.toString) shouldBe NonEmptySet("231z", "31z", "1z", "z")
    NonEmptySet(0).scanRight("z")(_.toString + _.toString) shouldBe NonEmptySet("0z", "z")
  }
  // it should "have a seq method" is pending
  it should "have a size method" in {
    NonEmptySet(5).size shouldBe 1
    NonEmptySet(1, 2, 3).size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    NonEmptySet(1).sliding(1).toSet shouldBe Set(NonEmptySet(1))
    NonEmptySet(1).sliding(2).toSet shouldBe Set(NonEmptySet(1))
    NonEmptySet(1, 2, 3).sliding(2).toSet shouldBe Set(NonEmptySet(2, 3), NonEmptySet(3, 1))
    NonEmptySet(1, 2, 3).sliding(1).toSet shouldBe Set(NonEmptySet(2), NonEmptySet(3), NonEmptySet(1))
    NonEmptySet(1, 2, 3).sliding(3).toSet shouldBe Set(NonEmptySet(2, 3, 1))
    NonEmptySet(1, 2, 3, 4, 5).sliding(3).toSet shouldBe Set(NonEmptySet(5, 1, 2), NonEmptySet(1, 2, 3), NonEmptySet(2, 3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(2).toSet shouldBe Set(NonEmptySet(5, 1), NonEmptySet(1, 2), NonEmptySet(2, 3), NonEmptySet(3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(1).toSet shouldBe Set(NonEmptySet(1), NonEmptySet(2), NonEmptySet(3), NonEmptySet(4), NonEmptySet(5))
    NonEmptySet(1, 2, 3, 4, 5).sliding(4).toSet shouldBe Set(NonEmptySet(5, 1, 2, 3), NonEmptySet(1, 2, 3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(5).toSet shouldBe Set(NonEmptySet(1, 2, 3, 4, 5))

    NonEmptySet(1).sliding(1, 1).toSet shouldBe Set(NonEmptySet(1))
    NonEmptySet(1).sliding(1, 2).toSet shouldBe Set(NonEmptySet(1))
    NonEmptySet(1, 2, 3).sliding(1, 1).toSet shouldBe Set(NonEmptySet(1), NonEmptySet(2), NonEmptySet(3))
    NonEmptySet(1, 2, 3).sliding(2, 1).toSet shouldBe Set(NonEmptySet(2, 3), NonEmptySet(3, 1))
    NonEmptySet(1, 2, 3).sliding(2, 2).toSet shouldBe Set(NonEmptySet(2, 3), NonEmptySet(1))
    NonEmptySet(1, 2, 3).sliding(3, 2).toSet shouldBe Set(NonEmptySet(1, 2, 3))
    NonEmptySet(1, 2, 3).sliding(3, 1).toSet shouldBe Set(NonEmptySet(1, 2, 3))
    NonEmptySet(1, 2, 3, 4, 5).sliding(3, 1).toSet shouldBe Set(NonEmptySet(5, 1, 2), NonEmptySet(1, 2, 3), NonEmptySet(2, 3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(2, 2).toSet shouldBe Set(NonEmptySet(5, 1), NonEmptySet(2, 3), NonEmptySet(4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(2, 3).toSet shouldBe Set(NonEmptySet(5, 1), NonEmptySet(3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(2, 4).toSet shouldBe Set(NonEmptySet(5, 1), NonEmptySet(4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(3, 1).toSet shouldBe Set(NonEmptySet(5, 1, 2), NonEmptySet(1, 2, 3), NonEmptySet(2, 3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(3, 2).toSet shouldBe Set(NonEmptySet(5, 1, 2), NonEmptySet(2, 3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(3, 3).toSet shouldBe Set(NonEmptySet(5, 1, 2), NonEmptySet(3, 4))
    NonEmptySet(1, 2, 3, 4, 5).sliding(3, 4).toSet shouldBe Set(NonEmptySet(5, 1, 2), NonEmptySet(4))
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
    NonEmptySet(1).stringPrefix shouldBe "NonEmptySet"
    NonEmptySet(1, 2, 3).stringPrefix shouldBe "NonEmptySet"
  }
  it should "have a sum method" in {
    NonEmptySet(1).sum shouldBe 1
    NonEmptySet(5).sum shouldBe 5
    NonEmptySet(1, 2, 3).sum shouldBe 6
    NonEmptySet(1, 2, 3, 4, 5).sum shouldBe 15
    NonEmptySet(1.1, 2.2, 3.3).sum shouldBe 6.6
  }
  /*
    it should not have a tail method
      scala> Vector(1).tail
      res7: scala.collection.immutable.Vector[Int] = Vector()

    it should not have a tails method
      scala> Vector(1).tails.toSet
      res8: Set[scala.collection.immutable.Vector[Int]] = Set(Vector(1), Vector())

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
    NonEmptySet(1).to(Set) shouldBe Set(1)
    NonEmptySet(1, 2, 3).to(Set) shouldBe Set(1, 2, 3)
    NonEmptySet(1, 2, 3).to(scala.collection.mutable.ListBuffer) shouldBe ListBuffer(2, 3, 1)
    NonEmptySet(1, 2, 3).to(Vector) shouldBe Vector(2, 3, 1)
  }
  it should "have a toArray method" in {
    NonEmptySet(1, 2, 3).toArray should === (Array(2, 3, 1))
    NonEmptySet("a", "b").toArray should === (Array("b", "a"))
    NonEmptySet(1).toArray should === (Array(1))
  }
  it should "have a toBuffer method" in {
    NonEmptySet(1, 2, 3).toBuffer should === (Buffer(2, 3, 1))
    NonEmptySet("a", "b").toBuffer should === (Buffer("b", "a"))
    NonEmptySet(1).toBuffer should === (Buffer(1))
  }
  it should "have a toIndexedSeq method" in {
    NonEmptySet(1, 2, 3).toIndexedSeq should === (IndexedSeq(2, 3, 1))
    NonEmptySet("a", "b").toIndexedSeq should === (IndexedSeq("b", "a"))
    NonEmptySet(1).toIndexedSeq should === (IndexedSeq(1))
  }
  it should "have a toIterable method" in {
    NonEmptySet(1, 2, 3).toIterable should === (Set(2, 3, 1))
    NonEmptySet("a", "b").toIterable should === (Set("b", "a"))
    NonEmptySet(1).toIterable should === (Set(1))
  }
  it should "have a toIterator method" in {
    NonEmptySet(1, 2, 3).toIterator.toSet should === (Iterator(2, 3, 1).toSet)
    NonEmptySet("a", "b").toIterator.toSet should === (Iterator("b", "a").toSet)
    NonEmptySet(1).toIterator.toSet should === (Iterator(1).toSet)
    NonEmptySet(1, 2, 3).toIterator shouldBe an [Iterator[_]]
    NonEmptySet("a", "b").toIterator shouldBe an [Iterator[_]]
    NonEmptySet(1).toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toList method" in {
    NonEmptySet(1, 2, 3).toList should === (List(2, 3, 1))
    NonEmptySet("a", "b").toList should === (List("b", "a"))
    NonEmptySet(1).toList should === (List(1))
  }
  it should "have a toMap method" in {
    NonEmptySet("1" -> 1, "2" -> 2, "3" -> 3).toMap should === (Map("1" -> 1, "2" -> 2, "3" -> 3))
    NonEmptySet('A' -> "a", 'B' -> "b").toMap should === (Map('A' -> "a", 'B' -> "b"))
    NonEmptySet("1" -> 1).toMap should === (Map("1" -> 1))
  }
  it should "have a toSeq method" in {
    NonEmptySet(1, 2, 3).toSeq should === (Seq(2, 3, 1))
    NonEmptySet("a", "b").toSeq should === (Seq("b", "a"))
    NonEmptySet(1).toSeq should === (Seq(1))
  }
  it should "have a toSet method" in {
    NonEmptySet(1, 2, 3).toSet should === (Set(2, 3, 1))
    NonEmptySet("a", "b").toSet should === (Set("b", "a"))
    NonEmptySet(1).toSet should === (Set(1))
  }
  it should "have a toStream method" in {
    NonEmptySet(1, 2, 3).toStream should === (Stream(2, 3, 1))
    NonEmptySet("a", "b").toStream should === (Stream("b", "a"))
    NonEmptySet(1).toStream should === (Stream(1))
  }
  it should "have a toString method" in {
    NonEmptySet(1, 2, 3).toString should === ("NonEmptySet(2, 3, 1)")
    NonEmptySet(1, 2, 3).toString should === ("NonEmptySet(2, 3, 1)")
    NonEmptySet(1).toString should === ("NonEmptySet(1)")
  }
  it should "have a toVector method" in {
    NonEmptySet(1, 2, 3).toVector should === (Vector(2, 3, 1))
    NonEmptySet("a", "b").toVector should === (Vector("b", "a"))
    NonEmptySet(1).toVector should === (Vector(1))
  }
  it should "have a transpose method" in {
    NonEmptySet(NonEmptySet(1, 2, 3), NonEmptySet(4, 5, 6), NonEmptySet(7, 8, 9)).transpose shouldBe NonEmptySet(NonEmptySet(1, 4, 7), NonEmptySet(2, 5, 8), NonEmptySet(3, 6, 9))
    NonEmptySet(NonEmptySet(1, 2), NonEmptySet(3, 4), NonEmptySet(5, 6), NonEmptySet(7, 8)).transpose shouldBe NonEmptySet(NonEmptySet(1, 3, 5, 7), NonEmptySet(2, 4, 6, 8))
    NonEmptySet(NonEmptySet(1, 2), NonEmptySet(3, 4), NonEmptySet(5, 6), NonEmptySet(7, 8)).transpose.transpose shouldBe NonEmptySet(NonEmptySet(1, 2), NonEmptySet(3, 4), NonEmptySet(5, 6), NonEmptySet(7, 8))
    NonEmptySet(NonEmptySet(1, 2, 3), NonEmptySet(4, 5, 6), NonEmptySet(7, 8, 9)).transpose.transpose shouldBe NonEmptySet(NonEmptySet(1, 2, 3), NonEmptySet(4, 5, 6), NonEmptySet(7, 8, 9))
  }
  it should "have a union method that takes a GenSeq" in {
    NonEmptySet(1) union Set(1) shouldBe NonEmptySet(1, 1)
    NonEmptySet(1) union Set(1, 2) shouldBe NonEmptySet(1, 1, 2)
    NonEmptySet(1, 2) union Set(1, 2) shouldBe NonEmptySet(1, 2, 1, 2)
    NonEmptySet(1, 2) union Set(1) shouldBe NonEmptySet(1, 2, 1)
    NonEmptySet(1, 2) union Set(3, 4, 5) shouldBe NonEmptySet(1, 2, 3, 4, 5)
    NonEmptySet(1, 2, 3) union Set(3, 4, 5) shouldBe NonEmptySet(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes an Every" in {
    NonEmptySet(1) union Every(1) shouldBe NonEmptySet(1, 1)
    NonEmptySet(1) union Every(1, 2) shouldBe NonEmptySet(1, 1, 2)
    NonEmptySet(1, 2) union Every(1, 2) shouldBe NonEmptySet(1, 2, 1, 2)
    NonEmptySet(1, 2) union Every(1) shouldBe NonEmptySet(1, 2, 1)
    NonEmptySet(1, 2) union Every(3, 4, 5) shouldBe NonEmptySet(1, 2, 3, 4, 5)
    NonEmptySet(1, 2, 3) union Every(3, 4, 5) shouldBe NonEmptySet(1, 2, 3, 3, 4, 5)
  }
  it should "have a union method that takes a NonEmptySet" in {
    NonEmptySet(1) union NonEmptySet(1) shouldBe NonEmptySet(1, 1)
    NonEmptySet(1) union NonEmptySet(1, 2) shouldBe NonEmptySet(1, 1, 2)
    NonEmptySet(1, 2) union NonEmptySet(1, 2) shouldBe NonEmptySet(1, 2, 1, 2)
    NonEmptySet(1, 2) union NonEmptySet(1) shouldBe NonEmptySet(1, 2, 1)
    NonEmptySet(1, 2) union NonEmptySet(3, 4, 5) shouldBe NonEmptySet(1, 2, 3, 4, 5)
    NonEmptySet(1, 2, 3) union NonEmptySet(3, 4, 5) shouldBe NonEmptySet(1, 2, 3, 3, 4, 5)
  }
  it should "have an unzip method" in {
    NonEmptySet((1, 2)).unzip shouldBe (NonEmptySet(1),NonEmptySet(2))
    NonEmptySet((1, 2), (3, 4)).unzip shouldBe (NonEmptySet(1, 3), NonEmptySet(2, 4))
    NonEmptySet((1, 2), (3, 4), (5, 6)).unzip shouldBe (NonEmptySet(1, 3, 5), NonEmptySet(2, 4, 6))
  }
  it should "have an unzip3 method" in {
    NonEmptySet((1, 2, 3)).unzip3 shouldBe (NonEmptySet(1), NonEmptySet(2), NonEmptySet(3))
    NonEmptySet((1, 2, 3), (4, 5, 6)).unzip3 shouldBe (NonEmptySet(1, 4), NonEmptySet(2, 5), NonEmptySet(3, 6))
    NonEmptySet((1, 2, 3), (4, 5, 6), (7, 8, 9)).unzip3 shouldBe (NonEmptySet(1, 4, 7), NonEmptySet(2, 5, 8), NonEmptySet(3, 6, 9))
  }
  /*
  it should not have 2 view methods, because I don't want to support views in Every
  */
  /*
    it should not have a zip method
      scala> Set(1) zip Nil
      res0: Set[(Int, Nothing)] = Set()
  */
  it should "have a zipAll method that takes an Iterable" in {

    // Empty on right
    NonEmptySet(1).zipAll(Nil, -1, -2) shouldBe NonEmptySet((1, -2))
    NonEmptySet(1, 2).zipAll(Nil, -1, -2) shouldBe NonEmptySet((1, -2), (2, -2))

    // Same length
    NonEmptySet(1).zipAll(Set(1), -1, -2) shouldBe NonEmptySet((1, 1))
    NonEmptySet(1, 2).zipAll(Set(1, 2), -1, -2) shouldBe NonEmptySet((2, 1), (1, 2))

    // Non-empty, longer on right
    NonEmptySet(1).zipAll(Set(10, 20), -1, -2) shouldBe NonEmptySet((1,10), (-1,20))
    NonEmptySet(1, 2).zipAll(Set(10, 20, 30), -1, -2) shouldBe NonEmptySet((2, 10), (1, 20), (-1, 30))

    // Non-empty, shorter on right
    NonEmptySet(1, 2, 3).zipAll(Set(10, 20), -1, -2) shouldBe NonEmptySet((2, 10), (3, 20), (1, -2))
    NonEmptySet(1, 2, 3, 4).zipAll(Set(10, 20, 30), -1, -2) shouldBe NonEmptySet((2,10), (3,20), (4,30), (1,-2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    NonEmptySet(1).zipAll(Every(1), -1, -2) shouldBe NonEmptySet((1, 1))
    NonEmptySet(1, 2).zipAll(Every(1, 2), -1, -2) shouldBe NonEmptySet((2, 1), (1, 2))

    // Non-empty, longer on right
    NonEmptySet(1).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptySet((1,10), (-1,20))
    NonEmptySet(1, 2).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptySet((2,10), (1,20), (-1,30))

    // Non-empty, shorter on right
    NonEmptySet(1, 2, 3).zipAll(Every(10, 20), -1, -2) shouldBe NonEmptySet((2,10), (3,20), (1,-2))
    NonEmptySet(1, 2, 3, 4).zipAll(Every(10, 20, 30), -1, -2) shouldBe NonEmptySet((2,10), (3,20), (4,30), (1,-2))
  }
  it should "have a zipAll method that takes a NonEmptySet" in {

    // Same length
    NonEmptySet(1).zipAll(NonEmptySet(1), -1, -2) shouldBe NonEmptySet((1, 1))
    NonEmptySet(1, 2).zipAll(NonEmptySet(1, 2), -1, -2) shouldBe NonEmptySet((1, 1), (2, 2))

    // Non-empty, longer on right
    NonEmptySet(1).zipAll(NonEmptySet(10, 20), -1, -2) shouldBe NonEmptySet((1,20), (-1,10))
    NonEmptySet(1, 2).zipAll(NonEmptySet(10, 20, 30), -1, -2) shouldBe NonEmptySet((2,20), (1,30), (-1,10))

    // Non-empty, shorter on right
    NonEmptySet(1, 2, 3).zipAll(NonEmptySet(10, 20), -1, -2) shouldBe NonEmptySet((2,20), (3,10), (1,-2))
    NonEmptySet(1, 2, 3, 4).zipAll(NonEmptySet(10, 20, 30), -1, -2) shouldBe NonEmptySet((2,20), (3,30), (4,10), (1,-2))
  }
  it should "have a zipWithIndex method" in {
    NonEmptySet(99).zipWithIndex shouldBe NonEmptySet((99,0))
    NonEmptySet(1, 2, 3, 4, 5).zipWithIndex shouldBe NonEmptySet((5,0), (4,4), (1,1), (2,2), (3,3))
  }
  "End" should "have a pretty toString" in {
    End.toString shouldBe "End"
  }
}

