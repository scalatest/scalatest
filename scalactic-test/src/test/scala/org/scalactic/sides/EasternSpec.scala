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
package org.scalactic.sides

import org.scalatest._
import org.scalactic._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
// SKIP-SCALATESTJS-START
import SharedHelpers.serializeRoundtrip
// SKIP-SCALATESTJS-END

class EasternSpec extends UnitSpec with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "An Eastern" can "be either West or East" in {
    West(7).eastern.isWest shouldBe true
    West(7).eastern.isEast shouldBe false
    East("oops").eastern.isEast shouldBe true
    East("oops").eastern.isWest shouldBe false
    West(7).eastern.value shouldBe an [West[_]]
    East("oops").eastern.value shouldBe an [East[_]]
  }
  it can "be used with map" in {
    West(8).elseEast[ErrorMessage].eastern map (_.toUpperCase) should equal (West(8).eastern)
    West[Int].elseEast("eight").eastern map (_.toUpperCase) should equal (East("EIGHT").eastern)
  }
  it can "be used with recover" in {
    West[Throwable].elseEast(8).eastern recover {
      case iae: IllegalArgumentException => 9
    } should equal (East(8).eastern)
    West(new IllegalArgumentException).elseEast[Int].eastern recover {
      case iae: IllegalArgumentException => 9
    } should equal (East(9).eastern)
  }
  it can "be used with recoverWith" in {
    West[Throwable].elseEast(8).eastern recoverWith {
      case iae: IllegalArgumentException => East(9).eastern
    } should equal (East(8).eastern)
    West(new IllegalArgumentException).elseEast[Int].eastern recoverWith {
      case iae: IllegalArgumentException => East(9).eastern
    } should equal (East(9).eastern)
  }
  it can "be used with foreach" in {
    var vCount = 0
    var eCount = 0
    East(8).eastern foreach { vCount += _ }
    vCount should equal (8)
    West("eight").elseEast[Int].eastern foreach { eCount += _ }
    eCount should equal (0)
  }
  it can "be used with flatMap" in {
    West[String].elseEast(8).eastern flatMap ((x: Int) => East(x + 1).eastern) should equal (East(9).eastern)
    West("eight").elseEast[Int].eastern flatMap ((x: Int) => East(x + 1).eastern) should equal (West("eight").eastern)
  }
  it can "be used with filter" in {
    East(12).eastern.filter(isRound) shouldBe West("12 was not a round number").eastern
    East(10).eastern.filter(isRound) shouldBe East(10).eastern
    West(12).elseEast[Int].eastern.filter(isRound) shouldBe West(12).eastern
    (for (i <- East(10).eastern if isRound(i)) yield i) shouldBe East(10).eastern
    (for (i <- East(12).eastern if isRound(i)) yield i) shouldBe West("12 was not a round number").eastern
    (for (i <- East(12).eastern if isRound(i)) yield i) shouldBe West("12 was not a round number").eastern
    (for (i <- East(30).eastern if isRound(i) && isDivBy3(i)) yield i) shouldBe East(30).eastern
    (for (i <- East(10).eastern if isRound(i) && isDivBy3(i)) yield i) shouldBe West("10 was not divisible by 3").eastern
    (for (i <- East(3).eastern if isRound(i) && isDivBy3(i)) yield i) shouldBe West("3 was not a round number").eastern
    (for (i <- East(2).eastern if isRound(i) && isDivBy3(i)) yield i) shouldBe West("2 was not a round number").eastern
  }
  it can "be used with exists" in {
    East(12).eastern.exists(_ == 12) shouldBe true
    East(12).eastern.exists(_ == 13) shouldBe false
    West(12).elseEast[Int].eastern.exists(_ == 12) shouldBe false
  }
  it can "be used with forall" in {
    East(12).eastern.forall(_ > 10) shouldBe true
    East(7).eastern.forall(_ > 10) shouldBe false
    West(12).elseEast[Int].eastern.forall(_ > 10) shouldBe true
    West(7).elseEast[Int].eastern.forall(_ > 10) shouldBe true
  }
  it can "be used with getOrElse, which takes a by-name" in {

    East(12).eastern.getOrElse(17) shouldBe 12
    West(12).elseEast[Int].eastern.getOrElse(17) shouldBe 17

    var x = 16 // should not increment if West
    East(12).eastern getOrElse { x += 1; x } shouldBe 12
    x shouldBe 16
    West(12).elseEast[Int].eastern getOrElse { x += 1; x } shouldBe 17
    x shouldBe 17
  }
  it can "be used with orElse, which takes a by-name" in {

    East(12).eastern.orElse(East(13).eastern) shouldBe East(12).eastern
    West(12).eastern.orElse(East(13).eastern) shouldBe East(13).eastern

    East(12).eastern.orElse(West(13).eastern) shouldBe East(12).eastern
    West(12).eastern.orElse(West(13).eastern) shouldBe West(13).eastern

    var x = 16 // should not increment if East
    East(12).eastern orElse { x += 1; East(x).eastern } shouldBe East(12).eastern
    x shouldBe 16
    West(12).elseEast[Int].eastern orElse { x += 1; East(x).eastern } shouldBe East(17).eastern
    x shouldBe 17

    var y = 16 // should not increment if East
    East(12).eastern orElse { y += 1; West(y).eastern } shouldBe East(12).eastern
    y shouldBe 16
    West(12).elseEast[Int].eastern orElse { y += 1; West(y).eastern } shouldBe West(17).eastern
    y shouldBe 17
  }
  it can "be used with toOption" in {
    East(12).eastern.toOption shouldBe Some(12)
    West(12).elseEast[Int].eastern.toOption shouldBe None
  }
  it can "be used with toSeq" in {
    East(12).eastern.toSeq shouldEqual Seq(12)
    West(12).elseEast[Int].eastern.toSeq shouldEqual Seq.empty
  }
// toArray, toBuffer, toIndexedSeq, toIterable, toIterator, toList, 
// toSeq, toStream, toTraversable, toVector
  it can "be used with toEither" in {
    East(12).eastern.toEither shouldBe Right(12)
    West(12).eastern.toEither shouldBe Left(12)
  }
  it can "be used with toOr" in {
    East(12).eastern.toOr shouldBe Good(12)
    West(12).eastern.toOr shouldBe Bad(12)
  }
  it can "be used with toTry, if the error type is a subtype of Throwable" in {
    West[Throwable].elseEast(12).eastern.toTry shouldBe Success(12)
    West[RuntimeException].elseEast(12).eastern.toTry shouldBe Success(12)
    val ex = new RuntimeException("oops")
    West(ex).elseEast[Int].eastern.toTry shouldBe Failure(ex)
    West(ex).elseEast[Int].eastern.toTry shouldBe Failure(ex)
    "West(12).elseEast[Int].eastern.toTry" shouldNot typeCheck
  }
  it can "be used with swap" in {
    West(12).elseEast[String].eastern.swap should === (West[String].elseEast(12).eastern)
    West[Int].elseEast("hi").eastern.swap should === (West("hi").elseEast[Int].eastern)
  }
  it can "be used with transform" in {
    West[String].elseEast(12).eastern.transform((s: String) => East(s.toUpperCase).eastern, (i: Int) => West(i + 1).eastern) should === (West(13).eastern)
    West("hi").elseEast[Int].eastern.transform( (s: String) => East(s.toUpperCase).eastern, (i: Int) => West(i + 1).eastern) should === (East("HI").eastern)
    West[String].elseEast(12).eastern.transform((s: String) => West(s.toUpperCase).eastern, (i: Int) => East(i + 1).eastern) should === (East(13).eastern)
    West("hi").elseEast[Int].eastern.transform((s: String) => West(s.toUpperCase).eastern, (i: Int) => East(i + 1).eastern) should === (West("HI").eastern)
  }
  it can "be folded with fold" in {
    West[String].elseEast(3).eastern.fold(_.length, _ + 1) shouldBe 4
    West("howdy").elseEast[Int].eastern.fold(_.length ,_ + 1) shouldBe 5
  }
/*
  it can "be used with zip" in {
    West(12).elseEast[Every[ErrorMessage]] zip West("hi").elseEast[Every[ErrorMessage]] should === (West((12, "hi")).elseEast[Every[ErrorMessage]])
    West[Int].elseEast(One("so")) zip West[String].elseEast(One("ho")) should === (East(Many("so", "ho")))
    (West(12): Int Or Every[ErrorMessage]) zip East[Every[ErrorMessage]](One("ho")) should === (East(One("ho")))
    East[Every[ErrorMessage]](One("so")) zip West[String]("hi") should === (East(One("so")))

    West[Int](12) zip West[String]("hi") should === (West[(Int, String)]((12, "hi")))
    East[One[ErrorMessage]](One("so")) zip East[Every[ErrorMessage]](One("ho")) should === (East(Many("so", "ho")))
    West[Int](12) zip East[Every[ErrorMessage]](One("ho")) should === (East(One("ho")))
    East[One[ErrorMessage]](One("so")) zip West[String]("hi") should === (East(One("so")))

    West[Int](12) zip West[String]("hi") should === (West[(Int, String)]((12, "hi")))
    East[Every[ErrorMessage]](One("so")) zip East[One[ErrorMessage]](One("ho")) should === (East(Many("so", "ho")))
    West[Int](12) zip East[One[ErrorMessage]](One("ho")) should === (East(One("ho")))
    East[Every[ErrorMessage]](One("so")) zip West[String]("hi") should === (East(One("so")))

    // Works when right hand side ERR type is a supertype of left hand side ERR type, because that's what Every's ++ does.
    West[Int].elseEast(One("oops")) zip West[Int].elseEast(One(-1: Any)) shouldBe East(Many("oops", -1))
    West[Int].elseEast(One("oops": Any)) zip West[Int].elseEast(One(-1)) shouldBe East(Many("oops", -1))
    West[Int].elseEast(One("oops")) zip West[Int].elseEast(One(-1)) shouldBe East(Many("oops", -1))
    West[Int].elseEast(One(-1)) zip West[Int].elseEast(One("oops": Any)) shouldBe East(Many(-1, "oops"))
  }

  it can "be used with when" in {
    West[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 100) Pass else Fail(i + " was not less than 100"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe West(12)
    West[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe East(One("12 was not less than 3"))
    West[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe East(Many("12 was not less than 3", "12 was not odd"))
    West[Int](12).when(
      (i: Int) => if (i > 99) Pass else Fail(i + " was not greater than 99"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe East(Many("12 was not greater than 99", "12 was not less than 3", "12 was not odd"))
    West[Int].elseEast[Every[ErrorMessage]](One("original error")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe East(One("original error"))
    West[Int].elseEast[Every[ErrorMessage]](Many("original error 1", "original error 2")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe East(Many("original error 1", "original error 2"))
    West("hi").elseEast[Every[Int]].when((i: String) => Fail(2.0)) shouldBe East(One(2.0))

    (for (i <- West(10) when isRound) yield i) shouldBe West(10)
    (for (i <- West(12) when isRound) yield i) shouldBe East(One("12 was not a round number"))
    (for (i <- West(12) when isRound) yield i) shouldBe East(One("12 was not a round number"))
    (for (i <- West(30) when (isRound, isDivBy3)) yield i) shouldBe West(30)
    (for (i <- West(10) when (isRound, isDivBy3)) yield i) shouldBe East(One("10 was not divisible by 3"))
    (for (i <- West(3) when (isRound, isDivBy3)) yield i) shouldBe East(One("3 was not a round number"))
    (for (i <- West(2) when (isRound, isDivBy3)) yield i) shouldBe East(Many("2 was not a round number", "2 was not divisible by 3"))
  }
  it can "be created with the attempt helper method" in {
    attempt { 2 / 1 } should === (West(2))
    val divByZero = attempt { throw new ArithmeticException("/ by zero") }
    divByZero.isEast shouldBe true
    divByZero match {
      case East(ex) =>
        ex shouldBe an [ArithmeticException]
        ex.getMessage shouldBe "/ by zero"
      case _ => fail()
    }
    divByZero.isEast shouldBe true
    intercept[VirtualMachineError] {
      attempt { throw new VirtualMachineError {} }
    }
  }
  it can "be created from a Try via the from(Try) factory method" in {
    Or.from(Success(12)) shouldBe West(12)
    Or.from(Success(12): Try[Int]) shouldBe West(12)
    val ex = new Exception("oops")
    Or.from(Failure(ex)) shouldBe East(ex)
    Or.from(Failure(ex): Try[Int]) shouldBe East(ex)
  }
  it can "be created with the from(Either) factory method" in {
    Or.from(Right(12)) shouldBe West(12)
    Or.from(Right(12): Either[String, Int]) shouldBe West(12)
    val ex = new Exception("oops")
    Or.from(Left(ex)) shouldBe East(ex)
    Or.from(Left("oops")) shouldBe East("oops")
    Or.from(Left("oops"): Either[String, String]) shouldBe East("oops")
  }
  it can "be created with the from(Option, EastIfNone) factory method" in {
    Or.from(Some(12), "won't be used") shouldBe West(12)
    Or.from(Some(12): Option[Int], "won't be used") shouldBe West(12)
    val ex = new Exception("oops")
    Or.from(None, ex) shouldBe East(ex)
    Or.from(None, "oops") shouldBe East("oops")
    Or.from(None: Option[String], "oops") shouldBe East("oops")
  }

  it can "be validated with collection.validatedBy" in {

    def isOdd(i: Int): Int Or One[ErrorMessage] =
      if (i % 2 == 1) West(i) else East(One(s"$i was not odd"))

    // List
    List.empty[Int].validatedBy(isOdd) shouldBe West(List.empty[Int])

    List(3).validatedBy(isOdd) shouldBe West(List(3))
    List(4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    List(3, 5).validatedBy(isOdd) shouldBe West(List(3, 5))
    List(4, 6).validatedBy(isOdd) shouldBe East(Every("4 was not odd", "6 was not odd"))
    List(3, 4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    List(4, 3).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    List(3, 5, 7).validatedBy(isOdd) shouldBe West(List(3, 5, 7))

    // Vector
    Vector.empty[Int].validatedBy(isOdd) shouldBe West(Vector.empty[Int])

    Vector(3).validatedBy(isOdd) shouldBe West(Vector(3))
    Vector(4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    Vector(3, 5).validatedBy(isOdd) shouldBe West(Vector(3, 5))
    Vector(4, 6).validatedBy(isOdd) shouldBe East(Every("4 was not odd", "6 was not odd"))
    Vector(3, 4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    Vector(4, 3).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    Vector(3, 5, 7).validatedBy(isOdd) shouldBe West(Vector(3, 5, 7))

    // Iterator
    List.empty[Int].iterator.validatedBy(isOdd).map(_.toStream) shouldBe West(List.empty[Int].iterator).map(_.toStream)

    List(3).iterator.validatedBy(isOdd).map(_.toStream) shouldBe West(List(3).iterator).map(_.toStream)
    List(4).iterator.validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    List(3, 5).iterator.validatedBy(isOdd).map(_.toStream) shouldBe West(List(3, 5).iterator).map(_.toStream)
    List(4, 6).iterator.validatedBy(isOdd) shouldBe East(Every("4 was not odd", "6 was not odd"))
    List(3, 4).iterator.validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    List(4, 3).iterator.validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    List(3, 5, 7).iterator.validatedBy(isOdd).map(_.toStream) shouldBe West(List(3, 5, 7).iterator).map(_.toStream)

    // Set
    Set.empty[Int].validatedBy(isOdd) shouldBe West(Set.empty[Int])

    Set(3).validatedBy(isOdd) shouldBe West(Set(3))
    Set(4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    Set(3, 5).validatedBy(isOdd) shouldBe West(Set(3, 5))
    Set(4, 6).validatedBy(isOdd) shouldBe East(Every("4 was not odd", "6 was not odd"))
    Set(3, 4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    Set(4, 3).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    Set(3, 5, 7).validatedBy(isOdd) shouldBe West(Set(3, 5, 7))

    Set.empty[Int].validatedBy(isOdd) shouldBe West(Set.empty[Int])

    // Every
    One(3).validatedBy(isOdd) shouldBe West(One(3))
    Every(3).validatedBy(isOdd) shouldBe West(One(3))
    One(4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    Every(4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    Many(3, 5).validatedBy(isOdd) shouldBe West(Many(3, 5))
    Every(3, 5).validatedBy(isOdd) shouldBe West(Many(3, 5))
    Many(4, 6).validatedBy(isOdd) shouldBe East(Every("4 was not odd", "6 was not odd"))
    Every(4, 6).validatedBy(isOdd) shouldBe East(Every("4 was not odd", "6 was not odd"))
    Many(3, 4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    Every(3, 4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    Many(4, 3).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
    Every(4, 3).validatedBy(isOdd) shouldBe East(One("4 was not odd"))

    Many(3, 5, 7).validatedBy(isOdd) shouldBe West(Every(3, 5, 7))
    Every(3, 5, 7).validatedBy(isOdd) shouldBe West(Many(3, 5, 7))

    // Option
    Some(3).validatedBy(isOdd) shouldBe West(Some(3))
    (None: Option[Int]).validatedBy(isOdd) shouldBe West(None)
    Some(4).validatedBy(isOdd) shouldBe East(One("4 was not odd"))
  }

  it can "be validated with collection.validatedBy when the map goes to a different type" in {
    def parseAge(input: String): Int Or One[ErrorMessage] = {
      try {
        val age = input.trim.toInt
        if (age >= 0) West(age) else East(One(s""""${age}" is not a valid age"""))
      }
      catch {
        case _: NumberFormatException => East(One(s""""${input}" is not a valid integer"""))
      }
    }

    Some("29").validatedBy(parseAge) shouldBe West(Some(29))
    Some("-30").validatedBy(parseAge) shouldBe East(One("\"-30\" is not a valid age"))

    Every("29", "30", "31").validatedBy(parseAge) shouldBe West(Many(29, 30, 31))
    Every("29", "-30", "31").validatedBy(parseAge) shouldBe East(One("\"-30\" is not a valid age"))
    Every("29", "-30", "-31").validatedBy(parseAge) shouldBe East(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))

    List("29", "30", "31").validatedBy(parseAge) shouldBe West(List(29, 30, 31))
    List("29", "-30", "31").validatedBy(parseAge) shouldBe East(One("\"-30\" is not a valid age"))
    List("29", "-30", "-31").validatedBy(parseAge) shouldBe East(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))
  }

  it can "be combined with collection.combined" in {

    // List
    // Is this the right answer? Has to be, because couldn't come up with an error anyway.
    List.empty[Int Or Every[String]].combined shouldBe West(List.empty[Int])

    //  def combine[G, ELE, EVERY[b] <: Every[b], SEQ[s]](xs: SEQ[G Or EVERY[ELE]])(implicit seq: Sequenceable[SEQ]): SEQ[G] Or Every[ELE] =
    // G = Int, ELE = Nothing, SEQ = List
    List(West(3)).combined shouldBe West(List(3))
    List(East(One("oops"))).combined shouldBe East(One("oops"))

    List(West(3), West(4)).combined shouldBe West(List(3, 4))
    List(East(One("darn")), East(One("oops"))).combined shouldBe East(Every("darn", "oops"))
    List(West(3), East(One("oops"))).combined shouldBe East(One("oops"))
    List(East(One("oops")), West(3)).combined shouldBe East(One("oops"))

    List(West(3), West(4), West(5)).combined shouldBe West(List(3, 4, 5))

    // Vector
    Vector.empty[Int Or Every[String]].combined shouldBe West(Vector.empty[Int])

    Vector(West(3)).combined shouldBe West(Vector(3))
    Vector(East(One("oops"))).combined shouldBe East(One("oops"))

    Vector(West(3), West(4)).combined shouldBe West(Vector(3, 4))
    Vector(East(One("darn")), East(One("oops"))).combined shouldBe East(Every("darn", "oops"))
    Vector(West(3), East(One("oops"))).combined shouldBe East(One("oops"))
    Vector(East(One("oops")), West(3)).combined shouldBe East(One("oops"))

    Vector(West(3), West(4), West(5)).combined shouldBe West(Vector(3, 4, 5))

    // Do the same thing with Iterator
    (List.empty[Int Or Every[String]].iterator).combined.map(_.toStream) shouldEqual (West(List.empty[Int].iterator).map(_.toStream))

    List(West(3)).iterator.combined.map(_.toStream) shouldEqual (West(List(3).iterator).map(_.toStream))
    List(East(One("oops"))).iterator.combined shouldEqual (East(One("oops")))

    List(West(3), West(4)).iterator.combined.map(_.toStream) shouldEqual (West(List(3, 4).iterator).map(_.toStream))
    List(East(One("darn")), East(One("oops"))).iterator.combined shouldEqual (East(Every("darn", "oops")))
    List(West(3), East(One("oops"))).iterator.combined shouldEqual (East(One("oops")))
    List(East(One("oops")), West(3)).iterator.combined shouldEqual (East(One("oops")))

    List(West(3), West(4), West(5)).iterator.combined.map(_.toStream) shouldEqual (West(List(3, 4, 5).iterator).map(_.toStream))

    // Set
    Set.empty[Int Or Every[String]].combined shouldBe West(Set.empty[Int])
    Set(West[Int](3), East[Every[String]](Every("oops"))).asInstanceOf[Set[Int Or Every[String]]].combined shouldBe East(One("oops"))
    Set(West[Int](3), East[Every[String]](Every("oops"))).combined shouldBe East(One("oops"))

    Set(West(3)).combined shouldBe West(Set(3))
    Set(East(One("oops"))).combined shouldBe East(One("oops"))

    Set(West(3), West(4)).combined shouldBe West(Set(3, 4))
    Set(East(One("darn")), East(One("oops"))).combined shouldBe East(Every("darn", "oops"))
    Set(West(3), East(One("oops"))).combined shouldBe East(One("oops"))
    Set(East(One("oops")), West(3)).combined shouldBe East(One("oops"))

    Set(West(3), West(4), West(5)).combined shouldBe West(Set(3, 4, 5))

    // Every
    Every(West(3).elseEast[Every[String]], West[Int].elseEast(Every("oops"))).combined shouldBe East(One("oops"))

    Every(West(3)).combined shouldBe West(Every(3))
    One(West(3)).combined shouldBe West(Every(3))
    Every(East(One("oops"))).combined shouldBe East(One("oops"))
    One(East(One("oops"))).combined shouldBe East(One("oops"))

    Every(West(3), West(4)).combined shouldBe West(Every(3, 4))
    Many(West(3), West(4)).combined shouldBe West(Every(3, 4))
    Every(East(One("darn")), East(One("oops"))).combined shouldBe East(Every("darn", "oops"))
    Many(East(One("darn")), East(One("oops"))).combined shouldBe East(Every("darn", "oops"))
    Every(West(3), East(One("oops"))).combined shouldBe East(One("oops"))
    Every(East(One("oops")), West(3)).combined shouldBe East(One("oops"))

    Every(West(3), West(4), West(5)).combined shouldBe West(Every(3, 4, 5))

    // Option
    Some(West(3)).combined shouldBe West(Some(3))
    (None: Option[Int Or Every[ErrorMessage]]).combined shouldBe West(None)
    Some(East(One("oops"))).combined shouldBe East(One("oops"))
    Some(East(Many("oops", "idoops"))).combined shouldBe East(Many("oops", "idoops"))
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Or.from(Success(12)) shouldBe West(12))
    val ex = new Exception("oops")
    serializeRoundtrip(Or.from(Failure(ex)) shouldBe East(ex))
  }
  // SKIP-SCALATESTJS-END
  "A West" can "be widened to an Or type via .asOr" in {
    West(1).asOr shouldBe West(1)
    /*
      scala> xs.foldLeft(West(6).elseEast[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) West(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,org.scalautils.ErrorMessage]
       required: org.scalautils.West[Int,org.scalautils.ErrorMessage]
                    xs.foldLeft(West(6).elseEast[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) West(x) else acc) }
                                                                         ^

      scala> xs.foldLeft(West(6).elseEast[ErrorMessage].asOr) { (acc, x) => acc orElse (if (x % 2 == 0) West(x) else acc) }
      res2: org.scalautils.Or[Int,org.scalautils.ErrorMessage] = West(6)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(West(6).elseEast[ErrorMessage].asOr) {
      (acc, x) => acc orElse (if (x % 2 == 0) West(x) else acc)
    } shouldBe West(6)
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(West(1)) shouldBe West(1)
  }
  // SKIP-SCALATESTJS-END
  "A East" can "be widened to an Or type via .asOr" in {
    East("oops").asOr shouldBe East("oops")
    /*
      scala> xs.foldLeft(West[Int].elseEast("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) West(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,String]
       required: org.scalautils.East[Int,String]
                    xs.foldLeft(West[Int].elseEast("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) West(x) else acc) }
                                                                               ^

      scala> xs.foldLeft(West[Int].elseEast("no evens").asOr) { (acc, x) => acc orElse (if (x % 2 == 0) West(x) else acc) }
      res7: org.scalautils.Or[Int,String] = West(2)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(West[Int].elseEast("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) West(x) else acc)
    } shouldBe West(2)
    val ys = List(1, 3, 5)
    ys.foldLeft(West[Int].elseEast("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) West(x) else acc)
    } shouldBe East("no evens")
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(East("oops")) shouldBe East("oops")
  }
  // SKIP-SCALATESTJS-END
  "The Or companion" should "offer a concise type lambda syntax" in {
    trait Functor[Context[_]] {
      def map[A, B](ca: Context[A])(f: A => B): Context[B]
    }
    // One way:
    class OrFunctor[BAD] extends Functor[Or.B[BAD]#G] {
      override def map[G, H](ca: G Or BAD)(f: G => H): H Or BAD = ca.map(f)
    }
    class EastOrFunctor[GOOD] extends Functor[Or.G[GOOD]#B] {
      override def map[B, C](ca: GOOD Or B)(f: B => C): GOOD Or C = ca.badMap(f)
    }
/*
    // Other way:
    class OrFunctor[B] extends Functor[Or.BAD[B]#GOOD] {
      override def map[G, H](ca: G Or B)(f: G => H): H Or B = ca.map(f)
    }
    class EastOrFunctor[G] extends Functor[Or.GOOD[G]#BAD] {
      override def map[B, C](ca: G Or B)(f: B => C): G Or C = ca.badMap(f)
    }
*/
  }
*/
}

