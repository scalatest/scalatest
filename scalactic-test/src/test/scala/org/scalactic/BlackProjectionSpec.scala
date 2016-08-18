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

import org.scalatest._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
// SKIP-SCALATESTJS-START
import SharedHelpers.serializeRoundtrip
// SKIP-SCALATESTJS-END

class BlackProjectionSpec extends UnitSpec with Accumulation with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "A BlackProjection" can "be either Black or White" in {
    Black(7).black.isBlack shouldBe true
    Black(7).black.isWhite shouldBe false
    White("oops").black.isWhite shouldBe true
    White("oops").black.isBlack shouldBe false
    Black(7).black.underlying shouldBe an [Black[_]]
    White("oops").black.underlying shouldBe an [White[_]]
  }
  it can "be used with map" in {
    Black(8).black map (_ + 1) should equal (Black(9).black)
    Black[Int].otherwiseWhite("eight").black map (_ + 1) should equal (White("eight").black)
  }
  it can "be used with recover" in {
    Black(8).otherwiseWhite[Throwable].black recover {
      case iae: IllegalArgumentException => 9
    } should equal (Black(8).black)
    Black[Int].otherwiseWhite(new IllegalArgumentException).black recover {
      case iae: IllegalArgumentException => 9
    } should equal (Black(9).black)
  }
  it can "be used with recoverWith" in {
    Black(8).otherwiseWhite[Throwable].black recoverWith {
      case iae: IllegalArgumentException => Black(9).black
    } should equal (Black(8).black)
    Black[Int].otherwiseWhite(new IllegalArgumentException).black recoverWith {
      case iae: IllegalArgumentException => Black(9).black
    } should equal (Black(9).black)
  }
  it can "be used with foreach" in {
    var vCount = 0
    var eCount = 0
    Black(8).black foreach { vCount += _ }
    vCount should equal (8)
    Black[Int].otherwiseWhite("eight").black foreach { eCount += _ }
    eCount should equal (0)
  }
  it can "be used with flatMap" in {
    Black(8).otherwiseWhite[String].black flatMap ((x: Int) => Black(x + 1).black) should equal (Black(9).black)
    Black[Int].otherwiseWhite("eight").black flatMap ((x: Int) => Black(x + 1).black) should equal (White("eight").black)
  }
  it can "be used with filter" in {
    Black(12).black.filter(isRound) shouldBe White("12 was not a round number").black
    Black(10).black.filter(isRound) shouldBe Black(10).black
    Black[Int].otherwiseWhite(12).black.filter(isRound) shouldBe White(12).black
    (for (i <- Black(10).black if isRound(i)) yield i) shouldBe Black(10).black
    (for (i <- Black(12).black if isRound(i)) yield i) shouldBe White("12 was not a round number").black
    (for (i <- Black(12).black if isRound(i)) yield i) shouldBe White("12 was not a round number").black
    (for (i <- Black(30).black if isRound(i) && isDivBy3(i)) yield i) shouldBe Black(30).black
    (for (i <- Black(10).black if isRound(i) && isDivBy3(i)) yield i) shouldBe White("10 was not divisible by 3").black
    (for (i <- Black(3).black if isRound(i) && isDivBy3(i)) yield i) shouldBe White("3 was not a round number").black
    (for (i <- Black(2).black if isRound(i) && isDivBy3(i)) yield i) shouldBe White("2 was not a round number").black
  }
  it can "be used with exists" in {
    Black(12).black.exists(_ == 12) shouldBe true
    Black(12).black.exists(_ == 13) shouldBe false
    Black[Int].otherwiseWhite(12).black.exists(_ == 12) shouldBe false
  }
  it can "be used with forall" in {
    Black(12).black.forall(_ > 10) shouldBe true
    Black(7).black.forall(_ > 10) shouldBe false
    Black[Int].otherwiseWhite(12).black.forall(_ > 10) shouldBe true
    Black[Int].otherwiseWhite(7).black.forall(_ > 10) shouldBe true
  }
  it can "be used with getOrElse, which takes a by-name" in {

    Black(12).black.getOrElse(17) shouldBe 12
    Black[Int].otherwiseWhite(12).black.getOrElse(17) shouldBe 17

    var x = 16 // should not increment if Black
    Black(12).black getOrElse { x += 1; x } shouldBe 12
    x shouldBe 16
    Black[Int].otherwiseWhite(12).black getOrElse { x += 1; x } shouldBe 17
    x shouldBe 17
  }
  it can "be used with orElse, which takes a by-name" in {

    Black(12).black.orElse(Black(13).black) shouldBe Black(12).black
    White(12).black.orElse(Black(13).black) shouldBe Black(13).black

    Black(12).black.orElse(White(13).black) shouldBe Black(12).black
    White(12).black.orElse(White(13).black) shouldBe White(13).black

    var x = 16 // should not increment if Black
    Black(12).black orElse { x += 1; Black(x).black } shouldBe Black(12).black
    x shouldBe 16
    Black[Int].otherwiseWhite(12).black orElse { x += 1; Black(x).black } shouldBe Black(17).black
    x shouldBe 17

    var y = 16 // should not increment if Black
    Black(12).black orElse { y += 1; White(y).black } shouldBe Black(12).black
    y shouldBe 16
    Black[Int].otherwiseWhite(12).black orElse { y += 1; White(y).black } shouldBe White(17).black
    y shouldBe 17
  }
/*
  it can "be used with toOption" in {
    Black(12).toOption shouldBe Some(12)
    Black[Int].otherwiseWhite(12).toOption shouldBe None
  }
  it can "be used with toSeq" in {
    Black(12).toSeq shouldEqual Seq(12)
    Black[Int].otherwiseWhite(12).toSeq shouldEqual Seq.empty
  }
// toArray, toBuffer, toIndexedSeq, toIterable, toIterator, toList, 
// toSeq, toStream, toTraversable, toVector
  it can "be used with toEither" in {
    Black(12).toEither shouldBe Right(12)
    White(12).toEither shouldBe Left(12)
  }
  it can "be used with accumulating" in {
    Black(12).otherwiseWhite[Int].accumulating shouldBe Black(12).otherwiseWhite[Every[Int]]
    Black[Int].otherwiseWhite(12).accumulating shouldBe Black[Int].otherwiseWhite(One(12))
  }
  it can "be used with toTry, if the error type is a subtype of Throwable" in {
    Black(12).otherwiseWhite[Throwable].toTry shouldBe Success(12)
    Black(12).otherwiseWhite[RuntimeException].toTry shouldBe Success(12)
    val ex = new RuntimeException("oops")
    Black[Int].otherwiseWhite(ex).toTry shouldBe Failure(ex)
    Black[Int].otherwiseWhite(ex).toTry shouldBe Failure(ex)
    // Does not compile: Black[Int, Int](12).toTry shouldBe Success(12)
  }
  it can "be used with transform" in {
    Black(12).otherwiseWhite[String].transform((i: Int) => Black(i + 1), (s: String) => White(s.toUpperCase)) should === (Black(13))
    Black[Int].otherwiseWhite("hi").transform((i: Int) => Black(i + 1), (s: String) => White(s.toUpperCase)) should === (White("HI"))
    Black(12).otherwiseWhite[String].transform((i: Int) => White(i + 1), (s: String) => Black(s.toUpperCase)) should === (White(13))
    Black[Int].otherwiseWhite("hi").transform((i: Int) => White(i + 1), (s: String) => Black(s.toUpperCase)) should === (Black("HI"))
  }
  it can "be used with swap" in {
    Black(12).otherwiseWhite[String].swap should === (Black[String].otherwiseWhite(12))
    Black[Int].otherwiseWhite("hi").swap should === (Black("hi").otherwiseWhite[Int])
  }
  it can "be used with zip" in {
    Black(12).otherwiseWhite[Every[ErrorMessage]] zip Black("hi").otherwiseWhite[Every[ErrorMessage]] should === (Black((12, "hi")).otherwiseWhite[Every[ErrorMessage]])
    Black[Int].otherwiseWhite(One("so")) zip Black[String].otherwiseWhite(One("ho")) should === (White(Many("so", "ho")))
    (Black(12): Int Or Every[ErrorMessage]) zip White[Every[ErrorMessage]](One("ho")) should === (White(One("ho")))
    White[Every[ErrorMessage]](One("so")) zip Black[String]("hi") should === (White(One("so")))

    Black[Int](12) zip Black[String]("hi") should === (Black[(Int, String)]((12, "hi")))
    White[One[ErrorMessage]](One("so")) zip White[Every[ErrorMessage]](One("ho")) should === (White(Many("so", "ho")))
    Black[Int](12) zip White[Every[ErrorMessage]](One("ho")) should === (White(One("ho")))
    White[One[ErrorMessage]](One("so")) zip Black[String]("hi") should === (White(One("so")))

    Black[Int](12) zip Black[String]("hi") should === (Black[(Int, String)]((12, "hi")))
    White[Every[ErrorMessage]](One("so")) zip White[One[ErrorMessage]](One("ho")) should === (White(Many("so", "ho")))
    Black[Int](12) zip White[One[ErrorMessage]](One("ho")) should === (White(One("ho")))
    White[Every[ErrorMessage]](One("so")) zip Black[String]("hi") should === (White(One("so")))

    // Works when right hand side ERR type is a supertype of left hand side ERR type, because that's what Every's ++ does.
    Black[Int].otherwiseWhite(One("oops")) zip Black[Int].otherwiseWhite(One(-1: Any)) shouldBe White(Many("oops", -1))
    Black[Int].otherwiseWhite(One("oops": Any)) zip Black[Int].otherwiseWhite(One(-1)) shouldBe White(Many("oops", -1))
    Black[Int].otherwiseWhite(One("oops")) zip Black[Int].otherwiseWhite(One(-1)) shouldBe White(Many("oops", -1))
    Black[Int].otherwiseWhite(One(-1)) zip Black[Int].otherwiseWhite(One("oops": Any)) shouldBe White(Many(-1, "oops"))
  }

  it can "be used with when" in {
    Black[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 100) Pass else Fail(i + " was not less than 100"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Black(12)
    Black[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe White(One("12 was not less than 3"))
    Black[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe White(Many("12 was not less than 3", "12 was not odd"))
    Black[Int](12).when(
      (i: Int) => if (i > 99) Pass else Fail(i + " was not greater than 99"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe White(Many("12 was not greater than 99", "12 was not less than 3", "12 was not odd"))
    Black[Int].otherwiseWhite[Every[ErrorMessage]](One("original error")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe White(One("original error"))
    Black[Int].otherwiseWhite[Every[ErrorMessage]](Many("original error 1", "original error 2")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe White(Many("original error 1", "original error 2"))
    Black("hi").otherwiseWhite[Every[Int]].when((i: String) => Fail(2.0)) shouldBe White(One(2.0))

    (for (i <- Black(10) when isRound) yield i) shouldBe Black(10)
    (for (i <- Black(12) when isRound) yield i) shouldBe White(One("12 was not a round number"))
    (for (i <- Black(12) when isRound) yield i) shouldBe White(One("12 was not a round number"))
    (for (i <- Black(30) when (isRound, isDivBy3)) yield i) shouldBe Black(30)
    (for (i <- Black(10) when (isRound, isDivBy3)) yield i) shouldBe White(One("10 was not divisible by 3"))
    (for (i <- Black(3) when (isRound, isDivBy3)) yield i) shouldBe White(One("3 was not a round number"))
    (for (i <- Black(2) when (isRound, isDivBy3)) yield i) shouldBe White(Many("2 was not a round number", "2 was not divisible by 3"))
  }
  it can "be created with the attempt helper method" in {
    attempt { 2 / 1 } should === (Black(2))
    val divByZero = attempt { throw new ArithmeticException("/ by zero") }
    divByZero.isWhite shouldBe true
    divByZero match {
      case White(ex) =>
        ex shouldBe an [ArithmeticException]
        ex.getMessage shouldBe "/ by zero"
      case _ => fail()
    }
    divByZero.isWhite shouldBe true
    intercept[VirtualMachineError] {
      attempt { throw new VirtualMachineError {} }
    }
  }
  it can "be created from a Try via the from(Try) factory method" in {
    Or.from(Success(12)) shouldBe Black(12)
    Or.from(Success(12): Try[Int]) shouldBe Black(12)
    val ex = new Exception("oops")
    Or.from(Failure(ex)) shouldBe White(ex)
    Or.from(Failure(ex): Try[Int]) shouldBe White(ex)
  }
  it can "be created with the from(Either) factory method" in {
    Or.from(Right(12)) shouldBe Black(12)
    Or.from(Right(12): Either[String, Int]) shouldBe Black(12)
    val ex = new Exception("oops")
    Or.from(Left(ex)) shouldBe White(ex)
    Or.from(Left("oops")) shouldBe White("oops")
    Or.from(Left("oops"): Either[String, String]) shouldBe White("oops")
  }
  it can "be created with the from(Option, WhiteIfNone) factory method" in {
    Or.from(Some(12), "won't be used") shouldBe Black(12)
    Or.from(Some(12): Option[Int], "won't be used") shouldBe Black(12)
    val ex = new Exception("oops")
    Or.from(None, ex) shouldBe White(ex)
    Or.from(None, "oops") shouldBe White("oops")
    Or.from(None: Option[String], "oops") shouldBe White("oops")
  }

  it can "be validated with collection.validatedBy" in {

    def isOdd(i: Int): Int Or One[ErrorMessage] =
      if (i % 2 == 1) Black(i) else White(One(s"$i was not odd"))

    // List
    List.empty[Int].validatedBy(isOdd) shouldBe Black(List.empty[Int])

    List(3).validatedBy(isOdd) shouldBe Black(List(3))
    List(4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    List(3, 5).validatedBy(isOdd) shouldBe Black(List(3, 5))
    List(4, 6).validatedBy(isOdd) shouldBe White(Every("4 was not odd", "6 was not odd"))
    List(3, 4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    List(4, 3).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    List(3, 5, 7).validatedBy(isOdd) shouldBe Black(List(3, 5, 7))

    // Vector
    Vector.empty[Int].validatedBy(isOdd) shouldBe Black(Vector.empty[Int])

    Vector(3).validatedBy(isOdd) shouldBe Black(Vector(3))
    Vector(4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    Vector(3, 5).validatedBy(isOdd) shouldBe Black(Vector(3, 5))
    Vector(4, 6).validatedBy(isOdd) shouldBe White(Every("4 was not odd", "6 was not odd"))
    Vector(3, 4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    Vector(4, 3).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    Vector(3, 5, 7).validatedBy(isOdd) shouldBe Black(Vector(3, 5, 7))

    // Iterator
    List.empty[Int].iterator.validatedBy(isOdd).map(_.toStream) shouldBe Black(List.empty[Int].iterator).map(_.toStream)

    List(3).iterator.validatedBy(isOdd).map(_.toStream) shouldBe Black(List(3).iterator).map(_.toStream)
    List(4).iterator.validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    List(3, 5).iterator.validatedBy(isOdd).map(_.toStream) shouldBe Black(List(3, 5).iterator).map(_.toStream)
    List(4, 6).iterator.validatedBy(isOdd) shouldBe White(Every("4 was not odd", "6 was not odd"))
    List(3, 4).iterator.validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    List(4, 3).iterator.validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    List(3, 5, 7).iterator.validatedBy(isOdd).map(_.toStream) shouldBe Black(List(3, 5, 7).iterator).map(_.toStream)

    // Set
    Set.empty[Int].validatedBy(isOdd) shouldBe Black(Set.empty[Int])

    Set(3).validatedBy(isOdd) shouldBe Black(Set(3))
    Set(4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    Set(3, 5).validatedBy(isOdd) shouldBe Black(Set(3, 5))
    Set(4, 6).validatedBy(isOdd) shouldBe White(Every("4 was not odd", "6 was not odd"))
    Set(3, 4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    Set(4, 3).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    Set(3, 5, 7).validatedBy(isOdd) shouldBe Black(Set(3, 5, 7))

    Set.empty[Int].validatedBy(isOdd) shouldBe Black(Set.empty[Int])

    // Every
    One(3).validatedBy(isOdd) shouldBe Black(One(3))
    Every(3).validatedBy(isOdd) shouldBe Black(One(3))
    One(4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    Every(4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    Many(3, 5).validatedBy(isOdd) shouldBe Black(Many(3, 5))
    Every(3, 5).validatedBy(isOdd) shouldBe Black(Many(3, 5))
    Many(4, 6).validatedBy(isOdd) shouldBe White(Every("4 was not odd", "6 was not odd"))
    Every(4, 6).validatedBy(isOdd) shouldBe White(Every("4 was not odd", "6 was not odd"))
    Many(3, 4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    Every(3, 4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    Many(4, 3).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
    Every(4, 3).validatedBy(isOdd) shouldBe White(One("4 was not odd"))

    Many(3, 5, 7).validatedBy(isOdd) shouldBe Black(Every(3, 5, 7))
    Every(3, 5, 7).validatedBy(isOdd) shouldBe Black(Many(3, 5, 7))

    // Option
    Some(3).validatedBy(isOdd) shouldBe Black(Some(3))
    (None: Option[Int]).validatedBy(isOdd) shouldBe Black(None)
    Some(4).validatedBy(isOdd) shouldBe White(One("4 was not odd"))
  }

  it can "be validated with collection.validatedBy when the map goes to a different type" in {
    def parseAge(input: String): Int Or One[ErrorMessage] = {
      try {
        val age = input.trim.toInt
        if (age >= 0) Black(age) else White(One(s""""${age}" is not a valid age"""))
      }
      catch {
        case _: NumberFormatException => White(One(s""""${input}" is not a valid integer"""))
      }
    }

    Some("29").validatedBy(parseAge) shouldBe Black(Some(29))
    Some("-30").validatedBy(parseAge) shouldBe White(One("\"-30\" is not a valid age"))

    Every("29", "30", "31").validatedBy(parseAge) shouldBe Black(Many(29, 30, 31))
    Every("29", "-30", "31").validatedBy(parseAge) shouldBe White(One("\"-30\" is not a valid age"))
    Every("29", "-30", "-31").validatedBy(parseAge) shouldBe White(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))

    List("29", "30", "31").validatedBy(parseAge) shouldBe Black(List(29, 30, 31))
    List("29", "-30", "31").validatedBy(parseAge) shouldBe White(One("\"-30\" is not a valid age"))
    List("29", "-30", "-31").validatedBy(parseAge) shouldBe White(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))
  }

  it can "be combined with collection.combined" in {

    // List
    // Is this the right answer? Has to be, because couldn't come up with an error anyway.
    List.empty[Int Or Every[String]].combined shouldBe Black(List.empty[Int])

    //  def combine[G, ELE, EVERY[b] <: Every[b], SEQ[s]](xs: SEQ[G Or EVERY[ELE]])(implicit seq: Sequenceable[SEQ]): SEQ[G] Or Every[ELE] =
    // G = Int, ELE = Nothing, SEQ = List
    List(Black(3)).combined shouldBe Black(List(3))
    List(White(One("oops"))).combined shouldBe White(One("oops"))

    List(Black(3), Black(4)).combined shouldBe Black(List(3, 4))
    List(White(One("darn")), White(One("oops"))).combined shouldBe White(Every("darn", "oops"))
    List(Black(3), White(One("oops"))).combined shouldBe White(One("oops"))
    List(White(One("oops")), Black(3)).combined shouldBe White(One("oops"))

    List(Black(3), Black(4), Black(5)).combined shouldBe Black(List(3, 4, 5))

    // Vector
    Vector.empty[Int Or Every[String]].combined shouldBe Black(Vector.empty[Int])

    Vector(Black(3)).combined shouldBe Black(Vector(3))
    Vector(White(One("oops"))).combined shouldBe White(One("oops"))

    Vector(Black(3), Black(4)).combined shouldBe Black(Vector(3, 4))
    Vector(White(One("darn")), White(One("oops"))).combined shouldBe White(Every("darn", "oops"))
    Vector(Black(3), White(One("oops"))).combined shouldBe White(One("oops"))
    Vector(White(One("oops")), Black(3)).combined shouldBe White(One("oops"))

    Vector(Black(3), Black(4), Black(5)).combined shouldBe Black(Vector(3, 4, 5))

    // Do the same thing with Iterator
    (List.empty[Int Or Every[String]].iterator).combined.map(_.toStream) shouldEqual (Black(List.empty[Int].iterator).map(_.toStream))

    List(Black(3)).iterator.combined.map(_.toStream) shouldEqual (Black(List(3).iterator).map(_.toStream))
    List(White(One("oops"))).iterator.combined shouldEqual (White(One("oops")))

    List(Black(3), Black(4)).iterator.combined.map(_.toStream) shouldEqual (Black(List(3, 4).iterator).map(_.toStream))
    List(White(One("darn")), White(One("oops"))).iterator.combined shouldEqual (White(Every("darn", "oops")))
    List(Black(3), White(One("oops"))).iterator.combined shouldEqual (White(One("oops")))
    List(White(One("oops")), Black(3)).iterator.combined shouldEqual (White(One("oops")))

    List(Black(3), Black(4), Black(5)).iterator.combined.map(_.toStream) shouldEqual (Black(List(3, 4, 5).iterator).map(_.toStream))

    // Set
    Set.empty[Int Or Every[String]].combined shouldBe Black(Set.empty[Int])
    Set(Black[Int](3), White[Every[String]](Every("oops"))).asInstanceOf[Set[Int Or Every[String]]].combined shouldBe White(One("oops"))
    Set(Black[Int](3), White[Every[String]](Every("oops"))).combined shouldBe White(One("oops"))

    Set(Black(3)).combined shouldBe Black(Set(3))
    Set(White(One("oops"))).combined shouldBe White(One("oops"))

    Set(Black(3), Black(4)).combined shouldBe Black(Set(3, 4))
    Set(White(One("darn")), White(One("oops"))).combined shouldBe White(Every("darn", "oops"))
    Set(Black(3), White(One("oops"))).combined shouldBe White(One("oops"))
    Set(White(One("oops")), Black(3)).combined shouldBe White(One("oops"))

    Set(Black(3), Black(4), Black(5)).combined shouldBe Black(Set(3, 4, 5))

    // Every
    Every(Black(3).otherwiseWhite[Every[String]], Black[Int].otherwiseWhite(Every("oops"))).combined shouldBe White(One("oops"))

    Every(Black(3)).combined shouldBe Black(Every(3))
    One(Black(3)).combined shouldBe Black(Every(3))
    Every(White(One("oops"))).combined shouldBe White(One("oops"))
    One(White(One("oops"))).combined shouldBe White(One("oops"))

    Every(Black(3), Black(4)).combined shouldBe Black(Every(3, 4))
    Many(Black(3), Black(4)).combined shouldBe Black(Every(3, 4))
    Every(White(One("darn")), White(One("oops"))).combined shouldBe White(Every("darn", "oops"))
    Many(White(One("darn")), White(One("oops"))).combined shouldBe White(Every("darn", "oops"))
    Every(Black(3), White(One("oops"))).combined shouldBe White(One("oops"))
    Every(White(One("oops")), Black(3)).combined shouldBe White(One("oops"))

    Every(Black(3), Black(4), Black(5)).combined shouldBe Black(Every(3, 4, 5))

    // Option
    Some(Black(3)).combined shouldBe Black(Some(3))
    (None: Option[Int Or Every[ErrorMessage]]).combined shouldBe Black(None)
    Some(White(One("oops"))).combined shouldBe White(One("oops"))
    Some(White(Many("oops", "idoops"))).combined shouldBe White(Many("oops", "idoops"))
  }
  it can "be folded with fold" in {
    Black(3).otherwiseWhite[String].fold(_ + 1, _.length) shouldBe 4
    Black[Int].otherwiseWhite("howdy").fold(_ + 1, _.length) shouldBe 5

  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Or.from(Success(12)) shouldBe Black(12))
    val ex = new Exception("oops")
    serializeRoundtrip(Or.from(Failure(ex)) shouldBe White(ex))
  }
  // SKIP-SCALATESTJS-END
  "A Black" can "be widened to an Or type via .asOr" in {
    Black(1).asOr shouldBe Black(1)
    /*
      scala> xs.foldLeft(Black(6).otherwiseWhite[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) Black(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,org.scalautils.ErrorMessage]
       required: org.scalautils.Black[Int,org.scalautils.ErrorMessage]
                    xs.foldLeft(Black(6).otherwiseWhite[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) Black(x) else acc) }
                                                                         ^

      scala> xs.foldLeft(Black(6).otherwiseWhite[ErrorMessage].asOr) { (acc, x) => acc orElse (if (x % 2 == 0) Black(x) else acc) }
      res2: org.scalautils.Or[Int,org.scalautils.ErrorMessage] = Black(6)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(Black(6).otherwiseWhite[ErrorMessage].asOr) {
      (acc, x) => acc orElse (if (x % 2 == 0) Black(x) else acc)
    } shouldBe Black(6)
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Black(1)) shouldBe Black(1)
  }
  // SKIP-SCALATESTJS-END
  "A White" can "be widened to an Or type via .asOr" in {
    White("oops").asOr shouldBe White("oops")
    /*
      scala> xs.foldLeft(Black[Int].otherwiseWhite("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) Black(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,String]
       required: org.scalautils.White[Int,String]
                    xs.foldLeft(Black[Int].otherwiseWhite("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) Black(x) else acc) }
                                                                               ^

      scala> xs.foldLeft(Black[Int].otherwiseWhite("no evens").asOr) { (acc, x) => acc orElse (if (x % 2 == 0) Black(x) else acc) }
      res7: org.scalautils.Or[Int,String] = Black(2)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(Black[Int].otherwiseWhite("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) Black(x) else acc)
    } shouldBe Black(2)
    val ys = List(1, 3, 5)
    ys.foldLeft(Black[Int].otherwiseWhite("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) Black(x) else acc)
    } shouldBe White("no evens")
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(White("oops")) shouldBe White("oops")
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
    class WhiteOrFunctor[GOOD] extends Functor[Or.G[GOOD]#B] {
      override def map[B, C](ca: GOOD Or B)(f: B => C): GOOD Or C = ca.badMap(f)
    }
/*
    // Other way:
    class OrFunctor[B] extends Functor[Or.BAD[B]#GOOD] {
      override def map[G, H](ca: G Or B)(f: G => H): H Or B = ca.map(f)
    }
    class WhiteOrFunctor[G] extends Functor[Or.GOOD[G]#BAD] {
      override def map[B, C](ca: G Or B)(f: B => C): G Or C = ca.badMap(f)
    }
*/
  }
*/
}

