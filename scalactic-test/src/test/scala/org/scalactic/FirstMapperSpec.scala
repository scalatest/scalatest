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

class FirstMapperSpec extends UnitSpec with Accumulation with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "An FirstMapper" can "be either First or Second" in {
    First(7).firstMapper.isFirst shouldBe true
    First(7).firstMapper.isSecond shouldBe false
    Second("oops").firstMapper.isSecond shouldBe true
    Second("oops").firstMapper.isFirst shouldBe false
    First(7).firstMapper.value shouldBe an [First[_]]
    Second("oops").firstMapper.value shouldBe an [Second[_]]
  }
  it can "be used with map" in {
    First(8).firstMapper map (_ + 1) should equal (First(9).firstMapper)
    First[Int].elseSecond("eight").firstMapper map (_ + 1) should equal (Second("eight").firstMapper)
  }
  it can "be used with recover" in {
    First(8).elseSecond[Throwable].firstMapper recover {
      case iae: IllegalArgumentException => 9
    } should equal (First(8).firstMapper)
    First[Int].elseSecond(new IllegalArgumentException).firstMapper recover {
      case iae: IllegalArgumentException => 9
    } should equal (First(9).firstMapper)
  }
  it can "be used with recoverWith" in {
    First(8).elseSecond[Throwable].firstMapper recoverWith {
      case iae: IllegalArgumentException => First(9).firstMapper
    } should equal (First(8).firstMapper)
    First[Int].elseSecond(new IllegalArgumentException).firstMapper recoverWith {
      case iae: IllegalArgumentException => First(9).firstMapper
    } should equal (First(9).firstMapper)
  }
  it can "be used with foreach" in {
    var vCount = 0
    var eCount = 0
    First(8).firstMapper foreach { vCount += _ }
    vCount should equal (8)
    First[Int].elseSecond("eight").firstMapper foreach { eCount += _ }
    eCount should equal (0)
  }
  it can "be used with flatMap" in {
    First(8).elseSecond[String].firstMapper flatMap ((x: Int) => First(x + 1).firstMapper) should equal (First(9).firstMapper)
    First[Int].elseSecond("eight").firstMapper flatMap ((x: Int) => First(x + 1).firstMapper) should equal (Second("eight").firstMapper)
  }
  it can "be used with filter" in {
    First(12).firstMapper.filter(isRound) shouldBe Second("12 was not a round number").firstMapper
    First(10).firstMapper.filter(isRound) shouldBe First(10).firstMapper
    First[Int].elseSecond(12).firstMapper.filter(isRound) shouldBe Second(12).firstMapper
    (for (i <- First(10).firstMapper if isRound(i)) yield i) shouldBe First(10).firstMapper
    (for (i <- First(12).firstMapper if isRound(i)) yield i) shouldBe Second("12 was not a round number").firstMapper
    (for (i <- First(12).firstMapper if isRound(i)) yield i) shouldBe Second("12 was not a round number").firstMapper
    (for (i <- First(30).firstMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe First(30).firstMapper
    (for (i <- First(10).firstMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Second("10 was not divisible by 3").firstMapper
    (for (i <- First(3).firstMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Second("3 was not a round number").firstMapper
    (for (i <- First(2).firstMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Second("2 was not a round number").firstMapper
  }
  it can "be used with exists" in {
    First(12).firstMapper.exists(_ == 12) shouldBe true
    First(12).firstMapper.exists(_ == 13) shouldBe false
    First[Int].elseSecond(12).firstMapper.exists(_ == 12) shouldBe false
  }
  it can "be used with forall" in {
    First(12).firstMapper.forall(_ > 10) shouldBe true
    First(7).firstMapper.forall(_ > 10) shouldBe false
    First[Int].elseSecond(12).firstMapper.forall(_ > 10) shouldBe true
    First[Int].elseSecond(7).firstMapper.forall(_ > 10) shouldBe true
  }
  it can "be used with getOrElse, which takes a by-name" in {

    First(12).firstMapper.getOrElse(17) shouldBe 12
    First[Int].elseSecond(12).firstMapper.getOrElse(17) shouldBe 17

    var x = 16 // should not increment if First
    First(12).firstMapper getOrElse { x += 1; x } shouldBe 12
    x shouldBe 16
    First[Int].elseSecond(12).firstMapper getOrElse { x += 1; x } shouldBe 17
    x shouldBe 17
  }
  it can "be used with orElse, which takes a by-name" in {

    First(12).firstMapper.orElse(First(13).firstMapper) shouldBe First(12).firstMapper
    Second(12).firstMapper.orElse(First(13).firstMapper) shouldBe First(13).firstMapper

    First(12).firstMapper.orElse(Second(13).firstMapper) shouldBe First(12).firstMapper
    Second(12).firstMapper.orElse(Second(13).firstMapper) shouldBe Second(13).firstMapper

    var x = 16 // should not increment if First
    First(12).firstMapper orElse { x += 1; First(x).firstMapper } shouldBe First(12).firstMapper
    x shouldBe 16
    First[Int].elseSecond(12).firstMapper orElse { x += 1; First(x).firstMapper } shouldBe First(17).firstMapper
    x shouldBe 17

    var y = 16 // should not increment if First
    First(12).firstMapper orElse { y += 1; Second(y).firstMapper } shouldBe First(12).firstMapper
    y shouldBe 16
    First[Int].elseSecond(12).firstMapper orElse { y += 1; Second(y).firstMapper } shouldBe Second(17).firstMapper
    y shouldBe 17
  }
  it can "be used with toOption" in {
    First(12).firstMapper.toOption shouldBe Some(12)
    First[Int].elseSecond(12).firstMapper.toOption shouldBe None
  }
  it can "be used with toSeq" in {
    First(12).firstMapper.toSeq shouldEqual Seq(12)
    First[Int].elseSecond(12).firstMapper.toSeq shouldEqual Seq.empty
  }
// toArray, toBuffer, toIndexedSeq, toIterable, toIterator, toList, 
// toSeq, toStream, toTraversable, toVector
  it can "be used with toEither" in {
    First(12).firstMapper.toEither shouldBe Right(12)
    Second(12).firstMapper.toEither shouldBe Left(12)
  }
  it can "be used with toOr" in {
    First(12).firstMapper.toOr shouldBe Good(12)
    Second(12).firstMapper.toOr shouldBe Bad(12)
  }
  it can "be used with toTry, if the error type is a subtype of Throwable" in {
    First(12).elseSecond[Throwable].firstMapper.toTry shouldBe Success(12)
    First(12).elseSecond[RuntimeException].firstMapper.toTry shouldBe Success(12)
    val ex = new RuntimeException("oops")
    First[Int].elseSecond(ex).firstMapper.toTry shouldBe Failure(ex)
    First[Int].elseSecond(ex).firstMapper.toTry shouldBe Failure(ex)
    "First[Int].elseSecond(12).firstMapper.toTry" shouldNot typeCheck
  }
  it can "be used with swap" in {
    First(12).elseSecond[String].firstMapper.swap should === (First[String].elseSecond(12).firstMapper)
    First[Int].elseSecond("hi").firstMapper.swap should === (First("hi").elseSecond[Int].firstMapper)
  }
  it can "be used with transform" in {
    First(12).elseSecond[String].firstMapper.transform((i: Int) => First(i + 1).firstMapper, (s: String) => Second(s.toUpperCase).firstMapper) should === (First(13).firstMapper)
    First[Int].elseSecond("hi").firstMapper.transform((i: Int) => First(i + 1).firstMapper, (s: String) => Second(s.toUpperCase).firstMapper) should === (Second("HI").firstMapper)
    First(12).elseSecond[String].firstMapper.transform((i: Int) => Second(i + 1).firstMapper, (s: String) => First(s.toUpperCase).firstMapper) should === (Second(13).firstMapper)
    First[Int].elseSecond("hi").firstMapper.transform((i: Int) => Second(i + 1).firstMapper, (s: String) => First(s.toUpperCase).firstMapper) should === (First("HI").firstMapper)
  }
  it can "be folded with fold" in {
    First(3).elseSecond[String].firstMapper.fold(_ + 1, _.length) shouldBe 4
    First[Int].elseSecond("howdy").firstMapper.fold(_ + 1, _.length) shouldBe 5
  }
/*
  it can "be used with zip" in {
    First(12).elseSecond[Every[ErrorMessage]] zip First("hi").elseSecond[Every[ErrorMessage]] should === (First((12, "hi")).elseSecond[Every[ErrorMessage]])
    First[Int].elseSecond(One("so")) zip First[String].elseSecond(One("ho")) should === (Second(Many("so", "ho")))
    (First(12): Int Or Every[ErrorMessage]) zip Second[Every[ErrorMessage]](One("ho")) should === (Second(One("ho")))
    Second[Every[ErrorMessage]](One("so")) zip First[String]("hi") should === (Second(One("so")))

    First[Int](12) zip First[String]("hi") should === (First[(Int, String)]((12, "hi")))
    Second[One[ErrorMessage]](One("so")) zip Second[Every[ErrorMessage]](One("ho")) should === (Second(Many("so", "ho")))
    First[Int](12) zip Second[Every[ErrorMessage]](One("ho")) should === (Second(One("ho")))
    Second[One[ErrorMessage]](One("so")) zip First[String]("hi") should === (Second(One("so")))

    First[Int](12) zip First[String]("hi") should === (First[(Int, String)]((12, "hi")))
    Second[Every[ErrorMessage]](One("so")) zip Second[One[ErrorMessage]](One("ho")) should === (Second(Many("so", "ho")))
    First[Int](12) zip Second[One[ErrorMessage]](One("ho")) should === (Second(One("ho")))
    Second[Every[ErrorMessage]](One("so")) zip First[String]("hi") should === (Second(One("so")))

    // Works when right hand side ERR type is a supertype of left hand side ERR type, because that's what Every's ++ does.
    First[Int].elseSecond(One("oops")) zip First[Int].elseSecond(One(-1: Any)) shouldBe Second(Many("oops", -1))
    First[Int].elseSecond(One("oops": Any)) zip First[Int].elseSecond(One(-1)) shouldBe Second(Many("oops", -1))
    First[Int].elseSecond(One("oops")) zip First[Int].elseSecond(One(-1)) shouldBe Second(Many("oops", -1))
    First[Int].elseSecond(One(-1)) zip First[Int].elseSecond(One("oops": Any)) shouldBe Second(Many(-1, "oops"))
  }

  it can "be used with when" in {
    First[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 100) Pass else Fail(i + " was not less than 100"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe First(12)
    First[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Second(One("12 was not less than 3"))
    First[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe Second(Many("12 was not less than 3", "12 was not odd"))
    First[Int](12).when(
      (i: Int) => if (i > 99) Pass else Fail(i + " was not greater than 99"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe Second(Many("12 was not greater than 99", "12 was not less than 3", "12 was not odd"))
    First[Int].elseSecond[Every[ErrorMessage]](One("original error")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Second(One("original error"))
    First[Int].elseSecond[Every[ErrorMessage]](Many("original error 1", "original error 2")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Second(Many("original error 1", "original error 2"))
    First("hi").elseSecond[Every[Int]].when((i: String) => Fail(2.0)) shouldBe Second(One(2.0))

    (for (i <- First(10) when isRound) yield i) shouldBe First(10)
    (for (i <- First(12) when isRound) yield i) shouldBe Second(One("12 was not a round number"))
    (for (i <- First(12) when isRound) yield i) shouldBe Second(One("12 was not a round number"))
    (for (i <- First(30) when (isRound, isDivBy3)) yield i) shouldBe First(30)
    (for (i <- First(10) when (isRound, isDivBy3)) yield i) shouldBe Second(One("10 was not divisible by 3"))
    (for (i <- First(3) when (isRound, isDivBy3)) yield i) shouldBe Second(One("3 was not a round number"))
    (for (i <- First(2) when (isRound, isDivBy3)) yield i) shouldBe Second(Many("2 was not a round number", "2 was not divisible by 3"))
  }
  it can "be created with the attempt helper method" in {
    attempt { 2 / 1 } should === (First(2))
    val divByZero = attempt { throw new ArithmeticException("/ by zero") }
    divByZero.isSecond shouldBe true
    divByZero match {
      case Second(ex) =>
        ex shouldBe an [ArithmeticException]
        ex.getMessage shouldBe "/ by zero"
      case _ => fail()
    }
    divByZero.isSecond shouldBe true
    intercept[VirtualMachineError] {
      attempt { throw new VirtualMachineError {} }
    }
  }
  it can "be created from a Try via the from(Try) factory method" in {
    Or.from(Success(12)) shouldBe First(12)
    Or.from(Success(12): Try[Int]) shouldBe First(12)
    val ex = new Exception("oops")
    Or.from(Failure(ex)) shouldBe Second(ex)
    Or.from(Failure(ex): Try[Int]) shouldBe Second(ex)
  }
  it can "be created with the from(Either) factory method" in {
    Or.from(Right(12)) shouldBe First(12)
    Or.from(Right(12): Either[String, Int]) shouldBe First(12)
    val ex = new Exception("oops")
    Or.from(Left(ex)) shouldBe Second(ex)
    Or.from(Left("oops")) shouldBe Second("oops")
    Or.from(Left("oops"): Either[String, String]) shouldBe Second("oops")
  }
  it can "be created with the from(Option, SecondIfNone) factory method" in {
    Or.from(Some(12), "won't be used") shouldBe First(12)
    Or.from(Some(12): Option[Int], "won't be used") shouldBe First(12)
    val ex = new Exception("oops")
    Or.from(None, ex) shouldBe Second(ex)
    Or.from(None, "oops") shouldBe Second("oops")
    Or.from(None: Option[String], "oops") shouldBe Second("oops")
  }

  it can "be validated with collection.validatedBy" in {

    def isOdd(i: Int): Int Or One[ErrorMessage] =
      if (i % 2 == 1) First(i) else Second(One(s"$i was not odd"))

    // List
    List.empty[Int].validatedBy(isOdd) shouldBe First(List.empty[Int])

    List(3).validatedBy(isOdd) shouldBe First(List(3))
    List(4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    List(3, 5).validatedBy(isOdd) shouldBe First(List(3, 5))
    List(4, 6).validatedBy(isOdd) shouldBe Second(Every("4 was not odd", "6 was not odd"))
    List(3, 4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    List(4, 3).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    List(3, 5, 7).validatedBy(isOdd) shouldBe First(List(3, 5, 7))

    // Vector
    Vector.empty[Int].validatedBy(isOdd) shouldBe First(Vector.empty[Int])

    Vector(3).validatedBy(isOdd) shouldBe First(Vector(3))
    Vector(4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    Vector(3, 5).validatedBy(isOdd) shouldBe First(Vector(3, 5))
    Vector(4, 6).validatedBy(isOdd) shouldBe Second(Every("4 was not odd", "6 was not odd"))
    Vector(3, 4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    Vector(4, 3).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    Vector(3, 5, 7).validatedBy(isOdd) shouldBe First(Vector(3, 5, 7))

    // Iterator
    List.empty[Int].iterator.validatedBy(isOdd).map(_.toStream) shouldBe First(List.empty[Int].iterator).map(_.toStream)

    List(3).iterator.validatedBy(isOdd).map(_.toStream) shouldBe First(List(3).iterator).map(_.toStream)
    List(4).iterator.validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    List(3, 5).iterator.validatedBy(isOdd).map(_.toStream) shouldBe First(List(3, 5).iterator).map(_.toStream)
    List(4, 6).iterator.validatedBy(isOdd) shouldBe Second(Every("4 was not odd", "6 was not odd"))
    List(3, 4).iterator.validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    List(4, 3).iterator.validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    List(3, 5, 7).iterator.validatedBy(isOdd).map(_.toStream) shouldBe First(List(3, 5, 7).iterator).map(_.toStream)

    // Set
    Set.empty[Int].validatedBy(isOdd) shouldBe First(Set.empty[Int])

    Set(3).validatedBy(isOdd) shouldBe First(Set(3))
    Set(4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    Set(3, 5).validatedBy(isOdd) shouldBe First(Set(3, 5))
    Set(4, 6).validatedBy(isOdd) shouldBe Second(Every("4 was not odd", "6 was not odd"))
    Set(3, 4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    Set(4, 3).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    Set(3, 5, 7).validatedBy(isOdd) shouldBe First(Set(3, 5, 7))

    Set.empty[Int].validatedBy(isOdd) shouldBe First(Set.empty[Int])

    // Every
    One(3).validatedBy(isOdd) shouldBe First(One(3))
    Every(3).validatedBy(isOdd) shouldBe First(One(3))
    One(4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    Every(4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    Many(3, 5).validatedBy(isOdd) shouldBe First(Many(3, 5))
    Every(3, 5).validatedBy(isOdd) shouldBe First(Many(3, 5))
    Many(4, 6).validatedBy(isOdd) shouldBe Second(Every("4 was not odd", "6 was not odd"))
    Every(4, 6).validatedBy(isOdd) shouldBe Second(Every("4 was not odd", "6 was not odd"))
    Many(3, 4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    Every(3, 4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    Many(4, 3).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
    Every(4, 3).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))

    Many(3, 5, 7).validatedBy(isOdd) shouldBe First(Every(3, 5, 7))
    Every(3, 5, 7).validatedBy(isOdd) shouldBe First(Many(3, 5, 7))

    // Option
    Some(3).validatedBy(isOdd) shouldBe First(Some(3))
    (None: Option[Int]).validatedBy(isOdd) shouldBe First(None)
    Some(4).validatedBy(isOdd) shouldBe Second(One("4 was not odd"))
  }

  it can "be validated with collection.validatedBy when the map goes to a different type" in {
    def parseAge(input: String): Int Or One[ErrorMessage] = {
      try {
        val age = input.trim.toInt
        if (age >= 0) First(age) else Second(One(s""""${age}" is not a valid age"""))
      }
      catch {
        case _: NumberFormatException => Second(One(s""""${input}" is not a valid integer"""))
      }
    }

    Some("29").validatedBy(parseAge) shouldBe First(Some(29))
    Some("-30").validatedBy(parseAge) shouldBe Second(One("\"-30\" is not a valid age"))

    Every("29", "30", "31").validatedBy(parseAge) shouldBe First(Many(29, 30, 31))
    Every("29", "-30", "31").validatedBy(parseAge) shouldBe Second(One("\"-30\" is not a valid age"))
    Every("29", "-30", "-31").validatedBy(parseAge) shouldBe Second(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))

    List("29", "30", "31").validatedBy(parseAge) shouldBe First(List(29, 30, 31))
    List("29", "-30", "31").validatedBy(parseAge) shouldBe Second(One("\"-30\" is not a valid age"))
    List("29", "-30", "-31").validatedBy(parseAge) shouldBe Second(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))
  }

  it can "be combined with collection.combined" in {

    // List
    // Is this the right answer? Has to be, because couldn't come up with an error anyway.
    List.empty[Int Or Every[String]].combined shouldBe First(List.empty[Int])

    //  def combine[G, ELE, EVERY[b] <: Every[b], SEQ[s]](xs: SEQ[G Or EVERY[ELE]])(implicit seq: Sequenceable[SEQ]): SEQ[G] Or Every[ELE] =
    // G = Int, ELE = Nothing, SEQ = List
    List(First(3)).combined shouldBe First(List(3))
    List(Second(One("oops"))).combined shouldBe Second(One("oops"))

    List(First(3), First(4)).combined shouldBe First(List(3, 4))
    List(Second(One("darn")), Second(One("oops"))).combined shouldBe Second(Every("darn", "oops"))
    List(First(3), Second(One("oops"))).combined shouldBe Second(One("oops"))
    List(Second(One("oops")), First(3)).combined shouldBe Second(One("oops"))

    List(First(3), First(4), First(5)).combined shouldBe First(List(3, 4, 5))

    // Vector
    Vector.empty[Int Or Every[String]].combined shouldBe First(Vector.empty[Int])

    Vector(First(3)).combined shouldBe First(Vector(3))
    Vector(Second(One("oops"))).combined shouldBe Second(One("oops"))

    Vector(First(3), First(4)).combined shouldBe First(Vector(3, 4))
    Vector(Second(One("darn")), Second(One("oops"))).combined shouldBe Second(Every("darn", "oops"))
    Vector(First(3), Second(One("oops"))).combined shouldBe Second(One("oops"))
    Vector(Second(One("oops")), First(3)).combined shouldBe Second(One("oops"))

    Vector(First(3), First(4), First(5)).combined shouldBe First(Vector(3, 4, 5))

    // Do the same thing with Iterator
    (List.empty[Int Or Every[String]].iterator).combined.map(_.toStream) shouldEqual (First(List.empty[Int].iterator).map(_.toStream))

    List(First(3)).iterator.combined.map(_.toStream) shouldEqual (First(List(3).iterator).map(_.toStream))
    List(Second(One("oops"))).iterator.combined shouldEqual (Second(One("oops")))

    List(First(3), First(4)).iterator.combined.map(_.toStream) shouldEqual (First(List(3, 4).iterator).map(_.toStream))
    List(Second(One("darn")), Second(One("oops"))).iterator.combined shouldEqual (Second(Every("darn", "oops")))
    List(First(3), Second(One("oops"))).iterator.combined shouldEqual (Second(One("oops")))
    List(Second(One("oops")), First(3)).iterator.combined shouldEqual (Second(One("oops")))

    List(First(3), First(4), First(5)).iterator.combined.map(_.toStream) shouldEqual (First(List(3, 4, 5).iterator).map(_.toStream))

    // Set
    Set.empty[Int Or Every[String]].combined shouldBe First(Set.empty[Int])
    Set(First[Int](3), Second[Every[String]](Every("oops"))).asInstanceOf[Set[Int Or Every[String]]].combined shouldBe Second(One("oops"))
    Set(First[Int](3), Second[Every[String]](Every("oops"))).combined shouldBe Second(One("oops"))

    Set(First(3)).combined shouldBe First(Set(3))
    Set(Second(One("oops"))).combined shouldBe Second(One("oops"))

    Set(First(3), First(4)).combined shouldBe First(Set(3, 4))
    Set(Second(One("darn")), Second(One("oops"))).combined shouldBe Second(Every("darn", "oops"))
    Set(First(3), Second(One("oops"))).combined shouldBe Second(One("oops"))
    Set(Second(One("oops")), First(3)).combined shouldBe Second(One("oops"))

    Set(First(3), First(4), First(5)).combined shouldBe First(Set(3, 4, 5))

    // Every
    Every(First(3).elseSecond[Every[String]], First[Int].elseSecond(Every("oops"))).combined shouldBe Second(One("oops"))

    Every(First(3)).combined shouldBe First(Every(3))
    One(First(3)).combined shouldBe First(Every(3))
    Every(Second(One("oops"))).combined shouldBe Second(One("oops"))
    One(Second(One("oops"))).combined shouldBe Second(One("oops"))

    Every(First(3), First(4)).combined shouldBe First(Every(3, 4))
    Many(First(3), First(4)).combined shouldBe First(Every(3, 4))
    Every(Second(One("darn")), Second(One("oops"))).combined shouldBe Second(Every("darn", "oops"))
    Many(Second(One("darn")), Second(One("oops"))).combined shouldBe Second(Every("darn", "oops"))
    Every(First(3), Second(One("oops"))).combined shouldBe Second(One("oops"))
    Every(Second(One("oops")), First(3)).combined shouldBe Second(One("oops"))

    Every(First(3), First(4), First(5)).combined shouldBe First(Every(3, 4, 5))

    // Option
    Some(First(3)).combined shouldBe First(Some(3))
    (None: Option[Int Or Every[ErrorMessage]]).combined shouldBe First(None)
    Some(Second(One("oops"))).combined shouldBe Second(One("oops"))
    Some(Second(Many("oops", "idoops"))).combined shouldBe Second(Many("oops", "idoops"))
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Or.from(Success(12)) shouldBe First(12))
    val ex = new Exception("oops")
    serializeRoundtrip(Or.from(Failure(ex)) shouldBe Second(ex))
  }
  // SKIP-SCALATESTJS-END
  "A First" can "be widened to an Or type via .asOr" in {
    First(1).asOr shouldBe First(1)
    /*
      scala> xs.foldLeft(First(6).elseSecond[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) First(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,org.scalautils.ErrorMessage]
       required: org.scalautils.First[Int,org.scalautils.ErrorMessage]
                    xs.foldLeft(First(6).elseSecond[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) First(x) else acc) }
                                                                         ^

      scala> xs.foldLeft(First(6).elseSecond[ErrorMessage].asOr) { (acc, x) => acc orElse (if (x % 2 == 0) First(x) else acc) }
      res2: org.scalautils.Or[Int,org.scalautils.ErrorMessage] = First(6)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(First(6).elseSecond[ErrorMessage].asOr) {
      (acc, x) => acc orElse (if (x % 2 == 0) First(x) else acc)
    } shouldBe First(6)
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(First(1)) shouldBe First(1)
  }
  // SKIP-SCALATESTJS-END
  "A Second" can "be widened to an Or type via .asOr" in {
    Second("oops").asOr shouldBe Second("oops")
    /*
      scala> xs.foldLeft(First[Int].elseSecond("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) First(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,String]
       required: org.scalautils.Second[Int,String]
                    xs.foldLeft(First[Int].elseSecond("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) First(x) else acc) }
                                                                               ^

      scala> xs.foldLeft(First[Int].elseSecond("no evens").asOr) { (acc, x) => acc orElse (if (x % 2 == 0) First(x) else acc) }
      res7: org.scalautils.Or[Int,String] = First(2)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(First[Int].elseSecond("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) First(x) else acc)
    } shouldBe First(2)
    val ys = List(1, 3, 5)
    ys.foldLeft(First[Int].elseSecond("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) First(x) else acc)
    } shouldBe Second("no evens")
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Second("oops")) shouldBe Second("oops")
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
    class SecondOrFunctor[GOOD] extends Functor[Or.G[GOOD]#B] {
      override def map[B, C](ca: GOOD Or B)(f: B => C): GOOD Or C = ca.badMap(f)
    }
/*
    // Other way:
    class OrFunctor[B] extends Functor[Or.BAD[B]#GOOD] {
      override def map[G, H](ca: G Or B)(f: G => H): H Or B = ca.map(f)
    }
    class SecondOrFunctor[G] extends Functor[Or.GOOD[G]#BAD] {
      override def map[B, C](ca: G Or B)(f: B => C): G Or C = ca.badMap(f)
    }
*/
  }
*/
}

