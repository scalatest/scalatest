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

class StarMapperSpec extends UnitSpec with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "An StarMapper" can "be either Port or Star" in {
    Port(7).starMapper.isPort shouldBe true
    Port(7).starMapper.isStar shouldBe false
    Star("oops").starMapper.isStar shouldBe true
    Star("oops").starMapper.isPort shouldBe false
    Port(7).starMapper.value shouldBe an [Port[_]]
    Star("oops").starMapper.value shouldBe an [Star[_]]
  }
  it can "be used with map" in {
    Port(8).elseStar[ErrorMessage].starMapper map (_.toUpperCase) should equal (Port(8).starMapper)
    Port[Int].elseStar("eight").starMapper map (_.toUpperCase) should equal (Star("EIGHT").starMapper)
  }
  it can "be used with recover" in {
    Port[Throwable].elseStar(8).starMapper recover {
      case iae: IllegalArgumentException => 9
    } should equal (Star(8).starMapper)
    Port(new IllegalArgumentException).elseStar[Int].starMapper recover {
      case iae: IllegalArgumentException => 9
    } should equal (Star(9).starMapper)
  }
  it can "be used with recoverWith" in {
    Port[Throwable].elseStar(8).starMapper recoverWith {
      case iae: IllegalArgumentException => Star(9).starMapper
    } should equal (Star(8).starMapper)
    Port(new IllegalArgumentException).elseStar[Int].starMapper recoverWith {
      case iae: IllegalArgumentException => Star(9).starMapper
    } should equal (Star(9).starMapper)
  }
  it can "be used with foreach" in {
    var vCount = 0
    var eCount = 0
    Star(8).starMapper foreach { vCount += _ }
    vCount should equal (8)
    Port("eight").elseStar[Int].starMapper foreach { eCount += _ }
    eCount should equal (0)
  }
  it can "be used with flatMap" in {
    Port[String].elseStar(8).starMapper flatMap ((x: Int) => Star(x + 1).starMapper) should equal (Star(9).starMapper)
    Port("eight").elseStar[Int].starMapper flatMap ((x: Int) => Star(x + 1).starMapper) should equal (Port("eight").starMapper)
  }
  it can "be used with filter" in {
    Star(12).starMapper.filter(isRound) shouldBe Port("12 was not a round number").starMapper
    Star(10).starMapper.filter(isRound) shouldBe Star(10).starMapper
    Port(12).elseStar[Int].starMapper.filter(isRound) shouldBe Port(12).starMapper
    (for (i <- Star(10).starMapper if isRound(i)) yield i) shouldBe Star(10).starMapper
    (for (i <- Star(12).starMapper if isRound(i)) yield i) shouldBe Port("12 was not a round number").starMapper
    (for (i <- Star(12).starMapper if isRound(i)) yield i) shouldBe Port("12 was not a round number").starMapper
    (for (i <- Star(30).starMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Star(30).starMapper
    (for (i <- Star(10).starMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Port("10 was not divisible by 3").starMapper
    (for (i <- Star(3).starMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Port("3 was not a round number").starMapper
    (for (i <- Star(2).starMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Port("2 was not a round number").starMapper
  }
  it can "be used with exists" in {
    Star(12).starMapper.exists(_ == 12) shouldBe true
    Star(12).starMapper.exists(_ == 13) shouldBe false
    Port(12).elseStar[Int].starMapper.exists(_ == 12) shouldBe false
  }
  it can "be used with forall" in {
    Star(12).starMapper.forall(_ > 10) shouldBe true
    Star(7).starMapper.forall(_ > 10) shouldBe false
    Port(12).elseStar[Int].starMapper.forall(_ > 10) shouldBe true
    Port(7).elseStar[Int].starMapper.forall(_ > 10) shouldBe true
  }
  it can "be used with getOrElse, which takes a by-name" in {

    Star(12).starMapper.getOrElse(17) shouldBe 12
    Port(12).elseStar[Int].starMapper.getOrElse(17) shouldBe 17

    var x = 16 // should not increment if Port
    Star(12).starMapper getOrElse { x += 1; x } shouldBe 12
    x shouldBe 16
    Port(12).elseStar[Int].starMapper getOrElse { x += 1; x } shouldBe 17
    x shouldBe 17
  }
  it can "be used with orElse, which takes a by-name" in {

    Star(12).starMapper.orElse(Star(13).starMapper) shouldBe Star(12).starMapper
    Port(12).starMapper.orElse(Star(13).starMapper) shouldBe Star(13).starMapper

    Star(12).starMapper.orElse(Port(13).starMapper) shouldBe Star(12).starMapper
    Port(12).starMapper.orElse(Port(13).starMapper) shouldBe Port(13).starMapper

    var x = 16 // should not increment if Star
    Star(12).starMapper orElse { x += 1; Star(x).starMapper } shouldBe Star(12).starMapper
    x shouldBe 16
    Port(12).elseStar[Int].starMapper orElse { x += 1; Star(x).starMapper } shouldBe Star(17).starMapper
    x shouldBe 17

    var y = 16 // should not increment if Star
    Star(12).starMapper orElse { y += 1; Port(y).starMapper } shouldBe Star(12).starMapper
    y shouldBe 16
    Port(12).elseStar[Int].starMapper orElse { y += 1; Port(y).starMapper } shouldBe Port(17).starMapper
    y shouldBe 17
  }
  it can "be used with toOption" in {
    Star(12).starMapper.toOption shouldBe Some(12)
    Port(12).elseStar[Int].starMapper.toOption shouldBe None
  }
  it can "be used with toSeq" in {
    Star(12).starMapper.toSeq shouldEqual Seq(12)
    Port(12).elseStar[Int].starMapper.toSeq shouldEqual Seq.empty
  }
// toArray, toBuffer, toIndexedSeq, toIterable, toIterator, toList, 
// toSeq, toStream, toTraversable, toVector
  it can "be used with toEither" in {
    Star(12).starMapper.toEither shouldBe Right(12)
    Port(12).starMapper.toEither shouldBe Left(12)
  }
  it can "be used with toOr" in {
    Star(12).starMapper.toOr shouldBe Good(12)
    Port(12).starMapper.toOr shouldBe Bad(12)
  }
  it can "be used with toTry, if the error type is a subtype of Throwable" in {
    Port[Throwable].elseStar(12).starMapper.toTry shouldBe Success(12)
    Port[RuntimeException].elseStar(12).starMapper.toTry shouldBe Success(12)
    val ex = new RuntimeException("oops")
    Port(ex).elseStar[Int].starMapper.toTry shouldBe Failure(ex)
    Port(ex).elseStar[Int].starMapper.toTry shouldBe Failure(ex)
    "Port(12).elseStar[Int].starMapper.toTry" shouldNot typeCheck
  }
  it can "be used with swap" in {
    Port(12).elseStar[String].starMapper.swap should === (Port[String].elseStar(12).starMapper)
    Port[Int].elseStar("hi").starMapper.swap should === (Port("hi").elseStar[Int].starMapper)
  }
  it can "be used with transform" in {
    Port[String].elseStar(12).starMapper.transform((s: String) => Star(s.toUpperCase).starMapper, (i: Int) => Port(i + 1).starMapper) should === (Port(13).starMapper)
    Port("hi").elseStar[Int].starMapper.transform( (s: String) => Star(s.toUpperCase).starMapper, (i: Int) => Port(i + 1).starMapper) should === (Star("HI").starMapper)
    Port[String].elseStar(12).starMapper.transform((s: String) => Port(s.toUpperCase).starMapper, (i: Int) => Star(i + 1).starMapper) should === (Star(13).starMapper)
    Port("hi").elseStar[Int].starMapper.transform((s: String) => Port(s.toUpperCase).starMapper, (i: Int) => Star(i + 1).starMapper) should === (Port("HI").starMapper)
  }
  it can "be folded with fold" in {
    Port[String].elseStar(3).starMapper.fold(_.length, _ + 1) shouldBe 4
    Port("howdy").elseStar[Int].starMapper.fold(_.length ,_ + 1) shouldBe 5
  }
/*
  it can "be used with zip" in {
    Port(12).elseStar[Every[ErrorMessage]] zip Port("hi").elseStar[Every[ErrorMessage]] should === (Port((12, "hi")).elseStar[Every[ErrorMessage]])
    Port[Int].elseStar(One("so")) zip Port[String].elseStar(One("ho")) should === (Star(Many("so", "ho")))
    (Port(12): Int Or Every[ErrorMessage]) zip Star[Every[ErrorMessage]](One("ho")) should === (Star(One("ho")))
    Star[Every[ErrorMessage]](One("so")) zip Port[String]("hi") should === (Star(One("so")))

    Port[Int](12) zip Port[String]("hi") should === (Port[(Int, String)]((12, "hi")))
    Star[One[ErrorMessage]](One("so")) zip Star[Every[ErrorMessage]](One("ho")) should === (Star(Many("so", "ho")))
    Port[Int](12) zip Star[Every[ErrorMessage]](One("ho")) should === (Star(One("ho")))
    Star[One[ErrorMessage]](One("so")) zip Port[String]("hi") should === (Star(One("so")))

    Port[Int](12) zip Port[String]("hi") should === (Port[(Int, String)]((12, "hi")))
    Star[Every[ErrorMessage]](One("so")) zip Star[One[ErrorMessage]](One("ho")) should === (Star(Many("so", "ho")))
    Port[Int](12) zip Star[One[ErrorMessage]](One("ho")) should === (Star(One("ho")))
    Star[Every[ErrorMessage]](One("so")) zip Port[String]("hi") should === (Star(One("so")))

    // Works when right hand side ERR type is a supertype of left hand side ERR type, because that's what Every's ++ does.
    Port[Int].elseStar(One("oops")) zip Port[Int].elseStar(One(-1: Any)) shouldBe Star(Many("oops", -1))
    Port[Int].elseStar(One("oops": Any)) zip Port[Int].elseStar(One(-1)) shouldBe Star(Many("oops", -1))
    Port[Int].elseStar(One("oops")) zip Port[Int].elseStar(One(-1)) shouldBe Star(Many("oops", -1))
    Port[Int].elseStar(One(-1)) zip Port[Int].elseStar(One("oops": Any)) shouldBe Star(Many(-1, "oops"))
  }

  it can "be used with when" in {
    Port[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 100) Pass else Fail(i + " was not less than 100"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Port(12)
    Port[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Star(One("12 was not less than 3"))
    Port[Int](12).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe Star(Many("12 was not less than 3", "12 was not odd"))
    Port[Int](12).when(
      (i: Int) => if (i > 99) Pass else Fail(i + " was not greater than 99"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 1) Pass else Fail(i + " was not odd")
    ) shouldBe Star(Many("12 was not greater than 99", "12 was not less than 3", "12 was not odd"))
    Port[Int].elseStar[Every[ErrorMessage]](One("original error")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Star(One("original error"))
    Port[Int].elseStar[Every[ErrorMessage]](Many("original error 1", "original error 2")).when(
      (i: Int) => if (i > 0) Pass else Fail(i + " was not greater than 0"),
      (i: Int) => if (i < 3) Pass else Fail(i + " was not less than 3"),
      (i: Int) => if (i % 2 == 0) Pass else Fail(i + " was not even")
    ) shouldBe Star(Many("original error 1", "original error 2"))
    Port("hi").elseStar[Every[Int]].when((i: String) => Fail(2.0)) shouldBe Star(One(2.0))

    (for (i <- Port(10) when isRound) yield i) shouldBe Port(10)
    (for (i <- Port(12) when isRound) yield i) shouldBe Star(One("12 was not a round number"))
    (for (i <- Port(12) when isRound) yield i) shouldBe Star(One("12 was not a round number"))
    (for (i <- Port(30) when (isRound, isDivBy3)) yield i) shouldBe Port(30)
    (for (i <- Port(10) when (isRound, isDivBy3)) yield i) shouldBe Star(One("10 was not divisible by 3"))
    (for (i <- Port(3) when (isRound, isDivBy3)) yield i) shouldBe Star(One("3 was not a round number"))
    (for (i <- Port(2) when (isRound, isDivBy3)) yield i) shouldBe Star(Many("2 was not a round number", "2 was not divisible by 3"))
  }
  it can "be created with the attempt helper method" in {
    attempt { 2 / 1 } should === (Port(2))
    val divByZero = attempt { throw new ArithmeticException("/ by zero") }
    divByZero.isStar shouldBe true
    divByZero match {
      case Star(ex) =>
        ex shouldBe an [ArithmeticException]
        ex.getMessage shouldBe "/ by zero"
      case _ => fail()
    }
    divByZero.isStar shouldBe true
    intercept[VirtualMachineError] {
      attempt { throw new VirtualMachineError {} }
    }
  }
  it can "be created from a Try via the from(Try) factory method" in {
    Or.from(Success(12)) shouldBe Port(12)
    Or.from(Success(12): Try[Int]) shouldBe Port(12)
    val ex = new Exception("oops")
    Or.from(Failure(ex)) shouldBe Star(ex)
    Or.from(Failure(ex): Try[Int]) shouldBe Star(ex)
  }
  it can "be created with the from(Either) factory method" in {
    Or.from(Right(12)) shouldBe Port(12)
    Or.from(Right(12): Either[String, Int]) shouldBe Port(12)
    val ex = new Exception("oops")
    Or.from(Left(ex)) shouldBe Star(ex)
    Or.from(Left("oops")) shouldBe Star("oops")
    Or.from(Left("oops"): Either[String, String]) shouldBe Star("oops")
  }
  it can "be created with the from(Option, StarIfNone) factory method" in {
    Or.from(Some(12), "won't be used") shouldBe Port(12)
    Or.from(Some(12): Option[Int], "won't be used") shouldBe Port(12)
    val ex = new Exception("oops")
    Or.from(None, ex) shouldBe Star(ex)
    Or.from(None, "oops") shouldBe Star("oops")
    Or.from(None: Option[String], "oops") shouldBe Star("oops")
  }

  it can "be validated with collection.validatedBy" in {

    def isOdd(i: Int): Int Or One[ErrorMessage] =
      if (i % 2 == 1) Port(i) else Star(One(s"$i was not odd"))

    // List
    List.empty[Int].validatedBy(isOdd) shouldBe Port(List.empty[Int])

    List(3).validatedBy(isOdd) shouldBe Port(List(3))
    List(4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    List(3, 5).validatedBy(isOdd) shouldBe Port(List(3, 5))
    List(4, 6).validatedBy(isOdd) shouldBe Star(Every("4 was not odd", "6 was not odd"))
    List(3, 4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    List(4, 3).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    List(3, 5, 7).validatedBy(isOdd) shouldBe Port(List(3, 5, 7))

    // Vector
    Vector.empty[Int].validatedBy(isOdd) shouldBe Port(Vector.empty[Int])

    Vector(3).validatedBy(isOdd) shouldBe Port(Vector(3))
    Vector(4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    Vector(3, 5).validatedBy(isOdd) shouldBe Port(Vector(3, 5))
    Vector(4, 6).validatedBy(isOdd) shouldBe Star(Every("4 was not odd", "6 was not odd"))
    Vector(3, 4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    Vector(4, 3).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    Vector(3, 5, 7).validatedBy(isOdd) shouldBe Port(Vector(3, 5, 7))

    // Iterator
    List.empty[Int].iterator.validatedBy(isOdd).map(_.toStream) shouldBe Port(List.empty[Int].iterator).map(_.toStream)

    List(3).iterator.validatedBy(isOdd).map(_.toStream) shouldBe Port(List(3).iterator).map(_.toStream)
    List(4).iterator.validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    List(3, 5).iterator.validatedBy(isOdd).map(_.toStream) shouldBe Port(List(3, 5).iterator).map(_.toStream)
    List(4, 6).iterator.validatedBy(isOdd) shouldBe Star(Every("4 was not odd", "6 was not odd"))
    List(3, 4).iterator.validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    List(4, 3).iterator.validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    List(3, 5, 7).iterator.validatedBy(isOdd).map(_.toStream) shouldBe Port(List(3, 5, 7).iterator).map(_.toStream)

    // Set
    Set.empty[Int].validatedBy(isOdd) shouldBe Port(Set.empty[Int])

    Set(3).validatedBy(isOdd) shouldBe Port(Set(3))
    Set(4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    Set(3, 5).validatedBy(isOdd) shouldBe Port(Set(3, 5))
    Set(4, 6).validatedBy(isOdd) shouldBe Star(Every("4 was not odd", "6 was not odd"))
    Set(3, 4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    Set(4, 3).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    Set(3, 5, 7).validatedBy(isOdd) shouldBe Port(Set(3, 5, 7))

    Set.empty[Int].validatedBy(isOdd) shouldBe Port(Set.empty[Int])

    // Every
    One(3).validatedBy(isOdd) shouldBe Port(One(3))
    Every(3).validatedBy(isOdd) shouldBe Port(One(3))
    One(4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    Every(4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    Many(3, 5).validatedBy(isOdd) shouldBe Port(Many(3, 5))
    Every(3, 5).validatedBy(isOdd) shouldBe Port(Many(3, 5))
    Many(4, 6).validatedBy(isOdd) shouldBe Star(Every("4 was not odd", "6 was not odd"))
    Every(4, 6).validatedBy(isOdd) shouldBe Star(Every("4 was not odd", "6 was not odd"))
    Many(3, 4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    Every(3, 4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    Many(4, 3).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
    Every(4, 3).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))

    Many(3, 5, 7).validatedBy(isOdd) shouldBe Port(Every(3, 5, 7))
    Every(3, 5, 7).validatedBy(isOdd) shouldBe Port(Many(3, 5, 7))

    // Option
    Some(3).validatedBy(isOdd) shouldBe Port(Some(3))
    (None: Option[Int]).validatedBy(isOdd) shouldBe Port(None)
    Some(4).validatedBy(isOdd) shouldBe Star(One("4 was not odd"))
  }

  it can "be validated with collection.validatedBy when the map goes to a different type" in {
    def parseAge(input: String): Int Or One[ErrorMessage] = {
      try {
        val age = input.trim.toInt
        if (age >= 0) Port(age) else Star(One(s""""${age}" is not a valid age"""))
      }
      catch {
        case _: NumberFormatException => Star(One(s""""${input}" is not a valid integer"""))
      }
    }

    Some("29").validatedBy(parseAge) shouldBe Port(Some(29))
    Some("-30").validatedBy(parseAge) shouldBe Star(One("\"-30\" is not a valid age"))

    Every("29", "30", "31").validatedBy(parseAge) shouldBe Port(Many(29, 30, 31))
    Every("29", "-30", "31").validatedBy(parseAge) shouldBe Star(One("\"-30\" is not a valid age"))
    Every("29", "-30", "-31").validatedBy(parseAge) shouldBe Star(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))

    List("29", "30", "31").validatedBy(parseAge) shouldBe Port(List(29, 30, 31))
    List("29", "-30", "31").validatedBy(parseAge) shouldBe Star(One("\"-30\" is not a valid age"))
    List("29", "-30", "-31").validatedBy(parseAge) shouldBe Star(Many("\"-30\" is not a valid age", "\"-31\" is not a valid age"))
  }

  it can "be combined with collection.combined" in {

    // List
    // Is this the right answer? Has to be, because couldn't come up with an error anyway.
    List.empty[Int Or Every[String]].combined shouldBe Port(List.empty[Int])

    //  def combine[G, ELE, EVERY[b] <: Every[b], SEQ[s]](xs: SEQ[G Or EVERY[ELE]])(implicit seq: Sequenceable[SEQ]): SEQ[G] Or Every[ELE] =
    // G = Int, ELE = Nothing, SEQ = List
    List(Port(3)).combined shouldBe Port(List(3))
    List(Star(One("oops"))).combined shouldBe Star(One("oops"))

    List(Port(3), Port(4)).combined shouldBe Port(List(3, 4))
    List(Star(One("darn")), Star(One("oops"))).combined shouldBe Star(Every("darn", "oops"))
    List(Port(3), Star(One("oops"))).combined shouldBe Star(One("oops"))
    List(Star(One("oops")), Port(3)).combined shouldBe Star(One("oops"))

    List(Port(3), Port(4), Port(5)).combined shouldBe Port(List(3, 4, 5))

    // Vector
    Vector.empty[Int Or Every[String]].combined shouldBe Port(Vector.empty[Int])

    Vector(Port(3)).combined shouldBe Port(Vector(3))
    Vector(Star(One("oops"))).combined shouldBe Star(One("oops"))

    Vector(Port(3), Port(4)).combined shouldBe Port(Vector(3, 4))
    Vector(Star(One("darn")), Star(One("oops"))).combined shouldBe Star(Every("darn", "oops"))
    Vector(Port(3), Star(One("oops"))).combined shouldBe Star(One("oops"))
    Vector(Star(One("oops")), Port(3)).combined shouldBe Star(One("oops"))

    Vector(Port(3), Port(4), Port(5)).combined shouldBe Port(Vector(3, 4, 5))

    // Do the same thing with Iterator
    (List.empty[Int Or Every[String]].iterator).combined.map(_.toStream) shouldEqual (Port(List.empty[Int].iterator).map(_.toStream))

    List(Port(3)).iterator.combined.map(_.toStream) shouldEqual (Port(List(3).iterator).map(_.toStream))
    List(Star(One("oops"))).iterator.combined shouldEqual (Star(One("oops")))

    List(Port(3), Port(4)).iterator.combined.map(_.toStream) shouldEqual (Port(List(3, 4).iterator).map(_.toStream))
    List(Star(One("darn")), Star(One("oops"))).iterator.combined shouldEqual (Star(Every("darn", "oops")))
    List(Port(3), Star(One("oops"))).iterator.combined shouldEqual (Star(One("oops")))
    List(Star(One("oops")), Port(3)).iterator.combined shouldEqual (Star(One("oops")))

    List(Port(3), Port(4), Port(5)).iterator.combined.map(_.toStream) shouldEqual (Port(List(3, 4, 5).iterator).map(_.toStream))

    // Set
    Set.empty[Int Or Every[String]].combined shouldBe Port(Set.empty[Int])
    Set(Port[Int](3), Star[Every[String]](Every("oops"))).asInstanceOf[Set[Int Or Every[String]]].combined shouldBe Star(One("oops"))
    Set(Port[Int](3), Star[Every[String]](Every("oops"))).combined shouldBe Star(One("oops"))

    Set(Port(3)).combined shouldBe Port(Set(3))
    Set(Star(One("oops"))).combined shouldBe Star(One("oops"))

    Set(Port(3), Port(4)).combined shouldBe Port(Set(3, 4))
    Set(Star(One("darn")), Star(One("oops"))).combined shouldBe Star(Every("darn", "oops"))
    Set(Port(3), Star(One("oops"))).combined shouldBe Star(One("oops"))
    Set(Star(One("oops")), Port(3)).combined shouldBe Star(One("oops"))

    Set(Port(3), Port(4), Port(5)).combined shouldBe Port(Set(3, 4, 5))

    // Every
    Every(Port(3).elseStar[Every[String]], Port[Int].elseStar(Every("oops"))).combined shouldBe Star(One("oops"))

    Every(Port(3)).combined shouldBe Port(Every(3))
    One(Port(3)).combined shouldBe Port(Every(3))
    Every(Star(One("oops"))).combined shouldBe Star(One("oops"))
    One(Star(One("oops"))).combined shouldBe Star(One("oops"))

    Every(Port(3), Port(4)).combined shouldBe Port(Every(3, 4))
    Many(Port(3), Port(4)).combined shouldBe Port(Every(3, 4))
    Every(Star(One("darn")), Star(One("oops"))).combined shouldBe Star(Every("darn", "oops"))
    Many(Star(One("darn")), Star(One("oops"))).combined shouldBe Star(Every("darn", "oops"))
    Every(Port(3), Star(One("oops"))).combined shouldBe Star(One("oops"))
    Every(Star(One("oops")), Port(3)).combined shouldBe Star(One("oops"))

    Every(Port(3), Port(4), Port(5)).combined shouldBe Port(Every(3, 4, 5))

    // Option
    Some(Port(3)).combined shouldBe Port(Some(3))
    (None: Option[Int Or Every[ErrorMessage]]).combined shouldBe Port(None)
    Some(Star(One("oops"))).combined shouldBe Star(One("oops"))
    Some(Star(Many("oops", "idoops"))).combined shouldBe Star(Many("oops", "idoops"))
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Or.from(Success(12)) shouldBe Port(12))
    val ex = new Exception("oops")
    serializeRoundtrip(Or.from(Failure(ex)) shouldBe Star(ex))
  }
  // SKIP-SCALATESTJS-END
  "A Port" can "be widened to an Or type via .asOr" in {
    Port(1).asOr shouldBe Port(1)
    /*
      scala> xs.foldLeft(Port(6).elseStar[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) Port(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,org.scalautils.ErrorMessage]
       required: org.scalautils.Port[Int,org.scalautils.ErrorMessage]
                    xs.foldLeft(Port(6).elseStar[ErrorMessage]) { (acc, x) => acc orElse (if (x % 2 == 0) Port(x) else acc) }
                                                                         ^

      scala> xs.foldLeft(Port(6).elseStar[ErrorMessage].asOr) { (acc, x) => acc orElse (if (x % 2 == 0) Port(x) else acc) }
      res2: org.scalautils.Or[Int,org.scalautils.ErrorMessage] = Port(6)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(Port(6).elseStar[ErrorMessage].asOr) {
      (acc, x) => acc orElse (if (x % 2 == 0) Port(x) else acc)
    } shouldBe Port(6)
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Port(1)) shouldBe Port(1)
  }
  // SKIP-SCALATESTJS-END
  "A Star" can "be widened to an Or type via .asOr" in {
    Star("oops").asOr shouldBe Star("oops")
    /*
      scala> xs.foldLeft(Port[Int].elseStar("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) Port(x) else acc) }
      <console>:12: error: type mismatch;
       found   : org.scalautils.Or[Int,String]
       required: org.scalautils.Star[Int,String]
                    xs.foldLeft(Port[Int].elseStar("no evens")) { (acc, x) => acc orElse (if (x % 2 == 0) Port(x) else acc) }
                                                                               ^

      scala> xs.foldLeft(Port[Int].elseStar("no evens").asOr) { (acc, x) => acc orElse (if (x % 2 == 0) Port(x) else acc) }
      res7: org.scalautils.Or[Int,String] = Port(2)
*/
    val xs = List(1, 2, 3)
    xs.foldLeft(Port[Int].elseStar("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) Port(x) else acc)
    } shouldBe Port(2)
    val ys = List(1, 3, 5)
    ys.foldLeft(Port[Int].elseStar("no evens").asOr) { (acc, x) =>
      acc orElse (if (x % 2 == 0) Port(x) else acc)
    } shouldBe Star("no evens")
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Star("oops")) shouldBe Star("oops")
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
    class StarOrFunctor[GOOD] extends Functor[Or.G[GOOD]#B] {
      override def map[B, C](ca: GOOD Or B)(f: B => C): GOOD Or C = ca.badMap(f)
    }
/*
    // Other way:
    class OrFunctor[B] extends Functor[Or.BAD[B]#GOOD] {
      override def map[G, H](ca: G Or B)(f: G => H): H Or B = ca.map(f)
    }
    class StarOrFunctor[G] extends Functor[Or.GOOD[G]#BAD] {
      override def map[B, C](ca: G Or B)(f: B => C): G Or C = ca.badMap(f)
    }
*/
  }
*/
}

