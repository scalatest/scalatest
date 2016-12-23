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

class BadMapperSpec extends UnitSpec with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "A BadMapper" can "be either Good or Bad" in {
    Good(7).badMapper.isGood shouldBe true
    Good(7).badMapper.isBad shouldBe false
    Bad("oops").badMapper.isBad shouldBe true
    Bad("oops").badMapper.isGood shouldBe false
    Good(7).badMapper.value shouldBe an [Good[_]]
    Bad("oops").badMapper.value shouldBe an [Bad[_]]
  }
  it can "be used with map" in {
    Good(8).orBad[ErrorMessage].badMapper map (_.toUpperCase) should equal (Good(8).badMapper)
    Good[Int].orBad("eight").badMapper map (_.toUpperCase) should equal (Bad("EIGHT").badMapper)
  }
  it can "be used with recover" in {
    Good[Throwable].orBad(8).badMapper recover {
      case iae: IllegalArgumentException => 9
    } should equal (Bad(8).badMapper)
    Good(new IllegalArgumentException).orBad[Int].badMapper recover {
      case iae: IllegalArgumentException => 9
    } should equal (Bad(9).badMapper)
  }
  it can "be used with recoverWith" in {
    Good[Throwable].orBad(8).badMapper recoverWith {
      case iae: IllegalArgumentException => Bad(9).badMapper
    } should equal (Bad(8).badMapper)
    Good(new IllegalArgumentException).orBad[Int].badMapper recoverWith {
      case iae: IllegalArgumentException => Bad(9).badMapper
    } should equal (Bad(9).badMapper)
  }
  it can "be used with foreach" in {
    var vCount = 0
    var eCount = 0
    Bad(8).badMapper foreach { vCount += _ }
    vCount should equal (8)
    Good("eight").orBad[Int].badMapper foreach { eCount += _ }
    eCount should equal (0)
  }
  it can "be used with flatMap" in {
    Good[String].orBad(8).badMapper flatMap ((x: Int) => Bad(x + 1).badMapper) should equal (Bad(9).badMapper)
    Good("eight").orBad[Int].badMapper flatMap ((x: Int) => Bad(x + 1).badMapper) should equal (Good("eight").badMapper)
  }
  it can "be used with filter" in {
    Bad(12).badMapper.filter(isRound) shouldBe Good("12 was not a round number").badMapper
    Bad(10).badMapper.filter(isRound) shouldBe Bad(10).badMapper
    Good(12).orBad[Int].badMapper.filter(isRound) shouldBe Good(12).badMapper
    (for (i <- Bad(10).badMapper if isRound(i)) yield i) shouldBe Bad(10).badMapper
    (for (i <- Bad(12).badMapper if isRound(i)) yield i) shouldBe Good("12 was not a round number").badMapper
    (for (i <- Bad(12).badMapper if isRound(i)) yield i) shouldBe Good("12 was not a round number").badMapper
    (for (i <- Bad(30).badMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Bad(30).badMapper
    (for (i <- Bad(10).badMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Good("10 was not divisible by 3").badMapper
    (for (i <- Bad(3).badMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Good("3 was not a round number").badMapper
    (for (i <- Bad(2).badMapper if isRound(i) && isDivBy3(i)) yield i) shouldBe Good("2 was not a round number").badMapper
  }
  it can "be used with exists" in {
    Bad(12).badMapper.exists(_ == 12) shouldBe true
    Bad(12).badMapper.exists(_ == 13) shouldBe false
    Good(12).orBad[Int].badMapper.exists(_ == 12) shouldBe false
  }
  it can "be used with forall" in {
    Bad(12).badMapper.forall(_ > 10) shouldBe true
    Bad(7).badMapper.forall(_ > 10) shouldBe false
    Good(12).orBad[Int].badMapper.forall(_ > 10) shouldBe true
    Good(7).orBad[Int].badMapper.forall(_ > 10) shouldBe true
  }
  it can "be used with getOrElse, which takes a by-name" in {

    Bad(12).badMapper.getOrElse(17) shouldBe 12
    Good(12).orBad[Int].badMapper.getOrElse(17) shouldBe 17

    var x = 16 // should not increment if Good
    Bad(12).badMapper getOrElse { x += 1; x } shouldBe 12
    x shouldBe 16
    Good(12).orBad[Int].badMapper getOrElse { x += 1; x } shouldBe 17
    x shouldBe 17
  }
  it can "be used with orElse, which takes a by-name" in {

    Bad(12).badMapper.orElse(Bad(13).badMapper) shouldBe Bad(12).badMapper
    Good(12).badMapper.orElse(Bad(13).badMapper) shouldBe Bad(13).badMapper

    Bad(12).badMapper.orElse(Good(13).badMapper) shouldBe Bad(12).badMapper
    Good(12).badMapper.orElse(Good(13).badMapper) shouldBe Good(13).badMapper

    var x = 16 // should not increment if Bad
    Bad(12).badMapper orElse { x += 1; Bad(x).badMapper } shouldBe Bad(12).badMapper
    x shouldBe 16
    Good(12).orBad[Int].badMapper orElse { x += 1; Bad(x).badMapper } shouldBe Bad(17).badMapper
    x shouldBe 17

    var y = 16 // should not increment if Bad
    Bad(12).badMapper orElse { y += 1; Good(y).badMapper } shouldBe Bad(12).badMapper
    y shouldBe 16
    Good(12).orBad[Int].badMapper orElse { y += 1; Good(y).badMapper } shouldBe Good(17).badMapper
    y shouldBe 17
  }
  it can "be used with toOption" in {
    Bad(12).badMapper.toOption shouldBe Some(12)
    Good(12).orBad[Int].badMapper.toOption shouldBe None
  }
  it can "be used with toSeq" in {
    Bad(12).badMapper.toSeq shouldEqual Seq(12)
    Good(12).orBad[Int].badMapper.toSeq shouldEqual Seq.empty
  }
// toArray, toBuffer, toIndexedSeq, toIterable, toIterator, toList, 
// toSeq, toStream, toTraversable, toVector
  it can "be used with toEither" in {
    Bad(12).badMapper.toEither shouldBe Right(12)
    Good(12).badMapper.toEither shouldBe Left(12)
  }
  it can "be used with toOr" in {
    Bad(12).badMapper.toOr shouldBe Good(12)
    Good(12).badMapper.toOr shouldBe Bad(12)
  }
  it can "be used with toTry, if the error type is a subtype of Throwable" in {
    Good[Throwable].orBad(12).badMapper.toTry shouldBe Success(12)
    Good[RuntimeException].orBad(12).badMapper.toTry shouldBe Success(12)
    val ex = new RuntimeException("oops")
    Good(ex).orBad[Int].badMapper.toTry shouldBe Failure(ex)
    Good(ex).orBad[Int].badMapper.toTry shouldBe Failure(ex)
    "Good(12).orBad[Int].badMapper.toTry" shouldNot typeCheck
  }
  it can "be used with swap" in {
    Good(12).orBad[String].badMapper.swap should === (Good[String].orBad(12).badMapper)
    Good[Int].orBad("hi").badMapper.swap should === (Good("hi").orBad[Int].badMapper)
  }
  it can "be used with transform" in {
    Good[String].orBad(12).badMapper.transform((s: String) => Bad(s.toUpperCase).badMapper, (i: Int) => Good(i + 1).badMapper) should === (Good(13).badMapper)
    Good("hi").orBad[Int].badMapper.transform( (s: String) => Bad(s.toUpperCase).badMapper, (i: Int) => Good(i + 1).badMapper) should === (Bad("HI").badMapper)
    Good[String].orBad(12).badMapper.transform((s: String) => Good(s.toUpperCase).badMapper, (i: Int) => Bad(i + 1).badMapper) should === (Bad(13).badMapper)
    Good("hi").orBad[Int].badMapper.transform((s: String) => Good(s.toUpperCase).badMapper, (i: Int) => Bad(i + 1).badMapper) should === (Good("HI").badMapper)
  }
  it can "be folded with fold" in {
    Good[String].orBad(3).badMapper.fold(_.length, _ + 1) shouldBe 4
    Good("howdy").orBad[Int].badMapper.fold(_.length ,_ + 1) shouldBe 5
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly 1" in {
    serializeRoundtrip(Or.from(Success(12)).badMapper shouldBe Good(12).badMapper)
    val ex = new Exception("oops")
    serializeRoundtrip(Or.from(Failure(ex)).badMapper shouldBe Bad(ex).badMapper)
  }
  // SKIP-SCALATESTJS-END
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly 2" in {
    serializeRoundtrip(Good(1).badMapper) shouldBe Good(1).badMapper
  }
  // SKIP-SCALATESTJS-END
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly 3" in {
    serializeRoundtrip(Bad("oops").badMapper) shouldBe Bad("oops").badMapper
  }
  // SKIP-SCALATESTJS-END
}

