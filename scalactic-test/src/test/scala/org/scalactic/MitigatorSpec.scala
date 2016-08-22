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

class MitigatorSpec extends UnitSpec with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "A Mitigator" can "be either Good or Bad" in {
    Good(7).mitigator.isGood shouldBe true
    Good(7).mitigator.isBad shouldBe false
    Bad("oops").mitigator.isBad shouldBe true
    Bad("oops").mitigator.isGood shouldBe false
    Good(7).mitigator.value shouldBe an [Good[_]]
    Bad("oops").mitigator.value shouldBe an [Bad[_]]
  }
  it can "be used with map" in {
    Good(8).orBad[ErrorMessage].mitigator map (_.toUpperCase) should equal (Good(8).mitigator)
    Good[Int].orBad("eight").mitigator map (_.toUpperCase) should equal (Bad("EIGHT").mitigator)
  }
  it can "be used with recover" in {
    Good[Throwable].orBad(8).mitigator recover {
      case iae: IllegalArgumentException => 9
    } should equal (Bad(8).mitigator)
    Good(new IllegalArgumentException).orBad[Int].mitigator recover {
      case iae: IllegalArgumentException => 9
    } should equal (Bad(9).mitigator)
  }
  it can "be used with recoverWith" in {
    Good[Throwable].orBad(8).mitigator recoverWith {
      case iae: IllegalArgumentException => Bad(9).mitigator
    } should equal (Bad(8).mitigator)
    Good(new IllegalArgumentException).orBad[Int].mitigator recoverWith {
      case iae: IllegalArgumentException => Bad(9).mitigator
    } should equal (Bad(9).mitigator)
  }
  it can "be used with foreach" in {
    var vCount = 0
    var eCount = 0
    Bad(8).mitigator foreach { vCount += _ }
    vCount should equal (8)
    Good("eight").orBad[Int].mitigator foreach { eCount += _ }
    eCount should equal (0)
  }
  it can "be used with flatMap" in {
    Good[String].orBad(8).mitigator flatMap ((x: Int) => Bad(x + 1).mitigator) should equal (Bad(9).mitigator)
    Good("eight").orBad[Int].mitigator flatMap ((x: Int) => Bad(x + 1).mitigator) should equal (Good("eight").mitigator)
  }
  it can "be used with filter" in {
    Bad(12).mitigator.filter(isRound) shouldBe Good("12 was not a round number").mitigator
    Bad(10).mitigator.filter(isRound) shouldBe Bad(10).mitigator
    Good(12).orBad[Int].mitigator.filter(isRound) shouldBe Good(12).mitigator
    (for (i <- Bad(10).mitigator if isRound(i)) yield i) shouldBe Bad(10).mitigator
    (for (i <- Bad(12).mitigator if isRound(i)) yield i) shouldBe Good("12 was not a round number").mitigator
    (for (i <- Bad(12).mitigator if isRound(i)) yield i) shouldBe Good("12 was not a round number").mitigator
    (for (i <- Bad(30).mitigator if isRound(i) && isDivBy3(i)) yield i) shouldBe Bad(30).mitigator
    (for (i <- Bad(10).mitigator if isRound(i) && isDivBy3(i)) yield i) shouldBe Good("10 was not divisible by 3").mitigator
    (for (i <- Bad(3).mitigator if isRound(i) && isDivBy3(i)) yield i) shouldBe Good("3 was not a round number").mitigator
    (for (i <- Bad(2).mitigator if isRound(i) && isDivBy3(i)) yield i) shouldBe Good("2 was not a round number").mitigator
  }
  it can "be used with exists" in {
    Bad(12).mitigator.exists(_ == 12) shouldBe true
    Bad(12).mitigator.exists(_ == 13) shouldBe false
    Good(12).orBad[Int].mitigator.exists(_ == 12) shouldBe false
  }
  it can "be used with forall" in {
    Bad(12).mitigator.forall(_ > 10) shouldBe true
    Bad(7).mitigator.forall(_ > 10) shouldBe false
    Good(12).orBad[Int].mitigator.forall(_ > 10) shouldBe true
    Good(7).orBad[Int].mitigator.forall(_ > 10) shouldBe true
  }
  it can "be used with getOrElse, which takes a by-name" in {

    Bad(12).mitigator.getOrElse(17) shouldBe 12
    Good(12).orBad[Int].mitigator.getOrElse(17) shouldBe 17

    var x = 16 // should not increment if Good
    Bad(12).mitigator getOrElse { x += 1; x } shouldBe 12
    x shouldBe 16
    Good(12).orBad[Int].mitigator getOrElse { x += 1; x } shouldBe 17
    x shouldBe 17
  }
  it can "be used with orElse, which takes a by-name" in {

    Bad(12).mitigator.orElse(Bad(13).mitigator) shouldBe Bad(12).mitigator
    Good(12).mitigator.orElse(Bad(13).mitigator) shouldBe Bad(13).mitigator

    Bad(12).mitigator.orElse(Good(13).mitigator) shouldBe Bad(12).mitigator
    Good(12).mitigator.orElse(Good(13).mitigator) shouldBe Good(13).mitigator

    var x = 16 // should not increment if Bad
    Bad(12).mitigator orElse { x += 1; Bad(x).mitigator } shouldBe Bad(12).mitigator
    x shouldBe 16
    Good(12).orBad[Int].mitigator orElse { x += 1; Bad(x).mitigator } shouldBe Bad(17).mitigator
    x shouldBe 17

    var y = 16 // should not increment if Bad
    Bad(12).mitigator orElse { y += 1; Good(y).mitigator } shouldBe Bad(12).mitigator
    y shouldBe 16
    Good(12).orBad[Int].mitigator orElse { y += 1; Good(y).mitigator } shouldBe Good(17).mitigator
    y shouldBe 17
  }
  it can "be used with toOption" in {
    Bad(12).mitigator.toOption shouldBe Some(12)
    Good(12).orBad[Int].mitigator.toOption shouldBe None
  }
  it can "be used with toSeq" in {
    Bad(12).mitigator.toSeq shouldEqual Seq(12)
    Good(12).orBad[Int].mitigator.toSeq shouldEqual Seq.empty
  }
// toArray, toBuffer, toIndexedSeq, toIterable, toIterator, toList, 
// toSeq, toStream, toTraversable, toVector
  it can "be used with toEither" in {
    Bad(12).mitigator.toEither shouldBe Right(12)
    Good(12).mitigator.toEither shouldBe Left(12)
  }
  it can "be used with toOr" in {
    Bad(12).mitigator.toOr shouldBe Good(12)
    Good(12).mitigator.toOr shouldBe Bad(12)
  }
  it can "be used with toTry, if the error type is a subtype of Throwable" in {
    Good[Throwable].orBad(12).mitigator.toTry shouldBe Success(12)
    Good[RuntimeException].orBad(12).mitigator.toTry shouldBe Success(12)
    val ex = new RuntimeException("oops")
    Good(ex).orBad[Int].mitigator.toTry shouldBe Failure(ex)
    Good(ex).orBad[Int].mitigator.toTry shouldBe Failure(ex)
    "Good(12).orBad[Int].mitigator.toTry" shouldNot typeCheck
  }
  it can "be used with swap" in {
    Good(12).orBad[String].mitigator.swap should === (Good[String].orBad(12).mitigator)
    Good[Int].orBad("hi").mitigator.swap should === (Good("hi").orBad[Int].mitigator)
  }
  it can "be used with transform" in {
    Good[String].orBad(12).mitigator.transform((s: String) => Bad(s.toUpperCase).mitigator, (i: Int) => Good(i + 1).mitigator) should === (Good(13).mitigator)
    Good("hi").orBad[Int].mitigator.transform( (s: String) => Bad(s.toUpperCase).mitigator, (i: Int) => Good(i + 1).mitigator) should === (Bad("HI").mitigator)
    Good[String].orBad(12).mitigator.transform((s: String) => Good(s.toUpperCase).mitigator, (i: Int) => Bad(i + 1).mitigator) should === (Bad(13).mitigator)
    Good("hi").orBad[Int].mitigator.transform((s: String) => Good(s.toUpperCase).mitigator, (i: Int) => Bad(i + 1).mitigator) should === (Good("HI").mitigator)
  }
  it can "be folded with fold" in {
    Good[String].orBad(3).mitigator.fold(_.length, _ + 1) shouldBe 4
    Good("howdy").orBad[Int].mitigator.fold(_.length ,_ + 1) shouldBe 5
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly 1" in {
    serializeRoundtrip(Or.from(Success(12)).mitigator shouldBe Good(12).mitigator)
    val ex = new Exception("oops")
    serializeRoundtrip(Or.from(Failure(ex)).mitigator shouldBe Bad(ex).mitigator)
  }
  // SKIP-SCALATESTJS-END
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly 2" in {
    serializeRoundtrip(Good(1).mitigator) shouldBe Good(1).mitigator
  }
  // SKIP-SCALATESTJS-END
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly 3" in {
    serializeRoundtrip(Bad("oops").mitigator) shouldBe Bad("oops").mitigator
  }
  // SKIP-SCALATESTJS-END
}

