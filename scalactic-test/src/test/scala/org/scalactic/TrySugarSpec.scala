/*
 * Copyright 2001-2014 Artima, Inc.
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
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import prop.TableDrivenPropertyChecks._

class TrySugarSpec extends UnitSpec with Accumulation with TrySugar {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 == 0) Pass else Fail(i + " was not a round number")

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 == 0) Pass else Fail(i + " was not divisible by 3")

  case class SomeException(msg: String) extends Exception(msg)

  "TrySugar" should "enable toOr to be invoked on Trys" in {
    Success(12).toOr shouldBe Good(12)
    (Success(12): Try[Int]).toOr shouldBe Good(12)
    val ex = new Exception("oops")
    Failure(ex).toOr shouldBe Bad(ex)
    (Failure(ex): Try[Int]).toOr shouldBe Bad(ex)
  }

  it should "offer a validating method that takes a T => Validation" in {
    Success(12).validating(isRound) shouldBe Failure(ValidationFailedException("12 was not a round number"))
    Success(10).validating(isRound) shouldBe Success(10)
    Failure(SomeException("oops")).validating(isRound) shouldBe Failure(SomeException("oops"))
  }

  it should "allow multiple validation functions to be passed to validating" in {
    Success(12).validating(isRound, isDivBy3) shouldBe Failure(ValidationFailedException("12 was not a round number"))
    Success(10).validating(isRound, isDivBy3) shouldBe Failure(ValidationFailedException("10 was not divisible by 3"))
    Success(30).validating(isRound, isDivBy3) shouldBe Success(30)
    Failure(SomeException("oops")).validating(isRound) shouldBe Failure(SomeException("oops"))
  }

  it should "require at least one parameter to be passed to validating" in {
    "Try(30).validating()" shouldNot compile
  }
}

