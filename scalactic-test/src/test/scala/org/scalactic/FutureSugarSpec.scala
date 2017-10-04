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

import scala.concurrent.Future
// SKIP-SCALATESTJS-START
import scala.concurrent.ExecutionContext.Implicits.global
// SKIP-SCALATESTJS-END
//SCALATESTJS-ONLY import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import org.scalatest.concurrent.ScalaFutures
import exceptions.ValidationFailedException
import org.scalatest.exceptions.TestFailedException

class FutureSugarSpec extends UnitSpec with Accumulation with FutureSugar with ScalaFutures {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 == 0) Pass else Fail(i + " was not a round number")

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 == 0) Pass else Fail(i + " was not divisible by 3")

  case class SomeException(msg: String) extends Exception(msg)

  "FutureSugar" should "offer a validating method that takes a T => Validation" in {
    Future.successful(12).validating(isRound).failed.futureValue shouldBe ValidationFailedException("12 was not a round number")
    Future.successful(10).validating(isRound).futureValue shouldBe 10
    // Scala 2.10 has problem compiling the following:
    // Future.failed(SomeException("oops")).validating(isRound).failed.futureValue shouldBe SomeException("oops")
    Future.failed[Int](SomeException("oops")).validating(isRound).failed.futureValue shouldBe SomeException("oops")
  }

  it should "allow multiple validation functions to be passed to validating" in {
    Future.successful(12).validating(isRound, isDivBy3).failed.futureValue shouldBe ValidationFailedException("12 was not a round number")
    Future.successful(10).validating(isRound, isDivBy3).failed.futureValue shouldBe ValidationFailedException("10 was not divisible by 3")
    Future.successful(30).validating(isRound, isDivBy3).futureValue shouldBe 30
    // Scala 2.10 has problem compiling the following:
    // Future.failed(SomeException("oops")).validating(isRound).failed.futureValue shouldBe SomeException("oops")
    Future.failed[Int](SomeException("oops")).validating(isRound).failed.futureValue shouldBe SomeException("oops")
  }

  it should "require at least one parameter to be passed to validating" in {
    "Try(30).validating()" shouldNot compile
  }
}

