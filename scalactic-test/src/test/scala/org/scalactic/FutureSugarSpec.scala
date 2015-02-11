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

/*
class FutureSugarSpec extends UnitSpec with Accumulation with FutureSugar {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 == 0) Pass else Fail(i + " was not a round number")

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 == 0) Pass else Fail(i + " was not divisible by 3")

  it should "offer a validating method that takes a T => Validation" in {
    Future.successful(12).validating(isRound) shouldBe Failure(ValidationException("12 was not a round number"))
    Future.successful(10).validating(isRound) shouldBe Success(10)
    Future.failed(new "oops").validating(isRound) shouldBe Failure("oops")
  }
}

*/
