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

class EitherSugarSpec extends UnitSpec with Accumulation with EitherSugar {

  "EitherSugar" should "enable toOr to be invoked on Eithers" in {
    Right(12).toOr shouldBe Good(12)
    (Right(12): Either[String, Int]).toOr shouldBe Good(12)
    val ex = new Exception("oops")
    Left(ex).toOr shouldBe Bad(ex)
    Left("oops").toOr shouldBe Bad("oops")
    (Left("oops"): Either[String, String]).toOr shouldBe Bad("oops")
  }
}

