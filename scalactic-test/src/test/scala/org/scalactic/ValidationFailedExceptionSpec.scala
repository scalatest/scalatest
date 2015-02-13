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

class ValidationFailedExceptionSpec extends UnitSpec {

  "ValidationFailedException" should "require an error message string and return it from message" in {
    ValidationFailedException("I meant to do that!").error shouldBe "I meant to do that!"
    new ValidationFailedException("I meant to do that!").error shouldBe "I meant to do that!"
  }

  it should "throw a NPE if null is passed for the error message" in {
    a [NullPointerException] shouldBe thrownBy { ValidationFailedException(null) }
    a [NullPointerException] shouldBe thrownBy { new ValidationFailedException(null) }
  }

  it should "return that same error message from getMessage" in {
    ValidationFailedException("I meant to do that!").getMessage shouldBe "I meant to do that!"
    new ValidationFailedException("I meant to do that!").getMessage shouldBe "I meant to do that!"
  }

  it should "take any type of error object, and pass the toString up as message" in {
    case class ErrorValue(msg: String) {
      override def toString = msg
    }
    ValidationFailedException(ErrorValue("I meant to do that too!")).error shouldBe ErrorValue("I meant to do that too!")
    new ValidationFailedException(ErrorValue("I meant to do that too!")).error shouldBe ErrorValue("I meant to do that too!")

    ValidationFailedException(ErrorValue("I meant to do that too!")).getMessage shouldBe "I meant to do that too!"
    new ValidationFailedException(ErrorValue("I meant to do that too!")).getMessage shouldBe "I meant to do that too!"
  }
}

