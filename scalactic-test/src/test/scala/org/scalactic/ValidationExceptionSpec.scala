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

class ValidationExceptionSpec extends UnitSpec {

  "ValidationException" should "require an error message string and return it from message" in {
    ValidationException("I meant to do that!").errorMessage shouldBe "I meant to do that!"
    new ValidationException("I meant to do that!").errorMessage shouldBe "I meant to do that!"
  }

  it should "throw a NPE if null is passed for the error message" in {
    a [NullPointerException] shouldBe thrownBy { ValidationException(null) }
    a [NullPointerException] shouldBe thrownBy { new ValidationException(null) }
  }

  it should "return that same error message from getMessage" in {
    ValidationException("I meant to do that!").getMessage shouldBe "I meant to do that!"
    new ValidationException("I meant to do that!").getMessage shouldBe "I meant to do that!"
  }
}

