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

class OptionSugarSpec extends UnitSpec with Accumulation with OptionSugar {

  "OptionSugar" should "enable toOr to be invoked on Options" in {
    Some(12).toOr("won't be used") shouldBe Good(12)
    (Some(12): Option[Int]).toOr("won't be used") shouldBe Good(12)
    val ex = new Exception("oops")
    None.toOr(ex) shouldBe Bad(ex)
    None.toOr("oops") shouldBe Bad("oops")
    (None: Option[String]).toOr("oops") shouldBe Bad("oops")
  }
  it should "take a byName for the orElse" in {
    var noneChangedThis = false
    var someChangedThis = false
    None.toOr{noneChangedThis = true; "oops"} shouldBe Bad("oops")
    Some(12).toOr{noneChangedThis = true; "oops"} shouldBe Good(12)
    noneChangedThis shouldBe true
    someChangedThis shouldBe false
  }
}

