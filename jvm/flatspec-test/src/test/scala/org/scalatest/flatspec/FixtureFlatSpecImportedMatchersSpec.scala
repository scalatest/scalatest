/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.flatspec

import org.scalatest.StringFixture
import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.FixtureAnyFlatSpec

/*
This tests that matchers works with FlatSpec when matchers are imported,
something that broke in 2.1.RC1.
*/
class FixtureFlatSpecImportedMatchersSpec extends FixtureAnyFlatSpec with StringFixture {
  "This spec" should "work OK" in { _ =>
    "hello" should startWith ("he")
    "hello" should endWith ("lo")
    "hello" should include ("el")
    "hello" should startWith regex ("h*")
    "hello" should endWith regex (".*o")
    "hello" should include regex ("l*")
  }
  it should "still work OK" in { _ =>
    "dude" should not startWith ("he")
    "dude" should not endWith ("lo")
    "dude" should not include ("el")
    "dude" should not startWith regex ("h*l")
    "dude" should not endWith regex ("e*o")
    "dude" should not include regex ("e*l")
  }
}

