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
package org.scalatest

trait StringFixture { this: FixtureTestSuite =>
  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }
}

trait StringFixtureFunSuite extends funsuite.FixtureAnyFunSuite with StringFixture
trait StringFixtureFunSpec extends funspec.FixtureAnyFunSpec with StringFixture
trait StringFixtureWordSpec extends wordspec.FixtureAnyWordSpec with StringFixture
trait StringFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with StringFixture
trait StringFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with StringFixture
trait StringFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with StringFixture
trait StringFixturePropSpec extends propspec.FixtureAnyPropSpec with StringFixture
