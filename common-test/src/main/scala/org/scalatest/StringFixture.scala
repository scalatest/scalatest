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
package org.scalatest

trait StringFixture { this: fixture.Suite =>
  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }
}

// SKIP-SCALATESTJS-START
trait StringFixtureSpec extends fixture.RefSpec with StringFixture
// SKIP-SCALATESTJS-END
trait StringFixtureFunSuite extends fixture.FunSuite with StringFixture
trait StringFixtureFunSpec extends fixture.FunSpec with StringFixture
trait StringFixtureWordSpec extends fixture.WordSpec with StringFixture
trait StringFixtureFlatSpec extends fixture.FlatSpec with StringFixture
trait StringFixtureFreeSpec extends fixture.FreeSpec with StringFixture
trait StringFixtureFeatureSpec extends fixture.FeatureSpec with StringFixture
trait StringFixturePropSpec extends fixture.PropSpec with StringFixture
