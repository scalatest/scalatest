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
/**
  * A <code>FixtureTestSuite</code> mixin to be used in conjunction
  * with the <code>Fixture</code> type. See that type's documentation
  * for information on how to build a fixture.
  * 
  * It provides an implementation of the <code>withFixture</code>
  * method so that all that remains for you to do is define the
  * <code>FixtureParam</code> type and of course the 
  * <code>fixture</code> method.
  */
trait GenericFixtureSuite extends FixtureTestSuite {

  def fixture: Fixture[FixtureParam]
  override protected def withFixture(test: OneArgTest): Outcome =
    fixture(test, test.toNoArgTest.andThen(withFixture))
}


