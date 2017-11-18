/*
  * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest.jmock

import org.scalatest._
import org.scalatest.fixture

/**
  * Trait that will pass a new <code>JMockCycle</code> into any test that needs one.
  *
  * <p>
  * This trait, which must be mixed into a <code>fixture.AsyncSuite</code>, defines the
  * <code>Fixture</code> type to be <code>JMockCycle</code> and defines a
  * <code>withFixture</code> method that instantiates a new <code>JMockCycle</code>
  * and passes it to the test function.
  * </p>
  *
  * @author Bill Venners
  */
trait AsyncJMockCycleFixture { this: fixture.AsyncTestSuite =>

  /**
    * Defines the <code>Fixture</code> type to be <code>JMockCycle</code>.
    */
  type FixtureParam = JMockCycle

  /**
    * Instantiates a new <code>JMockCycle</code> and passes it to the test function.
    *
    * @param test the test function to which to pass a new <code>JMockCycle</code>
    */
  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    test(new JMockCycle)
  }
}
