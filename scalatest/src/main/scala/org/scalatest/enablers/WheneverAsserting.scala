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
package org.scalatest.enablers

import org.scalatest.Assertion
import org.scalatest.Expectation
import org.scalatest.exceptions.DiscardedEvaluationException

trait WheneverAsserting[T] {
  type Result
  def whenever(condition: Boolean)(fun: => T): Result
}

abstract class UnitWheneverAsserting {

  implicit def assertingNatureOfT[T]: WheneverAsserting[T] { type Result = Unit } = {
    new WheneverAsserting[T] {
      type Result = Unit
      def whenever(condition: Boolean)(fun: => T): Unit =
        if (!condition)
          throw new DiscardedEvaluationException
        else
         fun
    }
  }
}

abstract class ExpectationWheneverAsserting extends UnitWheneverAsserting {

  implicit def assertingNatureOfExpectation: WheneverAsserting[Expectation] { type Result = Expectation } = {
    new WheneverAsserting[Expectation] {
      type Result = Expectation
      def whenever(condition: Boolean)(fun: => Expectation): Expectation =
        if (!condition)
          throw new DiscardedEvaluationException
        else
         fun
    }
  }
}

object WheneverAsserting extends ExpectationWheneverAsserting {

  implicit def assertingNatureOfAssertion: WheneverAsserting[Assertion] { type Result = Assertion } = {
    new WheneverAsserting[Assertion] {
      type Result = Assertion
      def whenever(condition: Boolean)(fun: => Assertion): Assertion =
        if (!condition)
          throw new DiscardedEvaluationException
        else
         fun
    }
  }
}
