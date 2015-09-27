/*
 * Copyright 2001-2015 Artima, Inc.
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
import org.scalatest.Succeeded
import org.scalatest.PendingStatement
import org.scalatest.Assertions
import org.scalatest.Expectation
import org.scalatest.Fact
import org.scalatest.Succeeded
import org.scalatest.exceptions.DiscardedEvaluationException

trait NewAsserting[T] {
  type Result <: AnyRef
  def whenever(condition: Boolean)(fun: => T): Result
}

trait LowPriorityNewAssertingImplicits {
  implicit def assertingNatureOfNonExpectation[T]: NewAsserting[T] { type Result = Assertion } =
    new NewAsserting[T] {
      type Result = Assertion
      def whenever(condition: Boolean)(fun: => T): Assertion = {
      if (!condition)
        throw new DiscardedEvaluationException
      else {
       fun
       Succeeded
      }
    }
  }
}

object NewAsserting extends LowPriorityNewAssertingImplicits {
/*
  implicit def assertingNatureOfExpectation[T <: Expectation]: NewAsserting[T] { type Result = Expectation } =
    new NewAsserting[T] {
      type Result = Expectation
      def whenever(condition: Boolean)(fun: => T): Expectation = {
        if (!condition) Fact.VacuousYes(Fact.No("The whenever condition was false", "the whenever condition was false"))
        else fun
      }
    }
*/
  implicit val assertingNatureOfExpectation: NewAsserting[Expectation] { type Result = Expectation } =
    new NewAsserting[Expectation] {
      type Result = Expectation
      def whenever(condition: Boolean)(fun: => Expectation): Expectation = {
        if (!condition) Fact.VacuousYes(Fact.No("The whenever condition was false", "the whenever condition was false"))
        else fun
      }
    }
}

