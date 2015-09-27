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

trait OldAsserting[T] {
  def result: T
}

trait LowPriorityOldAssertingImplicits {
  implicit val assertingNatureOfUnit: OldAsserting[Unit] =
    new OldAsserting[Unit] {
      def result: Unit = ()
    }
  implicit val assertingNatureOfOldAssertionWithPendingStatement: OldAsserting[Assertion with PendingStatement] =
    new OldAsserting[Assertion with PendingStatement] {
      def result: Assertion with PendingStatement = Assertions.pending // Should never be used
    }
}

object OldAsserting extends LowPriorityOldAssertingImplicits {
  implicit val assertingNatureOfAssertion: OldAsserting[Assertion] =
    new OldAsserting[Assertion] {
      def result: Assertion = Succeeded
    }
}

