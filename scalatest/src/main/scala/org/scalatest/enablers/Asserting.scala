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

trait Asserting[T] {
  def result: T
}

trait LowPriorityAssertingImplicits {

  implicit val assertingNatureOfUnit: Asserting[Unit] =
    new Asserting[Unit] {
      def result: Unit = ()
    }

  implicit val assertingNatureOfAssertionWithPendingStatement: Asserting[Assertion with PendingStatement] =
    new Asserting[Assertion with PendingStatement] {
      def result: Assertion with PendingStatement = Assertions.pending // Should never be used
    }
}

object Asserting extends LowPriorityAssertingImplicits {

  implicit val assertingNatureOfAssertion: Asserting[Assertion] =
    new Asserting[Assertion] {
      def result: Assertion = Succeeded
    }
}


