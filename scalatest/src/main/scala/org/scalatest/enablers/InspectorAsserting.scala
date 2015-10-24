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
import org.scalatest.Succeeded

trait InspectorAsserting[T] {
  type Result
  val Singleton: Result
}

abstract class LowPriorityInspectorAsserting {

  implicit def assertingNatureOfT[T]: InspectorAsserting[T] { type Result = Unit } = {
    new InspectorAsserting[T] {
      type Result = Unit
      val Singleton = ()
    }
  }
}

abstract class MediumPriorityInspectorAsserting extends LowPriorityInspectorAsserting {
  implicit def assertingNatureOfAssertion: InspectorAsserting[Assertion] { type Result = Assertion } = {
    new InspectorAsserting[Assertion] {
      type Result = Assertion
      val Singleton = Succeeded
    }
  }

/*
  implicit def assertingNatureOfExpectation: InspectorAsserting[Expectation] { type Result = Expectation } = {
    new InspectorAsserting[Expectation] {
      type Result = Expectation
    }
  }
*/
}

object InspectorAsserting extends MediumPriorityInspectorAsserting {

/*
  implicit def assertingNatureOfNothing: InspectorAsserting[Nothing] { type Result = Nothing } = {
    new InspectorAsserting[Nothing] {
      type Result = Nothing
      val Singleton = throw new NoSuchElementException
    }
  }
*/
  implicit def assertingNatureOfString: InspectorAsserting[String] { type Result = Unit } = {
    new InspectorAsserting[String] {
      type Result = Unit
      val Singleton = ()
    }
  }
}
