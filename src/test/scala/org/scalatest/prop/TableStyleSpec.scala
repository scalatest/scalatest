/*
 * Copyright 2001-2011 Artima, Inc.
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
package org.scalatest.prop

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class TableStyleSpec extends FunSpec with ShouldMatchers with TableDrivenPropertyChecks {

  describe("A TableFor1") {
    it("should be usable to test stateful functions") {

      object FiboGen {
        private var prev: Option[(Option[Int], Int)] = None
        def next: Int =
          prev match {
            case None =>
              prev = Some(None, 0)
              0
            case Some((None, 0)) =>
              prev = Some((Some(0), 1))
              1
            case Some((Some(prev1), prev2)) =>
              val result = prev1 + prev2
              prev = Some((Some(prev2), result))
              result
          }
      }

      val first14FiboNums =
        Table("n", 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233)

      forAll (first14FiboNums) { n => FiboGen.next should equal (n) }
    }
  }

  describe("A TableFor2") {
    it("should be usable to test mutable objects in a state machine-like way") {

      class Counter {
        private var v = 0
        def reset() { v = 0 }
        def click() { v += 1 }
        def enter(n: Int) { v = n }
        def value = v
      }

      abstract class Action
      case object Start extends Action
      case object Click extends Action
      case class Enter(n: Int) extends Action

      val stateTransitions =
        Table(
          ("action", "expectedValue"),
          (Start,    0),
          (Click,    1),
          (Click,    2),
          (Click,    3),
          (Enter(5), 5),
          (Click,    6),
          (Enter(1), 1),
          (Click,    2),
          (Click,    3)
        )

      val counter = new Counter
      forAll (stateTransitions) { (action, expectedValue) =>
        action match {
          case Start => counter.reset()
          case Click => counter.click()
          case Enter(n) => counter.enter(n)
        }
        counter.value should equal (expectedValue)
      }
    }
  }
}

