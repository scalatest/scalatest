/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class RunningTestSpec extends AnyFunSpec {

  describe("The RunningTest class") {
    it("should determine equality based on the suite ID and test name only") {
      val a = new RunningTest("suite name", "suite ID", "test name", 0)
      val b = new RunningTest("suite name", "suite ID", "test name", 0)
      val c = new RunningTest("different suite name", "suite ID", "test name", 0)
      val d = new RunningTest("suite name", "different suite ID", "test name", 0)
      val e = new RunningTest("suite name", "suite ID", "different test name", 0)
      val f = new RunningTest("suite name", "suite ID", "test name", 99)
      a should equal (b)
      a should equal (c)
      a should not equal (d)
      a should not equal (e)
      a should equal (f)
    }
  }
}
