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
package org.scalatest.prop

import org.scalactic.anyvals._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PrettyFunction0Spec extends AnyFunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("A PrettyFunction0") {
    it("should return the constant passed to its constructor") {
      forAll { (i: Int) =>
        val f = new PrettyFunction0(i)
        f() shouldBe i
      }
    }
    it("should have a pretty toString") {
      forAll { (i: Int) =>
        val f = new PrettyFunction0(i)
        f.toString shouldBe s"() => $i"
      }
    }
    it("should offer an apply method in its companion object") {
      forAll { (i: Int) =>
        val f = PrettyFunction0(i)
        f() shouldBe i
        f.toString shouldBe s"() => $i"
      }
    }
    it("should offer an equals method that compares the constant result") {
      forAll { (i: Int) =>
        val f = PrettyFunction0(i)
        val g = PrettyFunction0(i)
        f shouldEqual g
      }
    }
    it("should offer a hashCode method that returns the hashCode of the constant result") {
      forAll { (i: Int) =>
        val f = PrettyFunction0(i)
        val g = PrettyFunction0(i)
        f.hashCode shouldEqual g.hashCode
      }
    }
  }
}
