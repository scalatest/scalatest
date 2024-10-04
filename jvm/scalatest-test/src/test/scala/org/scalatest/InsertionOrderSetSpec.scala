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

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
class InsertionOrderSetSpec extends AnyFunSpec with Matchers {
  describe("An InsertionOrderSet") {
    it("should offer an apply method in the companion object") {
      InsertionOrderSet(List(1, 2, 3)) shouldBe (InsertionOrderSet(List(1, 2, 3)))
    }
    it("should ensure duplicates can't be passed to the constructor") {
      InsertionOrderSet(List(1, 2, 3, 3)) shouldBe InsertionOrderSet(List(1, 2, 3))
    }
    it("should ensure duplicates can't be added") {
      InsertionOrderSet(List(1, 2, 3)) + 3 shouldBe InsertionOrderSet(List(1, 2, 3))
    }
    it("should ensure non-duplicates can be added") {
      InsertionOrderSet(List(1, 2, 3)) + 4 shouldBe InsertionOrderSet(List(1, 2, 3, 4))
    }
    it("should return Iterator that iterates elements in the order they were inserted") {
      val set = InsertionOrderSet(List(2, 1, 3))
      val itr = set.iterator
      itr.next shouldBe 2
      itr.next shouldBe 1
      itr.next shouldBe 3
    }
    it("should return true when contains is called with element it contains") {
      val set = InsertionOrderSet(List(2, 1, 3))
      set.contains(1) shouldBe true
    }
    it("should return false when contains is called with element it contains") {
      val set = InsertionOrderSet(List(2, 1, 3))
      set.contains(6) shouldBe false
    }
    it("should remove element passed in from -") {
      InsertionOrderSet(List(2, 1, 3)) - 1 shouldBe InsertionOrderSet(List(2, 3))
    }
  }
}
