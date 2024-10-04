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

import org.scalatest.exceptions.TestFailedException
import SharedHelpers._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeAnSymbolSpec extends AnyFunSpec with FruitMocks {

  describe("The be an ('symbol) syntax") {

    it("should do nothing if the object has an appropriately named method, which returns true") {
      appleMock should be an (Symbol("apple"))
      isAppleMock should be an (Symbol("apple"))
    }

    it("should throw TestFailedException with an appropriate error message if the object has an appropriately named method, but it returns false") {
      val ex5 = intercept[TestFailedException] {
        List(1, 2) should be an (Symbol("empty"))
      }
      assert(ex5.message === Some("List(1, 2) was not an empty"))
      assert(ex5.failedCodeFileName === Some("ShouldBeAnSymbolSpec.scala"))
      assert(ex5.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    it("should throw TestFailedException if no <symbol> or is<Symbol> method exists") {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should be an (Symbol("apple"))
      }
      ex1.getMessage should equal ("NoPredicateMock has neither an apple nor an isApple method")
      // Check message for name that starts with a consonant (should use a instead of an)
      val ex2 = intercept[TestFailedException] {
        noPredicateMock should be an (Symbol("crabApple"))
      }
      ex2.getMessage should equal ("NoPredicateMock has neither a crabApple nor an isCrabApple method")
    }

    it("should do nothing if the object has an appropriately named method, which returns false when used with not") {
      notAppleMock should not { be an (Symbol("apple")) }
      notAppleMock should not be an (Symbol("apple"))
      isNotAppleMock should not { be an (Symbol("apple")) }
      isNotAppleMock should not be an (Symbol("apple"))
    }

    it("should throw TestFailedException if no <symbol> or is<Symbol> method exists, when used with not") {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should not { be an (Symbol("apple")) }
      }
      ex1.getMessage should equal ("NoPredicateMock has neither an apple nor an isApple method")
      val ex2 = intercept[TestFailedException] {
        noPredicateMock should not (be an (Symbol("orange")))
      }
      ex2.getMessage should equal ("NoPredicateMock has neither an orange nor an isOrange method")
      val ex3 = intercept[TestFailedException] {
        noPredicateMock should not be an (Symbol("apple"))
      }
      ex3.getMessage should equal ("NoPredicateMock has neither an apple nor an isApple method")
      val ex4 = intercept[TestFailedException] {
        noPredicateMock should not be an (Symbol("orange"))
      }
      ex4.getMessage should equal ("NoPredicateMock has neither an orange nor an isOrange method")
    }

    it("should do nothing if the object has an appropriately named method, which returns true, when used in a logical-and expression") {
      appleMock should ((be an (Symbol("apple"))) and (be an (Symbol("apple"))))
      appleMock should (be an (Symbol("apple")) and (be an (Symbol("apple"))))
      appleMock should (be an (Symbol("apple")) and be an (Symbol("apple")))
      isAppleMock should ((be an (Symbol("apple"))) and (be an (Symbol("apple"))))
      isAppleMock should (be an (Symbol("apple")) and (be an (Symbol("apple"))))
      isAppleMock should (be an (Symbol("apple")) and be an (Symbol("apple")))
    }

    it("should do nothing if the object has an appropriately named method, which returns true, when used in a logical-or expression") {

      appleMock should ((be an (Symbol("orange"))) or (be an (Symbol("apple"))))
      appleMock should (be an (Symbol("orange")) or (be an (Symbol("apple"))))
      appleMock should (be an (Symbol("orange")) or be an (Symbol("apple")))
      isAppleMock should ((be an (Symbol("orange"))) or (be an (Symbol("apple"))))
      isAppleMock should (be an (Symbol("orange")) or (be an (Symbol("apple"))))
      isAppleMock should (be an (Symbol("orange")) or be an (Symbol("apple")))

      appleMock should ((be an (Symbol("apple"))) or (be an (Symbol("orange"))))
      appleMock should (be an (Symbol("apple")) or (be an (Symbol("orange"))))
      appleMock should (be an (Symbol("apple")) or be an (Symbol("orange")))
      isAppleMock should ((be an (Symbol("apple"))) or (be an (Symbol("orange"))))
      isAppleMock should (be an (Symbol("apple")) or (be an (Symbol("orange"))))
      isAppleMock should (be an (Symbol("apple")) or be an (Symbol("orange")))
    }

    it("should do nothing if the object has an appropriately named method, which returns false, when used in a logical-and expression with not") {

      notAppleMock should (not (be an (Symbol("apple"))) and not (be an (Symbol("apple"))))
      notAppleMock should ((not be an (Symbol("apple"))) and (not be an (Symbol("apple"))))
      notAppleMock should (not be an (Symbol("apple")) and not be an (Symbol("apple")))

      isNotAppleMock should (not (be an (Symbol("apple"))) and not (be an (Symbol("apple"))))
      isNotAppleMock should ((not be an (Symbol("apple"))) and (not be an (Symbol("apple"))))
      isNotAppleMock should (not be an (Symbol("apple")) and not be an (Symbol("apple")))
    }

    it("should do nothing if the object has an appropriately named method, which returns false, when used in a logical-or expression with not") {

      notAppleMock should (not (be an (Symbol("apple"))) or not (be an (Symbol("apple"))))
      notAppleMock should ((not be an (Symbol("apple"))) or (not be an (Symbol("apple"))))
      notAppleMock should (not be an (Symbol("apple")) or not be an (Symbol("apple")))

      isNotAppleMock should (not (be an (Symbol("apple"))) or not (be an (Symbol("apple"))))
      isNotAppleMock should ((not be an (Symbol("apple"))) or (not be an (Symbol("apple"))))
      isNotAppleMock should (not be an (Symbol("apple")) or not be an (Symbol("apple")))

      notAppleMock should (not (be an (Symbol("orange"))) or not (be an (Symbol("apple"))))
      notAppleMock should ((not be an (Symbol("orange"))) or (not be an (Symbol("apple"))))
      notAppleMock should (not be an (Symbol("orange")) or not be an (Symbol("apple")))

      isNotAppleMock should (not (be an (Symbol("orange"))) or not (be an (Symbol("apple"))))
      isNotAppleMock should ((not be an (Symbol("orange"))) or (not be an (Symbol("apple"))))
      isNotAppleMock should (not be an (Symbol("orange")) or not be an (Symbol("apple")))
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns false") {
      val caught1 = intercept[TestFailedException] {
        notAppleMock should be an (Symbol("apple"))
      }
      assert(caught1.getMessage === "NotAppleMock was not an apple")
      val caught2 = intercept[TestFailedException] {
        isNotAppleMock should be an (Symbol("apple"))
      }
      assert(caught2.getMessage === "IsNotAppleMock was not an apple")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns true when used with not") {
      val caught1 = intercept[TestFailedException] {
        appleMock should not { be an (Symbol("apple")) }
      }
      assert(caught1.getMessage === "AppleMock was an apple")
      val caught2 = intercept[TestFailedException] {
        appleMock should not be an (Symbol("apple"))
      }
      assert(caught2.getMessage === "AppleMock was an apple")
      val caught3 = intercept[TestFailedException] {
        isAppleMock should not { be an (Symbol("apple")) }
      }
      assert(caught3.getMessage === "IsAppleMock was an apple")
      val caught4 = intercept[TestFailedException] {
        isAppleMock should not be an (Symbol("apple"))
      }
      assert(caught4.getMessage === "IsAppleMock was an apple")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-and expression") {
      val caught1 = intercept[TestFailedException] {
        appleMock should ((be an (Symbol("apple"))) and (be an (Symbol("orange"))))
      }
      assert(caught1.getMessage === "AppleMock was an apple, but AppleMock was not an orange")
      val caught2 = intercept[TestFailedException] {
        appleMock should (be an (Symbol("apple")) and (be an (Symbol("orange"))))
      }
      assert(caught2.getMessage === "AppleMock was an apple, but AppleMock was not an orange")
      val caught3 = intercept[TestFailedException] {
        appleMock should (be an (Symbol("apple")) and be an (Symbol("orange")))
      }
      assert(caught3.getMessage === "AppleMock was an apple, but AppleMock was not an orange")
      val caught4 = intercept[TestFailedException] {
        isAppleMock should ((be an (Symbol("apple"))) and (be an (Symbol("orange"))))
      }
      assert(caught4.getMessage === "IsAppleMock was an apple, but IsAppleMock was not an orange")
      val caught5 = intercept[TestFailedException] {
        isAppleMock should (be an (Symbol("apple")) and (be an (Symbol("orange"))))
      }
      assert(caught5.getMessage === "IsAppleMock was an apple, but IsAppleMock was not an orange")
      val caught6 = intercept[TestFailedException] {
        isAppleMock should (be an (Symbol("apple")) and be an (Symbol("orange")))
      }
      assert(caught6.getMessage === "IsAppleMock was an apple, but IsAppleMock was not an orange")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-or expression") {

      val caught1 = intercept[TestFailedException] {
        notAppleMock should ((be an (Symbol("apple"))) or (be an (Symbol("apple"))))
      }
      assert(caught1.getMessage === "NotAppleMock was not an apple, and NotAppleMock was not an apple")
      val caught2 = intercept[TestFailedException] {
        notAppleMock should (be an (Symbol("apple")) or (be an (Symbol("apple"))))
      }
      assert(caught2.getMessage === "NotAppleMock was not an apple, and NotAppleMock was not an apple")
      val caught3 = intercept[TestFailedException] {
        notAppleMock should (be an (Symbol("apple")) or be an (Symbol("apple")))
      }
      assert(caught3.getMessage === "NotAppleMock was not an apple, and NotAppleMock was not an apple")
      val caught4 = intercept[TestFailedException] {
        isNotAppleMock should ((be an (Symbol("apple"))) or (be an (Symbol("apple"))))
      }
      assert(caught4.getMessage === "IsNotAppleMock was not an apple, and IsNotAppleMock was not an apple")
      val caught5 = intercept[TestFailedException] {
        isNotAppleMock should (be an (Symbol("apple")) or (be an (Symbol("apple"))))
      }
      assert(caught5.getMessage === "IsNotAppleMock was not an apple, and IsNotAppleMock was not an apple")
      val caught6 = intercept[TestFailedException] {
        isNotAppleMock should (be an (Symbol("apple")) or be an (Symbol("apple")))
      }
      assert(caught6.getMessage === "IsNotAppleMock was not an apple, and IsNotAppleMock was not an apple")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-and expression with not") {

      val caught1 = intercept[TestFailedException] {
        appleMock should (not (be an (Symbol("orange"))) and not (be an (Symbol("apple"))))
      }
      assert(caught1.getMessage === "AppleMock was not an orange, but AppleMock was an apple")
      val caught2 = intercept[TestFailedException] {
        appleMock should ((not be an (Symbol("orange"))) and (not be an (Symbol("apple"))))
      }
      assert(caught2.getMessage === "AppleMock was not an orange, but AppleMock was an apple")
      val caught3 = intercept[TestFailedException] {
        appleMock should (not be an (Symbol("orange")) and not be an (Symbol("apple")))
      }
      assert(caught3.getMessage === "AppleMock was not an orange, but AppleMock was an apple")
      val caught4 = intercept[TestFailedException] {
        isAppleMock should (not (be an (Symbol("orange"))) and not (be an (Symbol("apple"))))
      }
      assert(caught4.getMessage === "IsAppleMock was not an orange, but IsAppleMock was an apple")
      val caught5 = intercept[TestFailedException] {
        isAppleMock should ((not be an (Symbol("orange"))) and (not be an (Symbol("apple"))))
      }
      assert(caught5.getMessage === "IsAppleMock was not an orange, but IsAppleMock was an apple")
      val caught6 = intercept[TestFailedException] {
        isAppleMock should (not be an (Symbol("orange")) and not be an (Symbol("apple")))
      }
      assert(caught6.getMessage === "IsAppleMock was not an orange, but IsAppleMock was an apple")
      // Check that the error message "short circuits"
      val caught7 = intercept[TestFailedException] {
        appleMock should (not (be an (Symbol("apple"))) and not (be an (Symbol("orange"))))
      }
      assert(caught7.getMessage === "AppleMock was an apple")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-or expression with not") {

      val caught1 = intercept[TestFailedException] {
        appleMock should (not (be an (Symbol("apple"))) or not (be an (Symbol("apple"))))
      }
      assert(caught1.getMessage === "AppleMock was an apple, and AppleMock was an apple")
      val caught2 = intercept[TestFailedException] {
        appleMock should ((not be an (Symbol("apple"))) or (not be an (Symbol("apple"))))
      }
      assert(caught2.getMessage === "AppleMock was an apple, and AppleMock was an apple")
      val caught3 = intercept[TestFailedException] {
        appleMock should (not be an (Symbol("apple")) or not be an (Symbol("apple")))
      }
      assert(caught3.getMessage === "AppleMock was an apple, and AppleMock was an apple")
      val caught4 = intercept[TestFailedException] {
        isAppleMock should (not (be an (Symbol("apple"))) or not (be an (Symbol("apple"))))
      }
      assert(caught4.getMessage === "IsAppleMock was an apple, and IsAppleMock was an apple")
      val caught5 = intercept[TestFailedException] {
        isAppleMock should ((not be an (Symbol("apple"))) or (not be an (Symbol("apple"))))
      }
      assert(caught5.getMessage === "IsAppleMock was an apple, and IsAppleMock was an apple")
      val caught6 = intercept[TestFailedException] {
        isAppleMock should (not be an (Symbol("apple")) or not be an (Symbol("apple")))
      }
      assert(caught6.getMessage === "IsAppleMock was an apple, and IsAppleMock was an apple")
    }
  }
}
