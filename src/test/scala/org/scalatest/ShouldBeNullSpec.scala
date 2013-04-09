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
package org.scalatest

import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import org.scalatest.exceptions.TestFailedException

class ShouldBeNullSpec extends Spec with Matchers with Checkers with ReturnsNormallyThrowsAssertion {

  val nullMap: Map[Int, String] = null
  val map = Map(1 -> "one", 2 -> "two")

  object `the be (null) syntax` {

    def `should work in its basic for with or without not` {

      map should not be (null)
      map should not (be (null))
      map should (not be (null))
      map should (not (be (null)))
      nullMap should be (null)
      nullMap should (be (null))
      // null should be (null) this doesn't compile. can't do implicits on the Null type, which is fine
      // null should (be (null))

      val caught1 = intercept[TestFailedException] {
        map should be (null)
      }
      assert(caught1.getMessage === "Map(1 -> one, 2 -> two) was not null")

      val caught2 = intercept[TestFailedException] {
        map should (be (null))
      }
      assert(caught2.getMessage === "Map(1 -> one, 2 -> two) was not null")

      val caught3 = intercept[TestFailedException] {
        nullMap should not be (null)
      }
      assert(caught3.getMessage === "The reference was null")

      val caught4 = intercept[TestFailedException] {
        nullMap should not (be (null))
      }
      assert(caught4.getMessage === "The reference was null")

      val caught5 = intercept[TestFailedException] {
        nullMap should not ((be (null)))
      }
      assert(caught5.getMessage === "The reference was null")
    }

    def `should throw a NullPointerException if they try to short circuit with a null check first` {
      // The reason I check this is I warn that this will happen in the ShouldMatcher scaladoc
      intercept[NullPointerException] {
        nullMap should (not be (null) and contain key (7))
      }
      intercept[NullPointerException] {
        nullMap should (be (null) and contain key (7))
      }
    }

    def `should compile and run when used with and (with or without not)` {

      val caught1 = intercept[TestFailedException] {
        Map(1 -> "one") should (contain key (7) and not be (null))
      }
      assert(caught1.getMessage === "Map(1 -> one) did not contain key 7")

      val caught2 = intercept[TestFailedException] {
        Map(1 -> "one") should (contain key (7) and not (be (null)))
      }
      assert(caught2.getMessage === "Map(1 -> one) did not contain key 7")

      val caught3 = intercept[TestFailedException] {
        Map(1 -> "one") should (contain key (7) and (not (be (null))))
      }
      assert(caught3.getMessage === "Map(1 -> one) did not contain key 7")

      Map(1 -> "one") should (contain key (1) and not be (null))
      Map(1 -> "one") should (contain key (1) and not (be (null)))
      Map(1 -> "one") should (contain key (1) and (not (be (null))))

      val caught4 = intercept[TestFailedException] {
        Map(1 -> "one") should (contain key (1) and be (null))
      }
      assert(caught4.getMessage === "Map(1 -> one) contained key 1, but Map(1 -> one) was not null")

      val caught5 = intercept[TestFailedException] {
        Map(1 -> "one") should (contain key (1) and (be (null)))
      }
      assert(caught5.getMessage === "Map(1 -> one) contained key 1, but Map(1 -> one) was not null")

      val caught6 = intercept[TestFailedException] {
        Map(1 -> "one") should (be (null) and not be (null))
      }
      assert(caught6.getMessage === "Map(1 -> one) was not null")

      val caught7 = intercept[TestFailedException] {
        Map(1 -> "one") should (be (null) and not (be (null)))
      }
      assert(caught7.getMessage === "Map(1 -> one) was not null")

      val caught8 = intercept[TestFailedException] {
        Map(1 -> "one") should (be (null) and (not (be (null))))
      }
      assert(caught8.getMessage === "Map(1 -> one) was not null")

      nullMap should (be (null) and be (null))
      nullMap should (be (null) and (be (null)))

      val caught9 = intercept[TestFailedException] {
        nullMap should (be (null) and not be (null))
      }
      assert(caught9.getMessage === "The reference was null, but the reference was null")

      val caught10 = intercept[TestFailedException] {
        nullMap should (be (null) and not (be (null)))
      }
      assert(caught10.getMessage === "The reference was null, but the reference was null")

      val caught11 = intercept[TestFailedException] {
        nullMap should (be (null) and (not (be (null))))
      }
      assert(caught11.getMessage === "The reference was null, but the reference was null")
    }

    def `should compile and run when used with or (with or without not)` {

      Map(1 -> "one") should (contain key (7) or not be (null))
      Map(1 -> "one") should (contain key (7) or not (be (null)))
      Map(1 -> "one") should (contain key (7) or (not (be (null))))

      Map(1 -> "one") should (contain key (1) or not be (null))
      Map(1 -> "one") should (contain key (1) or not (be (null)))
      Map(1 -> "one") should (contain key (1) or (not (be (null))))

      val caught4 = intercept[TestFailedException] {
        Map(1 -> "one") should (contain key (7) or be (null))
      }
      assert(caught4.getMessage === "Map(1 -> one) did not contain key 7, and Map(1 -> one) was not null")

      val caught5 = intercept[TestFailedException] {
        Map(1 -> "one") should (contain key (7) or (be (null)))
      }
      assert(caught5.getMessage === "Map(1 -> one) did not contain key 7, and Map(1 -> one) was not null")

      val caught6 = intercept[TestFailedException] {
        Map(1 -> "one") should (be (null) or be (null))
      }
      assert(caught6.getMessage === "Map(1 -> one) was not null, and Map(1 -> one) was not null")

      val caught7 = intercept[TestFailedException] {
        Map(1 -> "one") should (be (null) or (be (null)))
      }
      assert(caught7.getMessage === "Map(1 -> one) was not null, and Map(1 -> one) was not null")

      nullMap should (be (null) or be (null))
      nullMap should (be (null) or (be (null)))
      nullMap should (be (null) or not be (null))
      nullMap should (be (null) or not (be (null)))
      nullMap should (be (null) or (not (be (null))))
      nullMap should (not be (null) or be (null))
      nullMap should (not (be (null)) or be (null))
    }
  }

  object `the shouldBe null syntax` {

    def `should work in its basic` {
      nullMap shouldBe null

      val caught1 = intercept[TestFailedException] {
        map shouldBe null
      }
      assert(caught1.getMessage === "Map(1 -> one, 2 -> two) was not null")
    }
  }
}

