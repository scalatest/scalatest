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
package org.scalatest

import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import org.scalactic.{Prettifier, TypeCheckedTripleEquals}
import org.scalatest.funspec.AnyFunSpec

class AssertionsWithTypeCheckedTripleEqualsSpec extends AnyFunSpec with TypeCheckedTripleEquals {

  val fileName: String = "AssertionsWithTypeCheckedTripleEqualsSpec.scala"

  private val prettifier = Prettifier.default

  def didNotEqual(left: Any, right: Any): String = {
    val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
    FailureMessages.didNotEqual(prettifier, leftee, rightee)
  }

  def equaled(left: Any, right: Any): String =
    FailureMessages.equaled(prettifier, left, right)

  describe("The assert(boolean) method") {
    val a = 3

    it("should do nothing when is used to check a === 3 with TypeCheckedTripleEquals") {
      assert(a === 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5 with TypeCheckedTripleEquals") {
      val e = intercept[TestFailedException] {
        assert(a === 5)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a with TypeCheckedTripleEquals") {
      assert(3 === a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a with TypeCheckedTripleEquals ") {
      val e = intercept[TestFailedException] {
        assert(5 === a)
      }
      assert(e.message === Some(didNotEqual(5, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5 using TypeCheckedTripleEquals") {
      assert(a !== 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3 using TypeCheckedTripleEquals") {
      val e = intercept[TestFailedException] {
        assert(a !== 3)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a using TypeCheckedTripleEquals") {
      assert(5 !== a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a using TypeCheckedTripleEquals") {
      val e = intercept[TestFailedException] {
        assert(3 !== a)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

  }

  describe("The assert(boolean, clue) method") {

    val a = 3

    it("should do nothing when is used to check a === 3 using TypeCheckedTripleEquals") {
      assert(a === 3, "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5 using TypeCheckedTripleEquals") {
      val e = intercept[TestFailedException] {
        assert(a === 5, "dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a using TypeCheckedTripleEquals") {
      assert(3 === a, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a using TypeCheckedTripleEquals") {
      val e = intercept[TestFailedException] {
        assert(5 === a, ", dude")
      }
      assert(e.message === Some(didNotEqual(5, 3) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5 using TypeCheckedTripleEquals") {
      assert(a !== 5, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3 using TypeCheckedTripleEquals") {
      val e = intercept[TestFailedException] {
        assert(a !== 3, ". dude")
      }
      assert(e.message === Some(equaled(3, 3) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a using TypeCheckedTripleEquals") {
      assert(5 !== a, "; dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a using TypeCheckedTripleEquals") {
      val e = intercept[TestFailedException] {
        assert(3 !== a, "; dude")
      }
      assert(e.message === Some(equaled(3, 3) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

  }

}