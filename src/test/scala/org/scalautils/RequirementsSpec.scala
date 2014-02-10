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
package org.scalautils

import org.scalatest._

class RequirementsSpec extends FunSpec with Requirements with OptionValues {

  private def neverRuns1(f: => Unit): Boolean = true
  private def neverRuns2(f: => Unit)(a: Int): Boolean = true
  private def neverRuns3[T](f: => Unit)(a: T): Boolean = true

  def didNotEqual(left: Any, right: Any): String =
    left + " did not equal " + right

  def equaled(left: Any, right: Any): String =
    left + " equaled " + right

  describe("The require(boolean) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      require(a == 3)
    }

    it("should throw IllegalArgumentException when is used to check a == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 5 == b") {
      require(5 == b)
    }

    it("should throw IllegalArgumentException when is used to check 3 == b") {
      val e = intercept[IllegalArgumentException] {
        require(3 == b)
      }
      assert(e.getMessage == didNotEqual(3, b))
    }

    it("should do nothing when is used to check 3 == 3") {
      require(3 == 3)
    }

    it("should throw IllegalArgumentException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalArgumentException] {
        require(3 == 5)
      }
      assert(e.getMessage == "Expression was false")
    }

    it("should throw IllegalArgumentException when is used to check a == b") {
      val e = intercept[IllegalArgumentException] {
        require(a == b)
      }
      assert(e.getMessage == didNotEqual(a, b))
    }

    it("should throw IllegalArgumentException when is used to check a == null") {
      val e = intercept[IllegalArgumentException] {
        require(a == null)
      }
      assert(e.getMessage == didNotEqual(a, null))
    }

    it("should throw IllegalArgumentException when is used to check null == a") {
      val e = intercept[IllegalArgumentException] {
        require(null == a)
      }
      assert(e.getMessage == didNotEqual(null, a))
    }

    it("should do nothing when is used to check a === 3") {
      require(a === 3)
    }

    it("should throw IllegalArgumentException when is used to check a === 5") {
      val e = intercept[IllegalArgumentException] {
        require(a === 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 3 === a") {
      require(3 === a)
    }

    it("should throw IllegalArgumentException when is used to check 5 === a") {
      val e = intercept[IllegalArgumentException] {
        require(5 === a)
      }
      assert(e.getMessage == didNotEqual(5, a))
    }

    it("should do nothing when is used to check a !== 5") {
      require(a !== 5)
    }

    it("should throw IllegalArgumentException when is used to check a !== 3") {
      val e = intercept[IllegalArgumentException] {
        require(a !== 3)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check 5 !== a") {
      require(5 !== a)
    }

    it("should throw IllegalArgumentException when is used to check 3 !== a") {
      val e = intercept[IllegalArgumentException] {
        require(3 !== a)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      require(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      require(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      require(neverRuns3(sys.error("Sad times 3"))(0))
    }

  }

  describe("The require(boolean, clue) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      require(a == 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check a == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 5 == b") {
      require(5 == b, ", dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 == b") {
      val e = intercept[IllegalArgumentException] {
        require(3 == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, b) + ", dude")
    }

    it("should do nothing when is used to check a != 5") {
      require(a != 5, ". dude")
    }

    it("should throw IllegalArgumentException when is used to check a != 3") {
      val e = intercept[IllegalArgumentException] {
        require(a != 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 3 != b") {
      require(3 != b, "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 5 != b") {
      val e = intercept[IllegalArgumentException] {
        require(5 != b, "; dude")
      }
      assert(e.getMessage == equaled(5, b) + "; dude")
    }

    it("should do nothing when is used to check 3 == 3") {
      require(3 == 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalArgumentException] {
        require(3 == 5, "dude")
      }
      assert(e.getMessage == "dude")
    }

    it("should throw IllegalArgumentException when is used to check a == b") {
      val e = intercept[IllegalArgumentException] {
        require(a == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(a, b) + ", dude")
    }

    it("should throw IllegalArgumentException when is used to check a == null") {
      val e = intercept[IllegalArgumentException] {
        require(a == null, ". dude")
      }
      assert(e.getMessage == didNotEqual(a, null) + ". dude")
    }

    it("should throw IllegalArgumentException when is used to check null == a") {
      val e = intercept[IllegalArgumentException] {
        require(null == a, "; dude")
      }
      assert(e.getMessage == didNotEqual(null, a) + "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 != a") {
      val e = intercept[IllegalArgumentException] {
        require(3 != a, ", dude")
      }
      assert(e.getMessage == equaled(3, a) + ", dude")
    }

    it("should do nothing when is used to check 5 != a") {
      require(5 != a, ". dude")
    }

    it("should do nothing when is used to check a === 3") {
      require(a === 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check a === 5") {
      val e = intercept[IllegalArgumentException] {
        require(a === 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 3 === a") {
      require(3 === a, ", dude")
    }

    it("should throw IllegalArgumentException when is used to check 5 === a") {
      val e = intercept[IllegalArgumentException] {
        require(5 === a, ", dude")
      }
      assert(e.getMessage == didNotEqual(5, a) + ", dude")
    }

    it("should do nothing when is used to check a !== 5") {
      require(a !== 5, ". dude")
    }

    it("should throw IllegalArgumentException when is used to check a !== 3") {
      val e = intercept[IllegalArgumentException] {
        require(a !== 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 5 !== a") {
      require(5 !== a, "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 !== a") {
      val e = intercept[IllegalArgumentException] {
        require(3 !== a, "; dude")
      }
      assert(e.getMessage == equaled(3, a) + "; dude")
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      require(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      require(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      require(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
    }

  }

  describe("The requireState(boolean) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      requireState(a == 3)
    }

    it("should throw IllegalStateException when is used to check a == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 5 == b") {
      requireState(5 == b)
    }

    it("should throw IllegalStateException when is used to check 3 == b") {
      val e = intercept[IllegalStateException] {
        requireState(3 == b)
      }
      assert(e.getMessage == didNotEqual(3, b))
    }

    it("should do nothing when is used to check 3 == 3") {
      requireState(3 == 3)
    }

    it("should throw IllegalStateException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalStateException] {
        requireState(3 == 5)
      }
      assert(e.getMessage == "Expression was false")
    }

    it("should throw IllegalStateException when is used to check a == b") {
      val e = intercept[IllegalStateException] {
        requireState(a == b)
      }
      assert(e.getMessage == didNotEqual(a, b))
    }

    it("should throw IllegalStateException when is used to check a == null") {
      val e = intercept[IllegalStateException] {
        requireState(a == null)
      }
      assert(e.getMessage == didNotEqual(a, null))
    }

    it("should throw IllegalStateException when is used to check null == a") {
      val e = intercept[IllegalStateException] {
        requireState(null == a)
      }
      assert(e.getMessage == didNotEqual(null, a))
    }

    it("should do nothing when is used to check a === 3") {
      requireState(a === 3)
    }

    it("should throw IllegalStateException when is used to check a === 5") {
      val e = intercept[IllegalStateException] {
        requireState(a === 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 3 === a") {
      requireState(3 === a)
    }

    it("should throw IllegalStateException when is used to check 5 === a") {
      val e = intercept[IllegalStateException] {
        requireState(5 === a)
      }
      assert(e.getMessage == didNotEqual(5, a))
    }

    it("should do nothing when is used to check a !== 5") {
      requireState(a !== 5)
    }

    it("should throw IllegalStateException when is used to check a !== 3") {
      val e = intercept[IllegalStateException] {
        requireState(a !== 3)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check 5 !== a") {
      requireState(5 !== a)
    }

    it("should throw IllegalStateException when is used to check 3 !== a") {
      val e = intercept[IllegalStateException] {
        requireState(3 !== a)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      requireState(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      requireState(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      requireState(neverRuns3(sys.error("Sad times 3"))(0))
    }

  }

  describe("The requireState(boolean, clue) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      requireState(a == 3, "dude")
    }

    it("should throw IllegalStateException when is used to check a == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 5 == b") {
      requireState(5 == b, ", dude")
    }

    it("should throw IllegalStateException when is used to check 3 == b") {
      val e = intercept[IllegalStateException] {
        requireState(3 == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, b) + ", dude")
    }

    it("should do nothing when is used to check a != 5") {
      requireState(a != 5, ". dude")
    }

    it("should throw IllegalStateException when is used to check a != 3") {
      val e = intercept[IllegalStateException] {
        requireState(a != 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 3 != b") {
      requireState(3 != b, "; dude")
    }

    it("should throw IllegalStateException when is used to check 5 != b") {
      val e = intercept[IllegalStateException] {
        requireState(5 != b, "; dude")
      }
      assert(e.getMessage == equaled(5, b) + "; dude")
    }

    it("should do nothing when is used to check 3 == 3") {
      requireState(3 == 3, "dude")
    }

    it("should throw IllegalStateException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalStateException] {
        requireState(3 == 5, "dude")
      }
      assert(e.getMessage == "dude")
    }

    it("should throw IllegalStateException when is used to check a == b") {
      val e = intercept[IllegalStateException] {
        requireState(a == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(a, b) + ", dude")
    }

    it("should throw IllegalStateException when is used to check a == null") {
      val e = intercept[IllegalStateException] {
        requireState(a == null, ". dude")
      }
      assert(e.getMessage == didNotEqual(a, null) + ". dude")
    }

    it("should throw IllegalStateException when is used to check null == a") {
      val e = intercept[IllegalStateException] {
        requireState(null == a, "; dude")
      }
      assert(e.getMessage == didNotEqual(null, a) + "; dude")
    }

    it("should throw IllegalStateException when is used to check 3 != a") {
      val e = intercept[IllegalStateException] {
        requireState(3 != a, ", dude")
      }
      assert(e.getMessage == equaled(3, a) + ", dude")
    }

    it("should do nothing when is used to check 5 != a") {
      requireState(5 != a, ". dude")
    }

    it("should do nothing when is used to check a === 3") {
      requireState(a === 3, "dude")
    }

    it("should throw IllegalStateException when is used to check a === 5") {
      val e = intercept[IllegalStateException] {
        requireState(a === 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 3 === a") {
      requireState(3 === a, ", dude")
    }

    it("should throw IllegalStateException when is used to check 5 === a") {
      val e = intercept[IllegalStateException] {
        requireState(5 === a, ", dude")
      }
      assert(e.getMessage == didNotEqual(5, a) + ", dude")
    }

    it("should do nothing when is used to check a !== 5") {
      requireState(a !== 5, ". dude")
    }

    it("should throw IllegalStateException when is used to check a !== 3") {
      val e = intercept[IllegalStateException] {
        requireState(a !== 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 5 !== a") {
      requireState(5 !== a, "; dude")
    }

    it("should throw IllegalStateException when is used to check 3 !== a") {
      val e = intercept[IllegalStateException] {
        requireState(3 !== a, "; dude")
      }
      assert(e.getMessage == equaled(3, a) + "; dude")
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      requireState(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      requireState(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      requireState(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
    }

  }

  describe("The requireNonNull(...) method") {

    val prefix = "prefix text"
    val string = "some text"
    val suffix = "suffix tex"

    val nullPrefix = null
    val nullString = null
    val nullSuffix = null

    it("should do nothing when passed in parameters are non-null") {
      requireNonNull(prefix, string, suffix)
    }

    it("should throw NullPointerException with correct message when one of passed parameters is null") {
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, nullString, suffix)
      }
      assert(e.getMessage == "nullString was null")
    }

    it("should throw NullPointerException with correct message when two of passed parameters is null") {
      val e = intercept[NullPointerException] {
        requireNonNull(nullPrefix, nullString, suffix)
      }
      assert(e.getMessage == "nullPrefix and nullString were null")
    }

    it("should throw NullPointerException with correct message when three of passed parameters is null") {
      val e = intercept[NullPointerException] {
        requireNonNull(nullPrefix, nullString, nullSuffix)
      }
      assert(e.getMessage == "nullPrefix, nullString and nullSuffix were null")
    }

    it("should throw NullPointerException with correct message when one of passed parameters through object property is null") {
      class AClass {
        val aNull: String = null
      }
      val aClass = new AClass
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, aClass.aNull, suffix)
      }
      assert(e.getMessage == "aClass.aNull was null")
    }

    it("should throw NullPointerException with correct message when one of passed parameters through object method call is null") {
      class AClass {
        def returnNull: String = null
      }
      val aClass = new AClass
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, aClass.returnNull, suffix)
      }
      assert(e.getMessage == "aClass.returnNull was null")
    }

    it("should throw NullPointerException with correct message when one of passed parameters through method call is null") {
      def returnNull: String = null
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, returnNull, suffix)
      }
      assert(e.getMessage == "returnNull was null")
    }

  }

}