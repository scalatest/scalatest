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
package org.scalactic

import org.scalatest._

class TypeCheckedTripleEqualsExplicitlySpec extends funspec.AnyFunSpec with matchers.should.Matchers with TypeCheckedTripleEquals with ExplicitlySpecHelpers {

  describe("The Explicitly DSL") {
    describe("when used with === on identical types") {
      it("should allow an Equality to be specified explicitly") {
        assert(1 !== 2)
        assert((1 === 2)(decided by intInequality))
        assert(1 === 1)
        assert((1 !== 1)(decided by intInequality))

        implicit val strIneq = stringInequality
        assert(" Hi" === "hI ")
        assert { (" Hi" !== "hI ") (decided by defaultEquality[String]) }
      }
    }
    describe("when used with supertype === subtype") {
      it("should allow an Equality to be specified explicitly") {
        assert(new Fruit("orange") !== new Apple)
        assert((new Fruit("orange") === new Apple)(decided by fruitInequality))
        assert(new Fruit("apple") === new Apple)
        assert((new Fruit("apple") !== new Apple)(decided by fruitInequality))
      }
    }
    describe("when used with subtype === supertype") {
      it("should allow an Equality to be specified explicitly") {
        assert(new Apple !== new Fruit("orange"))
        assert((new Apple === new Fruit("orange"))(decided by fruitInequality))
        assert(new Apple === new Fruit("apple"))
        assert((new Apple !== new Fruit("apple"))(decided by fruitInequality))
      }
    }
  }
  describe("The determined by syntax") {
    it("should produce an Equivalence if used with an Equivalence (that is not an Equality)") {
      assert(1 !== 2)
      1 should !== (2)

      assert((1 === 2)(determined by intInequivalence))
      (1 should === (2)) (determined by intInequivalence)

      assert(1 === 1)
      1 should === (1)

      assert((1 !== 1)(determined by intInequivalence))
      (1 should !== (1)) (determined by intInequivalence)
    }
    it("should produce an Equivalence from \"after being\" syntax") {
      assert(("Hi" !== "hI"))
      assert(("Hi" === "hI")(after being downCased))
    }
    it("should produce an Equivalence from \"after being X and Y\" syntax") {
      assert((" Hi" !== "hI "))
      assert((" Hi" === "hI ")(after being downCased and chopped))
    }
    it("should produce an Equivalence from \"determined by <equivalence> afterBeing\" syntax") {
      implicit val stringIneq = stringInequivalence
      assert(("Hi" === "hI"))
      assert { ("Hi" !== "hI") (after being downCased) }
      assert { ("Hi" === "hI") (determined by defaultEquality[String] afterBeing downCased) }
    }
  }
}

