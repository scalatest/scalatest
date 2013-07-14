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

class ConversionCheckedLegacyTripleEqualsExplicitlySpec extends Spec with Matchers with ConversionCheckedLegacyTripleEquals with ExplicitlySpecHelpers {

  object `The Explicitly DSL` {
    object `when used with === on identical types` {
      def `should allow an Equality to be specified explicitly` {
        assert(1 !== 2)
        assert((1 === 2)(decided by intInequality))
        assert(1 === 1)
        assert((1 !== 1)(decided by intInequality))
      }
    }
    object `when used with toType === fromType` {
      def `should allow an Equality to be specified explicitly` {
        assert(new Fruit("orange") !== new Pomme)
        assert((new Fruit("orange") === new Pomme)(decided by fruitInequality))
        assert(new Fruit("apple") === new Pomme)
        assert((new Fruit("apple") !== new Pomme)(decided by fruitInequality))
      }
    }
    object `when used with fromType === toType` {
      def `should allow an Equality to be specified explicitly` {
        assert(new Pomme !== new Fruit("orange"))
        assert((new Pomme === new Fruit("orange"))(decided by fruitInequality))
        assert(new Pomme === new Fruit("apple"))
        assert((new Pomme !== new Fruit("apple"))(decided by fruitInequality))
      }
    }
  }
  object `The determined by syntax` {
    def `should produce an Equivalence if used with an Equivalence (that is not an Equality)` {
      assert(1 !== 2)
      1 should !== (2)

      assert((1 === 2)(determined by intInequivalence))
      (1 should === (2)) (determined by intInequivalence)
      // TODO: Try and make this work without the parens, i.e: 1 should === (2) (determined by intInequivalence)

      assert(1 === 1)
      1 should === (1)

      assert((1 !== 1)(determined by intInequivalence))
      (1 should !== (1)) (determined by intInequivalence)
    }
    def `should produce an Equivalence from "after being" syntax` {
      assert(("Hi" !== "hI"))
      assert { ("Hi" === "hI") (after being downCased) }
    }
    def `should produce an Equivalence from "after being X and Y" syntax` {
      assert((" Hi" !== "hI "))
      assert { (" Hi" === "hI ") (after being downCased and chopped) }
    }
/*
    def `should produce an Equivalence from "determined by <equivalence> afterBeing" syntax` {
      implicit val stringIneq = stringInequivalence
      assert(" Hi" === "hI ")
      assert { (" Hi" !== "hI ") (determined by defaultEquality[String]) }
      // assert { (" Hi" === "hI ") (after being downCased and chopped) }
    }
*/
  }
}

