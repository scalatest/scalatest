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
package org.scalactic

import org.scalatest._

class NormalizationSpec extends Spec with StringNormalizations {

  object `A Uniformity` {
    object `when anded with another Uniformity` {
      def `should produce a Uniformity` { 
        assert(lowerCased.isInstanceOf[Uniformity[_]])
        assert((lowerCased and trimmed).isInstanceOf[Uniformity[_]])
      }
    }
    object `when anded with a regular Normalization (on left or right)` {
      val shouted: Normalization[String] = 
        new Normalization[String] {
          def normalized(s: String): String = s.toUpperCase
        }
      def `should produce a Normalization that is not also a Uniformity` { 
        assert(!shouted.isInstanceOf[Uniformity[_]])
        assert(trimmed.isInstanceOf[Uniformity[_]])
        val tAndS: Normalization[String] = trimmed and shouted
        assert(!tAndS.isInstanceOf[Uniformity[_]])
        val sAndT: Normalization[String] = shouted and trimmed
        assert(!sAndT.isInstanceOf[Uniformity[_]])
      }
    }
    def `can be converted to a NormalizingEquality that delegates to implicit Equality without using the Explicitly DSL` {
      assert(lowerCased.toEquality.areEqual("howdy", "HOWDY"))
      assert((lowerCased and trimmed).toEquality.areEqual(" howdy", "HOWDY "))
      assert(!lowerCased.toEquality.areEqual("howdy", "HOWDX"))
      assert(!(lowerCased and trimmed).toEquality.areEqual(" howdy", "HOWDX "))
      assert(!lowerCased.toEquality.areEqual("howdy", "XOWDY"))
      assert(!(lowerCased and trimmed).toEquality.areEqual(" howdy", "XOWDY "))
      implicit val firstCharStringEquality =
        new Equality[String] {
          def areEqual(a: String, b: Any): Boolean =
            b match {
              case bString: String => a(0) == bString(0)
              case _ => false
            }
        }
      assert(lowerCased.toEquality.areEqual("howdy", "HOWDY"))
      assert((lowerCased and trimmed).toEquality.areEqual(" howdy", "HOWDY "))
      assert(lowerCased.toEquality.areEqual("howdy", "HOWDX"))
      assert((lowerCased and trimmed).toEquality.areEqual(" howdy", "HOWDX "))
      assert(!lowerCased.toEquality.areEqual("howdy", "XOWDY"))
      assert(!(lowerCased and trimmed).toEquality.areEqual(" howdy", "XOWDY "))
    }
  }
  object `A Normalization` {
    def `can be converted to a NormalizingEquivalence that delegates to implicit Equivalence without using the Explicitly DSL` {
      assert(lowerCased.toEquivalence.areEquivalent("howdy", "HOWDY"))
      assert((lowerCased and trimmed).toEquivalence.areEquivalent(" howdy", "HOWDY "))
      assert(!lowerCased.toEquivalence.areEquivalent("howdy", "HOWDX"))
      assert(!(lowerCased and trimmed).toEquivalence.areEquivalent(" howdy", "HOWDX "))
      assert(!lowerCased.toEquivalence.areEquivalent("howdy", "XOWDY"))
      assert(!(lowerCased and trimmed).toEquivalence.areEquivalent(" howdy", "XOWDY "))
      implicit val firstCharStringEquivalence =
        new Equivalence[String] {
          def areEquivalent(a: String, b: String): Boolean = a(0) == b(0)
        }
      assert(lowerCased.toEquivalence.areEquivalent("howdy", "HOWDY"))
      assert((lowerCased and trimmed).toEquivalence.areEquivalent(" howdy", "HOWDY "))
      assert(lowerCased.toEquivalence.areEquivalent("howdy", "HOWDX"))
      assert((lowerCased and trimmed).toEquivalence.areEquivalent(" howdy", "HOWDX "))
      assert(!lowerCased.toEquivalence.areEquivalent("howdy", "XOWDY"))
      assert(!(lowerCased and trimmed).toEquivalence.areEquivalent(" howdy", "XOWDY "))
    }
  }
}

