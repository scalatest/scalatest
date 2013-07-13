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
  }
}

