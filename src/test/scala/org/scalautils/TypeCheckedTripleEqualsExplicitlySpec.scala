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

class TypeCheckedTripleEqualsExplicitlySpec extends Spec with Matchers with TypeCheckedTripleEquals {

  val inequality = 
    new Equality[Int] {
      def areEqual(a: Int, b: Any): Boolean = a != b
    }

  object `The Explicitly DSL` {
    object `when used with ===` {
      def `should allow an Equality to specified explicitly` {
        assert((1 === 2)(decided by inequality))
        assert((1 !== 1)(decided by inequality))
      }
    }
  }
}

