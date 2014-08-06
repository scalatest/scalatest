/*
 * Copyright 2001-2014 Artima, Inc.
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

class RecursiveEqualitySpec extends Spec with Matchers with NonImplicitAssertions {
  object `An Option` {
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    def `should do recursive equality under the new policies` {
      new UncheckedEquality { Some("hi") shouldEqual Some("HI") }
      new CheckedEquality { Some("hi") shouldEqual Some("HI") }
      new EnabledEquality { Some("hi") shouldEqual Some("HI") }
    }
    def `should NOT do recursive equality under the old, deprecated policies` {
      new TripleEquals { Some("hi") should not equal Some("HI") }
      new TypeCheckedTripleEquals { Some("hi") should not equal Some("HI") }
      new ConversionCheckedTripleEquals { Some("hi") should not equal Some("HI") }
    }
  }
}

