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

class EnabledEqualitySpec extends Spec with Matchers with NonImplicitAssertions {
  object `EnabledEquality ` {
    def `should not allow two function1's to be compared` {
      val fun = (i: Int) => i + 1
      new UncheckedEquality { fun shouldEqual fun }
      new CheckedEquality { fun shouldEqual fun }
      new EnabledEquality { "fun shouldEqual fun" shouldNot typeCheck }
    }
    def `should not allow anything to be compared with Any` {

      new UncheckedEquality { "hi" should not equal 1 }
      new UncheckedEquality { "hi" should not equal (1: Any) }
      new UncheckedEquality { ("hi": Any) should not equal 1 }

      new CheckedEquality { """"hi" should not equal 1""" shouldNot typeCheck }
      new CheckedEquality { "hi" should not equal (1: Any) }
      new CheckedEquality { ("hi": Any) should not equal 1 }

      new EnabledEquality { """"hi" shouldEqual fun""" shouldNot typeCheck }
      new EnabledEquality { """"hi" should not equal (1: Any)""" shouldNot typeCheck }
      new EnabledEquality { """("hi": Any) should not equal 1""" shouldNot typeCheck }
    }
  }
}

