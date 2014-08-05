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
    def `should allow two Strings to be compared` {
      new EnabledEquality { "hi" shouldEqual "hi" }
    }
    def `should not allow a DigitString to be compared with an Int, despite the DigitString => Int implicit conversion` {
      val agent007 = DigitString("007")
      agent007 should not equal DigitString("07")
      DigitString("007") should equal (agent007)
      new UncheckedEquality { agent007 should not equal 7 }
      new UncheckedEquality { 7 should not equal agent007 }
      new ConversionCheckedTripleEquals { assert(agent007 === 7) }
      new ConversionCheckedTripleEquals { assert(7 === agent007) }
      new TypeCheckedTripleEquals { "assert(agent007 === 7)" shouldNot typeCheck }
      new TypeCheckedTripleEquals { "assert(7 === agent007)" shouldNot typeCheck }
      new CheckedEquality { "agent007 should not equal 7" shouldNot typeCheck }
      new CheckedEquality { "7 should not equal agent007" shouldNot typeCheck }
      new EnabledEquality { "agent007 should not equal 7" shouldNot typeCheck }
      new EnabledEquality { "7 should not equal agent007" shouldNot typeCheck }
    }
    def `should not allow a Double to be compared with a Complex, despite the Double => Complex implicit conversion` {
      val seven = Complex(7.0, 0.0)
      seven should not equal Complex(7.0, 1.0)
      Complex(7.0, 0.0) should equal (seven)
      new UncheckedEquality { seven should not equal 7.0 }
      new UncheckedEquality { 7.0 should not equal seven }
      new ConversionCheckedTripleEquals { assert(seven === 7.0) }
      new ConversionCheckedTripleEquals { assert(7.0 === seven) }
      new TypeCheckedTripleEquals { "assert(seven === 7.0)" shouldNot typeCheck }
      new TypeCheckedTripleEquals { "assert(7.0 === seven)" shouldNot typeCheck }
      new CheckedEquality { "seven should not equal 7.0" shouldNot typeCheck }
      new CheckedEquality { "7.0 should not equal seven" shouldNot typeCheck }
      new EnabledEquality { "seven should not equal 7.0" shouldNot typeCheck }
      new EnabledEquality { "7.0 should not equal seven" shouldNot typeCheck }
    }
/*
    def `should allow a Double to be compared with a Complex, if an implicit EqualityEnabledBetween[Double, Complex] exists` {
      implicit val enabler = EqualityEnabledBetween[Double, Complex]
      val seven = Complex(7.0, 0.0)
      seven should not equal Complex(7.0, 1.0)
      Complex(7.0, 0.0) should equal (seven)
      new UncheckedEquality { seven should not equal 7.0 }
      new UncheckedEquality { 7.0 should not equal seven }
      new ConversionCheckedTripleEquals { assert(seven === 7.0) }
      new ConversionCheckedTripleEquals { assert(7.0 === seven) }
      new TypeCheckedTripleEquals { assert(seven === 7.0) }
      new TypeCheckedTripleEquals { assert(7.0 === seven) }
      new CheckedEquality { seven should not equal 7.0 }
      new CheckedEquality { 7.0 should not equal seven }
      new EnabledEquality { seven should not equal 7.0 }
      new EnabledEquality { 7.0 should not equal seven }
    }
*/
  }
}

