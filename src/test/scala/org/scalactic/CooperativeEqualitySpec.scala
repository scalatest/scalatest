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

class CooperativeEqualitySpec extends Spec with Matchers with NonImplicitAssertions {
  object `An Option` {

    implicit val enabler = EnabledEqualityBetween[Int, Complex]

    def `should only be comparable if a CooperativeEqualityConstraint exists between their element types under Checked- and EnabledEquality` {

      // New policies
      // Both sides Some
      new UncheckedEquality { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      "new CheckedEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      // Both sides Option
      new UncheckedEquality { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      "new CheckedEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      // Left side Some, right side Option
      new UncheckedEquality { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      "new CheckedEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      // Left side Option, right side Some
      new UncheckedEquality { Option(42) should not equal Some(Complex(42.0, 0.0)) }
      "new CheckedEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      implicit class Asifier[T](o: T) {
        def as[U](implicit cnv: T => U): U = cnv(o)
      }
      new CheckedEquality { Option(42.as[Complex]) shouldEqual Some(Complex(42.0, 0.0)) }
      "new EnabledEquality { Option(42.as[Complex]) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      implicit val enableComplexComparisons = EnabledEqualityFor[Complex]
      new EnabledEquality { Option(42.as[Complex]) shouldEqual Some(Complex(42.0, 0.0)) }

      // Deprecated policies
      // Both sides Some
      new TripleEquals { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      // Both sides Option
      new TripleEquals { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      // Left side Some, right side Option
      new TripleEquals { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      // Left side Option, right side Some
      new TripleEquals { Option(42) should not equal Some(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Option(42) should not equal Some(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Option(42) should not equal Some(Complex(42.0, 0.0)) }
    }

    def `should be comparable if any EqualityConstraint exists between their element types under RecursiveOptionEquality` {

      import RecursiveOptionEquality._

      // New policies
      // Both sides Some
      new UncheckedEquality { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      new CheckedEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
      new EnabledEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
      // Both sides Option
      new UncheckedEquality { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      new CheckedEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
      new EnabledEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
      // Left side Some, right side Option
      new UncheckedEquality { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      new CheckedEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
      new EnabledEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
      // Left side Option, right side Some
      new UncheckedEquality { Option(42) should not equal Some(Complex(42.0, 0.0)) }
      // These shouldn't type check because the implicit widening conversion goes
      // from Int, which is inside Option, to Complex, which is inside Some, but
      // you can't convert an Option to a Some safely.
      "new CheckedEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck

      // Deprecated policies
      // Both sides Some
      new TripleEquals { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      // Both sides Option
      new TripleEquals { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      // Left side Some, right side Option
      new TripleEquals { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      // Left side Option, right side Some
      new TripleEquals { Option(42) should not equal Some(Complex(42.0, 0.0)) }
      new TypeCheckedTripleEquals { Option(42) should not equal Some(Complex(42.0, 0.0)) }
      new ConversionCheckedTripleEquals { Option(42) should not equal Some(Complex(42.0, 0.0)) }
    }
  }
}

