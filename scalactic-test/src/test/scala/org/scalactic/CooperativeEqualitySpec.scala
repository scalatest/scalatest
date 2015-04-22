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

class CooperativeEqualitySpec extends FunSpec with Matchers with NonImplicitAssertions {
  describe("Two Options") {

    describe("when cooperative Equality instances are defined for both element types") {

      implicit val intEquality =
        new Equality[Int] {
          def areEqual(a: Int, b: Any): Boolean =
            b match {
              case complex: Complex => complex.imaginary == 0 && complex.real == a
              case simple => simple == a
            }
        }

      implicit val complexEquality =
        new Equality[Complex] {
          def areEqual(a: Complex, b: Any): Boolean =
            b match {
              case int: Int => a.imaginary == 0 && a.real == int
              case other => other == a
            }
        }

      implicit val enabler = EnabledEqualityBetween[Int, Complex]

      // This is a sanity check, a test to make sure the test is testing what
      // I think it is testing
      it("should be comparable under Checked- and EnabledEquality outside of Options") {
        // New policies
        // Both sides Some
        new UncheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new CheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new EnabledEquality { 42 shouldEqual Complex(42.0, 0.0) }
        // Both sides Option
        new UncheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new CheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new EnabledEquality { 42 shouldEqual Complex(42.0, 0.0) }
        // Left side Some, right side Option
        new UncheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new CheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new EnabledEquality { 42 shouldEqual Complex(42.0, 0.0) }
        // Left side Option, right side Some
        new UncheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new CheckedEquality { 42 shouldEqual Complex(42.0, 0.0) }
        new EnabledEquality { 42 shouldEqual Complex(42.0, 0.0) }
  
        // Deprecated policies
        // Both sides Some
        new TripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new TypeCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new ConversionCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        // Both sides Option
        new TripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new TypeCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new ConversionCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        // Left side Some, right side Option
        new TripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new TypeCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new ConversionCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        // Left side Option, right side Some
        new TripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new TypeCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
        new ConversionCheckedTripleEquals { 42 shouldEqual Complex(42.0, 0.0) }
      }

      it("should not by default be comparable under Checked- and EnabledEquality") {
  
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
        import AsMethods._
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
  
      it("should be comparable under Checked- and EnabledEquality if also under RecursiveOptionEquality") {
  
        import RecursiveOptionEquality._
  
        // New policies
        // Both sides Some
        new UncheckedEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new CheckedEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new EnabledEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
        // Both sides Option
        new UncheckedEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new CheckedEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new EnabledEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
        // Left side Some, right side Option
        new UncheckedEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new CheckedEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new EnabledEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
        // Left side Option, right side Some
        new UncheckedEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new CheckedEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new EnabledEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }
  
        // Deprecated policies
        // Both sides Some
        new TripleEquals { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new TypeCheckedTripleEquals { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new ConversionCheckedTripleEquals { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }
        // Both sides Option
        new TripleEquals { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new TypeCheckedTripleEquals { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new ConversionCheckedTripleEquals { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }
        // Left side Some, right side Option
        new TripleEquals { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new TypeCheckedTripleEquals { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
        new ConversionCheckedTripleEquals { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }
        // Left side Option, right side Some
        new TripleEquals { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new TypeCheckedTripleEquals { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }
        new ConversionCheckedTripleEquals { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }
      }
    }
  }
}

