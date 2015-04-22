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

class RecursiveEqualitySpec extends FunSpec with Matchers with NonImplicitAssertions {
  describe("An Option") {

    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under the all policies by default") {

      // New policies
      // Both sides Some
      new UncheckedEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new CheckedEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new EnabledEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      // Both sides Option
      new UncheckedEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new CheckedEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new EnabledEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      // Left side Some, right side Option
      new UncheckedEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new CheckedEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new EnabledEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      // Left side Option, right side Some
      new UncheckedEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new CheckedEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new EnabledEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }

      // Deprecated policies
      // Both sides Some
      new TripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      // Both sides Option
      new TripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      // Left side Some, right side Option
      new TripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      // Left side Option, right side Some
      new TripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
    }

    it("should do recursive equality under any policy under RecursiveOptionEquality") {

      import RecursiveOptionEquality._

      // New policies
      // Both sides Some
      new UncheckedEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new CheckedEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new EnabledEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      // Both sides Option
      new UncheckedEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new CheckedEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new EnabledEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Some, right side Option
      new UncheckedEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new CheckedEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new EnabledEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Option, right side Some
      new UncheckedEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new CheckedEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new EnabledEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }

      // Deprecated policies
      // Both sides Some
      new TripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      // Both sides Option
      new TripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Some, right side Option
      new TripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Option, right side Some
      new TripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
    }
  }
}

