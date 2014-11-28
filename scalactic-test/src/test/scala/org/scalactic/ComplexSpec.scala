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

class ComplexSpec extends Spec with Matchers {
  object `A Complex ` {
    def `should equal another Complex if both the real and imaginary parts are equal` {
      Complex(2.0, 3.0) should equal (Complex(2.0, 3.0))
      Complex(2.0, 3.0) should not equal (Complex(1.0, 3.0))
      Complex(2.0, 3.0) should not equal (Complex(2.0, 1.0))
      Complex(2.0, 3.0) should not equal (Complex(1.0, 1.0))
    }
    def `should have implicit conversions from DigitString, Int, and Double` {
      def iKnowYouAreButWhatAmI(c: Complex): Complex = c
      iKnowYouAreButWhatAmI(Complex(2.0, 3.0)) should equal (Complex(2.0, 3.0))
      iKnowYouAreButWhatAmI(3.0) should equal (Complex(3.0, 0.0))
      iKnowYouAreButWhatAmI(4) should equal (Complex(4.0, 0.0))
      iKnowYouAreButWhatAmI(DigitString("007")) should equal (Complex(7.0, 0.0))
    }
  }
}

