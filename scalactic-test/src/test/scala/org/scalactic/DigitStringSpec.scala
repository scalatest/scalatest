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

class DigitStringSpec extends FunSpec with Matchers {
  describe("A DigitString ") {
    it("should throw IllegalArgumentException if a non-digit or empty string is passed") {
      an [IllegalArgumentException] should be thrownBy { DigitString("00B") }
      an [IllegalArgumentException] should be thrownBy { DigitString("") }
      an [IllegalArgumentException] should be thrownBy { DigitString("CafeBabe") }
    }
    it("should throw IllegalArgumentException if digits are passed that represent an number greater than Int.MaxValue or Int.MinValue") {
      /*
      scala> Int.MaxValue
      res1: Int = 2147483647
      
      scala> "2147483647".toInt
      res2: Int = 2147483647
      
      scala> "2147483648".toInt
      java.lang.NumberFormatException: For input string: "2147483648"
        at java.lang.NumberFormatException.forInputString(NumberFormatException.java:48)
        at java.lang.Integer.parseInt(Integer.java:465)
        at java.lang.Integer.parseInt(Integer.java:499)
        at scala.collection.immutable.StringLike$class.toInt(StringLike.scala:241)
        at scala.collection.immutable.StringOps.toInt(StringOps.scala:30)
        ... 32 elided
      
      scala> Int.MinValue
      res4: Int = -2147483648
      
      scala> "-2147483648".toInt
      res5: Int = -2147483648
      
      scala> "-2147483649".toInt
      java.lang.NumberFormatException: For input string: "-2147483649"
        at java.lang.NumberFormatException.forInputString(NumberFormatException.java:48)
        at java.lang.Integer.parseInt(Integer.java:465)
        at java.lang.Integer.parseInt(Integer.java:499)
        at scala.collection.immutable.StringLike$class.toInt(StringLike.scala:241)
        at scala.collection.immutable.StringOps.toInt(StringOps.scala:30)
        ... 32 elided
      */
      noException should be thrownBy { DigitString("2147483647") }
      an [IllegalArgumentException] should be thrownBy { DigitString("2147483648") }
      noException should be thrownBy { DigitString("-2147483648") }
      an [IllegalArgumentException] should be thrownBy { DigitString("-2147483649") }
      an [IllegalArgumentException] should be thrownBy { DigitString("-1-") }
    }
    it("should equal another DigitString if both the strings are equal") {
      DigitString("5") should equal (DigitString("5"))
      DigitString("06") should equal (DigitString("06"))
      DigitString("007") should equal (DigitString("007"))
      DigitString("08") should not equal (DigitString("008"))
    }
    it("can be used where an Int is needed") {
      def iKnowYouAreButWhatAmI(i: Int): Int = i
      iKnowYouAreButWhatAmI(7) should equal (7)
      iKnowYouAreButWhatAmI(DigitString("7")) should equal (7)
      iKnowYouAreButWhatAmI(DigitString("07")) should equal (7)
      iKnowYouAreButWhatAmI(DigitString("007")) should equal (7)
    }
    it("can give you an Int by invoking toInt directly on it") {
      DigitString("007").toInt should equal (7)
    }
  }
}

