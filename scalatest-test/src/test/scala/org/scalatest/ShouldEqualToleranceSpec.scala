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
package org.scalatest

import org.scalactic._
import Matchers._
import exceptions.TestFailedException

class ShouldEqualToleranceSpec extends FunSpec with Tolerance {

  val sevenDotOh = 7.0
  val minusSevenDotOh = -7.0
  val sevenDotOhFloat = 7.0f
  val minusSevenDotOhFloat = -7.0f
  val sevenLong = 7L
  val minusSevenLong = -7L
  val sevenInt = 7
  val minusSevenInt = -7
  val sevenShort: Short = 7
  val minusSevenShort: Short = -7
  val sevenByte: Byte = 7
  val minusSevenByte: Byte = -7

  /*
    I decided that for X +- Y, Y can be any numeric type that's implicitly
    convertible to X. So if X is Double, Y could be Double, Float, Long, Int, Short, Byte.
    If X is Long, Y could be Long, Int, Short, Byte. If X is Short, Y could be Short or Byte.
    And if X is Byte, Y must be Byte.
    assert(minusSevenDotOhFloat === (-6.8f +- 0.2d))
  */
 /* Chose not to do the symmetry, because no one needs it and implementing it would require an implicit. So these fail:
      (7.1 +- 0.2) should equal sevenDotOh
      (7.5 +- 0.2) should not equal sevenDotOh
 */
  describe("The should equal syntax") {

    it("should succeed if the number is within the given interval") {

      // Double +- Double
      sevenDotOh should equal (7.1 +- 0.2)
      sevenDotOh should equal (6.9 +- 0.2)
      sevenDotOh should equal (7.0 +- 0.2)
      sevenDotOh should equal (7.2 +- 0.2)
      sevenDotOh should equal (6.8 +- 0.2)
      minusSevenDotOh should equal (-7.1 +- 0.2)
      minusSevenDotOh should equal (-6.9 +- 0.2)
      minusSevenDotOh should equal (-7.0 +- 0.2)
      minusSevenDotOh should equal (-7.2 +- 0.2)
      minusSevenDotOh should equal (-6.8 +- 0.2)

      // Double +- Float
      sevenDotOh should equal (7.1 +- 0.2f)
      sevenDotOh should equal (6.9 +- 0.2f)
      sevenDotOh should equal (7.0 +- 0.2f)
      sevenDotOh should equal (7.2 +- 0.2f)
      sevenDotOh should equal (6.8 +- 0.2f)
      minusSevenDotOh should equal (-7.1 +- 0.2f)
      minusSevenDotOh should equal (-6.9 +- 0.2f)
      minusSevenDotOh should equal (-7.0 +- 0.2f)
      minusSevenDotOh should equal (-7.2 +- 0.2f)
      minusSevenDotOh should equal (-6.8 +- 0.2f)

      // Double +- Long
      sevenDotOh should equal (7.1 +- 2L)
      sevenDotOh should equal (6.9 +- 2L)
      sevenDotOh should equal (7.0 +- 2L)
      sevenDotOh should equal (7.2 +- 2L)
      sevenDotOh should equal (6.8 +- 2L)
      minusSevenDotOh should equal (-7.1 +- 2L)
      minusSevenDotOh should equal (-6.9 +- 2L)
      minusSevenDotOh should equal (-7.0 +- 2L)
      minusSevenDotOh should equal (-7.2 +- 2L)
      minusSevenDotOh should equal (-6.8 +- 2L)

      // Double +- Int
      sevenDotOh should equal (7.1 +- 2)
      sevenDotOh should equal (6.9 +- 2)
      sevenDotOh should equal (7.0 +- 2)
      sevenDotOh should equal (7.2 +- 2)
      sevenDotOh should equal (6.8 +- 2)
      minusSevenDotOh should equal (-7.1 +- 2)
      minusSevenDotOh should equal (-6.9 +- 2)
      minusSevenDotOh should equal (-7.0 +- 2)
      minusSevenDotOh should equal (-7.2 +- 2)
      minusSevenDotOh should equal (-6.8 +- 2)

      // Double +- Short
      sevenDotOh should equal (7.1 +- 2.toShort)
      sevenDotOh should equal (6.9 +- 2.toShort)
      sevenDotOh should equal (7.0 +- 2.toShort)
      sevenDotOh should equal (7.2 +- 2.toShort)
      sevenDotOh should equal (6.8 +- 2.toShort)
      minusSevenDotOh should equal (-7.1 +- 2.toShort)
      minusSevenDotOh should equal (-6.9 +- 2.toShort)
      minusSevenDotOh should equal (-7.0 +- 2.toShort)
      minusSevenDotOh should equal (-7.2 +- 2.toShort)
      minusSevenDotOh should equal (-6.8 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should equal (7.1 +- 2.toByte)
      sevenDotOh should equal (6.9 +- 2.toByte)
      sevenDotOh should equal (7.0 +- 2.toByte)
      sevenDotOh should equal (7.2 +- 2.toByte)
      sevenDotOh should equal (6.8 +- 2.toByte)
      minusSevenDotOh should equal (-7.1 +- 2.toByte)
      minusSevenDotOh should equal (-6.9 +- 2.toByte)
      minusSevenDotOh should equal (-7.0 +- 2.toByte)
      minusSevenDotOh should equal (-7.2 +- 2.toByte)
      minusSevenDotOh should equal (-6.8 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should equal (7.1f +- 0.2f)
      sevenDotOhFloat should equal (6.9f +- 0.2f)
      sevenDotOhFloat should equal (7.0f +- 0.2f)
      sevenDotOhFloat should equal (7.2f +- 0.2f)
      sevenDotOhFloat should equal (6.8f +- 0.2f)
      minusSevenDotOhFloat should equal (-7.1f +- 0.2f)
      minusSevenDotOhFloat should equal (-6.9f +- 0.2f)
      minusSevenDotOhFloat should equal (-7.0f +- 0.2f)
      minusSevenDotOhFloat should equal (-7.2f +- 0.2f)
      minusSevenDotOhFloat should equal (-6.8f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should equal (7.1f +- 2L)
      sevenDotOhFloat should equal (6.9f +- 2L)
      sevenDotOhFloat should equal (7.0f +- 2L)
      sevenDotOhFloat should equal (7.2f +- 2L)
      sevenDotOhFloat should equal (6.8f +- 2L)
      minusSevenDotOhFloat should equal (-7.1f +- 2L)
      minusSevenDotOhFloat should equal (-6.9f +- 2L)
      minusSevenDotOhFloat should equal (-7.0f +- 2L)
      minusSevenDotOhFloat should equal (-7.2f +- 2L)
      minusSevenDotOhFloat should equal (-6.8f +- 2L)

      // Float +- Int
      sevenDotOhFloat should equal (7.1f +- 2)
      sevenDotOhFloat should equal (6.9f +- 2)
      sevenDotOhFloat should equal (7.0f +- 2)
      sevenDotOhFloat should equal (7.2f +- 2)
      sevenDotOhFloat should equal (6.8f +- 2)
      minusSevenDotOhFloat should equal (-7.1f +- 2)
      minusSevenDotOhFloat should equal (-6.9f +- 2)
      minusSevenDotOhFloat should equal (-7.0f +- 2)
      minusSevenDotOhFloat should equal (-7.2f +- 2)
      minusSevenDotOhFloat should equal (-6.8f +- 2)

      // Float +- Short
      sevenDotOhFloat should equal (7.1f +- 2.toShort)
      sevenDotOhFloat should equal (6.9f +- 2.toShort)
      sevenDotOhFloat should equal (7.0f +- 2.toShort)
      sevenDotOhFloat should equal (7.2f +- 2.toShort)
      sevenDotOhFloat should equal (6.8f +- 2.toShort)
      minusSevenDotOhFloat should equal (-7.1f +- 2.toShort)
      minusSevenDotOhFloat should equal (-6.9f +- 2.toShort)
      minusSevenDotOhFloat should equal (-7.0f +- 2.toShort)
      minusSevenDotOhFloat should equal (-7.2f +- 2.toShort)
      minusSevenDotOhFloat should equal (-6.8f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should equal (7.1f +- 2.toByte)
      sevenDotOhFloat should equal (6.9f +- 2.toByte)
      sevenDotOhFloat should equal (7.0f +- 2.toByte)
      sevenDotOhFloat should equal (7.2f +- 2.toByte)
      sevenDotOhFloat should equal (6.8f +- 2.toByte)
      minusSevenDotOhFloat should equal (-7.1f +- 2.toByte)
      minusSevenDotOhFloat should equal (-6.9f +- 2.toByte)
      minusSevenDotOhFloat should equal (-7.0f +- 2.toByte)
      minusSevenDotOhFloat should equal (-7.2f +- 2.toByte)
      minusSevenDotOhFloat should equal (-6.8f +- 2.toByte)

      // Long +- Long
      sevenLong should equal (9L +- 2L)
      sevenLong should equal (8L +- 2L)
      sevenLong should equal (7L +- 2L)
      sevenLong should equal (6L +- 2L)
      sevenLong should equal (5L +- 2L)
      minusSevenLong should equal (-9L +- 2L)
      minusSevenLong should equal (-8L +- 2L)
      minusSevenLong should equal (-7L +- 2L)
      minusSevenLong should equal (-6L +- 2L)
      minusSevenLong should equal (-5L +- 2L)

      // Long +- Int
      sevenLong should equal (9L +- 2)
      sevenLong should equal (8L +- 2)
      sevenLong should equal (7L +- 2)
      sevenLong should equal (6L +- 2)
      sevenLong should equal (5L +- 2)
      minusSevenLong should equal (-9L +- 2)
      minusSevenLong should equal (-8L +- 2)
      minusSevenLong should equal (-7L +- 2)
      minusSevenLong should equal (-6L +- 2)
      minusSevenLong should equal (-5L +- 2)

      // Long +- Short
      sevenLong should equal (9L +- 2.toShort)
      sevenLong should equal (8L +- 2.toShort)
      sevenLong should equal (7L +- 2.toShort)
      sevenLong should equal (6L +- 2.toShort)
      sevenLong should equal (5L +- 2.toShort)
      minusSevenLong should equal (-9L +- 2.toShort)
      minusSevenLong should equal (-8L +- 2.toShort)
      minusSevenLong should equal (-7L +- 2.toShort)
      minusSevenLong should equal (-6L +- 2.toShort)
      minusSevenLong should equal (-5L +- 2.toShort)

      // Long +- Byte
      sevenLong should equal (9L +- 2.toByte)
      sevenLong should equal (8L +- 2.toByte)
      sevenLong should equal (7L +- 2.toByte)
      sevenLong should equal (6L +- 2.toByte)
      sevenLong should equal (5L +- 2.toByte)
      minusSevenLong should equal (-9L +- 2.toByte)
      minusSevenLong should equal (-8L +- 2.toByte)
      minusSevenLong should equal (-7L +- 2.toByte)
      minusSevenLong should equal (-6L +- 2.toByte)
      minusSevenLong should equal (-5L +- 2.toByte)

      // Int +- Int
      sevenInt should equal (9 +- 2)
      sevenInt should equal (8 +- 2)
      sevenInt should equal (7 +- 2)
      sevenInt should equal (6 +- 2)
      sevenInt should equal (5 +- 2)
      minusSevenInt should equal (-9 +- 2)
      minusSevenInt should equal (-8 +- 2)
      minusSevenInt should equal (-7 +- 2)
      minusSevenInt should equal (-6 +- 2)
      minusSevenInt should equal (-5 +- 2)

      // Int +- Short
      sevenInt should equal (9 +- 2.toShort)
      sevenInt should equal (8 +- 2.toShort)
      sevenInt should equal (7 +- 2.toShort)
      sevenInt should equal (6 +- 2.toShort)
      sevenInt should equal (5 +- 2.toShort)
      minusSevenInt should equal (-9 +- 2.toShort)
      minusSevenInt should equal (-8 +- 2.toShort)
      minusSevenInt should equal (-7 +- 2.toShort)
      minusSevenInt should equal (-6 +- 2.toShort)
      minusSevenInt should equal (-5 +- 2.toShort)

      // Int +- Byte
      sevenInt should equal (9 +- 2.toByte)
      sevenInt should equal (8 +- 2.toByte)
      sevenInt should equal (7 +- 2.toByte)
      sevenInt should equal (6 +- 2.toByte)
      sevenInt should equal (5 +- 2.toByte)
      minusSevenInt should equal (-9 +- 2.toByte)
      minusSevenInt should equal (-8 +- 2.toByte)
      minusSevenInt should equal (-7 +- 2.toByte)
      minusSevenInt should equal (-6 +- 2.toByte)
      minusSevenInt should equal (-5 +- 2.toByte)

      // Short +- Short
      sevenShort should equal (9.toShort +- 2.toShort)
      sevenShort should equal (8.toShort +- 2.toShort)
      sevenShort should equal (7.toShort +- 2.toShort)
      sevenShort should equal (6.toShort +- 2.toShort)
      sevenShort should equal (5.toShort +- 2.toShort)
      minusSevenShort should equal ((-9).toShort +- 2.toShort)
      minusSevenShort should equal ((-8).toShort +- 2.toShort)
      minusSevenShort should equal ((-7).toShort +- 2.toShort)
      minusSevenShort should equal ((-6).toShort +- 2.toShort)
      minusSevenShort should equal ((-5).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should equal (9.toShort +- 2.toByte)
      sevenShort should equal (8.toShort +- 2.toByte)
      sevenShort should equal (7.toShort +- 2.toByte)
      sevenShort should equal (6.toShort +- 2.toByte)
      sevenShort should equal (5.toShort +- 2.toByte)
      minusSevenShort should equal ((-9).toShort +- 2.toByte)
      minusSevenShort should equal ((-8).toShort +- 2.toByte)
      minusSevenShort should equal ((-7).toShort +- 2.toByte)
      minusSevenShort should equal ((-6).toShort +- 2.toByte)
      minusSevenShort should equal ((-5).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should equal (9.toByte +- 2.toByte)
      sevenByte should equal (8.toByte +- 2.toByte)
      sevenByte should equal (7.toByte +- 2.toByte)
      sevenByte should equal (6.toByte +- 2.toByte)
      sevenByte should equal (5.toByte +- 2.toByte)
      minusSevenByte should equal ((-9).toByte +- 2.toByte)
      minusSevenByte should equal ((-8).toByte +- 2.toByte)
      minusSevenByte should equal ((-7).toByte +- 2.toByte)
      minusSevenByte should equal ((-6).toByte +- 2.toByte)
      minusSevenByte should equal ((-5).toByte +- 2.toByte)
    }

    it("should throw TFE if the number is outside the given interval") {

      // Double +- Double
      val caught = intercept[TestFailedException] { sevenDotOh should equal (7.5 +- 0.2) }
      assert(caught.getMessage === sevenDotOh + " did not equal 7.5 plus or minus 0.2")
      assertThrows[TestFailedException] { sevenDotOh should equal (6.5 +- 0.2) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-7.5 +- 0.2) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-6.5 +- 0.2) }

      // Double +- Float
      assertThrows[TestFailedException] { sevenDotOh should equal (7.5 +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOh should equal (6.5 +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-7.5 +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-6.5 +- 0.2f) }

      // Double +- Long
      assertThrows[TestFailedException] { sevenDotOh should equal (4.0 +- 2L) }
      assertThrows[TestFailedException] { sevenDotOh should equal (9.1 +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2L) }

      // Double +- Int
      assertThrows[TestFailedException] { sevenDotOh should equal (4.0 +- 2) }
      assertThrows[TestFailedException] { sevenDotOh should equal (9.1 +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2) }

      // Double +- Short
      assertThrows[TestFailedException] { sevenDotOh should equal (4.0 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOh should equal (9.1 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2.toShort) }

      // Double +- Byte
      assertThrows[TestFailedException] { sevenDotOh should equal (4.0 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOh should equal (9.1 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2.toByte) }

      // Float +- Float
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (7.5f +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (6.5f +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-7.5f +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-6.5f +- 0.2f) }

      // Float +- Long
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2L) }
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2L) }

      // Float +- Int
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2) }
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2) }

      // Float +- Short
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2.toShort) }

      // Float +- Byte
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2.toByte) }

      // Long +- Long
      assertThrows[TestFailedException] { sevenLong should equal (4L +- 2L) }
      assertThrows[TestFailedException] { sevenLong should equal (10L +- 2L) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-4L +- 2L) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-10L +- 2L) }

      // Long +- Int
      assertThrows[TestFailedException] { sevenLong should equal (4L +- 2) }
      assertThrows[TestFailedException] { sevenLong should equal (10L +- 2) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-4L +- 2) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-10L +- 2) }

      // Long +- Short
      assertThrows[TestFailedException] { sevenLong should equal (4L +- 2.toShort) }
      assertThrows[TestFailedException] { sevenLong should equal (10L +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-4L +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-10L +- 2.toShort) }

      // Long +- Byte
      assertThrows[TestFailedException] { sevenLong should equal (4L +- 2.toByte) }
      assertThrows[TestFailedException] { sevenLong should equal (10L +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-4L +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenLong should equal (-10L +- 2.toByte) }

      // Int +- Int
      assertThrows[TestFailedException] { sevenInt should equal (4 +- 2) }
      assertThrows[TestFailedException] { sevenInt should equal (10 +- 2) }
      assertThrows[TestFailedException] { minusSevenInt should equal (-4 +- 2) }
      assertThrows[TestFailedException] { minusSevenInt should equal (-10 +- 2) }

      // Int +- Short
      assertThrows[TestFailedException] { sevenInt should equal (4 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenInt should equal (10 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenInt should equal (-4 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenInt should equal (-10 +- 2.toShort) }

      // Int +- Byte
      assertThrows[TestFailedException] { sevenInt should equal (4 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenInt should equal (10 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenInt should equal (-4 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenInt should equal (-10 +- 2.toByte) }

      // Short +- Short
      assertThrows[TestFailedException] { sevenShort should equal (4.toShort +- 2.toShort) }
      assertThrows[TestFailedException] { sevenShort should equal (10.toShort +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenShort should equal ((-4).toShort +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenShort should equal ((-10).toShort +- 2.toShort) }

      // Short +- Byte
      assertThrows[TestFailedException] { sevenShort should equal (4.toShort +- 2.toByte) }
      assertThrows[TestFailedException] { sevenShort should equal (10.toShort +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenShort should equal ((-4).toShort +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenShort should equal ((-10).toShort +- 2.toByte) }

      // Byte +- Byte
      assertThrows[TestFailedException] { sevenByte should equal (4.toByte +- 2.toByte) }
      assertThrows[TestFailedException] { sevenByte should equal (10.toByte +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenByte should equal ((-4).toByte +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenByte should equal ((-10).toByte +- 2.toByte) }
    }

    it("should succeed if the number is outside the given interval when used with not") {

      // Double +- Double
      sevenDotOh should not equal (7.5 +- 0.2)
      sevenDotOh should not equal (6.5 +- 0.2)
      minusSevenDotOh should not equal (-7.5 +- 0.2)
      minusSevenDotOh should not equal (-6.5 +- 0.2)

      // Double +- Float
      sevenDotOh should not equal (7.5 +- 0.2f)
      sevenDotOh should not equal (6.5 +- 0.2f)
      minusSevenDotOh should not equal (-7.5 +- 0.2f)
      minusSevenDotOh should not equal (-6.5 +- 0.2f)

      // Double +- Long
      sevenDotOh should not equal (4.0 +- 2L)
      sevenDotOh should not equal (9.1 +- 2L)
      minusSevenDotOh should not equal (-4.0 +- 2L)
      minusSevenDotOh should not equal (-9.1 +- 2L)

      // Double +- Int
      sevenDotOh should not equal (4.0 +- 2)
      sevenDotOh should not equal (9.1 +- 2)
      minusSevenDotOh should not equal (-4.0 +- 2)
      minusSevenDotOh should not equal (-9.1 +- 2)

      // Double +- Short
      sevenDotOh should not equal (4.0 +- 2.toShort)
      sevenDotOh should not equal (9.1 +- 2.toShort)
      minusSevenDotOh should not equal (-4.0 +- 2.toShort)
      minusSevenDotOh should not equal (-9.1 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should not equal (4.0 +- 2.toByte)
      sevenDotOh should not equal (9.1 +- 2.toByte)
      minusSevenDotOh should not equal (-4.0 +- 2.toByte)
      minusSevenDotOh should not equal (-9.1 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should not equal (7.5f +- 0.2f)
      sevenDotOhFloat should not equal (6.5f +- 0.2f)
      minusSevenDotOhFloat should not equal (-7.5f +- 0.2f)
      minusSevenDotOhFloat should not equal (-6.5f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should not equal (4.0f +- 2L)
      sevenDotOhFloat should not equal (9.1f +- 2L)
      minusSevenDotOhFloat should not equal (-4.0f +- 2L)
      minusSevenDotOhFloat should not equal (-9.1f +- 2L)

      // Float +- Int
      sevenDotOhFloat should not equal (4.0f +- 2)
      sevenDotOhFloat should not equal (9.1f +- 2)
      minusSevenDotOhFloat should not equal (-4.0f +- 2)
      minusSevenDotOhFloat should not equal (-9.1f +- 2)

      // Float +- Short
      sevenDotOhFloat should not equal (4.0f +- 2.toShort)
      sevenDotOhFloat should not equal (9.1f +- 2.toShort)
      minusSevenDotOhFloat should not equal (-4.0f +- 2.toShort)
      minusSevenDotOhFloat should not equal (-9.1f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should not equal (4.0f +- 2.toByte)
      sevenDotOhFloat should not equal (9.1f +- 2.toByte)
      minusSevenDotOhFloat should not equal (-4.0f +- 2.toByte)
      minusSevenDotOhFloat should not equal (-9.1f +- 2.toByte)

      // Long +- Long
      sevenLong should not equal (4L +- 2L)
      sevenLong should not equal (10L +- 2L)
      minusSevenLong should not equal (-4L +- 2L)
      minusSevenLong should not equal (-10L +- 2L)

      // Long +- Int
      sevenLong should not equal (4L +- 2)
      sevenLong should not equal (10L +- 2)
      minusSevenLong should not equal (-4L +- 2)
      minusSevenLong should not equal (-10L +- 2)

      // Long +- Short
      sevenLong should not equal (4L +- 2.toShort)
      sevenLong should not equal (10L +- 2.toShort)
      minusSevenLong should not equal (-4L +- 2.toShort)
      minusSevenLong should not equal (-10L +- 2.toShort)

      // Long +- Byte
      sevenLong should not equal (4L +- 2.toByte)
      sevenLong should not equal (10L +- 2.toByte)
      minusSevenLong should not equal (-4L +- 2.toByte)
      minusSevenLong should not equal (-10L +- 2.toByte)

      // Int +- Int
      sevenInt should not equal (4 +- 2)
      sevenInt should not equal (10 +- 2)
      minusSevenInt should not equal (-4 +- 2)
      minusSevenInt should not equal (-10 +- 2)

      // Int +- Short
      sevenInt should not equal (4 +- 2.toShort)
      sevenInt should not equal (10 +- 2.toShort)
      minusSevenInt should not equal (-4 +- 2.toShort)
      minusSevenInt should not equal (-10 +- 2.toShort)

      // Int +- Byte
      sevenInt should not equal (4 +- 2.toByte)
      sevenInt should not equal (10 +- 2.toByte)
      minusSevenInt should not equal (-4 +- 2.toByte)
      minusSevenInt should not equal (-10 +- 2.toByte)

      // Short +- Short
      sevenShort should not equal (4.toShort +- 2.toShort)
      sevenShort should not equal (10.toShort +- 2.toShort)
      minusSevenShort should not equal ((-4).toShort +- 2.toShort)
      minusSevenShort should not equal ((-10).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should not equal (4.toShort +- 2.toByte)
      sevenShort should not equal (10.toShort +- 2.toByte)
      minusSevenShort should not equal ((-4).toShort +- 2.toByte)
      minusSevenShort should not equal ((-10).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should not equal (4.toByte +- 2.toByte)
      sevenByte should not equal (10.toByte +- 2.toByte)
      minusSevenByte should not equal ((-4).toByte +- 2.toByte)
      minusSevenByte should not equal ((-10).toByte +- 2.toByte)
    }

    it("should throw TFE if the number is within the given interval when used with not") {

      // Double +- Double
      val caught = intercept[TestFailedException] { sevenDotOh should not equal (7.1 +- 0.2) }
      assert(caught.getMessage === sevenDotOh + " equaled 7.1 plus or minus 0.2")
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.9 +- 0.2) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.0 +- 0.2) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.2 +- 0.2) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.8 +- 0.2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 0.2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 0.2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 0.2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 0.2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 0.2) }

      // Double +- Float
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.1 +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.9 +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.0 +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.2 +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.8 +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 0.2f) }

      // Double +- Long
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.1 +- 2L) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.9 +- 2L) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.0 +- 2L) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.2 +- 2L) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.8 +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2L) }

      // Double +- Int
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.1 +- 2) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.9 +- 2) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.0 +- 2) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.2 +- 2) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.8 +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2) }

      // Double +- Short
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.1 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.9 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.0 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.2 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.8 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2.toShort) }

      // Double +- Byte
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.1 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.9 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.0 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (7.2 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOh should not equal (6.8 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2.toByte) }

      // Float +- Float
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 0.2f) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 0.2f) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 0.2f) }

      // Float +- Long
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2L) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2L) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2L) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2L) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2L) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2L) }

      // Float +- Int
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2) }

      // Float +- Short
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2.toShort) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2.toShort) }

      // Float +- Byte
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2.toByte) }
      assertThrows[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2.toByte) }

      // Long +- Long
      assertThrows[TestFailedException] { sevenLong should not equal (9L +- 2L) }
      assertThrows[TestFailedException] { sevenLong should not equal (8L +- 2L) }
      assertThrows[TestFailedException] { sevenLong should not equal (7L +- 2L) }
      assertThrows[TestFailedException] { sevenLong should not equal (6L +- 2L) }
      assertThrows[TestFailedException] { sevenLong should not equal (5L +- 2L) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-9L +- 2L) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-8L +- 2L) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-7L +- 2L) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-6L +- 2L) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-5L +- 2L) }

      // Long +- Int
      assertThrows[TestFailedException] { sevenLong should not equal (9L +- 2) }
      assertThrows[TestFailedException] { sevenLong should not equal (8L +- 2) }
      assertThrows[TestFailedException] { sevenLong should not equal (7L +- 2) }
      assertThrows[TestFailedException] { sevenLong should not equal (6L +- 2) }
      assertThrows[TestFailedException] { sevenLong should not equal (5L +- 2) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-9L +- 2) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-8L +- 2) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-7L +- 2) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-6L +- 2) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-5L +- 2) }

      // Long +- Short
      assertThrows[TestFailedException] { sevenLong should not equal (9L +- 2.toShort) }
      assertThrows[TestFailedException] { sevenLong should not equal (8L +- 2.toShort) }
      assertThrows[TestFailedException] { sevenLong should not equal (7L +- 2.toShort) }
      assertThrows[TestFailedException] { sevenLong should not equal (6L +- 2.toShort) }
      assertThrows[TestFailedException] { sevenLong should not equal (5L +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-9L +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-8L +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-7L +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-6L +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-5L +- 2.toShort) }

      // Long +- Byte
      assertThrows[TestFailedException] { sevenLong should not equal (9L +- 2.toByte) }
      assertThrows[TestFailedException] { sevenLong should not equal (8L +- 2.toByte) }
      assertThrows[TestFailedException] { sevenLong should not equal (7L +- 2.toByte) }
      assertThrows[TestFailedException] { sevenLong should not equal (6L +- 2.toByte) }
      assertThrows[TestFailedException] { sevenLong should not equal (5L +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-9L +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-8L +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-7L +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-6L +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenLong should not equal (-5L +- 2.toByte) }

      // Int +- Int
      assertThrows[TestFailedException] { sevenInt should not equal (9 +- 2) }
      assertThrows[TestFailedException] { sevenInt should not equal (8 +- 2) }
      assertThrows[TestFailedException] { sevenInt should not equal (7 +- 2) }
      assertThrows[TestFailedException] { sevenInt should not equal (6 +- 2) }
      assertThrows[TestFailedException] { sevenInt should not equal (5 +- 2) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-9 +- 2) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-8 +- 2) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-7 +- 2) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-6 +- 2) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-5 +- 2) }

      // Int +- Short
      assertThrows[TestFailedException] { sevenInt should not equal (9 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenInt should not equal (8 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenInt should not equal (7 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenInt should not equal (6 +- 2.toShort) }
      assertThrows[TestFailedException] { sevenInt should not equal (5 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-9 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-8 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-7 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-6 +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-5 +- 2.toShort) }

      // Int +- Byte
      assertThrows[TestFailedException] { sevenInt should not equal (9 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenInt should not equal (8 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenInt should not equal (7 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenInt should not equal (6 +- 2.toByte) }
      assertThrows[TestFailedException] { sevenInt should not equal (5 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-9 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-8 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-7 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-6 +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenInt should not equal (-5 +- 2.toByte) }

      // Short +- Short
      assertThrows[TestFailedException] { sevenShort should not equal (9.toShort +- 2.toShort) }
      assertThrows[TestFailedException] { sevenShort should not equal (8.toShort +- 2.toShort) }
      assertThrows[TestFailedException] { sevenShort should not equal (7.toShort +- 2.toShort) }
      assertThrows[TestFailedException] { sevenShort should not equal (6.toShort +- 2.toShort) }
      assertThrows[TestFailedException] { sevenShort should not equal (5.toShort +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-9).toShort +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-8).toShort +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-7).toShort +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-6).toShort +- 2.toShort) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-5).toShort +- 2.toShort) }

      // Short +- Byte
      assertThrows[TestFailedException] { sevenShort should not equal (9.toShort +- 2.toByte) }
      assertThrows[TestFailedException] { sevenShort should not equal (8.toShort +- 2.toByte) }
      assertThrows[TestFailedException] { sevenShort should not equal (7.toShort +- 2.toByte) }
      assertThrows[TestFailedException] { sevenShort should not equal (6.toShort +- 2.toByte) }
      assertThrows[TestFailedException] { sevenShort should not equal (5.toShort +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-9).toShort +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-8).toShort +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-7).toShort +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-6).toShort +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenShort should not equal ((-5).toShort +- 2.toByte) }

      // Byte +- Byte
      assertThrows[TestFailedException] { sevenByte should not equal (9.toByte +- 2.toByte) }
      assertThrows[TestFailedException] { sevenByte should not equal (8.toByte +- 2.toByte) }
      assertThrows[TestFailedException] { sevenByte should not equal (7.toByte +- 2.toByte) }
      assertThrows[TestFailedException] { sevenByte should not equal (6.toByte +- 2.toByte) }
      assertThrows[TestFailedException] { sevenByte should not equal (5.toByte +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenByte should not equal ((-9).toByte +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenByte should not equal ((-8).toByte +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenByte should not equal ((-7).toByte +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenByte should not equal ((-6).toByte +- 2.toByte) }
      assertThrows[TestFailedException] { minusSevenByte should not equal ((-5).toByte +- 2.toByte) }
    }

    it("should succeed when equal and used in a logical-and expression") {
      // Double +- Double
      sevenDotOh should (equal (7.1 +- 0.2) and equal (7.1 +- 0.2))
      sevenDotOh should (equal (6.9 +- 0.2) and equal (6.9 +- 0.2))
      sevenDotOh should (equal (7.0 +- 0.2) and equal (7.0 +- 0.2))
      sevenDotOh should (equal (7.2 +- 0.2) and equal (7.2 +- 0.2))
      sevenDotOh should (equal (6.8 +- 0.2) and equal (6.8 +- 0.2))
      minusSevenDotOh should (equal (-7.1 +- 0.2) and equal (-7.1 +- 0.2))
      minusSevenDotOh should (equal (-6.9 +- 0.2) and equal (-6.9 +- 0.2))
      minusSevenDotOh should (equal (-7.0 +- 0.2) and equal (-7.0 +- 0.2))
      minusSevenDotOh should (equal (-7.2 +- 0.2) and equal (-7.2 +- 0.2))
      minusSevenDotOh should (equal (-6.8 +- 0.2) and equal (-6.8 +- 0.2))

      // Double +- Float
      sevenDotOh should (equal (7.1 +- 0.2f) and equal (7.1 +- 0.2f))
      sevenDotOh should (equal (6.9 +- 0.2f) and equal (6.9 +- 0.2f))
      sevenDotOh should (equal (7.0 +- 0.2f) and equal (7.0 +- 0.2f))
      sevenDotOh should (equal (7.2 +- 0.2f) and equal (7.2 +- 0.2f))
      sevenDotOh should (equal (6.8 +- 0.2f) and equal (6.8 +- 0.2f))
      minusSevenDotOh should (equal (-7.1 +- 0.2f) and equal (-7.1 +- 0.2f))
      minusSevenDotOh should (equal (-6.9 +- 0.2f) and equal (-6.9 +- 0.2f))
      minusSevenDotOh should (equal (-7.0 +- 0.2f) and equal (-7.0 +- 0.2f))
      minusSevenDotOh should (equal (-7.2 +- 0.2f) and equal (-7.2 +- 0.2f))
      minusSevenDotOh should (equal (-6.8 +- 0.2f) and equal (-6.8 +- 0.2f))

      // Double +- Long
      sevenDotOh should (equal (7.1 +- 2L) and equal (7.1 +- 2L))
      sevenDotOh should (equal (6.9 +- 2L) and equal (6.9 +- 2L))
      sevenDotOh should (equal (7.0 +- 2L) and equal (7.0 +- 2L))
      sevenDotOh should (equal (7.2 +- 2L) and equal (7.2 +- 2L))
      sevenDotOh should (equal (6.8 +- 2L) and equal (6.8 +- 2L))
      minusSevenDotOh should (equal (-7.1 +- 2L) and equal (-7.1 +- 2L))
      minusSevenDotOh should (equal (-6.9 +- 2L) and equal (-6.9 +- 2L))
      minusSevenDotOh should (equal (-7.0 +- 2L) and equal (-7.0 +- 2L))
      minusSevenDotOh should (equal (-7.2 +- 2L) and equal (-7.2 +- 2L))
      minusSevenDotOh should (equal (-6.8 +- 2L) and equal (-6.8 +- 2L))

      // Double +- Int
      sevenDotOh should (equal (7.1 +- 2) and equal (7.1 +- 2))
      sevenDotOh should (equal (6.9 +- 2) and equal (6.9 +- 2))
      sevenDotOh should (equal (7.0 +- 2) and equal (7.0 +- 2))
      sevenDotOh should (equal (7.2 +- 2) and equal (7.2 +- 2))
      sevenDotOh should (equal (6.8 +- 2) and equal (6.8 +- 2))
      minusSevenDotOh should (equal (-7.1 +- 2) and equal (-7.1 +- 2))
      minusSevenDotOh should (equal (-6.9 +- 2) and equal (-6.9 +- 2))
      minusSevenDotOh should (equal (-7.0 +- 2) and equal (-7.0 +- 2))
      minusSevenDotOh should (equal (-7.2 +- 2) and equal (-7.2 +- 2))
      minusSevenDotOh should (equal (-6.8 +- 2) and equal (-6.8 +- 2))

      // Double +- Short
      sevenDotOh should (equal (7.1 +- 2.toShort) and equal (7.1 +- 2.toShort))
      sevenDotOh should (equal (6.9 +- 2.toShort) and equal (6.9 +- 2.toShort))
      sevenDotOh should (equal (7.0 +- 2.toShort) and equal (7.0 +- 2.toShort))
      sevenDotOh should (equal (7.2 +- 2.toShort) and equal (7.2 +- 2.toShort))
      sevenDotOh should (equal (6.8 +- 2.toShort) and equal (6.8 +- 2.toShort))
      minusSevenDotOh should (equal (-7.1 +- 2.toShort) and equal (-7.1 +- 2.toShort))
      minusSevenDotOh should (equal (-6.9 +- 2.toShort) and equal (-6.9 +- 2.toShort))
      minusSevenDotOh should (equal (-7.0 +- 2.toShort) and equal (-7.0 +- 2.toShort))
      minusSevenDotOh should (equal (-7.2 +- 2.toShort) and equal (-7.2 +- 2.toShort))
      minusSevenDotOh should (equal (-6.8 +- 2.toShort) and equal (-6.8 +- 2.toShort))

      // Double +- Byte
      sevenDotOh should (equal (7.1 +- 2.toByte) and equal (7.1 +- 2.toByte))
      sevenDotOh should (equal (6.9 +- 2.toByte) and equal (6.9 +- 2.toByte))
      sevenDotOh should (equal (7.0 +- 2.toByte) and equal (7.0 +- 2.toByte))
      sevenDotOh should (equal (7.2 +- 2.toByte) and equal (7.2 +- 2.toByte))
      sevenDotOh should (equal (6.8 +- 2.toByte) and equal (6.8 +- 2.toByte))
      minusSevenDotOh should (equal (-7.1 +- 2.toByte) and equal (-7.1 +- 2.toByte))
      minusSevenDotOh should (equal (-6.9 +- 2.toByte) and equal (-6.9 +- 2.toByte))
      minusSevenDotOh should (equal (-7.0 +- 2.toByte) and equal (-7.0 +- 2.toByte))
      minusSevenDotOh should (equal (-7.2 +- 2.toByte) and equal (-7.2 +- 2.toByte))
      minusSevenDotOh should (equal (-6.8 +- 2.toByte) and equal (-6.8 +- 2.toByte))

      // Float +- Float
      sevenDotOhFloat should (equal (7.1f +- 0.2f) and equal (7.1f +- 0.2f))
      sevenDotOhFloat should (equal (6.9f +- 0.2f) and equal (6.9f +- 0.2f))
      sevenDotOhFloat should (equal (7.0f +- 0.2f) and equal (7.0f +- 0.2f))
      sevenDotOhFloat should (equal (7.2f +- 0.2f) and equal (7.2f +- 0.2f))
      sevenDotOhFloat should (equal (6.8f +- 0.2f) and equal (6.8f +- 0.2f))
      minusSevenDotOhFloat should (equal (-7.1f +- 0.2f) and equal (-7.1f +- 0.2f))
      minusSevenDotOhFloat should (equal (-6.9f +- 0.2f) and equal (-6.9f +- 0.2f))
      minusSevenDotOhFloat should (equal (-7.0f +- 0.2f) and equal (-7.0f +- 0.2f))
      minusSevenDotOhFloat should (equal (-7.2f +- 0.2f) and equal (-7.2f +- 0.2f))
      minusSevenDotOhFloat should (equal (-6.8f +- 0.2f) and equal (-6.8f +- 0.2f))

      // Float +- Long
      sevenDotOhFloat should (equal (7.1f +- 2L) and equal (7.1f +- 2L))
      sevenDotOhFloat should (equal (6.9f +- 2L) and equal (6.9f +- 2L))
      sevenDotOhFloat should (equal (7.0f +- 2L) and equal (7.0f +- 2L))
      sevenDotOhFloat should (equal (7.2f +- 2L) and equal (7.2f +- 2L))
      sevenDotOhFloat should (equal (6.8f +- 2L) and equal (6.8f +- 2L))
      minusSevenDotOhFloat should (equal (-7.1f +- 2L) and equal (-7.1f +- 2L))
      minusSevenDotOhFloat should (equal (-6.9f +- 2L) and equal (-6.9f +- 2L))
      minusSevenDotOhFloat should (equal (-7.0f +- 2L) and equal (-7.0f +- 2L))
      minusSevenDotOhFloat should (equal (-7.2f +- 2L) and equal (-7.2f +- 2L))
      minusSevenDotOhFloat should (equal (-6.8f +- 2L) and equal (-6.8f +- 2L))

      // Float +- Int
      sevenDotOhFloat should (equal (7.1f +- 2) and equal (7.1f +- 2))
      sevenDotOhFloat should (equal (6.9f +- 2) and equal (6.9f +- 2))
      sevenDotOhFloat should (equal (7.0f +- 2) and equal (7.0f +- 2))
      sevenDotOhFloat should (equal (7.2f +- 2) and equal (7.2f +- 2))
      sevenDotOhFloat should (equal (6.8f +- 2) and equal (6.8f +- 2))
      minusSevenDotOhFloat should (equal (-7.1f +- 2) and equal (-7.1f +- 2))
      minusSevenDotOhFloat should (equal (-6.9f +- 2) and equal (-6.9f +- 2))
      minusSevenDotOhFloat should (equal (-7.0f +- 2) and equal (-7.0f +- 2))
      minusSevenDotOhFloat should (equal (-7.2f +- 2) and equal (-7.2f +- 2))
      minusSevenDotOhFloat should (equal (-6.8f +- 2) and equal (-6.8f +- 2))

      // Float +- Short
      sevenDotOhFloat should (equal (7.1f +- 2.toShort) and equal (7.1f +- 2.toShort))
      sevenDotOhFloat should (equal (6.9f +- 2.toShort) and equal (6.9f +- 2.toShort))
      sevenDotOhFloat should (equal (7.0f +- 2.toShort) and equal (7.0f +- 2.toShort))
      sevenDotOhFloat should (equal (7.2f +- 2.toShort) and equal (7.2f +- 2.toShort))
      sevenDotOhFloat should (equal (6.8f +- 2.toShort) and equal (6.8f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-7.1f +- 2.toShort) and equal (-7.1f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-6.9f +- 2.toShort) and equal (-6.9f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-7.0f +- 2.toShort) and equal (-7.0f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-7.2f +- 2.toShort) and equal (-7.2f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-6.8f +- 2.toShort) and equal (-6.8f +- 2.toShort))

      // Float +- Byte
      sevenDotOhFloat should (equal (7.1f +- 2.toByte) and equal (7.1f +- 2.toByte))
      sevenDotOhFloat should (equal (6.9f +- 2.toByte) and equal (6.9f +- 2.toByte))
      sevenDotOhFloat should (equal (7.0f +- 2.toByte) and equal (7.0f +- 2.toByte))
      sevenDotOhFloat should (equal (7.2f +- 2.toByte) and equal (7.2f +- 2.toByte))
      sevenDotOhFloat should (equal (6.8f +- 2.toByte) and equal (6.8f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-7.1f +- 2.toByte) and equal (-7.1f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-6.9f +- 2.toByte) and equal (-6.9f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-7.0f +- 2.toByte) and equal (-7.0f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-7.2f +- 2.toByte) and equal (-7.2f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-6.8f +- 2.toByte) and equal (-6.8f +- 2.toByte))

      // Long +- Long
      sevenLong should (equal (9L +- 2L) and equal (9L +- 2L))
      sevenLong should (equal (8L +- 2L) and equal (8L +- 2L))
      sevenLong should (equal (7L +- 2L) and equal (7L +- 2L))
      sevenLong should (equal (6L +- 2L) and equal (6L +- 2L))
      sevenLong should (equal (5L +- 2L) and equal (5L +- 2L))
      minusSevenLong should (equal (-9L +- 2L) and equal (-9L +- 2L))
      minusSevenLong should (equal (-8L +- 2L) and equal (-8L +- 2L))
      minusSevenLong should (equal (-7L +- 2L) and equal (-7L +- 2L))
      minusSevenLong should (equal (-6L +- 2L) and equal (-6L +- 2L))
      minusSevenLong should (equal (-5L +- 2L) and equal (-5L +- 2L))

      // Long +- Int
      sevenLong should (equal (9L +- 2) and equal (9L +- 2))
      sevenLong should (equal (8L +- 2) and equal (8L +- 2))
      sevenLong should (equal (7L +- 2) and equal (7L +- 2))
      sevenLong should (equal (6L +- 2) and equal (6L +- 2))
      sevenLong should (equal (5L +- 2) and equal (5L +- 2))
      minusSevenLong should (equal (-9L +- 2) and equal (-9L +- 2))
      minusSevenLong should (equal (-8L +- 2) and equal (-8L +- 2))
      minusSevenLong should (equal (-7L +- 2) and equal (-7L +- 2))
      minusSevenLong should (equal (-6L +- 2) and equal (-6L +- 2))
      minusSevenLong should (equal (-5L +- 2) and equal (-5L +- 2))

      // Long +- Short
      sevenLong should (equal (9L +- 2.toShort) and equal (9L +- 2.toShort))
      sevenLong should (equal (8L +- 2.toShort) and equal (8L +- 2.toShort))
      sevenLong should (equal (7L +- 2.toShort) and equal (7L +- 2.toShort))
      sevenLong should (equal (6L +- 2.toShort) and equal (6L +- 2.toShort))
      sevenLong should (equal (5L +- 2.toShort) and equal (5L +- 2.toShort))
      minusSevenLong should (equal (-9L +- 2.toShort) and equal (-9L +- 2.toShort))
      minusSevenLong should (equal (-8L +- 2.toShort) and equal (-8L +- 2.toShort))
      minusSevenLong should (equal (-7L +- 2.toShort) and equal (-7L +- 2.toShort))
      minusSevenLong should (equal (-6L +- 2.toShort) and equal (-6L +- 2.toShort))
      minusSevenLong should (equal (-5L +- 2.toShort) and equal (-5L +- 2.toShort))

      // Long +- Byte
      sevenLong should (equal (9L +- 2.toByte) and equal (9L +- 2.toByte))
      sevenLong should (equal (8L +- 2.toByte) and equal (8L +- 2.toByte))
      sevenLong should (equal (7L +- 2.toByte) and equal (7L +- 2.toByte))
      sevenLong should (equal (6L +- 2.toByte) and equal (6L +- 2.toByte))
      sevenLong should (equal (5L +- 2.toByte) and equal (5L +- 2.toByte))
      minusSevenLong should (equal (-9L +- 2.toByte) and equal (-9L +- 2.toByte))
      minusSevenLong should (equal (-8L +- 2.toByte) and equal (-8L +- 2.toByte))
      minusSevenLong should (equal (-7L +- 2.toByte) and equal (-7L +- 2.toByte))
      minusSevenLong should (equal (-6L +- 2.toByte) and equal (-6L +- 2.toByte))
      minusSevenLong should (equal (-5L +- 2.toByte) and equal (-5L +- 2.toByte))

      // Int +- Int
      sevenInt should (equal (9 +- 2) and equal (9 +- 2))
      sevenInt should (equal (8 +- 2) and equal (8 +- 2))
      sevenInt should (equal (7 +- 2) and equal (7 +- 2))
      sevenInt should (equal (6 +- 2) and equal (6 +- 2))
      sevenInt should (equal (5 +- 2) and equal (5 +- 2))
      minusSevenInt should (equal (-9 +- 2) and equal (-9 +- 2))
      minusSevenInt should (equal (-8 +- 2) and equal (-8 +- 2))
      minusSevenInt should (equal (-7 +- 2) and equal (-7 +- 2))
      minusSevenInt should (equal (-6 +- 2) and equal (-6 +- 2))
      minusSevenInt should (equal (-5 +- 2) and equal (-5 +- 2))

      // Int +- Short
      sevenInt should (equal (9 +- 2.toShort) and equal (9 +- 2.toShort))
      sevenInt should (equal (8 +- 2.toShort) and equal (8 +- 2.toShort))
      sevenInt should (equal (7 +- 2.toShort) and equal (7 +- 2.toShort))
      sevenInt should (equal (6 +- 2.toShort) and equal (6 +- 2.toShort))
      sevenInt should (equal (5 +- 2.toShort) and equal (5 +- 2.toShort))
      minusSevenInt should (equal (-9 +- 2.toShort) and equal (-9 +- 2.toShort))
      minusSevenInt should (equal (-8 +- 2.toShort) and equal (-8 +- 2.toShort))
      minusSevenInt should (equal (-7 +- 2.toShort) and equal (-7 +- 2.toShort))
      minusSevenInt should (equal (-6 +- 2.toShort) and equal (-6 +- 2.toShort))
      minusSevenInt should (equal (-5 +- 2.toShort) and equal (-5 +- 2.toShort))

      // Int +- Byte
      sevenInt should (equal (9 +- 2.toByte) and equal (9 +- 2.toByte))
      sevenInt should (equal (8 +- 2.toByte) and equal (8 +- 2.toByte))
      sevenInt should (equal (7 +- 2.toByte) and equal (7 +- 2.toByte))
      sevenInt should (equal (6 +- 2.toByte) and equal (6 +- 2.toByte))
      sevenInt should (equal (5 +- 2.toByte) and equal (5 +- 2.toByte))
      minusSevenInt should (equal (-9 +- 2.toByte) and equal (-9 +- 2.toByte))
      minusSevenInt should (equal (-8 +- 2.toByte) and equal (-8 +- 2.toByte))
      minusSevenInt should (equal (-7 +- 2.toByte) and equal (-7 +- 2.toByte))
      minusSevenInt should (equal (-6 +- 2.toByte) and equal (-6 +- 2.toByte))
      minusSevenInt should (equal (-5 +- 2.toByte) and equal (-5 +- 2.toByte))

      // Short +- Short
      sevenShort should (equal (9.toShort +- 2.toShort) and equal (9.toShort +- 2.toShort))
      sevenShort should (equal (8.toShort +- 2.toShort) and equal (8.toShort +- 2.toShort))
      sevenShort should (equal (7.toShort +- 2.toShort) and equal (7.toShort +- 2.toShort))
      sevenShort should (equal (6.toShort +- 2.toShort) and equal (6.toShort +- 2.toShort))
      sevenShort should (equal (5.toShort +- 2.toShort) and equal (5.toShort +- 2.toShort))
      minusSevenShort should (equal ((-9).toShort +- 2.toShort) and equal ((-9).toShort +- 2.toShort))
      minusSevenShort should (equal ((-8).toShort +- 2.toShort) and equal ((-8).toShort +- 2.toShort))
      minusSevenShort should (equal ((-7).toShort +- 2.toShort) and equal ((-7).toShort +- 2.toShort))
      minusSevenShort should (equal ((-6).toShort +- 2.toShort) and equal ((-6).toShort +- 2.toShort))
      minusSevenShort should (equal ((-5).toShort +- 2.toShort) and equal ((-5).toShort +- 2.toShort))

      // Short +- Byte
      sevenShort should (equal (9.toShort +- 2.toByte) and equal (9.toShort +- 2.toByte))
      sevenShort should (equal (8.toShort +- 2.toByte) and equal (8.toShort +- 2.toByte))
      sevenShort should (equal (7.toShort +- 2.toByte) and equal (7.toShort +- 2.toByte))
      sevenShort should (equal (6.toShort +- 2.toByte) and equal (6.toShort +- 2.toByte))
      sevenShort should (equal (5.toShort +- 2.toByte) and equal (5.toShort +- 2.toByte))
      minusSevenShort should (equal ((-9).toShort +- 2.toByte) and equal ((-9).toShort +- 2.toByte))
      minusSevenShort should (equal ((-8).toShort +- 2.toByte) and equal ((-8).toShort +- 2.toByte))
      minusSevenShort should (equal ((-7).toShort +- 2.toByte) and equal ((-7).toShort +- 2.toByte))
      minusSevenShort should (equal ((-6).toShort +- 2.toByte) and equal ((-6).toShort +- 2.toByte))
      minusSevenShort should (equal ((-5).toShort +- 2.toByte) and equal ((-5).toShort +- 2.toByte))

      // Byte +- Byte
      sevenByte should (equal (9.toByte +- 2.toByte) and equal (9.toByte +- 2.toByte))
      sevenByte should (equal (8.toByte +- 2.toByte) and equal (8.toByte +- 2.toByte))
      sevenByte should (equal (7.toByte +- 2.toByte) and equal (7.toByte +- 2.toByte))
      sevenByte should (equal (6.toByte +- 2.toByte) and equal (6.toByte +- 2.toByte))
      sevenByte should (equal (5.toByte +- 2.toByte) and equal (5.toByte +- 2.toByte))
      minusSevenByte should (equal ((-9).toByte +- 2.toByte) and equal ((-9).toByte +- 2.toByte))
      minusSevenByte should (equal ((-8).toByte +- 2.toByte) and equal ((-8).toByte +- 2.toByte))
      minusSevenByte should (equal ((-7).toByte +- 2.toByte) and equal ((-7).toByte +- 2.toByte))
      minusSevenByte should (equal ((-6).toByte +- 2.toByte) and equal ((-6).toByte +- 2.toByte))
      minusSevenByte should (equal ((-5).toByte +- 2.toByte) and equal ((-5).toByte +- 2.toByte))
    }

    it("should succeed when equal and used in a logical-or expression") {

      sevenDotOh should (equal (7.1 +- 0.2) or equal (7.1 +- 0.2))
      sevenDotOh should (equal (6.9 +- 0.2) or equal (6.9 +- 0.2))
      sevenDotOh should (equal (7.0 +- 0.2) or equal (7.0 +- 0.2))
      sevenDotOh should (equal (7.2 +- 0.2) or equal (7.2 +- 0.2))
      sevenDotOh should (equal (6.8 +- 0.2) or equal (6.8 +- 0.2))
      minusSevenDotOh should (equal (-7.1 +- 0.2) or equal (-7.1 +- 0.2))
      minusSevenDotOh should (equal (-6.9 +- 0.2) or equal (-6.9 +- 0.2))
      minusSevenDotOh should (equal (-7.0 +- 0.2) or equal (-7.0 +- 0.2))
      minusSevenDotOh should (equal (-7.2 +- 0.2) or equal (-7.2 +- 0.2))
      minusSevenDotOh should (equal (-6.8 +- 0.2) or equal (-6.8 +- 0.2))

      // Double +- Float
      sevenDotOh should (equal (7.1 +- 0.2f) or equal (7.1 +- 0.2f))
      sevenDotOh should (equal (6.9 +- 0.2f) or equal (6.9 +- 0.2f))
      sevenDotOh should (equal (7.0 +- 0.2f) or equal (7.0 +- 0.2f))
      sevenDotOh should (equal (7.2 +- 0.2f) or equal (7.2 +- 0.2f))
      sevenDotOh should (equal (6.8 +- 0.2f) or equal (6.8 +- 0.2f))
      minusSevenDotOh should (equal (-7.1 +- 0.2f) or equal (-7.1 +- 0.2f))
      minusSevenDotOh should (equal (-6.9 +- 0.2f) or equal (-6.9 +- 0.2f))
      minusSevenDotOh should (equal (-7.0 +- 0.2f) or equal (-7.0 +- 0.2f))
      minusSevenDotOh should (equal (-7.2 +- 0.2f) or equal (-7.2 +- 0.2f))
      minusSevenDotOh should (equal (-6.8 +- 0.2f) or equal (-6.8 +- 0.2f))

      // Double +- Long
      sevenDotOh should (equal (7.1 +- 2L) or equal (7.1 +- 2L))
      sevenDotOh should (equal (6.9 +- 2L) or equal (6.9 +- 2L))
      sevenDotOh should (equal (7.0 +- 2L) or equal (7.0 +- 2L))
      sevenDotOh should (equal (7.2 +- 2L) or equal (7.2 +- 2L))
      sevenDotOh should (equal (6.8 +- 2L) or equal (6.8 +- 2L))
      minusSevenDotOh should (equal (-7.1 +- 2L) or equal (-7.1 +- 2L))
      minusSevenDotOh should (equal (-6.9 +- 2L) or equal (-6.9 +- 2L))
      minusSevenDotOh should (equal (-7.0 +- 2L) or equal (-7.0 +- 2L))
      minusSevenDotOh should (equal (-7.2 +- 2L) or equal (-7.2 +- 2L))
      minusSevenDotOh should (equal (-6.8 +- 2L) or equal (-6.8 +- 2L))

      // Double +- Int
      sevenDotOh should (equal (7.1 +- 2) or equal (7.1 +- 2))
      sevenDotOh should (equal (6.9 +- 2) or equal (6.9 +- 2))
      sevenDotOh should (equal (7.0 +- 2) or equal (7.0 +- 2))
      sevenDotOh should (equal (7.2 +- 2) or equal (7.2 +- 2))
      sevenDotOh should (equal (6.8 +- 2) or equal (6.8 +- 2))
      minusSevenDotOh should (equal (-7.1 +- 2) or equal (-7.1 +- 2))
      minusSevenDotOh should (equal (-6.9 +- 2) or equal (-6.9 +- 2))
      minusSevenDotOh should (equal (-7.0 +- 2) or equal (-7.0 +- 2))
      minusSevenDotOh should (equal (-7.2 +- 2) or equal (-7.2 +- 2))
      minusSevenDotOh should (equal (-6.8 +- 2) or equal (-6.8 +- 2))

      // Double +- Short
      sevenDotOh should (equal (7.1 +- 2.toShort) or equal (7.1 +- 2.toShort))
      sevenDotOh should (equal (6.9 +- 2.toShort) or equal (6.9 +- 2.toShort))
      sevenDotOh should (equal (7.0 +- 2.toShort) or equal (7.0 +- 2.toShort))
      sevenDotOh should (equal (7.2 +- 2.toShort) or equal (7.2 +- 2.toShort))
      sevenDotOh should (equal (6.8 +- 2.toShort) or equal (6.8 +- 2.toShort))
      minusSevenDotOh should (equal (-7.1 +- 2.toShort) or equal (-7.1 +- 2.toShort))
      minusSevenDotOh should (equal (-6.9 +- 2.toShort) or equal (-6.9 +- 2.toShort))
      minusSevenDotOh should (equal (-7.0 +- 2.toShort) or equal (-7.0 +- 2.toShort))
      minusSevenDotOh should (equal (-7.2 +- 2.toShort) or equal (-7.2 +- 2.toShort))
      minusSevenDotOh should (equal (-6.8 +- 2.toShort) or equal (-6.8 +- 2.toShort))

      // Double +- Byte
      sevenDotOh should (equal (7.1 +- 2.toByte) or equal (7.1 +- 2.toByte))
      sevenDotOh should (equal (6.9 +- 2.toByte) or equal (6.9 +- 2.toByte))
      sevenDotOh should (equal (7.0 +- 2.toByte) or equal (7.0 +- 2.toByte))
      sevenDotOh should (equal (7.2 +- 2.toByte) or equal (7.2 +- 2.toByte))
      sevenDotOh should (equal (6.8 +- 2.toByte) or equal (6.8 +- 2.toByte))
      minusSevenDotOh should (equal (-7.1 +- 2.toByte) or equal (-7.1 +- 2.toByte))
      minusSevenDotOh should (equal (-6.9 +- 2.toByte) or equal (-6.9 +- 2.toByte))
      minusSevenDotOh should (equal (-7.0 +- 2.toByte) or equal (-7.0 +- 2.toByte))
      minusSevenDotOh should (equal (-7.2 +- 2.toByte) or equal (-7.2 +- 2.toByte))
      minusSevenDotOh should (equal (-6.8 +- 2.toByte) or equal (-6.8 +- 2.toByte))

      // Float +- Float
      sevenDotOhFloat should (equal (7.1f +- 0.2f) or equal (7.1f +- 0.2f))
      sevenDotOhFloat should (equal (6.9f +- 0.2f) or equal (6.9f +- 0.2f))
      sevenDotOhFloat should (equal (7.0f +- 0.2f) or equal (7.0f +- 0.2f))
      sevenDotOhFloat should (equal (7.2f +- 0.2f) or equal (7.2f +- 0.2f))
      sevenDotOhFloat should (equal (6.8f +- 0.2f) or equal (6.8f +- 0.2f))
      minusSevenDotOhFloat should (equal (-7.1f +- 0.2f) or equal (-7.1f +- 0.2f))
      minusSevenDotOhFloat should (equal (-6.9f +- 0.2f) or equal (-6.9f +- 0.2f))
      minusSevenDotOhFloat should (equal (-7.0f +- 0.2f) or equal (-7.0f +- 0.2f))
      minusSevenDotOhFloat should (equal (-7.2f +- 0.2f) or equal (-7.2f +- 0.2f))
      minusSevenDotOhFloat should (equal (-6.8f +- 0.2f) or equal (-6.8f +- 0.2f))

      // Float +- Long
      sevenDotOhFloat should (equal (7.1f +- 2L) or equal (7.1f +- 2L))
      sevenDotOhFloat should (equal (6.9f +- 2L) or equal (6.9f +- 2L))
      sevenDotOhFloat should (equal (7.0f +- 2L) or equal (7.0f +- 2L))
      sevenDotOhFloat should (equal (7.2f +- 2L) or equal (7.2f +- 2L))
      sevenDotOhFloat should (equal (6.8f +- 2L) or equal (6.8f +- 2L))
      minusSevenDotOhFloat should (equal (-7.1f +- 2L) or equal (-7.1f +- 2L))
      minusSevenDotOhFloat should (equal (-6.9f +- 2L) or equal (-6.9f +- 2L))
      minusSevenDotOhFloat should (equal (-7.0f +- 2L) or equal (-7.0f +- 2L))
      minusSevenDotOhFloat should (equal (-7.2f +- 2L) or equal (-7.2f +- 2L))
      minusSevenDotOhFloat should (equal (-6.8f +- 2L) or equal (-6.8f +- 2L))

      // Float +- Int
      sevenDotOhFloat should (equal (7.1f +- 2) or equal (7.1f +- 2))
      sevenDotOhFloat should (equal (6.9f +- 2) or equal (6.9f +- 2))
      sevenDotOhFloat should (equal (7.0f +- 2) or equal (7.0f +- 2))
      sevenDotOhFloat should (equal (7.2f +- 2) or equal (7.2f +- 2))
      sevenDotOhFloat should (equal (6.8f +- 2) or equal (6.8f +- 2))
      minusSevenDotOhFloat should (equal (-7.1f +- 2) or equal (-7.1f +- 2))
      minusSevenDotOhFloat should (equal (-6.9f +- 2) or equal (-6.9f +- 2))
      minusSevenDotOhFloat should (equal (-7.0f +- 2) or equal (-7.0f +- 2))
      minusSevenDotOhFloat should (equal (-7.2f +- 2) or equal (-7.2f +- 2))
      minusSevenDotOhFloat should (equal (-6.8f +- 2) or equal (-6.8f +- 2))

      // Float +- Short
      sevenDotOhFloat should (equal (7.1f +- 2.toShort) or equal (7.1f +- 2.toShort))
      sevenDotOhFloat should (equal (6.9f +- 2.toShort) or equal (6.9f +- 2.toShort))
      sevenDotOhFloat should (equal (7.0f +- 2.toShort) or equal (7.0f +- 2.toShort))
      sevenDotOhFloat should (equal (7.2f +- 2.toShort) or equal (7.2f +- 2.toShort))
      sevenDotOhFloat should (equal (6.8f +- 2.toShort) or equal (6.8f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-7.1f +- 2.toShort) or equal (-7.1f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-6.9f +- 2.toShort) or equal (-6.9f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-7.0f +- 2.toShort) or equal (-7.0f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-7.2f +- 2.toShort) or equal (-7.2f +- 2.toShort))
      minusSevenDotOhFloat should (equal (-6.8f +- 2.toShort) or equal (-6.8f +- 2.toShort))

      // Float +- Byte
      sevenDotOhFloat should (equal (7.1f +- 2.toByte) or equal (7.1f +- 2.toByte))
      sevenDotOhFloat should (equal (6.9f +- 2.toByte) or equal (6.9f +- 2.toByte))
      sevenDotOhFloat should (equal (7.0f +- 2.toByte) or equal (7.0f +- 2.toByte))
      sevenDotOhFloat should (equal (7.2f +- 2.toByte) or equal (7.2f +- 2.toByte))
      sevenDotOhFloat should (equal (6.8f +- 2.toByte) or equal (6.8f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-7.1f +- 2.toByte) or equal (-7.1f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-6.9f +- 2.toByte) or equal (-6.9f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-7.0f +- 2.toByte) or equal (-7.0f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-7.2f +- 2.toByte) or equal (-7.2f +- 2.toByte))
      minusSevenDotOhFloat should (equal (-6.8f +- 2.toByte) or equal (-6.8f +- 2.toByte))

      // Long +- Long
      sevenLong should (equal (9L +- 2L) or equal (9L +- 2L))
      sevenLong should (equal (8L +- 2L) or equal (8L +- 2L))
      sevenLong should (equal (7L +- 2L) or equal (7L +- 2L))
      sevenLong should (equal (6L +- 2L) or equal (6L +- 2L))
      sevenLong should (equal (5L +- 2L) or equal (5L +- 2L))
      minusSevenLong should (equal (-9L +- 2L) or equal (-9L +- 2L))
      minusSevenLong should (equal (-8L +- 2L) or equal (-8L +- 2L))
      minusSevenLong should (equal (-7L +- 2L) or equal (-7L +- 2L))
      minusSevenLong should (equal (-6L +- 2L) or equal (-6L +- 2L))
      minusSevenLong should (equal (-5L +- 2L) or equal (-5L +- 2L))

      // Long +- Int
      sevenLong should (equal (9L +- 2) or equal (9L +- 2))
      sevenLong should (equal (8L +- 2) or equal (8L +- 2))
      sevenLong should (equal (7L +- 2) or equal (7L +- 2))
      sevenLong should (equal (6L +- 2) or equal (6L +- 2))
      sevenLong should (equal (5L +- 2) or equal (5L +- 2))
      minusSevenLong should (equal (-9L +- 2) or equal (-9L +- 2))
      minusSevenLong should (equal (-8L +- 2) or equal (-8L +- 2))
      minusSevenLong should (equal (-7L +- 2) or equal (-7L +- 2))
      minusSevenLong should (equal (-6L +- 2) or equal (-6L +- 2))
      minusSevenLong should (equal (-5L +- 2) or equal (-5L +- 2))

      // Long +- Short
      sevenLong should (equal (9L +- 2.toShort) or equal (9L +- 2.toShort))
      sevenLong should (equal (8L +- 2.toShort) or equal (8L +- 2.toShort))
      sevenLong should (equal (7L +- 2.toShort) or equal (7L +- 2.toShort))
      sevenLong should (equal (6L +- 2.toShort) or equal (6L +- 2.toShort))
      sevenLong should (equal (5L +- 2.toShort) or equal (5L +- 2.toShort))
      minusSevenLong should (equal (-9L +- 2.toShort) or equal (-9L +- 2.toShort))
      minusSevenLong should (equal (-8L +- 2.toShort) or equal (-8L +- 2.toShort))
      minusSevenLong should (equal (-7L +- 2.toShort) or equal (-7L +- 2.toShort))
      minusSevenLong should (equal (-6L +- 2.toShort) or equal (-6L +- 2.toShort))
      minusSevenLong should (equal (-5L +- 2.toShort) or equal (-5L +- 2.toShort))

      // Long +- Byte
      sevenLong should (equal (9L +- 2.toByte) or equal (9L +- 2.toByte))
      sevenLong should (equal (8L +- 2.toByte) or equal (8L +- 2.toByte))
      sevenLong should (equal (7L +- 2.toByte) or equal (7L +- 2.toByte))
      sevenLong should (equal (6L +- 2.toByte) or equal (6L +- 2.toByte))
      sevenLong should (equal (5L +- 2.toByte) or equal (5L +- 2.toByte))
      minusSevenLong should (equal (-9L +- 2.toByte) or equal (-9L +- 2.toByte))
      minusSevenLong should (equal (-8L +- 2.toByte) or equal (-8L +- 2.toByte))
      minusSevenLong should (equal (-7L +- 2.toByte) or equal (-7L +- 2.toByte))
      minusSevenLong should (equal (-6L +- 2.toByte) or equal (-6L +- 2.toByte))
      minusSevenLong should (equal (-5L +- 2.toByte) or equal (-5L +- 2.toByte))

      // Int +- Int
      sevenInt should (equal (9 +- 2) or equal (9 +- 2))
      sevenInt should (equal (8 +- 2) or equal (8 +- 2))
      sevenInt should (equal (7 +- 2) or equal (7 +- 2))
      sevenInt should (equal (6 +- 2) or equal (6 +- 2))
      sevenInt should (equal (5 +- 2) or equal (5 +- 2))
      minusSevenInt should (equal (-9 +- 2) or equal (-9 +- 2))
      minusSevenInt should (equal (-8 +- 2) or equal (-8 +- 2))
      minusSevenInt should (equal (-7 +- 2) or equal (-7 +- 2))
      minusSevenInt should (equal (-6 +- 2) or equal (-6 +- 2))
      minusSevenInt should (equal (-5 +- 2) or equal (-5 +- 2))

      // Int +- Short
      sevenInt should (equal (9 +- 2.toShort) or equal (9 +- 2.toShort))
      sevenInt should (equal (8 +- 2.toShort) or equal (8 +- 2.toShort))
      sevenInt should (equal (7 +- 2.toShort) or equal (7 +- 2.toShort))
      sevenInt should (equal (6 +- 2.toShort) or equal (6 +- 2.toShort))
      sevenInt should (equal (5 +- 2.toShort) or equal (5 +- 2.toShort))
      minusSevenInt should (equal (-9 +- 2.toShort) or equal (-9 +- 2.toShort))
      minusSevenInt should (equal (-8 +- 2.toShort) or equal (-8 +- 2.toShort))
      minusSevenInt should (equal (-7 +- 2.toShort) or equal (-7 +- 2.toShort))
      minusSevenInt should (equal (-6 +- 2.toShort) or equal (-6 +- 2.toShort))
      minusSevenInt should (equal (-5 +- 2.toShort) or equal (-5 +- 2.toShort))

      // Int +- Byte
      sevenInt should (equal (9 +- 2.toByte) or equal (9 +- 2.toByte))
      sevenInt should (equal (8 +- 2.toByte) or equal (8 +- 2.toByte))
      sevenInt should (equal (7 +- 2.toByte) or equal (7 +- 2.toByte))
      sevenInt should (equal (6 +- 2.toByte) or equal (6 +- 2.toByte))
      sevenInt should (equal (5 +- 2.toByte) or equal (5 +- 2.toByte))
      minusSevenInt should (equal (-9 +- 2.toByte) or equal (-9 +- 2.toByte))
      minusSevenInt should (equal (-8 +- 2.toByte) or equal (-8 +- 2.toByte))
      minusSevenInt should (equal (-7 +- 2.toByte) or equal (-7 +- 2.toByte))
      minusSevenInt should (equal (-6 +- 2.toByte) or equal (-6 +- 2.toByte))
      minusSevenInt should (equal (-5 +- 2.toByte) or equal (-5 +- 2.toByte))

      // Short +- Short
      sevenShort should (equal (9.toShort +- 2.toShort) or equal (9.toShort +- 2.toShort))
      sevenShort should (equal (8.toShort +- 2.toShort) or equal (8.toShort +- 2.toShort))
      sevenShort should (equal (7.toShort +- 2.toShort) or equal (7.toShort +- 2.toShort))
      sevenShort should (equal (6.toShort +- 2.toShort) or equal (6.toShort +- 2.toShort))
      sevenShort should (equal (5.toShort +- 2.toShort) or equal (5.toShort +- 2.toShort))
      minusSevenShort should (equal ((-9).toShort +- 2.toShort) or equal ((-9).toShort +- 2.toShort))
      minusSevenShort should (equal ((-8).toShort +- 2.toShort) or equal ((-8).toShort +- 2.toShort))
      minusSevenShort should (equal ((-7).toShort +- 2.toShort) or equal ((-7).toShort +- 2.toShort))
      minusSevenShort should (equal ((-6).toShort +- 2.toShort) or equal ((-6).toShort +- 2.toShort))
      minusSevenShort should (equal ((-5).toShort +- 2.toShort) or equal ((-5).toShort +- 2.toShort))

      // Short +- Byte
      sevenShort should (equal (9.toShort +- 2.toByte) or equal (9.toShort +- 2.toByte))
      sevenShort should (equal (8.toShort +- 2.toByte) or equal (8.toShort +- 2.toByte))
      sevenShort should (equal (7.toShort +- 2.toByte) or equal (7.toShort +- 2.toByte))
      sevenShort should (equal (6.toShort +- 2.toByte) or equal (6.toShort +- 2.toByte))
      sevenShort should (equal (5.toShort +- 2.toByte) or equal (5.toShort +- 2.toByte))
      minusSevenShort should (equal ((-9).toShort +- 2.toByte) or equal ((-9).toShort +- 2.toByte))
      minusSevenShort should (equal ((-8).toShort +- 2.toByte) or equal ((-8).toShort +- 2.toByte))
      minusSevenShort should (equal ((-7).toShort +- 2.toByte) or equal ((-7).toShort +- 2.toByte))
      minusSevenShort should (equal ((-6).toShort +- 2.toByte) or equal ((-6).toShort +- 2.toByte))
      minusSevenShort should (equal ((-5).toShort +- 2.toByte) or equal ((-5).toShort +- 2.toByte))

      // Byte +- Byte
      sevenByte should (equal (9.toByte +- 2.toByte) or equal (9.toByte +- 2.toByte))
      sevenByte should (equal (8.toByte +- 2.toByte) or equal (8.toByte +- 2.toByte))
      sevenByte should (equal (7.toByte +- 2.toByte) or equal (7.toByte +- 2.toByte))
      sevenByte should (equal (6.toByte +- 2.toByte) or equal (6.toByte +- 2.toByte))
      sevenByte should (equal (5.toByte +- 2.toByte) or equal (5.toByte +- 2.toByte))
      minusSevenByte should (equal ((-9).toByte +- 2.toByte) or equal ((-9).toByte +- 2.toByte))
      minusSevenByte should (equal ((-8).toByte +- 2.toByte) or equal ((-8).toByte +- 2.toByte))
      minusSevenByte should (equal ((-7).toByte +- 2.toByte) or equal ((-7).toByte +- 2.toByte))
      minusSevenByte should (equal ((-6).toByte +- 2.toByte) or equal ((-6).toByte +- 2.toByte))
      minusSevenByte should (equal ((-5).toByte +- 2.toByte) or equal ((-5).toByte +- 2.toByte))
    }

    it("should succeed when not equal and used in a logical-and expression with not") {
      1 should { not { equal (3 +- 1) } and not { equal (3 +- 1) }}
      1 should { not equal (3 +- 1) and (not equal (3 +- 1)) }
      1 should (not equal (3 +- 1) and not equal (3 +- 1))
    }

    it("should succeed when not equal and used in a logical-or expression with not") {
      1 should { not { equal (3 +- 1) } or not { equal (3 +- 1) }}
      1 should { not equal (3 +- 1) or (not equal (3 +- 1)) }
      1 should (not equal (3 +- 1) or not equal (3 +- 1))
    }

    it("should throw a TFE when not equal and used in a logical-and expression") {
      val caught = intercept[TestFailedException] {
        1 should { equal (5 +- 1) and equal (2 +- 1) }
      }
      assert(caught.getMessage === "1 did not equal 5 plus or minus 1")
    }

    it("should throw a TFE when not equal and used in a logical-or expression") {
      val caught = intercept[TestFailedException] {
        1 should { equal (5 +- 1) or equal (4 +- 1) }
      }
      assert(caught.getMessage === "1 did not equal 5 plus or minus 1, and 1 did not equal 4 plus or minus 1")
    }

    it("should throw a TFE when equal and used in a logical-and expression with not") {

      val caught1 = intercept[TestFailedException] {
        1 should { not { equal (2 +- 1) } and not { equal (3 - 1) }}
      }
      assert(caught1.getMessage === "1 equaled 2 plus or minus 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not equal (2 +- 1) and (not equal (3 - 1)) }
      }
      assert(caught2.getMessage === "1 equaled 2 plus or minus 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not equal (2 +- 1) and not equal (3 - 1))
      }
      assert(caught3.getMessage === "1 equaled 2 plus or minus 1")

      val caught4 = intercept[TestFailedException] {
        1 should { not { equal (3 +- 1) } and not { equal (2 +- 1) }}
      }
      assert(caught4.getMessage === "1 did not equal 3 plus or minus 1, but 1 equaled 2 plus or minus 1")

      val caught5 = intercept[TestFailedException] {
        1 should { not equal (3 +- 1) and (not equal (2 +- 1)) }
      }
      assert(caught5.getMessage === "1 did not equal 3 plus or minus 1, but 1 equaled 2 plus or minus 1")

      val caught6 = intercept[TestFailedException] {
        1 should (not equal (3 +- 1) and not equal (2 +- 1))
      }
      assert(caught6.getMessage === "1 did not equal 3 plus or minus 1, but 1 equaled 2 plus or minus 1")
    }

    it("should throw a TFE when equal and used in a logical-or expression with not") {

      val caught1 = intercept[TestFailedException] {
        1 should { not { equal (2 +- 1) } or not { equal (2 +- 1) }}
      }
      assert(caught1.getMessage === "1 equaled 2 plus or minus 1, and 1 equaled 2 plus or minus 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not equal (2 +- 1) or { not equal (2 +- 1) }}
      }
      assert(caught2.getMessage === "1 equaled 2 plus or minus 1, and 1 equaled 2 plus or minus 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not equal (2 +- 1) or not equal (2 +- 1))
      }
      assert(caught3.getMessage === "1 equaled 2 plus or minus 1, and 1 equaled 2 plus or minus 1")
    }
  }

  describe("The X +- Y syntax") {

    it("should throw IllegalArgumentException if the number passed to the right is 0 or negative") {

      // Double +- Double
      val caught1 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -0.2)
      }
      assert(caught1.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.", caught1.getMessage)

      // Double +- Float
      val caught2 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -0.2f)
      }
      assert(caught2.getMessage === "-0.20000000298023224 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Long
      val caught3 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -2L)
      }
      // SKIP-SCALATESTJS-START
      assert(caught3.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught3.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Int
      val caught4 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -2)
      }
      // SKIP-SCALATESTJS-START
      assert(caught4.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught4.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Short
      val caught5 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- (-2).toShort)
      }
      // SKIP-SCALATESTJS-START
      assert(caught5.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught5.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Byte
      val caught6 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- (-2).toByte)
      }
      // SKIP-SCALATESTJS-START
      assert(caught6.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught6.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Float
      val caught7 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- -0.2f)
      }
      assert(caught7.getMessage === -0.2f + " passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Long
      val caught8 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- -2L)
      }
      // SKIP-SCALATESTJS-START
      assert(caught8.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught8.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Int
      val caught9 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- -2)
      }
      // SKIP-SCALATESTJS-START
      assert(caught9.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught9.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Short
      val caught10 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- (-2).toShort)
      }
      // SKIP-SCALATESTJS-START
      assert(caught10.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught10.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Byte
      val caught11 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- (-2).toByte)
      }
      // SKIP-SCALATESTJS-START
      assert(caught11.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(caught11.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Long
      val caught12 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- -2L)
      }
      assert(caught12.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Int
      val caught13 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- -2)
      }
      assert(caught13.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Short
      val caught14 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- (-2).toShort)
      }
      assert(caught14.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Byte
      val caught15 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- (-2).toByte)
      }
      assert(caught15.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Int
      val caught16 = intercept[IllegalArgumentException] {
        sevenInt should equal (9 +- -2)
      }
      assert(caught16.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Short
      val caught17 = intercept[IllegalArgumentException] {
        sevenInt should equal (9 +- (-2).toShort)
      }
      assert(caught17.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Byte
      val caught18 = intercept[IllegalArgumentException] {
        sevenInt should equal (9 +- (-2).toByte)
      }
      assert(caught18.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Short
      val caught19 = intercept[IllegalArgumentException] {
        sevenShort should equal (9.toShort +- (-2).toShort)
      }
      assert(caught19.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Byte
      val caught20 = intercept[IllegalArgumentException] {
        sevenShort should equal (9.toShort +- (-2).toByte)
      }
      assert(caught20.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Byte +- Byte
      val caught21 = intercept[IllegalArgumentException] {
        sevenByte should equal (9.toByte +- (-2).toByte)
      }
      assert(caught21.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")
    }
  }
}
