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

import org.scalautils._

// For shouldEqual instead of should equal
class ShouldEqualTokenToleranceSpec extends Spec with Matchers with Tolerance {

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
      (7.1 +- 0.2) shouldEqual sevenDotOh
      (7.5 +- 0.2) should not equal sevenDotOh
 */
  object `The shouldEqual syntax` {

    def `should succeed if the number is within the given interval` {

      // Double +- Double
      sevenDotOh shouldEqual (7.1 +- 0.2)
      sevenDotOh shouldEqual (6.9 +- 0.2)
      sevenDotOh shouldEqual (7.0 +- 0.2)
      sevenDotOh shouldEqual (7.2 +- 0.2)
      sevenDotOh shouldEqual (6.8 +- 0.2)
      minusSevenDotOh shouldEqual (-7.1 +- 0.2)
      minusSevenDotOh shouldEqual (-6.9 +- 0.2)
      minusSevenDotOh shouldEqual (-7.0 +- 0.2)
      minusSevenDotOh shouldEqual (-7.2 +- 0.2)
      minusSevenDotOh shouldEqual (-6.8 +- 0.2)

      // Double +- Float
      sevenDotOh shouldEqual (7.1 +- 0.2f)
      sevenDotOh shouldEqual (6.9 +- 0.2f)
      sevenDotOh shouldEqual (7.0 +- 0.2f)
      sevenDotOh shouldEqual (7.2 +- 0.2f)
      sevenDotOh shouldEqual (6.8 +- 0.2f)
      minusSevenDotOh shouldEqual (-7.1 +- 0.2f)
      minusSevenDotOh shouldEqual (-6.9 +- 0.2f)
      minusSevenDotOh shouldEqual (-7.0 +- 0.2f)
      minusSevenDotOh shouldEqual (-7.2 +- 0.2f)
      minusSevenDotOh shouldEqual (-6.8 +- 0.2f)

      // Double +- Long
      sevenDotOh shouldEqual (7.1 +- 2L)
      sevenDotOh shouldEqual (6.9 +- 2L)
      sevenDotOh shouldEqual (7.0 +- 2L)
      sevenDotOh shouldEqual (7.2 +- 2L)
      sevenDotOh shouldEqual (6.8 +- 2L)
      minusSevenDotOh shouldEqual (-7.1 +- 2L)
      minusSevenDotOh shouldEqual (-6.9 +- 2L)
      minusSevenDotOh shouldEqual (-7.0 +- 2L)
      minusSevenDotOh shouldEqual (-7.2 +- 2L)
      minusSevenDotOh shouldEqual (-6.8 +- 2L)

      // Double +- Int
      sevenDotOh shouldEqual (7.1 +- 2)
      sevenDotOh shouldEqual (6.9 +- 2)
      sevenDotOh shouldEqual (7.0 +- 2)
      sevenDotOh shouldEqual (7.2 +- 2)
      sevenDotOh shouldEqual (6.8 +- 2)
      minusSevenDotOh shouldEqual (-7.1 +- 2)
      minusSevenDotOh shouldEqual (-6.9 +- 2)
      minusSevenDotOh shouldEqual (-7.0 +- 2)
      minusSevenDotOh shouldEqual (-7.2 +- 2)
      minusSevenDotOh shouldEqual (-6.8 +- 2)

      // Double +- Short
      sevenDotOh shouldEqual (7.1 +- 2.toShort)
      sevenDotOh shouldEqual (6.9 +- 2.toShort)
      sevenDotOh shouldEqual (7.0 +- 2.toShort)
      sevenDotOh shouldEqual (7.2 +- 2.toShort)
      sevenDotOh shouldEqual (6.8 +- 2.toShort)
      minusSevenDotOh shouldEqual (-7.1 +- 2.toShort)
      minusSevenDotOh shouldEqual (-6.9 +- 2.toShort)
      minusSevenDotOh shouldEqual (-7.0 +- 2.toShort)
      minusSevenDotOh shouldEqual (-7.2 +- 2.toShort)
      minusSevenDotOh shouldEqual (-6.8 +- 2.toShort)

      // Double +- Byte
      sevenDotOh shouldEqual (7.1 +- 2.toByte)
      sevenDotOh shouldEqual (6.9 +- 2.toByte)
      sevenDotOh shouldEqual (7.0 +- 2.toByte)
      sevenDotOh shouldEqual (7.2 +- 2.toByte)
      sevenDotOh shouldEqual (6.8 +- 2.toByte)
      minusSevenDotOh shouldEqual (-7.1 +- 2.toByte)
      minusSevenDotOh shouldEqual (-6.9 +- 2.toByte)
      minusSevenDotOh shouldEqual (-7.0 +- 2.toByte)
      minusSevenDotOh shouldEqual (-7.2 +- 2.toByte)
      minusSevenDotOh shouldEqual (-6.8 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat shouldEqual (7.1f +- 0.2f)
      sevenDotOhFloat shouldEqual (6.9f +- 0.2f)
      sevenDotOhFloat shouldEqual (7.0f +- 0.2f)
      sevenDotOhFloat shouldEqual (7.2f +- 0.2f)
      sevenDotOhFloat shouldEqual (6.8f +- 0.2f)
      minusSevenDotOhFloat shouldEqual (-7.1f +- 0.2f)
      minusSevenDotOhFloat shouldEqual (-6.9f +- 0.2f)
      minusSevenDotOhFloat shouldEqual (-7.0f +- 0.2f)
      minusSevenDotOhFloat shouldEqual (-7.2f +- 0.2f)
      minusSevenDotOhFloat shouldEqual (-6.8f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat shouldEqual (7.1f +- 2L)
      sevenDotOhFloat shouldEqual (6.9f +- 2L)
      sevenDotOhFloat shouldEqual (7.0f +- 2L)
      sevenDotOhFloat shouldEqual (7.2f +- 2L)
      sevenDotOhFloat shouldEqual (6.8f +- 2L)
      minusSevenDotOhFloat shouldEqual (-7.1f +- 2L)
      minusSevenDotOhFloat shouldEqual (-6.9f +- 2L)
      minusSevenDotOhFloat shouldEqual (-7.0f +- 2L)
      minusSevenDotOhFloat shouldEqual (-7.2f +- 2L)
      minusSevenDotOhFloat shouldEqual (-6.8f +- 2L)

      // Float +- Int
      sevenDotOhFloat shouldEqual (7.1f +- 2)
      sevenDotOhFloat shouldEqual (6.9f +- 2)
      sevenDotOhFloat shouldEqual (7.0f +- 2)
      sevenDotOhFloat shouldEqual (7.2f +- 2)
      sevenDotOhFloat shouldEqual (6.8f +- 2)
      minusSevenDotOhFloat shouldEqual (-7.1f +- 2)
      minusSevenDotOhFloat shouldEqual (-6.9f +- 2)
      minusSevenDotOhFloat shouldEqual (-7.0f +- 2)
      minusSevenDotOhFloat shouldEqual (-7.2f +- 2)
      minusSevenDotOhFloat shouldEqual (-6.8f +- 2)

      // Float +- Short
      sevenDotOhFloat shouldEqual (7.1f +- 2.toShort)
      sevenDotOhFloat shouldEqual (6.9f +- 2.toShort)
      sevenDotOhFloat shouldEqual (7.0f +- 2.toShort)
      sevenDotOhFloat shouldEqual (7.2f +- 2.toShort)
      sevenDotOhFloat shouldEqual (6.8f +- 2.toShort)
      minusSevenDotOhFloat shouldEqual (-7.1f +- 2.toShort)
      minusSevenDotOhFloat shouldEqual (-6.9f +- 2.toShort)
      minusSevenDotOhFloat shouldEqual (-7.0f +- 2.toShort)
      minusSevenDotOhFloat shouldEqual (-7.2f +- 2.toShort)
      minusSevenDotOhFloat shouldEqual (-6.8f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat shouldEqual (7.1f +- 2.toByte)
      sevenDotOhFloat shouldEqual (6.9f +- 2.toByte)
      sevenDotOhFloat shouldEqual (7.0f +- 2.toByte)
      sevenDotOhFloat shouldEqual (7.2f +- 2.toByte)
      sevenDotOhFloat shouldEqual (6.8f +- 2.toByte)
      minusSevenDotOhFloat shouldEqual (-7.1f +- 2.toByte)
      minusSevenDotOhFloat shouldEqual (-6.9f +- 2.toByte)
      minusSevenDotOhFloat shouldEqual (-7.0f +- 2.toByte)
      minusSevenDotOhFloat shouldEqual (-7.2f +- 2.toByte)
      minusSevenDotOhFloat shouldEqual (-6.8f +- 2.toByte)

      // Long +- Long
      sevenLong shouldEqual (9L +- 2L)
      sevenLong shouldEqual (8L +- 2L)
      sevenLong shouldEqual (7L +- 2L)
      sevenLong shouldEqual (6L +- 2L)
      sevenLong shouldEqual (5L +- 2L)
      minusSevenLong shouldEqual (-9L +- 2L)
      minusSevenLong shouldEqual (-8L +- 2L)
      minusSevenLong shouldEqual (-7L +- 2L)
      minusSevenLong shouldEqual (-6L +- 2L)
      minusSevenLong shouldEqual (-5L +- 2L)

      // Long +- Int
      sevenLong shouldEqual (9L +- 2)
      sevenLong shouldEqual (8L +- 2)
      sevenLong shouldEqual (7L +- 2)
      sevenLong shouldEqual (6L +- 2)
      sevenLong shouldEqual (5L +- 2)
      minusSevenLong shouldEqual (-9L +- 2)
      minusSevenLong shouldEqual (-8L +- 2)
      minusSevenLong shouldEqual (-7L +- 2)
      minusSevenLong shouldEqual (-6L +- 2)
      minusSevenLong shouldEqual (-5L +- 2)

      // Long +- Short
      sevenLong shouldEqual (9L +- 2.toShort)
      sevenLong shouldEqual (8L +- 2.toShort)
      sevenLong shouldEqual (7L +- 2.toShort)
      sevenLong shouldEqual (6L +- 2.toShort)
      sevenLong shouldEqual (5L +- 2.toShort)
      minusSevenLong shouldEqual (-9L +- 2.toShort)
      minusSevenLong shouldEqual (-8L +- 2.toShort)
      minusSevenLong shouldEqual (-7L +- 2.toShort)
      minusSevenLong shouldEqual (-6L +- 2.toShort)
      minusSevenLong shouldEqual (-5L +- 2.toShort)

      // Long +- Byte
      sevenLong shouldEqual (9L +- 2.toByte)
      sevenLong shouldEqual (8L +- 2.toByte)
      sevenLong shouldEqual (7L +- 2.toByte)
      sevenLong shouldEqual (6L +- 2.toByte)
      sevenLong shouldEqual (5L +- 2.toByte)
      minusSevenLong shouldEqual (-9L +- 2.toByte)
      minusSevenLong shouldEqual (-8L +- 2.toByte)
      minusSevenLong shouldEqual (-7L +- 2.toByte)
      minusSevenLong shouldEqual (-6L +- 2.toByte)
      minusSevenLong shouldEqual (-5L +- 2.toByte)

      // Int +- Int
      sevenInt shouldEqual (9 +- 2)
      sevenInt shouldEqual (8 +- 2)
      sevenInt shouldEqual (7 +- 2)
      sevenInt shouldEqual (6 +- 2)
      sevenInt shouldEqual (5 +- 2)
      minusSevenInt shouldEqual (-9 +- 2)
      minusSevenInt shouldEqual (-8 +- 2)
      minusSevenInt shouldEqual (-7 +- 2)
      minusSevenInt shouldEqual (-6 +- 2)
      minusSevenInt shouldEqual (-5 +- 2)

      // Int +- Short
      sevenInt shouldEqual (9 +- 2.toShort)
      sevenInt shouldEqual (8 +- 2.toShort)
      sevenInt shouldEqual (7 +- 2.toShort)
      sevenInt shouldEqual (6 +- 2.toShort)
      sevenInt shouldEqual (5 +- 2.toShort)
      minusSevenInt shouldEqual (-9 +- 2.toShort)
      minusSevenInt shouldEqual (-8 +- 2.toShort)
      minusSevenInt shouldEqual (-7 +- 2.toShort)
      minusSevenInt shouldEqual (-6 +- 2.toShort)
      minusSevenInt shouldEqual (-5 +- 2.toShort)

      // Int +- Byte
      sevenInt shouldEqual (9 +- 2.toByte)
      sevenInt shouldEqual (8 +- 2.toByte)
      sevenInt shouldEqual (7 +- 2.toByte)
      sevenInt shouldEqual (6 +- 2.toByte)
      sevenInt shouldEqual (5 +- 2.toByte)
      minusSevenInt shouldEqual (-9 +- 2.toByte)
      minusSevenInt shouldEqual (-8 +- 2.toByte)
      minusSevenInt shouldEqual (-7 +- 2.toByte)
      minusSevenInt shouldEqual (-6 +- 2.toByte)
      minusSevenInt shouldEqual (-5 +- 2.toByte)

      // Short +- Short
      sevenShort shouldEqual (9.toShort +- 2.toShort)
      sevenShort shouldEqual (8.toShort +- 2.toShort)
      sevenShort shouldEqual (7.toShort +- 2.toShort)
      sevenShort shouldEqual (6.toShort +- 2.toShort)
      sevenShort shouldEqual (5.toShort +- 2.toShort)
      minusSevenShort shouldEqual ((-9).toShort +- 2.toShort)
      minusSevenShort shouldEqual ((-8).toShort +- 2.toShort)
      minusSevenShort shouldEqual ((-7).toShort +- 2.toShort)
      minusSevenShort shouldEqual ((-6).toShort +- 2.toShort)
      minusSevenShort shouldEqual ((-5).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort shouldEqual (9.toShort +- 2.toByte)
      sevenShort shouldEqual (8.toShort +- 2.toByte)
      sevenShort shouldEqual (7.toShort +- 2.toByte)
      sevenShort shouldEqual (6.toShort +- 2.toByte)
      sevenShort shouldEqual (5.toShort +- 2.toByte)
      minusSevenShort shouldEqual ((-9).toShort +- 2.toByte)
      minusSevenShort shouldEqual ((-8).toShort +- 2.toByte)
      minusSevenShort shouldEqual ((-7).toShort +- 2.toByte)
      minusSevenShort shouldEqual ((-6).toShort +- 2.toByte)
      minusSevenShort shouldEqual ((-5).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte shouldEqual (9.toByte +- 2.toByte)
      sevenByte shouldEqual (8.toByte +- 2.toByte)
      sevenByte shouldEqual (7.toByte +- 2.toByte)
      sevenByte shouldEqual (6.toByte +- 2.toByte)
      sevenByte shouldEqual (5.toByte +- 2.toByte)
      minusSevenByte shouldEqual ((-9).toByte +- 2.toByte)
      minusSevenByte shouldEqual ((-8).toByte +- 2.toByte)
      minusSevenByte shouldEqual ((-7).toByte +- 2.toByte)
      minusSevenByte shouldEqual ((-6).toByte +- 2.toByte)
      minusSevenByte shouldEqual ((-5).toByte +- 2.toByte)
    }

    def `should throw TFE if the number is outside the given interval` {

      // Double +- Double
      val caught = intercept[TestFailedException] { sevenDotOh shouldEqual (7.5 +- 0.2) }
      assert(caught.getMessage === "7.0 did not equal 7.5 plus or minus 0.2")
      intercept[TestFailedException] { sevenDotOh shouldEqual (6.5 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-7.5 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-6.5 +- 0.2) }

      // Double +- Float
      intercept[TestFailedException] { sevenDotOh shouldEqual (7.5 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh shouldEqual (6.5 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-7.5 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-6.5 +- 0.2f) }

      // Double +- Long
      intercept[TestFailedException] { sevenDotOh shouldEqual (4.0 +- 2L) }
      intercept[TestFailedException] { sevenDotOh shouldEqual (9.1 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-4.0 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-9.1 +- 2L) }

      // Double +- Int
      intercept[TestFailedException] { sevenDotOh shouldEqual (4.0 +- 2) }
      intercept[TestFailedException] { sevenDotOh shouldEqual (9.1 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-4.0 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-9.1 +- 2) }

      // Double +- Short
      intercept[TestFailedException] { sevenDotOh shouldEqual (4.0 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh shouldEqual (9.1 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-4.0 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-9.1 +- 2.toShort) }

      // Double +- Byte
      intercept[TestFailedException] { sevenDotOh shouldEqual (4.0 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh shouldEqual (9.1 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-4.0 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh shouldEqual (-9.1 +- 2.toByte) }

      // Float +- Float
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (7.5f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (6.5f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-7.5f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-6.5f +- 0.2f) }

      // Float +- Long
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (4.0f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (9.1f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-4.0f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-9.1f +- 2L) }

      // Float +- Int
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (4.0f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (9.1f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-4.0f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-9.1f +- 2) }

      // Float +- Short
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (4.0f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (9.1f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-4.0f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-9.1f +- 2.toShort) }

      // Float +- Byte
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (4.0f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat shouldEqual (9.1f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-4.0f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat shouldEqual (-9.1f +- 2.toByte) }

      // Long +- Long
      intercept[TestFailedException] { sevenLong shouldEqual (4L +- 2L) }
      intercept[TestFailedException] { sevenLong shouldEqual (10L +- 2L) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-4L +- 2L) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-10L +- 2L) }

      // Long +- Int
      intercept[TestFailedException] { sevenLong shouldEqual (4L +- 2) }
      intercept[TestFailedException] { sevenLong shouldEqual (10L +- 2) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-4L +- 2) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-10L +- 2) }

      // Long +- Short
      intercept[TestFailedException] { sevenLong shouldEqual (4L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong shouldEqual (10L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-4L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-10L +- 2.toShort) }

      // Long +- Byte
      intercept[TestFailedException] { sevenLong shouldEqual (4L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong shouldEqual (10L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-4L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong shouldEqual (-10L +- 2.toByte) }

      // Int +- Int
      intercept[TestFailedException] { sevenInt shouldEqual (4 +- 2) }
      intercept[TestFailedException] { sevenInt shouldEqual (10 +- 2) }
      intercept[TestFailedException] { minusSevenInt shouldEqual (-4 +- 2) }
      intercept[TestFailedException] { minusSevenInt shouldEqual (-10 +- 2) }

      // Int +- Short
      intercept[TestFailedException] { sevenInt shouldEqual (4 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt shouldEqual (10 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt shouldEqual (-4 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt shouldEqual (-10 +- 2.toShort) }

      // Int +- Byte
      intercept[TestFailedException] { sevenInt shouldEqual (4 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt shouldEqual (10 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt shouldEqual (-4 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt shouldEqual (-10 +- 2.toByte) }

      // Short +- Short
      intercept[TestFailedException] { sevenShort shouldEqual (4.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort shouldEqual (10.toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort shouldEqual ((-4).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort shouldEqual ((-10).toShort +- 2.toShort) }

      // Short +- Byte
      intercept[TestFailedException] { sevenShort shouldEqual (4.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort shouldEqual (10.toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort shouldEqual ((-4).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort shouldEqual ((-10).toShort +- 2.toByte) }

      // Byte +- Byte
      intercept[TestFailedException] { sevenByte shouldEqual (4.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte shouldEqual (10.toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte shouldEqual ((-4).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte shouldEqual ((-10).toByte +- 2.toByte) }
    }
  }
}
