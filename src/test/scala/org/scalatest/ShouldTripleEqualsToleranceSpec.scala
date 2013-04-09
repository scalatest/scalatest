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

class ShouldTripleEqualsToleranceSpec extends Spec /* with NonImplicitAssertions */ with Matchers with TripleEquals with Tolerance {

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
      (7.1 +- 0.2) should === (sevenDotOh)
      (7.5 +- 0.2) should !== (sevenDotOh)
 */
  object `The should === syntax` {

    def `should be true if the number is within the given interval` {

      // Double +- Double
      sevenDotOh should === (7.1 +- 0.2)
      sevenDotOh should === (6.9 +- 0.2)
      sevenDotOh should === (7.0 +- 0.2)
      sevenDotOh should === (7.2 +- 0.2)
      sevenDotOh should === (6.8 +- 0.2)
      minusSevenDotOh should === (-7.1 +- 0.2)
      minusSevenDotOh should === (-6.9 +- 0.2)
      minusSevenDotOh should === (-7.0 +- 0.2)
      minusSevenDotOh should === (-7.2 +- 0.2)
      minusSevenDotOh should === (-6.8 +- 0.2)

      // Double +- Float
      sevenDotOh should === (7.1 +- 0.2f)
      sevenDotOh should === (6.9 +- 0.2f)
      sevenDotOh should === (7.0 +- 0.2f)
      sevenDotOh should === (7.2 +- 0.2f)
      sevenDotOh should === (6.8 +- 0.2f)
      minusSevenDotOh should === (-7.1 +- 0.2f)
      minusSevenDotOh should === (-6.9 +- 0.2f)
      minusSevenDotOh should === (-7.0 +- 0.2f)
      minusSevenDotOh should === (-7.2 +- 0.2f)
      minusSevenDotOh should === (-6.8 +- 0.2f)

      // Double +- Long
      sevenDotOh should === (7.1 +- 2L)
      sevenDotOh should === (6.9 +- 2L)
      sevenDotOh should === (7.0 +- 2L)
      sevenDotOh should === (7.2 +- 2L)
      sevenDotOh should === (6.8 +- 2L)
      minusSevenDotOh should === (-7.1 +- 2L)
      minusSevenDotOh should === (-6.9 +- 2L)
      minusSevenDotOh should === (-7.0 +- 2L)
      minusSevenDotOh should === (-7.2 +- 2L)
      minusSevenDotOh should === (-6.8 +- 2L)

      // Double +- Int
      sevenDotOh should === (7.1 +- 2)
      sevenDotOh should === (6.9 +- 2)
      sevenDotOh should === (7.0 +- 2)
      sevenDotOh should === (7.2 +- 2)
      sevenDotOh should === (6.8 +- 2)
      minusSevenDotOh should === (-7.1 +- 2)
      minusSevenDotOh should === (-6.9 +- 2)
      minusSevenDotOh should === (-7.0 +- 2)
      minusSevenDotOh should === (-7.2 +- 2)
      minusSevenDotOh should === (-6.8 +- 2)

      // Double +- Short
      sevenDotOh should === (7.1 +- 2.toShort)
      sevenDotOh should === (6.9 +- 2.toShort)
      sevenDotOh should === (7.0 +- 2.toShort)
      sevenDotOh should === (7.2 +- 2.toShort)
      sevenDotOh should === (6.8 +- 2.toShort)
      minusSevenDotOh should === (-7.1 +- 2.toShort)
      minusSevenDotOh should === (-6.9 +- 2.toShort)
      minusSevenDotOh should === (-7.0 +- 2.toShort)
      minusSevenDotOh should === (-7.2 +- 2.toShort)
      minusSevenDotOh should === (-6.8 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should === (7.1 +- 2.toByte)
      sevenDotOh should === (6.9 +- 2.toByte)
      sevenDotOh should === (7.0 +- 2.toByte)
      sevenDotOh should === (7.2 +- 2.toByte)
      sevenDotOh should === (6.8 +- 2.toByte)
      minusSevenDotOh should === (-7.1 +- 2.toByte)
      minusSevenDotOh should === (-6.9 +- 2.toByte)
      minusSevenDotOh should === (-7.0 +- 2.toByte)
      minusSevenDotOh should === (-7.2 +- 2.toByte)
      minusSevenDotOh should === (-6.8 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should === (7.1f +- 0.2f)
      sevenDotOhFloat should === (6.9f +- 0.2f)
      sevenDotOhFloat should === (7.0f +- 0.2f)
      sevenDotOhFloat should === (7.2f +- 0.2f)
      sevenDotOhFloat should === (6.8f +- 0.2f)
      minusSevenDotOhFloat should === (-7.1f +- 0.2f)
      minusSevenDotOhFloat should === (-6.9f +- 0.2f)
      minusSevenDotOhFloat should === (-7.0f +- 0.2f)
      minusSevenDotOhFloat should === (-7.2f +- 0.2f)
      minusSevenDotOhFloat should === (-6.8f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should === (7.1f +- 2L)
      sevenDotOhFloat should === (6.9f +- 2L)
      sevenDotOhFloat should === (7.0f +- 2L)
      sevenDotOhFloat should === (7.2f +- 2L)
      sevenDotOhFloat should === (6.8f +- 2L)
      minusSevenDotOhFloat should === (-7.1f +- 2L)
      minusSevenDotOhFloat should === (-6.9f +- 2L)
      minusSevenDotOhFloat should === (-7.0f +- 2L)
      minusSevenDotOhFloat should === (-7.2f +- 2L)
      minusSevenDotOhFloat should === (-6.8f +- 2L)

      // Float +- Int
      sevenDotOhFloat should === (7.1f +- 2)
      sevenDotOhFloat should === (6.9f +- 2)
      sevenDotOhFloat should === (7.0f +- 2)
      sevenDotOhFloat should === (7.2f +- 2)
      sevenDotOhFloat should === (6.8f +- 2)
      minusSevenDotOhFloat should === (-7.1f +- 2)
      minusSevenDotOhFloat should === (-6.9f +- 2)
      minusSevenDotOhFloat should === (-7.0f +- 2)
      minusSevenDotOhFloat should === (-7.2f +- 2)
      minusSevenDotOhFloat should === (-6.8f +- 2)

      // Float +- Short
      sevenDotOhFloat should === (7.1f +- 2.toShort)
      sevenDotOhFloat should === (6.9f +- 2.toShort)
      sevenDotOhFloat should === (7.0f +- 2.toShort)
      sevenDotOhFloat should === (7.2f +- 2.toShort)
      sevenDotOhFloat should === (6.8f +- 2.toShort)
      minusSevenDotOhFloat should === (-7.1f +- 2.toShort)
      minusSevenDotOhFloat should === (-6.9f +- 2.toShort)
      minusSevenDotOhFloat should === (-7.0f +- 2.toShort)
      minusSevenDotOhFloat should === (-7.2f +- 2.toShort)
      minusSevenDotOhFloat should === (-6.8f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should === (7.1f +- 2.toByte)
      sevenDotOhFloat should === (6.9f +- 2.toByte)
      sevenDotOhFloat should === (7.0f +- 2.toByte)
      sevenDotOhFloat should === (7.2f +- 2.toByte)
      sevenDotOhFloat should === (6.8f +- 2.toByte)
      minusSevenDotOhFloat should === (-7.1f +- 2.toByte)
      minusSevenDotOhFloat should === (-6.9f +- 2.toByte)
      minusSevenDotOhFloat should === (-7.0f +- 2.toByte)
      minusSevenDotOhFloat should === (-7.2f +- 2.toByte)
      minusSevenDotOhFloat should === (-6.8f +- 2.toByte)

      // Long +- Long
      sevenLong should === (9L +- 2L)
      sevenLong should === (8L +- 2L)
      sevenLong should === (7L +- 2L)
      sevenLong should === (6L +- 2L)
      sevenLong should === (5L +- 2L)
      minusSevenLong should === (-9L +- 2L)
      minusSevenLong should === (-8L +- 2L)
      minusSevenLong should === (-7L +- 2L)
      minusSevenLong should === (-6L +- 2L)
      minusSevenLong should === (-5L +- 2L)

      // Long +- Int
      sevenLong should === (9L +- 2)
      sevenLong should === (8L +- 2)
      sevenLong should === (7L +- 2)
      sevenLong should === (6L +- 2)
      sevenLong should === (5L +- 2)
      minusSevenLong should === (-9L +- 2)
      minusSevenLong should === (-8L +- 2)
      minusSevenLong should === (-7L +- 2)
      minusSevenLong should === (-6L +- 2)
      minusSevenLong should === (-5L +- 2)

      // Long +- Short
      sevenLong should === (9L +- 2.toShort)
      sevenLong should === (8L +- 2.toShort)
      sevenLong should === (7L +- 2.toShort)
      sevenLong should === (6L +- 2.toShort)
      sevenLong should === (5L +- 2.toShort)
      minusSevenLong should === (-9L +- 2.toShort)
      minusSevenLong should === (-8L +- 2.toShort)
      minusSevenLong should === (-7L +- 2.toShort)
      minusSevenLong should === (-6L +- 2.toShort)
      minusSevenLong should === (-5L +- 2.toShort)

      // Long +- Byte
      sevenLong should === (9L +- 2.toByte)
      sevenLong should === (8L +- 2.toByte)
      sevenLong should === (7L +- 2.toByte)
      sevenLong should === (6L +- 2.toByte)
      sevenLong should === (5L +- 2.toByte)
      minusSevenLong should === (-9L +- 2.toByte)
      minusSevenLong should === (-8L +- 2.toByte)
      minusSevenLong should === (-7L +- 2.toByte)
      minusSevenLong should === (-6L +- 2.toByte)
      minusSevenLong should === (-5L +- 2.toByte)

      // Int +- Int
      sevenInt should === (9 +- 2)
      sevenInt should === (8 +- 2)
      sevenInt should === (7 +- 2)
      sevenInt should === (6 +- 2)
      sevenInt should === (5 +- 2)
      minusSevenInt should === (-9 +- 2)
      minusSevenInt should === (-8 +- 2)
      minusSevenInt should === (-7 +- 2)
      minusSevenInt should === (-6 +- 2)
      minusSevenInt should === (-5 +- 2)

      // Int +- Short
      sevenInt should === (9 +- 2.toShort)
      sevenInt should === (8 +- 2.toShort)
      sevenInt should === (7 +- 2.toShort)
      sevenInt should === (6 +- 2.toShort)
      sevenInt should === (5 +- 2.toShort)
      minusSevenInt should === (-9 +- 2.toShort)
      minusSevenInt should === (-8 +- 2.toShort)
      minusSevenInt should === (-7 +- 2.toShort)
      minusSevenInt should === (-6 +- 2.toShort)
      minusSevenInt should === (-5 +- 2.toShort)

      // Int +- Byte
      sevenInt should === (9 +- 2.toByte)
      sevenInt should === (8 +- 2.toByte)
      sevenInt should === (7 +- 2.toByte)
      sevenInt should === (6 +- 2.toByte)
      sevenInt should === (5 +- 2.toByte)
      minusSevenInt should === (-9 +- 2.toByte)
      minusSevenInt should === (-8 +- 2.toByte)
      minusSevenInt should === (-7 +- 2.toByte)
      minusSevenInt should === (-6 +- 2.toByte)
      minusSevenInt should === (-5 +- 2.toByte)

      // Short +- Short
      sevenShort should === (9.toShort +- 2.toShort)
      sevenShort should === (8.toShort +- 2.toShort)
      sevenShort should === (7.toShort +- 2.toShort)
      sevenShort should === (6.toShort +- 2.toShort)
      sevenShort should === (5.toShort +- 2.toShort)
      minusSevenShort should === ((-9).toShort +- 2.toShort)
      minusSevenShort should === ((-8).toShort +- 2.toShort)
      minusSevenShort should === ((-7).toShort +- 2.toShort)
      minusSevenShort should === ((-6).toShort +- 2.toShort)
      minusSevenShort should === ((-5).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should === (9.toShort +- 2.toByte)
      sevenShort should === (8.toShort +- 2.toByte)
      sevenShort should === (7.toShort +- 2.toByte)
      sevenShort should === (6.toShort +- 2.toByte)
      sevenShort should === (5.toShort +- 2.toByte)
      minusSevenShort should === ((-9).toShort +- 2.toByte)
      minusSevenShort should === ((-8).toShort +- 2.toByte)
      minusSevenShort should === ((-7).toShort +- 2.toByte)
      minusSevenShort should === ((-6).toShort +- 2.toByte)
      minusSevenShort should === ((-5).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should === (9.toByte +- 2.toByte)
      sevenByte should === (8.toByte +- 2.toByte)
      sevenByte should === (7.toByte +- 2.toByte)
      sevenByte should === (6.toByte +- 2.toByte)
      sevenByte should === (5.toByte +- 2.toByte)
      minusSevenByte should === ((-9).toByte +- 2.toByte)
      minusSevenByte should === ((-8).toByte +- 2.toByte)
      minusSevenByte should === ((-7).toByte +- 2.toByte)
      minusSevenByte should === ((-6).toByte +- 2.toByte)
      minusSevenByte should === ((-5).toByte +- 2.toByte)
    }

    def `should throw TFE if the number is outside the given interval` {

      // Double +- Double
      val caught = intercept[TestFailedException] { sevenDotOh should === (7.5 +- 0.2) }
      assert(caught.getMessage === "7.0 did not equal 7.5 plus or minus 0.2")
      intercept[TestFailedException] { sevenDotOh should === (6.5 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should === (-7.5 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should === (-6.5 +- 0.2) }

      // Double +- Float
      intercept[TestFailedException] { sevenDotOh should === (7.5 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should === (6.5 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should === (-7.5 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should === (-6.5 +- 0.2f) }

      // Double +- Long
      intercept[TestFailedException] { sevenDotOh should === (4.0 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should === (9.1 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should === (-4.0 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should === (-9.1 +- 2L) }

      // Double +- Int
      intercept[TestFailedException] { sevenDotOh should === (4.0 +- 2) }
      intercept[TestFailedException] { sevenDotOh should === (9.1 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should === (-4.0 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should === (-9.1 +- 2) }

      // Double +- Short
      intercept[TestFailedException] { sevenDotOh should === (4.0 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should === (9.1 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should === (-4.0 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should === (-9.1 +- 2.toShort) }

      // Double +- Byte
      intercept[TestFailedException] { sevenDotOh should === (4.0 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should === (9.1 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should === (-4.0 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should === (-9.1 +- 2.toByte) }

      // Float +- Float
      intercept[TestFailedException] { sevenDotOhFloat should === (7.5f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should === (6.5f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-7.5f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-6.5f +- 0.2f) }

      // Float +- Long
      intercept[TestFailedException] { sevenDotOhFloat should === (4.0f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should === (9.1f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-4.0f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-9.1f +- 2L) }

      // Float +- Int
      intercept[TestFailedException] { sevenDotOhFloat should === (4.0f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should === (9.1f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-4.0f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-9.1f +- 2) }

      // Float +- Short
      intercept[TestFailedException] { sevenDotOhFloat should === (4.0f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should === (9.1f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-4.0f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-9.1f +- 2.toShort) }

      // Float +- Byte
      intercept[TestFailedException] { sevenDotOhFloat should === (4.0f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should === (9.1f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-4.0f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should === (-9.1f +- 2.toByte) }

      // Long +- Long
      intercept[TestFailedException] { sevenLong should === (4L +- 2L) }
      intercept[TestFailedException] { sevenLong should === (10L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should === (-4L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should === (-10L +- 2L) }

      // Long +- Int
      intercept[TestFailedException] { sevenLong should === (4L +- 2) }
      intercept[TestFailedException] { sevenLong should === (10L +- 2) }
      intercept[TestFailedException] { minusSevenLong should === (-4L +- 2) }
      intercept[TestFailedException] { minusSevenLong should === (-10L +- 2) }

      // Long +- Short
      intercept[TestFailedException] { sevenLong should === (4L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should === (10L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should === (-4L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should === (-10L +- 2.toShort) }

      // Long +- Byte
      intercept[TestFailedException] { sevenLong should === (4L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should === (10L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should === (-4L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should === (-10L +- 2.toByte) }

      // Int +- Int
      intercept[TestFailedException] { sevenInt should === (4 +- 2) }
      intercept[TestFailedException] { sevenInt should === (10 +- 2) }
      intercept[TestFailedException] { minusSevenInt should === (-4 +- 2) }
      intercept[TestFailedException] { minusSevenInt should === (-10 +- 2) }

      // Int +- Short
      intercept[TestFailedException] { sevenInt should === (4 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should === (10 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should === (-4 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should === (-10 +- 2.toShort) }

      // Int +- Byte
      intercept[TestFailedException] { sevenInt should === (4 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should === (10 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should === (-4 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should === (-10 +- 2.toByte) }

      // Short +- Short
      intercept[TestFailedException] { sevenShort should === (4.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should === (10.toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should === ((-4).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should === ((-10).toShort +- 2.toShort) }

      // Short +- Byte
      intercept[TestFailedException] { sevenShort should === (4.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should === (10.toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should === ((-4).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should === ((-10).toShort +- 2.toByte) }

      // Byte +- Byte
      intercept[TestFailedException] { sevenByte should === (4.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should === (10.toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should === ((-4).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should === ((-10).toByte +- 2.toByte) }
    }
  }

  object `The !== syntax` {

    def `should succeed if the number is outside the given interval` {

      // Double +- Double
      sevenDotOh should !== (7.5 +- 0.2)
      sevenDotOh should !== (6.5 +- 0.2)
      minusSevenDotOh should !== (-7.5 +- 0.2)
      minusSevenDotOh should !== (-6.5 +- 0.2)

      // Double +- Float
      sevenDotOh should !== (7.5 +- 0.2f)
      sevenDotOh should !== (6.5 +- 0.2f)
      minusSevenDotOh should !== (-7.5 +- 0.2f)
      minusSevenDotOh should !== (-6.5 +- 0.2f)

      // Double +- Long
      sevenDotOh should !== (4.0 +- 2L)
      sevenDotOh should !== (9.1 +- 2L)
      minusSevenDotOh should !== (-4.0 +- 2L)
      minusSevenDotOh should !== (-9.1 +- 2L)

      // Double +- Int
      sevenDotOh should !== (4.0 +- 2)
      sevenDotOh should !== (9.1 +- 2)
      minusSevenDotOh should !== (-4.0 +- 2)
      minusSevenDotOh should !== (-9.1 +- 2)

      // Double +- Short
      sevenDotOh should !== (4.0 +- 2.toShort)
      sevenDotOh should !== (9.1 +- 2.toShort)
      minusSevenDotOh should !== (-4.0 +- 2.toShort)
      minusSevenDotOh should !== (-9.1 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should !== (4.0 +- 2.toByte)
      sevenDotOh should !== (9.1 +- 2.toByte)
      minusSevenDotOh should !== (-4.0 +- 2.toByte)
      minusSevenDotOh should !== (-9.1 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should !== (7.5f +- 0.2f)
      sevenDotOhFloat should !== (6.5f +- 0.2f)
      minusSevenDotOhFloat should !== (-7.5f +- 0.2f)
      minusSevenDotOhFloat should !== (-6.5f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should !== (4.0f +- 2L)
      sevenDotOhFloat should !== (9.1f +- 2L)
      minusSevenDotOhFloat should !== (-4.0f +- 2L)
      minusSevenDotOhFloat should !== (-9.1f +- 2L)

      // Float +- Int
      sevenDotOhFloat should !== (4.0f +- 2)
      sevenDotOhFloat should !== (9.1f +- 2)
      minusSevenDotOhFloat should !== (-4.0f +- 2)
      minusSevenDotOhFloat should !== (-9.1f +- 2)

      // Float +- Short
      sevenDotOhFloat should !== (4.0f +- 2.toShort)
      sevenDotOhFloat should !== (9.1f +- 2.toShort)
      minusSevenDotOhFloat should !== (-4.0f +- 2.toShort)
      minusSevenDotOhFloat should !== (-9.1f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should !== (4.0f +- 2.toByte)
      sevenDotOhFloat should !== (9.1f +- 2.toByte)
      minusSevenDotOhFloat should !== (-4.0f +- 2.toByte)
      minusSevenDotOhFloat should !== (-9.1f +- 2.toByte)

      // Long +- Long
      sevenLong should !== (4L +- 2L)
      sevenLong should !== (10L +- 2L)
      minusSevenLong should !== (-4L +- 2L)
      minusSevenLong should !== (-10L +- 2L)

      // Long +- Int
      sevenLong should !== (4L +- 2)
      sevenLong should !== (10L +- 2)
      minusSevenLong should !== (-4L +- 2)
      minusSevenLong should !== (-10L +- 2)

      // Long +- Short
      sevenLong should !== (4L +- 2.toShort)
      sevenLong should !== (10L +- 2.toShort)
      minusSevenLong should !== (-4L +- 2.toShort)
      minusSevenLong should !== (-10L +- 2.toShort)

      // Long +- Byte
      sevenLong should !== (4L +- 2.toByte)
      sevenLong should !== (10L +- 2.toByte)
      minusSevenLong should !== (-4L +- 2.toByte)
      minusSevenLong should !== (-10L +- 2.toByte)

      // Int +- Int
      sevenInt should !== (4 +- 2)
      sevenInt should !== (10 +- 2)
      minusSevenInt should !== (-4 +- 2)
      minusSevenInt should !== (-10 +- 2)

      // Int +- Short
      sevenInt should !== (4 +- 2.toShort)
      sevenInt should !== (10 +- 2.toShort)
      minusSevenInt should !== (-4 +- 2.toShort)
      minusSevenInt should !== (-10 +- 2.toShort)

      // Int +- Byte
      sevenInt should !== (4 +- 2.toByte)
      sevenInt should !== (10 +- 2.toByte)
      minusSevenInt should !== (-4 +- 2.toByte)
      minusSevenInt should !== (-10 +- 2.toByte)

      // Short +- Short
      sevenShort should !== (4.toShort +- 2.toShort)
      sevenShort should !== (10.toShort +- 2.toShort)
      minusSevenShort should !== ((-4).toShort +- 2.toShort)
      minusSevenShort should !== ((-10).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should !== (4.toShort +- 2.toByte)
      sevenShort should !== (10.toShort +- 2.toByte)
      minusSevenShort should !== ((-4).toShort +- 2.toByte)
      minusSevenShort should !== ((-10).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should !== (4.toByte +- 2.toByte)
      sevenByte should !== (10.toByte +- 2.toByte)
      minusSevenByte should !== ((-4).toByte +- 2.toByte)
      minusSevenByte should !== ((-10).toByte +- 2.toByte)
    }

    def `should throw TFE if the number is within the given interval` {

      // Double +- Double
      val caught = intercept[TestFailedException] { sevenDotOh should !== (7.1 +- 0.2) }
      assert(caught.getMessage === "7.0 equaled 7.1 plus or minus 0.2")
      intercept[TestFailedException] { sevenDotOh should !== (6.9 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should !== (7.0 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should !== (7.2 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should !== (6.8 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.1 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.9 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.0 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.2 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.8 +- 0.2) }

      // Double +- Float
      intercept[TestFailedException] { sevenDotOh should !== (7.1 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should !== (6.9 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should !== (7.0 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should !== (7.2 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should !== (6.8 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.1 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.9 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.0 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.2 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.8 +- 0.2f) }

      // Double +- Long
      intercept[TestFailedException] { sevenDotOh should !== (7.1 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should !== (6.9 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should !== (7.0 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should !== (7.2 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should !== (6.8 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.1 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.9 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.0 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.2 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.8 +- 2L) }

      // Double +- Int
      intercept[TestFailedException] { sevenDotOh should !== (7.1 +- 2) }
      intercept[TestFailedException] { sevenDotOh should !== (6.9 +- 2) }
      intercept[TestFailedException] { sevenDotOh should !== (7.0 +- 2) }
      intercept[TestFailedException] { sevenDotOh should !== (7.2 +- 2) }
      intercept[TestFailedException] { sevenDotOh should !== (6.8 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.1 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.9 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.0 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.2 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.8 +- 2) }

      // Double +- Short
      intercept[TestFailedException] { sevenDotOh should !== (7.1 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should !== (6.9 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should !== (7.0 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should !== (7.2 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should !== (6.8 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.1 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.9 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.0 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.2 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.8 +- 2.toShort) }

      // Double +- Byte
      intercept[TestFailedException] { sevenDotOh should !== (7.1 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should !== (6.9 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should !== (7.0 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should !== (7.2 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should !== (6.8 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.1 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.9 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.0 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-7.2 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should !== (-6.8 +- 2.toByte) }

      // Float +- Float
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.1f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.9f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.0f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.2f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.8f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.1f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.9f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.0f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.2f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.8f +- 0.2f) }

      // Float +- Long
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.1f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.9f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.0f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.2f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.8f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.1f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.9f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.0f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.2f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.8f +- 2L) }

      // Float +- Int
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.1f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.9f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.0f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.2f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.8f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.1f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.9f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.0f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.2f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.8f +- 2) }

      // Float +- Short
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.1f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.9f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.0f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.2f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.8f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.1f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.9f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.0f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.2f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.8f +- 2.toShort) }

      // Float +- Byte
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.1f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.9f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.0f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (7.2f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should !== (6.8f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.1f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.9f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.0f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-7.2f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should !== (-6.8f +- 2.toByte) }

      // Long +- Long
      intercept[TestFailedException] { sevenLong should !== (9L +- 2L) }
      intercept[TestFailedException] { sevenLong should !== (8L +- 2L) }
      intercept[TestFailedException] { sevenLong should !== (7L +- 2L) }
      intercept[TestFailedException] { sevenLong should !== (6L +- 2L) }
      intercept[TestFailedException] { sevenLong should !== (5L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should !== (-9L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should !== (-8L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should !== (-7L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should !== (-6L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should !== (-5L +- 2L) }

      // Long +- Int
      intercept[TestFailedException] { sevenLong should !== (9L +- 2) }
      intercept[TestFailedException] { sevenLong should !== (8L +- 2) }
      intercept[TestFailedException] { sevenLong should !== (7L +- 2) }
      intercept[TestFailedException] { sevenLong should !== (6L +- 2) }
      intercept[TestFailedException] { sevenLong should !== (5L +- 2) }
      intercept[TestFailedException] { minusSevenLong should !== (-9L +- 2) }
      intercept[TestFailedException] { minusSevenLong should !== (-8L +- 2) }
      intercept[TestFailedException] { minusSevenLong should !== (-7L +- 2) }
      intercept[TestFailedException] { minusSevenLong should !== (-6L +- 2) }
      intercept[TestFailedException] { minusSevenLong should !== (-5L +- 2) }

      // Long +- Short
      intercept[TestFailedException] { sevenLong should !== (9L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should !== (8L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should !== (7L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should !== (6L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should !== (5L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should !== (-9L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should !== (-8L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should !== (-7L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should !== (-6L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should !== (-5L +- 2.toShort) }

      // Long +- Byte
      intercept[TestFailedException] { sevenLong should !== (9L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should !== (8L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should !== (7L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should !== (6L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should !== (5L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should !== (-9L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should !== (-8L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should !== (-7L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should !== (-6L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should !== (-5L +- 2.toByte) }

      // Int +- Int
      intercept[TestFailedException] { sevenInt should !== (9 +- 2) }
      intercept[TestFailedException] { sevenInt should !== (8 +- 2) }
      intercept[TestFailedException] { sevenInt should !== (7 +- 2) }
      intercept[TestFailedException] { sevenInt should !== (6 +- 2) }
      intercept[TestFailedException] { sevenInt should !== (5 +- 2) }
      intercept[TestFailedException] { minusSevenInt should !== (-9 +- 2) }
      intercept[TestFailedException] { minusSevenInt should !== (-8 +- 2) }
      intercept[TestFailedException] { minusSevenInt should !== (-7 +- 2) }
      intercept[TestFailedException] { minusSevenInt should !== (-6 +- 2) }
      intercept[TestFailedException] { minusSevenInt should !== (-5 +- 2) }

      // Int +- Short
      intercept[TestFailedException] { sevenInt should !== (9 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should !== (8 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should !== (7 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should !== (6 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should !== (5 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should !== (-9 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should !== (-8 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should !== (-7 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should !== (-6 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should !== (-5 +- 2.toShort) }

      // Int +- Byte
      intercept[TestFailedException] { sevenInt should !== (9 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should !== (8 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should !== (7 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should !== (6 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should !== (5 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should !== (-9 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should !== (-8 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should !== (-7 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should !== (-6 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should !== (-5 +- 2.toByte) }

      // Short +- Short
      intercept[TestFailedException] { sevenShort should !== (9.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should !== (8.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should !== (7.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should !== (6.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should !== (5.toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should !== ((-9).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should !== ((-8).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should !== ((-7).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should !== ((-6).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should !== ((-5).toShort +- 2.toShort) }

      // Short +- Byte
      intercept[TestFailedException] { sevenShort should !== (9.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should !== (8.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should !== (7.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should !== (6.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should !== (5.toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should !== ((-9).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should !== ((-8).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should !== ((-7).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should !== ((-6).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should !== ((-5).toShort +- 2.toByte) }

      // Byte +- Byte
      intercept[TestFailedException] { sevenByte should !== (9.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should !== (8.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should !== (7.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should !== (6.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should !== (5.toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should !== ((-9).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should !== ((-8).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should !== ((-7).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should !== ((-6).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should !== ((-5).toByte +- 2.toByte) }
    }
  }

  object `The X +- Y syntax` {

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative` {

      // Double +- Double
      val caught1 = intercept[IllegalArgumentException] {
        sevenDotOh should === (7.1 +- -0.2)
      }
      assert(caught1.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.", caught1.getMessage)

      // Double +- Float
      val caught2 = intercept[IllegalArgumentException] {
        sevenDotOh should === (7.1 +- -0.2f)
      }
      assert(caught2.getMessage === "-0.20000000298023224 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Long
      val caught3 = intercept[IllegalArgumentException] {
        sevenDotOh should === (7.1 +- -2L)
      }
      assert(caught3.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Int
      val caught4 = intercept[IllegalArgumentException] {
        sevenDotOh should === (7.1 +- -2)
      }
      assert(caught4.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Short
      val caught5 = intercept[IllegalArgumentException] {
        sevenDotOh should === (7.1 +- (-2).toShort)
      }
      assert(caught5.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Byte
      val caught6 = intercept[IllegalArgumentException] {
        sevenDotOh should === (7.1 +- (-2).toByte)
      }
      assert(caught6.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Float
      val caught7 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should === (7.1f +- -0.2f)
      }
      assert(caught7.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Long
      val caught8 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should === (7.1f +- -2L)
      }
      assert(caught8.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Int
      val caught9 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should === (7.1f +- -2)
      }
      assert(caught9.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Short
      val caught10 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should === (7.1f +- (-2).toShort)
      }
      assert(caught10.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Byte
      val caught11 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should === (7.1f +- (-2).toByte)
      }
      assert(caught11.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Long
      val caught12 = intercept[IllegalArgumentException] {
        sevenLong should === (9L +- -2L)
      }
      assert(caught12.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Int
      val caught13 = intercept[IllegalArgumentException] {
        sevenLong should === (9L +- -2)
      }
      assert(caught13.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Short
      val caught14 = intercept[IllegalArgumentException] {
        sevenLong should === (9L +- (-2).toShort)
      }
      assert(caught14.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Byte
      val caught15 = intercept[IllegalArgumentException] {
        sevenLong should === (9L +- (-2).toByte)
      }
      assert(caught15.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Int
      val caught16 = intercept[IllegalArgumentException] {
        sevenInt should === (9 +- -2)
      }
      assert(caught16.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Short
      val caught17 = intercept[IllegalArgumentException] {
        sevenInt should === (9 +- (-2).toShort)
      }
      assert(caught17.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Byte
      val caught18 = intercept[IllegalArgumentException] {
        sevenInt should === (9 +- (-2).toByte)
      }
      assert(caught18.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Short
      val caught19 = intercept[IllegalArgumentException] {
        sevenShort should === (9.toShort +- (-2).toShort)
      }
      assert(caught19.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Byte
      val caught20 = intercept[IllegalArgumentException] {
        sevenShort should === (9.toShort +- (-2).toByte)
      }
      assert(caught20.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Byte +- Byte
      val caught21 = intercept[IllegalArgumentException] {
        sevenByte should === (9.toByte +- (-2).toByte)
      }
      assert(caught21.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")
    }
  }
}
