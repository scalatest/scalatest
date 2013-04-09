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

import org.scalatest.exceptions.TestFailedException

class ShouldPlusOrMinusSpec extends Spec with Matchers {

  object `The be (X +- Y) syntax` {

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
      minusSevenDotOhFloat should be (-6.8f +- 0.2d)
    */
    def `should do nothing if the number is within the specified range` {

      // Double +- Double
      sevenDotOh should be (7.1 +- 0.2)
      sevenDotOh should be (6.9 +- 0.2)
      sevenDotOh should be (7.0 +- 0.2)
      sevenDotOh should be (7.2 +- 0.2)
      sevenDotOh should be (6.8 +- 0.2)
      minusSevenDotOh should be (-7.1 +- 0.2)
      minusSevenDotOh should be (-6.9 +- 0.2)
      minusSevenDotOh should be (-7.0 +- 0.2)
      minusSevenDotOh should be (-7.2 +- 0.2)
      minusSevenDotOh should be (-6.8 +- 0.2)

      // Double +- Float
      sevenDotOh should be (7.1 +- 0.2f)
      sevenDotOh should be (6.9 +- 0.2f)
      sevenDotOh should be (7.0 +- 0.2f)
      sevenDotOh should be (7.2 +- 0.2f)
      sevenDotOh should be (6.8 +- 0.2f)
      minusSevenDotOh should be (-7.1 +- 0.2f)
      minusSevenDotOh should be (-6.9 +- 0.2f)
      minusSevenDotOh should be (-7.0 +- 0.2f)
      minusSevenDotOh should be (-7.2 +- 0.2f)
      minusSevenDotOh should be (-6.8 +- 0.2f)

      // Double +- Long
      sevenDotOh should be (7.1 +- 2L)
      sevenDotOh should be (6.9 +- 2L)
      sevenDotOh should be (7.0 +- 2L)
      sevenDotOh should be (7.2 +- 2L)
      sevenDotOh should be (6.8 +- 2L)
      minusSevenDotOh should be (-7.1 +- 2L)
      minusSevenDotOh should be (-6.9 +- 2L)
      minusSevenDotOh should be (-7.0 +- 2L)
      minusSevenDotOh should be (-7.2 +- 2L)
      minusSevenDotOh should be (-6.8 +- 2L)

      // Double +- Int
      sevenDotOh should be (7.1 +- 2)
      sevenDotOh should be (6.9 +- 2)
      sevenDotOh should be (7.0 +- 2)
      sevenDotOh should be (7.2 +- 2)
      sevenDotOh should be (6.8 +- 2)
      minusSevenDotOh should be (-7.1 +- 2)
      minusSevenDotOh should be (-6.9 +- 2)
      minusSevenDotOh should be (-7.0 +- 2)
      minusSevenDotOh should be (-7.2 +- 2)
      minusSevenDotOh should be (-6.8 +- 2)

      // Double +- Short
      sevenDotOh should be (7.1 +- 2.toShort)
      sevenDotOh should be (6.9 +- 2.toShort)
      sevenDotOh should be (7.0 +- 2.toShort)
      sevenDotOh should be (7.2 +- 2.toShort)
      sevenDotOh should be (6.8 +- 2.toShort)
      minusSevenDotOh should be (-7.1 +- 2.toShort)
      minusSevenDotOh should be (-6.9 +- 2.toShort)
      minusSevenDotOh should be (-7.0 +- 2.toShort)
      minusSevenDotOh should be (-7.2 +- 2.toShort)
      minusSevenDotOh should be (-6.8 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should be (7.1 +- 2.toByte)
      sevenDotOh should be (6.9 +- 2.toByte)
      sevenDotOh should be (7.0 +- 2.toByte)
      sevenDotOh should be (7.2 +- 2.toByte)
      sevenDotOh should be (6.8 +- 2.toByte)
      minusSevenDotOh should be (-7.1 +- 2.toByte)
      minusSevenDotOh should be (-6.9 +- 2.toByte)
      minusSevenDotOh should be (-7.0 +- 2.toByte)
      minusSevenDotOh should be (-7.2 +- 2.toByte)
      minusSevenDotOh should be (-6.8 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should be (7.1f +- 0.2f)
      sevenDotOhFloat should be (6.9f +- 0.2f)
      sevenDotOhFloat should be (7.0f +- 0.2f)
      sevenDotOhFloat should be (7.2f +- 0.2f)
      sevenDotOhFloat should be (6.8f +- 0.2f)
      minusSevenDotOhFloat should be (-7.1f +- 0.2f)
      minusSevenDotOhFloat should be (-6.9f +- 0.2f)
      minusSevenDotOhFloat should be (-7.0f +- 0.2f)
      minusSevenDotOhFloat should be (-7.2f +- 0.2f)
      minusSevenDotOhFloat should be (-6.8f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should be (7.1f +- 2L)
      sevenDotOhFloat should be (6.9f +- 2L)
      sevenDotOhFloat should be (7.0f +- 2L)
      sevenDotOhFloat should be (7.2f +- 2L)
      sevenDotOhFloat should be (6.8f +- 2L)
      minusSevenDotOhFloat should be (-7.1f +- 2L)
      minusSevenDotOhFloat should be (-6.9f +- 2L)
      minusSevenDotOhFloat should be (-7.0f +- 2L)
      minusSevenDotOhFloat should be (-7.2f +- 2L)
      minusSevenDotOhFloat should be (-6.8f +- 2L)

      // Float +- Int
      sevenDotOhFloat should be (7.1f +- 2)
      sevenDotOhFloat should be (6.9f +- 2)
      sevenDotOhFloat should be (7.0f +- 2)
      sevenDotOhFloat should be (7.2f +- 2)
      sevenDotOhFloat should be (6.8f +- 2)
      minusSevenDotOhFloat should be (-7.1f +- 2)
      minusSevenDotOhFloat should be (-6.9f +- 2)
      minusSevenDotOhFloat should be (-7.0f +- 2)
      minusSevenDotOhFloat should be (-7.2f +- 2)
      minusSevenDotOhFloat should be (-6.8f +- 2)

      // Float +- Short
      sevenDotOhFloat should be (7.1f +- 2.toShort)
      sevenDotOhFloat should be (6.9f +- 2.toShort)
      sevenDotOhFloat should be (7.0f +- 2.toShort)
      sevenDotOhFloat should be (7.2f +- 2.toShort)
      sevenDotOhFloat should be (6.8f +- 2.toShort)
      minusSevenDotOhFloat should be (-7.1f +- 2.toShort)
      minusSevenDotOhFloat should be (-6.9f +- 2.toShort)
      minusSevenDotOhFloat should be (-7.0f +- 2.toShort)
      minusSevenDotOhFloat should be (-7.2f +- 2.toShort)
      minusSevenDotOhFloat should be (-6.8f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should be (7.1f +- 2.toByte)
      sevenDotOhFloat should be (6.9f +- 2.toByte)
      sevenDotOhFloat should be (7.0f +- 2.toByte)
      sevenDotOhFloat should be (7.2f +- 2.toByte)
      sevenDotOhFloat should be (6.8f +- 2.toByte)
      minusSevenDotOhFloat should be (-7.1f +- 2.toByte)
      minusSevenDotOhFloat should be (-6.9f +- 2.toByte)
      minusSevenDotOhFloat should be (-7.0f +- 2.toByte)
      minusSevenDotOhFloat should be (-7.2f +- 2.toByte)
      minusSevenDotOhFloat should be (-6.8f +- 2.toByte)

      // Long +- Long
      sevenLong should be (9L +- 2L)
      sevenLong should be (8L +- 2L)
      sevenLong should be (7L +- 2L)
      sevenLong should be (6L +- 2L)
      sevenLong should be (5L +- 2L)
      minusSevenLong should be (-9L +- 2L)
      minusSevenLong should be (-8L +- 2L)
      minusSevenLong should be (-7L +- 2L)
      minusSevenLong should be (-6L +- 2L)
      minusSevenLong should be (-5L +- 2L)

      // Long +- Int
      sevenLong should be (9L +- 2)
      sevenLong should be (8L +- 2)
      sevenLong should be (7L +- 2)
      sevenLong should be (6L +- 2)
      sevenLong should be (5L +- 2)
      minusSevenLong should be (-9L +- 2)
      minusSevenLong should be (-8L +- 2)
      minusSevenLong should be (-7L +- 2)
      minusSevenLong should be (-6L +- 2)
      minusSevenLong should be (-5L +- 2)

      // Long +- Short
      sevenLong should be (9L +- 2.toShort)
      sevenLong should be (8L +- 2.toShort)
      sevenLong should be (7L +- 2.toShort)
      sevenLong should be (6L +- 2.toShort)
      sevenLong should be (5L +- 2.toShort)
      minusSevenLong should be (-9L +- 2.toShort)
      minusSevenLong should be (-8L +- 2.toShort)
      minusSevenLong should be (-7L +- 2.toShort)
      minusSevenLong should be (-6L +- 2.toShort)
      minusSevenLong should be (-5L +- 2.toShort)

      // Long +- Byte
      sevenLong should be (9L +- 2.toByte)
      sevenLong should be (8L +- 2.toByte)
      sevenLong should be (7L +- 2.toByte)
      sevenLong should be (6L +- 2.toByte)
      sevenLong should be (5L +- 2.toByte)
      minusSevenLong should be (-9L +- 2.toByte)
      minusSevenLong should be (-8L +- 2.toByte)
      minusSevenLong should be (-7L +- 2.toByte)
      minusSevenLong should be (-6L +- 2.toByte)
      minusSevenLong should be (-5L +- 2.toByte)

      // Int +- Int
      sevenInt should be (9 +- 2)
      sevenInt should be (8 +- 2)
      sevenInt should be (7 +- 2)
      sevenInt should be (6 +- 2)
      sevenInt should be (5 +- 2)
      minusSevenInt should be (-9 +- 2)
      minusSevenInt should be (-8 +- 2)
      minusSevenInt should be (-7 +- 2)
      minusSevenInt should be (-6 +- 2)
      minusSevenInt should be (-5 +- 2)

      // Int +- Short
      sevenInt should be (9 +- 2.toShort)
      sevenInt should be (8 +- 2.toShort)
      sevenInt should be (7 +- 2.toShort)
      sevenInt should be (6 +- 2.toShort)
      sevenInt should be (5 +- 2.toShort)
      minusSevenInt should be (-9 +- 2.toShort)
      minusSevenInt should be (-8 +- 2.toShort)
      minusSevenInt should be (-7 +- 2.toShort)
      minusSevenInt should be (-6 +- 2.toShort)
      minusSevenInt should be (-5 +- 2.toShort)

      // Int +- Byte
      sevenInt should be (9 +- 2.toByte)
      sevenInt should be (8 +- 2.toByte)
      sevenInt should be (7 +- 2.toByte)
      sevenInt should be (6 +- 2.toByte)
      sevenInt should be (5 +- 2.toByte)
      minusSevenInt should be (-9 +- 2.toByte)
      minusSevenInt should be (-8 +- 2.toByte)
      minusSevenInt should be (-7 +- 2.toByte)
      minusSevenInt should be (-6 +- 2.toByte)
      minusSevenInt should be (-5 +- 2.toByte)

      // Short +- Short
      sevenShort should be (9.toShort +- 2.toShort)
      sevenShort should be (8.toShort +- 2.toShort)
      sevenShort should be (7.toShort +- 2.toShort)
      sevenShort should be (6.toShort +- 2.toShort)
      sevenShort should be (5.toShort +- 2.toShort)
      minusSevenShort should be ((-9).toShort +- 2.toShort)
      minusSevenShort should be ((-8).toShort +- 2.toShort)
      minusSevenShort should be ((-7).toShort +- 2.toShort)
      minusSevenShort should be ((-6).toShort +- 2.toShort)
      minusSevenShort should be ((-5).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should be (9.toShort +- 2.toByte)
      sevenShort should be (8.toShort +- 2.toByte)
      sevenShort should be (7.toShort +- 2.toByte)
      sevenShort should be (6.toShort +- 2.toByte)
      sevenShort should be (5.toShort +- 2.toByte)
      minusSevenShort should be ((-9).toShort +- 2.toByte)
      minusSevenShort should be ((-8).toShort +- 2.toByte)
      minusSevenShort should be ((-7).toShort +- 2.toByte)
      minusSevenShort should be ((-6).toShort +- 2.toByte)
      minusSevenShort should be ((-5).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should be (9.toByte +- 2.toByte)
      sevenByte should be (8.toByte +- 2.toByte)
      sevenByte should be (7.toByte +- 2.toByte)
      sevenByte should be (6.toByte +- 2.toByte)
      sevenByte should be (5.toByte +- 2.toByte)
      minusSevenByte should be ((-9).toByte +- 2.toByte)
      minusSevenByte should be ((-8).toByte +- 2.toByte)
      minusSevenByte should be ((-7).toByte +- 2.toByte)
      minusSevenByte should be ((-6).toByte +- 2.toByte)
      minusSevenByte should be ((-5).toByte +- 2.toByte)
    }

    def `should do nothing if the number is within the specified range, when used with not` {

      // Double +- Double
      sevenDotOh should not { be (7.5 +- 0.2) }
      sevenDotOh should not be (7.5 +- 0.2)
      sevenDotOh should not be (6.5 +- 0.2)
      minusSevenDotOh should not { be (-7.5 +- 0.2) }
      minusSevenDotOh should not be (-7.5 +- 0.2)
      minusSevenDotOh should not be (-6.5 +- 0.2)

      // Double +- Float
      sevenDotOh should not { be (7.5 +- 0.2f) }
      sevenDotOh should not be (7.5 +- 0.2f)
      sevenDotOh should not be (6.5 +- 0.2f)
      minusSevenDotOh should not { be (-7.5 +- 0.2f) }
      minusSevenDotOh should not be (-7.5 +- 0.2f)
      minusSevenDotOh should not be (-6.5 +- 0.2f)

      // Double +- Long
      sevenDotOh should not { be (10.0 +- 2L) }
      sevenDotOh should not be (4.0 +- 2L)
      sevenDotOh should not be (9.1 +- 2L)
      minusSevenDotOh should not { be (-10.0 +- 2L) }
      minusSevenDotOh should not be (-4.0 +- 2L)
      minusSevenDotOh should not be (-9.1 +- 2L)

      // Double +- Int
      sevenDotOh should not { be (10.0 +- 2) }
      sevenDotOh should not be (4.0 +- 2)
      sevenDotOh should not be (9.1 +- 2)
      minusSevenDotOh should not { be (-10.0 +- 2) }
      minusSevenDotOh should not be (-4.0 +- 2)
      minusSevenDotOh should not be (-9.1 +- 2)

      // Double +- Short
      sevenDotOh should not { be (10.0 +- 2.toShort) }
      sevenDotOh should not be (4.0 +- 2.toShort)
      sevenDotOh should not be (9.1 +- 2.toShort)
      minusSevenDotOh should not { be (-10.0 +- 2.toShort) }
      minusSevenDotOh should not be (-4.0 +- 2.toShort)
      minusSevenDotOh should not be (-9.1 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should not { be (10.0 +- 2.toByte) }
      sevenDotOh should not be (4.0 +- 2.toByte)
      sevenDotOh should not be (9.1 +- 2.toByte)
      minusSevenDotOh should not { be (-10.0 +- 2.toByte) }
      minusSevenDotOh should not be (-4.0 +- 2.toByte)
      minusSevenDotOh should not be (-9.1 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should not { be (7.5f +- 0.2f) }
      sevenDotOhFloat should not be (7.5f +- 0.2f)
      sevenDotOhFloat should not be (6.5f +- 0.2f)
      minusSevenDotOhFloat should not { be (-7.5f +- 0.2f) }
      minusSevenDotOhFloat should not be (-7.5f +- 0.2f)
      minusSevenDotOhFloat should not be (-6.5f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should not { be (10.0f +- 2L) }
      sevenDotOhFloat should not be (4.0f +- 2L)
      sevenDotOhFloat should not be (9.1f +- 2L)
      minusSevenDotOhFloat should not { be (-10.0f +- 2L) }
      minusSevenDotOhFloat should not be (-4.0f +- 2L)
      minusSevenDotOhFloat should not be (-9.1f +- 2L)

      // Float +- Int
      sevenDotOhFloat should not { be (10.0f +- 2) }
      sevenDotOhFloat should not be (4.0f +- 2)
      sevenDotOhFloat should not be (9.1f +- 2)
      minusSevenDotOhFloat should not { be (-10.0f +- 2) }
      minusSevenDotOhFloat should not be (-4.0f +- 2)
      minusSevenDotOhFloat should not be (-9.1f +- 2)

      // Float +- Short
      sevenDotOhFloat should not { be (10.0f +- 2.toShort) }
      sevenDotOhFloat should not be (4.0f +- 2.toShort)
      sevenDotOhFloat should not be (9.1f +- 2.toShort)
      minusSevenDotOhFloat should not { be (-10.0f +- 2.toShort) }
      minusSevenDotOhFloat should not be (-4.0f +- 2.toShort)
      minusSevenDotOhFloat should not be (-9.1f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should not { be (10.0f +- 2.toByte) }
      sevenDotOhFloat should not be (4.0f +- 2.toByte)
      sevenDotOhFloat should not be (9.1f +- 2.toByte)
      minusSevenDotOhFloat should not { be (-10.0f +- 2.toByte) }
      minusSevenDotOhFloat should not be (-4.0f +- 2.toByte)
      minusSevenDotOhFloat should not be (-9.1f +- 2.toByte)

      // Long +- Long
      sevenLong should not { be (10L +- 2L) }
      sevenLong should not be (4L +- 2L)
      sevenLong should not be (10L +- 2L)
      minusSevenLong should not { be (-10L +- 2L) }
      minusSevenLong should not be (-4L +- 2L)
      minusSevenLong should not be (-10L +- 2L)

      // Long +- Int
      sevenLong should not { be (10L +- 2) }
      sevenLong should not be (4L +- 2)
      sevenLong should not be (10L +- 2)
      minusSevenLong should not { be (-10L +- 2) }
      minusSevenLong should not be (-4L +- 2)
      minusSevenLong should not be (-10L +- 2)

      // Long +- Short
      sevenLong should not { be (10L +- 2.toShort) }
      sevenLong should not be (4L +- 2.toShort)
      sevenLong should not be (10L +- 2.toShort)
      minusSevenLong should not { be (-10L +- 2.toShort) }
      minusSevenLong should not be (-4L +- 2.toShort)
      minusSevenLong should not be (-10L +- 2.toShort)

      // Long +- Byte
      sevenLong should not { be (10L +- 2.toByte) }
      sevenLong should not be (4L +- 2.toByte)
      sevenLong should not be (10L +- 2.toByte)
      minusSevenLong should not { be (-10L +- 2.toByte) }
      minusSevenLong should not be (-4L +- 2.toByte)
      minusSevenLong should not be (-10L +- 2.toByte)

      // Int +- Int
      sevenInt should not { be (10 +- 2) }
      sevenInt should not be (4 +- 2)
      sevenInt should not be (10 +- 2)
      minusSevenInt should not { be (-10 +- 2) }
      minusSevenInt should not be (-4 +- 2)
      minusSevenInt should not be (-10 +- 2)

      // Int +- Short
      sevenInt should not { be (10 +- 2.toShort) }
      sevenInt should not be (4 +- 2.toShort)
      sevenInt should not be (10 +- 2.toShort)
      minusSevenInt should not { be (-10 +- 2.toShort) }
      minusSevenInt should not be (-4 +- 2.toShort)
      minusSevenInt should not be (-10 +- 2.toShort)

      // Int +- Byte
      sevenInt should not { be (10 +- 2.toByte) }
      sevenInt should not be (4 +- 2.toByte)
      sevenInt should not be (10 +- 2.toByte)
      minusSevenInt should not { be (-10 +- 2.toByte) }
      minusSevenInt should not be (-4 +- 2.toByte)
      minusSevenInt should not be (-10 +- 2.toByte)

      // Short +- Short
      sevenShort should not { be (10.toShort +- 2.toShort) }
      sevenShort should not be (4.toShort +- 2.toShort)
      sevenShort should not be (10.toShort +- 2.toShort)
      minusSevenShort should not { be ((-10).toShort +- 2.toShort) }
      minusSevenShort should not be ((-4).toShort +- 2.toShort)
      minusSevenShort should not be ((-10).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should not { be (10.toShort +- 2.toByte) }
      sevenShort should not be (4.toShort +- 2.toByte)
      sevenShort should not be (10.toShort +- 2.toByte)
      minusSevenShort should not { be ((-10).toShort +- 2.toByte) }
      minusSevenShort should not be ((-4).toShort +- 2.toByte)
      minusSevenShort should not be ((-10).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should not { be (10.toByte +- 2.toByte) }
      sevenByte should not be (4.toByte +- 2.toByte)
      sevenByte should not be (10.toByte +- 2.toByte)
      minusSevenByte should not { be ((-10).toByte +- 2.toByte) }
      minusSevenByte should not be ((-4).toByte +- 2.toByte)
      minusSevenByte should not be ((-10).toByte +- 2.toByte)
    }

    def `should do nothing if the number is within the specified range, when used in a logical-and expression` {

      // Double +- Double
      sevenDotOh should ((be (7.1 +- 0.2)) and (be (7.1 +- 0.2)))
      sevenDotOh should (be (6.9 +- 0.2) and (be (7.1 +- 0.2)))
      sevenDotOh should (be (7.0 +- 0.2) and be (7.0 +- 0.2))

      // Double +- Float
      sevenDotOh should ((be (7.1 +- 0.2f)) and (be (7.1 +- 0.2f)))
      sevenDotOh should (be (6.9 +- 0.2f) and (be (7.1 +- 0.2f)))
      sevenDotOh should (be (7.0 +- 0.2f) and be (7.0 +- 0.2f))

      // Double +- Long
      sevenDotOh should ((be (7.1 +- 2L)) and (be (7.1 +- 2L)))
      sevenDotOh should (be (6.9 +- 2L) and (be (7.1 +- 2L)))
      sevenDotOh should (be (7.0 +- 2L) and be (7.0 +- 2L))

      // Double +- Int
      sevenDotOh should ((be (7.1 +- 2)) and (be (7.1 +- 2)))
      sevenDotOh should (be (6.9 +- 2) and (be (7.1 +- 2)))
      sevenDotOh should (be (7.0 +- 2) and be (7.0 +- 2))

      // Double +- Short
      sevenDotOh should ((be (7.1 +- 2.toShort)) and (be (7.1 +- 2.toShort)))
      sevenDotOh should (be (6.9 +- 2.toShort) and (be (7.1 +- 2.toShort)))
      sevenDotOh should (be (7.0 +- 2.toShort) and be (7.0 +- 2.toShort))

      // Double +- Byte
      sevenDotOh should ((be (7.1 +- 2.toByte)) and (be (7.1 +- 2.toByte)))
      sevenDotOh should (be (6.9 +- 2.toByte) and (be (7.1 +- 2.toByte)))
      sevenDotOh should (be (7.0 +- 2.toByte) and be (7.0 +- 2.toByte))

      // Float +- Float
      sevenDotOhFloat should ((be (7.1f +- 0.2f)) and (be (7.1f +- 0.2f)))
      sevenDotOhFloat should (be (6.9f +- 0.2f) and (be (7.1f +- 0.2f)))
      sevenDotOhFloat should (be (7.0f +- 0.2f) and be (7.0f +- 0.2f))

      // Float +- Long
      sevenDotOhFloat should ((be (7.1f +- 2L)) and (be (7.1f +- 2L)))
      sevenDotOhFloat should (be (6.9f +- 2L) and (be (7.1f +- 2L)))
      sevenDotOhFloat should (be (7.0f +- 2L) and be (7.0f +- 2L))

      // Float +- Int
      sevenDotOhFloat should ((be (7.1f +- 2)) and (be (7.1f +- 2)))
      sevenDotOhFloat should (be (6.9f +- 2) and (be (7.1f +- 2)))
      sevenDotOhFloat should (be (7.0f +- 2) and be (7.0f +- 2))

      // Float +- Short
      sevenDotOhFloat should ((be (7.1f +- 2.toShort)) and (be (7.1f +- 2.toShort)))
      sevenDotOhFloat should (be (6.9f +- 2.toShort) and (be (7.1f +- 2.toShort)))
      sevenDotOhFloat should (be (7.0f +- 2.toShort) and be (7.0f +- 2.toShort))

      // Float +- Byte
      sevenDotOhFloat should ((be (7.1f +- 2.toByte)) and (be (7.1f +- 2.toByte)))
      sevenDotOhFloat should (be (6.9f +- 2.toByte) and (be (7.1f +- 2.toByte)))
      sevenDotOhFloat should (be (7.0f +- 2.toByte) and be (7.0f +- 2.toByte))

      // Long +- Long
      sevenLong should ((be (9L +- 2L)) and (be (9L +- 2L)))
      sevenLong should (be (8L +- 2L) and (be (9L +- 2L)))
      sevenLong should (be (7L +- 2L) and be (7L +- 2L))

      // Long +- Int
      sevenLong should ((be (9L +- 2)) and (be (9L +- 2)))
      sevenLong should (be (8L +- 2) and (be (9L +- 2)))
      sevenLong should (be (7L +- 2) and be (7L +- 2))

      // Long +- Short
      sevenLong should ((be (9L +- 2.toShort)) and (be (9L +- 2.toShort)))
      sevenLong should (be (8L +- 2.toShort) and (be (9L +- 2.toShort)))
      sevenLong should (be (7L +- 2.toShort) and be (7L +- 2.toShort))

      // Long +- Byte
      sevenLong should ((be (9L +- 2.toByte)) and (be (9L +- 2.toByte)))
      sevenLong should (be (8L +- 2.toByte) and (be (9L +- 2.toByte)))
      sevenLong should (be (7L +- 2.toByte) and be (7L +- 2.toByte))

      // Int +- Int
      sevenInt should ((be (9 +- 2)) and (be (9 +- 2)))
      sevenInt should (be (8 +- 2) and (be (9 +- 2)))
      sevenInt should (be (7 +- 2) and be (7 +- 2))

      // Int +- Short
      sevenInt should ((be (9 +- 2.toShort)) and (be (9 +- 2.toShort)))
      sevenInt should (be (8 +- 2.toShort) and (be (9 +- 2.toShort)))
      sevenInt should (be (7 +- 2.toShort) and be (7 +- 2.toShort))

      // Int +- Byte
      sevenInt should ((be (9 +- 2.toByte)) and (be (9 +- 2.toByte)))
      sevenInt should (be (8 +- 2.toByte) and (be (9 +- 2.toByte)))
      sevenInt should (be (7 +- 2.toByte) and be (7 +- 2.toByte))

      // Short +- Short
      sevenShort should ((be (9.toShort +- 2.toShort)) and (be (9.toShort +- 2.toShort)))
      sevenShort should (be (8.toShort +- 2.toShort) and (be (9.toShort +- 2.toShort)))
      sevenShort should (be (7.toShort +- 2.toShort) and be (7.toShort +- 2.toShort))

      // Short +- Byte
      sevenShort should ((be (9.toShort +- 2.toByte)) and (be (9.toShort +- 2.toByte)))
      sevenShort should (be (8.toShort +- 2.toByte) and (be (9.toShort +- 2.toByte)))
      sevenShort should (be (7.toShort +- 2.toByte) and be (7.toShort +- 2.toByte))

      // Byte +- Byte
      sevenByte should ((be (9.toByte +- 2.toByte)) and (be (9.toByte +- 2.toByte)))
      sevenByte should (be (8.toByte +- 2.toByte) and (be (9.toByte +- 2.toByte)))
      sevenByte should (be (7.toByte +- 2.toByte) and be (7.toByte +- 2.toByte))
    }

    def `should do nothing if the number is within the specified range, when used in a logical-or expression` {

      // Double +- Double
      sevenDotOh should ((be (7.1 +- 0.2)) or (be (7.1 +- 0.2)))
      sevenDotOh should (be (6.9 +- 0.2) or (be (7.1 +- 0.2)))
      sevenDotOh should (be (7.0 +- 0.2) or be (7.0 +- 0.2))

      // Double +- Float
      sevenDotOh should ((be (7.1 +- 0.2f)) or (be (7.1 +- 0.2f)))
      sevenDotOh should (be (6.9 +- 0.2f) or (be (7.1 +- 0.2f)))
      sevenDotOh should (be (7.0 +- 0.2f) or be (7.0 +- 0.2f))

      // Double +- Long
      sevenDotOh should ((be (7.1 +- 2L)) or (be (7.1 +- 2L)))
      sevenDotOh should (be (6.9 +- 2L) or (be (7.1 +- 2L)))
      sevenDotOh should (be (7.0 +- 2L) or be (7.0 +- 2L))

      // Double +- Int
      sevenDotOh should ((be (7.1 +- 2)) or (be (7.1 +- 2)))
      sevenDotOh should (be (6.9 +- 2) or (be (7.1 +- 2)))
      sevenDotOh should (be (7.0 +- 2) or be (7.0 +- 2))

      // Double +- Short
      sevenDotOh should ((be (7.1 +- 2.toShort)) or (be (7.1 +- 2.toShort)))
      sevenDotOh should (be (6.9 +- 2.toShort) or (be (7.1 +- 2.toShort)))
      sevenDotOh should (be (7.0 +- 2.toShort) or be (7.0 +- 2.toShort))

      // Double +- Byte
      sevenDotOh should ((be (7.1 +- 2.toByte)) or (be (7.1 +- 2.toByte)))
      sevenDotOh should (be (6.9 +- 2.toByte) or (be (7.1 +- 2.toByte)))
      sevenDotOh should (be (7.0 +- 2.toByte) or be (7.0 +- 2.toByte))

      // Float +- Float
      sevenDotOhFloat should ((be (7.1f +- 0.2f)) or (be (7.1f +- 0.2f)))
      sevenDotOhFloat should (be (6.9f +- 0.2f) or (be (7.1f +- 0.2f)))
      sevenDotOhFloat should (be (7.0f +- 0.2f) or be (7.0f +- 0.2f))

      // Float +- Long
      sevenDotOhFloat should ((be (7.1f +- 2L)) or (be (7.1f +- 2L)))
      sevenDotOhFloat should (be (6.9f +- 2L) or (be (7.1f +- 2L)))
      sevenDotOhFloat should (be (7.0f +- 2L) or be (7.0f +- 2L))

      // Float +- Int
      sevenDotOhFloat should ((be (7.1f +- 2)) or (be (7.1f +- 2)))
      sevenDotOhFloat should (be (6.9f +- 2) or (be (7.1f +- 2)))
      sevenDotOhFloat should (be (7.0f +- 2) or be (7.0f +- 2))

      // Float +- Short
      sevenDotOhFloat should ((be (7.1f +- 2.toShort)) or (be (7.1f +- 2.toShort)))
      sevenDotOhFloat should (be (6.9f +- 2.toShort) or (be (7.1f +- 2.toShort)))
      sevenDotOhFloat should (be (7.0f +- 2.toShort) or be (7.0f +- 2.toShort))

      // Float +- Byte
      sevenDotOhFloat should ((be (7.1f +- 2.toByte)) or (be (7.1f +- 2.toByte)))
      sevenDotOhFloat should (be (6.9f +- 2.toByte) or (be (7.1f +- 2.toByte)))
      sevenDotOhFloat should (be (7.0f +- 2.toByte) or be (7.0f +- 2.toByte))

      // Long +- Long
      sevenLong should ((be (9L +- 2L)) or (be (9L +- 2L)))
      sevenLong should (be (8L +- 2L) or (be (9L +- 2L)))
      sevenLong should (be (7L +- 2L) or be (7L +- 2L))

      // Long +- Int
      sevenLong should ((be (9L +- 2)) or (be (9L +- 2)))
      sevenLong should (be (8L +- 2) or (be (9L +- 2)))
      sevenLong should (be (7L +- 2) or be (7L +- 2))

      // Long +- Short
      sevenLong should ((be (9L +- 2.toShort)) or (be (9L +- 2.toShort)))
      sevenLong should (be (8L +- 2.toShort) or (be (9L +- 2.toShort)))
      sevenLong should (be (7L +- 2.toShort) or be (7L +- 2.toShort))

      // Long +- Byte
      sevenLong should ((be (9L +- 2.toByte)) or (be (9L +- 2.toByte)))
      sevenLong should (be (8L +- 2.toByte) or (be (9L +- 2.toByte)))
      sevenLong should (be (7L +- 2.toByte) or be (7L +- 2.toByte))

      // Int +- Int
      sevenInt should ((be (9 +- 2)) or (be (9 +- 2)))
      sevenInt should (be (8 +- 2) or (be (9 +- 2)))
      sevenInt should (be (7 +- 2) or be (7 +- 2))

      // Int +- Short
      sevenInt should ((be (9 +- 2.toShort)) or (be (9 +- 2.toShort)))
      sevenInt should (be (8 +- 2.toShort) or (be (9 +- 2.toShort)))
      sevenInt should (be (7 +- 2.toShort) or be (7 +- 2.toShort))

      // Int +- Byte
      sevenInt should ((be (9 +- 2.toByte)) or (be (9 +- 2.toByte)))
      sevenInt should (be (8 +- 2.toByte) or (be (9 +- 2.toByte)))
      sevenInt should (be (7 +- 2.toByte) or be (7 +- 2.toByte))

      // Short +- Short
      sevenShort should ((be (9.toShort +- 2.toShort)) or (be (9.toShort +- 2.toShort)))
      sevenShort should (be (8.toShort +- 2.toShort) or (be (9.toShort +- 2.toShort)))
      sevenShort should (be (7.toShort +- 2.toShort) or be (7.toShort +- 2.toShort))

      // Short +- Byte
      sevenShort should ((be (9.toShort +- 2.toByte)) or (be (9.toShort +- 2.toByte)))
      sevenShort should (be (8.toShort +- 2.toByte) or (be (9.toShort +- 2.toByte)))
      sevenShort should (be (7.toShort +- 2.toByte) or be (7.toShort +- 2.toByte))

      // Byte +- Byte
      sevenByte should ((be (9.toByte +- 2.toByte)) or (be (9.toByte +- 2.toByte)))
      sevenByte should (be (8.toByte +- 2.toByte) or (be (9.toByte +- 2.toByte)))
      sevenByte should (be (7.toByte +- 2.toByte) or be (7.toByte +- 2.toByte))
    }

    def `should do nothing if the number is not within the specified range, when used in a logical-and expression with not` {

      // Double +- Double
      sevenDotOh should ((not be (17.1 +- 0.2)) and (not be (17.1 +- 0.2)))
      sevenDotOh should (not (be (16.9 +- 0.2)) and not (be (17.1 +- 0.2)))
      sevenDotOh should (not be (17.0 +- 0.2) and not be (17.0 +- 0.2))

      // Double +- Float
      sevenDotOh should ((not be (17.1 +- 0.2f)) and (not be (17.1 +- 0.2f)))
      sevenDotOh should (not (be (16.9 +- 0.2f)) and not (be (17.1 +- 0.2f)))
      sevenDotOh should (not be (17.0 +- 0.2f) and not be (17.0 +- 0.2f))

      // Double +- Long
      sevenDotOh should ((not be (17.1 +- 2L)) and (not be (17.1 +- 2L)))
      sevenDotOh should (not (be (16.9 +- 2L)) and not (be (17.1 +- 2L)))
      sevenDotOh should (not be (17.0 +- 2L) and not be (17.0 +- 2L))

      // Double +- Int
      sevenDotOh should ((not be (17.1 +- 2)) and (not be (17.1 +- 2)))
      sevenDotOh should (not (be (16.9 +- 2)) and not (be (17.1 +- 2)))
      sevenDotOh should (not be (17.0 +- 2) and not be (17.0 +- 2))

      // Double +- Short
      sevenDotOh should ((not be (17.1 +- 2.toShort)) and (not be (17.1 +- 2.toShort)))
      sevenDotOh should (not (be (16.9 +- 2.toShort)) and not (be (17.1 +- 2.toShort)))
      sevenDotOh should (not be (17.0 +- 2.toShort) and not be (17.0 +- 2.toShort))

      // Double +- Byte
      sevenDotOh should ((not be (17.1 +- 2.toByte)) and (not be (17.1 +- 2.toByte)))
      sevenDotOh should (not (be (16.9 +- 2.toByte)) and not (be (17.1 +- 2.toByte)))
      sevenDotOh should (not be (17.0 +- 2.toByte) and not be (17.0 +- 2.toByte))

      // Float +- Float
      sevenDotOhFloat should ((not be (17.1f +- 0.2f)) and (not be (17.1f +- 0.2f)))
      sevenDotOhFloat should (not (be (16.9f +- 0.2f)) and not (be (17.1f +- 0.2f)))
      sevenDotOhFloat should (not be (17.0f +- 0.2f) and not be (17.0f +- 0.2f))

      // Float +- Long
      sevenDotOhFloat should ((not be (17.1f +- 2L)) and (not be (17.1f +- 2L)))
      sevenDotOhFloat should (not (be (16.9f +- 2L)) and not (be (17.1f +- 2L)))
      sevenDotOhFloat should (not be (17.0f +- 2L) and not be (17.0f +- 2L))

      // Float +- Int
      sevenDotOhFloat should ((not be (17.1f +- 2)) and (not be (17.1f +- 2)))
      sevenDotOhFloat should (not (be (16.9f +- 2)) and not (be (17.1f +- 2)))
      sevenDotOhFloat should (not be (17.0f +- 2) and not be (17.0f +- 2))

      // Float +- Short
      sevenDotOhFloat should ((not be (17.1f +- 2.toShort)) and (not be (17.1f +- 2.toShort)))
      sevenDotOhFloat should (not (be (16.9f +- 2.toShort)) and not (be (17.1f +- 2.toShort)))
      sevenDotOhFloat should (not be (17.0f +- 2.toShort) and not be (17.0f +- 2.toShort))

      // Float +- Byte
      sevenDotOhFloat should ((not be (17.1f +- 2.toByte)) and (not be (17.1f +- 2.toByte)))
      sevenDotOhFloat should (not (be (16.9f +- 2.toByte)) and not (be (17.1f +- 2.toByte)))
      sevenDotOhFloat should (not be (17.0f +- 2.toByte) and not be (17.0f +- 2.toByte))

      // Long +- Long
      sevenLong should ((not be (19L +- 2L)) and (not be (19L +- 2L)))
      sevenLong should (not (be (18L +- 2L)) and not (be (19L +- 2L)))
      sevenLong should (not be (17L +- 2L) and not be (17L +- 2L))

      // Long +- Int
      sevenLong should ((not be (19L +- 2)) and (not be (19L +- 2)))
      sevenLong should (not (be (18L +- 2)) and not (be (19L +- 2)))
      sevenLong should (not be (17L +- 2) and not be (17L +- 2))

      // Long +- Short
      sevenLong should ((not be (19L +- 2.toShort)) and (not be (19L +- 2.toShort)))
      sevenLong should (not (be (18L +- 2.toShort)) and not (be (19L +- 2.toShort)))
      sevenLong should (not be (17L +- 2.toShort) and not be (17L +- 2.toShort))

      // Long +- Byte
      sevenLong should ((not be (19L +- 2.toByte)) and (not be (19L +- 2.toByte)))
      sevenLong should (not (be (18L +- 2.toByte)) and not (be (19L +- 2.toByte)))
      sevenLong should (not be (17L +- 2.toByte) and not be (17L +- 2.toByte))

      // Int +- Int
      sevenInt should ((not be (19 +- 2)) and (not be (19 +- 2)))
      sevenInt should (not (be (18 +- 2)) and not (be (19 +- 2)))
      sevenInt should (not be (17 +- 2) and not be (17 +- 2))

      // Int +- Short
      sevenInt should ((not be (19 +- 2.toShort)) and (not be (19 +- 2.toShort)))
      sevenInt should (not (be (18 +- 2.toShort)) and not (be (19 +- 2.toShort)))
      sevenInt should (not be (17 +- 2.toShort) and not be (17 +- 2.toShort))

      // Int +- Byte
      sevenInt should ((not be (19 +- 2.toByte)) and (not be (19 +- 2.toByte)))
      sevenInt should (not (be (18 +- 2.toByte)) and not (be (19 +- 2.toByte)))
      sevenInt should (not be (17 +- 2.toByte) and not be (17 +- 2.toByte))

      // Short +- Short
      sevenShort should ((not be (19.toShort +- 2.toShort)) and (not be (19.toShort +- 2.toShort)))
      sevenShort should (not (be (18.toShort +- 2.toShort)) and not (be (19.toShort +- 2.toShort)))
      sevenShort should (not be (17.toShort +- 2.toShort) and not be (17.toShort +- 2.toShort))

      // Short +- Byte
      sevenShort should ((not be (19.toShort +- 2.toByte)) and (not be (19.toShort +- 2.toByte)))
      sevenShort should (not (be (18.toShort +- 2.toByte)) and not (be (19.toShort +- 2.toByte)))
      sevenShort should (not be (17.toShort +- 2.toByte) and not be (17.toShort +- 2.toByte))

      // Byte +- Byte
      sevenByte should ((not be (19.toByte +- 2.toByte)) and (not be (19.toByte +- 2.toByte)))
      sevenByte should (not (be (18.toByte +- 2.toByte)) and not (be (19.toByte +- 2.toByte)))
      sevenByte should (not be (17.toByte +- 2.toByte) and not be (17.toByte +- 2.toByte))
    }

    def `should do nothing if the number is not within the specified range, when used in a logical-or expression with not` {

      // Double +- Double
      sevenDotOh should ((not be (17.1 +- 0.2)) or (not be (17.1 +- 0.2)))
      sevenDotOh should (not (be (16.9 +- 0.2)) or not (be (17.1 +- 0.2)))
      sevenDotOh should (not be (17.0 +- 0.2) or not be (17.0 +- 0.2))

      // Double +- Float
      sevenDotOh should ((not be (17.1 +- 0.2f)) or (not be (17.1 +- 0.2f)))
      sevenDotOh should (not (be (16.9 +- 0.2f)) or not (be (17.1 +- 0.2f)))
      sevenDotOh should (not be (17.0 +- 0.2f) or not be (17.0 +- 0.2f))

      // Double +- Long
      sevenDotOh should ((not be (17.1 +- 2L)) or (not be (17.1 +- 2L)))
      sevenDotOh should (not (be (16.9 +- 2L)) or not (be (17.1 +- 2L)))
      sevenDotOh should (not be (17.0 +- 2L) or not be (17.0 +- 2L))

      // Double +- Int
      sevenDotOh should ((not be (17.1 +- 2)) or (not be (17.1 +- 2)))
      sevenDotOh should (not (be (16.9 +- 2)) or not (be (17.1 +- 2)))
      sevenDotOh should (not be (17.0 +- 2) or not be (17.0 +- 2))

      // Double +- Short
      sevenDotOh should ((not be (17.1 +- 2.toShort)) or (not be (17.1 +- 2.toShort)))
      sevenDotOh should (not (be (16.9 +- 2.toShort)) or not (be (17.1 +- 2.toShort)))
      sevenDotOh should (not be (17.0 +- 2.toShort) or not be (17.0 +- 2.toShort))

      // Double +- Byte
      sevenDotOh should ((not be (17.1 +- 2.toByte)) or (not be (17.1 +- 2.toByte)))
      sevenDotOh should (not (be (16.9 +- 2.toByte)) or not (be (17.1 +- 2.toByte)))
      sevenDotOh should (not be (17.0 +- 2.toByte) or not be (17.0 +- 2.toByte))

      // Float +- Float
      sevenDotOhFloat should ((not be (17.1f +- 0.2f)) or (not be (17.1f +- 0.2f)))
      sevenDotOhFloat should (not (be (16.9f +- 0.2f)) or not (be (17.1f +- 0.2f)))
      sevenDotOhFloat should (not be (17.0f +- 0.2f) or not be (17.0f +- 0.2f))

      // Float +- Long
      sevenDotOhFloat should ((not be (17.1f +- 2L)) or (not be (17.1f +- 2L)))
      sevenDotOhFloat should (not (be (16.9f +- 2L)) or not (be (17.1f +- 2L)))
      sevenDotOhFloat should (not be (17.0f +- 2L) or not be (17.0f +- 2L))

      // Float +- Int
      sevenDotOhFloat should ((not be (17.1f +- 2)) or (not be (17.1f +- 2)))
      sevenDotOhFloat should (not (be (16.9f +- 2)) or not (be (17.1f +- 2)))
      sevenDotOhFloat should (not be (17.0f +- 2) or not be (17.0f +- 2))

      // Float +- Short
      sevenDotOhFloat should ((not be (17.1f +- 2.toShort)) or (not be (17.1f +- 2.toShort)))
      sevenDotOhFloat should (not (be (16.9f +- 2.toShort)) or not (be (17.1f +- 2.toShort)))
      sevenDotOhFloat should (not be (17.0f +- 2.toShort) or not be (17.0f +- 2.toShort))

      // Float +- Byte
      sevenDotOhFloat should ((not be (17.1f +- 2.toByte)) or (not be (17.1f +- 2.toByte)))
      sevenDotOhFloat should (not (be (16.9f +- 2.toByte)) or not (be (17.1f +- 2.toByte)))
      sevenDotOhFloat should (not be (17.0f +- 2.toByte) or not be (17.0f +- 2.toByte))

      // Long +- Long
      sevenLong should ((not be (19L +- 2L)) or (not be (19L +- 2L)))
      sevenLong should (not (be (18L +- 2L)) or not (be (19L +- 2L)))
      sevenLong should (not be (17L +- 2L) or not be (17L +- 2L))

      // Long +- Int
      sevenLong should ((not be (19L +- 2)) or (not be (19L +- 2)))
      sevenLong should (not (be (18L +- 2)) or not (be (19L +- 2)))
      sevenLong should (not be (17L +- 2) or not be (17L +- 2))

      // Long +- Short
      sevenLong should ((not be (19L +- 2.toShort)) or (not be (19L +- 2.toShort)))
      sevenLong should (not (be (18L +- 2.toShort)) or not (be (19L +- 2.toShort)))
      sevenLong should (not be (17L +- 2.toShort) or not be (17L +- 2.toShort))

      // Long +- Byte
      sevenLong should ((not be (19L +- 2.toByte)) or (not be (19L +- 2.toByte)))
      sevenLong should (not (be (18L +- 2.toByte)) or not (be (19L +- 2.toByte)))
      sevenLong should (not be (17L +- 2.toByte) or not be (17L +- 2.toByte))

      // Int +- Int
      sevenInt should ((not be (19 +- 2)) or (not be (19 +- 2)))
      sevenInt should (not (be (18 +- 2)) or not (be (19 +- 2)))
      sevenInt should (not be (17 +- 2) or not be (17 +- 2))

      // Int +- Short
      sevenInt should ((not be (19 +- 2.toShort)) or (not be (19 +- 2.toShort)))
      sevenInt should (not (be (18 +- 2.toShort)) or not (be (19 +- 2.toShort)))
      sevenInt should (not be (17 +- 2.toShort) or not be (17 +- 2.toShort))

      // Int +- Byte
      sevenInt should ((not be (19 +- 2.toByte)) or (not be (19 +- 2.toByte)))
      sevenInt should (not (be (18 +- 2.toByte)) or not (be (19 +- 2.toByte)))
      sevenInt should (not be (17 +- 2.toByte) or not be (17 +- 2.toByte))

      // Short +- Short
      sevenShort should ((not be (19.toShort +- 2.toShort)) or (not be (19.toShort +- 2.toShort)))
      sevenShort should (not (be (18.toShort +- 2.toShort)) or not (be (19.toShort +- 2.toShort)))
      sevenShort should (not be (17.toShort +- 2.toShort) or not be (17.toShort +- 2.toShort))

      // Short +- Byte
      sevenShort should ((not be (19.toShort +- 2.toByte)) or (not be (19.toShort +- 2.toByte)))
      sevenShort should (not (be (18.toShort +- 2.toByte)) or not (be (19.toShort +- 2.toByte)))
      sevenShort should (not be (17.toShort +- 2.toByte) or not be (17.toShort +- 2.toByte))

      // Byte +- Byte
      sevenByte should ((not be (19.toByte +- 2.toByte)) or (not be (19.toByte +- 2.toByte)))
      sevenByte should (not (be (18.toByte +- 2.toByte)) or not (be (19.toByte +- 2.toByte)))
      sevenByte should (not be (17.toByte +- 2.toByte) or not be (17.toByte +- 2.toByte))
    }

    def `should throw TestFailedException if the number is not within the specified range` {

      // Double +- Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh should be (17.1 +- 0.2)
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2")

      // Double +- Float
      val caught2 = intercept[TestFailedException] {
        sevenDotOh should be (17.1 +- 0.2f)
      }
      assert(caught2.getMessage === "7.0 was not 17.1 plus or minus 0.20000000298023224")

      // Double +- Long
      val caught3 = intercept[TestFailedException] {
        sevenDotOh should be (17.1 +- 2L)
      }
      assert(caught3.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Double +- Int
      val caught4 = intercept[TestFailedException] {
        sevenDotOh should be (17.1 +- 2)
      }
      assert(caught4.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Double +- Short
      val caught5 = intercept[TestFailedException] {
        sevenDotOh should be (17.1 +- 2.toShort)
      }
      assert(caught5.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Double +- Byte
      val caught6 = intercept[TestFailedException] {
        sevenDotOh should be (17.1 +- 2.toByte)
      }
      assert(caught6.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float +- Float
      val caught7 = intercept[TestFailedException] {
        sevenDotOhFloat should be (17.1f +- 0.2f)
      }
      assert(caught7.getMessage === "7.0 was not 17.1 plus or minus 0.2")

      // Float +- Long
      val caught8 = intercept[TestFailedException] {
        sevenDotOhFloat should be (17.1f +- 2L)
      }
      assert(caught8.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float +- Int
      val caught9 = intercept[TestFailedException] {
        sevenDotOhFloat should be (17.1f +- 2)
      }
      assert(caught9.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float +- Short
      val caught10 = intercept[TestFailedException] {
        sevenDotOhFloat should be (17.1f +- 2.toShort)
      }
      assert(caught10.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float +- Byte
      val caught11 = intercept[TestFailedException] {
        sevenDotOhFloat should be (17.1f +- 2.toByte)
      }
      assert(caught11.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Long +- Long
      val caught12 = intercept[TestFailedException] {
        sevenLong should be (19L +- 2L)
      }
      assert(caught12.getMessage === "7 was not 19 plus or minus 2")

      // Long +- Int
      val caught13 = intercept[TestFailedException] {
        sevenLong should be (19L +- 2)
      }
      assert(caught13.getMessage === "7 was not 19 plus or minus 2")

      // Long +- Short
      val caught14 = intercept[TestFailedException] {
        sevenLong should be (19L +- 2.toShort)
      }
      assert(caught14.getMessage === "7 was not 19 plus or minus 2")

      // Long +- Byte
      val caught15 = intercept[TestFailedException] {
        sevenLong should be (19L +- 2.toByte)
      }
      assert(caught15.getMessage === "7 was not 19 plus or minus 2")

      // Int +- Int
      val caught16 = intercept[TestFailedException] {
        sevenInt should be (19 +- 2)
      }
      assert(caught16.getMessage === "7 was not 19 plus or minus 2")

      // Int +- Short
      val caught17 = intercept[TestFailedException] {
        sevenInt should be (19 +- 2.toShort)
      }
      assert(caught17.getMessage === "7 was not 19 plus or minus 2")

      // Int +- Byte
      val caught18 = intercept[TestFailedException] {
        sevenInt should be (19 +- 2.toByte)
      }
      assert(caught18.getMessage === "7 was not 19 plus or minus 2")

      // Short +- Short
      val caught19 = intercept[TestFailedException] {
        sevenShort should be (19.toShort +- 2.toShort)
      }
      assert(caught19.getMessage === "7 was not 19 plus or minus 2")

      // Short +- Byte
      val caught20 = intercept[TestFailedException] {
        sevenShort should be (19.toShort +- 2.toByte)
      }
      assert(caught20.getMessage === "7 was not 19 plus or minus 2")

      // Byte +- Byte
      val caught21 = intercept[TestFailedException] {
        sevenByte should be (19.toByte +- 2.toByte)
      }
      assert(caught21.getMessage === "7 was not 19 plus or minus 2")
    }

    def `should throw TestFailedException if the number is within the specified range, when used with not` {

      // Double +- Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh should not be (7.1 +- 0.2)
      }
      assert(caught1.getMessage === "7.0 was 7.1 plus or minus 0.2")

      // Double +- Float
      val caught2 = intercept[TestFailedException] {
        sevenDotOh should not be (7.1 +- 0.2f)
      }
      assert(caught2.getMessage === "7.0 was 7.1 plus or minus 0.20000000298023224")

      // Double +- Long
      val caught3 = intercept[TestFailedException] {
        sevenDotOh should not be (7.1 +- 2L)
      }
      assert(caught3.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Double +- Int
      val caught4 = intercept[TestFailedException] {
        sevenDotOh should not be (7.1 +- 2)
      }
      assert(caught4.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Double +- Short
      val caught5 = intercept[TestFailedException] {
        sevenDotOh should not be (7.1 +- 2.toShort)
      }
      assert(caught5.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Double +- Byte
      val caught6 = intercept[TestFailedException] {
        sevenDotOh should not be (7.1 +- 2.toByte)
      }
      assert(caught6.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float +- Float
      val caught7 = intercept[TestFailedException] {
        sevenDotOhFloat should not be (7.1f +- 0.2f)
      }
      assert(caught7.getMessage === "7.0 was 7.1 plus or minus 0.2")

      // Float +- Long
      val caught8 = intercept[TestFailedException] {
        sevenDotOhFloat should not be (7.1f +- 2L)
      }
      assert(caught8.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float +- Int
      val caught9 = intercept[TestFailedException] {
        sevenDotOhFloat should not be (7.1f +- 2)
      }
      assert(caught9.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float +- Short
      val caught10 = intercept[TestFailedException] {
        sevenDotOhFloat should not be (7.1f +- 2.toShort)
      }
      assert(caught10.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float +- Byte
      val caught11 = intercept[TestFailedException] {
        sevenDotOhFloat should not be (7.1f +- 2.toByte)
      }
      assert(caught11.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Long +- Long
      val caught12 = intercept[TestFailedException] {
        sevenLong should not be (9L +- 2L)
      }
      assert(caught12.getMessage === "7 was 9 plus or minus 2")

      // Long +- Int
      val caught13 = intercept[TestFailedException] {
        sevenLong should not be (9L +- 2)
      }
      assert(caught13.getMessage === "7 was 9 plus or minus 2")

      // Long +- Short
      val caught14 = intercept[TestFailedException] {
        sevenLong should not be (9L +- 2.toShort)
      }
      assert(caught14.getMessage === "7 was 9 plus or minus 2")

      // Long +- Byte
      val caught15 = intercept[TestFailedException] {
        sevenLong should not be (9L +- 2.toByte)
      }
      assert(caught15.getMessage === "7 was 9 plus or minus 2")

      // Int +- Int
      val caught16 = intercept[TestFailedException] {
        sevenInt should not be (9 +- 2)
      }
      assert(caught16.getMessage === "7 was 9 plus or minus 2")

      // Int +- Short
      val caught17 = intercept[TestFailedException] {
        sevenInt should not be (9 +- 2.toShort)
      }
      assert(caught17.getMessage === "7 was 9 plus or minus 2")

      // Int +- Byte
      val caught18 = intercept[TestFailedException] {
        sevenInt should not be (9 +- 2.toByte)
      }
      assert(caught18.getMessage === "7 was 9 plus or minus 2")

      // Short +- Short
      val caught19 = intercept[TestFailedException] {
        sevenShort should not be (9.toShort +- 2.toShort)
      }
      assert(caught19.getMessage === "7 was 9 plus or minus 2")

      // Short +- Byte
      val caught20 = intercept[TestFailedException] {
        sevenShort should not be (9.toShort +- 2.toByte)
      }
      assert(caught20.getMessage === "7 was 9 plus or minus 2")

      // Byte +- Byte
      val caught21 = intercept[TestFailedException] {
        sevenByte should not be (9.toByte +- 2.toByte)
      }
      assert(caught21.getMessage === "7 was 9 plus or minus 2")
    }

    def `should throw TestFailedException if the number is not within the specified range, when used in a logical-and expression` {

      // Double +- Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh should ((be (17.1 +- 0.2)) and (be (17.1 +- 0.2)))
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh should (be (6.9 +- 0.2) and (be (17.1 +- 0.2)))
      }
      assert(caught2.getMessage === "7.0 was 6.9 plus or minus 0.2, but 7.0 was not 17.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh should (be (17.0 +- 0.2) and be (7.0 +- 0.2))
      }
      assert(caught3.getMessage === "7.0 was not 17.0 plus or minus 0.2")

      // Double +- Float
      val caught4 = intercept[TestFailedException] {
        sevenDotOh should ((be (17.1 +- 0.2f)) and (be (17.1 +- 0.2f)))
      }
      assert(caught4.getMessage === "7.0 was not 17.1 plus or minus 0.20000000298023224")
      val caught5 = intercept[TestFailedException] {
        sevenDotOh should (be (6.9 +- 0.2f) and (be (17.1 +- 0.2f)))
      }
      assert(caught5.getMessage === "7.0 was 6.9 plus or minus 0.20000000298023224, but 7.0 was not 17.1 plus or minus 0.20000000298023224")
      val caught6 = intercept[TestFailedException] {
        sevenDotOh should (be (17.0 +- 0.2f) and be (7.0 +- 0.2f))
      }
      assert(caught6.getMessage === "7.0 was not 17.0 plus or minus 0.20000000298023224")

      // Double +- Long
      val caught7 = intercept[TestFailedException] {
        sevenDotOh should ((be (17.1 +- 2L)) and (be (17.1 +- 2L)))
      }
      assert(caught7.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught8 = intercept[TestFailedException] {
        sevenDotOh should (be (6.9 +- 2L) and (be (17.1 +- 2L)))
      }
      assert(caught8.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught9 = intercept[TestFailedException] {
        sevenDotOh should (be (17.0 +- 2L) and be (7.0 +- 2L))
      }
      assert(caught9.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Double +- Int
      val caught10 = intercept[TestFailedException] {
        sevenDotOh should ((be (17.1 +- 2)) and (be (17.1 +- 2)))
      }
      assert(caught10.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught11 = intercept[TestFailedException] {
        sevenDotOh should (be (6.9 +- 2) and (be (17.1 +- 2)))
      }
      assert(caught2.getMessage === "7.0 was 6.9 plus or minus 0.2, but 7.0 was not 17.1 plus or minus 0.2")
      val caught12 = intercept[TestFailedException] {
        sevenDotOh should (be (7.0 +- 2) and be (17.0 +- 2))
      }
      assert(caught12.getMessage === "7.0 was 7.0 plus or minus 2.0, but 7.0 was not 17.0 plus or minus 2.0")

      // Double +- Short
      val caught13 = intercept[TestFailedException] {
        sevenDotOh should ((be (17.1 +- 2.toShort)) and (be (17.1 +- 2.toShort)))
      }
      assert(caught13.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught14 = intercept[TestFailedException] {
        sevenDotOh should (be (6.9 +- 2.toShort) and (be (17.1 +- 2.toShort)))
      }
      assert(caught14.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught15 = intercept[TestFailedException] {
        sevenDotOh should (be (17.0 +- 2.toShort) and be (7.0 +- 2.toShort))
      }
      assert(caught15.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Double +- Byte
      val caught16 = intercept[TestFailedException] {
        sevenDotOh should ((be (17.1 +- 2.toByte)) and (be (17.1 +- 2.toByte)))
      }
      assert(caught16.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught17 = intercept[TestFailedException] {
        sevenDotOh should (be (6.9 +- 2.toByte) and (be (17.1 +- 2.toByte)))
      }
      assert(caught17.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught18 = intercept[TestFailedException] {
        sevenDotOh should (be (17.0 +- 2.toByte) and be (7.0 +- 2.toByte))
      }
      assert(caught18.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float +- Float
      val caught19 = intercept[TestFailedException] {
        sevenDotOhFloat should ((be (17.1f +- 0.2f)) and (be (17.1f +- 0.2f)))
      }
      assert(caught19.getMessage === "7.0 was not 17.1 plus or minus 0.2")
      val caught20 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (6.9f +- 0.2f) and (be (17.1f +- 0.2f)))
      }
      assert(caught20.getMessage === "7.0 was 6.9 plus or minus 0.2, but 7.0 was not 17.1 plus or minus 0.2")
      val caught21 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (17.0f +- 0.2f) and be (7.0f +- 0.2f))
      }
      assert(caught21.getMessage === "7.0 was not 17.0 plus or minus 0.2")

      // Float +- Long
      val caught22 = intercept[TestFailedException] {
        sevenDotOhFloat should ((be (17.1f +- 2L)) and (be (17.1f +- 2L)))
      }
      assert(caught22.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught23 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (6.9f +- 2L) and (be (17.1f +- 2L)))
      }
      assert(caught23.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught24 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (17.0f +- 2L) and be (7.0f +- 2L))
      }
      assert(caught24.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float +- Int
      val caught25 = intercept[TestFailedException] {
        sevenDotOhFloat should ((be (17.1f +- 2)) and (be (17.1f +- 2)))
      }
      assert(caught25.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught26 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (6.9f +- 2) and (be (17.1f +- 2)))
      }
      assert(caught26.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught27 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (17.0f +- 2) and be (7.0f +- 2))
      }
      assert(caught27.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float +- Short
      val caught28 = intercept[TestFailedException] {
        sevenDotOhFloat should ((be (17.1f +- 2.toShort)) and (be (17.1f +- 2.toShort)))
      }
      assert(caught28.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught29 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (6.9f +- 2.toShort) and (be (17.1f +- 2.toShort)))
      }
      assert(caught29.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught30 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (17.0f +- 2.toShort) and be (7.0f +- 2.toShort))
      }
      assert(caught30.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float +- Byte
      val caught31 = intercept[TestFailedException] {
        sevenDotOhFloat should ((be (17.1f +- 2.toByte)) and (be (17.1f +- 2.toByte)))
      }
      assert(caught31.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught32 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (6.9f +- 2.toByte) and (be (17.1f +- 2.toByte)))
      }
      assert(caught32.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught33 = intercept[TestFailedException] {
        sevenDotOhFloat should (be (17.0f +- 2.toByte) and be (7.0f +- 2.toByte))
      }
      assert(caught33.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Long +- Long
      val caught34 = intercept[TestFailedException] {
        sevenLong should ((be (19L +- 2L)) and (be (9L +- 2L)))
      }
      assert(caught34.getMessage === "7 was not 19 plus or minus 2")
      val caught35 = intercept[TestFailedException] {
        sevenLong should (be (18L +- 2L) and (be (19L +- 2L)))
      }
      assert(caught35.getMessage === "7 was not 18 plus or minus 2")
      val caught36 = intercept[TestFailedException] {
        sevenLong should (be (17L +- 2L) and be (7L +- 2L))
      }
      assert(caught36.getMessage === "7 was not 17 plus or minus 2")

      // Long +- Int
      val caught37 = intercept[TestFailedException] {
        sevenLong should ((be (19L +- 2)) and (be (9L +- 2)))
      }
      assert(caught37.getMessage === "7 was not 19 plus or minus 2")
      val caught38 = intercept[TestFailedException] {
        sevenLong should (be (8L +- 2) and (be (19L +- 2)))
      }
      assert(caught38.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught39 = intercept[TestFailedException] {
        sevenLong should (be (17L +- 2) and be (7L +- 2))
      }
      assert(caught39.getMessage === "7 was not 17 plus or minus 2")

      // Long +- Short
      val caught40 = intercept[TestFailedException] {
        sevenLong should ((be (19L +- 2.toShort)) and (be (9L +- 2.toShort)))
      }
      assert(caught40.getMessage === "7 was not 19 plus or minus 2")
      val caught41 = intercept[TestFailedException] {
        sevenLong should (be (8L +- 2.toShort) and (be (19L +- 2.toShort)))
      }
      assert(caught41.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught42 = intercept[TestFailedException] {
        sevenLong should (be (17L +- 2.toShort) and be (7L +- 2.toShort))
      }
      assert(caught42.getMessage === "7 was not 17 plus or minus 2")

      // Long +- Byte
      val caught43 = intercept[TestFailedException] {
        sevenLong should ((be (19L +- 2.toByte)) and (be (9L +- 2.toByte)))
      }
      assert(caught43.getMessage === "7 was not 19 plus or minus 2")
      val caught44 = intercept[TestFailedException] {
        sevenLong should (be (8L +- 2.toByte) and (be (19L +- 2.toByte)))
      }
      assert(caught44.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught45 = intercept[TestFailedException] {
        sevenLong should (be (17L +- 2.toByte) and be (7L +- 2.toByte))
      }
      assert(caught45.getMessage === "7 was not 17 plus or minus 2")

      // Int +- Int
      val caught46 = intercept[TestFailedException] {
        sevenInt should ((be (19 +- 2)) and (be (9 +- 2)))
      }
      assert(caught46.getMessage === "7 was not 19 plus or minus 2")
      val caught47 = intercept[TestFailedException] {
        sevenInt should (be (8 +- 2) and (be (19 +- 2)))
      }
      assert(caught47.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught48 = intercept[TestFailedException] {
        sevenInt should (be (17 +- 2) and be (7 +- 2))
      }
      assert(caught48.getMessage === "7 was not 17 plus or minus 2")

      // Int +- Short
      val caught49 = intercept[TestFailedException] {
        sevenInt should ((be (9 +- 2.toShort)) and (be (19 +- 2.toShort)))
      }
      assert(caught49.getMessage === "7 was 9 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught50 = intercept[TestFailedException] {
        sevenInt should (be (8 +- 2.toShort) and (be (19 +- 2.toShort)))
      }
      assert(caught50.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught51 = intercept[TestFailedException] {
        sevenInt should (be (17 +- 2.toShort) and be (7 +- 2.toShort))
      }
      assert(caught51.getMessage === "7 was not 17 plus or minus 2")

      // Int +- Byte
      val caught52 = intercept[TestFailedException] {
        sevenInt should ((be (19 +- 2.toByte)) and (be (9 +- 2.toByte)))
      }
      assert(caught52.getMessage === "7 was not 19 plus or minus 2")
      val caught53 = intercept[TestFailedException] {
        sevenInt should (be (8 +- 2.toByte) and (be (19 +- 2.toByte)))
      }
      assert(caught53.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught54 = intercept[TestFailedException] {
        sevenInt should (be (17 +- 2.toByte) and be (7 +- 2.toByte))
      }
      assert(caught54.getMessage === "7 was not 17 plus or minus 2")

      // Short +- Short
      val caught55 = intercept[TestFailedException] {
        sevenShort should ((be (19.toShort +- 2.toShort)) and (be (9.toShort +- 2.toShort)))
      }
      assert(caught55.getMessage === "7 was not 19 plus or minus 2")
      val caught56 = intercept[TestFailedException] {
        sevenShort should (be (8.toShort +- 2.toShort) and (be (19.toShort +- 2.toShort)))
      }
      assert(caught56.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught57 = intercept[TestFailedException] {
        sevenShort should (be (17.toShort +- 2.toShort) and be (7.toShort +- 2.toShort))
      }
      assert(caught57.getMessage === "7 was not 17 plus or minus 2")

      // Short +- Byte
      val caught58 = intercept[TestFailedException] {
        sevenShort should ((be (19.toShort +- 2.toByte)) and (be (9.toShort +- 2.toByte)))
      }
      assert(caught58.getMessage === "7 was not 19 plus or minus 2")
      val caught59 = intercept[TestFailedException] {
        sevenShort should (be (8.toShort +- 2.toByte) and (be (19.toShort +- 2.toByte)))
      }
      assert(caught59.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught60 = intercept[TestFailedException] {
        sevenShort should (be (17.toShort +- 2.toByte) and be (7.toShort +- 2.toByte))
      }
      assert(caught60.getMessage === "7 was not 17 plus or minus 2")

      // Byte +- Byte
      val caught61 = intercept[TestFailedException] {
        sevenByte should ((be (19.toByte +- 2.toByte)) and (be (9.toByte +- 2.toByte)))
      }
      assert(caught61.getMessage === "7 was not 19 plus or minus 2")
      val caught62 = intercept[TestFailedException] {
        sevenByte should (be (8.toByte +- 2.toByte) and (be (19.toByte +- 2.toByte)))
      }
      assert(caught62.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught63 = intercept[TestFailedException] {
        sevenByte should (be (17.toByte +- 2.toByte) and be (7.toByte +- 2.toByte))
      }
      assert(caught63.getMessage === "7 was not 17 plus or minus 2")
    }

    def `should throw TestFailedException if the number is not within the specified range, when used in a logical-or expression` {

      // Double +- Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh should ((be (17.1 +- 0.2)) or (be (17.1 +- 0.2)))
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2, and 7.0 was not 17.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh should (be (16.9 +- 0.2) or (be (17.1 +- 0.2)))
      }
      assert(caught2.getMessage === "7.0 was not 16.9 plus or minus 0.2, and 7.0 was not 17.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh should (be (17.0 +- 0.2) or be (97.0 +- 0.2))
      }
      assert(caught3.getMessage === "7.0 was not 17.0 plus or minus 0.2, and 7.0 was not 97.0 plus or minus 0.2")
    }

    def `should throw TestFailedException if the number is within the specified range, when used in a logical-and expression with not` {

      // Double +- Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh should (not (be (17.1 +- 0.2)) and not (be (7.1 +- 0.2)))
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2, but 7.0 was 7.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh should (not be (16.9 +- 0.2) and (not be (7.1 +- 0.2)))
      }
      assert(caught2.getMessage === "7.0 was not 16.9 plus or minus 0.2, but 7.0 was 7.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh should (not be (17.0 +- 0.2) and not be (7.0 +- 0.2))
      }
      assert(caught3.getMessage === "7.0 was not 17.0 plus or minus 0.2, but 7.0 was 7.0 plus or minus 0.2")

      // Check that the error message "short circuits"
      val caught4 = intercept[TestFailedException] {
        sevenDotOh should (not (be (7.1 +- 0.2)) and not (be (7.1 +- 0.2)))
      }
      assert(caught4.getMessage === "7.0 was 7.1 plus or minus 0.2")
    }

    def `should throw TestFailedException if the number is within the specified range, when used in a logical-or expression with not` {

      // Double +- Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh should (not (be (7.1 +- 0.2)) or not (be (7.1 +- 0.2)))
      }
      assert(caught1.getMessage === "7.0 was 7.1 plus or minus 0.2, and 7.0 was 7.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh should ((not be (6.9 +- 0.2)) or (not be (7.1 +- 0.2)))
      }
      assert(caught2.getMessage === "7.0 was 6.9 plus or minus 0.2, and 7.0 was 7.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh should (not be (7.0 +- 0.2) or not be (7.0 +- 0.2))
      }
      assert(caught3.getMessage === "7.0 was 7.0 plus or minus 0.2, and 7.0 was 7.0 plus or minus 0.2")
    }

    def `should throw IllegalArgumentException if the number passed as the range is 0 or negative` {

      // Double +- Double
      val caught1 = intercept[IllegalArgumentException] {
        sevenDotOh should be (7.1 +- -0.2)
      }
      assert(caught1.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Float
      val caught2 = intercept[IllegalArgumentException] {
        sevenDotOh should be (7.1 +- -0.2f)
      }
      assert(caught2.getMessage === "-0.20000000298023224 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Long
      val caught3 = intercept[IllegalArgumentException] {
        sevenDotOh should be (7.1 +- -2L)
      }
      assert(caught3.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Int
      val caught4 = intercept[IllegalArgumentException] {
        sevenDotOh should be (7.1 +- -2)
      }
      assert(caught4.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Short
      val caught5 = intercept[IllegalArgumentException] {
        sevenDotOh should be (7.1 +- (-2).toShort)
      }
      assert(caught5.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Byte
      val caught6 = intercept[IllegalArgumentException] {
        sevenDotOh should be (7.1 +- (-2).toByte)
      }
      assert(caught6.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Float
      val caught7 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should be (7.1f +- -0.2f)
      }
      assert(caught7.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Long
      val caught8 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should be (7.1f +- -2L)
      }
      assert(caught8.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Int
      val caught9 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should be (7.1f +- -2)
      }
      assert(caught9.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Short
      val caught10 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should be (7.1f +- (-2).toShort)
      }
      assert(caught10.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Byte
      val caught11 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should be (7.1f +- (-2).toByte)
      }
      assert(caught11.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Long
      val caught12 = intercept[IllegalArgumentException] {
        sevenLong should be (9L +- -2L)
      }
      assert(caught12.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Int
      val caught13 = intercept[IllegalArgumentException] {
        sevenLong should be (9L +- -2)
      }
      assert(caught13.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Short
      val caught14 = intercept[IllegalArgumentException] {
        sevenLong should be (9L +- (-2).toShort)
      }
      assert(caught14.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Byte
      val caught15 = intercept[IllegalArgumentException] {
        sevenLong should be (9L +- (-2).toByte)
      }
      assert(caught15.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Int
      val caught16 = intercept[IllegalArgumentException] {
        sevenInt should be (9 +- -2)
      }
      assert(caught16.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Short
      val caught17 = intercept[IllegalArgumentException] {
        sevenInt should be (9 +- (-2).toShort)
      }
      assert(caught17.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Byte
      val caught18 = intercept[IllegalArgumentException] {
        sevenInt should be (9 +- (-2).toByte)
      }
      assert(caught18.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Short
      val caught19 = intercept[IllegalArgumentException] {
        sevenShort should be (9.toShort +- (-2).toShort)
      }
      assert(caught19.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Byte
      val caught20 = intercept[IllegalArgumentException] {
        sevenShort should be (9.toShort +- (-2).toByte)
      }
      assert(caught20.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Byte +- Byte
      val caught21 = intercept[IllegalArgumentException] {
        sevenByte should be (9.toByte +- (-2).toByte)
      }
      assert(caught21.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")
    }
  }
}
