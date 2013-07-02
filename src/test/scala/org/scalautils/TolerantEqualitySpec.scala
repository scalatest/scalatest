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
package org.scalautils

import org.scalatest._

// 1,$s/tolerantEquality\[Byte\](/tolerantByteEquality(tolerance = /

class TolerantEqualitySpec extends Spec with TripleEquals with TolerantNumerics {

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

  object `The === syntax` {

    def `should be true if the number is within the given interval for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(sevenDotOh === 7.1)
      assert(sevenDotOh === 6.9)
      assert(sevenDotOh === 7.0)
      assert(sevenDotOh === 7.2)
      assert(sevenDotOh === 6.8)
      assert(minusSevenDotOh === -7.1)
      assert(minusSevenDotOh === -6.9)
      assert(minusSevenDotOh === -7.0)
      assert(minusSevenDotOh === -7.2)
      assert(minusSevenDotOh === -6.8)
    }

    def `should be true if the number is within the given interval for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(sevenDotOhFloat === 7.1f)
      assert(sevenDotOhFloat === 6.9f)
      assert(sevenDotOhFloat === 7.0f)
      assert(sevenDotOhFloat === 7.2f)
      assert(sevenDotOhFloat === 6.8f)
      assert(minusSevenDotOhFloat === -7.1f)
      assert(minusSevenDotOhFloat === -6.9f)
      assert(minusSevenDotOhFloat === -7.0f)
      assert(minusSevenDotOhFloat === -7.2f)
      assert(minusSevenDotOhFloat === -6.8f)
    }

    def `should be true if the number is within the given interval for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(sevenLong === 9L)
      assert(sevenLong === 8L)
      assert(sevenLong === 7L)
      assert(sevenLong === 6L)
      assert(sevenLong === 5L)
      assert(minusSevenLong === -9L)
      assert(minusSevenLong === -8L)
      assert(minusSevenLong === -7L)
      assert(minusSevenLong === -6L)
      assert(minusSevenLong === -5L)
    }

    def `should be true if the number is within the given interval for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(sevenInt === 9)
      assert(sevenInt === 8)
      assert(sevenInt === 7)
      assert(sevenInt === 6)
      assert(sevenInt === 5)
      assert(minusSevenInt === -9)
      assert(minusSevenInt === -8)
      assert(minusSevenInt === -7)
      assert(minusSevenInt === -6)
      assert(minusSevenInt === -5)
    }

    def `should be true if the number is within the given interval for Short` {

      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(sevenShort === 9.toShort)
      assert(sevenShort === 8.toShort)
      assert(sevenShort === 7.toShort)
      assert(sevenShort === 6.toShort)
      assert(sevenShort === 5.toShort)
      assert(minusSevenShort === (-9).toShort)
      assert(minusSevenShort === (-8).toShort)
      assert(minusSevenShort === (-7).toShort)
      assert(minusSevenShort === (-6).toShort)
      assert(minusSevenShort === (-5).toShort)
    }

    def `should be true if the number is within the given interval for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(sevenByte === 9.toByte)
      assert(sevenByte === 8.toByte)
      assert(sevenByte === 7.toByte)
      assert(sevenByte === 6.toByte)
      assert(sevenByte === 5.toByte)
      assert(minusSevenByte === (-9).toByte)
      assert(minusSevenByte === (-8).toByte)
      assert(minusSevenByte === (-7).toByte)
      assert(minusSevenByte === (-6).toByte)
      assert(minusSevenByte === (-5).toByte)
    }

    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(7.1 === sevenDotOh)
      assert(6.9 === sevenDotOh)
      assert(7.0 === sevenDotOh)
      assert(7.2 === sevenDotOh)
      assert(6.8 === sevenDotOh)
      assert(-7.1 === minusSevenDotOh)
      assert(-6.9 === minusSevenDotOh)
      assert(-7.0 === minusSevenDotOh)
      assert(-7.2 === minusSevenDotOh)
      assert(-6.8 === minusSevenDotOh)
    }

    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(7.1f === sevenDotOhFloat)
      assert(6.9f === sevenDotOhFloat)
      assert(7.0f === sevenDotOhFloat)
      assert(7.2f === sevenDotOhFloat)
      assert(6.8f === sevenDotOhFloat)
      assert(-7.1f === minusSevenDotOhFloat)
      assert(-6.9f === minusSevenDotOhFloat)
      assert(-7.0f === minusSevenDotOhFloat)
      assert(-7.2f === minusSevenDotOhFloat)
      assert(-6.8f === minusSevenDotOhFloat)
    }

    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(9L === sevenLong)
      assert(8L === sevenLong)
      assert(7L === sevenLong)
      assert(6L === sevenLong)
      assert(5L === sevenLong)
      assert(-9L === minusSevenLong)
      assert(-8L === minusSevenLong)
      assert(-7L === minusSevenLong)
      assert(-6L === minusSevenLong)
      assert(-5L === minusSevenLong)
    }

    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(9 === sevenInt)
      assert(8 === sevenInt)
      assert(7 === sevenInt)
      assert(6 === sevenInt)
      assert(5 === sevenInt)
      assert(-9 === minusSevenInt)
      assert(-8 === minusSevenInt)
      assert(-7 === minusSevenInt)
      assert(-6 === minusSevenInt)
      assert(-5 === minusSevenInt)
    }

    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side for Short` {
      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(9.toShort === sevenShort)
      assert(8.toShort === sevenShort)
      assert(7.toShort === sevenShort)
      assert(6.toShort === sevenShort)
      assert(5.toShort === sevenShort)
      assert((-9).toShort === minusSevenShort)
      assert((-8).toShort === minusSevenShort)
      assert((-7).toShort === minusSevenShort)
      assert((-6).toShort === minusSevenShort)
      assert((-5).toShort === minusSevenShort)
    }

    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(9.toByte === sevenByte)
      assert(8.toByte === sevenByte)
      assert(7.toByte === sevenByte)
      assert(6.toByte === sevenByte)
      assert(5.toByte === sevenByte)
      assert((-9).toByte === minusSevenByte)
      assert((-8).toByte === minusSevenByte)
      assert((-7).toByte === minusSevenByte)
      assert((-6).toByte === minusSevenByte)
      assert((-5).toByte === minusSevenByte)
    }

    def `should be false if the number is outside the given interval for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(!(sevenDotOh === 7.5))
      assert(!(sevenDotOh === 6.5))
      assert(!(minusSevenDotOh === -7.5))
      assert(!(minusSevenDotOh === -6.5))
    }

    def `should be false if the number is outside the given interval for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(!(sevenDotOhFloat === 7.5f))
      assert(!(sevenDotOhFloat === 6.5f))
      assert(!(minusSevenDotOhFloat === -7.5f))
      assert(!(minusSevenDotOhFloat === -6.5f))
    }

    def `should be false if the number is outside the given interval for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(!(sevenLong === 4L))
      assert(!(sevenLong === 10L))
      assert(!(minusSevenLong === -4L))
      assert(!(minusSevenLong === -10L))
    }

    def `should be false if the number is outside the given interval for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(!(sevenInt === 4))
      assert(!(sevenInt === 10))
      assert(!(minusSevenInt === -4))
      assert(!(minusSevenInt === -10))
    }

    def `should be false if the number is outside the given interval for Short` {

      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(!(sevenShort === 4.toShort))
      assert(!(sevenShort === 10.toShort))
      assert(!(minusSevenShort === (-4).toShort))
      assert(!(minusSevenShort === (-10).toShort))
    }

    def `should be false if the number is outside the given interval for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(!(sevenByte === 4.toByte))
      assert(!(sevenByte === 10.toByte))
      assert(!(minusSevenByte === (-4).toByte))
      assert(!(minusSevenByte === (-10).toByte))
    }

    def `should, for symmetry, be false if the number is outside the given interval, when the interval is on the left hand side for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(!(7.5 === sevenDotOh))
      assert(!(6.5 === sevenDotOh))
      assert(!(-7.5 === minusSevenDotOh))
      assert(!(-6.5 === minusSevenDotOh))
    }

    def `should, for symmetry, be false if the number is outside the given interval, when the interval is on the left hand side for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(!(7.5f === sevenDotOhFloat))
      assert(!(6.5f === sevenDotOhFloat))
      assert(!(-7.5f === minusSevenDotOhFloat))
      assert(!(-6.5f === minusSevenDotOhFloat))
    }

    def `should, for symmetry, be false if the number is outside the given interval, when the interval is on the left hand side for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(!(4L === sevenLong))
      assert(!(10L === sevenLong))
      assert(!(-4L === minusSevenLong))
      assert(!(-10L === minusSevenLong))
    }

    def `should, for symmetry, be false if the number is outside the given interval, when the interval is on the left hand side for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(!(4 === sevenInt))
      assert(!(10 === sevenInt))
      assert(!(-4 === minusSevenInt))
      assert(!(-10 === minusSevenInt))
    }

    def `should, for symmetry, be false if the number is outside the given interval, when the interval is on the left hand side for Short` {

      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(!(4.toShort === sevenShort))
      assert(!(10.toShort === sevenShort))
      assert(!((-4).toShort === minusSevenShort))
      assert(!((-10).toShort === minusSevenShort))
    }

    def `should, for symmetry, be false if the number is outside the given interval, when the interval is on the left hand side for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(!(4.toByte === sevenByte))
      assert(!(10.toByte === sevenByte))
      assert(!((-4).toByte === minusSevenByte))
      assert(!((-10).toByte === minusSevenByte))
    }
  }

  object `The !== syntax` {

    def `should be true if the number is outside the given interval for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(sevenDotOh !== 7.5)
      assert(sevenDotOh !== 6.5)
      assert(minusSevenDotOh !== -7.5)
      assert(minusSevenDotOh !== -6.5)
    }

    def `should be true if the number is outside the given interval for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(sevenDotOhFloat !== 7.5f)
      assert(sevenDotOhFloat !== 6.5f)
      assert(minusSevenDotOhFloat !== -7.5f)
      assert(minusSevenDotOhFloat !== -6.5f)
    }

    def `should be true if the number is outside the given interval for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(sevenLong !== 4L)
      assert(sevenLong !== 10L)
      assert(minusSevenLong !== -4L)
      assert(minusSevenLong !== -10L)
    }

    def `should be true if the number is outside the given interval for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(sevenInt !== 4)
      assert(sevenInt !== 10)
      assert(minusSevenInt !== -4)
      assert(minusSevenInt !== -10)
    }

    def `should be true if the number is outside the given interval for Short` {

      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(sevenShort !== 4.toShort)
      assert(sevenShort !== 10.toShort)
      assert(minusSevenShort !== (-4).toShort)
      assert(minusSevenShort !== (-10).toShort)
    }

    def `should be true if the number is outside the given interval for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(sevenByte !== 4.toByte)
      assert(sevenByte !== 10.toByte)
      assert(minusSevenByte !== (-4).toByte)
      assert(minusSevenByte !== (-10).toByte)
    }

    def `should, for symmetry, be true if the number is outside the given interval when the interval is placed on the left hand side for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(7.5 !== sevenDotOh)
      assert(6.5 !== sevenDotOh)
      assert(-7.5 !== minusSevenDotOh)
      assert(-6.5 !== minusSevenDotOh)
    }

    def `should, for symmetry, be true if the number is outside the given interval when the interval is placed on the left hand side for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(7.5f !== sevenDotOhFloat)
      assert(6.5f !== sevenDotOhFloat)
      assert(-7.5f !== minusSevenDotOhFloat)
      assert(-6.5f !== minusSevenDotOhFloat)
    }

    def `should, for symmetry, be true if the number is outside the given interval when the interval is placed on the left hand side for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(4L !== sevenLong)
      assert(10L !== sevenLong)
      assert(-4L !== minusSevenLong)
      assert(-10L !== minusSevenLong)
    }

    def `should, for symmetry, be true if the number is outside the given interval when the interval is placed on the left hand side for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(4 !== sevenInt)
      assert(10 !== sevenInt)
      assert(-4 !== minusSevenInt)
      assert(-10 !== minusSevenInt)
    }

    def `should, for symmetry, be true if the number is outside the given interval when the interval is placed on the left hand side for Short` {

      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(4.toShort !== sevenShort)
      assert(10.toShort !== sevenShort)
      assert((-4).toShort !== minusSevenShort)
      assert((-10).toShort !== minusSevenShort)
    }

    def `should, for symmetry, be true if the number is outside the given interval when the interval is placed on the left hand side for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(4.toByte !== sevenByte)
      assert(10.toByte !== sevenByte)
      assert((-4).toByte !== minusSevenByte)
      assert((-10).toByte !== minusSevenByte)
    }

    def `should be false if the number is within the given interval for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(!(sevenDotOh !== 7.1))
      assert(!(sevenDotOh !== 6.9))
      assert(!(sevenDotOh !== 7.0))
      assert(!(sevenDotOh !== 7.2))
      assert(!(sevenDotOh !== 6.8))
      assert(!(minusSevenDotOh !== -7.1))
      assert(!(minusSevenDotOh !== -6.9))
      assert(!(minusSevenDotOh !== -7.0))
      assert(!(minusSevenDotOh !== -7.2))
      assert(!(minusSevenDotOh !== -6.8))
    }

    def `should be false if the number is within the given interval for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(!(sevenDotOhFloat !== 7.1f))
      assert(!(sevenDotOhFloat !== 6.9f))
      assert(!(sevenDotOhFloat !== 7.0f))
      assert(!(sevenDotOhFloat !== 7.2f))
      assert(!(sevenDotOhFloat !== 6.8f))
      assert(!(minusSevenDotOhFloat !== -7.1f))
      assert(!(minusSevenDotOhFloat !== -6.9f))
      assert(!(minusSevenDotOhFloat !== -7.0f))
      assert(!(minusSevenDotOhFloat !== -7.2f))
      assert(!(minusSevenDotOhFloat !== -6.8f))
    }

    def `should be false if the number is within the given interval for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(!(sevenLong !== 9L))
      assert(!(sevenLong !== 8L))
      assert(!(sevenLong !== 7L))
      assert(!(sevenLong !== 6L))
      assert(!(sevenLong !== 5L))
      assert(!(minusSevenLong !== -9L))
      assert(!(minusSevenLong !== -8L))
      assert(!(minusSevenLong !== -7L))
      assert(!(minusSevenLong !== -6L))
      assert(!(minusSevenLong !== -5L))
    }

    def `should be false if the number is within the given interval for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(!(sevenInt !== 9))
      assert(!(sevenInt !== 8))
      assert(!(sevenInt !== 7))
      assert(!(sevenInt !== 6))
      assert(!(sevenInt !== 5))
      assert(!(minusSevenInt !== -9))
      assert(!(minusSevenInt !== -8))
      assert(!(minusSevenInt !== -7))
      assert(!(minusSevenInt !== -6))
      assert(!(minusSevenInt !== -5))
    }

    def `should be false if the number is within the given interval for Short` {

      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(!(sevenShort !== 9.toShort))
      assert(!(sevenShort !== 8.toShort))
      assert(!(sevenShort !== 7.toShort))
      assert(!(sevenShort !== 6.toShort))
      assert(!(sevenShort !== 5.toShort))
      assert(!(minusSevenShort !== (-9).toShort))
      assert(!(minusSevenShort !== (-8).toShort))
      assert(!(minusSevenShort !== (-7).toShort))
      assert(!(minusSevenShort !== (-6).toShort))
      assert(!(minusSevenShort !== (-5).toShort))
    }

    def `should be false if the number is within the given interval for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(!(sevenByte !== 9.toByte))
      assert(!(sevenByte !== 8.toByte))
      assert(!(sevenByte !== 7.toByte))
      assert(!(sevenByte !== 6.toByte))
      assert(!(sevenByte !== 5.toByte))
      assert(!(minusSevenByte !== (-9).toByte))
      assert(!(minusSevenByte !== (-8).toByte))
      assert(!(minusSevenByte !== (-7).toByte))
      assert(!(minusSevenByte !== (-6).toByte))
      assert(!(minusSevenByte !== (-5).toByte))
    }

    def `should, for symmetry, be false if the number is within the given interval, when the interval is placed on the left hand side for Double` {

      // Double +- Double
      implicit val eq = tolerantDoubleEquality(tolerance = 0.2)
      assert(!(7.1 !== sevenDotOh))
      assert(!(6.9 !== sevenDotOh))
      assert(!(7.0 !== sevenDotOh))
      assert(!(7.2 !== sevenDotOh))
      assert(!(6.8 !== sevenDotOh))
      assert(!(-7.1 !== minusSevenDotOh))
      assert(!(-6.9 !== minusSevenDotOh))
      assert(!(-7.0 !== minusSevenDotOh))
      assert(!(-7.2 !== minusSevenDotOh))
      assert(!(-6.8 !== minusSevenDotOh))
    }

    def `should, for symmetry, be false if the number is within the given interval, when the interval is placed on the left hand side for Float` {

      // Float +- Float
      implicit val eq = tolerantFloatEquality(tolerance = 0.2f)
      assert(!(7.1f !== sevenDotOhFloat))
      assert(!(6.9f !== sevenDotOhFloat))
      assert(!(7.0f !== sevenDotOhFloat))
      assert(!(7.2f !== sevenDotOhFloat))
      assert(!(6.8f !== sevenDotOhFloat))
      assert(!(-7.1f !== minusSevenDotOhFloat))
      assert(!(-6.9f !== minusSevenDotOhFloat))
      assert(!(-7.0f !== minusSevenDotOhFloat))
      assert(!(-7.2f !== minusSevenDotOhFloat))
      assert(!(-6.8f !== minusSevenDotOhFloat))
    }

    def `should, for symmetry, be false if the number is within the given interval, when the interval is placed on the left hand side for Long` {

      // Long +- Long
      implicit val eq = tolerantLongEquality(tolerance = 2L)
      assert(!(9L !== sevenLong))
      assert(!(8L !== sevenLong))
      assert(!(7L !== sevenLong))
      assert(!(6L !== sevenLong))
      assert(!(5L !== sevenLong))
      assert(!(-9L !== minusSevenLong))
      assert(!(-8L !== minusSevenLong))
      assert(!(-7L !== minusSevenLong))
      assert(!(-6L !== minusSevenLong))
      assert(!(-5L !== minusSevenLong))
    }

    def `should, for symmetry, be false if the number is within the given interval, when the interval is placed on the left hand side for Int` {

      // Int +- Int
      implicit val eq = tolerantIntEquality(tolerance = 2)
      assert(!(9 !== sevenInt))
      assert(!(8 !== sevenInt))
      assert(!(7 !== sevenInt))
      assert(!(6 !== sevenInt))
      assert(!(5 !== sevenInt))
      assert(!(-9 !== minusSevenInt))
      assert(!(-8 !== minusSevenInt))
      assert(!(-7 !== minusSevenInt))
      assert(!(-6 !== minusSevenInt))
      assert(!(-5 !== minusSevenInt))
    }

    def `should, for symmetry, be false if the number is within the given interval, when the interval is placed on the left hand side for Short` {

      // Short +- Short
      implicit val eq = tolerantShortEquality(tolerance = 2.toShort)
      assert(!(9.toShort !== sevenShort))
      assert(!(8.toShort !== sevenShort))
      assert(!(7.toShort !== sevenShort))
      assert(!(6.toShort !== sevenShort))
      assert(!(5.toShort !== sevenShort))
      assert(!((-9).toShort !== minusSevenShort))
      assert(!((-8).toShort !== minusSevenShort))
      assert(!((-7).toShort !== minusSevenShort))
      assert(!((-6).toShort !== minusSevenShort))
      assert(!((-5).toShort !== minusSevenShort))
    }

    def `should, for symmetry, be false if the number is within the given interval, when the interval is placed on the left hand side for Byte` {

      // Byte +- Byte
      implicit val eq = tolerantByteEquality(tolerance = 2.toByte)
      assert(!(9.toByte !== sevenByte))
      assert(!(8.toByte !== sevenByte))
      assert(!(7.toByte !== sevenByte))
      assert(!(6.toByte !== sevenByte))
      assert(!(5.toByte !== sevenByte))
      assert(!((-9).toByte !== minusSevenByte))
      assert(!((-8).toByte !== minusSevenByte))
      assert(!((-7).toByte !== minusSevenByte))
      assert(!((-6).toByte !== minusSevenByte))
      assert(!((-5).toByte !== minusSevenByte))
    }
  }

  object `The X +- Y syntax` {

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative for Double` {

      // Double +- Double
      val caught1 = intercept[IllegalArgumentException] {
        tolerantDoubleEquality(tolerance = -0.2)
      }
      assert(caught1.getMessage === "-0.2 passed to tolerantDoubleEquality was zero or negative. Must be a positive non-zero number.", caught1.getMessage)
    }

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative for Float` {

      // Float +- Float
      val caught7 = intercept[IllegalArgumentException] {
        tolerantFloatEquality(tolerance = -0.2f)
      }
      assert(caught7.getMessage === "-0.2 passed to tolerantFloatEquality was zero or negative. Must be a positive non-zero number.")
    }

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative for Long` {

      // Long +- Long
      val caught12 = intercept[IllegalArgumentException] {
        tolerantLongEquality(tolerance = -2L)
      }
      assert(caught12.getMessage === "-2 passed to tolerantLongEquality was zero or negative. Must be a positive non-zero number.")
    }

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative for Int` {

      // Int +- Int
      val caught16 = intercept[IllegalArgumentException] {
        tolerantIntEquality(tolerance = -2)
      }
      assert(caught16.getMessage === "-2 passed to tolerantIntEquality was zero or negative. Must be a positive non-zero number.")
    }

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative for Short` {

      // Short +- Short
      val caught19 = intercept[IllegalArgumentException] {
        tolerantShortEquality(tolerance = (-2).toShort)
      }
      assert(caught19.getMessage === "-2 passed to tolerantShortEquality was zero or negative. Must be a positive non-zero number.")
    }

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative for Byte` {

      // Byte +- Byte
      val caught21 = intercept[IllegalArgumentException] {
        tolerantByteEquality(tolerance = (-2).toByte)
      }
      assert(caught21.getMessage === "-2 passed to tolerantByteEquality was zero or negative. Must be a positive non-zero number.")
    }
  }
}
