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

class ToleranceSpec extends Spec with TripleEquals with Tolerance {

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
  object `The === syntax` {

    def `should be true if the number is within the given interval` {

      // Double +- Double
      assert(sevenDotOh === (7.1 +- 0.2))
      assert(sevenDotOh === (6.9 +- 0.2))
      assert(sevenDotOh === (7.0 +- 0.2))
      assert(sevenDotOh === (7.2 +- 0.2))
      assert(sevenDotOh === (6.8 +- 0.2))
      assert(minusSevenDotOh === (-7.1 +- 0.2))
      assert(minusSevenDotOh === (-6.9 +- 0.2))
      assert(minusSevenDotOh === (-7.0 +- 0.2))
      assert(minusSevenDotOh === (-7.2 +- 0.2))
      assert(minusSevenDotOh === (-6.8 +- 0.2))

      // Double +- Float
      assert(sevenDotOh === (7.1 +- 0.2f))
      assert(sevenDotOh === (6.9 +- 0.2f))
      assert(sevenDotOh === (7.0 +- 0.2f))
      assert(sevenDotOh === (7.2 +- 0.2f))
      assert(sevenDotOh === (6.8 +- 0.2f))
      assert(minusSevenDotOh === (-7.1 +- 0.2f))
      assert(minusSevenDotOh === (-6.9 +- 0.2f))
      assert(minusSevenDotOh === (-7.0 +- 0.2f))
      assert(minusSevenDotOh === (-7.2 +- 0.2f))
      assert(minusSevenDotOh === (-6.8 +- 0.2f))

      // Double +- Long
      assert(sevenDotOh === (7.1 +- 2L))
      assert(sevenDotOh === (6.9 +- 2L))
      assert(sevenDotOh === (7.0 +- 2L))
      assert(sevenDotOh === (7.2 +- 2L))
      assert(sevenDotOh === (6.8 +- 2L))
      assert(minusSevenDotOh === (-7.1 +- 2L))
      assert(minusSevenDotOh === (-6.9 +- 2L))
      assert(minusSevenDotOh === (-7.0 +- 2L))
      assert(minusSevenDotOh === (-7.2 +- 2L))
      assert(minusSevenDotOh === (-6.8 +- 2L))

      // Double +- Int
      assert(sevenDotOh === (7.1 +- 2))
      assert(sevenDotOh === (6.9 +- 2))
      assert(sevenDotOh === (7.0 +- 2))
      assert(sevenDotOh === (7.2 +- 2))
      assert(sevenDotOh === (6.8 +- 2))
      assert(minusSevenDotOh === (-7.1 +- 2))
      assert(minusSevenDotOh === (-6.9 +- 2))
      assert(minusSevenDotOh === (-7.0 +- 2))
      assert(minusSevenDotOh === (-7.2 +- 2))
      assert(minusSevenDotOh === (-6.8 +- 2))

      // Double +- Short
      assert(sevenDotOh === (7.1 +- 2.toShort))
      assert(sevenDotOh === (6.9 +- 2.toShort))
      assert(sevenDotOh === (7.0 +- 2.toShort))
      assert(sevenDotOh === (7.2 +- 2.toShort))
      assert(sevenDotOh === (6.8 +- 2.toShort))
      assert(minusSevenDotOh === (-7.1 +- 2.toShort))
      assert(minusSevenDotOh === (-6.9 +- 2.toShort))
      assert(minusSevenDotOh === (-7.0 +- 2.toShort))
      assert(minusSevenDotOh === (-7.2 +- 2.toShort))
      assert(minusSevenDotOh === (-6.8 +- 2.toShort))

      // Double +- Byte
      assert(sevenDotOh === (7.1 +- 2.toByte))
      assert(sevenDotOh === (6.9 +- 2.toByte))
      assert(sevenDotOh === (7.0 +- 2.toByte))
      assert(sevenDotOh === (7.2 +- 2.toByte))
      assert(sevenDotOh === (6.8 +- 2.toByte))
      assert(minusSevenDotOh === (-7.1 +- 2.toByte))
      assert(minusSevenDotOh === (-6.9 +- 2.toByte))
      assert(minusSevenDotOh === (-7.0 +- 2.toByte))
      assert(minusSevenDotOh === (-7.2 +- 2.toByte))
      assert(minusSevenDotOh === (-6.8 +- 2.toByte))

      // Float +- Float
      assert(sevenDotOhFloat === (7.1f +- 0.2f))
      assert(sevenDotOhFloat === (6.9f +- 0.2f))
      assert(sevenDotOhFloat === (7.0f +- 0.2f))
      assert(sevenDotOhFloat === (7.2f +- 0.2f))
      assert(sevenDotOhFloat === (6.8f +- 0.2f))
      assert(minusSevenDotOhFloat === (-7.1f +- 0.2f))
      assert(minusSevenDotOhFloat === (-6.9f +- 0.2f))
      assert(minusSevenDotOhFloat === (-7.0f +- 0.2f))
      assert(minusSevenDotOhFloat === (-7.2f +- 0.2f))
      assert(minusSevenDotOhFloat === (-6.8f +- 0.2f))

      // Float +- Long
      assert(sevenDotOhFloat === (7.1f +- 2L))
      assert(sevenDotOhFloat === (6.9f +- 2L))
      assert(sevenDotOhFloat === (7.0f +- 2L))
      assert(sevenDotOhFloat === (7.2f +- 2L))
      assert(sevenDotOhFloat === (6.8f +- 2L))
      assert(minusSevenDotOhFloat === (-7.1f +- 2L))
      assert(minusSevenDotOhFloat === (-6.9f +- 2L))
      assert(minusSevenDotOhFloat === (-7.0f +- 2L))
      assert(minusSevenDotOhFloat === (-7.2f +- 2L))
      assert(minusSevenDotOhFloat === (-6.8f +- 2L))

      // Float +- Int
      assert(sevenDotOhFloat === (7.1f +- 2))
      assert(sevenDotOhFloat === (6.9f +- 2))
      assert(sevenDotOhFloat === (7.0f +- 2))
      assert(sevenDotOhFloat === (7.2f +- 2))
      assert(sevenDotOhFloat === (6.8f +- 2))
      assert(minusSevenDotOhFloat === (-7.1f +- 2))
      assert(minusSevenDotOhFloat === (-6.9f +- 2))
      assert(minusSevenDotOhFloat === (-7.0f +- 2))
      assert(minusSevenDotOhFloat === (-7.2f +- 2))
      assert(minusSevenDotOhFloat === (-6.8f +- 2))

      // Float +- Short
      assert(sevenDotOhFloat === (7.1f +- 2.toShort))
      assert(sevenDotOhFloat === (6.9f +- 2.toShort))
      assert(sevenDotOhFloat === (7.0f +- 2.toShort))
      assert(sevenDotOhFloat === (7.2f +- 2.toShort))
      assert(sevenDotOhFloat === (6.8f +- 2.toShort))
      assert(minusSevenDotOhFloat === (-7.1f +- 2.toShort))
      assert(minusSevenDotOhFloat === (-6.9f +- 2.toShort))
      assert(minusSevenDotOhFloat === (-7.0f +- 2.toShort))
      assert(minusSevenDotOhFloat === (-7.2f +- 2.toShort))
      assert(minusSevenDotOhFloat === (-6.8f +- 2.toShort))

      // Float +- Byte
      assert(sevenDotOhFloat === (7.1f +- 2.toByte))
      assert(sevenDotOhFloat === (6.9f +- 2.toByte))
      assert(sevenDotOhFloat === (7.0f +- 2.toByte))
      assert(sevenDotOhFloat === (7.2f +- 2.toByte))
      assert(sevenDotOhFloat === (6.8f +- 2.toByte))
      assert(minusSevenDotOhFloat === (-7.1f +- 2.toByte))
      assert(minusSevenDotOhFloat === (-6.9f +- 2.toByte))
      assert(minusSevenDotOhFloat === (-7.0f +- 2.toByte))
      assert(minusSevenDotOhFloat === (-7.2f +- 2.toByte))
      assert(minusSevenDotOhFloat === (-6.8f +- 2.toByte))

      // Long +- Long
      assert(sevenLong === (9L +- 2L))
      assert(sevenLong === (8L +- 2L))
      assert(sevenLong === (7L +- 2L))
      assert(sevenLong === (6L +- 2L))
      assert(sevenLong === (5L +- 2L))
      assert(minusSevenLong === (-9L +- 2L))
      assert(minusSevenLong === (-8L +- 2L))
      assert(minusSevenLong === (-7L +- 2L))
      assert(minusSevenLong === (-6L +- 2L))
      assert(minusSevenLong === (-5L +- 2L))

      // Long +- Int
      assert(sevenLong === (9L +- 2))
      assert(sevenLong === (8L +- 2))
      assert(sevenLong === (7L +- 2))
      assert(sevenLong === (6L +- 2))
      assert(sevenLong === (5L +- 2))
      assert(minusSevenLong === (-9L +- 2))
      assert(minusSevenLong === (-8L +- 2))
      assert(minusSevenLong === (-7L +- 2))
      assert(minusSevenLong === (-6L +- 2))
      assert(minusSevenLong === (-5L +- 2))

      // Long +- Short
      assert(sevenLong === (9L +- 2.toShort))
      assert(sevenLong === (8L +- 2.toShort))
      assert(sevenLong === (7L +- 2.toShort))
      assert(sevenLong === (6L +- 2.toShort))
      assert(sevenLong === (5L +- 2.toShort))
      assert(minusSevenLong === (-9L +- 2.toShort))
      assert(minusSevenLong === (-8L +- 2.toShort))
      assert(minusSevenLong === (-7L +- 2.toShort))
      assert(minusSevenLong === (-6L +- 2.toShort))
      assert(minusSevenLong === (-5L +- 2.toShort))

      // Long +- Byte
      assert(sevenLong === (9L +- 2.toByte))
      assert(sevenLong === (8L +- 2.toByte))
      assert(sevenLong === (7L +- 2.toByte))
      assert(sevenLong === (6L +- 2.toByte))
      assert(sevenLong === (5L +- 2.toByte))
      assert(minusSevenLong === (-9L +- 2.toByte))
      assert(minusSevenLong === (-8L +- 2.toByte))
      assert(minusSevenLong === (-7L +- 2.toByte))
      assert(minusSevenLong === (-6L +- 2.toByte))
      assert(minusSevenLong === (-5L +- 2.toByte))

      // Int +- Int
      assert(sevenInt === (9 +- 2))
      assert(sevenInt === (8 +- 2))
      assert(sevenInt === (7 +- 2))
      assert(sevenInt === (6 +- 2))
      assert(sevenInt === (5 +- 2))
      assert(minusSevenInt === (-9 +- 2))
      assert(minusSevenInt === (-8 +- 2))
      assert(minusSevenInt === (-7 +- 2))
      assert(minusSevenInt === (-6 +- 2))
      assert(minusSevenInt === (-5 +- 2))

      // Int +- Short
      assert(sevenInt === (9 +- 2.toShort))
      assert(sevenInt === (8 +- 2.toShort))
      assert(sevenInt === (7 +- 2.toShort))
      assert(sevenInt === (6 +- 2.toShort))
      assert(sevenInt === (5 +- 2.toShort))
      assert(minusSevenInt === (-9 +- 2.toShort))
      assert(minusSevenInt === (-8 +- 2.toShort))
      assert(minusSevenInt === (-7 +- 2.toShort))
      assert(minusSevenInt === (-6 +- 2.toShort))
      assert(minusSevenInt === (-5 +- 2.toShort))

      // Int +- Byte
      assert(sevenInt === (9 +- 2.toByte))
      assert(sevenInt === (8 +- 2.toByte))
      assert(sevenInt === (7 +- 2.toByte))
      assert(sevenInt === (6 +- 2.toByte))
      assert(sevenInt === (5 +- 2.toByte))
      assert(minusSevenInt === (-9 +- 2.toByte))
      assert(minusSevenInt === (-8 +- 2.toByte))
      assert(minusSevenInt === (-7 +- 2.toByte))
      assert(minusSevenInt === (-6 +- 2.toByte))
      assert(minusSevenInt === (-5 +- 2.toByte))

      // Short +- Short
      assert(sevenShort === (9.toShort +- 2.toShort))
      assert(sevenShort === (8.toShort +- 2.toShort))
      assert(sevenShort === (7.toShort +- 2.toShort))
      assert(sevenShort === (6.toShort +- 2.toShort))
      assert(sevenShort === (5.toShort +- 2.toShort))
      assert(minusSevenShort === ((-9).toShort +- 2.toShort))
      assert(minusSevenShort === ((-8).toShort +- 2.toShort))
      assert(minusSevenShort === ((-7).toShort +- 2.toShort))
      assert(minusSevenShort === ((-6).toShort +- 2.toShort))
      assert(minusSevenShort === ((-5).toShort +- 2.toShort))

      // Short +- Byte
      assert(sevenShort === (9.toShort +- 2.toByte))
      assert(sevenShort === (8.toShort +- 2.toByte))
      assert(sevenShort === (7.toShort +- 2.toByte))
      assert(sevenShort === (6.toShort +- 2.toByte))
      assert(sevenShort === (5.toShort +- 2.toByte))
      assert(minusSevenShort === ((-9).toShort +- 2.toByte))
      assert(minusSevenShort === ((-8).toShort +- 2.toByte))
      assert(minusSevenShort === ((-7).toShort +- 2.toByte))
      assert(minusSevenShort === ((-6).toShort +- 2.toByte))
      assert(minusSevenShort === ((-5).toShort +- 2.toByte))

      // Byte +- Byte
      assert(sevenByte === (9.toByte +- 2.toByte))
      assert(sevenByte === (8.toByte +- 2.toByte))
      assert(sevenByte === (7.toByte +- 2.toByte))
      assert(sevenByte === (6.toByte +- 2.toByte))
      assert(sevenByte === (5.toByte +- 2.toByte))
      assert(minusSevenByte === ((-9).toByte +- 2.toByte))
      assert(minusSevenByte === ((-8).toByte +- 2.toByte))
      assert(minusSevenByte === ((-7).toByte +- 2.toByte))
      assert(minusSevenByte === ((-6).toByte +- 2.toByte))
      assert(minusSevenByte === ((-5).toByte +- 2.toByte))
    }

    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side` {

      // Double +- Double
      assert((7.1 +- 0.2) === sevenDotOh)
      assert((6.9 +- 0.2) === sevenDotOh)
      assert((7.0 +- 0.2) === sevenDotOh)
      assert((7.2 +- 0.2) === sevenDotOh)
      assert((6.8 +- 0.2) === sevenDotOh)
      assert((-7.1 +- 0.2) === minusSevenDotOh)
      assert((-6.9 +- 0.2) === minusSevenDotOh)
      assert((-7.0 +- 0.2) === minusSevenDotOh)
      assert((-7.2 +- 0.2) === minusSevenDotOh)
      assert((-6.8 +- 0.2) === minusSevenDotOh)

      // Double +- Float
      assert((7.1 +- 0.2f) === sevenDotOh)
      assert((6.9 +- 0.2f) === sevenDotOh)
      assert((7.0 +- 0.2f) === sevenDotOh)
      assert((7.2 +- 0.2f) === sevenDotOh)
      assert((6.8 +- 0.2f) === sevenDotOh)
      assert((-7.1 +- 0.2f) === minusSevenDotOh)
      assert((-6.9 +- 0.2f) === minusSevenDotOh)
      assert((-7.0 +- 0.2f) === minusSevenDotOh)
      assert((-7.2 +- 0.2f) === minusSevenDotOh)
      assert((-6.8 +- 0.2f) === minusSevenDotOh)

      // Double +- Long
      assert((7.1 +- 2L) === sevenDotOh)
      assert((6.9 +- 2L) === sevenDotOh)
      assert((7.0 +- 2L) === sevenDotOh)
      assert((7.2 +- 2L) === sevenDotOh)
      assert((6.8 +- 2L) === sevenDotOh)
      assert((-7.1 +- 2L) === minusSevenDotOh)
      assert((-6.9 +- 2L) === minusSevenDotOh)
      assert((-7.0 +- 2L) === minusSevenDotOh)
      assert((-7.2 +- 2L) === minusSevenDotOh)
      assert((-6.8 +- 2L) === minusSevenDotOh)

      // Double +- Int
      assert((7.1 +- 2) === sevenDotOh)
      assert((6.9 +- 2) === sevenDotOh)
      assert((7.0 +- 2) === sevenDotOh)
      assert((7.2 +- 2) === sevenDotOh)
      assert((6.8 +- 2) === sevenDotOh)
      assert((-7.1 +- 2) === minusSevenDotOh)
      assert((-6.9 +- 2) === minusSevenDotOh)
      assert((-7.0 +- 2) === minusSevenDotOh)
      assert((-7.2 +- 2) === minusSevenDotOh)
      assert((-6.8 +- 2) === minusSevenDotOh)

      // Double +- Short
      assert((7.1 +- 2.toShort) === sevenDotOh)
      assert((6.9 +- 2.toShort) === sevenDotOh)
      assert((7.0 +- 2.toShort) === sevenDotOh)
      assert((7.2 +- 2.toShort) === sevenDotOh)
      assert((6.8 +- 2.toShort) === sevenDotOh)
      assert((-7.1 +- 2.toShort) === minusSevenDotOh)
      assert((-6.9 +- 2.toShort) === minusSevenDotOh)
      assert((-7.0 +- 2.toShort) === minusSevenDotOh)
      assert((-7.2 +- 2.toShort) === minusSevenDotOh)
      assert((-6.8 +- 2.toShort) === minusSevenDotOh)

      // Double +- Byte
      assert((7.1 +- 2.toByte) === sevenDotOh)
      assert((6.9 +- 2.toByte) === sevenDotOh)
      assert((7.0 +- 2.toByte) === sevenDotOh)
      assert((7.2 +- 2.toByte) === sevenDotOh)
      assert((6.8 +- 2.toByte) === sevenDotOh)
      assert((-7.1 +- 2.toByte) === minusSevenDotOh)
      assert((-6.9 +- 2.toByte) === minusSevenDotOh)
      assert((-7.0 +- 2.toByte) === minusSevenDotOh)
      assert((-7.2 +- 2.toByte) === minusSevenDotOh)
      assert((-6.8 +- 2.toByte) === minusSevenDotOh)

      // Float +- Float
      assert((7.1f +- 0.2f) === sevenDotOhFloat)
      assert((6.9f +- 0.2f) === sevenDotOhFloat)
      assert((7.0f +- 0.2f) === sevenDotOhFloat)
      assert((7.2f +- 0.2f) === sevenDotOhFloat)
      assert((6.8f +- 0.2f) === sevenDotOhFloat)
      assert((-7.1f +- 0.2f) === minusSevenDotOhFloat)
      assert((-6.9f +- 0.2f) === minusSevenDotOhFloat)
      assert((-7.0f +- 0.2f) === minusSevenDotOhFloat)
      assert((-7.2f +- 0.2f) === minusSevenDotOhFloat)
      assert((-6.8f +- 0.2f) === minusSevenDotOhFloat)

      // Float +- Long
      assert((7.1f +- 2L) === sevenDotOhFloat)
      assert((6.9f +- 2L) === sevenDotOhFloat)
      assert((7.0f +- 2L) === sevenDotOhFloat)
      assert((7.2f +- 2L) === sevenDotOhFloat)
      assert((6.8f +- 2L) === sevenDotOhFloat)
      assert((-7.1f +- 2L) === minusSevenDotOhFloat)
      assert((-6.9f +- 2L) === minusSevenDotOhFloat)
      assert((-7.0f +- 2L) === minusSevenDotOhFloat)
      assert((-7.2f +- 2L) === minusSevenDotOhFloat)
      assert((-6.8f +- 2L) === minusSevenDotOhFloat)

      // Float +- Int
      assert((7.1f +- 2) === sevenDotOhFloat)
      assert((6.9f +- 2) === sevenDotOhFloat)
      assert((7.0f +- 2) === sevenDotOhFloat)
      assert((7.2f +- 2) === sevenDotOhFloat)
      assert((6.8f +- 2) === sevenDotOhFloat)
      assert((-7.1f +- 2) === minusSevenDotOhFloat)
      assert((-6.9f +- 2) === minusSevenDotOhFloat)
      assert((-7.0f +- 2) === minusSevenDotOhFloat)
      assert((-7.2f +- 2) === minusSevenDotOhFloat)
      assert((-6.8f +- 2) === minusSevenDotOhFloat)

      // Float +- Short
      assert((7.1f +- 2.toShort) === sevenDotOhFloat)
      assert((6.9f +- 2.toShort) === sevenDotOhFloat)
      assert((7.0f +- 2.toShort) === sevenDotOhFloat)
      assert((7.2f +- 2.toShort) === sevenDotOhFloat)
      assert((6.8f +- 2.toShort) === sevenDotOhFloat)
      assert((-7.1f +- 2.toShort) === minusSevenDotOhFloat)
      assert((-6.9f +- 2.toShort) === minusSevenDotOhFloat)
      assert((-7.0f +- 2.toShort) === minusSevenDotOhFloat)
      assert((-7.2f +- 2.toShort) === minusSevenDotOhFloat)
      assert((-6.8f +- 2.toShort) === minusSevenDotOhFloat)

      // Float +- Byte
      assert((7.1f +- 2.toByte) === sevenDotOhFloat)
      assert((6.9f +- 2.toByte) === sevenDotOhFloat)
      assert((7.0f +- 2.toByte) === sevenDotOhFloat)
      assert((7.2f +- 2.toByte) === sevenDotOhFloat)
      assert((6.8f +- 2.toByte) === sevenDotOhFloat)
      assert((-7.1f +- 2.toByte) === minusSevenDotOhFloat)
      assert((-6.9f +- 2.toByte) === minusSevenDotOhFloat)
      assert((-7.0f +- 2.toByte) === minusSevenDotOhFloat)
      assert((-7.2f +- 2.toByte) === minusSevenDotOhFloat)
      assert((-6.8f +- 2.toByte) === minusSevenDotOhFloat)

      // Long +- Long
      assert((9L +- 2L) === sevenLong)
      assert((8L +- 2L) === sevenLong)
      assert((7L +- 2L) === sevenLong)
      assert((6L +- 2L) === sevenLong)
      assert((5L +- 2L) === sevenLong)
      assert((-9L +- 2L) === minusSevenLong)
      assert((-8L +- 2L) === minusSevenLong)
      assert((-7L +- 2L) === minusSevenLong)
      assert((-6L +- 2L) === minusSevenLong)
      assert((-5L +- 2L) === minusSevenLong)

      // Long +- Int
      assert((9L +- 2) === sevenLong)
      assert((8L +- 2) === sevenLong)
      assert((7L +- 2) === sevenLong)
      assert((6L +- 2) === sevenLong)
      assert((5L +- 2) === sevenLong)
      assert((-9L +- 2) === minusSevenLong)
      assert((-8L +- 2) === minusSevenLong)
      assert((-7L +- 2) === minusSevenLong)
      assert((-6L +- 2) === minusSevenLong)
      assert((-5L +- 2) === minusSevenLong)

      // Long +- Short
      assert((9L +- 2.toShort) === sevenLong)
      assert((8L +- 2.toShort) === sevenLong)
      assert((7L +- 2.toShort) === sevenLong)
      assert((6L +- 2.toShort) === sevenLong)
      assert((5L +- 2.toShort) === sevenLong)
      assert((-9L +- 2.toShort) === minusSevenLong)
      assert((-8L +- 2.toShort) === minusSevenLong)
      assert((-7L +- 2.toShort) === minusSevenLong)
      assert((-6L +- 2.toShort) === minusSevenLong)
      assert((-5L +- 2.toShort) === minusSevenLong)

      // Long +- Byte
      assert((9L +- 2.toByte) === sevenLong)
      assert((8L +- 2.toByte) === sevenLong)
      assert((7L +- 2.toByte) === sevenLong)
      assert((6L +- 2.toByte) === sevenLong)
      assert((5L +- 2.toByte) === sevenLong)
      assert((-9L +- 2.toByte) === minusSevenLong)
      assert((-8L +- 2.toByte) === minusSevenLong)
      assert((-7L +- 2.toByte) === minusSevenLong)
      assert((-6L +- 2.toByte) === minusSevenLong)
      assert((-5L +- 2.toByte) === minusSevenLong)

      // Int +- Int
      assert((9 +- 2) === sevenInt)
      assert((8 +- 2) === sevenInt)
      assert((7 +- 2) === sevenInt)
      assert((6 +- 2) === sevenInt)
      assert((5 +- 2) === sevenInt)
      assert((-9 +- 2) === minusSevenInt)
      assert((-8 +- 2) === minusSevenInt)
      assert((-7 +- 2) === minusSevenInt)
      assert((-6 +- 2) === minusSevenInt)
      assert((-5 +- 2) === minusSevenInt)

      // Int +- Short
      assert((9 +- 2.toShort) === sevenInt)
      assert((8 +- 2.toShort) === sevenInt)
      assert((7 +- 2.toShort) === sevenInt)
      assert((6 +- 2.toShort) === sevenInt)
      assert((5 +- 2.toShort) === sevenInt)
      assert((-9 +- 2.toShort) === minusSevenInt)
      assert((-8 +- 2.toShort) === minusSevenInt)
      assert((-7 +- 2.toShort) === minusSevenInt)
      assert((-6 +- 2.toShort) === minusSevenInt)
      assert((-5 +- 2.toShort) === minusSevenInt)

      // Int +- Byte
      assert((9 +- 2.toByte) === sevenInt)
      assert((8 +- 2.toByte) === sevenInt)
      assert((7 +- 2.toByte) === sevenInt)
      assert((6 +- 2.toByte) === sevenInt)
      assert((5 +- 2.toByte) === sevenInt)
      assert((-9 +- 2.toByte) === minusSevenInt)
      assert((-8 +- 2.toByte) === minusSevenInt)
      assert((-7 +- 2.toByte) === minusSevenInt)
      assert((-6 +- 2.toByte) === minusSevenInt)
      assert((-5 +- 2.toByte) === minusSevenInt)

      // Short +- Short
      assert((9.toShort +- 2.toShort) === sevenShort)
      assert((8.toShort +- 2.toShort) === sevenShort)
      assert((7.toShort +- 2.toShort) === sevenShort)
      assert((6.toShort +- 2.toShort) === sevenShort)
      assert((5.toShort +- 2.toShort) === sevenShort)
      assert(((-9).toShort +- 2.toShort) === minusSevenShort)
      assert(((-8).toShort +- 2.toShort) === minusSevenShort)
      assert(((-7).toShort +- 2.toShort) === minusSevenShort)
      assert(((-6).toShort +- 2.toShort) === minusSevenShort)
      assert(((-5).toShort +- 2.toShort) === minusSevenShort)

      // Short +- Byte
      assert((9.toShort +- 2.toByte) === sevenShort)
      assert((8.toShort +- 2.toByte) === sevenShort)
      assert((7.toShort +- 2.toByte) === sevenShort)
      assert((6.toShort +- 2.toByte) === sevenShort)
      assert((5.toShort +- 2.toByte) === sevenShort)
      assert(((-9).toShort +- 2.toByte) === minusSevenShort)
      assert(((-8).toShort +- 2.toByte) === minusSevenShort)
      assert(((-7).toShort +- 2.toByte) === minusSevenShort)
      assert(((-6).toShort +- 2.toByte) === minusSevenShort)
      assert(((-5).toShort +- 2.toByte) === minusSevenShort)

      // Byte +- Byte
      assert((9.toByte +- 2.toByte) === sevenByte)
      assert((8.toByte +- 2.toByte) === sevenByte)
      assert((7.toByte +- 2.toByte) === sevenByte)
      assert((6.toByte +- 2.toByte) === sevenByte)
      assert((5.toByte +- 2.toByte) === sevenByte)
      assert(((-9).toByte +- 2.toByte) === minusSevenByte)
      assert(((-8).toByte +- 2.toByte) === minusSevenByte)
      assert(((-7).toByte +- 2.toByte) === minusSevenByte)
      assert(((-6).toByte +- 2.toByte) === minusSevenByte)
      assert(((-5).toByte +- 2.toByte) === minusSevenByte)
    }

    def `should be false if the number is outside the given interval` {

      // Double +- Double
      assert(!(sevenDotOh === (7.5 +- 0.2)))
      assert(!(sevenDotOh === (6.5 +- 0.2)))
      assert(!(minusSevenDotOh === (-7.5 +- 0.2)))
      assert(!(minusSevenDotOh === (-6.5 +- 0.2)))

      // Double +- Float
      assert(!(sevenDotOh === (7.5 +- 0.2f)))
      assert(!(sevenDotOh === (6.5 +- 0.2f)))
      assert(!(minusSevenDotOh === (-7.5 +- 0.2f)))
      assert(!(minusSevenDotOh === (-6.5 +- 0.2f)))

      // Double +- Long
      assert(!(sevenDotOh === (4.0 +- 2L)))
      assert(!(sevenDotOh === (9.1 +- 2L)))
      assert(!(minusSevenDotOh === (-4.0 +- 2L)))
      assert(!(minusSevenDotOh === (-9.1 +- 2L)))

      // Double +- Int
      assert(!(sevenDotOh === (4.0 +- 2)))
      assert(!(sevenDotOh === (9.1 +- 2)))
      assert(!(minusSevenDotOh === (-4.0 +- 2)))
      assert(!(minusSevenDotOh === (-9.1 +- 2)))

      // Double +- Short
      assert(!(sevenDotOh === (4.0 +- 2.toShort)))
      assert(!(sevenDotOh === (9.1 +- 2.toShort)))
      assert(!(minusSevenDotOh === (-4.0 +- 2.toShort)))
      assert(!(minusSevenDotOh === (-9.1 +- 2.toShort)))

      // Double +- Byte
      assert(!(sevenDotOh === (4.0 +- 2.toByte)))
      assert(!(sevenDotOh === (9.1 +- 2.toByte)))
      assert(!(minusSevenDotOh === (-4.0 +- 2.toByte)))
      assert(!(minusSevenDotOh === (-9.1 +- 2.toByte)))

      // Float +- Float
      assert(!(sevenDotOhFloat === (7.5f +- 0.2f)))
      assert(!(sevenDotOhFloat === (6.5f +- 0.2f)))
      assert(!(minusSevenDotOhFloat === (-7.5f +- 0.2f)))
      assert(!(minusSevenDotOhFloat === (-6.5f +- 0.2f)))

      // Float +- Long
      assert(!(sevenDotOhFloat === (4.0f +- 2L)))
      assert(!(sevenDotOhFloat === (9.1f +- 2L)))
      assert(!(minusSevenDotOhFloat === (-4.0f +- 2L)))
      assert(!(minusSevenDotOhFloat === (-9.1f +- 2L)))

      // Float +- Int
      assert(!(sevenDotOhFloat === (4.0f +- 2)))
      assert(!(sevenDotOhFloat === (9.1f +- 2)))
      assert(!(minusSevenDotOhFloat === (-4.0f +- 2)))
      assert(!(minusSevenDotOhFloat === (-9.1f +- 2)))

      // Float +- Short
      assert(!(sevenDotOhFloat === (4.0f +- 2.toShort)))
      assert(!(sevenDotOhFloat === (9.1f +- 2.toShort)))
      assert(!(minusSevenDotOhFloat === (-4.0f +- 2.toShort)))
      assert(!(minusSevenDotOhFloat === (-9.1f +- 2.toShort)))

      // Float +- Byte
      assert(!(sevenDotOhFloat === (4.0f +- 2.toByte)))
      assert(!(sevenDotOhFloat === (9.1f +- 2.toByte)))
      assert(!(minusSevenDotOhFloat === (-4.0f +- 2.toByte)))
      assert(!(minusSevenDotOhFloat === (-9.1f +- 2.toByte)))

      // Long +- Long
      assert(!(sevenLong === (4L +- 2L)))
      assert(!(sevenLong === (10L +- 2L)))
      assert(!(minusSevenLong === (-4L +- 2L)))
      assert(!(minusSevenLong === (-10L +- 2L)))

      // Long +- Int
      assert(!(sevenLong === (4L +- 2)))
      assert(!(sevenLong === (10L +- 2)))
      assert(!(minusSevenLong === (-4L +- 2)))
      assert(!(minusSevenLong === (-10L +- 2)))

      // Long +- Short
      assert(!(sevenLong === (4L +- 2.toShort)))
      assert(!(sevenLong === (10L +- 2.toShort)))
      assert(!(minusSevenLong === (-4L +- 2.toShort)))
      assert(!(minusSevenLong === (-10L +- 2.toShort)))

      // Long +- Byte
      assert(!(sevenLong === (4L +- 2.toByte)))
      assert(!(sevenLong === (10L +- 2.toByte)))
      assert(!(minusSevenLong === (-4L +- 2.toByte)))
      assert(!(minusSevenLong === (-10L +- 2.toByte)))

      // Int +- Int
      assert(!(sevenInt === (4 +- 2)))
      assert(!(sevenInt === (10 +- 2)))
      assert(!(minusSevenInt === (-4 +- 2)))
      assert(!(minusSevenInt === (-10 +- 2)))

      // Int +- Short
      assert(!(sevenInt === (4 +- 2.toShort)))
      assert(!(sevenInt === (10 +- 2.toShort)))
      assert(!(minusSevenInt === (-4 +- 2.toShort)))
      assert(!(minusSevenInt === (-10 +- 2.toShort)))

      // Int +- Byte
      assert(!(sevenInt === (4 +- 2.toByte)))
      assert(!(sevenInt === (10 +- 2.toByte)))
      assert(!(minusSevenInt === (-4 +- 2.toByte)))
      assert(!(minusSevenInt === (-10 +- 2.toByte)))

      // Short +- Short
      assert(!(sevenShort === (4.toShort +- 2.toShort)))
      assert(!(sevenShort === (10.toShort +- 2.toShort)))
      assert(!(minusSevenShort === ((-4).toShort +- 2.toShort)))
      assert(!(minusSevenShort === ((-10).toShort +- 2.toShort)))

      // Short +- Byte
      assert(!(sevenShort === (4.toShort +- 2.toByte)))
      assert(!(sevenShort === (10.toShort +- 2.toByte)))
      assert(!(minusSevenShort === ((-4).toShort +- 2.toByte)))
      assert(!(minusSevenShort === ((-10).toShort +- 2.toByte)))

      // Byte +- Byte
      assert(!(sevenByte === (4.toByte +- 2.toByte)))
      assert(!(sevenByte === (10.toByte +- 2.toByte)))
      assert(!(minusSevenByte === ((-4).toByte +- 2.toByte)))
      assert(!(minusSevenByte === ((-10).toByte +- 2.toByte)))
    }

    def `should, for symmetry, be false if the number is outside the given interval, when the interval is on the left hand side` {

      // Double +- Double
      assert(!((7.5 +- 0.2) === sevenDotOh))
      assert(!((6.5 +- 0.2) === sevenDotOh))
      assert(!((-7.5 +- 0.2) === minusSevenDotOh))
      assert(!((-6.5 +- 0.2) === minusSevenDotOh))

      // Double +- Float
      assert(!((7.5 +- 0.2f) === sevenDotOh))
      assert(!((6.5 +- 0.2f) === sevenDotOh))
      assert(!((-7.5 +- 0.2f) === minusSevenDotOh))
      assert(!((-6.5 +- 0.2f) === minusSevenDotOh))

      // Double +- Long
      assert(!((4.0 +- 2L) === sevenDotOh))
      assert(!((9.1 +- 2L) === sevenDotOh))
      assert(!((-4.0 +- 2L) === minusSevenDotOh))
      assert(!((-9.1 +- 2L) === minusSevenDotOh))

      // Double +- Int
      assert(!((4.0 +- 2) === sevenDotOh))
      assert(!((9.1 +- 2) === sevenDotOh))
      assert(!((-4.0 +- 2) === minusSevenDotOh))
      assert(!((-9.1 +- 2) === minusSevenDotOh))

      // Double +- Short
      assert(!((4.0 +- 2.toShort) === sevenDotOh))
      assert(!((9.1 +- 2.toShort) === sevenDotOh))
      assert(!((-4.0 +- 2.toShort) === minusSevenDotOh))
      assert(!((-9.1 +- 2.toShort) === minusSevenDotOh))

      // Double +- Byte
      assert(!((4.0 +- 2.toByte) === sevenDotOh))
      assert(!((9.1 +- 2.toByte) === sevenDotOh))
      assert(!((-4.0 +- 2.toByte) === minusSevenDotOh))
      assert(!((-9.1 +- 2.toByte) === minusSevenDotOh))

      // Float +- Float
      assert(!((7.5f +- 0.2f) === sevenDotOhFloat))
      assert(!((6.5f +- 0.2f) === sevenDotOhFloat))
      assert(!((-7.5f +- 0.2f) === minusSevenDotOhFloat))
      assert(!((-6.5f +- 0.2f) === minusSevenDotOhFloat))

      // Float +- Long
      assert(!((4.0f +- 2L) === sevenDotOhFloat))
      assert(!((9.1f +- 2L) === sevenDotOhFloat))
      assert(!((-4.0f +- 2L) === minusSevenDotOhFloat))
      assert(!((-9.1f +- 2L) === minusSevenDotOhFloat))

      // Float +- Int
      assert(!((4.0f +- 2) === sevenDotOhFloat))
      assert(!((9.1f +- 2) === sevenDotOhFloat))
      assert(!((-4.0f +- 2) === minusSevenDotOhFloat))
      assert(!((-9.1f +- 2) === minusSevenDotOhFloat))

      // Float +- Short
      assert(!((4.0f +- 2.toShort) === sevenDotOhFloat))
      assert(!((9.1f +- 2.toShort) === sevenDotOhFloat))
      assert(!((-4.0f +- 2.toShort) === minusSevenDotOhFloat))
      assert(!((-9.1f +- 2.toShort) === minusSevenDotOhFloat))

      // Float +- Byte
      assert(!((4.0f +- 2.toByte) === sevenDotOhFloat))
      assert(!((9.1f +- 2.toByte) === sevenDotOhFloat))
      assert(!((-4.0f +- 2.toByte) === minusSevenDotOhFloat))
      assert(!((-9.1f +- 2.toByte) === minusSevenDotOhFloat))

      // Long +- Long
      assert(!((4L +- 2L) === sevenLong))
      assert(!((10L +- 2L) === sevenLong))
      assert(!((-4L +- 2L) === minusSevenLong))
      assert(!((-10L +- 2L) === minusSevenLong))

      // Long +- Int
      assert(!((4L +- 2) === sevenLong))
      assert(!((10L +- 2) === sevenLong))
      assert(!((-4L +- 2) === minusSevenLong))
      assert(!((-10L +- 2) === minusSevenLong))

      // Long +- Short
      assert(!((4L +- 2.toShort) === sevenLong))
      assert(!((10L +- 2.toShort) === sevenLong))
      assert(!((-4L +- 2.toShort) === minusSevenLong))
      assert(!((-10L +- 2.toShort) === minusSevenLong))

      // Long +- Byte
      assert(!((4L +- 2.toByte) === sevenLong))
      assert(!((10L +- 2.toByte) === sevenLong))
      assert(!((-4L +- 2.toByte) === minusSevenLong))
      assert(!((-10L +- 2.toByte) === minusSevenLong))

      // Int +- Int
      assert(!((4 +- 2) === sevenInt))
      assert(!((10 +- 2) === sevenInt))
      assert(!((-4 +- 2) === minusSevenInt))
      assert(!((-10 +- 2) === minusSevenInt))

      // Int +- Short
      assert(!((4 +- 2.toShort) === sevenInt))
      assert(!((10 +- 2.toShort) === sevenInt))
      assert(!((-4 +- 2.toShort) === minusSevenInt))
      assert(!((-10 +- 2.toShort) === minusSevenInt))

      // Int +- Byte
      assert(!((4 +- 2.toByte) === sevenInt))
      assert(!((10 +- 2.toByte) === sevenInt))
      assert(!((-4 +- 2.toByte) === minusSevenInt))
      assert(!((-10 +- 2.toByte) === minusSevenInt))

      // Short +- Short
      assert(!((4.toShort +- 2.toShort) === sevenShort))
      assert(!((10.toShort +- 2.toShort) === sevenShort))
      assert(!(((-4).toShort +- 2.toShort) === minusSevenShort))
      assert(!(((-10).toShort +- 2.toShort) === minusSevenShort))

      // Short +- Byte
      assert(!((4.toShort +- 2.toByte) === sevenShort))
      assert(!((10.toShort +- 2.toByte) === sevenShort))
      assert(!(((-4).toShort +- 2.toByte) === minusSevenShort))
      assert(!(((-10).toShort +- 2.toByte) === minusSevenShort))

      // Byte +- Byte
      assert(!((4.toByte +- 2.toByte) === sevenByte))
      assert(!((10.toByte +- 2.toByte) === sevenByte))
      assert(!(((-4).toByte +- 2.toByte) === minusSevenByte))
      assert(!(((-10).toByte +- 2.toByte) === minusSevenByte))
    }
  }

  object `The !== syntax` {

    def `should be true if the number is outside the given interval` {

      // Double +- Double
      assert(sevenDotOh !== (7.5 +- 0.2))
      assert(sevenDotOh !== (6.5 +- 0.2))
      assert(minusSevenDotOh !== (-7.5 +- 0.2))
      assert(minusSevenDotOh !== (-6.5 +- 0.2))

      // Double +- Float
      assert(sevenDotOh !== (7.5 +- 0.2f))
      assert(sevenDotOh !== (6.5 +- 0.2f))
      assert(minusSevenDotOh !== (-7.5 +- 0.2f))
      assert(minusSevenDotOh !== (-6.5 +- 0.2f))

      // Double +- Long
      assert(sevenDotOh !== (4.0 +- 2L))
      assert(sevenDotOh !== (9.1 +- 2L))
      assert(minusSevenDotOh !== (-4.0 +- 2L))
      assert(minusSevenDotOh !== (-9.1 +- 2L))

      // Double +- Int
      assert(sevenDotOh !== (4.0 +- 2))
      assert(sevenDotOh !== (9.1 +- 2))
      assert(minusSevenDotOh !== (-4.0 +- 2))
      assert(minusSevenDotOh !== (-9.1 +- 2))

      // Double +- Short
      assert(sevenDotOh !== (4.0 +- 2.toShort))
      assert(sevenDotOh !== (9.1 +- 2.toShort))
      assert(minusSevenDotOh !== (-4.0 +- 2.toShort))
      assert(minusSevenDotOh !== (-9.1 +- 2.toShort))

      // Double +- Byte
      assert(sevenDotOh !== (4.0 +- 2.toByte))
      assert(sevenDotOh !== (9.1 +- 2.toByte))
      assert(minusSevenDotOh !== (-4.0 +- 2.toByte))
      assert(minusSevenDotOh !== (-9.1 +- 2.toByte))

      // Float +- Float
      assert(sevenDotOhFloat !== (7.5f +- 0.2f))
      assert(sevenDotOhFloat !== (6.5f +- 0.2f))
      assert(minusSevenDotOhFloat !== (-7.5f +- 0.2f))
      assert(minusSevenDotOhFloat !== (-6.5f +- 0.2f))

      // Float +- Long
      assert(sevenDotOhFloat !== (4.0f +- 2L))
      assert(sevenDotOhFloat !== (9.1f +- 2L))
      assert(minusSevenDotOhFloat !== (-4.0f +- 2L))
      assert(minusSevenDotOhFloat !== (-9.1f +- 2L))

      // Float +- Int
      assert(sevenDotOhFloat !== (4.0f +- 2))
      assert(sevenDotOhFloat !== (9.1f +- 2))
      assert(minusSevenDotOhFloat !== (-4.0f +- 2))
      assert(minusSevenDotOhFloat !== (-9.1f +- 2))

      // Float +- Short
      assert(sevenDotOhFloat !== (4.0f +- 2.toShort))
      assert(sevenDotOhFloat !== (9.1f +- 2.toShort))
      assert(minusSevenDotOhFloat !== (-4.0f +- 2.toShort))
      assert(minusSevenDotOhFloat !== (-9.1f +- 2.toShort))

      // Float +- Byte
      assert(sevenDotOhFloat !== (4.0f +- 2.toByte))
      assert(sevenDotOhFloat !== (9.1f +- 2.toByte))
      assert(minusSevenDotOhFloat !== (-4.0f +- 2.toByte))
      assert(minusSevenDotOhFloat !== (-9.1f +- 2.toByte))

      // Long +- Long
      assert(sevenLong !== (4L +- 2L))
      assert(sevenLong !== (10L +- 2L))
      assert(minusSevenLong !== (-4L +- 2L))
      assert(minusSevenLong !== (-10L +- 2L))

      // Long +- Int
      assert(sevenLong !== (4L +- 2))
      assert(sevenLong !== (10L +- 2))
      assert(minusSevenLong !== (-4L +- 2))
      assert(minusSevenLong !== (-10L +- 2))

      // Long +- Short
      assert(sevenLong !== (4L +- 2.toShort))
      assert(sevenLong !== (10L +- 2.toShort))
      assert(minusSevenLong !== (-4L +- 2.toShort))
      assert(minusSevenLong !== (-10L +- 2.toShort))

      // Long +- Byte
      assert(sevenLong !== (4L +- 2.toByte))
      assert(sevenLong !== (10L +- 2.toByte))
      assert(minusSevenLong !== (-4L +- 2.toByte))
      assert(minusSevenLong !== (-10L +- 2.toByte))

      // Int +- Int
      assert(sevenInt !== (4 +- 2))
      assert(sevenInt !== (10 +- 2))
      assert(minusSevenInt !== (-4 +- 2))
      assert(minusSevenInt !== (-10 +- 2))

      // Int +- Short
      assert(sevenInt !== (4 +- 2.toShort))
      assert(sevenInt !== (10 +- 2.toShort))
      assert(minusSevenInt !== (-4 +- 2.toShort))
      assert(minusSevenInt !== (-10 +- 2.toShort))

      // Int +- Byte
      assert(sevenInt !== (4 +- 2.toByte))
      assert(sevenInt !== (10 +- 2.toByte))
      assert(minusSevenInt !== (-4 +- 2.toByte))
      assert(minusSevenInt !== (-10 +- 2.toByte))

      // Short +- Short
      assert(sevenShort !== (4.toShort +- 2.toShort))
      assert(sevenShort !== (10.toShort +- 2.toShort))
      assert(minusSevenShort !== ((-4).toShort +- 2.toShort))
      assert(minusSevenShort !== ((-10).toShort +- 2.toShort))

      // Short +- Byte
      assert(sevenShort !== (4.toShort +- 2.toByte))
      assert(sevenShort !== (10.toShort +- 2.toByte))
      assert(minusSevenShort !== ((-4).toShort +- 2.toByte))
      assert(minusSevenShort !== ((-10).toShort +- 2.toByte))

      // Byte +- Byte
      assert(sevenByte !== (4.toByte +- 2.toByte))
      assert(sevenByte !== (10.toByte +- 2.toByte))
      assert(minusSevenByte !== ((-4).toByte +- 2.toByte))
      assert(minusSevenByte !== ((-10).toByte +- 2.toByte))
    }

    def `should, for symmetry, be true if the number is outside the given interval when the interval is placed on the left hand side` {

      // Double +- Double
      assert((7.5 +- 0.2) !== sevenDotOh)
      assert((6.5 +- 0.2) !== sevenDotOh)
      assert((-7.5 +- 0.2) !== minusSevenDotOh)
      assert((-6.5 +- 0.2) !== minusSevenDotOh)

      // Double +- Float
      assert((7.5 +- 0.2f) !== sevenDotOh)
      assert((6.5 +- 0.2f) !== sevenDotOh)
      assert((-7.5 +- 0.2f) !== minusSevenDotOh)
      assert((-6.5 +- 0.2f) !== minusSevenDotOh)

      // Double +- Long
      assert((4.0 +- 2L) !== sevenDotOh)
      assert((9.1 +- 2L) !== sevenDotOh)
      assert((-4.0 +- 2L) !== minusSevenDotOh)
      assert((-9.1 +- 2L) !== minusSevenDotOh)

      // Double +- Int
      assert((4.0 +- 2) !== sevenDotOh)
      assert((9.1 +- 2) !== sevenDotOh)
      assert((-4.0 +- 2) !== minusSevenDotOh)
      assert((-9.1 +- 2) !== minusSevenDotOh)

      // Double +- Short
      assert((4.0 +- 2.toShort) !== sevenDotOh)
      assert((9.1 +- 2.toShort) !== sevenDotOh)
      assert((-4.0 +- 2.toShort) !== minusSevenDotOh)
      assert((-9.1 +- 2.toShort) !== minusSevenDotOh)

      // Double +- Byte
      assert((4.0 +- 2.toByte) !== sevenDotOh)
      assert((9.1 +- 2.toByte) !== sevenDotOh)
      assert((-4.0 +- 2.toByte) !== minusSevenDotOh)
      assert((-9.1 +- 2.toByte) !== minusSevenDotOh)

      // Float +- Float
      assert((7.5f +- 0.2f) !== sevenDotOhFloat)
      assert((6.5f +- 0.2f) !== sevenDotOhFloat)
      assert((-7.5f +- 0.2f) !== minusSevenDotOhFloat)
      assert((-6.5f +- 0.2f) !== minusSevenDotOhFloat)

      // Float +- Long
      assert((4.0f +- 2L) !== sevenDotOhFloat)
      assert((9.1f +- 2L) !== sevenDotOhFloat)
      assert((-4.0f +- 2L) !== minusSevenDotOhFloat)
      assert((-9.1f +- 2L) !== minusSevenDotOhFloat)

      // Float +- Int
      assert((4.0f +- 2) !== sevenDotOhFloat)
      assert((9.1f +- 2) !== sevenDotOhFloat)
      assert((-4.0f +- 2) !== minusSevenDotOhFloat)
      assert((-9.1f +- 2) !== minusSevenDotOhFloat)

      // Float +- Short
      assert((4.0f +- 2.toShort) !== sevenDotOhFloat)
      assert((9.1f +- 2.toShort) !== sevenDotOhFloat)
      assert((-4.0f +- 2.toShort) !== minusSevenDotOhFloat)
      assert((-9.1f +- 2.toShort) !== minusSevenDotOhFloat)

      // Float +- Byte
      assert((4.0f +- 2.toByte) !== sevenDotOhFloat)
      assert((9.1f +- 2.toByte) !== sevenDotOhFloat)
      assert((-4.0f +- 2.toByte) !== minusSevenDotOhFloat)
      assert((-9.1f +- 2.toByte) !== minusSevenDotOhFloat)

      // Long +- Long
      assert((4L +- 2L) !== sevenLong)
      assert((10L +- 2L) !== sevenLong)
      assert((-4L +- 2L) !== minusSevenLong)
      assert((-10L +- 2L) !== minusSevenLong)

      // Long +- Int
      assert((4L +- 2) !== sevenLong)
      assert((10L +- 2) !== sevenLong)
      assert((-4L +- 2) !== minusSevenLong)
      assert((-10L +- 2) !== minusSevenLong)

      // Long +- Short
      assert((4L +- 2.toShort) !== sevenLong)
      assert((10L +- 2.toShort) !== sevenLong)
      assert((-4L +- 2.toShort) !== minusSevenLong)
      assert((-10L +- 2.toShort) !== minusSevenLong)

      // Long +- Byte
      assert((4L +- 2.toByte) !== sevenLong)
      assert((10L +- 2.toByte) !== sevenLong)
      assert((-4L +- 2.toByte) !== minusSevenLong)
      assert((-10L +- 2.toByte) !== minusSevenLong)

      // Int +- Int
      assert((4 +- 2) !== sevenInt)
      assert((10 +- 2) !== sevenInt)
      assert((-4 +- 2) !== minusSevenInt)
      assert((-10 +- 2) !== minusSevenInt)

      // Int +- Short
      assert((4 +- 2.toShort) !== sevenInt)
      assert((10 +- 2.toShort) !== sevenInt)
      assert((-4 +- 2.toShort) !== minusSevenInt)
      assert((-10 +- 2.toShort) !== minusSevenInt)

      // Int +- Byte
      assert((4 +- 2.toByte) !== sevenInt)
      assert((10 +- 2.toByte) !== sevenInt)
      assert((-4 +- 2.toByte) !== minusSevenInt)
      assert((-10 +- 2.toByte) !== minusSevenInt)

      // Short +- Short
      assert((4.toShort +- 2.toShort) !== sevenShort)
      assert((10.toShort +- 2.toShort) !== sevenShort)
      assert(((-4).toShort +- 2.toShort) !== minusSevenShort)
      assert(((-10).toShort +- 2.toShort) !== minusSevenShort)

      // Short +- Byte
      assert((4.toShort +- 2.toByte) !== sevenShort)
      assert((10.toShort +- 2.toByte) !== sevenShort)
      assert(((-4).toShort +- 2.toByte) !== minusSevenShort)
      assert(((-10).toShort +- 2.toByte) !== minusSevenShort)

      // Byte +- Byte
      assert((4.toByte +- 2.toByte) !== sevenByte)
      assert((10.toByte +- 2.toByte) !== sevenByte)
      assert(((-4).toByte +- 2.toByte) !== minusSevenByte)
      assert(((-10).toByte +- 2.toByte) !== minusSevenByte)
    }

    def `should be false if the number is within the given interval` {

      // Double +- Double
      assert(!(sevenDotOh !== (7.1 +- 0.2)))
      assert(!(sevenDotOh !== (6.9 +- 0.2)))
      assert(!(sevenDotOh !== (7.0 +- 0.2)))
      assert(!(sevenDotOh !== (7.2 +- 0.2)))
      assert(!(sevenDotOh !== (6.8 +- 0.2)))
      assert(!(minusSevenDotOh !== (-7.1 +- 0.2)))
      assert(!(minusSevenDotOh !== (-6.9 +- 0.2)))
      assert(!(minusSevenDotOh !== (-7.0 +- 0.2)))
      assert(!(minusSevenDotOh !== (-7.2 +- 0.2)))
      assert(!(minusSevenDotOh !== (-6.8 +- 0.2)))

      // Double +- Float
      assert(!(sevenDotOh !== (7.1 +- 0.2f)))
      assert(!(sevenDotOh !== (6.9 +- 0.2f)))
      assert(!(sevenDotOh !== (7.0 +- 0.2f)))
      assert(!(sevenDotOh !== (7.2 +- 0.2f)))
      assert(!(sevenDotOh !== (6.8 +- 0.2f)))
      assert(!(minusSevenDotOh !== (-7.1 +- 0.2f)))
      assert(!(minusSevenDotOh !== (-6.9 +- 0.2f)))
      assert(!(minusSevenDotOh !== (-7.0 +- 0.2f)))
      assert(!(minusSevenDotOh !== (-7.2 +- 0.2f)))
      assert(!(minusSevenDotOh !== (-6.8 +- 0.2f)))

      // Double +- Long
      assert(!(sevenDotOh !== (7.1 +- 2L)))
      assert(!(sevenDotOh !== (6.9 +- 2L)))
      assert(!(sevenDotOh !== (7.0 +- 2L)))
      assert(!(sevenDotOh !== (7.2 +- 2L)))
      assert(!(sevenDotOh !== (6.8 +- 2L)))
      assert(!(minusSevenDotOh !== (-7.1 +- 2L)))
      assert(!(minusSevenDotOh !== (-6.9 +- 2L)))
      assert(!(minusSevenDotOh !== (-7.0 +- 2L)))
      assert(!(minusSevenDotOh !== (-7.2 +- 2L)))
      assert(!(minusSevenDotOh !== (-6.8 +- 2L)))

      // Double +- Int
      assert(!(sevenDotOh !== (7.1 +- 2)))
      assert(!(sevenDotOh !== (6.9 +- 2)))
      assert(!(sevenDotOh !== (7.0 +- 2)))
      assert(!(sevenDotOh !== (7.2 +- 2)))
      assert(!(sevenDotOh !== (6.8 +- 2)))
      assert(!(minusSevenDotOh !== (-7.1 +- 2)))
      assert(!(minusSevenDotOh !== (-6.9 +- 2)))
      assert(!(minusSevenDotOh !== (-7.0 +- 2)))
      assert(!(minusSevenDotOh !== (-7.2 +- 2)))
      assert(!(minusSevenDotOh !== (-6.8 +- 2)))

      // Double +- Short
      assert(!(sevenDotOh !== (7.1 +- 2.toShort)))
      assert(!(sevenDotOh !== (6.9 +- 2.toShort)))
      assert(!(sevenDotOh !== (7.0 +- 2.toShort)))
      assert(!(sevenDotOh !== (7.2 +- 2.toShort)))
      assert(!(sevenDotOh !== (6.8 +- 2.toShort)))
      assert(!(minusSevenDotOh !== (-7.1 +- 2.toShort)))
      assert(!(minusSevenDotOh !== (-6.9 +- 2.toShort)))
      assert(!(minusSevenDotOh !== (-7.0 +- 2.toShort)))
      assert(!(minusSevenDotOh !== (-7.2 +- 2.toShort)))
      assert(!(minusSevenDotOh !== (-6.8 +- 2.toShort)))

      // Double +- Byte
      assert(!(sevenDotOh !== (7.1 +- 2.toByte)))
      assert(!(sevenDotOh !== (6.9 +- 2.toByte)))
      assert(!(sevenDotOh !== (7.0 +- 2.toByte)))
      assert(!(sevenDotOh !== (7.2 +- 2.toByte)))
      assert(!(sevenDotOh !== (6.8 +- 2.toByte)))
      assert(!(minusSevenDotOh !== (-7.1 +- 2.toByte)))
      assert(!(minusSevenDotOh !== (-6.9 +- 2.toByte)))
      assert(!(minusSevenDotOh !== (-7.0 +- 2.toByte)))
      assert(!(minusSevenDotOh !== (-7.2 +- 2.toByte)))
      assert(!(minusSevenDotOh !== (-6.8 +- 2.toByte)))

      // Float +- Float
      assert(!(sevenDotOhFloat !== (7.1f +- 0.2f)))
      assert(!(sevenDotOhFloat !== (6.9f +- 0.2f)))
      assert(!(sevenDotOhFloat !== (7.0f +- 0.2f)))
      assert(!(sevenDotOhFloat !== (7.2f +- 0.2f)))
      assert(!(sevenDotOhFloat !== (6.8f +- 0.2f)))
      assert(!(minusSevenDotOhFloat !== (-7.1f +- 0.2f)))
      assert(!(minusSevenDotOhFloat !== (-6.9f +- 0.2f)))
      assert(!(minusSevenDotOhFloat !== (-7.0f +- 0.2f)))
      assert(!(minusSevenDotOhFloat !== (-7.2f +- 0.2f)))
      assert(!(minusSevenDotOhFloat !== (-6.8f +- 0.2f)))

      // Float +- Long
      assert(!(sevenDotOhFloat !== (7.1f +- 2L)))
      assert(!(sevenDotOhFloat !== (6.9f +- 2L)))
      assert(!(sevenDotOhFloat !== (7.0f +- 2L)))
      assert(!(sevenDotOhFloat !== (7.2f +- 2L)))
      assert(!(sevenDotOhFloat !== (6.8f +- 2L)))
      assert(!(minusSevenDotOhFloat !== (-7.1f +- 2L)))
      assert(!(minusSevenDotOhFloat !== (-6.9f +- 2L)))
      assert(!(minusSevenDotOhFloat !== (-7.0f +- 2L)))
      assert(!(minusSevenDotOhFloat !== (-7.2f +- 2L)))
      assert(!(minusSevenDotOhFloat !== (-6.8f +- 2L)))

      // Float +- Int
      assert(!(sevenDotOhFloat !== (7.1f +- 2)))
      assert(!(sevenDotOhFloat !== (6.9f +- 2)))
      assert(!(sevenDotOhFloat !== (7.0f +- 2)))
      assert(!(sevenDotOhFloat !== (7.2f +- 2)))
      assert(!(sevenDotOhFloat !== (6.8f +- 2)))
      assert(!(minusSevenDotOhFloat !== (-7.1f +- 2)))
      assert(!(minusSevenDotOhFloat !== (-6.9f +- 2)))
      assert(!(minusSevenDotOhFloat !== (-7.0f +- 2)))
      assert(!(minusSevenDotOhFloat !== (-7.2f +- 2)))
      assert(!(minusSevenDotOhFloat !== (-6.8f +- 2)))

      // Float +- Short
      assert(!(sevenDotOhFloat !== (7.1f +- 2.toShort)))
      assert(!(sevenDotOhFloat !== (6.9f +- 2.toShort)))
      assert(!(sevenDotOhFloat !== (7.0f +- 2.toShort)))
      assert(!(sevenDotOhFloat !== (7.2f +- 2.toShort)))
      assert(!(sevenDotOhFloat !== (6.8f +- 2.toShort)))
      assert(!(minusSevenDotOhFloat !== (-7.1f +- 2.toShort)))
      assert(!(minusSevenDotOhFloat !== (-6.9f +- 2.toShort)))
      assert(!(minusSevenDotOhFloat !== (-7.0f +- 2.toShort)))
      assert(!(minusSevenDotOhFloat !== (-7.2f +- 2.toShort)))
      assert(!(minusSevenDotOhFloat !== (-6.8f +- 2.toShort)))

      // Float +- Byte
      assert(!(sevenDotOhFloat !== (7.1f +- 2.toByte)))
      assert(!(sevenDotOhFloat !== (6.9f +- 2.toByte)))
      assert(!(sevenDotOhFloat !== (7.0f +- 2.toByte)))
      assert(!(sevenDotOhFloat !== (7.2f +- 2.toByte)))
      assert(!(sevenDotOhFloat !== (6.8f +- 2.toByte)))
      assert(!(minusSevenDotOhFloat !== (-7.1f +- 2.toByte)))
      assert(!(minusSevenDotOhFloat !== (-6.9f +- 2.toByte)))
      assert(!(minusSevenDotOhFloat !== (-7.0f +- 2.toByte)))
      assert(!(minusSevenDotOhFloat !== (-7.2f +- 2.toByte)))
      assert(!(minusSevenDotOhFloat !== (-6.8f +- 2.toByte)))

      // Long +- Long
      assert(!(sevenLong !== (9L +- 2L)))
      assert(!(sevenLong !== (8L +- 2L)))
      assert(!(sevenLong !== (7L +- 2L)))
      assert(!(sevenLong !== (6L +- 2L)))
      assert(!(sevenLong !== (5L +- 2L)))
      assert(!(minusSevenLong !== (-9L +- 2L)))
      assert(!(minusSevenLong !== (-8L +- 2L)))
      assert(!(minusSevenLong !== (-7L +- 2L)))
      assert(!(minusSevenLong !== (-6L +- 2L)))
      assert(!(minusSevenLong !== (-5L +- 2L)))

      // Long +- Int
      assert(!(sevenLong !== (9L +- 2)))
      assert(!(sevenLong !== (8L +- 2)))
      assert(!(sevenLong !== (7L +- 2)))
      assert(!(sevenLong !== (6L +- 2)))
      assert(!(sevenLong !== (5L +- 2)))
      assert(!(minusSevenLong !== (-9L +- 2)))
      assert(!(minusSevenLong !== (-8L +- 2)))
      assert(!(minusSevenLong !== (-7L +- 2)))
      assert(!(minusSevenLong !== (-6L +- 2)))
      assert(!(minusSevenLong !== (-5L +- 2)))

      // Long +- Short
      assert(!(sevenLong !== (9L +- 2.toShort)))
      assert(!(sevenLong !== (8L +- 2.toShort)))
      assert(!(sevenLong !== (7L +- 2.toShort)))
      assert(!(sevenLong !== (6L +- 2.toShort)))
      assert(!(sevenLong !== (5L +- 2.toShort)))
      assert(!(minusSevenLong !== (-9L +- 2.toShort)))
      assert(!(minusSevenLong !== (-8L +- 2.toShort)))
      assert(!(minusSevenLong !== (-7L +- 2.toShort)))
      assert(!(minusSevenLong !== (-6L +- 2.toShort)))
      assert(!(minusSevenLong !== (-5L +- 2.toShort)))

      // Long +- Byte
      assert(!(sevenLong !== (9L +- 2.toByte)))
      assert(!(sevenLong !== (8L +- 2.toByte)))
      assert(!(sevenLong !== (7L +- 2.toByte)))
      assert(!(sevenLong !== (6L +- 2.toByte)))
      assert(!(sevenLong !== (5L +- 2.toByte)))
      assert(!(minusSevenLong !== (-9L +- 2.toByte)))
      assert(!(minusSevenLong !== (-8L +- 2.toByte)))
      assert(!(minusSevenLong !== (-7L +- 2.toByte)))
      assert(!(minusSevenLong !== (-6L +- 2.toByte)))
      assert(!(minusSevenLong !== (-5L +- 2.toByte)))

      // Int +- Int
      assert(!(sevenInt !== (9 +- 2)))
      assert(!(sevenInt !== (8 +- 2)))
      assert(!(sevenInt !== (7 +- 2)))
      assert(!(sevenInt !== (6 +- 2)))
      assert(!(sevenInt !== (5 +- 2)))
      assert(!(minusSevenInt !== (-9 +- 2)))
      assert(!(minusSevenInt !== (-8 +- 2)))
      assert(!(minusSevenInt !== (-7 +- 2)))
      assert(!(minusSevenInt !== (-6 +- 2)))
      assert(!(minusSevenInt !== (-5 +- 2)))

      // Int +- Short
      assert(!(sevenInt !== (9 +- 2.toShort)))
      assert(!(sevenInt !== (8 +- 2.toShort)))
      assert(!(sevenInt !== (7 +- 2.toShort)))
      assert(!(sevenInt !== (6 +- 2.toShort)))
      assert(!(sevenInt !== (5 +- 2.toShort)))
      assert(!(minusSevenInt !== (-9 +- 2.toShort)))
      assert(!(minusSevenInt !== (-8 +- 2.toShort)))
      assert(!(minusSevenInt !== (-7 +- 2.toShort)))
      assert(!(minusSevenInt !== (-6 +- 2.toShort)))
      assert(!(minusSevenInt !== (-5 +- 2.toShort)))

      // Int +- Byte
      assert(!(sevenInt !== (9 +- 2.toByte)))
      assert(!(sevenInt !== (8 +- 2.toByte)))
      assert(!(sevenInt !== (7 +- 2.toByte)))
      assert(!(sevenInt !== (6 +- 2.toByte)))
      assert(!(sevenInt !== (5 +- 2.toByte)))
      assert(!(minusSevenInt !== (-9 +- 2.toByte)))
      assert(!(minusSevenInt !== (-8 +- 2.toByte)))
      assert(!(minusSevenInt !== (-7 +- 2.toByte)))
      assert(!(minusSevenInt !== (-6 +- 2.toByte)))
      assert(!(minusSevenInt !== (-5 +- 2.toByte)))

      // Short +- Short
      assert(!(sevenShort !== (9.toShort +- 2.toShort)))
      assert(!(sevenShort !== (8.toShort +- 2.toShort)))
      assert(!(sevenShort !== (7.toShort +- 2.toShort)))
      assert(!(sevenShort !== (6.toShort +- 2.toShort)))
      assert(!(sevenShort !== (5.toShort +- 2.toShort)))
      assert(!(minusSevenShort !== ((-9).toShort +- 2.toShort)))
      assert(!(minusSevenShort !== ((-8).toShort +- 2.toShort)))
      assert(!(minusSevenShort !== ((-7).toShort +- 2.toShort)))
      assert(!(minusSevenShort !== ((-6).toShort +- 2.toShort)))
      assert(!(minusSevenShort !== ((-5).toShort +- 2.toShort)))

      // Short +- Byte
      assert(!(sevenShort !== (9.toShort +- 2.toByte)))
      assert(!(sevenShort !== (8.toShort +- 2.toByte)))
      assert(!(sevenShort !== (7.toShort +- 2.toByte)))
      assert(!(sevenShort !== (6.toShort +- 2.toByte)))
      assert(!(sevenShort !== (5.toShort +- 2.toByte)))
      assert(!(minusSevenShort !== ((-9).toShort +- 2.toByte)))
      assert(!(minusSevenShort !== ((-8).toShort +- 2.toByte)))
      assert(!(minusSevenShort !== ((-7).toShort +- 2.toByte)))
      assert(!(minusSevenShort !== ((-6).toShort +- 2.toByte)))
      assert(!(minusSevenShort !== ((-5).toShort +- 2.toByte)))

      // Byte +- Byte
      assert(!(sevenByte !== (9.toByte +- 2.toByte)))
      assert(!(sevenByte !== (8.toByte +- 2.toByte)))
      assert(!(sevenByte !== (7.toByte +- 2.toByte)))
      assert(!(sevenByte !== (6.toByte +- 2.toByte)))
      assert(!(sevenByte !== (5.toByte +- 2.toByte)))
      assert(!(minusSevenByte !== ((-9).toByte +- 2.toByte)))
      assert(!(minusSevenByte !== ((-8).toByte +- 2.toByte)))
      assert(!(minusSevenByte !== ((-7).toByte +- 2.toByte)))
      assert(!(minusSevenByte !== ((-6).toByte +- 2.toByte)))
      assert(!(minusSevenByte !== ((-5).toByte +- 2.toByte)))
    }

    def `should, for symmetry, be false if the number is within the given interval, when the interval is placed on the left hand side` {

      // Double +- Double
      assert(!((7.1 +- 0.2) !== sevenDotOh))
      assert(!((6.9 +- 0.2) !== sevenDotOh))
      assert(!((7.0 +- 0.2) !== sevenDotOh))
      assert(!((7.2 +- 0.2) !== sevenDotOh))
      assert(!((6.8 +- 0.2) !== sevenDotOh))
      assert(!((-7.1 +- 0.2) !== minusSevenDotOh))
      assert(!((-6.9 +- 0.2) !== minusSevenDotOh))
      assert(!((-7.0 +- 0.2) !== minusSevenDotOh))
      assert(!((-7.2 +- 0.2) !== minusSevenDotOh))
      assert(!((-6.8 +- 0.2) !== minusSevenDotOh))

      // Double +- Float
      assert(!((7.1 +- 0.2f) !== sevenDotOh))
      assert(!((6.9 +- 0.2f) !== sevenDotOh))
      assert(!((7.0 +- 0.2f) !== sevenDotOh))
      assert(!((7.2 +- 0.2f) !== sevenDotOh))
      assert(!((6.8 +- 0.2f) !== sevenDotOh))
      assert(!((-7.1 +- 0.2f) !== minusSevenDotOh))
      assert(!((-6.9 +- 0.2f) !== minusSevenDotOh))
      assert(!((-7.0 +- 0.2f) !== minusSevenDotOh))
      assert(!((-7.2 +- 0.2f) !== minusSevenDotOh))
      assert(!((-6.8 +- 0.2f) !== minusSevenDotOh))

      // Double +- Long
      assert(!((7.1 +- 2L) !== sevenDotOh))
      assert(!((6.9 +- 2L) !== sevenDotOh))
      assert(!((7.0 +- 2L) !== sevenDotOh))
      assert(!((7.2 +- 2L) !== sevenDotOh))
      assert(!((6.8 +- 2L) !== sevenDotOh))
      assert(!((-7.1 +- 2L) !== minusSevenDotOh))
      assert(!((-6.9 +- 2L) !== minusSevenDotOh))
      assert(!((-7.0 +- 2L) !== minusSevenDotOh))
      assert(!((-7.2 +- 2L) !== minusSevenDotOh))
      assert(!((-6.8 +- 2L) !== minusSevenDotOh))

      // Double +- Int
      assert(!((7.1 +- 2) !== sevenDotOh))
      assert(!((6.9 +- 2) !== sevenDotOh))
      assert(!((7.0 +- 2) !== sevenDotOh))
      assert(!((7.2 +- 2) !== sevenDotOh))
      assert(!((6.8 +- 2) !== sevenDotOh))
      assert(!((-7.1 +- 2) !== minusSevenDotOh))
      assert(!((-6.9 +- 2) !== minusSevenDotOh))
      assert(!((-7.0 +- 2) !== minusSevenDotOh))
      assert(!((-7.2 +- 2) !== minusSevenDotOh))
      assert(!((-6.8 +- 2) !== minusSevenDotOh))

      // Double +- Short
      assert(!((7.1 +- 2.toShort) !== sevenDotOh))
      assert(!((6.9 +- 2.toShort) !== sevenDotOh))
      assert(!((7.0 +- 2.toShort) !== sevenDotOh))
      assert(!((7.2 +- 2.toShort) !== sevenDotOh))
      assert(!((6.8 +- 2.toShort) !== sevenDotOh))
      assert(!((-7.1 +- 2.toShort) !== minusSevenDotOh))
      assert(!((-6.9 +- 2.toShort) !== minusSevenDotOh))
      assert(!((-7.0 +- 2.toShort) !== minusSevenDotOh))
      assert(!((-7.2 +- 2.toShort) !== minusSevenDotOh))
      assert(!((-6.8 +- 2.toShort) !== minusSevenDotOh))

      // Double +- Byte
      assert(!((7.1 +- 2.toByte) !== sevenDotOh))
      assert(!((6.9 +- 2.toByte) !== sevenDotOh))
      assert(!((7.0 +- 2.toByte) !== sevenDotOh))
      assert(!((7.2 +- 2.toByte) !== sevenDotOh))
      assert(!((6.8 +- 2.toByte) !== sevenDotOh))
      assert(!((-7.1 +- 2.toByte) !== minusSevenDotOh))
      assert(!((-6.9 +- 2.toByte) !== minusSevenDotOh))
      assert(!((-7.0 +- 2.toByte) !== minusSevenDotOh))
      assert(!((-7.2 +- 2.toByte) !== minusSevenDotOh))
      assert(!((-6.8 +- 2.toByte) !== minusSevenDotOh))

      // Float +- Float
      assert(!((7.1f +- 0.2f) !== sevenDotOhFloat))
      assert(!((6.9f +- 0.2f) !== sevenDotOhFloat))
      assert(!((7.0f +- 0.2f) !== sevenDotOhFloat))
      assert(!((7.2f +- 0.2f) !== sevenDotOhFloat))
      assert(!((6.8f +- 0.2f) !== sevenDotOhFloat))
      assert(!((-7.1f +- 0.2f) !== minusSevenDotOhFloat))
      assert(!((-6.9f +- 0.2f) !== minusSevenDotOhFloat))
      assert(!((-7.0f +- 0.2f) !== minusSevenDotOhFloat))
      assert(!((-7.2f +- 0.2f) !== minusSevenDotOhFloat))
      assert(!((-6.8f +- 0.2f) !== minusSevenDotOhFloat))

      // Float +- Long
      assert(!((7.1f +- 2L) !== sevenDotOhFloat))
      assert(!((6.9f +- 2L) !== sevenDotOhFloat))
      assert(!((7.0f +- 2L) !== sevenDotOhFloat))
      assert(!((7.2f +- 2L) !== sevenDotOhFloat))
      assert(!((6.8f +- 2L) !== sevenDotOhFloat))
      assert(!((-7.1f +- 2L) !== minusSevenDotOhFloat))
      assert(!((-6.9f +- 2L) !== minusSevenDotOhFloat))
      assert(!((-7.0f +- 2L) !== minusSevenDotOhFloat))
      assert(!((-7.2f +- 2L) !== minusSevenDotOhFloat))
      assert(!((-6.8f +- 2L) !== minusSevenDotOhFloat))

      // Float +- Int
      assert(!((7.1f +- 2) !== sevenDotOhFloat))
      assert(!((6.9f +- 2) !== sevenDotOhFloat))
      assert(!((7.0f +- 2) !== sevenDotOhFloat))
      assert(!((7.2f +- 2) !== sevenDotOhFloat))
      assert(!((6.8f +- 2) !== sevenDotOhFloat))
      assert(!((-7.1f +- 2) !== minusSevenDotOhFloat))
      assert(!((-6.9f +- 2) !== minusSevenDotOhFloat))
      assert(!((-7.0f +- 2) !== minusSevenDotOhFloat))
      assert(!((-7.2f +- 2) !== minusSevenDotOhFloat))
      assert(!((-6.8f +- 2) !== minusSevenDotOhFloat))

      // Float +- Short
      assert(!((7.1f +- 2.toShort) !== sevenDotOhFloat))
      assert(!((6.9f +- 2.toShort) !== sevenDotOhFloat))
      assert(!((7.0f +- 2.toShort) !== sevenDotOhFloat))
      assert(!((7.2f +- 2.toShort) !== sevenDotOhFloat))
      assert(!((6.8f +- 2.toShort) !== sevenDotOhFloat))
      assert(!((-7.1f +- 2.toShort) !== minusSevenDotOhFloat))
      assert(!((-6.9f +- 2.toShort) !== minusSevenDotOhFloat))
      assert(!((-7.0f +- 2.toShort) !== minusSevenDotOhFloat))
      assert(!((-7.2f +- 2.toShort) !== minusSevenDotOhFloat))
      assert(!((-6.8f +- 2.toShort) !== minusSevenDotOhFloat))

      // Float +- Byte
      assert(!((7.1f +- 2.toByte) !== sevenDotOhFloat))
      assert(!((6.9f +- 2.toByte) !== sevenDotOhFloat))
      assert(!((7.0f +- 2.toByte) !== sevenDotOhFloat))
      assert(!((7.2f +- 2.toByte) !== sevenDotOhFloat))
      assert(!((6.8f +- 2.toByte) !== sevenDotOhFloat))
      assert(!((-7.1f +- 2.toByte) !== minusSevenDotOhFloat))
      assert(!((-6.9f +- 2.toByte) !== minusSevenDotOhFloat))
      assert(!((-7.0f +- 2.toByte) !== minusSevenDotOhFloat))
      assert(!((-7.2f +- 2.toByte) !== minusSevenDotOhFloat))
      assert(!((-6.8f +- 2.toByte) !== minusSevenDotOhFloat))

      // Long +- Long
      assert(!((9L +- 2L) !== sevenLong))
      assert(!((8L +- 2L) !== sevenLong))
      assert(!((7L +- 2L) !== sevenLong))
      assert(!((6L +- 2L) !== sevenLong))
      assert(!((5L +- 2L) !== sevenLong))
      assert(!((-9L +- 2L) !== minusSevenLong))
      assert(!((-8L +- 2L) !== minusSevenLong))
      assert(!((-7L +- 2L) !== minusSevenLong))
      assert(!((-6L +- 2L) !== minusSevenLong))
      assert(!((-5L +- 2L) !== minusSevenLong))

      // Long +- Int
      assert(!((9L +- 2) !== sevenLong))
      assert(!((8L +- 2) !== sevenLong))
      assert(!((7L +- 2) !== sevenLong))
      assert(!((6L +- 2) !== sevenLong))
      assert(!((5L +- 2) !== sevenLong))
      assert(!((-9L +- 2) !== minusSevenLong))
      assert(!((-8L +- 2) !== minusSevenLong))
      assert(!((-7L +- 2) !== minusSevenLong))
      assert(!((-6L +- 2) !== minusSevenLong))
      assert(!((-5L +- 2) !== minusSevenLong))

      // Long +- Short
      assert(!((9L +- 2.toShort) !== sevenLong))
      assert(!((8L +- 2.toShort) !== sevenLong))
      assert(!((7L +- 2.toShort) !== sevenLong))
      assert(!((6L +- 2.toShort) !== sevenLong))
      assert(!((5L +- 2.toShort) !== sevenLong))
      assert(!((-9L +- 2.toShort) !== minusSevenLong))
      assert(!((-8L +- 2.toShort) !== minusSevenLong))
      assert(!((-7L +- 2.toShort) !== minusSevenLong))
      assert(!((-6L +- 2.toShort) !== minusSevenLong))
      assert(!((-5L +- 2.toShort) !== minusSevenLong))

      // Long +- Byte
      assert(!((9L +- 2.toByte) !== sevenLong))
      assert(!((8L +- 2.toByte) !== sevenLong))
      assert(!((7L +- 2.toByte) !== sevenLong))
      assert(!((6L +- 2.toByte) !== sevenLong))
      assert(!((5L +- 2.toByte) !== sevenLong))
      assert(!((-9L +- 2.toByte) !== minusSevenLong))
      assert(!((-8L +- 2.toByte) !== minusSevenLong))
      assert(!((-7L +- 2.toByte) !== minusSevenLong))
      assert(!((-6L +- 2.toByte) !== minusSevenLong))
      assert(!((-5L +- 2.toByte) !== minusSevenLong))

      // Int +- Int
      assert(!((9 +- 2) !== sevenInt))
      assert(!((8 +- 2) !== sevenInt))
      assert(!((7 +- 2) !== sevenInt))
      assert(!((6 +- 2) !== sevenInt))
      assert(!((5 +- 2) !== sevenInt))
      assert(!((-9 +- 2) !== minusSevenInt))
      assert(!((-8 +- 2) !== minusSevenInt))
      assert(!((-7 +- 2) !== minusSevenInt))
      assert(!((-6 +- 2) !== minusSevenInt))
      assert(!((-5 +- 2) !== minusSevenInt))

      // Int +- Short
      assert(!((9 +- 2.toShort) !== sevenInt))
      assert(!((8 +- 2.toShort) !== sevenInt))
      assert(!((7 +- 2.toShort) !== sevenInt))
      assert(!((6 +- 2.toShort) !== sevenInt))
      assert(!((5 +- 2.toShort) !== sevenInt))
      assert(!((-9 +- 2.toShort) !== minusSevenInt))
      assert(!((-8 +- 2.toShort) !== minusSevenInt))
      assert(!((-7 +- 2.toShort) !== minusSevenInt))
      assert(!((-6 +- 2.toShort) !== minusSevenInt))
      assert(!((-5 +- 2.toShort) !== minusSevenInt))

      // Int +- Byte
      assert(!((9 +- 2.toByte) !== sevenInt))
      assert(!((8 +- 2.toByte) !== sevenInt))
      assert(!((7 +- 2.toByte) !== sevenInt))
      assert(!((6 +- 2.toByte) !== sevenInt))
      assert(!((5 +- 2.toByte) !== sevenInt))
      assert(!((-9 +- 2.toByte) !== minusSevenInt))
      assert(!((-8 +- 2.toByte) !== minusSevenInt))
      assert(!((-7 +- 2.toByte) !== minusSevenInt))
      assert(!((-6 +- 2.toByte) !== minusSevenInt))
      assert(!((-5 +- 2.toByte) !== minusSevenInt))

      // Short +- Short
      assert(!((9.toShort +- 2.toShort) !== sevenShort))
      assert(!((8.toShort +- 2.toShort) !== sevenShort))
      assert(!((7.toShort +- 2.toShort) !== sevenShort))
      assert(!((6.toShort +- 2.toShort) !== sevenShort))
      assert(!((5.toShort +- 2.toShort) !== sevenShort))
      assert(!(((-9).toShort +- 2.toShort) !== minusSevenShort))
      assert(!(((-8).toShort +- 2.toShort) !== minusSevenShort))
      assert(!(((-7).toShort +- 2.toShort) !== minusSevenShort))
      assert(!(((-6).toShort +- 2.toShort) !== minusSevenShort))
      assert(!(((-5).toShort +- 2.toShort) !== minusSevenShort))

      // Short +- Byte
      assert(!((9.toShort +- 2.toByte) !== sevenShort))
      assert(!((8.toShort +- 2.toByte) !== sevenShort))
      assert(!((7.toShort +- 2.toByte) !== sevenShort))
      assert(!((6.toShort +- 2.toByte) !== sevenShort))
      assert(!((5.toShort +- 2.toByte) !== sevenShort))
      assert(!(((-9).toShort +- 2.toByte) !== minusSevenShort))
      assert(!(((-8).toShort +- 2.toByte) !== minusSevenShort))
      assert(!(((-7).toShort +- 2.toByte) !== minusSevenShort))
      assert(!(((-6).toShort +- 2.toByte) !== minusSevenShort))
      assert(!(((-5).toShort +- 2.toByte) !== minusSevenShort))

      // Byte +- Byte
      assert(!((9.toByte +- 2.toByte) !== sevenByte))
      assert(!((8.toByte +- 2.toByte) !== sevenByte))
      assert(!((7.toByte +- 2.toByte) !== sevenByte))
      assert(!((6.toByte +- 2.toByte) !== sevenByte))
      assert(!((5.toByte +- 2.toByte) !== sevenByte))
      assert(!(((-9).toByte +- 2.toByte) !== minusSevenByte))
      assert(!(((-8).toByte +- 2.toByte) !== minusSevenByte))
      assert(!(((-7).toByte +- 2.toByte) !== minusSevenByte))
      assert(!(((-6).toByte +- 2.toByte) !== minusSevenByte))
      assert(!(((-5).toByte +- 2.toByte) !== minusSevenByte))
    }
  }

  object `The X +- Y syntax` {

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative` {

      // Double +- Double
      val caught1 = intercept[IllegalArgumentException] {
        assert(sevenDotOh === (7.1 +- -0.2))
      }
      assert(caught1.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.", caught1.getMessage)

      // Double +- Float
      val caught2 = intercept[IllegalArgumentException] {
        assert(sevenDotOh === (7.1 +- -0.2f))
      }
      assert(caught2.getMessage === "-0.20000000298023224 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Long
      val caught3 = intercept[IllegalArgumentException] {
        assert(sevenDotOh === (7.1 +- -2L))
      }
      assert(caught3.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Int
      val caught4 = intercept[IllegalArgumentException] {
        assert(sevenDotOh === (7.1 +- -2))
      }
      assert(caught4.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Short
      val caught5 = intercept[IllegalArgumentException] {
        assert(sevenDotOh === (7.1 +- (-2).toShort))
      }
      assert(caught5.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Byte
      val caught6 = intercept[IllegalArgumentException] {
        assert(sevenDotOh === (7.1 +- (-2).toByte))
      }
      assert(caught6.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Float
      val caught7 = intercept[IllegalArgumentException] {
        assert(sevenDotOhFloat === (7.1f +- -0.2f))
      }
      assert(caught7.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Long
      val caught8 = intercept[IllegalArgumentException] {
        assert(sevenDotOhFloat === (7.1f +- -2L))
      }
      assert(caught8.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Int
      val caught9 = intercept[IllegalArgumentException] {
        assert(sevenDotOhFloat === (7.1f +- -2))
      }
      assert(caught9.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Short
      val caught10 = intercept[IllegalArgumentException] {
        assert(sevenDotOhFloat === (7.1f +- (-2).toShort))
      }
      assert(caught10.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Byte
      val caught11 = intercept[IllegalArgumentException] {
        assert(sevenDotOhFloat === (7.1f +- (-2).toByte))
      }
      assert(caught11.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Long
      val caught12 = intercept[IllegalArgumentException] {
        assert(sevenLong === (9L +- -2L))
      }
      assert(caught12.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Int
      val caught13 = intercept[IllegalArgumentException] {
        assert(sevenLong === (9L +- -2))
      }
      assert(caught13.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Short
      val caught14 = intercept[IllegalArgumentException] {
        assert(sevenLong === (9L +- (-2).toShort))
      }
      assert(caught14.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Byte
      val caught15 = intercept[IllegalArgumentException] {
        assert(sevenLong === (9L +- (-2).toByte))
      }
      assert(caught15.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Int
      val caught16 = intercept[IllegalArgumentException] {
        assert(sevenInt === (9 +- -2))
      }
      assert(caught16.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Short
      val caught17 = intercept[IllegalArgumentException] {
        assert(sevenInt === (9 +- (-2).toShort))
      }
      assert(caught17.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Byte
      val caught18 = intercept[IllegalArgumentException] {
        assert(sevenInt === (9 +- (-2).toByte))
      }
      assert(caught18.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Short
      val caught19 = intercept[IllegalArgumentException] {
        assert(sevenShort === (9.toShort +- (-2).toShort))
      }
      assert(caught19.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Byte
      val caught20 = intercept[IllegalArgumentException] {
        assert(sevenShort === (9.toShort +- (-2).toByte))
      }
      assert(caught20.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Byte +- Byte
      val caught21 = intercept[IllegalArgumentException] {
        assert(sevenByte === (9.toByte +- (-2).toByte))
      }
      assert(caught21.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")
    }
  }
}
