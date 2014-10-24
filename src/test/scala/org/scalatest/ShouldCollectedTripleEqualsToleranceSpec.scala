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
import SharedHelpers._
import Matchers._
import TripleEquals._
import exceptions.TestFailedException

class ShouldCollectedTripleEqualsToleranceSpec extends Spec /* with NonImplicitAssertions */ with Tolerance {

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
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- 0.2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.9 +- 0.2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.0 +- 0.2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.2 +- 0.2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.8 +- 0.2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.1 +- 0.2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.9 +- 0.2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.0 +- 0.2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.2 +- 0.2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.8 +- 0.2)

      // Double +- Float
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- 0.2f)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.9 +- 0.2f)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.0 +- 0.2f)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.2 +- 0.2f)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.8 +- 0.2f)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.1 +- 0.2f)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.9 +- 0.2f)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.0 +- 0.2f)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.2 +- 0.2f)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.8 +- 0.2f)

      // Double +- Long
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- 2L)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.9 +- 2L)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.0 +- 2L)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.2 +- 2L)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.8 +- 2L)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.1 +- 2L)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.9 +- 2L)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.0 +- 2L)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.2 +- 2L)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.8 +- 2L)

      // Double +- Int
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- 2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.9 +- 2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.0 +- 2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.2 +- 2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.8 +- 2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.1 +- 2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.9 +- 2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.0 +- 2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.2 +- 2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.8 +- 2)

      // Double +- Short
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- 2.toShort)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.9 +- 2.toShort)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.0 +- 2.toShort)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.2 +- 2.toShort)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.8 +- 2.toShort)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.1 +- 2.toShort)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.9 +- 2.toShort)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.0 +- 2.toShort)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.2 +- 2.toShort)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.8 +- 2.toShort)

      // Double +- Byte
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- 2.toByte)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.9 +- 2.toByte)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.0 +- 2.toByte)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.2 +- 2.toByte)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.8 +- 2.toByte)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.1 +- 2.toByte)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.9 +- 2.toByte)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.0 +- 2.toByte)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.2 +- 2.toByte)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.8 +- 2.toByte)

      // Float +- Float
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- 0.2f)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.9f +- 0.2f)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.0f +- 0.2f)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.2f +- 0.2f)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.8f +- 0.2f)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.1f +- 0.2f)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.9f +- 0.2f)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.0f +- 0.2f)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.2f +- 0.2f)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.8f +- 0.2f)

      // Float +- Long
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- 2L)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.9f +- 2L)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.0f +- 2L)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.2f +- 2L)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.8f +- 2L)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.1f +- 2L)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.9f +- 2L)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.0f +- 2L)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.2f +- 2L)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.8f +- 2L)

      // Float +- Int
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- 2)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.9f +- 2)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.0f +- 2)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.2f +- 2)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.8f +- 2)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.1f +- 2)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.9f +- 2)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.0f +- 2)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.2f +- 2)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.8f +- 2)

      // Float +- Short
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- 2.toShort)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.9f +- 2.toShort)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.0f +- 2.toShort)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.2f +- 2.toShort)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.8f +- 2.toShort)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.1f +- 2.toShort)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.9f +- 2.toShort)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.0f +- 2.toShort)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.2f +- 2.toShort)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.8f +- 2.toShort)

      // Float +- Byte
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- 2.toByte)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.9f +- 2.toByte)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.0f +- 2.toByte)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.2f +- 2.toByte)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.8f +- 2.toByte)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.1f +- 2.toByte)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.9f +- 2.toByte)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.0f +- 2.toByte)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.2f +- 2.toByte)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.8f +- 2.toByte)

      // Long +- Long
      all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- 2L)
      all (List(sevenLong, sevenLong, sevenLong)) should === (8L +- 2L)
      all (List(sevenLong, sevenLong, sevenLong)) should === (7L +- 2L)
      all (List(sevenLong, sevenLong, sevenLong)) should === (6L +- 2L)
      all (List(sevenLong, sevenLong, sevenLong)) should === (5L +- 2L)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-9L +- 2L)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-8L +- 2L)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-7L +- 2L)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-6L +- 2L)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-5L +- 2L)

      // Long +- Int
      all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- 2)
      all (List(sevenLong, sevenLong, sevenLong)) should === (8L +- 2)
      all (List(sevenLong, sevenLong, sevenLong)) should === (7L +- 2)
      all (List(sevenLong, sevenLong, sevenLong)) should === (6L +- 2)
      all (List(sevenLong, sevenLong, sevenLong)) should === (5L +- 2)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-9L +- 2)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-8L +- 2)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-7L +- 2)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-6L +- 2)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-5L +- 2)

      // Long +- Short
      all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- 2.toShort)
      all (List(sevenLong, sevenLong, sevenLong)) should === (8L +- 2.toShort)
      all (List(sevenLong, sevenLong, sevenLong)) should === (7L +- 2.toShort)
      all (List(sevenLong, sevenLong, sevenLong)) should === (6L +- 2.toShort)
      all (List(sevenLong, sevenLong, sevenLong)) should === (5L +- 2.toShort)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-9L +- 2.toShort)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-8L +- 2.toShort)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-7L +- 2.toShort)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-6L +- 2.toShort)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-5L +- 2.toShort)

      // Long +- Byte
      all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- 2.toByte)
      all (List(sevenLong, sevenLong, sevenLong)) should === (8L +- 2.toByte)
      all (List(sevenLong, sevenLong, sevenLong)) should === (7L +- 2.toByte)
      all (List(sevenLong, sevenLong, sevenLong)) should === (6L +- 2.toByte)
      all (List(sevenLong, sevenLong, sevenLong)) should === (5L +- 2.toByte)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-9L +- 2.toByte)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-8L +- 2.toByte)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-7L +- 2.toByte)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-6L +- 2.toByte)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-5L +- 2.toByte)

      // Int +- Int
      all (List(sevenInt, sevenInt, sevenInt)) should === (9 +- 2)
      all (List(sevenInt, sevenInt, sevenInt)) should === (8 +- 2)
      all (List(sevenInt, sevenInt, sevenInt)) should === (7 +- 2)
      all (List(sevenInt, sevenInt, sevenInt)) should === (6 +- 2)
      all (List(sevenInt, sevenInt, sevenInt)) should === (5 +- 2)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-9 +- 2)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-8 +- 2)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-7 +- 2)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-6 +- 2)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-5 +- 2)

      // Int +- Short
      all (List(sevenInt, sevenInt, sevenInt)) should === (9 +- 2.toShort)
      all (List(sevenInt, sevenInt, sevenInt)) should === (8 +- 2.toShort)
      all (List(sevenInt, sevenInt, sevenInt)) should === (7 +- 2.toShort)
      all (List(sevenInt, sevenInt, sevenInt)) should === (6 +- 2.toShort)
      all (List(sevenInt, sevenInt, sevenInt)) should === (5 +- 2.toShort)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-9 +- 2.toShort)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-8 +- 2.toShort)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-7 +- 2.toShort)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-6 +- 2.toShort)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-5 +- 2.toShort)

      // Int +- Byte
      all (List(sevenInt, sevenInt, sevenInt)) should === (9 +- 2.toByte)
      all (List(sevenInt, sevenInt, sevenInt)) should === (8 +- 2.toByte)
      all (List(sevenInt, sevenInt, sevenInt)) should === (7 +- 2.toByte)
      all (List(sevenInt, sevenInt, sevenInt)) should === (6 +- 2.toByte)
      all (List(sevenInt, sevenInt, sevenInt)) should === (5 +- 2.toByte)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-9 +- 2.toByte)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-8 +- 2.toByte)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-7 +- 2.toByte)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-6 +- 2.toByte)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-5 +- 2.toByte)

      // Short +- Short
      all (List(sevenShort, sevenShort, sevenShort)) should === (9.toShort +- 2.toShort)
      all (List(sevenShort, sevenShort, sevenShort)) should === (8.toShort +- 2.toShort)
      all (List(sevenShort, sevenShort, sevenShort)) should === (7.toShort +- 2.toShort)
      all (List(sevenShort, sevenShort, sevenShort)) should === (6.toShort +- 2.toShort)
      all (List(sevenShort, sevenShort, sevenShort)) should === (5.toShort +- 2.toShort)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-9).toShort +- 2.toShort)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-8).toShort +- 2.toShort)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-7).toShort +- 2.toShort)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-6).toShort +- 2.toShort)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-5).toShort +- 2.toShort)

      // Short +- Byte
      all (List(sevenShort, sevenShort, sevenShort)) should === (9.toShort +- 2.toByte)
      all (List(sevenShort, sevenShort, sevenShort)) should === (8.toShort +- 2.toByte)
      all (List(sevenShort, sevenShort, sevenShort)) should === (7.toShort +- 2.toByte)
      all (List(sevenShort, sevenShort, sevenShort)) should === (6.toShort +- 2.toByte)
      all (List(sevenShort, sevenShort, sevenShort)) should === (5.toShort +- 2.toByte)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-9).toShort +- 2.toByte)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-8).toShort +- 2.toByte)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-7).toShort +- 2.toByte)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-6).toShort +- 2.toByte)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-5).toShort +- 2.toByte)

      // Byte +- Byte
      all (List(sevenByte, sevenByte, sevenByte)) should === (9.toByte +- 2.toByte)
      all (List(sevenByte, sevenByte, sevenByte)) should === (8.toByte +- 2.toByte)
      all (List(sevenByte, sevenByte, sevenByte)) should === (7.toByte +- 2.toByte)
      all (List(sevenByte, sevenByte, sevenByte)) should === (6.toByte +- 2.toByte)
      all (List(sevenByte, sevenByte, sevenByte)) should === (5.toByte +- 2.toByte)
      all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should === ((-9).toByte +- 2.toByte)
      all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should === ((-8).toByte +- 2.toByte)
      all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should === ((-7).toByte +- 2.toByte)
      all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should === ((-6).toByte +- 2.toByte)
      all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should === ((-5).toByte +- 2.toByte)
    }

    def `should throw TFE if the number is outside the given interval` {

      // Double +- Double
      val caught = intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.5 +- 0.2) }
      caught.message should be (Some("'all' inspection failed, because: \n" +
                                "  at index 0, 7.0 did not equal 7.5 plus or minus 0.2 (ShouldCollectedTripleEqualsToleranceSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                "in List(7.0, 7.0, 7.0)"))
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.5 +- 0.2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.5 +- 0.2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.5 +- 0.2) }

      // Double +- Float
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.5 +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (6.5 +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-7.5 +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-6.5 +- 0.2f) }

      // Double +- Long
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (4.0 +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (9.1 +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-4.0 +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-9.1 +- 2L) }

      // Double +- Int
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (4.0 +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (9.1 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-4.0 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-9.1 +- 2) }

      // Double +- Short
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (4.0 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (9.1 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-4.0 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-9.1 +- 2.toShort) }

      // Double +- Byte
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (4.0 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (9.1 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-4.0 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should === (-9.1 +- 2.toByte) }

      // Float +- Float
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.5f +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (6.5f +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-7.5f +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-6.5f +- 0.2f) }

      // Float +- Long
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (4.0f +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (9.1f +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-4.0f +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-9.1f +- 2L) }

      // Float +- Int
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (4.0f +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (9.1f +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-4.0f +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-9.1f +- 2) }

      // Float +- Short
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (4.0f +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (9.1f +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-4.0f +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-9.1f +- 2.toShort) }

      // Float +- Byte
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (4.0f +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (9.1f +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-4.0f +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should === (-9.1f +- 2.toByte) }

      // Long +- Long
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (4L +- 2L) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (10L +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-4L +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-10L +- 2L) }

      // Long +- Int
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (4L +- 2) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (10L +- 2) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-4L +- 2) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-10L +- 2) }

      // Long +- Short
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (4L +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (10L +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-4L +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-10L +- 2.toShort) }

      // Long +- Byte
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (4L +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should === (10L +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-4L +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should === (-10L +- 2.toByte) }

      // Int +- Int
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should === (4 +- 2) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should === (10 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-4 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-10 +- 2) }

      // Int +- Short
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should === (4 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should === (10 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-4 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-10 +- 2.toShort) }

      // Int +- Byte
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should === (4 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should === (10 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-4 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should === (-10 +- 2.toByte) }

      // Short +- Short
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should === (4.toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should === (10.toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-4).toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-10).toShort +- 2.toShort) }

      // Short +- Byte
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should === (4.toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should === (10.toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-4).toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should === ((-10).toShort +- 2.toByte) }

      // Byte +- Byte
      intercept[TestFailedException] { all (List(sevenByte, sevenByte, sevenByte)) should === (4.toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenByte, sevenByte, sevenByte)) should === (10.toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should === ((-4).toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should === ((-10).toByte +- 2.toByte) }
    }
  }

  object `The !== syntax` {

    def `should succeed if the number is outside the given interval` {

      // Double +- Double
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.5 +- 0.2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.5 +- 0.2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.5 +- 0.2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.5 +- 0.2)

      // Double +- Float
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.5 +- 0.2f)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.5 +- 0.2f)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.5 +- 0.2f)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.5 +- 0.2f)

      // Double +- Long
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (4.0 +- 2L)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (9.1 +- 2L)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-4.0 +- 2L)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-9.1 +- 2L)

      // Double +- Int
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (4.0 +- 2)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (9.1 +- 2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-4.0 +- 2)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-9.1 +- 2)

      // Double +- Short
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (4.0 +- 2.toShort)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (9.1 +- 2.toShort)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-4.0 +- 2.toShort)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-9.1 +- 2.toShort)

      // Double +- Byte
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (4.0 +- 2.toByte)
      all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (9.1 +- 2.toByte)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-4.0 +- 2.toByte)
      all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-9.1 +- 2.toByte)

      // Float +- Float
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.5f +- 0.2f)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.5f +- 0.2f)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.5f +- 0.2f)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.5f +- 0.2f)

      // Float +- Long
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (4.0f +- 2L)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (9.1f +- 2L)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-4.0f +- 2L)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-9.1f +- 2L)

      // Float +- Int
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (4.0f +- 2)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (9.1f +- 2)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-4.0f +- 2)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-9.1f +- 2)

      // Float +- Short
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (4.0f +- 2.toShort)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (9.1f +- 2.toShort)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-4.0f +- 2.toShort)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-9.1f +- 2.toShort)

      // Float +- Byte
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (4.0f +- 2.toByte)
      all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (9.1f +- 2.toByte)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-4.0f +- 2.toByte)
      all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-9.1f +- 2.toByte)

      // Long +- Long
      all (List(sevenLong, sevenLong, sevenLong)) should !== (4L +- 2L)
      all (List(sevenLong, sevenLong, sevenLong)) should !== (10L +- 2L)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-4L +- 2L)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-10L +- 2L)

      // Long +- Int
      all (List(sevenLong, sevenLong, sevenLong)) should !== (4L +- 2)
      all (List(sevenLong, sevenLong, sevenLong)) should !== (10L +- 2)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-4L +- 2)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-10L +- 2)

      // Long +- Short
      all (List(sevenLong, sevenLong, sevenLong)) should !== (4L +- 2.toShort)
      all (List(sevenLong, sevenLong, sevenLong)) should !== (10L +- 2.toShort)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-4L +- 2.toShort)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-10L +- 2.toShort)

      // Long +- Byte
      all (List(sevenLong, sevenLong, sevenLong)) should !== (4L +- 2.toByte)
      all (List(sevenLong, sevenLong, sevenLong)) should !== (10L +- 2.toByte)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-4L +- 2.toByte)
      all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-10L +- 2.toByte)

      // Int +- Int
      all (List(sevenInt, sevenInt, sevenInt)) should !== (4 +- 2)
      all (List(sevenInt, sevenInt, sevenInt)) should !== (10 +- 2)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-4 +- 2)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-10 +- 2)

      // Int +- Short
      all (List(sevenInt, sevenInt, sevenInt)) should !== (4 +- 2.toShort)
      all (List(sevenInt, sevenInt, sevenInt)) should !== (10 +- 2.toShort)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-4 +- 2.toShort)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-10 +- 2.toShort)

      // Int +- Byte
      all (List(sevenInt, sevenInt, sevenInt)) should !== (4 +- 2.toByte)
      all (List(sevenInt, sevenInt, sevenInt)) should !== (10 +- 2.toByte)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-4 +- 2.toByte)
      all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-10 +- 2.toByte)

      // Short +- Short
      all (List(sevenShort, sevenShort, sevenShort)) should !== (4.toShort +- 2.toShort)
      all (List(sevenShort, sevenShort, sevenShort)) should !== (10.toShort +- 2.toShort)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-4).toShort +- 2.toShort)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-10).toShort +- 2.toShort)

      // Short +- Byte
      all (List(sevenShort, sevenShort, sevenShort)) should !== (4.toShort +- 2.toByte)
      all (List(sevenShort, sevenShort, sevenShort)) should !== (10.toShort +- 2.toByte)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-4).toShort +- 2.toByte)
      all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-10).toShort +- 2.toByte)

      // Byte +- Byte
      all (List(sevenByte, sevenByte, sevenByte)) should !== (4.toByte +- 2.toByte)
      all (List(sevenByte, sevenByte, sevenByte)) should !== (10.toByte +- 2.toByte)
      all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should !== ((-4).toByte +- 2.toByte)
      all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should !== ((-10).toByte +- 2.toByte)
    }

    def `should throw TFE if the number is within the given interval` {

      // Double +- Double
      val caught = intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.1 +- 0.2) }
      caught.message should be (Some("'all' inspection failed, because: \n" +
                                "  at index 0, 7.0 equaled 7.1 plus or minus 0.2 (ShouldCollectedTripleEqualsToleranceSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                "in List(7.0, 7.0, 7.0)"))
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.9 +- 0.2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.0 +- 0.2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.2 +- 0.2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.8 +- 0.2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.1 +- 0.2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.9 +- 0.2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.0 +- 0.2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.2 +- 0.2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.8 +- 0.2) }

      // Double +- Float
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.1 +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.9 +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.0 +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.2 +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.8 +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.1 +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.9 +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.0 +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.2 +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.8 +- 0.2f) }

      // Double +- Long
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.1 +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.9 +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.0 +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.2 +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.8 +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.1 +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.9 +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.0 +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.2 +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.8 +- 2L) }

      // Double +- Int
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.1 +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.9 +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.0 +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.2 +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.8 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.1 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.9 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.0 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.2 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.8 +- 2) }

      // Double +- Short
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.1 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.9 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.0 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.2 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.8 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.1 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.9 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.0 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.2 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.8 +- 2.toShort) }

      // Double +- Byte
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.1 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.9 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.0 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (7.2 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should !== (6.8 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.1 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.9 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.0 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-7.2 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOh, minusSevenDotOh, minusSevenDotOh)) should !== (-6.8 +- 2.toByte) }

      // Float +- Float
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.1f +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.9f +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.0f +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.2f +- 0.2f) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.8f +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.1f +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.9f +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.0f +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.2f +- 0.2f) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.8f +- 0.2f) }

      // Float +- Long
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.1f +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.9f +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.0f +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.2f +- 2L) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.8f +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.1f +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.9f +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.0f +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.2f +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.8f +- 2L) }

      // Float +- Int
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.1f +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.9f +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.0f +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.2f +- 2) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.8f +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.1f +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.9f +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.0f +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.2f +- 2) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.8f +- 2) }

      // Float +- Short
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.1f +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.9f +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.0f +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.2f +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.8f +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.1f +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.9f +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.0f +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.2f +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.8f +- 2.toShort) }

      // Float +- Byte
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.1f +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.9f +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.0f +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (7.2f +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should !== (6.8f +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.1f +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.9f +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.0f +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-7.2f +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenDotOhFloat, minusSevenDotOhFloat, minusSevenDotOhFloat)) should !== (-6.8f +- 2.toByte) }

      // Long +- Long
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (9L +- 2L) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (8L +- 2L) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (7L +- 2L) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (6L +- 2L) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (5L +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-9L +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-8L +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-7L +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-6L +- 2L) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-5L +- 2L) }

      // Long +- Int
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (9L +- 2) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (8L +- 2) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (7L +- 2) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (6L +- 2) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (5L +- 2) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-9L +- 2) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-8L +- 2) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-7L +- 2) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-6L +- 2) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-5L +- 2) }

      // Long +- Short
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (9L +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (8L +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (7L +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (6L +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (5L +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-9L +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-8L +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-7L +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-6L +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-5L +- 2.toShort) }

      // Long +- Byte
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (9L +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (8L +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (7L +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (6L +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenLong, sevenLong, sevenLong)) should !== (5L +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-9L +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-8L +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-7L +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-6L +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenLong, minusSevenLong, minusSevenLong)) should !== (-5L +- 2.toByte) }

      // Int +- Int
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (9 +- 2) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (8 +- 2) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (7 +- 2) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (6 +- 2) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (5 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-9 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-8 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-7 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-6 +- 2) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-5 +- 2) }

      // Int +- Short
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (9 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (8 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (7 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (6 +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (5 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-9 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-8 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-7 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-6 +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-5 +- 2.toShort) }

      // Int +- Byte
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (9 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (8 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (7 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (6 +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenInt, sevenInt, sevenInt)) should !== (5 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-9 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-8 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-7 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-6 +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenInt, minusSevenInt, minusSevenInt)) should !== (-5 +- 2.toByte) }

      // Short +- Short
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (9.toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (8.toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (7.toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (6.toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (5.toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-9).toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-8).toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-7).toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-6).toShort +- 2.toShort) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-5).toShort +- 2.toShort) }

      // Short +- Byte
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (9.toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (8.toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (7.toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (6.toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenShort, sevenShort, sevenShort)) should !== (5.toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-9).toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-8).toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-7).toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-6).toShort +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenShort, minusSevenShort, minusSevenShort)) should !== ((-5).toShort +- 2.toByte) }

      // Byte +- Byte
      intercept[TestFailedException] { all (List(sevenByte, sevenByte, sevenByte)) should !== (9.toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenByte, sevenByte, sevenByte)) should !== (8.toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenByte, sevenByte, sevenByte)) should !== (7.toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenByte, sevenByte, sevenByte)) should !== (6.toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(sevenByte, sevenByte, sevenByte)) should !== (5.toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should !== ((-9).toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should !== ((-8).toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should !== ((-7).toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should !== ((-6).toByte +- 2.toByte) }
      intercept[TestFailedException] { all (List(minusSevenByte, minusSevenByte, minusSevenByte)) should !== ((-5).toByte +- 2.toByte) }
    }
  }

  object `The X +- Y syntax` {

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative` {

      // Double +- Double
      val caught1 = intercept[IllegalArgumentException] {
        all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- -0.2)
      }
      assert(caught1.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.", caught1.getMessage)

      // Double +- Float
      val caught2 = intercept[IllegalArgumentException] {
        all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- -0.2f)
      }
      assert(caught2.getMessage === "-0.20000000298023224 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Long
      val caught3 = intercept[IllegalArgumentException] {
        all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- -2L)
      }
      assert(caught3.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Int
      val caught4 = intercept[IllegalArgumentException] {
        all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- -2)
      }
      assert(caught4.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Short
      val caught5 = intercept[IllegalArgumentException] {
        all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- (-2).toShort)
      }
      assert(caught5.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Byte
      val caught6 = intercept[IllegalArgumentException] {
        all (List(sevenDotOh, sevenDotOh, sevenDotOh)) should === (7.1 +- (-2).toByte)
      }
      assert(caught6.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Float
      val caught7 = intercept[IllegalArgumentException] {
        all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- -0.2f)
      }
      assert(caught7.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Long
      val caught8 = intercept[IllegalArgumentException] {
        all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- -2L)
      }
      assert(caught8.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Int
      val caught9 = intercept[IllegalArgumentException] {
        all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- -2)
      }
      assert(caught9.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Short
      val caught10 = intercept[IllegalArgumentException] {
        all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- (-2).toShort)
      }
      assert(caught10.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Byte
      val caught11 = intercept[IllegalArgumentException] {
        all (List(sevenDotOhFloat, sevenDotOhFloat, sevenDotOhFloat)) should === (7.1f +- (-2).toByte)
      }
      assert(caught11.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Long
      val caught12 = intercept[IllegalArgumentException] {
        all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- -2L)
      }
      assert(caught12.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Int
      val caught13 = intercept[IllegalArgumentException] {
        all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- -2)
      }
      assert(caught13.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Short
      val caught14 = intercept[IllegalArgumentException] {
        all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- (-2).toShort)
      }
      assert(caught14.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Byte
      val caught15 = intercept[IllegalArgumentException] {
        all (List(sevenLong, sevenLong, sevenLong)) should === (9L +- (-2).toByte)
      }
      assert(caught15.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Int
      val caught16 = intercept[IllegalArgumentException] {
        all (List(sevenInt, sevenInt, sevenInt)) should === (9 +- -2)
      }
      assert(caught16.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Short
      val caught17 = intercept[IllegalArgumentException] {
        all (List(sevenInt, sevenInt, sevenInt)) should === (9 +- (-2).toShort)
      }
      assert(caught17.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Byte
      val caught18 = intercept[IllegalArgumentException] {
        all (List(sevenInt, sevenInt, sevenInt)) should === (9 +- (-2).toByte)
      }
      assert(caught18.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Short
      val caught19 = intercept[IllegalArgumentException] {
        all (List(sevenShort, sevenShort, sevenShort)) should === (9.toShort +- (-2).toShort)
      }
      assert(caught19.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Byte
      val caught20 = intercept[IllegalArgumentException] {
        all (List(sevenShort, sevenShort, sevenShort)) should === (9.toShort +- (-2).toByte)
      }
      assert(caught20.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Byte +- Byte
      val caught21 = intercept[IllegalArgumentException] {
        all (List(sevenByte, sevenByte, sevenByte)) should === (9.toByte +- (-2).toByte)
      }
      assert(caught21.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")
    }
  }
}
