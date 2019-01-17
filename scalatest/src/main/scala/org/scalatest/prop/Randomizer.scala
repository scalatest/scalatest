/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.prop

import org.scalactic.anyvals._
import scala.annotation.tailrec
import org.scalactic.Requirements._

/**
  * Provide random values, of many different types.
  *
  * This is loosely inspired by [[java.util.Random]] (and is designed to produce the same values in
  * conventional cases), with two major differences:
  *
  *  - It provides random values for many more types;
  *  - In proper Scala fashion, this class is immutable.
  *
  * On the first of those points, this returns many data types, including many of the tightly-defined
  * numeric types from Scalactic. These allow you to put tight constraints on precisely what numbers
  * you want to have available -- positive, negative, zeroes, infinities and so on. We strongly recommend
  * that you use the function that most exactly describes the values you are looking for.
  *
  * That second point is the more important one. You shouldn't call the same [[Randomizer]] over and
  * over, the way you would do in Java. Instead, each call to a [[Randomizer]] function returns the
  * ''next'' [[Randomizer]], which you should use for the next call.
  *
  * @param seed
  */
class Randomizer(private[scalatest] val seed: Long) { thisRandomizer =>

  /**
    * Computes the next Randomizer to use.
    *
    * Since Randomizer is immutable (and would thus return the same value over and over),
    * you don't usually use the same instance more than once. Instead,
    * you should fetch the next one, and use that for the next operation.
    *
    * You usually don't need to call this directly; instead, the next Randomizer is returned from
    * each operation, along with the random value.
    *
    * @return The next Randomizer, ready to use.
    */
  def nextRandomizer: Randomizer = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    new Randomizer(newSeed)
  }

  /**
    * Get a certain number of random bits.
    *
    * You don't usually call this directly. Instead, call more-specific functions such as
    * [[nextInt]], which calls this for the correct number of bits and then casts that
    * to the desired type.
    *
    * @param bits The number of random bits you need.
    * @return The random bits, and the next Randomizer to user.
    */
  def next(bits: Int): (Int, Randomizer) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val newInt = (newSeed >>> (48 - bits)).toInt
    (newInt, new Randomizer(newSeed))
  }

  /**
    * Get a random 8-bit Byte.
    *
    * @return A random Byte, and the next Randomizer to use.
    */
  def nextByte: (Byte, Randomizer) = {
    val (i, r) = next(8) 
    (i.toByte, r)
  }

  /**
    * Get a random 16-bit Short.
    *
    * @return A random Short, and the next Randomizer to use.
    */
  def nextShort: (Short, Randomizer) = {
    val (i, r) = next(16) 
    (i.toShort, r)
  }

  /**
    * Get a random 16-bit Char.
    *
    * Note: this intentionally avoids invalid Unicode chars between 0xD800 and 0xDFFF;
    * when one of those is generated, this instead returns a Char
    * between 0x0000 and 0x00FF. So this function slightly favors that first
    * code block. (Which is the most common one in practice, anyway.)
    *
    * @return A random Char, and the next Randomizer to use.
    */
  def nextChar: (Char, Randomizer) = {
    val (i, r) = thisRandomizer.next(16) 
    if (i >= 0xD800 && i <= 0xDFFF) (((i - 0xD800) & 0xFF).toChar, r)
    else (i.toChar, r)
  }

  /**
    * Get a random 32-bit Int.
    *
    * @return A random Int, and the next Randomizer to use.
    */
  def nextInt: (Int, Randomizer) = next(32)

  /**
    * Get a random 64-bit Long.
    *
    * @return A random Long, and the next Randomizer to use.
    */
  def nextLong: (Long, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(32)
    val (ib, rb) = ra.next(32)
    ((ia.toLong << 32) + ib, rb)
  }

  /**
    * Get a random Float between 0 and 1.
    *
    * When working with Floats, you often want a value between 0 and 1 -- this sort
    * of proportion is one of the more common use cases for Floats. This function
    * makes it easy to grab one.
    *
    * @return A random Float in that range, and the next Randomizer to use.
    */
  def nextFloatBetween0And1: (Float, Randomizer) = {
    val (i, r) = thisRandomizer.next(24)
    (i / ((1 << 24).toFloat), r)
  }

  /**
    * Get a random Float.
    *
    * This will randomize the sign, exponent and mantissa, so it can return any possible Float.
    *
    * @return A random Float, and the next Randomizer to use.
    */
  def nextFloat: (Float, Randomizer) = { // Uses same algorithm as ScalaCheck for this one
    val (s, rs) = chooseInt(0, 1)
    val (e, re) = chooseInt(0, 0xfe)
    val (m, rm) = chooseInt(0, 0x7fffff)
    (java.lang.Float.intBitsToFloat((s << 31) | (e << 23) | m), rm)
  }

  /**
    * Get a random Double between 0 and 1.
    *
    * When working with Double, you often want a value between 0 and 1 -- this sort
    * of proportion is a common use case for Double. This function
    * makes it easy to grab one.
    *
    * @return A random Double in that range, and the next Randomizer to use.
    */
  def nextDoubleBetween0And1: (Double, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(26)
    val (ib, rb) = ra.next(27)
    (((ia.toLong << 27) + ib) / (1L << 53).toDouble, rb)
  }

  /**
    * Get a random Double.
    *
    * This will randomize the sign, exponent and mantissa, so it can return any possible Double.
    *
    * @return A random Double, and the next Randomizer to use.
    */
  def nextDouble: (Double, Randomizer) = { // Uses same algorithm as ScalaCheck for this one
    val (s, rs) = thisRandomizer.chooseLong(0L, 1L)
    val (e, re) = rs.chooseLong(0L, 0x7feL)
    val (m, rm) = re.chooseLong(0L, 0xfffffffffffffL)
    (java.lang.Double.longBitsToDouble((s << 63) | (e << 52) | m), rm)
  }

  /**
    * Get a random Integer greater than zero.
    *
    * Note: if the underlying algorithm tries to generate 0, this generates 1, so it is
    * very slightly biased towards returning 1.
    *
    * @return A random positive Integer, and the next Randomizer to use.
    */
  def nextPosInt: (PosInt, Randomizer) = {
    val (i, r) = next(31) // 31 ensures sign bit is 0
    val pos = if (i == 0) 1 else i
    (PosInt.ensuringValid(pos), r)
  }

  /**
    * Get a random Integer greater than or equal to zero.
    *
    * @return A random positive Integer (or zero), and the next Randomizer to use.
    */
  def nextPosZInt: (PosZInt, Randomizer) = {
    val (i, r) = next(31) // 31 ensures sign bit is 0
    (PosZInt.ensuringValid(i), r)
  }

  /**
    * Get a random Long greater than zero.
    *
    * @return A random positive Long, and the next Randomizer to use.
    */
  def nextPosLong: (PosLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val candidate = (ia.toLong << 32) + ib
    val pos = if (candidate == 0L) 1L else candidate
    (PosLong.ensuringValid(pos), rb)
  }

  /**
    * Get a random Long greater than or equal to zero.
    *
    * @return A random positive Long (or zero), and the next Randomizer to use.
    */
  def nextPosZLong: (PosZLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val pos = (ia.toLong << 32) + ib
    (PosZLong.ensuringValid(pos), rb)
  }

  /**
    * Get a random Float greater than zero.
    *
    * Note: it is possible (although rare) for this to return [[Float.PositiveInfinity]].
    * If you want to avoid that, use [[nextPosFiniteFloat]] instead.
    *
    * @return A random positive Float, and the next Randomizer to use.
    */
  def nextPosFloat: (PosFloat, Randomizer) = {
    val (f, r) = nextFloat
    val candidate = f.abs // 0.0f or greater
    val pos = if (candidate <= 1.0f) candidate else candidate + 1.0f
    (PosFloat.ensuringValid(pos), r)
  }

  /**
    * Get a random, finite Float greater than zero.
    *
    * This methods guards against returning [[Float.PositiveInfinity]] -- you will
    * always receive a real value.
    *
    * @return A random positive Float, and the next Randomizer to use.
    */
  def nextPosFiniteFloat: (PosFiniteFloat, Randomizer) = {
    val (n, r) = nextFloat
    val posFinite =
      n match {
        case 0.0F => Float.MinPositiveValue
        case -0.0F => -Float.MinPositiveValue
        case Float.PositiveInfinity => Float.MaxValue
        case Float.NegativeInfinity => Float.MaxValue
        case v if v < 0.0F => -v
        case _ => n
      }
    (PosFiniteFloat.ensuringValid(posFinite), r)
  }

  /**
    * Get a random Float greater than or equal to zero.
    *
    * Note: it is possible (although rare) for this to return [[Float.PositiveInfinity]].
    * If you want to avoid that, use [[nextPosZFiniteFloat]] instead.
    *
    * @return A random positive Float, and the next Randomizer to use.
    */
  def nextPosZFloat: (PosZFloat, Randomizer) = {
    val (f, r) = nextFloat
    val pos = f.abs // 0.0f or greater
    (PosZFloat.ensuringValid(pos), r)
  }

  /**
    * Get a random non-infinite Float.
    *
    * This can return either a positive or negative value, or zero, but guards against
    * returning either [[Float.PositiveInfinity]] or [[Float.NegativeInfinity]].
    *
    * @return A random finite Float, and the next Randomizer to use.
    */
  def nextFiniteFloat: (FiniteFloat, Randomizer) = {
    val (n, r) = nextFloat
    val finite =
      n match {
        case Float.PositiveInfinity => Float.MaxValue
        case Float.NegativeInfinity => Float.MaxValue
        case _ => n
      }
    (FiniteFloat.ensuringValid(finite), r)
  }

  /**
    * Get a random non-infinite Double.
    *
    * This can return either a positive or negative value, or zero, but guards against
    * returning either [[Double.PositiveInfinity]] or [[Double.NegativeInfinity]].
    *
    * @return A random finite Double, and the next Randomizer to use.
    */
  def nextFiniteDouble: (FiniteDouble, Randomizer) = {
    val (n, r) = nextDouble // TODO: Study nextFloat and nextDouble to see if it produces NaNs or Infinities.
    val finite =            // See if it produces non-normal (less than max precision) values
      n match {
        case Double.PositiveInfinity => Double.MaxValue
        case Double.NegativeInfinity => Double.MaxValue
        case _ => n
      }
    (FiniteDouble.ensuringValid(finite), r)
  }

  /**
    * Get a random Float greater than or equal to zero.
    *
    * This guards against returning [[Float.PositiveInfinity]].
    *
    * @return A random positive Float, and the next Randomizer to use.
    */
  def nextPosZFiniteFloat: (PosZFiniteFloat, Randomizer) = {
    val (n, r) = nextFloat
    val posZFinite =
      n match {
        case Float.PositiveInfinity => Float.MaxValue
        case Float.NegativeInfinity => Float.MaxValue
        case v if v < 0.0F => -v
        case _ => n
      }
    (PosZFiniteFloat.ensuringValid(posZFinite), r)
  }

  /**
    * Get a random Double greater than zero.
    *
    * Note: it is possible (although rare) for this to return [[Double.PositiveInfinity]].
    * If you want to avoid that, use [[nextPosFiniteDouble]] instead.
    *
    * @return A random positive Double, and the next Randomizer to use.
    */
  def nextPosDouble: (PosDouble, Randomizer) = {
    val (d, r) = nextDouble
    val candidate = d.abs // 0.0 or greater
    val pos = if (candidate == 0.0) Double.MinPositiveValue else candidate
    (PosDouble.ensuringValid(pos), r)
  }

  /**
    * Get a random Double greater than zero.
    *
    * This guards against returning [[Double.PositiveInfinity]], so you can have confidence that the returned number
    * is real and finite.
    *
    * @return A random positive Double, and the next Randomizer to use.
    */
  def nextPosFiniteDouble: (PosFiniteDouble, Randomizer) = {
    val (d, r) = nextDouble
    val posFinite =
      d match {
        case 0.0 => Double.MinPositiveValue
        case -0.0 => Double.MinPositiveValue
        case Double.PositiveInfinity => Double.MaxValue
        case Double.NegativeInfinity => Double.MaxValue
        case v if v < 0.0 => -v
        case _ => d
      }
    (PosFiniteDouble.ensuringValid(posFinite), r)
  }

  /**
    * Get a random non-zero Double.
    *
    * This can return any Double ''except'' zero.
    *
    * Note that this can return infinite values; if you want to avoid that, use
    * [[nextNonZeroFiniteDouble]] instead.
    *
    * @return A random non-zero Double, and the next Randomizer to use.
    */
  def nextNonZeroDouble: (NonZeroDouble, Randomizer) = {
    val (d, r) = nextDouble
    val nonZero = if (d == 0.0 || d == -0.0) Double.MinPositiveValue else d
    (NonZeroDouble.ensuringValid(nonZero), r)
  }

  /**
    * Get a random non-zero Double.
    *
    * This can return any Double ''except'' zero, [[Double.PositiveInfinity]], or
    * [[Double.NegativeInfinity]].
    *
    * @return A random non-zero Double, and the next Randomizer to use.
    */
  def nextNonZeroFiniteDouble: (NonZeroFiniteDouble, Randomizer) = {
    val (d, r) = nextDouble
    val nonZeroFinite =
      d match {
        case 0.0 => Double.MinPositiveValue
        case -0.0 => -Double.MinPositiveValue
        case Double.PositiveInfinity => Double.MaxValue
        case Double.NegativeInfinity => Double.MinValue
        case v if v > 0.0 => -v
        case _ => d
      }
    (NonZeroFiniteDouble.ensuringValid(nonZeroFinite), r)
  }

  /**
    * Get a random non-zero Float.
    *
    * This can return any Float ''except'' zero.
    *
    * Note that this can return infinite values; if you want to avoid that, use
    * [[nextNonZeroFiniteFloat]] instead.
    *
    * @return A random non-zero Float, and the next Randomizer to use.
    */
  def nextNonZeroFloat: (NonZeroFloat, Randomizer) = {
    val (f, r) = nextFloat
    val nonZero = if (f == 0.0F || f == -0.0F) Float.MinPositiveValue else f
    (NonZeroFloat.ensuringValid(nonZero), r)
  }

  /**
    * Get a random non-zero Float.
    *
    * This can return any Float ''except'' zero, [[Float.PositiveInfinity]], or
    * [[Float.NegativeInfinity]].
    *
    * @return A random non-zero Float, and the next Randomizer to use.
    */
  def nextNonZeroFiniteFloat: (NonZeroFiniteFloat, Randomizer) = {
    val (n, r) = nextFloat
    val nonZeroFinite =
      n match {
        case 0.0F => Float.MinPositiveValue
        case -0.0F => -Float.MinPositiveValue
        case Float.PositiveInfinity => Float.MaxValue
        case Float.NegativeInfinity => Float.MinValue
        case v if v > 0.0F => -v
        case _ => n
      }
    (NonZeroFiniteFloat.ensuringValid(nonZeroFinite), r)
  }

  /**
    * Get a random non-zero Int.
    *
    * This can return any Int ''except'' zero.
    *
    * @return A random non-zero Int, and the next Randomizer to use.
    */
  def nextNonZeroInt: (NonZeroInt, Randomizer) = {
    val (i, r) = nextInt
    val nonZero = if (i == 0) 1 else i
    (NonZeroInt.ensuringValid(nonZero), r)
  }

  /**
    * Get a random non-zero Long.
    *
    * This can return any Long ''except'' zero.
    *
    * @return A random non-zero Long, and the next Randomizer to use.
    */
  def nextNonZeroLong: (NonZeroLong, Randomizer) = {
    val (i, r) = nextLong
    val nonZero = if (i == 0) 1 else i
    (NonZeroLong.ensuringValid(nonZero), r)
  }

  /**
    * Get a random Double less than zero.
    *
    * Note: it is possible (although rare) for this to return [[Double.NegativeInfinity]].
    * If you want to avoid that, use [[nextNegFiniteDouble]] instead.
    *
    * @return A random negative Double, and the next Randomizer to use.
    */
  def nextNegDouble: (NegDouble, Randomizer) = {
    val (d, r) = nextDouble
    val neg =
      d match {
        case 0.0 => -Double.MinPositiveValue
        case -0.0 => -Double.MinPositiveValue
        case v if v > 0.0 => -v
        case _ => d
      }
    (NegDouble.ensuringValid(neg), r)
  }

  /**
    * Get a random Double less than zero.
    *
    * This guards against returning [[Double.NegativeInfinity]], so you can have confidence that the returned number
    * is real and finite.
    *
    * @return A random negative Double, and the next Randomizer to use.
    */
  def nextNegFiniteDouble: (NegFiniteDouble, Randomizer) = {
    val (d, r) = nextDouble
    val negFinite =
      d match {
        case 0.0 => -Double.MinPositiveValue
        case -0.0 => -Double.MinPositiveValue
        case Double.PositiveInfinity => Double.MinValue
        case Double.NegativeInfinity => Double.MinValue
        case v if v > 0.0 => -v
        case _ => d
      }
    (NegFiniteDouble.ensuringValid(negFinite), r)
  }

  /**
    * Get a random Float less than zero.
    *
    * Note: it is possible (although rare) for this to return [[Float.NegativeInfinity]].
    * If you want to avoid that, use [[nextNegFiniteFloat]] instead.
    *
    * @return A random negative Float, and the next Randomizer to use.
    */
  def nextNegFloat: (NegFloat, Randomizer) = {
    val (f, r) = nextFloat
    val neg =
      f match {
        case 0.0F => -Float.MinPositiveValue
        case -0.0F => -Float.MinPositiveValue
        case v if v > 0.0F => -v
        case _ => f
      }
    (NegFloat.ensuringValid(neg), r)
  }

  /**
    * Get a random Float less than zero.
    *
    * This guards against returning [[Float.NegativeInfinity]], so you can have confidence that the returned number
    * is real and finite.
    *
    * @return A random negative Float, and the next Randomizer to use.
    */
  def nextNegFiniteFloat: (NegFiniteFloat, Randomizer) = {
    val (n, r) = nextFloat
    val negFinite =
      n match {
        case 0.0 => -Float.MinPositiveValue
        case -0.0 => -Float.MinPositiveValue
        case Float.PositiveInfinity => Float.MinValue
        case Float.NegativeInfinity => Float.MinValue
        case v if v > 0.0 => -v
        case _ => n
      }
    (NegFiniteFloat.ensuringValid(negFinite), r)
  }

  /**
    * Get a random Int less than zero.
    *
    * @return A random negative Int, and the next Randomizer to use.
    */
  def nextNegInt: (NegInt, Randomizer) = {
    val (n, r) = nextInt
    val neg =
      n match {
        case 0 => -1
        case v if v > 0 => -v
        case _ => n
      }
    (NegInt.ensuringValid(neg), r)
  }

  /**
    * Get a random Long less than zero.
    *
    * @return A random negative Long, and the next Randomizer to use.
    */
  def nextNegLong: (NegLong, Randomizer) = {
    val (n, r) = nextLong
    val neg =
      n match {
        case 0L => -1L
        case v if v > 0L => -v
        case _ => n
      }
    (NegLong.ensuringValid(neg), r)
  }

  /**
    * Get a random Double less than or equal to zero.
    *
    * Note: it is possible (although rare) for this to return [[Double.NegativeInfinity]].
    * If you want to avoid that, use [[nextNegZFiniteDouble]] instead.
    *
    * @return A random negative-or-zero Double, and the next Randomizer to use.
    */
  def nextNegZDouble: (NegZDouble, Randomizer) = {
    val (d, r) = nextDouble
    val negZ = if (d > 0.0) -d else d
    (NegZDouble.ensuringValid(negZ), r)
  }

  /**
    * Get a random Double less than or equal to zero.
    *
    * This guards against [[Double.NegativeInfinity]], so it will always return a non-infinite value.
    *
    * @return A random negative-or-zero Double, and the next Randomizer to use.
    */
  def nextNegZFiniteDouble: (NegZFiniteDouble, Randomizer) = {
    val (d, r) = nextDouble
    val negFinite =
      d match {
        case Double.PositiveInfinity => Double.MinValue
        case Double.NegativeInfinity => Double.MinValue
        case v if v > 0.0 => -v
        case _ => d
      }
    (NegZFiniteDouble.ensuringValid(negFinite), r)
  }

  /**
    * Get a random Float less than or equal to zero.
    *
    * Note: it is possible (although rare) for this to return [[Float.NegativeInfinity]].
    * If you want to avoid that, use [[nextNegZFiniteFloat]] instead.
    *
    * @return A random negative-or-zero Float, and the next Randomizer to use.
    */
  def nextNegZFloat: (NegZFloat, Randomizer) = {
    val (n, r) = nextFloat
    val negZ = if (n > 0.0F) -n else n
    (NegZFloat.ensuringValid(negZ), r)
  }

  /**
    * Get a random Float less than or equal to zero.
    *
    * This guards against [[Float.NegativeInfinity]], so it will always return a non-infinite value.
    *
    * @return A random negative-or-zero Float, and the next Randomizer to use.
    */
  def nextNegZFiniteFloat: (NegZFiniteFloat, Randomizer) = {
    val (n, r) = nextFloat
    val negZFinite =
      n match {
        case Float.PositiveInfinity => Float.MinValue
        case Float.NegativeInfinity => Float.MinValue
        case v if v > 0.0 => -v
        case _ => n
      }
    (NegZFiniteFloat.ensuringValid(negZFinite), r)
  }

  /**
    * Get a random Int less than or equal to zero.
    *
    * @return A random negative-or-zero Int, and the next Randomizer to use.
    */
  def nextNegZInt: (NegZInt, Randomizer) = {
    val (n, r) = nextInt
    val negZ = if (n > 0) -n else n
    (NegZInt.ensuringValid(negZ), r)
  }

  /**
    * Get a random Long less than or equal to zero.
    *
    * @return A random negative-or-zero Long, and the next Randomizer to use.
    */
  def nextNegZLong: (NegZLong, Randomizer) = {
    val (n, r) = nextLong
    val negZ = if (n > 0L) -n else n
    (NegZLong.ensuringValid(negZ), r)
  }

  /**
    * Get a random Double greater than or equal to zero.
    *
    * Note: it is possible (although rare) for this to return [[Double.NegativeInfinity]].
    * If you want to avoid that, use [[nextPosZFiniteDouble]] instead.
    *
    * @return A random positive Double, and the next Randomizer to use.
    */
  def nextPosZDouble: (PosZDouble, Randomizer) = {
    val (d, r) = nextDouble
    val pos = d.abs // 0.0 or greater
    (PosZDouble.ensuringValid(pos), r)
  }

  /**
    * Get a random Double greater than or equal to zero.
    *
    * This guards against returning [[Double.NegativeInfinity]].
    *
    * @return A random negative Double, and the next Randomizer to use.
    */
  def nextPosZFiniteDouble: (PosZFiniteDouble, Randomizer) = {
    val (d, r) = nextDouble
    val posZFinite =
      d match {
        case Double.PositiveInfinity => Double.MaxValue
        case Double.NegativeInfinity => Double.MaxValue
        case v if v < 0.0 => -v
        case _ => d
      }
    (PosZFiniteDouble.ensuringValid(posZFinite), r)
  }

  /**
    * Get a random String.
    *
    * This takes a desired length, and generates a String of that length. Do not expect this String to
    * make any sense -- it is literally just a string of random Unicode characters, and will typically
    * contain little or no conventional ASCII.
    *
    * @param length The number of characters to include in the String.
    * @return A highly-random String of the specified length, and the next Randomizer to use.
    */
  // Maybe add in some > 16 bit UTF-16 encodings
  def nextString(length: PosZInt): (String, Randomizer) = {
    @tailrec
    def loop(acc: List[Char], count: Int, nextRnd: Randomizer): (String, Randomizer) = {
      if (count == length.value) (acc.mkString, nextRnd)
      else {
        val (c, r) = nextRnd.nextChar
        loop(c :: acc, count + 1, r)
      }
    }
    loop(List.empty, 0, thisRandomizer)
  }

  /**
    * Get a random list of elements of type [[T]].
    *
    * This function requires an implicit parameter that says how to generate values of [[T]], as well
    * as the number of them to generate. (Which may be zero.)
    *
    * @param length How many values to generate for the List.
    * @param genOfT A [[Generator]], that provides values of type [[T]].
    * @tparam T The type to generate.
    * @return A List of values of the desired type.
    */
  def nextList[T](length: PosZInt)(implicit genOfT: Generator[T]): (List[T], Randomizer) = {
    @tailrec
    def loop(acc: List[T], count: Int, nextRnd: Randomizer): (List[T], Randomizer) = {
      if (count == length.value) (acc, nextRnd)
      else {
        val (o, _, r) = genOfT.next(SizeParam(PosZInt(0), length, length), Nil, nextRnd) // Because starts at 0 and goes to a max value of type Int
        loop(o :: acc, count + 1, r)
      }
    }
    loop(List.empty, 0, thisRandomizer)
  }

  /**
    * Given a range of Chars, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseChar(from: Char, to: Char): (Char, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextChar
      val nextBetween = min + (nextValue % (max - min + 1)).abs
      (nextBetween.toChar, nextRnd)
    }
  }

  /**
    * Given a range of Bytes, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseByte(from: Byte, to: Byte): (Byte, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextByte
      val nextBetween = min + (nextValue % (max - min + 1)).abs
      (nextBetween.toByte, nextRnd)
    }
  }

  /**
    * Given a range of Shorts, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseShort(from: Short, to: Short): (Short, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextShort
      val nextBetween = min + (nextValue % (max - min + 1)).abs
      (nextBetween.toShort, nextRnd)
    }
  }

  /**
    * Given a range of Ints, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseInt(from: Int, to: Int): (Int, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // generate a positive Int
      val (nextValue, nextRnd) = next(31) // 31 ensures sign bit is 0
      val nextBetween =
        // Special case -- we need to guard against this, because it winds up overflowing and throwing in the modulo
        // in the normal case. This isn't a problem with the smaller types, because they get widened to Int for the math:
        if ((min == Int.MinValue) && (max == Int.MaxValue))
          nextValue
        else
          (nextValue % (max - min + 1)) + min
      (nextBetween, nextRnd)
    }
  }

  // TODO: chooseFloat(), chooseDouble() and at least some of their variants are broken if various edge cases are specified as values.
  // See Issue: https://github.com/scalatest/scalatest/issues/1473
  //
  // Note that those functions likely can and should be simplified (the problem was discovered when I tried to simplify as I've done for
  // the integral chooseXX() functions, and then realized that it's not so simple), but should generally be rethought, and quite
  // possibly heavily refactored.
  //
  // These functions are scattered throughout the rest of this file -- there are a lot of them. It isn't obvious offhand which
  // ones need improvement.

  /**
    * Given a range of Floats, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseFloat(from: Float, to: Float): (Float, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (nextBetween, nextNextRnd)
      }
    }
  }

  /**
    * Given a range of positive Floats, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
 def choosePosFloat(from: PosFloat, to: PosFloat): (PosFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (PosFloat.ensuringValid(nextBetween), nextNextRnd)
      }
    }
  }

  /**
    * Given a range of positive, finite Floats, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosFiniteFloat(from: PosFiniteFloat, to: PosFiniteFloat): (PosFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = finiteFloatBetweenAlgorithm(between0And1, min, max)
        (PosFiniteFloat.ensuringValid(nextBetween), nextNextRnd)
      }
    }
  }

  /**
    * Given a range of positive Floats (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosZFloat(from: PosZFloat, to: PosZFloat): (PosZFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (PosZFloat.ensuringValid(nextBetween), nextNextRnd)
      }
    }
  }

  /**
    * Given a range of positive, finite Floats (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosZFiniteFloat(from: PosZFiniteFloat, to: PosZFiniteFloat): (PosZFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        //val nextBetween = min + (nextValue % (max - min)).abs
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = finiteFloatBetweenAlgorithm(between0And1, min, max)
        (PosZFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of Doubles, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseDouble(from: Double, to: Double): (Double, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (nextBetween, nextNextRnd)
      }
    }
  }

  /**
    * Given a range of positive Ints, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosInt(from: PosInt, to: PosInt): (PosInt, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // generate a positive Int
      val (nextValue, nextRnd) = next(31) // 31 ensures sign bit is 0

      val nextBetween = (nextValue % (max - min + 1)) + min
      (PosInt.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of positive Ints (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosZInt(from: PosZInt, to: PosZInt): (PosZInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // generate a positive Int
      val (nextValue, nextRnd) = next(31) // 31 ensures sign bit is 0

      val nextBetween = (nextValue % (max - min + 1)) + min
      (PosZInt.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of Longs, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseLong(from: Long, to: Long): (Long, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // Generate a positive Long (positive because we are using it as a kicker to the
      // modulus below):
      val (nextValue, nextRnd) = nextPosZLong

      // See chooseInt():
      val nextBetween =
        if ((min == Long.MinValue) && (max == Long.MaxValue))
          nextValue.value
        else
          (nextValue % (max - min + 1)) + min
      (nextBetween, nextRnd)
    }
  }

  /**
    * Given a range of positive Longs, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosLong(from: PosLong, to: PosLong): (PosLong, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // Generate a positive Long
      val (nextValue, nextRnd) = nextPosZLong

      val nextBetween = (nextValue % (max - min + 1)) + min
      (PosLong.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of positive Longs (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosZLong(from: PosZLong, to: PosZLong): (PosZLong, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // Generate a positive Long
      val (nextValue, nextRnd) = nextPosZLong

      val nextBetween = (nextValue % (max - min + 1)) + min
      (PosZLong.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of positive Doubles, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosDouble(from: PosDouble, to: PosDouble): (PosDouble, Randomizer) = {
    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (PosDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of positive finite Doubles, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosFiniteDouble(from: PosFiniteDouble, to: PosFiniteDouble): (PosFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (PosFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of positive Doubles (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosZDouble(from: PosZDouble, to: PosZDouble): (PosZDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (PosZDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of positive, finite Doubles (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def choosePosZFiniteDouble(from: PosZFiniteDouble, to: PosZFiniteDouble): (PosZFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (PosZFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative Ints, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegInt(from: NegInt, to: NegInt): (NegInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegInt

      val nextBetween = (nextValue % (max - min + 1)).abs + min
      (NegInt.ensuringValid(nextBetween), nextRnd)
    }
  }

    /**
      * Given a range of negative Longs, chooses one of them randomly.
      *
      * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
      * will cope appropriately if they are in reverse order.
      *
      * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
      *
      * @param from One end of the range to select from.
      * @param to The other end of the range.
      * @return A value from that range, inclusive of the ends.
      */
  def chooseNegLong(from: NegLong, to: NegLong): (NegLong, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegLong

      val nextBetween = (nextValue % (max - min + 1)).abs + min
      (NegLong.ensuringValid(nextBetween), nextRnd)
    }
  }

    /**
      * Given a range of negative Floats, chooses one of them randomly.
      *
      * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
      * will cope appropriately if they are in reverse order.
      *
      * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
      *
      * @param from One end of the range to select from.
      * @param to The other end of the range.
      * @return A value from that range, inclusive of the ends.
      */
  def chooseNegFloat(from: NegFloat, to: NegFloat): (NegFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (NegFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative, finite Floats, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegFiniteFloat(from: NegFiniteFloat, to: NegFiniteFloat): (NegFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = finiteFloatBetweenAlgorithm(between0And1, min, max)
        (NegFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative Doubles, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegDouble(from: NegDouble, to: NegDouble): (NegDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (NegDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative, finite Doubles, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegFiniteDouble(from: NegFiniteDouble, to: NegFiniteDouble): (NegFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (NegFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative Ints (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegZInt(from: NegZInt, to: NegZInt): (NegZInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegZInt

      val nextBetween = (nextValue % (max - min + 1)).abs + min
      (NegZInt.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of negative Longs (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegZLong(from: NegZLong, to: NegZLong): (NegZLong, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegZLong

      val nextBetween = (nextValue % (max - min + 1)).abs + min
      (NegZLong.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of negative Floats (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegZFloat(from: NegZFloat, to: NegZFloat): (NegZFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (NegZFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative, finite Floats (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegZFiniteFloat(from: NegZFiniteFloat, to: NegZFiniteFloat): (NegZFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = finiteFloatBetweenAlgorithm(between0And1, min, max)
        (NegZFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative Doubles (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegZDouble(from: NegZDouble, to: NegZDouble): (NegZDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (NegZDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of negative, finite Doubles (maybe including zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNegZFiniteDouble(from: NegZFiniteDouble, to: NegZFiniteDouble): (NegZFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (NegZFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of Ints (excluding zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * In order to avoid returning 0, this function is very slightly biased towards returning 1 instead.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNonZeroInt(from: NonZeroInt, to: NonZeroInt): (NonZeroInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNonZeroInt

      // See chooseInt():
      val nextBetween =
        if ((min == Int.MinValue) && (max == Int.MaxValue))
          nextValue.value
        else
          (nextValue % (max - min + 1)).abs + min

      if (nextBetween == 0)
        (NonZeroInt(1), nextRnd)
      else
        (NonZeroInt.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of Longs (excluding zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * In order to avoid returning 0, this function is very slightly biased towards returning 1 instead.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNonZeroLong(from: NonZeroLong, to: NonZeroLong): (NonZeroLong, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNonZeroLong

      // See chooseInt():
      val nextBetween: Long =
        if ((min == Long.MinValue) && (max == Long.MaxValue))
          nextValue.value
        else
          (nextValue % (max - min + 1)).abs + min

      if (nextBetween == 0L)
        (NonZeroLong(1L), nextRnd)
      else
        (NonZeroLong.ensuringValid(nextBetween), nextRnd)
    }
  }

  /**
    * Given a range of Floats (excluding zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * In order to avoid returning 0, this function is very slightly biased towards returning 1 instead.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNonZeroFloat(from: NonZeroFloat, to: NonZeroFloat): (NonZeroFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        if (nextBetween == 0.0f)
          (NonZeroFloat(1.0f), nextRnd)
        else
          (NonZeroFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given the endpoints of a finite-Float range, and a scaling factor, this computes the number that far across
    * the range.
    *
    * This deals with edge cases that can result in infinities in the math.
    *
    * @param between0And1 A scaling factor between 0 and 1.
    * @param min The lower end of the range.
    * @param max The upper end of the range.
    * @return The Float that is the scaling factor distance across that range.
    */
  def finiteFloatBetweenAlgorithm(between0And1: Float, min: Float, max: Float): Float = {
    if (min < 0.0f && max > 0.0f) {
      val maxMinusMin = max - min
      if (maxMinusMin.isInfinity) {
        val neg = between0And1 * min
        val pos = between0And1 * max
        min + neg + pos
      }
      else
        min + (between0And1 * maxMinusMin).abs
    }
    else
      min + (between0And1 * (max - min)).abs
  }

  /**
    * Given a range of finite Floats (excluding zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * In order to avoid returning 0, this function is very slightly biased towards returning 1 instead.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNonZeroFiniteFloat(from: NonZeroFiniteFloat, to: NonZeroFiniteFloat): (NonZeroFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = finiteFloatBetweenAlgorithm(between0And1, min, max)
        if (nextBetween == 0.0f)
          (NonZeroFiniteFloat(1.0f), nextRnd)
        else
          (NonZeroFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of Doubles (excluding zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * In order to avoid returning 0, this function is very slightly biased towards returning 1 instead.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNonZeroDouble(from: NonZeroDouble, to: NonZeroDouble): (NonZeroDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        if (nextBetween == 0.0)
          (NonZeroDouble(1.0), nextRnd)
        else
          (NonZeroDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  /**
    * Given a range of finite Doubles (excluding zero), chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * In order to avoid returning 0, this function is very slightly biased towards returning 1 instead.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseNonZeroFiniteDouble(from: NonZeroFiniteDouble, to: NonZeroFiniteDouble): (NonZeroFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        if (nextBetween == 0.0)
          (NonZeroFiniteDouble(1.0), nextNextRnd)
        else
          (NonZeroFiniteDouble.ensuringValid(nextBetween), nextNextRnd)

      }
    }
  }

  /**
    * Given a range of finite Floats, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseFiniteFloat(from: FiniteFloat, to: FiniteFloat): (FiniteFloat, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextFloatBetween0And1
        val nextBetween = finiteFloatBetweenAlgorithm(between0And1, min, max)
        (FiniteFloat.ensuringValid(nextBetween), nextNextRnd)
      }
    }
  }

  /**
    * Given a range of finite Doubles, chooses one of them randomly.
    *
    * Note that, while the ''from'' parameter is usually smaller than ''to'', that is not required; the function
    * will cope appropriately if they are in reverse order.
    *
    * The choice is inclusive: either the ''from'' or ''to'' values may be returned.
    *
    * @param from One end of the range to select from.
    * @param to The other end of the range.
    * @return A value from that range, inclusive of the ends.
    */
  def chooseFiniteDouble(from: FiniteDouble, to: FiniteDouble): (FiniteDouble, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val (between0And1, nextNextRnd) = nextRnd.nextDoubleBetween0And1
        val nextBetween = min + (between0And1 * (max - min)).abs
        (FiniteDouble.ensuringValid(nextBetween), nextNextRnd)
      }
    }
  }
}

object Randomizer {

  import java.util.concurrent.atomic.AtomicReference

  /**
    * This seed is empty under ordinary circumstances. It is here so that the test
    * Runner can poke in a seed value to be used during a test run. If set, it will be used
    * as the seed for all calls to [[Randomizer.default()]].
    */
  private[scalatest] val defaultSeed: AtomicReference[Option[Long]] = new AtomicReference(None)

  /**
    * Creates a new Randomizer, whose seed is initialized based on the current time.
    *
    * This should not be considered a strong source of randomness -- in cases where high entropy really
    * matters, it's a bit mediocre -- but for general purposes it's typically good enough.
    *
    * @return A Randomizer, ready to begin producing random values.
    */
  def default(): Randomizer =
    apply(
      defaultSeed.get() match {
        case Some(seed) => seed
        case None => System.currentTimeMillis()
      }
    )

  /**
    * A Randomizer, initialized with the specified seed value.
    *
    * Since Randomizer is only pseudo-random, and is actually deterministic based on the seed, this lets
    * you re-create a set of "random" values. If you use the same seed over and over, you will get the
    * same values.
    *
    * This is particularly useful during testing and debugging. If you save the seed that is used to
    * generate your initial data, you can then re-run the test using that seed to reliably re-create
    * your "random" events.
    *
    * If you want to create an adequate seed to feed into here, the value of [[System.currentTimeMillis()]] is
    * reasonable (and is used in [[Randomizer.default()]]). It's a somewhat weak seed, but decent for most
    * purposes.
    *
    * @param seed A number that will be used to initialize a new Randomizer.
    * @return A Randomizer, ready to begin producing random values.
    */
  def apply(seed: Long): Randomizer = new Randomizer((seed ^ 0x5DEECE66DL) & ((1L << 48) - 1))

  /**
    * Randomizes the order of the provided List.
    *
    * This takes a pre-created Randomizer, that you pass in. This means that, if you re-create a Randomizer
    * with the same seed over and over, you will get the same shuffled order.
    *
    * This takes O(n) time.
    *
    * @param xs A List of values.
    * @param rnd A Randomizer that will be used to shuffle the values.
    * @tparam T The type of the passed-in values.
    * @return The List of values, rearranged in random order.
    */
  def shuffle[T](xs: List[T], rnd: Randomizer): (List[T], Randomizer) = {

    import scala.collection.mutable.ArrayBuffer

    val buf = ArrayBuffer.empty[T]
    buf ++= xs

    def swap(i: Int, j: Int) {
      val tmp = buf(i)
      buf(i) = buf(j)
      buf(j) = tmp
    }

    var nextRnd = rnd

    for (n <- buf.length to 2 by -1) {
      val (ni, nr) = rnd.nextInt
      nextRnd = nr
      val k = ni.abs % n
      swap(n - 1, k)
    }

    (buf.toList, nextRnd)
  }
}
