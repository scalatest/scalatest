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
import java.lang.Float.{intBitsToFloat, floatToIntBits}
import java.lang.Double.{longBitsToDouble, doubleToLongBits}

/**
  * Provide random values, of many different types.
  *
  * This is loosely inspired by [[java.util.Random]] (and is designed to produce the same values in
  * conventional cases), with two major differences:
  *
  * - It provides random values for many more types;
  * - In proper Scala fashion, this class is immutable.
  *
  * On the first of those points, this returns many data types, including many of the tightly-defined
  * numeric types from Scalactic. These allow you to put tight constraints on precisely what numbers
  * you want to have available -- positive, negative, zeroes, infinities and so on. We strongly recommend
  * that you use the function that most exactly describes the values you are looking for.
  *
  * That second point is the more important one. You shouldn't call the same [[Randomizer]] over and
  * over, the way you would do in Java. Instead, each call to a [[Randomizer]] function returns the
  * *next* [[Randomizer]], which you should use for the next call.
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
    /*
    0.0f is 32 zero bits. The sign is 0 for positive. The exponent of 0 is a special
    case for 0.0f and for subnormal values. It means 0 if the mantissa is also 0.
    1.0f is represented as 1.00 * (2 ** 0). The 1 to the left of the binary point
    is implicit, so this means that the mantissa is all zeros again for 1.0. An
    exponent of 0 is represented by 127 (0x7f). (To get the actual exponent from the biased
    one, subtract 127.)

    So except for 0.0f, which we'll need to handle specially, we can generate
    a random 23 bits for the mantissa, and for the exponent a number between 1 and 126 (0x7e).
    */

    // The total number of possible images for Float between 0.0, inclusive, and 1.0, exclusive, is:
    // (2 ** 23) * (126) + 1
    // 2 to the power of 23 (0x800000) is how many different mantissas can be represented in 23 bits.
    // 126 (or 0x7e) is how many exponents there are for numbers between 0.0f, exclusive, and 1.0f, exclusive.
    // 1 is for 0.0f, which we must handle specially, because its mantissas must be all zeros.

    // scala> java.lang.Integer.toHexString(0x800000 * 0x7e + 1)
    // res39: String = 3f000001

    // scala> val twoTo32D = math.pow(2, 32)
    // twoTo64D: Double = 1.8446744073709552E19
    //
    // scala> val totalPossible0To1Floats = (0x800000 * 0x7e + 1).toDouble
    // totalPossible0To1Floats: Double = 1.056964609E9
    //
    // scala> val percentSpaceUsed = totalPossible0To1Floats / twoTo32D
    // percentSpaceUsed: Double = 0.24609375023283064
    //
    // scala> val eachFloatIsWorth = 1.0 / percentSpaceUsed
    // eachFloatIsWorth: Double = 4.063492059647571

    // Essentially, the Float points between 0.0f and 1.0f consist of around 25%
    // of the space of all Float points. So we'll give 0.0f four chances to
    // win each draw.
    val (x, r) = nextInt

    // Pick four lucky numbers to play the lotto with:
    if (x == 111 || x == 333 || x == 555 || x == 777)
      (0.0f, r)
    else {
      val (e, re) = r.chooseInt(1, 0x7e)     // The exponent (8 bits, value 1 to 126)
      val (m, rm) = re.chooseInt(0, 0x7fffff) // The mantissa (23 bits)
      val f = intBitsToFloat((e << 23) | m)
      (f, rm)
    }
  }

  /**
    * Get a random Float.
    *
    * This will randomize the sign, exponent and mantissa, so it can return any possible Float.
    *
    * @return A random Float, and the next Randomizer to use.
    */
  def nextFloat: (Float, Randomizer) = {

    // The space of possible Floats
    // 2 to the power of 32, which is covered by Int.MinValue to Int.MaxValue
    // - (2 to the power of 23, where 23 is the number of mantissa bits)
    // + (3 for the two infinities and one NaN)

    // scala> val twoTo32 = math.pow(2, 32).toLong
    // twoTo32: Long = 4294967296
    // 
    // scala> val twoTo23 = math.pow(2, 23).toLong
    // twoTo23: Long = 8388608
    // 
    // scala> val totalSizeOfSpace = twoTo32 - twoTo23 + 3
    // totalSizeOfSpace: Long = 4286578691

    // What percentage of the total space of Ints is used by Floats?
    // scala> val twoTo32D = twoTo32.toDouble
    // twoTo32D: Double = 4.294967296E9
    // 
    // scala> val twoTo23D = twoTo23.toDouble
    // twoTo23D: Double = 8388608.0
    // 
    // scala> val percentSpaceUsed = (twoTo32D - twoTo23D) / twoTo32D
    // percentSpaceUsed: Double = 0.998046875
    //
    // Each float is worth a little more than an Int but not much:
    // scala> val eachFloatIsWorth = 1.0 / percentSpaceUsed
    // eachFloatIsWorth: Double = 1.0019569471624266

    // Thus we can randomly pick one number out of the nextInt and decide that's NaN
    val (x, r) = nextInt

    // Pick one lucky number to play the lotto with:
    if (x == 999) (Float.NaN, r)
    else r.nextExtRealFloatValue
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
    /*
    0.0 is 64 zero bits. The sign is 0 for positive. The exponent of 0 is a special
    case for 0.0 and for subnormal values. It means 0 if the mantissa is also 0.
    1.0 is represented as 1.00 * (2 ** 0). The 1 to the left of the binary point
    is implicit, so this means that the mantissa is all zeros again for 1.0. An
    exponent of 0 is represented by 1023 (0x3ff). (To get the actual exponent from the biased
    one, subtract 1023.)

    So except for 0.0, which we'll need to handle specially, we can generate
    a random 52 bits for the mantissa, and for the exponent a number between 1 and 1022 (0x3fe).
    */

    // scala> java.lang.Long.toHexString(math.pow(2, 52).toLong)
    // res17: String = 10000000000000

    // The total number of possible images for Double between 0.0, inclusive, and 1.0, exclusive, is:
    // (2 ** 52) * (1022) + 1
    // 2 to the power of 52 (0x10000000000000L) is how many different mantissas can be represented in 52 bits.
    // 1022 (or 0x3fe) is how many exponents there are for numbers between 0.0, exclusive, and 1.0, exclusive.
    // 1 is for 0.0, which we must handle specially, because its mantissas must be all zeros.

    // scala> val twoTo64D = math.pow(2, 64)
    // twoTo64D: Double = 1.8446744073709552E19
    // 
    // scala> val totalPossible0To1Doubles = (0x10000000000000L * 0x3fe + 1).toDouble
    // totalPossible0To1Doubles: Double = 4.6026788191726469E18
    // 
    // scala> val percentSpaceUsed = totalPossible0To1Doubles / twoTo64D
    // percentSpaceUsed: Double = 0.24951171875
    // 
    // scala> val eachDoubleIsWorth = 1.0 / percentSpaceUsed
    // eachDoubleIsWorth: Double = 4.007827788649706

    // Essentially, the Double points between 0.0 and 1.0 consist of around 25%
    // of the space of all Double points. So we'll give 0.0 four chances to
    // win each draw.
    val (x, r) = nextLong

    // Pick one and a third lucky numbers to play the lotto with:
    if (x == 1111L || x == 3333L || x == 5555L || x == 7777L)
      (0.0, r)
    else {
      val (e, re) = r.chooseLong(1, 0x3fe)     // The exponent (8 bits, value 1 to 126)
      val (m, rm) = re.chooseLong(0, 0x10000000000000L) // The mantissa (23 bits)
      val f = longBitsToDouble((e << 52) | m)
      (f, rm)
    }
  }

  /**
    * Get a random Double.
    *
    * This will randomize the sign, exponent and mantissa, so it can return any possible Double.
    *
    * @return A random Double, and the next Randomizer to use.
    */
  def nextDouble: (Double, Randomizer) = { // Uses same algorithm as ScalaCheck for this one
    // The space of possible Doubles
    // 2 to the power of 64, which is covered by Long.MinValue to Long.MaxValue
    // - (2 to the power of 52, where 52 is the number of mantissa bits)
    // + (3 for the two infinities and one NaN)

    // scala> val twoTo64D = math.pow(2, 64)
    // twoTo64D: Double = 1.8446744073709552E19
    // 
    // scala> val twoTo52D = math.pow(2, 52)
    // twoTo52D: Double = 4.503599627370496E15
    // 
    // What percentage of the total space of Longs is used by Doubles?
    // scala> val percentSpaceUsed = (twoTo64D - twoTo52D) / twoTo64D
    // percentSpaceUsed: Double = 0.999755859375
    // 
    // Each Double value is worth a little more than an Long but not much:
    // scala> val eachDoubleIsWorth = 1.0 / percentSpaceUsed
    // eachDoubleIsWorth: Double = 1.0002442002442002

    // Thus we can randomly pick one number out of the nextLong and decide that's NaN
    val (x, r) = nextLong

    // Pick one lucky number to play the lotto with:
    if (x == 999L)
      (Double.NaN, r)
    else r.nextExtRealDoubleValue
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

  // This is needed to distinguish negative and positive
  // zero floats because -0.0f == 0.0f
  private def isNegativeZeroFloat(f: Float): Boolean = {
    // scala> java.lang.Float.floatToIntBits(-0.0f)
    // res34: Int = -2147483648
    // 
    val NegativeZeroFloatBits: Int = -2147483648
    val bits: Int = floatToIntBits(f)
    bits == NegativeZeroFloatBits
  }

  // This is needed to distinguish negative and positive
  // zero doubles because -0.0 == 0.0
  private def isNegativeZeroDouble(d: Double): Boolean = {
    // scala> java.lang.Double.doubleToLongBits(-0.0)
    // res46: Long = -9223372036854775808
    // 
    val NegativeZeroDoubleBits: Long = -9223372036854775808L
    val bits: Long = doubleToLongBits(d)
    bits == NegativeZeroDoubleBits
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
    val (f, r) = nextPosZFloat
    val candidate = f.value
    val pos = forcePosFloat(candidate)
    (PosFloat.ensuringValid(pos), r)
  }

  private def forcePosFloat(candidate: Float): Float = {
    // A PosZFloat cannot be -0.0f, but that equals 0.0f also
    // scala> -0.0f == 0.0f
    // res37: Boolean = true
    if (candidate == 0.0F) Float.MinPositiveValue
    else candidate
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
    val (f, r) = nextPosZFiniteFloat
    val candidate = f.value
    val posFinite = forcePosFloat(candidate)
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
    // scala> Float.NegativeInfinity.abs
    // res30: Float = Infinity
    //
    // scala> -0.0f.abs
    // res31: Float = 0.0
    //
    // scala> PosZFloat(-0.0f)
    // res0: org.scalactic.anyvals.PosZFloat = PosZFloat(-0.0f)
    val (candidate, r) = nextExtRealFloatValue
    val posZ = forcePosZFloat(candidate)
    (PosZFloat.ensuringValid(posZ), r)
  }

  private def nextBit: (Int, Randomizer) = {
    val (i, rs) = nextInt
    (i & 1, rs)
  }

  /**
    * Get a random non-infinite Float.
    *
    * This can return either a positive or negative value, or zero, but guards against
    * returning either [[Float.PositiveInfinity]], [[Float.NegativeInfinity]], or [[Float.NaN]].
    *
    * @return A random finite Float, and the next Randomizer to use.
    */
  def nextFiniteFloat: (FiniteFloat, Randomizer) = {
    // The exponent portion of a Float occupies 8 bits. It can be 0 (which represents an exponent of -127)
    // to 255 (which is a reserved value for NaN values and +/- infinity). The highest regular (non-reserved)
    // exponent therefore is 254, which in hex is 0xfe (which represents an exponent of +127).
    // Thus by chosing the exponent Int between 0 and 0xfe, we can't get a NaN or an infinity.
    val (s, rs) = nextBit                   // The sign bit (1 bit)
    val (e, re) = rs.chooseInt(0, 0xfe)     // The exponent (8 bits)
    val (m, rm) = re.chooseInt(0, 0x7fffff) // The mantissa (23 bits)
    val finite = intBitsToFloat((s << 31) | (e << 23) | m)
    (FiniteFloat.ensuringValid(finite), rm)
  }

  private def nextFiniteFloatValue: (Float, Randomizer) = {
    val (ff, r) = nextFiniteFloat
    (ff.value, r)
  }

  // An extended real float includes all the numeric ones plus positive and negative infinity
  // In other words, the whole space other than the NaNs. This may be a missing abstraction in anyvals.
  // Let's think about that.
  private def nextExtRealFloatValue: (Float, Randomizer) = {

    // See the comment in nextFloat for an explanation of why we just
    // grab nextInt and pick two values out of it for +/- infinity
    val (x, r) = nextInt

    // Pick two lucky numbers to play the lotto with:
    if (x == 777)
      (Float.NegativeInfinity, r)
    else if (x == 888)
      (Float.PositiveInfinity, r)
    else r.nextFiniteFloatValue
  }

  /**
    * Get a random non-infinite Double.
    *
    * This can return either a positive or negative value, or zero, but guards against
    * returning either [[Double.PositiveInfinity]], [[Double.NegativeInfinity]], or [[Double.NaN]].
    *
    * @return A random finite Double, and the next Randomizer to use.
    */
  def nextFiniteDouble: (FiniteDouble, Randomizer) = {
    // The exponent portion of a Double occupies 11 bits. It can be 0 (which represents an exponent of -1023)
    // to 2047 (which is a reserved value for NaN values and +/- infinity). The highest regular (non-reserved)
    // exponent therefore is 2046, which in hex is 0x7fe (which represents an exponent of +1023).
    // Thus by chosing the exponent Int between 0 and 0x7fe, we can't get a NaN or an infinity.
    val (s, rs) = nextBit                             // The sign bit (1 bit)
    val (e, re) = rs.chooseLong(0L, 0x7feL)           // The exponent (11 bits)
    val (m, rm) = re.chooseLong(0L, 0xfffffffffffffL) // The mantissa (52 bits)
    val finite = longBitsToDouble((s.toLong << 63) | (e << 52) | m)
    (FiniteDouble.ensuringValid(finite), rm)
  }

  private def nextFiniteDoubleValue: (Double, Randomizer) = {
    val (fd, r) = nextFiniteDouble
    (fd.value, r)
  }

  // An extended real float includes all the numeric ones plus positive and negative infinity
  // In other words, the whole space other than the NaNs. This may be a missing abstraction in anyvals.
  // Let's think about that.
  private def nextExtRealDoubleValue: (Double, Randomizer) = {

    // See the comment in nextDouble for an explanation of why we just
    // grab nextLong and pick two values out of it for +/- infinity
    val (x, r) = nextLong

    // Pick two lucky numbers to play the lotto with:
    if (x == 777L)
      (Double.NegativeInfinity, r)
    else if (x == 888L)
      (Double.PositiveInfinity, r)
    else r.nextFiniteDoubleValue
  }

  private def forcePosZFloat(candidate: Float): Float = {
    if (isNegativeZeroFloat(candidate)) candidate // leave it negative zero
    else candidate.abs // 0.0f or greater
  }

  /**
    * Get a random Float greater than or equal to zero.
    *
    * This guards against returning [[Float.PositiveInfinity]].
    *
    * @return A random positive Float, and the next Randomizer to use.
    */
  def nextPosZFiniteFloat: (PosZFiniteFloat, Randomizer) = {
    val (candidate, r) = nextFiniteFloatValue
    val posZFinite = forcePosZFloat(candidate)
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
    val (d, r) = nextPosZDouble
    val candidate = d.value
    val pos = forcePosDouble(candidate)
    (PosDouble.ensuringValid(pos), r)
  }

  private def forcePosDouble(candidate: Double): Double = {
    // A PosZDouble can be -0.0, but that equals 0.0 also
    // scala> -0.0 == 0.0
    // res37: Boolean = true
    if (candidate == 0.0) Double.MinPositiveValue
    else candidate
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
    val (d, r) = nextPosZFiniteDouble
    val candidate = d.value
    val posFinite = forcePosDouble(candidate)
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
    val (candidate, r) = nextExtRealDoubleValue
    val nonZero = forceNonZeroDoubleValue(candidate)
    (NonZeroDouble.ensuringValid(nonZero), r)
  }

  private def forceNonZeroDoubleValue(candidate: Double): Double =  {
    if (isNegativeZeroDouble(candidate)) -Double.MinPositiveValue
    else if (candidate == 0.0) Double.MinPositiveValue
    else candidate
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
    val (candidate, r) = nextFiniteDoubleValue
    val nonZeroFinite = forceNonZeroDoubleValue(candidate)
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
    val (candidate, r) = nextExtRealFloatValue
    val nonZero = forceNonZeroFloatValue(candidate)
    (NonZeroFloat.ensuringValid(nonZero), r)
  }

  private def forceNonZeroFloatValue(candidate: Float): Float =  {
    if (isNegativeZeroFloat(candidate)) -Float.MinPositiveValue
    else if (candidate == 0.0F) Float.MinPositiveValue
    else candidate
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
    val (candidate, r) = nextFiniteFloatValue
    val nonZeroFinite = forceNonZeroFloatValue(candidate)
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
    val (d, r) = nextNegZDouble
    val candidate = d.value
    val neg = forceNegDouble(candidate)
    (NegDouble.ensuringValid(neg), r)
  }

  private def forceNegDouble(candidate: Double): Double = {
    // A NegZDouble can be +0.0, but that equals -0.0 also
    // scala> 0.0 == -0.0
    // res38: Boolean = true
    if (candidate == -0.0) -Double.MinPositiveValue
    else candidate
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
    val (d, r) = nextNegZFiniteDouble
    val candidate = d.value
    val negFinite = forceNegDouble(candidate)
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
    val (f, r) = nextNegZFloat
    val candidate = f.value
    val neg = forceNegFloat(candidate)
    (NegFloat.ensuringValid(neg), r)
  }

  private def forceNegFloat(candidate: Float): Float = {
    // A NegZFloat can be +0.0f, but that equals -0.0f also
    // scala> 0.0f == -0.0f
    // res38: Boolean = true
    if (candidate == -0.0F) -Float.MinPositiveValue
    else candidate
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
    val (f, r) = nextNegZFiniteFloat
    val candidate = f.value
    val negFinite = forceNegFloat(candidate)
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
    // scala> NegZDouble(0.0)
    // res0: org.scalactic.anyvals.NegZDouble = NegZDouble(0.0)
    val (candidate, r) = nextExtRealDoubleValue
    val negZ = forceNegZDouble(candidate)
    (NegZDouble.ensuringValid(negZ), r)
  }

  private def forceNegZDouble(candidate: Double): Double = {
      // scala> -0.0 > 0.0
      // res42: Boolean = false
    if (candidate > 0.0) -candidate
    else candidate // This will leave positive zero as positive zero, which is what we want
  }

  /**
    * Get a random Double less than or equal to zero.
    *
    * This guards against [[Double.NegativeInfinity]], so it will always return a non-infinite value.
    *
    * @return A random negative-or-zero Double, and the next Randomizer to use.
    */
  def nextNegZFiniteDouble: (NegZFiniteDouble, Randomizer) = {
    val (candidate, r) = nextFiniteDoubleValue
    val negZFinite = forceNegZDouble(candidate)
    (NegZFiniteDouble.ensuringValid(negZFinite), r)
  }

  // TODO: probably mention that NegZ can include posivie 0.0, and that PosZ can 
  // include -0.0 in the docs.
  /**
    * Get a random Float less than or equal to zero.
    *
    * Note: it is possible (although rare) for this to return [[Float.NegativeInfinity]].
    * If you want to avoid that, use [[nextNegZFiniteFloat]] instead.
    *
    * @return A random negative-or-zero Float, and the next Randomizer to use.
    */
  def nextNegZFloat: (NegZFloat, Randomizer) = {
    // scala> NegZFloat(0.0f)
    // res0: org.scalactic.anyvals.NegZFloat = NegZFloat(0.0f)
    val (candidate, r) = nextExtRealFloatValue
    val negZ = forceNegZFloat(candidate)
    (NegZFloat.ensuringValid(negZ), r)
  }

  private def forceNegZFloat(candidate: Float): Float = {
    // scala> -0.0f > 0.0f
    // res42: Boolean = false
    if (candidate > 0.0F) -candidate
    else candidate // This will leave positive zero as positive zero, which is what we want
  }

  /**
    * Get a random Float less than or equal to zero.
    *
    * This guards against [[Float.NegativeInfinity]], so it will always return a non-infinite value.
    *
    * @return A random negative-or-zero Float, and the next Randomizer to use.
    */
  def nextNegZFiniteFloat: (NegZFiniteFloat, Randomizer) = {
    val (candidate, r) = nextFiniteFloatValue
    val negZFinite = forceNegZFloat(candidate)
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
    // scala> Double.NegativeInfinity.abs
    // res30: Double = Infinity
    //
    // scala> -0.0.abs
    // res31: Double = 0.0
    //
    // scala> PosZDouble(-0.0)
    // TODO: Re-verify all of these PosZ ones. I am copying and changing the comment.
    // res0: org.scalactic.anyvals.PosZDouble = PosZDouble(-0.0)
    val (candidate, r) = nextExtRealDoubleValue
    val posZ = forcePosZDoubleValue(candidate)
    (PosZDouble.ensuringValid(posZ), r)
  }

  private def forcePosZDoubleValue(candidate: Double): Double = {
    if (isNegativeZeroDouble(candidate)) candidate // leave it negative zero
    else candidate.abs // 0.0 or greater
  }

  /**
    * Get a random Double greater than or equal to zero.
    *
    * This guards against returning [[Double.NegativeInfinity]].
    *
    * @return A random negative Double, and the next Randomizer to use.
    */
  def nextPosZFiniteDouble: (PosZFiniteDouble, Randomizer) = {
    val (candidate, r) = nextFiniteDoubleValue
    val posZFinite = forcePosZDoubleValue(candidate)
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
    val (i, nextRnd) = chooseInt(from.toInt, to.toInt)
    (i.toChar, nextRnd)
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
    val (s, nextRnd) = chooseShort(from.toShort, to.toShort)
    (s.toByte, nextRnd)
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
      // See chooseInt for comments that explain this algo
      val min: Int = math.min(from, to).toInt // Widen both to next larger type, so max - min will not overflow
      val max: Int = math.max(from, to).toInt

      // The divisor for modulo on the random number will be one more than the absolute value of max - min
      val divisor: Int = (max - min).abs + 1

      val (dividend, nextRnd) = nextInt

      // When you add this to min, it will be a number between min and max
      // It won't overflow, because we're using Ints here not Shorts.
      val remainder: Int = dividend.abs % divisor

      // The max remainder in this case is 2 ** 16 - 1, so this won't
      // overflow even if min is Short.MinValue and remainder is 2 ** 16 - 1. 
      ((min + remainder).toShort, nextRnd)
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
    if (from == to)
      (from, thisRandomizer)
    else {
      // scala> math.min(Int.MinValue, Int.MaxValue)
      // res0: Int = -2147483648
      //
      // scala> math.max(Int.MinValue, Int.MaxValue)
      // res1: Int = 2147483647
      //
      // scala> math.min(Int.MaxValue, Int.MinValue)
      // res2: Int = -2147483648
      //
      // scala> math.max(Int.MaxValue, Int.MinValue)
      // res3: Int = 2147483647
      //
      // Thus I trust math.min and math.max to always work. I widen the result
      // to the next larger type so that max - min will not overflow. For example:
      //
      // scala> Int.MinValue - Int.MaxValue
      // res4: Int = 1
      //
      // scala> Int.MaxValue - Int.MinValue
      // res5: Int = -1
      //
      // But:
      //
      // scala> Int.MinValue.toLong - Int.MaxValue.toLong
      // res8: Long = -4294967295
      //
      // scala> Int.MaxValue.toLong - Int.MinValue.toLong
      // res9: Long = 4294967295
      val min: Long = math.min(from, to).toLong // Widen both to next larger type, so max - min will not overflow
      val max: Long = math.max(from, to).toLong

      // The divisor for modulo on the random number will be one more than the absolute value of max - min
      // For example:
      //
      // scala> Int.MaxValue.toLong - Int.MinValue.toLong + 1
      // res11: Long = 4294967296
      //
      // scala> math.pow(2, 32).toLong
      // res13: Long = 4294967296
      val divisor: Long = max - min + 1

      val (dividend, nextRnd) = nextLong

      // When you add this to min, it will be a number between min and max
      // It won't overflow, because we're using Longs here not Ints.
      val remainder: Long = dividend.abs % divisor

      // The max remainder in this case is 2 ** 32 - 1, so this won't
      // overflow even if min is Int.MinValue and remainder is 2 ** 32 - 1. 
      //
      // scala> (Int.MinValue.toLong + 4294967295L).toInt
      // res15: Int = 2147483647
      ((min + remainder).toInt, nextRnd)
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
    // The IEEE-754 floating point spec arranges things such that given any two positive Floats (including
    // positive 0.0), a and b, a < b iff floatToIntBits(a) < floatToIntBits(b).
    // 
    // The bits comparison of positive Floats works for +0.0f, +infinity, and +NaN. All of
    // All of those special values are arranged in a total order that places 0.0f as the smallest, 
    // positive infinity just higher than the largest (greatest magnitude) representable positive real number,
    // and all the other left-over values at the positive extreme represents variants of +NaN.
    //
    // Thus we can just convert the from and to to bits, get an integral number between those two, and
    // convert those bits back to PosFloat.
    val (n, nextRnd) = chooseInt(floatToIntBits(from), floatToIntBits(to))
    (PosFloat.ensuringValid(intBitsToFloat(n)), nextRnd)
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
    // See choosePosFloat for a comment that explains this algo
    val (n, nextRnd) = chooseInt(floatToIntBits(from), floatToIntBits(to))
    (PosFiniteFloat.ensuringValid(intBitsToFloat(n)), nextRnd)
  }

  // This helper method is shared by choosePosZFloat, choosePosZFiniteFloat,
  // chooseNegZFloat, chooseNegZFiniteFloat. That latter two negate their from and
  // to values before invoking this method, then negate the result that this method returns.
  private def choosePositiveOrZeroFloat(from: Float, to: Float): (Float, Randomizer) = {

    // This local method assumes it will never be passed -0.0f for posFrom or posTo.
    def choosePosOrPlusZero(posOrPlusZeroFrom: Float, posOrPlusZeroTo: Float): (Float, Randomizer) = {
      // See the comment for choosePosFloat for an explanation of this algo.
      val (n, nextRnd) = chooseInt(floatToIntBits(posOrPlusZeroFrom), floatToIntBits(posOrPlusZeroTo))
      (intBitsToFloat(n), nextRnd)
    }

    // We will handle a -0.0 specially, because it is the only one that can't
    // participate in the technique of converting +0.0 to positive Floats to Int
    // bits and finding a point that way.
    val fromIsNegZero = isNegativeZeroFloat(from)
    val toIsNegZero = isNegativeZeroFloat(to)
    if (fromIsNegZero || toIsNegZero) {
      // See the comment in nextFloat for an explanation of why each Float point (distinct value)
      // is essentially worth one Int point. Here we are looking at extremely close
      // to half of the Float point space, because looking at just positive numbers,
      // positive infinity (if called for PosZFiniteFloat), and the two zeros, so each
      // positive point will be worth around two Int points.

      // Thus we can randomly pick two number out of the nextInt and decide that's -0.0.
      val (x, r) = nextInt

      // Pick two lucky numbers to play the lotto with:
      if (x == 111 || x == 555)
        (-0.0f, r)
      else {
        // Change the negative zero(s) to positive zero(s)
        val nonNegFrom = if (fromIsNegZero) 0.0f else from
        val nonNegTo = if (toIsNegZero) 0.0f else to
        choosePosOrPlusZero(nonNegFrom, nonNegTo)
      }
    }
    else choosePosOrPlusZero(from, to)
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
    val (n, nextRnd) = choosePositiveOrZeroFloat(from.value, to.value)
    (PosZFloat.ensuringValid(n), nextRnd)
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
    val (n, nextRnd) = choosePositiveOrZeroFloat(from.value, to.value)
    (PosZFiniteFloat.ensuringValid(n), nextRnd)
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
    val (i, nextRnd) = chooseInt(from.value, to.value)
    (PosInt.ensuringValid(i), nextRnd)
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
    val (i, nextRnd) = chooseInt(from.value, to.value)
    (PosZInt.ensuringValid(i), nextRnd)
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
    if (from == to)
      (from, thisRandomizer)
    else {
      // See the comments in chooseInt for an explanation of this algo
      val min: BigInt = BigInt(math.min(from, to)) // Widen both to next larger type, so max - min will not overflow
      val max: BigInt = BigInt(math.max(from, to))

      // The divisor for modulo on the random number will be one more than the absolute value of max - min
      val divisor: BigInt = max - min + 1

      val (dividend, nextRnd) = nextLong

      // When you add this to min, it will be a number between min and max
      // It won't overflow, because we're using BigInts here not Longs.
      val remainder: BigInt = dividend.abs % divisor

      // The max remainder in this case is 2 ** 32 - 1, so this won't
      // overflow even if min is Int.MinValue and remainder is 2 ** 32 - 1. 
      ((min + remainder).toLong, nextRnd)
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
    val (n, nextRnd) = chooseLong(from.value, to.value)
    (PosLong.ensuringValid(n), nextRnd)
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
    val (n, nextRnd) = chooseLong(from.value, to.value)
    (PosZLong.ensuringValid(n), nextRnd)
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
    val (i, nextRnd) = chooseInt(from.value, to.value)
    (NegInt.ensuringValid(i), nextRnd)
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
    val (n, nextRnd) = chooseLong(from.value, to.value)
    (NegLong.ensuringValid(n), nextRnd)
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
    // See choosePosFloat for a comment that explains the algo we are using. The algo
    // works for positive floats, and we have here a negative float. Negative and positive
    // floats are symmetric. For every negative float value there is a corresponding 
    // positive float value of the same magnitude. So we negate our from and to, use
    // the algorithm for positive floats, then negate the result to get a negative float
    // to return.
    val posFrom: Float = -(from.value)
    val posTo: Float = -(to.value)
    val (n, nextRnd) = chooseInt(floatToIntBits(posFrom), floatToIntBits(posTo))
    val posN: Float = intBitsToFloat(n)
    val negN: Float = -posN
    (NegFloat.ensuringValid(negN), nextRnd)
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
    // See chooseNegFloat for a comment that explains this algo
    val posFrom: Float = -(from.value)
    val posTo: Float = -(to.value)
    val (n, nextRnd) = chooseInt(floatToIntBits(posFrom), floatToIntBits(posTo))
    val posN: Float = intBitsToFloat(n)
    val negN: Float = -posN
    (NegFiniteFloat.ensuringValid(negN), nextRnd)
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
    val (i, nextRnd) = chooseInt(from.value, to.value)
    (NegZInt.ensuringValid(i), nextRnd)
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
    val (n, nextRnd) = chooseLong(from.value, to.value)
    (NegZLong.ensuringValid(n), nextRnd)
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
    // Use the algo for selecting a PosZFloat by negating from and to before invoking the algo,
    // then negating its result.
    val posFrom: Float = -(from.value)
    val posTo: Float = -(to.value)
    val (n, nextRnd) = choosePositiveOrZeroFloat(posFrom, posTo)
    (NegZFloat.ensuringValid(-n), nextRnd)
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
    // Use the algo for selecting a PosZFloat by negating from and to before invoking the algo,
    // then negating its result.
    val posFrom: Float = -(from.value)
    val posTo: Float = -(to.value)
    val (n, nextRnd) = choosePositiveOrZeroFloat(posFrom, posTo)
    (NegZFiniteFloat.ensuringValid(-n), nextRnd)
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
    val (i, nextRnd) = chooseInt(from.value, to.value)
    // If 0 is between min and max, since neither min nor max can be 0 given this from
    // and two were NonZeroInts, min must be negative and max positive. Thus the minimum
    // that max can be is 1, which is what we use here if i is 0:
    val nonZero = if (i == 0) 1 else i
    (NonZeroInt.ensuringValid(nonZero), nextRnd)
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
    val (n, nextRnd) = chooseLong(from.value, to.value)
    // If 0 is between min and max, since neither min nor max can be 0 given this from
    // and two were NonZeroLongs, min must be negative and max positive. Thus the minimum
    // that max can be is 1, which is what we use here if n is 0:
    val nonZero = if (n == 0L) 1L else n
    (NonZeroLong.ensuringValid(nonZero), nextRnd)
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
