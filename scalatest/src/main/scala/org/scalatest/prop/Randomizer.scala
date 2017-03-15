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

// Wrote this class by looking at the Javadoc of java.util.Random.
// And by testing its behavior against that of java.util.Random.
// Maybe this should be a trait, so that people can, hmm. Could 
// make subclasses with extra methods, like nextSmallInt or something,
// and in a pattern match narrow the type and call that method.
class Randomizer(private[scalatest] val seed: Long) { thisRandomizer =>
  def nextRandomizer: Randomizer = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    new Randomizer(newSeed)
  }
  def next(bits: Int): (Int, Randomizer) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val newInt = (newSeed >>> (48 - bits)).toInt
    (newInt, new Randomizer(newSeed))
  }
  def nextByte: (Byte, Randomizer) = {
    val (i, r) = next(8) 
    (i.toByte, r)
  }
  def nextShort: (Short, Randomizer) = {
    val (i, r) = next(16) 
    (i.toShort, r)
  }
  // When an invalid Unicode char between 0xD800 and 0xDFFF is generated, just
  // return a character between 0x0000 and 0x00FF. These characters are more
  // common in practice anyway. So this generator does favor slightly
  // the first code block.
  def nextChar: (Char, Randomizer) = {
    val (i, r) = thisRandomizer.next(16) 
    if (i >= 0xD800 && i <= 0xDFFF) (((i - 0xD800) & 0xFF).toChar, r)
    else (i.toChar, r)
  }
  def nextInt: (Int, Randomizer) = next(32) 
  def nextLong: (Long, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(32)
    val (ib, rb) = ra.next(32)
    ((ia.toLong << 32) + ib, rb)
  }
  def nextFloatBetween0And1: (Float, Randomizer) = {
    val (i, r) = thisRandomizer.next(24)
    (i / ((1 << 24).toFloat), r)
  }
  def nextFloat: (Float, Randomizer) = { // Use same algorithm as ScalaCheck for this one
    val (s, rs) = chooseInt(0, 1)
    val (e, re) = chooseInt(0, 0xfe)
    val (m, rm) = chooseInt(0, 0x7fffff)
    (java.lang.Float.intBitsToFloat((s << 31) | (e << 23) | m), rm)
  }
  def nextDoubleBetween0And1: (Double, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(26)
    val (ib, rb) = ra.next(27)
    (((ia.toLong << 27) + ib) / (1L << 53).toDouble, rb)
  }
  def nextDouble: (Double, Randomizer) = { // Use same algorithm as ScalaCheck for this one
    val (s, rs) = thisRandomizer.chooseLong(0L, 1L)
    val (e, re) = rs.chooseLong(0L, 0x7feL)
    val (m, rm) = re.chooseLong(0L, 0xfffffffffffffL)
    (java.lang.Double.longBitsToDouble((s << 63) | (e << 52) | m), rm)
  }
  def nextPosInt: (PosInt, Randomizer) = {
    val (i, r) = next(31) // 31 ensures sign bit is 0
    val pos = if (i == 0) 1 else i
    (PosInt.ensuringValid(pos), r)
  }
  def nextPosZInt: (PosZInt, Randomizer) = {
    val (i, r) = next(31) // 31 ensures sign bit is 0
    (PosZInt.ensuringValid(i), r)
  }
  def nextPosLong: (PosLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val candidate = (ia.toLong << 32) + ib
    val pos = if (candidate == 0L) 1L else candidate
    (PosLong.ensuringValid(pos), rb)
  }
  def nextPosZLong: (PosZLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val pos = (ia.toLong << 32) + ib
    (PosLong.ensuringValid(pos), rb)
  }
  def nextPosFloat: (PosFloat, Randomizer) = {
    val (f, r) = nextFloat
    val candidate = f.abs // 0.0f or greater
    val pos = if (candidate <= 1.0f) candidate else candidate + 1.0f
    (PosFloat.ensuringValid(pos), r)
  }
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
  def nextPosZFloat: (PosZFloat, Randomizer) = {
    val (f, r) = nextFloat
    val pos = f.abs // 0.0f or greater
    (PosZFloat.ensuringValid(pos), r)
  }
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
  def nextPosDouble: (PosDouble, Randomizer) = {
    val (d, r) = nextDouble
    val candidate = d.abs // 0.0 or greater
    val pos = if (candidate <= 1.0) candidate else candidate + 1.0 // TODO: Is this correct? If so, document why, because it looks wrong to me.
    (PosDouble.ensuringValid(pos), r)
  }
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
  def nextNonZeroDouble: (NonZeroDouble, Randomizer) = {
    val (d, r) = nextDouble
    val nonZero = if (d == 0.0 || d == -0.0) Double.MinPositiveValue else d
    (NonZeroDouble.ensuringValid(nonZero), r)
  }
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
  def nextNonZeroFloat: (NonZeroFloat, Randomizer) = {
    val (f, r) = nextFloat
    val nonZero = if (f == 0.0F || f == -0.0F) Float.MinPositiveValue else f
    (NonZeroFloat.ensuringValid(nonZero), r)
  }
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
  def nextNonZeroInt: (NonZeroInt, Randomizer) = {
    val (i, r) = nextInt
    val nonZero = if (i == 0) 1 else i
    (NonZeroInt.ensuringValid(nonZero), r)
  }
  def nextNonZeroLong: (NonZeroLong, Randomizer) = {
    val (i, r) = nextLong
    val nonZero = if (i == 0) 1 else i
    (NonZeroLong.ensuringValid(nonZero), r)
  }
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
  def nextNegZDouble: (NegZDouble, Randomizer) = {
    val (d, r) = nextDouble
    val negZ = if (d > 0.0) -d else d
    (NegZDouble.ensuringValid(negZ), r)
  }
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
  def nextNegZFloat: (NegZFloat, Randomizer) = {
    val (n, r) = nextFloat
    val negZ = if (n > 0.0F) -n else n
    (NegZFloat.ensuringValid(negZ), r)
  }
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
  def nextNegZInt: (NegZInt, Randomizer) = {
    val (n, r) = nextInt
    val negZ = if (n > 0) -n else n
    (NegZInt.ensuringValid(negZ), r)
  }
  def nextNegZLong: (NegZLong, Randomizer) = {
    val (n, r) = nextLong
    val negZ = if (n > 0L) -n else n
    (NegZLong.ensuringValid(negZ), r)
  }
  def nextPosZDouble: (PosZDouble, Randomizer) = {
    val (d, r) = nextDouble
    val pos = d.abs // 0.0 or greater
    (PosZDouble.ensuringValid(pos), r)
  }
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
  // Maybe add in some > 16 bit UTF-16 encodings
  def nextString(length: Int): (String, Randomizer) = {
    require(length >= 0, "; the length passed to nextString must be >= 0")
    @tailrec
    def loop(acc: List[Char], count: Int, nextRnd: Randomizer): (String, Randomizer) = {
      if (count == length) (acc.mkString, nextRnd)
      else {
        val (c, r) = nextRnd.nextChar
        loop(c :: acc, count + 1, r)
      }
    }
    loop(List.empty, 0, thisRandomizer)
  }
  // TODO: Not sure if we should have a nextList here because it ties Randomizer to Generator.
  // And length should be a PosZInt, which will then simplify the line of code 7 down from here, the genOfT.next one.
  def nextList[T](length: Int)(implicit genOfT: Generator[T]): (List[T], Randomizer) = {
    require(length >= 0, "; the length passed to nextString must be >= 0")
    @tailrec
    def loop(acc: List[T], count: Int, nextRnd: Randomizer): (List[T], Randomizer) = {
      if (count == length) (acc, nextRnd)
      else {
        val (o, _, r) = genOfT.next(SizeParam(PosZInt(0), PosZInt.ensuringValid(length), PosZInt.ensuringValid(length)), Nil, nextRnd) // Because starts at 0 and goes to a max value of type Int
        loop(o :: acc, count + 1, r)
      }
    }
    loop(List.empty, 0, thisRandomizer)
  }

  def chooseChar(from: Char, to: Char): (Char, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextChar
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min + 1)).abs
        (nextBetween.toChar, nextRnd)
      }
    }
  }

  def chooseByte(from: Byte, to: Byte): (Byte, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextByte
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min + 1)).abs
        (nextBetween.toByte, nextRnd)
      }
    }
  }

  def chooseShort(from: Short, to: Short): (Short, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextShort
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min + 1)).abs
        (nextBetween.toShort, nextRnd)
      }
    }
  }

  def chooseInt(from: Int, to: Int): (Int, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // generate a positive Int
      val nextPair = next(31) // 31 ensures sign bit is 0
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = (nextValue % (max - min + 1)) + min
        (nextBetween, nextRnd)
      }
    }
  }

  def chooseFloat(from: Float, to: Float): (Float, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (nextBetween, nextRnd)
      }
    }
  }

  def choosePosFloat(from: PosFloat, to: PosFloat): (PosFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def choosePosFiniteFloat(from: PosFiniteFloat, to: PosFiniteFloat): (PosFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def choosePosZFloat(from: PosZFloat, to: PosZFloat): (PosZFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosZFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def choosePosZFiniteFloat(from: PosZFiniteFloat, to: PosZFiniteFloat): (PosZFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosZFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseDouble(from: Double, to: Double): (Double, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (nextBetween, nextRnd)
      }
    }
  }

  def choosePosInt(from: PosInt, to: PosInt): (PosInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // generate a positive Int
      val (nextValue, nextRnd) = next(31) // 31 ensures sign bit is 0

      if (nextValue >= min && nextValue <= max)
        (PosInt.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)) + min
        (PosInt.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def choosePosZInt(from: PosZInt, to: PosZInt): (PosZInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // generate a positive Int
      val (nextValue, nextRnd) = next(31) // 31 ensures sign bit is 0

      if (nextValue >= min && nextValue <= max)
        (PosZInt.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)) + min
        (PosZInt.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseLong(from: Long, to: Long): (Long, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // Generate a positive Long
      val (ia, nextRnd) = thisRandomizer.next(31) // 31 ensures sign bit is 0
      val (ib, nextNextRnd) = nextRnd.next(32)
      val nextValue = (ia.toLong << 32) + ib

      if (nextValue >= min && nextValue <= max)
        (nextValue, nextNextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)) + min
        (nextBetween, nextNextRnd)
      }
    }
  }

  def choosePosLong(from: PosLong, to: PosLong): (PosLong, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // Generate a positive Long
      val (ia, nextRnd) = thisRandomizer.next(31) // 31 ensures sign bit is 0
      val (ib, nextNextRnd) = nextRnd.next(32)
      val nextValue = (ia.toLong << 32) + ib

      if (nextValue >= min && nextValue <= max)
        (PosLong.ensuringValid(nextValue), nextNextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)) + min
        (PosLong.ensuringValid(nextBetween), nextNextRnd)
      }
    }
  }

  def choosePosZLong(from: PosZLong, to: PosZLong): (PosZLong, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      // Generate a positive Long
      val (ia, nextRnd) = thisRandomizer.next(31) // 31 ensures sign bit is 0
      val (ib, nextNextRnd) = nextRnd.next(32)
      val nextValue = (ia.toLong << 32) + ib

      if (nextValue >= min && nextValue <= max)
        (PosZLong.ensuringValid(nextValue), nextNextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)) + min
        (PosZLong.ensuringValid(nextBetween), nextNextRnd)
      }
    }
  }

  def choosePosDouble(from: PosDouble, to: PosDouble): (PosDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def choosePosFiniteDouble(from: PosFiniteDouble, to: PosFiniteDouble): (PosFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def choosePosZDouble(from: PosZDouble, to: PosZDouble): (PosZDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosZDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def choosePosZFiniteDouble(from: PosZFiniteDouble, to: PosZFiniteDouble): (PosZFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer) // TODO: Shouldn't this be thisRandomizer because I didn't use it? I am trying this in choosePosInt.
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextPosZFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (PosZFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegInt(from: NegInt, to: NegInt): (NegInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegInt

      if (nextValue >= min && nextValue <= max)
        (NegInt.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)).abs + min
        (NegInt.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegLong(from: NegLong, to: NegLong): (NegLong, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegLong

      if (nextValue >= min && nextValue <= max)
        (NegLong.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)).abs + min
        (NegLong.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

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
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

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
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

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
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

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
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegZInt(from: NegZInt, to: NegZInt): (NegZInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegZInt

      if (nextValue >= min && nextValue <= max)
        (NegZInt.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)).abs + min
        (NegZInt.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegZLong(from: NegZLong, to: NegZLong): (NegZLong, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNegZLong

      if (nextValue >= min && nextValue <= max)
        (NegZLong.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)).abs + min
        (NegZLong.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegZFloat(from: NegZFloat, to: NegZFloat): (NegZFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegZFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegZFiniteFloat(from: NegZFiniteFloat, to: NegZFiniteFloat): (NegZFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegZFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegZDouble(from: NegZDouble, to: NegZDouble): (NegZDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegZDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNegZFiniteDouble(from: NegZFiniteDouble, to: NegZFiniteDouble): (NegZFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNegZFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (NegZFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNonZeroInt(from: NonZeroInt, to: NonZeroInt): (NonZeroInt, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNonZeroInt

      if (nextValue >= min && nextValue <= max)
        (NonZeroInt.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween = (nextValue % (max - min + 1)).abs + min
        if (nextBetween == 0)
          (NonZeroInt(1), nextRnd)
        else
          (NonZeroInt.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNonZeroLong(from: NonZeroLong, to: NonZeroLong): (NonZeroLong, Randomizer) = {

    if (from == to) {
      (from, thisRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val (nextValue, nextRnd) = nextNonZeroLong

      if (nextValue >= min && nextValue <= max)
        (NonZeroLong.ensuringValid(nextValue), nextRnd)
      else {
        val nextBetween: Long = (nextValue % (max - min + 1)).abs + min
        if (nextBetween == 0L)
          (NonZeroLong(1L), nextRnd)
        else
          (NonZeroLong.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNonZeroFloat(from: NonZeroFloat, to: NonZeroFloat): (NonZeroFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        if (nextBetween == 0.0f)
          (NonZeroFloat(1.0f), nextRnd)
        else
          (NonZeroFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNonZeroFiniteFloat(from: NonZeroFiniteFloat, to: NonZeroFiniteFloat): (NonZeroFiniteFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        if (nextBetween == 0.0f)
          (NonZeroFiniteFloat(1.0f), nextRnd)
        else
          (NonZeroFiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNonZeroDouble(from: NonZeroDouble, to: NonZeroDouble): (NonZeroDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        if (nextBetween == 0.0)
          (NonZeroDouble(1.0), nextRnd)
        else
          (NonZeroDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseNonZeroFiniteDouble(from: NonZeroFiniteDouble, to: NonZeroFiniteDouble): (NonZeroFiniteDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextNonZeroFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        if (nextBetween == 0.0)
          (NonZeroFiniteDouble(1.0), nextRnd)
        else
          (NonZeroFiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseFiniteFloat(from: FiniteFloat, to: FiniteFloat): (FiniteFloat, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextFiniteFloat
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (FiniteFloat.ensuringValid(nextBetween), nextRnd)
      }
    }
  }

  def chooseFiniteDouble(from: FiniteDouble, to: FiniteDouble): (FiniteDouble, Randomizer) = {

    if (from == to) {
      (from, nextRandomizer)
    }
    else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      val nextPair = nextFiniteDouble
      val (nextValue, nextRnd) = nextPair

      if (nextValue >= min && nextValue <= max)
        nextPair
      else {
        val nextBetween = min + (nextValue % (max - min)).abs
        (FiniteDouble.ensuringValid(nextBetween), nextRnd)
      }
    }
  }
}

object Randomizer {

  import java.util.concurrent.atomic.AtomicReference

  private[scalatest] val defaultSeed: AtomicReference[Option[Long]] = new AtomicReference(None)

  def default(): Randomizer =
    apply(
      defaultSeed.get() match {
        case Some(seed) => seed
        case None => System.currentTimeMillis()
      }
    )

  def apply(seed: Long): Randomizer = new Randomizer((seed ^ 0x5DEECE66DL) & ((1L << 48) - 1))

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


