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
    (PosInt.from(pos).get, r)
  }
  def nextPosZInt: (PosZInt, Randomizer) = {
    val (i, r) = next(31) // 31 ensures sign bit is 0
    (PosZInt.from(i).get, r)
  }
  def nextPosLong: (PosLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val candidate = (ia.toLong << 32) + ib
    val pos = if (candidate == 0L) 1L else candidate
    (PosLong.from(pos).get, rb)
  }
  def nextPosZLong: (PosZLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val pos = (ia.toLong << 32) + ib
    (PosLong.from(pos).get, rb)
  }
  def nextPosFloat: (PosFloat, Randomizer) = {
    val (f, r) = nextFloat
    val candidate = f.abs // 0.0f or greater
    val pos = if (candidate <= 1.0f) candidate else candidate + 1.0f
    (PosFloat.from(pos).get, r)
  }
  def nextPosZFloat: (PosZFloat, Randomizer) = {
    val (f, r) = nextFloat
    val pos = f.abs // 0.0f or greater
    (PosZFloat.from(pos).get, r)
  }
  def nextPosDouble: (PosDouble, Randomizer) = {
    val (d, r) = nextDouble
    val candidate = d.abs // 0.0 or greater
    val pos = if (candidate <= 1.0) candidate else candidate + 1.0
    (PosDouble.from(pos).get, r)
  }
  def nextPosZDouble: (PosZDouble, Randomizer) = {
    val (d, r) = nextDouble
    val pos = d.abs // 0.0 or greater
    (PosZDouble.from(pos).get, r)
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
  def nextList[T](length: Int)(implicit genOfT: Generator[T]): (List[T], Randomizer) = {
    require(length >= 0, "; the length passed to nextString must be >= 0")
    @tailrec
    def loop(acc: List[T], count: Int, nextRnd: Randomizer): (List[T], Randomizer) = {
      if (count == length) (acc, nextRnd)
      else {
        val (o, _, r) = genOfT.next(length, Nil, nextRnd)
        loop(o :: acc, count + 1, r)
      }
    }
    loop(List.empty, 0, thisRandomizer)
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
        (PosInt.from(nextValue).get, nextRnd) // Here should use unsafeFrom because proven above
      else {
        val nextBetween = (nextValue % (max - min + 1)) + min
        (PosInt.from(nextBetween).get, nextRnd) // And also here. Should be proven by the math that it is positive.
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
}

object Randomizer {
  def default(): Randomizer =
    new Randomizer(
      (System.currentTimeMillis() ^ 0x5DEECE66DL) & ((1L << 48) - 1)
    )
  // Note, this method where you pass the seed in will produce edges in always the same order, so it 
  // is completely predictable. Maybe I should offer a way to let people customize edges too I suppose.
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


