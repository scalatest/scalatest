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
class Randomizer(seed: Long, edges: Edges) { thisRandomizer =>
  def nextRandomizer: Randomizer = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    new Randomizer(newSeed, edges)
  }
  def next(bits: Int): (Int, Randomizer) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val newInt = (newSeed >>> (48 - bits)).toInt
    (newInt, new Randomizer(newSeed, edges))
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
    val (i, r) = nextRandomizer.next(16) 
    if (i >= 0xD800 && i <= 0xDFFF) (((i - 0xD800) & 0xFF).toChar, r)
    else (i.toChar, r)
  }
  def nextInt: (Int, Randomizer) = next(32) 
  def nextLong: (Long, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(32)
    val (ib, rb) = ra.next(32)
    ((ia.toLong << 32) + ib, rb)
  }
  def nextByteWithEdges: (Byte, Randomizer) = {
    edges.byteEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(byteEdges = tail)))
      case Nil => nextByte
    }
  }
  def nextShortWithEdges: (Short, Randomizer) = {
    edges.shortEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(shortEdges = tail)))
      case Nil => nextShort
    }
  }
  def nextCharWithEdges: (Char, Randomizer) = {
    edges.charEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(charEdges = tail)))
      case Nil => nextChar
    }
  }
  def nextIntWithEdges: (Int, Randomizer) = {
    edges.intEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(intEdges = tail)))
      case Nil => nextInt
    }
  }
  def nextLongWithEdges: (Long, Randomizer) = {
    edges.longEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(longEdges = tail)))
      case Nil => nextLong
    }
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
  def nextFloatWithEdges: (Float, Randomizer) = {
    edges.floatEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(floatEdges = tail)))
      case Nil => nextFloat
    }
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
  def nextDoubleWithEdges: (Double, Randomizer) = {
    edges.doubleEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(doubleEdges = tail)))
      case Nil => nextDouble
    }
  }
  def nextPosInt: (PosInt, Randomizer) = {
    val (i, r) = next(31) // 31 ensures sign bit is 0
    val pos = if (i == 0) 1 else i
    (PosInt.from(pos).get, r)
  }
  def nextPosIntWithEdges: (PosInt, Randomizer) = {
    edges.posIntEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posIntEdges = tail)))
      case Nil => nextPosInt
    }
  }
  def nextPosZInt: (PosZInt, Randomizer) = {
    val (i, r) = next(31) // 31 ensures sign bit is 0
    (PosZInt.from(i).get, r)
  }
  def nextPosZIntWithEdges: (PosZInt, Randomizer) = {
    edges.posZIntEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posZIntEdges = tail)))
      case Nil => nextPosZInt
    }
  }
  def nextPosLong: (PosLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val candidate = (ia.toLong << 32) + ib
    val pos = if (candidate == 0L) 1L else candidate
    (PosLong.from(pos).get, rb)
  }
  def nextPosLongWithEdges: (PosLong, Randomizer) = {
    edges.posLongEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posLongEdges = tail)))
      case Nil => nextPosLong
    }
  }
  def nextPosZLong: (PosZLong, Randomizer) = {
    val (ia, ra) = thisRandomizer.next(31) // 31 ensures sign bit is 0
    val (ib, rb) = ra.next(32)
    val pos = (ia.toLong << 32) + ib
    (PosLong.from(pos).get, rb)
  }
  def nextPosZLongWithEdges: (PosZLong, Randomizer) = {
    edges.posZLongEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posZLongEdges = tail)))
      case Nil => nextPosZLong
    }
  }
  def nextPosFloat: (PosFloat, Randomizer) = {
    val (f, r) = nextFloat
    val candidate = f.abs // 0.0f or greater
    val pos = if (candidate <= 1.0f) candidate else candidate + 1.0f
    (PosFloat.from(pos).get, r)
  }
  def nextPosFloatWithEdges: (PosFloat, Randomizer) = {
    edges.posFloatEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posFloatEdges = tail)))
      case Nil => nextPosFloat
    }
  }
  def nextPosZFloat: (PosZFloat, Randomizer) = {
    val (f, r) = nextFloat
    val pos = f.abs // 0.0f or greater
    (PosZFloat.from(pos).get, r)
  }
  def nextPosZFloatWithEdges: (PosZFloat, Randomizer) = {
    edges.posZFloatEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posZFloatEdges = tail)))
      case Nil => nextPosZFloat
    }
  }
  def nextPosDouble: (PosDouble, Randomizer) = {
    val (d, r) = nextDouble
    val candidate = d.abs // 0.0 or greater
    val pos = if (candidate <= 1.0) candidate else candidate + 1.0
    (PosDouble.from(pos).get, r)
  }
  def nextPosDoubleWithEdges: (PosDouble, Randomizer) = {
    edges.posDoubleEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posDoubleEdges = tail)))
      case Nil => nextPosDouble
    }
  }
  def nextPosZDouble: (PosZDouble, Randomizer) = {
    val (d, r) = nextDouble
    val pos = d.abs // 0.0 or greater
    (PosZDouble.from(pos).get, r)
  }
  def nextPosZDoubleWithEdges: (PosZDouble, Randomizer) = {
    edges.posZDoubleEdges match {
      case head :: tail => (head, new Randomizer(seed, edges.copy(posZDoubleEdges = tail)))
      case Nil => nextPosZDouble
    }
  }
  // Maybe add in some > 16 bit UTF-16 encodings
  def nextString(length: Int): (String, Randomizer) = {
    require(length >= 0, "; the length passed to nextString must be >= 0")
    @tailrec
    def loop(acc: List[Char], count: Int, nextRandomizer: Randomizer): (String, Randomizer) = {
      if (count == length) (acc.mkString, nextRandomizer)
      else {
        val (c, r) = nextRandomizer.nextChar
        loop(c :: acc, count + 1, r)
      }
    }
    loop(List.empty, 0, thisRandomizer)
  }
  def nextList[T](length: Int)(implicit genOfT: Generator[T]): (List[T], Randomizer) = {
    require(length >= 0, "; the length passed to nextString must be >= 0")
    @tailrec
    def loop(acc: List[T], count: Int, nextRandomizer: Randomizer): (List[T], Randomizer) = {
      if (count == length) (acc, nextRandomizer)
      else {
        val (o, r) = genOfT.next(length, nextRandomizer)
        loop(o :: acc, count + 1, r)
      }
    }
    loop(List.empty, 0, thisRandomizer)
  }
  def chooseInt(from: Int, to: Int): (Int, Randomizer) = {
    if(from == to) {
      (from, this.nextInt._2)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)
      @annotation.tailrec
      def loop(state: Randomizer): (Int, Randomizer) = {
        val next = state.nextInt
        if (min <= next._1 && next._1 <= max) {
          next
        } else if(0 < (max - min)){
          val x = (next._1 % (max - min + 1)) + min
          if (min <= x && x <= max) {
            x -> next._2
          } else {
            loop(next._2)
          }
        } else {
          loop(next._2)
        }
      }
      loop(this)
    }
  }
  def chooseLong(from: Long, to: Long): (Long, Randomizer) = {
    if(from == to) {
      (from, this.nextLong._2)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)
      @annotation.tailrec
      def loop(state: Randomizer): (Long, Randomizer) = {
        val next = state.nextLong
        if (min <= next._1 && next._1 <= max) {
          next
        } else if(0 < (max - min)){
          val x = (next._1 % (max - min + 1)) + min
          if (min <= x && x <= max) {
            x -> next._2
          } else {
            loop(next._2)
          }
        } else {
          loop(next._2)
        }
      }
      loop(this)
    }
  }
}

object Randomizer {
  private val byteEdges = List(Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue)
  private val shortEdges = List(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue)
  private val charEdges = List(Char.MinValue, Char.MaxValue)
  private val intEdges = List(Int.MinValue, -1, 0, 1, Int.MaxValue)
  private val posIntEdges = List(PosInt(1), PosInt.MaxValue)
  private val posZIntEdges = List(PosZInt(0), PosZInt(1), PosZInt.MaxValue)
  private val posLongEdges = List(PosLong(1L), PosLong.MaxValue)
  private val posZLongEdges = List(PosZLong(0L), PosZLong(1L), PosZLong.MaxValue)
  private val posFloatEdges = List(PosFloat(1.0f), PosFloat.MaxValue)
  private val posZFloatEdges = List(PosZFloat(0.0f), PosZFloat(1.0f), PosZFloat.MaxValue)
  private val posDoubleEdges = List(PosDouble(1.0), PosDouble.MaxValue)
  private val posZDoubleEdges = List(PosZDouble(0.0), PosZDouble(1.0), PosZDouble.MaxValue)
  private val longEdges = List(Long.MinValue, -1, 0, 1, Long.MaxValue)
  private val standardEdges = 
    Edges(
      byteEdges,
      shortEdges,
      charEdges,
      intEdges,
      longEdges,
      List(0.0f),
      List(0.0),
      posIntEdges,
      posZIntEdges,
      posLongEdges,
      posZLongEdges,
      posFloatEdges,
      posZFloatEdges,
      posDoubleEdges,
      posZDoubleEdges
    )
  def default(): Randomizer =
    new Randomizer(
      (System.currentTimeMillis() ^ 0x5DEECE66DL) & ((1L << 48) - 1),
      Edges(
        scala.util.Random.shuffle(byteEdges),
        scala.util.Random.shuffle(shortEdges),
        scala.util.Random.shuffle(charEdges),
        scala.util.Random.shuffle(intEdges),
        scala.util.Random.shuffle(longEdges),
        List(0.0f),
        List(0.0),
        scala.util.Random.shuffle(posIntEdges),
        scala.util.Random.shuffle(posZIntEdges),
        scala.util.Random.shuffle(posLongEdges),
        scala.util.Random.shuffle(posZLongEdges),
        scala.util.Random.shuffle(posFloatEdges),
        scala.util.Random.shuffle(posZFloatEdges),
        scala.util.Random.shuffle(posDoubleEdges),
        scala.util.Random.shuffle(posZDoubleEdges)
      )
    )
  // Note, this method where you pass the seed in will produce edges in always the same order, so it 
  // is completely predictable. Maybe I should offer a way to let people customize edges too I suppose.
  def apply(seed: Long): Randomizer = new Randomizer((seed ^ 0x5DEECE66DL) & ((1L << 48) - 1), standardEdges)
}


