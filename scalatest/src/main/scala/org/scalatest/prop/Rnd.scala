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

// Wrote this class by looking at the Javadoc of java.util.Random.
// And by testing its behavior against that of java.util.Random.
// Maybe this should be a trait, so that people can, hmm. Could 
// make subclasses with extra methods, like nextSmallInt or something,
// and in a pattern match narrow the type and call that method.
class Rnd(seed: Long, intEdges: List[Int], longEdges: List[Long], doubleEdges: List[Double]) { thisRnd =>
  def nextRnd: Rnd = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    new Rnd(newSeed, intEdges, longEdges, doubleEdges)
  }
  def next(bits: Int): (Int, Rnd) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val newInt = (newSeed >>> (48 - bits)).toInt
    (newInt, new Rnd(newSeed, intEdges, longEdges, doubleEdges))
  }
  def nextInt: (Int, Rnd) = next(32) 
  def nextLong: (Long, Rnd) = {
    val (ia, ra) = thisRnd.next(32)
    val (ib, rb) = ra.next(32)
    ((ia.toLong << 32) + ib, rb)
  }
  def nextIntWithEdges: (Int, Rnd) = {
    intEdges match {
      case head :: tail => (head, new Rnd(seed, tail, longEdges, doubleEdges))
      case Nil => nextInt
    }
  }
  def nextLongWithEdges: (Long, Rnd) = {
    longEdges match {
      case head :: tail => (head, new Rnd(seed, intEdges, tail, doubleEdges))
      case Nil => nextLong
    }
  }
  def nextDouble: (Double, Rnd) = {
    val (ia, ra) = thisRnd.next(26)
    val (ib, rb) = ra.next(27)
    (((ia.toLong << 27) + ib) / (1L << 53).toDouble, rb)
  }
  def nextDoubleWithEdges: (Double, Rnd) = {
    doubleEdges match {
      case head :: tail => (head, new Rnd(seed, intEdges, longEdges, tail))
      case Nil => nextDouble
    }
  }
  def chooseInt(from: Int, to: Int): (Int, Rnd) = {
    if(from == to) {
      (from, this.nextInt._2)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)
      @annotation.tailrec
      def loop(state: Rnd): (Int, Rnd) = {
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
}

object Rnd {
    // scala.util.Random.shuffle(List(Int.MinValue, -1, 0, 1, Int.MaxValue))
  private val intEdges = List(Int.MinValue, -1, 0, 1, Int.MaxValue)
  private val longEdges = List(Long.MinValue, -1, 0, 1, Long.MaxValue)
  def default(): Rnd =
    new Rnd(
      (System.currentTimeMillis() ^ 0x5DEECE66DL) & ((1L << 48) - 1),
      scala.util.Random.shuffle(intEdges),
      scala.util.Random.shuffle(longEdges),
      List(0.0)
    )
  // Note, this method where you pass the seed in will produce edges in always the same order, so it 
  // is completely predictable. Maybe I should offer a way to let people customize edges too I suppose.
  def apply(seed: Long): Rnd = new Rnd((seed ^ 0x5DEECE66DL) & ((1L << 48) - 1), intEdges, longEdges, List(0.0))
}

