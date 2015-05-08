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
class Rnd private (seed: Long) { thisRnd =>
  def nextRnd: Rnd = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    new Rnd(newSeed)
  }
  def next(bits: Int): (Int, Rnd) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val newInt = (newSeed >>> (48 - bits)).toInt
    (newInt, new Rnd(newSeed))
  }
  def nextInt: (Int, Rnd) = next(32) 
  def nextDouble: (Double, Rnd) = {
    val (ia, ra) = thisRnd.next(26)
    val (ib, rb) = ra.next(27)
    (((ia.toLong << 27) + ib) / (1L << 53).toDouble, rb)
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
  def default(): Rnd = apply(System.currentTimeMillis())
  def apply(seed: Long): Rnd = new Rnd((seed ^ 0x5DEECE66DL) & ((1L << 48) - 1))
}

