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

import scala.collection.mutable.ListBuffer

trait Gen[T] { thisGenOfT =>
  def next(size: Int = 100, rnd: Rnd = Rnd.default): (T, Rnd)
  def map[U](f: T => U): Gen[U] =
    new Gen[U] {
      def next(size: Int, rnd: Rnd): (U, Rnd) = {
        val (nextT, nextRnd) = thisGenOfT.next(size, rnd)
        (f(nextT), nextRnd)
      }
    }
  def flatMap[U](f: T => Gen[U]): Gen[U] = 
    new Gen[U] { thisInnerGen =>
      def next(size: Int, rnd: Rnd): (U, Rnd) = {
        val (nextT, nextRnd) = thisGenOfT.next(size, rnd)
        val (a, b) = f(nextT).next(size, nextRnd)
        (a, b)
      }
    }
}

object Gen {
  def chooseInt(from: Int, to: Int): Gen[Int] =
    new Gen[Int] { thisIntGen =>
      def next(size: Int, rnd: Rnd): (Int, Rnd) = {
        val (nextInt, nextRnd) = rnd.chooseInt(from, to)
        (nextInt, nextRnd)
      }
    }

  implicit val intGen: Gen[Int] =
    new Gen[Int] {
      def next(size: Int, rnd: Rnd): (Int, Rnd) = rnd.nextIntWithEdges
      override def toString = "Gen[Int]"
    }

  implicit val longGen: Gen[Long] =
    new Gen[Long] {
      def next(size: Int, rnd: Rnd): (Long, Rnd) = rnd.nextLongWithEdges
      override def toString = "Gen[Long]"
    }

  implicit val floatGen: Gen[Float] =
    new Gen[Float] {
      def next(size: Int, rnd: Rnd): (Float, Rnd) = rnd.nextFloatWithEdges
      override def toString = "Gen[Float]"
    }

  implicit val doubleGen: Gen[Double] =
    new Gen[Double] {
      def next(size: Int, rnd: Rnd): (Double, Rnd) = rnd.nextDoubleWithEdges
      override def toString = "Gen[Double]"
    }

/*
  implicit val longGen: Gen[Long] = Gen.long // edges
  implicit val shortGen: Gen[Short] = Gen.short // edges
  implicit val charGen: Gen[Char] = Gen.char // edges
  implicit val floatGen: Gen[Float] = Gen.float // 0.0
  implicit val byteGen: Gen[Byte] = Gen.byte // edges
*/
}

