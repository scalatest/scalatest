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
import org.scalactic.anyvals._

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

  implicit val byteGen: Gen[Byte] =
    new Gen[Byte] {
      def next(size: Int, rnd: Rnd): (Byte, Rnd) = rnd.nextByteWithEdges
      override def toString = "Gen[Byte]"
    }

  implicit val shortGen: Gen[Short] =
    new Gen[Short] {
      def next(size: Int, rnd: Rnd): (Short, Rnd) = rnd.nextShortWithEdges
      override def toString = "Gen[Short]"
    }

  implicit val charGen: Gen[Char] =
    new Gen[Char] {
      def next(size: Int, rnd: Rnd): (Char, Rnd) = rnd.nextCharWithEdges
      override def toString = "Gen[Char]"
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

  implicit val posIntGen: Gen[PosInt] =
    new Gen[PosInt] {
      def next(size: Int, rnd: Rnd): (PosInt, Rnd) = rnd.nextPosIntWithEdges
      override def toString = "Gen[PosInt]"
    }

  implicit val posZIntGen: Gen[PosZInt] =
    new Gen[PosZInt] {
      def next(size: Int, rnd: Rnd): (PosZInt, Rnd) = rnd.nextPosZIntWithEdges
      override def toString = "Gen[PosZInt]"
    }

  implicit val posLongGen: Gen[PosLong] =
    new Gen[PosLong] {
      def next(size: Int, rnd: Rnd): (PosLong, Rnd) = rnd.nextPosLongWithEdges
      override def toString = "Gen[PosLong]"
    }

  implicit val posZLongGen: Gen[PosZLong] =
    new Gen[PosZLong] {
      def next(size: Int, rnd: Rnd): (PosZLong, Rnd) = rnd.nextPosZLongWithEdges
      override def toString = "Gen[PosZLong]"
    }

  implicit val posFloatGen: Gen[PosFloat] =
    new Gen[PosFloat] {
      def next(size: Int, rnd: Rnd): (PosFloat, Rnd) = rnd.nextPosFloatWithEdges
      override def toString = "Gen[PosFloat]"
    }

  implicit val posZFloatGen: Gen[PosZFloat] =
    new Gen[PosZFloat] {
      def next(size: Int, rnd: Rnd): (PosZFloat, Rnd) = rnd.nextPosZFloatWithEdges
      override def toString = "Gen[PosZFloat]"
    }

  implicit val posDoubleGen: Gen[PosDouble] =
    new Gen[PosDouble] {
      def next(size: Int, rnd: Rnd): (PosDouble, Rnd) = rnd.nextPosDoubleWithEdges
      override def toString = "Gen[PosDouble]"
    }

  implicit val posZDoubleGen: Gen[PosZDouble] =
    new Gen[PosZDouble] {
      def next(size: Int, rnd: Rnd): (PosZDouble, Rnd) = rnd.nextPosZDoubleWithEdges
      override def toString = "Gen[PosZDouble]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  implicit val stringGen: Gen[String] =
    new Gen[String] {
      def next(size: Int, rnd: Rnd): (String, Rnd) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        rnd.nextString(size)
      }
      override def toString = "Gen[String]"
    }
}

