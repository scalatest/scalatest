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

trait Generator[T] { thisGeneratorOfT =>
  def next(size: Int = 100, rnd: Rnd = Rnd.default): (T, Rnd)
  def map[U](f: T => U): Generator[U] =
    new Generator[U] {
      def next(size: Int, rnd: Rnd): (U, Rnd) = {
        val (nextT, nextRnd) = thisGeneratorOfT.next(size, rnd)
        (f(nextT), nextRnd)
      }
    }
  def flatMap[U](f: T => Generator[U]): Generator[U] = 
    new Generator[U] { thisInnerGenerator =>
      def next(size: Int, rnd: Rnd): (U, Rnd) = {
        val (nextT, nextRnd) = thisGeneratorOfT.next(size, rnd)
        val (a, b) = f(nextT).next(size, nextRnd)
        (a, b)
      }
    }
  def shrink(init: T): Stream[T] = Stream.empty
}

object Generator {

  def chooseInt(from: Int, to: Int): Generator[Int] =
    new Generator[Int] { thisIntGenerator =>
      def next(size: Int, rnd: Rnd): (Int, Rnd) = {
        val (nextInt, nextRnd) = rnd.chooseInt(from, to)
        (nextInt, nextRnd)
      }
    }

  implicit val byteGenerator: Generator[Byte] =
    new Generator[Byte] {
      def next(size: Int, rnd: Rnd): (Byte, Rnd) = rnd.nextByteWithEdges
      override def toString = "Generator[Byte]"
    }

  implicit val shortGenerator: Generator[Short] =
    new Generator[Short] {
      def next(size: Int, rnd: Rnd): (Short, Rnd) = rnd.nextShortWithEdges
      override def toString = "Generator[Short]"
    }

  implicit val charGenerator: Generator[Char] =
    new Generator[Char] {
      def next(size: Int, rnd: Rnd): (Char, Rnd) = rnd.nextCharWithEdges
      override def toString = "Generator[Char]"
    }

  implicit val intGenerator: Generator[Int] =
    new Generator[Int] {
      def next(size: Int, rnd: Rnd): (Int, Rnd) = rnd.nextIntWithEdges
      override def toString = "Generator[Int]"
      override def shrink(init: Int): Stream[Int] = 0 #:: 1 #:: -1 #:: Stream.empty
    }

  implicit val longGenerator: Generator[Long] =
    new Generator[Long] {
      def next(size: Int, rnd: Rnd): (Long, Rnd) = rnd.nextLongWithEdges
      override def toString = "Generator[Long]"
    }

  implicit val floatGenerator: Generator[Float] =
    new Generator[Float] {
      def next(size: Int, rnd: Rnd): (Float, Rnd) = rnd.nextFloatWithEdges
      override def toString = "Generator[Float]"
    }

  implicit val doubleGenerator: Generator[Double] =
    new Generator[Double] {
      def next(size: Int, rnd: Rnd): (Double, Rnd) = rnd.nextDoubleWithEdges
      override def toString = "Generator[Double]"
    }

  implicit val posIntGenerator: Generator[PosInt] =
    new Generator[PosInt] {
      def next(size: Int, rnd: Rnd): (PosInt, Rnd) = rnd.nextPosIntWithEdges
      override def toString = "Generator[PosInt]"
    }

  implicit val posZIntGenerator: Generator[PosZInt] =
    new Generator[PosZInt] {
      def next(size: Int, rnd: Rnd): (PosZInt, Rnd) = rnd.nextPosZIntWithEdges
      override def toString = "Generator[PosZInt]"
    }

  implicit val posLongGenerator: Generator[PosLong] =
    new Generator[PosLong] {
      def next(size: Int, rnd: Rnd): (PosLong, Rnd) = rnd.nextPosLongWithEdges
      override def toString = "Generator[PosLong]"
    }

  implicit val posZLongGenerator: Generator[PosZLong] =
    new Generator[PosZLong] {
      def next(size: Int, rnd: Rnd): (PosZLong, Rnd) = rnd.nextPosZLongWithEdges
      override def toString = "Generator[PosZLong]"
    }

  implicit val posFloatGenerator: Generator[PosFloat] =
    new Generator[PosFloat] {
      def next(size: Int, rnd: Rnd): (PosFloat, Rnd) = rnd.nextPosFloatWithEdges
      override def toString = "Generator[PosFloat]"
    }

  implicit val posZFloatGenerator: Generator[PosZFloat] =
    new Generator[PosZFloat] {
      def next(size: Int, rnd: Rnd): (PosZFloat, Rnd) = rnd.nextPosZFloatWithEdges
      override def toString = "Generator[PosZFloat]"
    }

  implicit val posDoubleGenerator: Generator[PosDouble] =
    new Generator[PosDouble] {
      def next(size: Int, rnd: Rnd): (PosDouble, Rnd) = rnd.nextPosDoubleWithEdges
      override def toString = "Generator[PosDouble]"
    }

  implicit val posZDoubleGenerator: Generator[PosZDouble] =
    new Generator[PosZDouble] {
      def next(size: Int, rnd: Rnd): (PosZDouble, Rnd) = rnd.nextPosZDoubleWithEdges
      override def toString = "Generator[PosZDouble]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  implicit val stringGenerator: Generator[String] =
    new Generator[String] {
      def next(size: Int, rnd: Rnd): (String, Rnd) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        rnd.nextString(size)
      }
      override def toString = "Generator[String]"
    }
}

