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
import org.scalactic.{Bad, Good, Or}

trait Generator[T] { thisGeneratorOfT =>
  def next(size: Int = 100, rnd: Randomizer = Randomizer.default): (T, Randomizer)
  def map[U](f: T => U): Generator[U] =
    new Generator[U] {
      def next(size: Int, rnd: Randomizer): (U, Randomizer) = {
        val (nextT, nextRandomizer) = thisGeneratorOfT.next(size, rnd)
        (f(nextT), nextRandomizer)
      }
    }
  def flatMap[U](f: T => Generator[U]): Generator[U] = 
    new Generator[U] { thisInnerGenerator =>
      def next(size: Int, rnd: Randomizer): (U, Randomizer) = {
        val (nextT, nextRandomizer) = thisGeneratorOfT.next(size, rnd)
        val (a, b) = f(nextT).next(size, nextRandomizer)
        (a, b)
      }
    }
  def shrink(init: T): Stream[T] = Stream.empty
}

trait LowerPriorityGeneratorImplicits {

  import org.scalacheck.{Arbitrary, Gen}
  import org.scalacheck.rng.Seed

  @deprecated("Please define your own arbitary Generator.")
  implicit def scalacheckArbitaryGenerator[T](arb: Arbitrary[T]): Generator[T] =
    new Generator[T] {
      def next(size: Int, rnd: Randomizer): (T, Randomizer) = {
        arb.arbitrary.apply(Gen.Parameters.default.withSize(size), Seed.random()) match {
          case Some(nextT) => (nextT, rnd)
          case None => throw new IllegalStateException("Unable to generate value using ScalaCheck Arbitary.")
        }
      }
    }

}

object Generator extends LowerPriorityGeneratorImplicits {

  def chooseInt(from: Int, to: Int): Generator[Int] =
    new Generator[Int] { thisIntGenerator =>
      def next(size: Int, rnd: Randomizer): (Int, Randomizer) = {
        val (nextInt, nextRandomizer) = rnd.chooseInt(from, to)
        (nextInt, nextRandomizer)
      }
    }

  def oneOf[T](seq: T*): Generator[T] =
    new Generator[T] {
      def next(size: Int, rnd: Randomizer): (T, Randomizer) = {
        val (nextInt, nextRandomizer) = rnd.chooseInt(0, seq.length - 1)
        val nextT = seq(nextInt)
        (nextT, nextRandomizer)
      }
    }

  implicit val byteGenerator: Generator[Byte] =
    new Generator[Byte] {
      def next(size: Int, rnd: Randomizer): (Byte, Randomizer) = rnd.nextByteWithEdges
      override def toString = "Generator[Byte]"
    }

  implicit val shortGenerator: Generator[Short] =
    new Generator[Short] {
      def next(size: Int, rnd: Randomizer): (Short, Randomizer) = rnd.nextShortWithEdges
      override def toString = "Generator[Short]"
    }

  implicit val charGenerator: Generator[Char] =
    new Generator[Char] {
      def next(size: Int, rnd: Randomizer): (Char, Randomizer) = rnd.nextCharWithEdges
      override def toString = "Generator[Char]"
    }

  implicit val intGenerator: Generator[Int] =
    new Generator[Int] {
      def next(size: Int, rnd: Randomizer): (Int, Randomizer) = rnd.nextIntWithEdges
      override def toString = "Generator[Int]"
      override def shrink(init: Int): Stream[Int] = 0 #:: 1 #:: -1 #:: Stream.empty
    }

  implicit val longGenerator: Generator[Long] =
    new Generator[Long] {
      def next(size: Int, rnd: Randomizer): (Long, Randomizer) = rnd.nextLongWithEdges
      override def toString = "Generator[Long]"
    }

  implicit val floatGenerator: Generator[Float] =
    new Generator[Float] {
      def next(size: Int, rnd: Randomizer): (Float, Randomizer) = rnd.nextFloatWithEdges
      override def toString = "Generator[Float]"
    }

  implicit val doubleGenerator: Generator[Double] =
    new Generator[Double] {
      def next(size: Int, rnd: Randomizer): (Double, Randomizer) = rnd.nextDoubleWithEdges
      override def toString = "Generator[Double]"
    }

  implicit val posIntGenerator: Generator[PosInt] =
    new Generator[PosInt] {
      def next(size: Int, rnd: Randomizer): (PosInt, Randomizer) = rnd.nextPosIntWithEdges
      override def toString = "Generator[PosInt]"
    }

  implicit val posZIntGenerator: Generator[PosZInt] =
    new Generator[PosZInt] {
      def next(size: Int, rnd: Randomizer): (PosZInt, Randomizer) = rnd.nextPosZIntWithEdges
      override def toString = "Generator[PosZInt]"
    }

  implicit val posLongGenerator: Generator[PosLong] =
    new Generator[PosLong] {
      def next(size: Int, rnd: Randomizer): (PosLong, Randomizer) = rnd.nextPosLongWithEdges
      override def toString = "Generator[PosLong]"
    }

  implicit val posZLongGenerator: Generator[PosZLong] =
    new Generator[PosZLong] {
      def next(size: Int, rnd: Randomizer): (PosZLong, Randomizer) = rnd.nextPosZLongWithEdges
      override def toString = "Generator[PosZLong]"
    }

  implicit val posFloatGenerator: Generator[PosFloat] =
    new Generator[PosFloat] {
      def next(size: Int, rnd: Randomizer): (PosFloat, Randomizer) = rnd.nextPosFloatWithEdges
      override def toString = "Generator[PosFloat]"
    }

  implicit val posZFloatGenerator: Generator[PosZFloat] =
    new Generator[PosZFloat] {
      def next(size: Int, rnd: Randomizer): (PosZFloat, Randomizer) = rnd.nextPosZFloatWithEdges
      override def toString = "Generator[PosZFloat]"
    }

  implicit val posDoubleGenerator: Generator[PosDouble] =
    new Generator[PosDouble] {
      def next(size: Int, rnd: Randomizer): (PosDouble, Randomizer) = rnd.nextPosDoubleWithEdges
      override def toString = "Generator[PosDouble]"
    }

  implicit val posZDoubleGenerator: Generator[PosZDouble] =
    new Generator[PosZDouble] {
      def next(size: Int, rnd: Randomizer): (PosZDouble, Randomizer) = rnd.nextPosZDoubleWithEdges
      override def toString = "Generator[PosZDouble]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  implicit val stringGenerator: Generator[String] =
    new Generator[String] {
      def next(size: Int, rnd: Randomizer): (String, Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        rnd.nextString(size)
      }
      override def toString = "Generator[String]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  implicit def listGenerator[T](implicit genOfT: Generator[T]): Generator[List[T]] =
    new Generator[List[T]] {
      def next(size: Int, rnd: Randomizer): (List[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        rnd.nextList[T](size)
      }
      override def toString = "Generator[List[T]]"
    }

  implicit def function1Generator[T1, R](implicit genOfR: Generator[R]): Generator[T1 => R] =
    new Generator[T1 => R] {
      def next(size: Int, rnd: Randomizer): (T1 => R, Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextR, nextRnd) = genOfR.next(size, rnd)
        (t1 => nextR, nextRnd)

      }
    }

  implicit def optionGenerator[T](implicit genOfT: Generator[T]): Generator[Option[T]] =
    new Generator[Option[T]] {
      def next(size: Int, rnd: Randomizer): (Option[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        if (nextInt % 10 == 0)
          (None, nextRnd)
        else {
          val (nextT, nextRnd) = genOfT.next(size, rnd)
          (Some(nextT), nextRnd)
        }
      }
    }

  implicit def orGenerator[G, B](implicit genOfG: Generator[G], genOfB: Generator[B]): Generator[Or[G, B]] =
    new Generator[Or[G, B]] {
      def next(size: Int, rnd: Randomizer): (Or[G, B], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        if (nextInt % 4 == 0) {
          val (nextB, nextRnd) = genOfB.next(size, rnd)
          (Bad(nextB), nextRnd)
        }
        else {
          val (nextG, nextRnd) = genOfG.next(size, rnd)
          (Good(nextG), nextRnd)
        }
      }
    }
}

