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
import scala.annotation.tailrec

trait Generator[T] { thisGeneratorOfT =>

  def initEdges(maxLength: Int, rnd: Randomizer): (List[T], Randomizer) = (Nil, rnd)

  def next(size: Int = 100, edges: List[T] = Nil, rnd: Randomizer = Randomizer.default): (T, List[T], Randomizer)
  def map[U](f: T => U): Generator[U] =
    new Generator[U] { thisGeneratorOfU => 
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[U], Randomizer) = {
        val (listOfT, nextRnd) = thisGeneratorOfT.initEdges(maxLength, rnd)
        (listOfT.map(f), nextRnd)
      }
      def next(size: Int, edges: List[U], rnd: Randomizer): (U, List[U], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => 
            (head, tail, rnd)
          case _ =>
            val (nextT, _, nextRandomizer) = thisGeneratorOfT.next(size, Nil, rnd)
            (f(nextT), Nil, nextRandomizer)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[U], Randomizer) = { 
        val (cansOfT, nextRnd) = thisGeneratorOfT.canonicals(rnd)
        (cansOfT.map(f), nextRnd)
      }
      override def shrink(value: U, rnd: Randomizer): (Iterator[U], Randomizer) = canonicals(rnd)
    }
  def flatMap[U](f: T => Generator[U]): Generator[U] = 
    new Generator[U] { thisGeneratorOfU =>
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[U], Randomizer) = {
        val (listOfT, nextRnd) = thisGeneratorOfT.initEdges(maxLength, rnd)
        val listOfGenOfU: List[Generator[U]] = listOfT.map(f)
        val (listOfListOfU, nextNextRnd): (List[List[U]], Randomizer) = {
          @tailrec
          def loop(remainingGenOfU: List[Generator[U]], nRnd: Randomizer, acc: List[List[U]]): (List[List[U]], Randomizer) = {
            remainingGenOfU match {
              case head :: tail =>
                val (listOfU, nnRnd) = head.initEdges(maxLength, nRnd)
                loop(tail, nnRnd, listOfU :: acc)
              case _ => (acc, nRnd)
            }
          }
          loop(listOfGenOfU, nextRnd, Nil)
        }
        val listOfU: List[U] = listOfListOfU.flatten
        val distinctEdges: List[U] = listOfU.distinct
        (distinctEdges.take(maxLength), nextNextRnd)
      }
      def next(size: Int, edges: List[U], rnd: Randomizer): (U, List[U], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => 
            (head, tail, rnd)
          case _ => 
            val (nextT, _, nextRandomizer) = thisGeneratorOfT.next(size, Nil, rnd)
            val genOfU: Generator[U] = f(nextT)
            val (u, _, nextNextRandomizer) = genOfU.next(size, Nil, nextRandomizer)
            (u, Nil, nextNextRandomizer)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[U], Randomizer) = { 
        val (cansOfT, rnd1) = thisGeneratorOfT.canonicals(rnd)
        var currentRnd = rnd1 // Local var, one thread; TODO: Do this with a tailrec loop
        def getCanonicals(o: T): Iterator[U] = {
          val genOfU: Generator[U] = f(o)
          val (canonicals, nextRnd) = genOfU.canonicals(currentRnd)
          currentRnd = nextRnd
          canonicals
        }
        (cansOfT.flatMap(getCanonicals), currentRnd)
      }
      override def shrink(value: U, rnd: Randomizer): (Iterator[U], Randomizer) = canonicals(rnd)
    }
  def shrink(value: T, rnd: Randomizer): (Iterator[T], Randomizer) = (Iterator.empty, rnd)
  def canonicals(rnd: Randomizer): (Iterator[T], Randomizer) = (Iterator.empty, rnd)
  def sample: T = {
    val rnd = Randomizer.default
    val (size, nextRnd) = rnd.chooseInt(1, 100)
    val (value, _, _) = next(size, Nil, nextRnd)
    value
  }
  def samples(length: PosInt): List[T] = {
    @tailrec
    def loop(count: Int, rnd: Randomizer, acc: List[T]): List[T] = {
      if (count == length.value) acc
      else {
        val (size, nextRnd) = rnd.chooseInt(1, 100)
        val (value, _, nextNextRnd) = next(size, Nil, rnd)
        loop(count + 1, nextNextRnd, value :: acc)
      }
    }
    loop(0, Randomizer.default, Nil)
  }
}

trait LowerPriorityGeneratorImplicits {

  import org.scalacheck.{Arbitrary, Gen, Shrink}
  import org.scalacheck.rng.Seed

  implicit def scalaCheckArbitaryGenerator[T](arb: Arbitrary[T], shrk: Shrink[T]): Generator[T] =
    new Generator[T] {
      def next(size: Int, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            arb.arbitrary.apply(Gen.Parameters.default.withSize(size), Seed(rnd.seed)) match {
              case Some(nextT) => (nextT, Nil, rnd.nextRandomizer)
              case None => throw new IllegalStateException("Unable to generate value using ScalaCheck Arbitary.")
            }
        }
      }
      override def shrink(value: T, rnd: Randomizer): (Iterator[T], Randomizer) = {
        (shrk.shrink(value).take(10000).reverse.toIterator, rnd)
      }
    }
}

object Generator extends LowerPriorityGeneratorImplicits {

  def oneOf[T](seq: T*): Generator[T] =
    new Generator[T] {
      def next(size: Int, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(0, seq.length - 1)
            val nextT = seq(nextInt)
            (nextT, Nil, nextRandomizer)
        }
      }
    }

  implicit val byteGenerator: Generator[Byte] =
    new Generator[Byte] {
      private val byteEdges = List(Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Byte], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(byteEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[Byte], rnd: Randomizer): (Byte, List[Byte], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (b, nextRnd) = rnd.nextByte
            (b, Nil, nextRnd)
        }
      }
      private val byteCanonicals: List[Byte] = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Byte], Randomizer) = (byteCanonicals.iterator, rnd)
      override def shrink(n: Byte, rnd: Randomizer): (Iterator[Byte], Randomizer) = {
        @tailrec
        def shrinkLoop(n: Byte, acc: List[Byte]): List[Byte] = {
          if (n == 0) acc
          else {
            val half: Byte = (n / 2).toByte
            if (half == 0) 0.toByte :: acc
            else shrinkLoop(half, (-half).toByte :: half :: acc)
          }
        }
        (shrinkLoop(n, Nil).iterator, rnd)
      }
      override def toString = "Generator[Byte]"
    }

  implicit val shortGenerator: Generator[Short] =
    new Generator[Short] {
      private val shortEdges = List(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Short], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(shortEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[Short], rnd: Randomizer): (Short, List[Short], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (s, nextRnd) = rnd.nextShort
            (s, Nil, nextRnd)
        }
      }
      private val shortCanonicals: List[Short] = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Short], Randomizer) = (shortCanonicals.iterator, rnd)
      override def shrink(n: Short, rnd: Randomizer): (Iterator[Short], Randomizer) = {
        @tailrec
        def shrinkLoop(n: Short, acc: List[Short]): List[Short] = {
          if (n == 0) acc
          else {
            val half: Short = (n / 2).toShort
            if (half == 0) 0.toShort :: acc
            else shrinkLoop(half, (-half).toShort :: half :: acc)
          }
        }
        (shrinkLoop(n, Nil).iterator, rnd)
      }
      override def toString = "Generator[Short]"
    }

  implicit val charGenerator: Generator[Char] =
    new Generator[Char] {
      private val charEdges = List(Char.MinValue, Char.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Char], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(charEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[Char], rnd: Randomizer): (Char, List[Char], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (c, nextRnd) = rnd.nextChar
            (c, Nil, nextRnd)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[Char], Randomizer) = {
        val lowerAlphaChars = "abcdefghikjlmnopqrstuvwxyz"
        val upperAlphaChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        val numericChars = "0123456789"
        val (lowerCharIndex, rnd1) = rnd.chooseInt(0, lowerAlphaChars.length - 1)
        val (upperCharIndex, rnd2) = rnd1.chooseInt(0, upperAlphaChars.length - 1)
        val (numericCharIndex, rnd3) = rnd1.chooseInt(0, numericChars.length - 1)
        val lowerChar = lowerAlphaChars(lowerCharIndex)
        val upperChar = upperAlphaChars(upperCharIndex)
        val numericChar = numericChars(numericCharIndex)
        (Iterator(lowerChar, upperChar, numericChar), rnd3)
      }
      override def shrink(c: Char, rnd: Randomizer): (Iterator[Char], Randomizer) = {
        val userFriendlyChars = "abcdefghikjlmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        if (userFriendlyChars.indexOf(c) >= 0) (Iterator.empty, rnd)
        else (userFriendlyChars.toIterator, rnd)
      }
      override def toString = "Generator[Char]"
    }

  implicit val intGenerator: Generator[Int] =
    new Generator[Int] {
      private val intEdges = List(Int.MinValue, -1, 0, 1, Int.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Int], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(intEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[Int], rnd: Randomizer): (Int, List[Int], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (i, nextRnd) = rnd.nextInt
            (i, Nil, nextRnd)
        }
      }
      override def toString = "Generator[Int]"
      private val intCanonicals = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Int], Randomizer) = (intCanonicals.iterator, rnd)
      override def shrink(i: Int, rnd: Randomizer): (Iterator[Int], Randomizer) = {
        @tailrec
        def shrinkLoop(i: Int, acc: List[Int]): List[Int] = {
          if (i == 0) acc
          else {
            val half: Int = i / 2
            if (half == 0) 0 :: acc
            else shrinkLoop(half, -half :: half :: acc)
          }
        }
        (shrinkLoop(i, Nil).iterator, rnd)
      }
    }

  implicit val longGenerator: Generator[Long] =
    new Generator[Long] {
      private val longEdges = List(Long.MinValue, -1, 0, 1, Long.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Long], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(longEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[Long], rnd: Randomizer): (Long, List[Long], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (n, nextRnd) = rnd.nextLong
            (n, Nil, nextRnd)
        }
      }
      private val longCanonicals: List[Long] = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Long], Randomizer) = (longCanonicals.iterator, rnd)
      override def shrink(n: Long, rnd: Randomizer): (Iterator[Long], Randomizer) = {
        @tailrec
        def shrinkLoop(n: Long, acc: List[Long]): List[Long] = {
          if (n == 0L) acc
          else {
            val half: Long = n / 2
            if (half == 0L) 0L :: acc
            else shrinkLoop(half, -half :: half :: acc)
          }
        }
        (shrinkLoop(n, Nil).iterator, rnd)
      }
      override def toString = "Generator[Long]"
    }

  implicit val floatGenerator: Generator[Float] =
    new Generator[Float] {
      private val posFloatEdges = List(0.0f)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Float], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        (posFloatEdges.take(maxLength), rnd)
      }
      def next(size: Int, edges: List[Float], rnd: Randomizer): (Float, List[Float], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (f, nextRnd) = rnd.nextFloat
            (f, Nil, nextRnd)
        }
      }
      private val floatCanonicals: List[Float] = List(0.0f, 1.0f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f)
      override def canonicals(rnd: Randomizer): (Iterator[Float], Randomizer) = (floatCanonicals.iterator, rnd)
      override def shrink(f: Float, rnd: Randomizer): (Iterator[Float], Randomizer) = {
        @tailrec
        def shrinkLoop(f: Float, acc: List[Float]): List[Float] = {
          if (f == 0.0f) acc
          else if (f <= 1.0f && f >= -1.0f) 0.0f :: acc
          else if (!f.isWhole) {
            // Nearest whole numbers closer to zero
            val (nearest, nearestNeg) = if (f > 0.0f) (f.floor, (-f).ceil) else (f.ceil, (-f).floor)
            shrinkLoop(nearest, nearestNeg :: nearest :: acc)
          }
          else {
            val sqrt: Float = math.sqrt(f.abs.toDouble).toFloat
            if (sqrt < 1.0f) 0.0f :: acc
            else {
              val whole: Float = sqrt.floor
              val negWhole: Float = math.rint((-whole).toDouble).toFloat
              val (first, second) = if (f > 0.0f) (negWhole, whole) else (whole, negWhole)
              shrinkLoop(first, first :: second :: acc)
            }
          }
        }
        (shrinkLoop(f, Nil).iterator, rnd)
      }
      override def toString = "Generator[Float]"
    }

  implicit val doubleGenerator: Generator[Double] =
    new Generator[Double] {
      private val posDoubleEdges = List(0.0)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Double], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        (posDoubleEdges.take(maxLength), rnd)
      }
      def next(size: Int, edges: List[Double], rnd: Randomizer): (Double, List[Double], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (d, nextRnd) = rnd.nextDouble
            (d, Nil, nextRnd)
        }
      }
      private val doubleCanonicals: List[Double] = List(0.0, 1.0, -1.0, 2.0, -2.0, 3.0, -3.0)
      override def canonicals(rnd: Randomizer): (Iterator[Double], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def shrink(d: Double, rnd: Randomizer): (Iterator[Double], Randomizer) = {
        @tailrec
        def shrinkLoop(d: Double, acc: List[Double]): List[Double] = {
          if (d == 0.0) acc
          else if (d <= 1.0 && d >= -1.0) 0.0 :: acc
          else if (!d.isWhole) {
            // Nearest whole numbers closer to zero
            val (nearest, nearestNeg) = if (d > 0.0) (d.floor, (-d).ceil) else (d.ceil, (-d).floor)
            shrinkLoop(nearest, nearestNeg :: nearest :: acc)
          }
          else {
            val sqrt: Double = math.sqrt(d.abs)
            if (sqrt < 1.0) 0.0 :: acc
            else {
              val whole: Double = sqrt.floor
              val negWhole: Double = math.rint(-whole)
              val (first, second) = if (d > 0.0) (negWhole, whole) else (whole, negWhole)
              shrinkLoop(first, first :: second :: acc)
            }
          }
        }
        (shrinkLoop(d, Nil).iterator, rnd)
      }
      override def toString = "Generator[Double]"
    }

  implicit val posIntGenerator: Generator[PosInt] =
    new Generator[PosInt] {
      private val posIntEdges = List(PosInt(1), PosInt.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosInt], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosInt], rnd: Randomizer): (PosInt, List[PosInt], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posInt, nextRnd) = rnd.nextPosInt
            (posInt, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosInt]"
    }

  implicit val posZIntGenerator: Generator[PosZInt] =
    new Generator[PosZInt] {
      private val posZIntEdges = List(PosZInt(0), PosZInt(1), PosZInt.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosZInt], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posZIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosZInt], rnd: Randomizer): (PosZInt, List[PosZInt], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZInt, nextRnd) = rnd.nextPosZInt
            (posZInt, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosZInt]"
    }

  implicit val posLongGenerator: Generator[PosLong] =
    new Generator[PosLong] {
      private val posLongEdges = List(PosLong(1L), PosLong.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosLong], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosLong], rnd: Randomizer): (PosLong, List[PosLong], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posLong, nextRnd) = rnd.nextPosLong
            (posLong, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosLong]"
    }

  implicit val posZLongGenerator: Generator[PosZLong] =
    new Generator[PosZLong] {
      private val posZLongEdges = List(PosZLong(0L), PosZLong(1L), PosZLong.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosZLong], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posZLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosZLong], rnd: Randomizer): (PosZLong, List[PosZLong], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZLong, nextRnd) = rnd.nextPosZLong
            (posZLong, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosZLong]"
    }

  implicit val posFloatGenerator: Generator[PosFloat] =
    new Generator[PosFloat] {
      private val posFloatEdges = List(PosFloat(1.0f), PosFloat.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosFloat], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosFloat], rnd: Randomizer): (PosFloat, List[PosFloat], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZFloat, nextRnd) = rnd.nextPosFloat
            (posZFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosFloat]"
    }

  implicit val posZFloatGenerator: Generator[PosZFloat] =
    new Generator[PosZFloat] {
      private val posZFloatEdges = List(PosZFloat(0.0f), PosZFloat(1.0f), PosZFloat.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosZFloat], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosZFloat], rnd: Randomizer): (PosZFloat, List[PosZFloat], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZFloat, nextRnd) = rnd.nextPosZFloat
            (posZFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosZFloat]"
    }

  implicit val posDoubleGenerator: Generator[PosDouble] =
    new Generator[PosDouble] {
      private val posDoubleEdges = List(PosDouble(1.0), PosDouble.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosDouble], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosDouble], rnd: Randomizer): (PosDouble, List[PosDouble], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posDouble, nextRnd) = rnd.nextPosDouble
            (posDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosDouble]"
    }

  implicit val posZDoubleGenerator: Generator[PosZDouble] =
    new Generator[PosZDouble] {
      private val posZDoubleEdges = List(PosZDouble(0.0), PosZDouble(1.0), PosZDouble.MaxValue)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosZDouble], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(posZDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosZDouble], rnd: Randomizer): (PosZDouble, List[PosZDouble], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZDouble, nextRnd) = rnd.nextPosZDouble
            (posZDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosZDouble]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  implicit val stringGenerator: Generator[String] =
    new Generator[String] {
      private val stringEdges = List("")
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[String], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        (stringEdges.take(maxLength), rnd)
      }
      def next(size: Int, edges: List[String], rnd: Randomizer): (String, List[String], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (s, nextRnd) = rnd.nextString(size)
            (s, Nil, nextRnd)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[String], Randomizer) = { 
        val (canonicalsOfChar, rnd1) = charGenerator.canonicals(rnd)
        (Iterator("") ++ canonicalsOfChar.map(t => s"$t"), rnd1)
      }
      override def shrink(s: String, rnd: Randomizer): (Iterator[String], Randomizer) = {

        val lowerAlphaChars = "abcdefghikjlmnopqrstuvwxyz"
        val upperAlphaChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        val numericChars = "0123456789"
        val (lowerCharIndex, rnd1) = rnd.chooseInt(0, lowerAlphaChars.length - 1)
        val (upperCharIndex, rnd2) = rnd1.chooseInt(0, upperAlphaChars.length - 1)
        val (numericCharIndex, rnd3) = rnd1.chooseInt(0, numericChars.length - 1)
        val lowerChar = lowerAlphaChars(lowerCharIndex)
        val upperChar = upperAlphaChars(upperCharIndex)
        val numericChar = numericChars(numericCharIndex)
        val candidateChars: List[Char] = List(lowerChar, upperChar, numericChar) ++ s.distinct.toList
        val candidateStrings: List[String] = candidateChars.map(_.toString)

        val lastBatch =
          new Iterator[String] {
            private var nextString = s.take(2)
            def hasNext: Boolean = nextString.length < s.length
            def next: String = {
              val result = nextString
              nextString = s.take(result.length * 2)
              result
            }
          }

        if (s.isEmpty) (Iterator.empty, rnd)
        else (
          Iterator("") ++ candidateStrings ++ lastBatch,
          rnd3
        )
      }
      override def toString = "Generator[String]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  implicit def listGenerator[T](implicit genOfT: Generator[T]): Generator[List[T]] =
    new Generator[List[T]] {
      private val listEdges = List(Nil)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[List[T]], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        (listEdges.take(maxLength), rnd)
      }
      def next(size: Int, edges: List[List[T]], rnd: Randomizer): (List[T], List[List[T]], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (listOfT, nextRnd) = rnd.nextList[T](size)
            (listOfT, Nil, nextRnd)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = { 
        val (canonicalsOfT, rnd1) = genOfT.canonicals(rnd)
        (canonicalsOfT.map(t => List(t)), rnd1)
      }
      override def shrink(xs: List[T], rnd: Randomizer): (Iterator[List[T]], Randomizer) = {

        val (canonicalTsIt, rnd1) = genOfT.canonicals(rnd)
        val canonicalListOfTsIt: Iterator[List[T]] = canonicalTsIt.map(t => List(t))
        // Only include distinctListsOfTs if the list to shrink (xs) does not contain
        // just one element itself. If it does, then xs will appear in the output, which
        // we don't need, since we already know it fails.
        val distinctListOfTsIt: Iterator[List[T]] = if (xs.nonEmpty && xs.tail.nonEmpty) xs.distinct.map(t => List(t)).iterator else Iterator.empty

        val lastBatch =
          new Iterator[List[T]] {
            private var nextT = xs.take(2)
            def hasNext: Boolean = nextT.length < xs.length
            def next: List[T] = {
              if (!hasNext)
                throw new NoSuchElementException
              val result = nextT
              nextT = xs.take(result.length * 2)
              result
            }
          }

        if (xs.isEmpty) (Iterator.empty, rnd)
        else (
          Iterator(Nil) ++ canonicalListOfTsIt ++ distinctListOfTsIt ++ lastBatch,
          rnd1
        )
      }
      override def toString = "Generator[List[T]]"
    }

  implicit def function0Generator[T](implicit genOfT: Generator[T]): Generator[() => T] = {
    new Generator[() => T] { thisGeneratorOfFunction0 =>
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[() => T], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to initEdges must be >= 0")
        val (edgesOfT, nextRnd) = genOfT.initEdges(maxLength, rnd)
        val edges = edgesOfT.map(t => PrettyFunction0(t))
        (edges, nextRnd)
      }
      def next(size: Int = 100, edges: List[() => T] = Nil, rnd: Randomizer = Randomizer.default): (() => T, List[() => T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextT, _, nextRnd) = genOfT.next(size, Nil, rnd)
            (PrettyFunction0(nextT), Nil, nextRnd)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[() => T], Randomizer) = {
        val (canonicalsOfT, nextRnd) = genOfT.canonicals(rnd)
        val canonicals = canonicalsOfT.map(t => PrettyFunction0(t))
        (canonicals, nextRnd)
      }
      override def shrink(f: () => T, rnd: Randomizer): (Iterator[() => T], Randomizer) = {
        val (shrinksOfT, nextRnd) = genOfT.shrink(f(), rnd)
        val shrinks = shrinksOfT.map(t => PrettyFunction0(t))
        (shrinks, nextRnd)
      }
    }
  }

  implicit def function1IntToListOfStringGenerator: Generator[Int => List[String]] = {
    object IntToListOfStringIdentity extends PrettyFunction1[Int, List[String]] {
      def apply(i: Int): List[String] = List(i.toString)
      override def toString = "(i: Int) => List(i.toString)"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToListOfStringChars extends PrettyFunction1[Int, List[String]] {
      def apply(i: Int): List[String] = i.toString.toList.map(_.toString)
      override def toString = "(i: Int) => i.toString.toList.map(_.toString)"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToListOfStringCharPairs extends PrettyFunction1[Int, List[String]] {
      def apply(i: Int): List[String] = i.toString.toList.sliding(2).toList.map(_.mkString)
      override def toString = "(i: Int) => i.toString.toList.sliding(2).toList.map(_.mkString)"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    val funs: Vector[Int => List[String]] =
      Vector(
        IntToListOfStringIdentity,
        IntToListOfStringChars,
        IntToListOfStringCharPairs
      )
    new Generator[Int => List[String]] {
      def next(size: Int, edges: List[Int => List[String]], rnd: Randomizer): (Int => List[String], List[Int => List[String]], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[Int => List[String]]"
    }
  }
  implicit def function1StringToListOfLongGenerator: Generator[String => List[Long]] = {
    object StringToListOfLongLength extends PrettyFunction1[String, List[Long]] {
      def apply(s: String): List[Long] = List(s.length.toLong)
      override def toString = "(s: String) => List(s.length.toLong)"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToListOfLongChars extends PrettyFunction1[String, List[Long]] {
      def apply(s: String): List[Long] = s.toList.map(_.toLong)
      override def toString = "(s: String) => s.toList.map(_.toLong)"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToListOfReverseLongChars extends PrettyFunction1[String, List[Long]] {
      def apply(s: String): List[Long] = s.toList.reverse.map(_.toLong)
      override def toString = "(s: String) => s.toList.reverse.map(_.toLong)"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    val funs: Vector[String => List[Long]] =
      Vector(
        StringToListOfLongLength,
        StringToListOfLongChars,
        StringToListOfReverseLongChars
      )
    new Generator[String => List[Long]] {
      def next(size: Int, edges: List[String => List[Long]], rnd: Randomizer): (String => List[Long], List[String => List[Long]], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[String => List[Long]]"
    }
  }

  implicit def function1IntToIntGenerator: Generator[Int => Int] = {
    object IntToIntIdentity extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i
      override def toString = "(i: Int) => i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntIncr extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + 1
      override def toString = "(i: Int) => i + 1"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntSquare extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i * 1
      override def toString = "(i: Int) => i * i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntHalf extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i / 2
      override def toString = "(i: Int) => i / 2"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntAddMax extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + Int.MaxValue
      override def toString = "(i: Int) => i + Int.MaxValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntAddMin extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + Int.MinValue
      override def toString = "(i: Int) => i + Int.MinValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntSubtractMax extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i - Int.MaxValue
      override def toString = "(i: Int) => i - Int.MaxValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntSubtractMin extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i - Int.MinValue
      override def toString = "(i: Int) => i - Int.MinValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntAbs extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i.abs
      override def toString = "(i: Int) => i.abs"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntNegate extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = -i
      override def toString = "(i: Int) => -i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntComplement extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = ~i
      override def toString = "(i: Int) => ~i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    val funs: Vector[Int => Int] =
      Vector(
        IntToIntIdentity,
        IntToIntIncr,
        IntToIntSquare,
        IntToIntHalf,
        IntToIntAddMax,
        IntToIntAddMin,
        IntToIntSubtractMax,
        IntToIntSubtractMin,
        IntToIntAbs,
        IntToIntNegate,
        IntToIntComplement
      )
    new Generator[Int => Int] {
      def next(size: Int, edges: List[Int => Int], rnd: Randomizer): (Int => Int, List[Int => Int], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[Int => Int]"
    }
  }

  implicit def function1IntToOptionOfIntGenerator: Generator[Int => Option[Int]] = {
    object IntToOptionOfIntNonZero extends PrettyFunction1[Int, Option[Int]] {
      def apply(i: Int): Option[Int] = if (i != 0) Some(i) else None
      override def toString = "(i: Int) => if (i != 0) Some(i) else None"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToOptionOfIntPositive extends PrettyFunction1[Int, Option[Int]] {
      def apply(i: Int): Option[Int] = if (i > 0) Some(i) else None
      override def toString = "(i: Int) => if (i > 0) Some(i) else None"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToOptionOfIntNegative extends PrettyFunction1[Int, Option[Int]] {
      def apply(i: Int): Option[Int] = if (i < 0) Some(i) else None
      override def toString = "(i: Int) => if (i < 0) Some(i) else None"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToOptionOfIntEven extends PrettyFunction1[Int, Option[Int]] {
      def apply(i: Int): Option[Int] = if (i % 2 == 0) Some(i) else None
      override def toString = "(i: Int) => if (i % 2 == 0) Some(i) else None"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToOptionOfIntOdd extends PrettyFunction1[Int, Option[Int]] {
      def apply(i: Int): Option[Int] = if (i % 2 == 1) Some(i) else None
      override def toString = "(i: Int) => if (i % 2 == 1) Some(i) else None"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    val funs: Vector[Int => Option[Int]] =
      Vector(
        IntToOptionOfIntNonZero,
        IntToOptionOfIntPositive,
        IntToOptionOfIntNegative,
        IntToOptionOfIntEven,
        IntToOptionOfIntOdd
      )
    new Generator[Int => Option[Int]] {
      def next(size: Int, edges: List[Int => Option[Int]], rnd: Randomizer): (Int => Option[Int], List[Int => Option[Int]], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[Int => Option[Int]]"
    }
  }

  implicit def function1IntToStringGenerator: Generator[Int => String] = {
    object IntToStringIdentity extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = i.toString
      override def toString = "(i: Int) => i.toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringIncr extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (i + 1).toString
      override def toString = "(i: Int) => (i + 1).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringSquare extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (i * 1).toString
      override def toString = "(i: Int) => (i * i).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringAddMax extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (i + Int.MaxValue).toString
      override def toString = "(i: Int) => (i + Int.MaxValue).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringAddMin extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (i + Int.MinValue).toString
      override def toString = "(i: Int) => (i + Int.MinValue).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringSubtractMax extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (i - Int.MaxValue).toString
      override def toString = "(i: Int) => (i - Int.MaxValue).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringSubtractMin extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (i - Int.MinValue).toString
      override def toString = "(i: Int) => (i - Int.MinValue).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringAbs extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = i.abs.toString
      override def toString = "(i: Int) => i.abs.toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringNegate extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (-i).toString
      override def toString = "(i: Int) => (-i).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToStringComplement extends PrettyFunction1[Int, String] {
      def apply(i: Int): String = (~i).toString
      override def toString = "(i: Int) => (~i).toString"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    val funs: Vector[Int => String] =
      Vector(
        IntToStringIdentity,
        IntToStringIncr,
        IntToStringSquare,
        IntToStringAddMax,
        IntToStringAddMin,
        IntToStringSubtractMax,
        IntToStringSubtractMin,
        IntToStringAbs,
        IntToStringNegate,
        IntToStringComplement
      )
    new Generator[Int => String] {
      def next(size: Int, edges: List[Int => String], rnd: Randomizer): (Int => String, List[Int => String], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[Int => String]"
    }
  }

  implicit def function1StringToIntGenerator: Generator[String => Int] = {
    object StringToIntLength extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = s.length
      override def toString = "(s: String) => s.length"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToIntHead extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = if (s.isEmpty) 0 else s.charAt(0)
      override def toString = "(s: String) => if (s.isEmpty) 0 else s.charAt(0).toInt"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToIntSum extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = s.toList.map(_.toInt).sum
      override def toString = "(s: String) => s.toList.map(_.toInt).sum"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToIntProd extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = s.toList.map(_.toInt).product
      override def toString = "(s: String) => s.toList.map(_.toInt).product"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToIntUpperSum extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = s.toUpperCase.toList.map(_.toInt).sum
      override def toString = "(s: String) => s.toUpperCase.toList.map(_.toInt).sum"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToIntUpperProd extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = s.toUpperCase.toList.map(_.toInt).product
      override def toString = "(s: String) => s.toUpperCase.toList.map(_.toInt).product"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToIntLowerSum extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = s.toLowerCase.toList.map(_.toInt).sum
      override def toString = "(s: String) => s.toLowerCase.toList.map(_.toInt).sum"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToIntLowerProd extends PrettyFunction1[String, Int] {
      def apply(s: String): Int = s.toLowerCase.toList.map(_.toInt).product
      override def toString = "(s: String) => s.toLowerCase.toList.map(_.toInt).product"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    val funs: Vector[String => Int] =
      Vector(
        StringToIntLength,
        StringToIntHead,
        StringToIntSum,
        StringToIntProd,
        StringToIntUpperSum,
        StringToIntUpperProd,
        StringToIntLowerSum,
        StringToIntLowerProd
      )
    new Generator[String => Int] {
      def next(size: Int, edges: List[String => Int], rnd: Randomizer): (String => Int, List[String => Int], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[String => Int]"
    }
  }
  implicit def function1StringToStringGenerator: Generator[String => String] = {
    object StringToStringEcho extends PrettyFunction1[String, String] {
      def apply(s: String): String = s
      override def toString = "(s: String) => s"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToStringReverse extends PrettyFunction1[String, String] {
      def apply(s: String): String = s.reverse
      override def toString = "(s: String) => s.reverse"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToStringLength extends PrettyFunction1[String, String] {
      def apply(s: String): String = s.length.toString
      override def toString = "(s: String) => s.length.toString"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToStringHead extends PrettyFunction1[String, String] {
      def apply(s: String): String = if (s.isEmpty) s else s.substring(0, 1)
      override def toString = "(s: String) => if (s.isEmpty) s else s.substring(0, 1)"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToStringFirstHalf extends PrettyFunction1[String, String] {
      def apply(s: String): String = s.substring(0, s.length / 2)
      override def toString = "(s: String) => s.substring(0, s.length / 2)"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToStringSecondHalf extends PrettyFunction1[String, String] {
      def apply(s: String): String = s.substring(s.length / 2)
      override def toString = "(s: String) => s.substring(s.length / 2)"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToStringLower extends PrettyFunction1[String, String] {
      def apply(s: String): String = s.toLowerCase
      override def toString = "(s: String) => s.toLowerCase"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToStringUpper extends PrettyFunction1[String, String] {
      def apply(s: String): String = s.toUpperCase
      override def toString = "(s: String) => s.toUpperCase"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    val funs: Vector[String => String] =
      Vector(
        StringToStringEcho,
        StringToStringReverse,
        StringToStringLength,
        StringToStringHead,
        StringToStringFirstHalf,
        StringToStringSecondHalf,
        StringToStringLower,
        StringToStringUpper
      )
    new Generator[String => String] {
      def next(size: Int, edges: List[String => String], rnd: Randomizer): (String => String, List[String => String], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[String => String]"
    }
  }

  implicit def function1StringToOptionOfStringGenerator: Generator[String => Option[String]] = {
    object StringToOptionOfStringNonZero extends PrettyFunction1[String, Option[String]] {
      def apply(s: String): Option[String] = if (s.length != 0) Some(s) else None
      override def toString = "(s: String) => if (s.length != 0) Some(s) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringEvenLen extends PrettyFunction1[String, Option[String]] {
      def apply(s: String): Option[String] = if (s.length % 2 == 0) Some(s) else None
      override def toString = "(s: String) => if (s.length % 2 == 0) Some(s) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringOddLen extends PrettyFunction1[String, Option[String]] {
      def apply(s: String): Option[String] = if (s.length % 2 == 1) Some(s) else None
      override def toString = "(s: String) => if (s.length % 2 == 1) Some(s) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringEvenSum extends PrettyFunction1[String, Option[String]] {
      def apply(s: String): Option[String] = if (s.sum % 2 == 0) Some(s) else None
      override def toString = "(s: String) => if (s.sum % 2 == 0) Some(s) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringOddSum extends PrettyFunction1[String, Option[String]] {
      def apply(s: String): Option[String] = if (s.sum % 2 == 1) Some(s) else None
      override def toString = "(s: String) => if (s.sum % 2 == 1) Some(s) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringEvenProd extends PrettyFunction1[String, Option[String]] {
      def apply(s: String): Option[String] = if (s.product % 2 == 0) Some(s) else None
      override def toString = "(s: String) => if (s.product % 2 == 0) Some(s) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringOddProd extends PrettyFunction1[String, Option[String]] {
      def apply(s: String): Option[String] = if (s.product % 2 == 1) Some(s) else None
      override def toString = "(s: String) => if (s.product % 2 == 1) Some(s) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    val funs: Vector[String => Option[String]] =
      Vector(
        StringToOptionOfStringNonZero,
        StringToOptionOfStringEvenLen,
        StringToOptionOfStringOddLen,
        StringToOptionOfStringEvenSum,
        StringToOptionOfStringOddSum,
        StringToOptionOfStringEvenProd,
        StringToOptionOfStringOddProd
      )
    new Generator[String => Option[String]] {
      def next(size: Int, edges: List[String => Option[String]], rnd: Randomizer): (String => Option[String], List[String => Option[String]], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[String => Option[String]]"
    }
  }
  implicit def function1StringToOptionOfLongGenerator: Generator[String => Option[Long]] = {
    object StringToOptionOfStringNonZero extends PrettyFunction1[String, Option[Long]] {
      def apply(s: String): Option[Long] = if (s.length != 0) Some(s.hashCode.toLong) else None
      override def toString = "(s: String) => if (s.length != 0) Some(s.hashCode.toLong) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringEvenLen extends PrettyFunction1[String, Option[Long]] {
      def apply(s: String): Option[Long] = if (s.length % 2 == 0) Some(s.hashCode.toLong) else None
      override def toString = "(s: String) => if (s.length % 2 == 0) Some(s.hashCode.toLong) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringOddLen extends PrettyFunction1[String, Option[Long]] {
      def apply(s: String): Option[Long] = if (s.length % 2 == 1) Some(s.hashCode.toLong) else None
      override def toString = "(s: String) => if (s.length % 2 == 1) Some(s.hashCode.toLong) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringEvenSum extends PrettyFunction1[String, Option[Long]] {
      def apply(s: String): Option[Long] = if (s.sum % 2 == 0) Some(s.hashCode.toLong) else None
      override def toString = "(s: String) => if (s.sum % 2 == 0) Some(s.hashCode.toLong) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringOddSum extends PrettyFunction1[String, Option[Long]] {
      def apply(s: String): Option[Long] = if (s.sum % 2 == 1) Some(s.hashCode.toLong) else None
      override def toString = "(s: String) => if (s.sum % 2 == 1) Some(s.hashCode.toLong) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringEvenProd extends PrettyFunction1[String, Option[Long]] {
      def apply(s: String): Option[Long] = if (s.product % 2 == 0) Some(s.hashCode.toLong) else None
      override def toString = "(s: String) => if (s.product % 2 == 0) Some(s.hashCode.toLong) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToOptionOfStringOddProd extends PrettyFunction1[String, Option[Long]] {
      def apply(s: String): Option[Long] = if (s.product % 2 == 1) Some(s.hashCode.toLong) else None
      override def toString = "(s: String) => if (s.product % 2 == 1) Some(s.hashCode.toLong) else None"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    val funs: Vector[String => Option[Long]] =
      Vector(
        StringToOptionOfStringNonZero,
        StringToOptionOfStringEvenLen,
        StringToOptionOfStringOddLen,
        StringToOptionOfStringEvenSum,
        StringToOptionOfStringOddSum,
        StringToOptionOfStringEvenProd,
        StringToOptionOfStringOddProd
      )
    new Generator[String => Option[Long]] {
      def next(size: Int, edges: List[String => Option[Long]], rnd: Randomizer): (String => Option[Long], List[String => Option[Long]], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[String => Option[Long]]"
    }
  }
  implicit def function1StringToLongGenerator: Generator[String => Long] = {
    object StringToLongLength extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = s.length.toLong
      override def toString = "(s: String) => s.length.toLong"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToLongHead extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = if (s.isEmpty) 0L else s.charAt(0).toLong
      override def toString = "(s: String) => if (s.isEmpty) 0L else s.charAt(0).toLong"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToLongSum extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = s.toList.map(_.toLong).sum
      override def toString = "(s: String) => s.toList.map(_.toLong).sum"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToLongProd extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = s.toList.map(_.toLong).product
      override def toString = "(s: String) => s.toList.map(_.toLong).product"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToLongUpperSum extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = s.toUpperCase.toList.map(_.toLong).sum
      override def toString = "(s: String) => s.toUpperCase.toList.map(_.toLong).sum"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToLongUpperProd extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = s.toUpperCase.toList.map(_.toLong).product
      override def toString = "(s: String) => s.toUpperCase.toList.map(_.toLong).product"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToLongLowerSum extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = s.toLowerCase.toList.map(_.toLong).sum
      override def toString = "(s: String) => s.toLowerCase.toList.map(_.toLong).sum"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    object StringToLongLowerProd extends PrettyFunction1[String, Long] {
      def apply(s: String): Long = s.toLowerCase.toList.map(_.toLong).product
      override def toString = "(s: String) => s.toLowerCase.toList.map(_.toLong).product"
      val paramName: String = "s"
      val paramTypeName: String = "String"
    }
    val funs: Vector[String => Long] =
      Vector(
        StringToLongLength,
        StringToLongHead,
        StringToLongSum,
        StringToLongProd,
        StringToLongUpperSum,
        StringToLongUpperProd,
        StringToLongLowerSum,
        StringToLongLowerProd
      )
    new Generator[String => Long] {
      def next(size: Int, edges: List[String => Long], rnd: Randomizer): (String => Long, List[String => Long], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[String => Long]"
    }
  }

  implicit def function1AToOptionBGenerator[A, B](
    implicit genOfAToB: Generator[A => B],
    genOfBToOptB: Generator[B => Option[B]]
  ): Generator[A => Option[B]] = {
    for {
      aToB <- genOfAToB
      bToOptB <- genOfBToOptB
    } yield {
      (aToB, bToOptB) match {
        case (aToBPretty: PrettyFunction1[A, B], bToOptBPretty: PrettyFunction1[B, Option[B]]) =>
          PrettyFunction1.chain(aToBPretty, bToOptBPretty)
        case _ => aToB andThen bToOptB
      }
    }
  }

  implicit def function1AToBOrCGenerator[A, B, C](
    implicit genOfAToOptB: Generator[A => Option[B]],
    genOfAToC: Generator[A => C]
  ): Generator[A => B Or C] = {
    for {
      aToOptB <- genOfAToOptB
      aToC <- genOfAToC
    } yield {
      (aToOptB, aToC) match {
        case (aToOptBPretty: PrettyFunction1[A, Option[B]], aToCPretty: PrettyFunction1[A, C]) =>
          new PrettyFunction1[A, B Or C] {
            def apply(a: A): B Or C = Or.from(aToOptBPretty(a), aToCPretty(a))
            val paramName = aToOptBPretty.paramName
            val paramTypeName = aToOptBPretty.paramTypeName
            override def toString = {
              s"(${aToOptBPretty.paramName}: ${aToOptBPretty.paramTypeName}) => { " + 
              s"val f = ${aToOptBPretty.toString}; " +
              s"val g = ${aToCPretty.toString}; " +
              s"Or.from(f(${aToOptBPretty.paramName}), g(${aToOptBPretty.paramName})) }"
            }
          }
        case _ => (a: A) => Or.from(aToOptB(a), aToC(a))
      }
    }
  }

  implicit def optionGenerator[T](implicit genOfT: Generator[T]): Generator[Option[T]] =
    new Generator[Option[T]] {
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Option[T]], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to initEdges must be >= 0")
        // Subtract one from length, and we'll wrap those in Somes. Subtract one so that None can be the first edge.
        val (edgesOfT, nextRnd) = genOfT.initEdges(if (maxLength > 0) maxLength - 1 else 0, rnd)
        val edges = None :: edgesOfT.map(t => Some(t))
        (edges, nextRnd)
      }
      def next(size: Int, edges: List[Option[T]], rnd: Randomizer): (Option[T], List[Option[T]], Randomizer) = {

        require(size >= 0, "; the size passed to next must be >= 0")
      
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            if (nextInt % 10 == 0)
              (None, Nil, nextRnd)
            else {
              val (nextT, _, nextNextRnd) = genOfT.next(size, Nil, nextRnd)
              (Some(nextT), Nil, nextNextRnd)
            }
        }
      }
    }

  implicit def orGenerator[G, B](implicit genOfG: Generator[G], genOfB: Generator[B]): Generator[G Or B] =
    new Generator[G Or B] {
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[G Or B], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to initEdges must be >= 0")
        val (edgesOfG, nextRnd) = genOfG.initEdges(maxLength, rnd)
        val (edgesOfB, nextNextRnd) = genOfB.initEdges(maxLength, nextRnd)
        // Fill up to maxLength, favoring Good over Bad if maxLength is odd. Else just dividing it
        // down the middle, half Good, half Bad. And filling in with the other if one side runs out.
        @tailrec
        def loop(count: Int, remainingG: List[G], remainingB: List[B], acc: List[G Or B]): List[G Or B] = {
          (count, remainingG, remainingB) match {
            case (0, _, _) => acc
            case (_, Nil, Nil) => acc
            case (c, gHead :: gTail, Nil) => loop(c - 1, gTail, Nil, Good(gHead) :: acc)
            case (c, Nil, bHead :: bTail) => loop(c - 1, Nil, bTail, Bad(bHead) :: acc)
            case (c, gHead :: gTail, _) if c % 2 == 0 => loop(c - 1, gTail, remainingB, Good(gHead) :: acc)
            case (c, _, bHead :: bTail) => loop(c - 1, remainingG, bTail, Bad(bHead) :: acc)
          }
        }
        (loop(maxLength, edgesOfG, edgesOfB, Nil), nextNextRnd)
      }
      def next(size: Int, edges: List[G Or B], rnd: Randomizer): (G Or B, List[G Or B], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => 
            (head, tail, rnd)
          case _ => 
            val (nextInt, nextRnd) = rnd.nextInt
            if (nextInt % 4 == 0) {
              val (nextB, _, nextRnd) = genOfB.next(size, Nil, rnd)
              (Bad(nextB), Nil, nextRnd)
            }
            else {
              val (nextG, _, nextRnd) = genOfG.next(size, Nil, rnd)
              (Good(nextG), Nil, nextRnd)
            }
        }
      }
    }
  implicit def tuple2Generator[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] =
    new Generator2[A, B, (A, B)]((a: A, b: B) => (a, b), (c: (A, B)) => c._1, (c: (A, B)) => c._2)(genOfA, genOfB)

  implicit def tuple3Generator[A, B, C](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[(A, B, C)] = {
    new Generator[(A, B, C)] { thisGeneratorOfT =>
      private val underlying: Generator[(A, B, C)] = {
        for {
          a <- genOfA
          b <- genOfB
          c <- genOfC
        } yield (a, b, c)
      }
      def next(size: Int, edges: List[(A, B, C)], rnd: Randomizer): ((A, B, C), List[(A, B, C)], Randomizer) = underlying.next(size, edges, rnd)
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[(A, B, C)], Randomizer) = underlying.initEdges(maxLength, rnd)
      override def map[U](f: ((A, B, C)) => U): Generator[U] = underlying.map(f)
      override def flatMap[U](f: ((A, B, C)) => Generator[U]): Generator[U] = underlying.flatMap(f)
      override def canonicals(rnd: Randomizer): (Iterator[(A, B, C)], Randomizer) = underlying.canonicals(rnd) 
      override def shrink(value: (A, B, C), rnd: Randomizer): (Iterator[(A, B, C)], Randomizer) = {
        val (aValue, bValue, cValue) = value
        val (itOfA, rnd1) = genOfA.shrink(aValue, rnd)
        val (itOfB, rnd2) = genOfB.shrink(bValue, rnd1)
        val (itOfC, rnd3) = genOfC.shrink(cValue, rnd2)
        val streamOfA: Stream[A] = itOfA.toStream
        val streamOfB: Stream[B] = itOfB.toStream
        val streamOfC: Stream[C] = itOfC.toStream
        val streamOfABC: Stream[(A, B, C)] =
          for {
            a <- streamOfA
            b <- streamOfB
            c <- streamOfC
          } yield (a, b, c)
        (streamOfABC.iterator, rnd3)
      }
    }
  }
}


