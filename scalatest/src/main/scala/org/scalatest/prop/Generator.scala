/*
 * Copyright 2001-2016 Artima, Inc.
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
import scala.reflect.runtime.universe.TypeTag
import org.scalatest.Resources

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
  def withFilter(f: T => Boolean): Generator[T] = filter(f)
  def filter(f: T => Boolean): Generator[T] =
    new Generator[T] { thisFilteredGeneratorOfT =>
      private final val MaxLoopCount: Int = 100000
      def next(size: Int, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        @tailrec
        def loop(count: Int, nextEdges: List[T], nextRnd: Randomizer): (T, List[T], Randomizer) = {
          if (count > MaxLoopCount)
            throw new IllegalStateException(s"A Generator produced by calling filter or withFilter on another Generator (possibly by using an 'if' clause in a for expression) has filtered out $MaxLoopCount objects in a row in its next method, so aborting. Please define the Generator without using filter or withFilter.")
          val candidateResult = thisGeneratorOfT.next(size, nextEdges, nextRnd)
          val (nextT, nextNextEdges, nextNextRnd) = candidateResult
          if (!f(nextT)) loop(count + 1, nextNextEdges, nextNextRnd)
          else candidateResult
        }
        loop(0, edges, rnd)
      }
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
  implicit def listGenerator[T](implicit genOfT: Generator[T]): Generator[List[T]] with HavingLength[List[T]] =
    new Generator[List[T]] with HavingLength[List[T]] { outerGenOfListOfT => 
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
      def havingLength(len: PosZInt): Generator[List[T]] =
        new Generator[List[T]] {
          override def initEdges(maxLength: Int, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd)
          def next(size: Int, edges: List[List[T]], rnd: Randomizer): (List[T], List[List[T]], Randomizer) =
            outerGenOfListOfT.next(len, edges, rnd)
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd) 
          override def shrink(xs: List[T], rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def toString = s"Generator[List[T] /* having length $len */]"
        }
      def havingLengthsBetween(from: PosZInt, to: PosZInt): Generator[List[T]] = {
        require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
        new Generator[List[T]] {
          // TODO: Think about whether edges should have one list each of length from and to
          override def initEdges(maxLength: Int, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd)
          def next(size: Int, edges: List[List[T]], rnd: Randomizer): (List[T], List[List[T]], Randomizer) =
            outerGenOfListOfT.next(from + (size % (to - from + 1)), edges, rnd) // This assumes from < to, and i'm not guaranteeing that yet
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd) 
          // TODO: Seems like shrink can also behave sensibly by going from from up to xs length
          override def shrink(xs: List[T], rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def toString = s"Generator[List[T] /* having lengths between $from and $to (inclusive) */]"
        }
      }
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

  implicit val function1IntToIntGenerator: Generator[Int => Int] = {
    object IntToIntIdentity extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i
      override def toString = "(i: Int) => i"
      val simpleName: String = "i => i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntIncr extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + 1
      override def toString = "(i: Int) => i + 1"
      val simpleName: String = "i => i + 1"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntIncrBy2 extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + 2
      override def toString = "(i: Int) => i + 2"
      val simpleName: String = "i => i + 2"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntIncrBy3 extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + 3
      override def toString = "(i: Int) => i + 3"
      val simpleName: String = "i => i + 3"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntIncrByMax extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + Int.MaxValue
      override def toString = "(i: Int) => i + Int.MaxValue"
      val simpleName: String = "i => i + Int.MaxValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntIncrByMin extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i + Int.MinValue
      override def toString = "(i: Int) => i + Int.MinValue"
      val simpleName: String = "i => i + Int.MinValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntDecr extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i - 1
      override def toString = "(i: Int) => i - 1"
      val simpleName: String = "i => i - 1"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntDecrBy2 extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i - 2
      override def toString = "(i: Int) => i - 2"
      val simpleName: String = "i => i - 2"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntDecrBy3 extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i - 3
      override def toString = "(i: Int) => i - 3"
      val simpleName: String = "i => i - 3"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntDecrByMax extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i - Int.MaxValue
      override def toString = "(i: Int) => i - Int.MaxValue"
      val simpleName: String = "i => i - Int.MaxValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntDecrByMin extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i - Int.MinValue
      override def toString = "(i: Int) => i - Int.MinValue"
      val simpleName: String = "i => i - Int.MinValue"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntSquare extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i * i
      override def toString = "(i: Int) => i * i"
      val simpleName: String = "i => i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntCube extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i * i * i
      override def toString = "(i: Int) => i * i * i"
      val simpleName: String = "i => i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntHalf extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i / 2
      override def toString = "(i: Int) => i / 2"
      val simpleName: String = "i => i / 2"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntThird extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i / 3
      override def toString = "(i: Int) => i / 3"
      val simpleName: String = "i => i / 3"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntFourth extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = i / 3
      override def toString = "(i: Int) => i / 4"
      val simpleName: String = "i => i / 4"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntNegate extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = -i
      override def toString = "(i: Int) => -i"
      val simpleName: String = "i => -i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToIntComplement extends PrettyFunction1[Int, Int] {
      def apply(i: Int): Int = ~i
      override def toString = "(i: Int) => ~i"
      val simpleName: String = "i => ~i"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    val funs: Vector[Int => Int] =
      Vector(
        IntToIntIdentity,
        IntToIntIncr,
        IntToIntIncrBy2,
        IntToIntIncrBy3,
        IntToIntIncrByMax,
        IntToIntIncrByMin,
        IntToIntDecr,
        IntToIntDecrBy2,
        IntToIntDecrBy3,
        IntToIntDecrByMax,
        IntToIntDecrByMin,
        IntToIntSquare,
        IntToIntCube,
        IntToIntHalf,
        IntToIntThird,
        IntToIntFourth,
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

  implicit val function1IntToShortGenerator: Generator[Int => Short] = {
    object IntToShortConvert extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = i.toShort
      override def toString = "(i: Int) => i.toShort"
      val simpleName: String = "i => i.toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortIncr extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i + 1).toShort
      override def toString = "(i: Int) => (i + 1).toShort"
      val simpleName: String = "i => (i + 1).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortIncrBy2 extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i + 2).toShort
      override def toString = "(i: Int) => (i + 2).toShort"
      val simpleName: String = "i => (i + 2).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortIncrBy3 extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i + 3).toShort
      override def toString = "(i: Int) => (i + 3).toShort"
      val simpleName: String = "i => (i + 3).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortIncrByMax extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i + Int.MaxValue).toShort
      override def toString = "(i: Int) => (i + Int.MaxValue).toShort"
      val simpleName: String = "i => (i + Int.MaxValue).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortIncrByMin extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i + Int.MinValue).toShort
      override def toString = "(i: Int) => (i + Int.MinValue).toShort"
      val simpleName: String = "i => (i + Int.MinValue).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortDecr extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i - 1).toShort
      override def toString = "(i: Int) => (i - 1).toShort"
      val simpleName: String = "i => (i - 1).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortDecrBy2 extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i - 2).toShort
      override def toString = "(i: Int) => (i - 2).toShort"
      val simpleName: String = "i => (i - 2).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortDecrBy3 extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i - 3).toShort
      override def toString = "(i: Int) => (i - 3).toShort"
      val simpleName: String = "i => (i - 3).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortDecrByMax extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i - Int.MaxValue).toShort
      override def toString = "(i: Int) => (i - Int.MaxValue).toShort"
      val simpleName: String = "i => (i - Int.MaxValue).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortDecrByMin extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i - Int.MinValue).toShort
      override def toString = "(i: Int) => (i - Int.MinValue).toShort"
      val simpleName: String = "i => (i - Int.MinValue).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortSquare extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i * i).toShort
      override def toString = "(i: Int) => (i * i).toShort"
      val simpleName: String = "i => (i).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortCube extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i * i * i).toShort
      override def toString = "(i: Int) => (i * i * i).toShort"
      val simpleName: String = "i => (i).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortHalf extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i / 2).toShort
      override def toString = "(i: Int) => (i / 2).toShort"
      val simpleName: String = "i => (i / 2).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortThird extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i / 3).toShort
      override def toString = "(i: Int) => (i / 3).toShort"
      val simpleName: String = "i => (i / 3).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortFourth extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (i / 3).toShort
      override def toString = "(i: Int) => (i / 4).toShort"
      val simpleName: String = "i => (i / 4).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortNegate extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (-i).toShort
      override def toString = "(i: Int) => (-i).toShort"
      val simpleName: String = "i => (-i).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    object IntToShortComplement extends PrettyFunction1[Int, Short] {
      def apply(i: Int): Short = (~i).toShort
      override def toString = "(i: Int) => (~i).toShort"
      val simpleName: String = "i => (~i).toShort"
      val paramName: String = "i"
      val paramTypeName: String = "Int"
    }
    val funs: Vector[Int => Short] =
      Vector(
        IntToShortConvert,
        IntToShortIncr,
        IntToShortIncrBy2,
        IntToShortIncrBy3,
        IntToShortIncrByMax,
        IntToShortIncrByMin,
        IntToShortDecr,
        IntToShortDecrBy2,
        IntToShortDecrBy3,
        IntToShortDecrByMax,
        IntToShortDecrByMin,
        IntToShortSquare,
        IntToShortCube,
        IntToShortHalf,
        IntToShortThird,
        IntToShortFourth,
        IntToShortNegate,
        IntToShortComplement
      )
    new Generator[Int => Short] {
      def next(size: Int, edges: List[Int => Short], rnd: Randomizer): (Int => Short, List[Int => Short], Randomizer) = {
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
      override def toString = "Generator[Int => Short]"
    }
  }

  implicit val function1ShortToByteGenerator: Generator[Short => Byte] = {
    object ShortToByteConvert extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = i.toByte
      override def toString = "(i: Short) => i.toByte"
      val simpleName: String = "i => i.toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteIncr extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i + 1).toByte
      override def toString = "(i: Short) => (i + 1).toByte"
      val simpleName: String = "i => (i + 1).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteIncrBy2 extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i + 2).toByte
      override def toString = "(i: Short) => (i + 2).toByte"
      val simpleName: String = "i => (i + 2).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteIncrBy3 extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i + 3).toByte
      override def toString = "(i: Short) => (i + 3).toByte"
      val simpleName: String = "i => (i + 3).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteIncrByMax extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i + Short.MaxValue).toByte
      override def toString = "(i: Short) => (i + Short.MaxValue).toByte"
      val simpleName: String = "i => (i + Short.MaxValue).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteIncrByMin extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i + Short.MinValue).toByte
      override def toString = "(i: Short) => (i + Short.MinValue).toByte"
      val simpleName: String = "i => (i + Short.MinValue).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteDecr extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i - 1).toByte
      override def toString = "(i: Short) => (i - 1).toByte"
      val simpleName: String = "i => (i - 1).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteDecrBy2 extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i - 2).toByte
      override def toString = "(i: Short) => (i - 2).toByte"
      val simpleName: String = "i => (i - 2).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteDecrBy3 extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i - 3).toByte
      override def toString = "(i: Short) => (i - 3).toByte"
      val simpleName: String = "i => (i - 3).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteDecrByMax extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i - Short.MaxValue).toByte
      override def toString = "(i: Short) => (i - Short.MaxValue).toByte"
      val simpleName: String = "i => (i - Short.MaxValue).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteDecrByMin extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i - Short.MinValue).toByte
      override def toString = "(i: Short) => (i - Short.MinValue).toByte"
      val simpleName: String = "i => (i - Short.MinValue).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteSquare extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i * i).toByte
      override def toString = "(i: Short) => (i * i).toByte"
      val simpleName: String = "i => (i).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteCube extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i * i * i).toByte
      override def toString = "(i: Short) => (i * i * i).toByte"
      val simpleName: String = "i => (i).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteHalf extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i / 2).toByte
      override def toString = "(i: Short) => (i / 2).toByte"
      val simpleName: String = "i => (i / 2).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteThird extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i / 3).toByte
      override def toString = "(i: Short) => (i / 3).toByte"
      val simpleName: String = "i => (i / 3).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteFourth extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (i / 3).toByte
      override def toString = "(i: Short) => (i / 4).toByte"
      val simpleName: String = "i => (i / 4).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteNegate extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (-i).toByte
      override def toString = "(i: Short) => (-i).toByte"
      val simpleName: String = "i => (-i).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    object ShortToByteComplement extends PrettyFunction1[Short, Byte] {
      def apply(i: Short): Byte = (~i).toByte
      override def toString = "(i: Short) => (~i).toByte"
      val simpleName: String = "i => (~i).toByte"
      val paramName: String = "i"
      val paramTypeName: String = "Short"
    }
    val funs: Vector[Short => Byte] =
      Vector(
        ShortToByteConvert,
        ShortToByteIncr,
        ShortToByteIncrBy2,
        ShortToByteIncrBy3,
        ShortToByteIncrByMax,
        ShortToByteIncrByMin,
        ShortToByteDecr,
        ShortToByteDecrBy2,
        ShortToByteDecrBy3,
        ShortToByteDecrByMax,
        ShortToByteDecrByMin,
        ShortToByteSquare,
        ShortToByteCube,
        ShortToByteHalf,
        ShortToByteThird,
        ShortToByteFourth,
        ShortToByteNegate,
        ShortToByteComplement
      )
    new Generator[Short => Byte] {
      def next(size: Int, edges: List[Short => Byte], rnd: Randomizer): (Short => Byte, List[Short => Byte], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextShort, nextRnd) = rnd.nextShort
            val idx = (if (nextShort == Short.MinValue) Short.MaxValue else nextShort.abs) % funs.length
            (funs(idx), Nil, nextRnd)
        }
      }
      override def toString = "Generator[Short => Byte]"
    }
  }

  implicit def function1AToBGenerator[A, B](implicit genOfB: Generator[B], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B]): Generator[A => B] = {
    new Generator[A => B] {
      def next(size: Int, edges: List[A => B], rnd: Randomizer): (A => B, List[A => B], Randomizer) = {

        val intToIntGen: Generator[Int => Int] = function1IntToIntGenerator
        val (intToInt, _, rnd1) = intToIntGen.next(10, Nil, rnd)

        object AToB extends PrettyFunction1[A, B] {
          def apply(a: A): B = org.scalatest.prop.valueOf[B](a, intToInt)
          val simpleName = {
            val typeOfA = typeTagOfA.tpe
            val typeOfB = typeTagOfB.tpe
            val intToIntName: String = 
              intToInt match {
                case prf: PrettyFunction1[_, _] => prf.simpleName
                case _ => intToInt.toString
              }
            s"o => org.scalatest.prop.valueOf[$typeOfB](o, $intToIntName)"
          }
          override def toString = {
            val typeOfA = typeTagOfA.tpe
            val typeOfB = typeTagOfB.tpe
            val intToIntName: String = 
              intToInt match {
                case prf: PrettyFunction1[_, _] => prf.simpleName
                case _ => intToInt.toString
              }
            s"(o: $typeOfA) => org.scalatest.prop.valueOf[$typeOfB](o, $intToIntName)"
          }
          val paramName: String = "a"
          val paramTypeName: String = "A"
        }

        (AToB, Nil, rnd1)
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
    new GeneratorFor2[A, B, (A, B)]((a: A, b: B) => (a, b), (c: (A, B)) => c)(genOfA, genOfB)

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


