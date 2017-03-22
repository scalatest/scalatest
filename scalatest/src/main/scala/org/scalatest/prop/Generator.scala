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
import org.scalactic.source.TypeInfo
import org.scalatest.Resources
import CommonGenerators.first1000Primes

trait Generator[T] { thisGeneratorOfT =>

  def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[T], Randomizer) = (Nil, rnd)

  def next(szp: SizeParam, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer)
  def map[U](f: T => U): Generator[U] =
    new Generator[U] { thisGeneratorOfU => 
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[U], Randomizer) = {
        val (listOfT, nextRnd) = thisGeneratorOfT.initEdges(maxLength, rnd)
        (listOfT.map(f), nextRnd)
      }
      def next(szp: SizeParam, edges: List[U], rnd: Randomizer): (U, List[U], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextT, _, nextRandomizer) = thisGeneratorOfT.next(szp, Nil, rnd)
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[U], Randomizer) = {
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
      def next(szp: SizeParam, edges: List[U], rnd: Randomizer): (U, List[U], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextT, _, nextRandomizer) = thisGeneratorOfT.next(szp, Nil, rnd)
            val genOfU: Generator[U] = f(nextT)
            val (u, _, nextNextRandomizer) = genOfU.next(szp, Nil, nextRandomizer)
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
      def next(szp: SizeParam, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        @tailrec
        def loop(count: Int, nextEdges: List[T], nextRnd: Randomizer): (T, List[T], Randomizer) = {
          if (count > MaxLoopCount)
            throw new IllegalStateException(s"A Generator produced by calling filter or withFilter on another Generator (possibly by using an 'if' clause in a for expression) has filtered out $MaxLoopCount objects in a row in its next method, so aborting. Please define the Generator without using filter or withFilter.")
          val candidateResult = thisGeneratorOfT.next(szp, nextEdges, nextRnd)
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
    val maxSize = PosZInt(100)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 100, inclusive
    val (value, _, _) = next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    value
  }
  def samples(length: PosInt): List[T] = {
    @tailrec
    def loop(count: Int, rnd: Randomizer, acc: List[T]): List[T] = {
      if (count == length.value) acc
      else {
        val maxSize = PosZInt(100)
        val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 100, inclusive
        val (value, _, nextNextRnd) = next(SizeParam(PosZInt(0), maxSize, size), Nil, rnd)
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
      def next(szp: SizeParam, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            arb.arbitrary.apply(Gen.Parameters.default.withSize(szp.size), Seed(rnd.seed)) match {
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

  // I don't want to make Generator covariant, because then an implicit search for a Generator[<supertype>] would
  // be satisfied if it finds just a Generator[<subtype>], but that would not generate anything except subtypes.
  // It would be sound, but you would't get a good variety of supertype values.
  import scala.language.implicitConversions
  implicit def widen[T, U](genOfT: Generator[T])(implicit ev: T <:< U): Generator[U] = genOfT.map(o => (o: U))

  private[prop] val byteEdges = List(Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue)
  private[prop] val shortEdges = List(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue)
  private[prop] val charEdges = List(Char.MinValue, Char.MaxValue)
  private[prop] val intEdges = List(Int.MinValue, -1, 0, 1, Int.MaxValue)
  private[prop] val longEdges = List(Long.MinValue, -1, 0, 1, Long.MaxValue)
  private[prop] val floatEdges = List(0.0f)
  private[prop] val doubleEdges = List(0.0)
  private[prop] val posIntEdges = List(PosInt(1), PosInt.MaxValue)
  private[prop] val posZIntEdges = List(PosZInt(0), PosZInt(1), PosZInt.MaxValue)
  private[prop] val posLongEdges = List(PosLong(1L), PosLong.MaxValue)
  private[prop] val posZLongEdges = List(PosZLong(0L), PosZLong(1L), PosZLong.MaxValue)
  private[prop] val posFloatEdges = List(PosFloat(1.0f), PosFloat.MaxValue)
  private[prop] val posZFloatEdges = List(PosZFloat(0.0f), PosZFloat(1.0f), PosZFloat.MaxValue)
  private[prop] val posFiniteFloatEdges = List(PosFiniteFloat(1.0f), PosFiniteFloat.MaxValue)
  private[prop] val posZFiniteFloatEdges = List(PosZFiniteFloat(0.0f), PosZFiniteFloat(1.0f), PosZFiniteFloat.MaxValue)
  private[prop] val posDoubleEdges = List(PosDouble(1.0), PosDouble.MaxValue)
  private[prop] val posFiniteDoubleEdges = List(PosFiniteDouble(1.0), PosFiniteDouble.MaxValue)
  private[prop] val finiteDoubleEdges = List(FiniteDouble.MinValue, FiniteDouble(-1.0), FiniteDouble(0.0), FiniteDouble(1.0), FiniteDouble.MaxValue)
  private[prop] val finiteFloatEdges = List(FiniteFloat.MinValue, FiniteFloat(-1.0F), FiniteFloat(0.0F), FiniteFloat(1.0F), FiniteFloat.MaxValue)
  private[prop] val posZDoubleEdges = List(PosZDouble(0.0), PosZDouble(1.0), PosZDouble.MaxValue)
  private[prop] val posZFiniteDoubleEdges = List(PosZFiniteDouble(0.0), PosZFiniteDouble(1.0), PosZFiniteDouble.MaxValue)
  private[prop] val nonZeroDoubleEdges = List(NonZeroDouble.MinValue, NonZeroDouble(-1.0), NonZeroDouble(1.0), NonZeroDouble.MaxValue)
  private[prop] val nonZeroFiniteDoubleEdges = List(NonZeroFiniteDouble.MinValue, NonZeroFiniteDouble(-1.0), NonZeroFiniteDouble(1.0), NonZeroFiniteDouble.MaxValue)
  private[prop] val nonZeroFloatEdges = List(NonZeroFloat.MinValue, NonZeroFloat(-1.0F), NonZeroFloat(1.0F), NonZeroFloat.MaxValue)
  private[prop] val nonZeroFiniteFloatEdges = List(NonZeroFiniteFloat.MinValue, NonZeroFiniteFloat(-1.0F), NonZeroFiniteFloat(1.0F), NonZeroFiniteFloat.MaxValue)
  private[prop] val nonZeroIntEdges = List(NonZeroInt.MinValue, NonZeroInt(-1), NonZeroInt(1), NonZeroInt.MaxValue)
  private[prop] val nonZeroLongEdges = List(NonZeroLong.MinValue, NonZeroLong(-1L), NonZeroLong(1L), NonZeroLong.MaxValue)
  private[prop] val negDoubleEdges = List(NegDouble.MinValue, NegDouble(-1.0), NegDouble.MaxValue)
  private[prop] val negFiniteDoubleEdges = List(NegFiniteDouble.MinValue, NegFiniteDouble(-1.0), NegFiniteDouble.MaxValue)
  private[prop] val negFloatEdges = List(NegFloat.MinValue, NegFloat(-1.0F), NegFloat.MaxValue)
  private[prop] val negFiniteFloatEdges = List(NegFiniteFloat.MinValue, NegFiniteFloat(-1.0F), NegFiniteFloat.MaxValue)
  private[prop] val negIntEdges = List(NegInt.MinValue, NegInt.MaxValue)
  private[prop] val negLongEdges = List(NegLong.MinValue, NegLong.MaxValue)
  private[prop] val negZDoubleEdges = List(NegZDouble.MinValue, NegZDouble(-1.0), NegZDouble.ensuringValid(-Double.MinPositiveValue), NegZDouble.MaxValue)
  private[prop] val negZFiniteDoubleEdges = List(NegZFiniteDouble.MinValue, NegZFiniteDouble(-1.0), NegZFiniteDouble.ensuringValid(-Double.MinPositiveValue), NegZFiniteDouble.MaxValue)
  private[prop] val negZFloatEdges = List(NegZFloat.MinValue, NegZFloat(-1.0F), NegZFloat.ensuringValid(-Float.MinPositiveValue), NegZFloat.MaxValue)
  private[prop] val negZFiniteFloatEdges = List(NegZFiniteFloat.MinValue, NegZFiniteFloat(-1.0F), NegZFiniteFloat.ensuringValid(-Float.MinPositiveValue), NegZFiniteFloat.MaxValue)
  private[prop] val negZIntEdges = List(NegZInt.MinValue, NegZInt(-1), NegZInt.MaxValue)
  private[prop] val negZLongEdges = List(NegZLong.MinValue, NegZLong(-1L), NegZLong.MaxValue)
  private[prop] val numericCharEdges = List(NumericChar('0'))

  implicit val byteGenerator: Generator[Byte] =
    new Generator[Byte] {

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Byte], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(byteEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Byte], rnd: Randomizer): (Byte, List[Byte], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Short], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(shortEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Short], rnd: Randomizer): (Short, List[Short], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Char], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(charEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Char], rnd: Randomizer): (Char, List[Char], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Int], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(intEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Int], rnd: Randomizer): (Int, List[Int], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Long], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(longEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Long], rnd: Randomizer): (Long, List[Long], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Float], Randomizer) = {
        (floatEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[Float], rnd: Randomizer): (Float, List[Float], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Double], Randomizer) = {
        (doubleEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[Double], rnd: Randomizer): (Double, List[Double], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosInt], rnd: Randomizer): (PosInt, List[PosInt], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZInt], rnd: Randomizer): (PosZInt, List[PosZInt], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosLong], rnd: Randomizer): (PosLong, List[PosLong], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZLong], rnd: Randomizer): (PosZLong, List[PosZLong], Randomizer) = {
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosFloat], rnd: Randomizer): (PosFloat, List[PosFloat], Randomizer) = {
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

  implicit val posFiniteFloatGenerator: Generator[PosFiniteFloat] =
    new Generator[PosFiniteFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosFiniteFloat], rnd: Randomizer): (PosFiniteFloat, List[PosFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posFiniteFloat, nextRnd) = rnd.nextPosFiniteFloat
            (posFiniteFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosFiniteFloat]"
    }

  implicit val finiteFloatGenerator: Generator[FiniteFloat] =
    new Generator[FiniteFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[FiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(finiteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[FiniteFloat], rnd: Randomizer): (FiniteFloat, List[FiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (finiteFloat, nextRnd) = rnd.nextFiniteFloat
            (finiteFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[FiniteFloat]"
    }

  implicit val finiteDoubleGenerator: Generator[FiniteDouble] =
    new Generator[FiniteDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[FiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(finiteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[FiniteDouble], rnd: Randomizer): (FiniteDouble, List[FiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (finiteDouble, nextRnd) = rnd.nextFiniteDouble
            (finiteDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[FiniteDouble]"
    }

  implicit val posZFloatGenerator: Generator[PosZFloat] =
    new Generator[PosZFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZFloat], rnd: Randomizer): (PosZFloat, List[PosZFloat], Randomizer) = {
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

  implicit val posZFiniteFloatGenerator: Generator[PosZFiniteFloat] =
    new Generator[PosZFiniteFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZFiniteFloat], rnd: Randomizer): (PosZFiniteFloat, List[PosZFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZFiniteFloat, nextRnd) = rnd.nextPosZFiniteFloat
            (posZFiniteFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosZFiniteFloat]"
    }

  implicit val posDoubleGenerator: Generator[PosDouble] =
    new Generator[PosDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosDouble], rnd: Randomizer): (PosDouble, List[PosDouble], Randomizer) = {
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

  implicit val posFiniteDoubleGenerator: Generator[PosFiniteDouble] =
    new Generator[PosFiniteDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosFiniteDouble], rnd: Randomizer): (PosFiniteDouble, List[PosFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posFiniteDouble, nextRnd) = rnd.nextPosFiniteDouble
            (posFiniteDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosFiniteDouble]"
    }

  implicit val posZDoubleGenerator: Generator[PosZDouble] =
    new Generator[PosZDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZDouble], rnd: Randomizer): (PosZDouble, List[PosZDouble], Randomizer) = {
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

  implicit val posZFiniteDoubleGenerator: Generator[PosZFiniteDouble] =
    new Generator[PosZFiniteDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZFiniteDouble], rnd: Randomizer): (PosZFiniteDouble, List[PosZFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZFiniteDouble, nextRnd) = rnd.nextPosZFiniteDouble
            (posZFiniteDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[PosZFiniteDouble]"
    }

  implicit val nonZeroDoubleGenerator: Generator[NonZeroDouble] =
    new Generator[NonZeroDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroDouble], rnd: Randomizer): (NonZeroDouble, List[NonZeroDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nonZeroDouble, nextRnd) = rnd.nextNonZeroDouble
            (nonZeroDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NonZeroDouble]"
    }

  implicit val nonZeroFiniteDoubleGenerator: Generator[NonZeroFiniteDouble] =
    new Generator[NonZeroFiniteDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroFiniteDouble], rnd: Randomizer): (NonZeroFiniteDouble, List[NonZeroFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nonZeroFiniteDouble, nextRnd) = rnd.nextNonZeroFiniteDouble
            (nonZeroFiniteDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NonZeroFiniteDouble]"
    }

  implicit val nonZeroFloatGenerator: Generator[NonZeroFloat] =
    new Generator[NonZeroFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroFloat], rnd: Randomizer): (NonZeroFloat, List[NonZeroFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nonZeroFloat, nextRnd) = rnd.nextNonZeroFloat
            (nonZeroFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NonZeroFloat]"
    }

  implicit val nonZeroFiniteFloatGenerator: Generator[NonZeroFiniteFloat] =
    new Generator[NonZeroFiniteFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroFiniteFloat], rnd: Randomizer): (NonZeroFiniteFloat, List[NonZeroFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nonZeroFiniteFloat, nextRnd) = rnd.nextNonZeroFiniteFloat
            (nonZeroFiniteFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NonZeroFiniteFloat]"
    }

  implicit val nonZeroIntGenerator: Generator[NonZeroInt] =
    new Generator[NonZeroInt] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroInt], rnd: Randomizer): (NonZeroInt, List[NonZeroInt], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nonZeroInt, nextRnd) = rnd.nextNonZeroInt
            (nonZeroInt, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NonZeroInt]"
    }

  implicit val nonZeroLongGenerator: Generator[NonZeroLong] =
    new Generator[NonZeroLong] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroLong], rnd: Randomizer): (NonZeroLong, List[NonZeroLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nonZeroLong, nextRnd) = rnd.nextNonZeroLong
            (nonZeroLong, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NonZeroLong]"
    }

  implicit val negDoubleGenerator: Generator[NegDouble] =
    new Generator[NegDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegDouble], rnd: Randomizer): (NegDouble, List[NegDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negDouble, nextRnd) = rnd.nextNegDouble
            (negDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegDouble]"
    }

  implicit val negFiniteDoubleGenerator: Generator[NegFiniteDouble] =
    new Generator[NegFiniteDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegFiniteDouble], rnd: Randomizer): (NegFiniteDouble, List[NegFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negFiniteDouble, nextRnd) = rnd.nextNegFiniteDouble
            (negFiniteDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegFiniteDouble]"
    }

  implicit val negFloatGenerator: Generator[NegFloat] =
    new Generator[NegFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegFloat], rnd: Randomizer): (NegFloat, List[NegFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negFloat, nextRnd) = rnd.nextNegFloat
            (negFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegFloat]"
    }

  implicit val negFiniteFloatGenerator: Generator[NegFiniteFloat] =
    new Generator[NegFiniteFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegFiniteFloat], rnd: Randomizer): (NegFiniteFloat, List[NegFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negFiniteFloat, nextRnd) = rnd.nextNegFiniteFloat
            (negFiniteFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegFiniteFloat]"
    }

  implicit val negIntGenerator: Generator[NegInt] =
    new Generator[NegInt] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegInt], rnd: Randomizer): (NegInt, List[NegInt], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negInt, nextRnd) = rnd.nextNegInt
            (negInt, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegInt]"
    }

  implicit val negLongGenerator: Generator[NegLong] =
    new Generator[NegLong] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegLong], rnd: Randomizer): (NegLong, List[NegLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negLong, nextRnd) = rnd.nextNegLong
            (negLong, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegLong]"
    }

  implicit val negZDoubleGenerator: Generator[NegZDouble] =
    new Generator[NegZDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZDouble], rnd: Randomizer): (NegZDouble, List[NegZDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negZDouble, nextRnd) = rnd.nextNegZDouble
            (negZDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegZDouble]"
    }

  implicit val negZFiniteDoubleGenerator: Generator[NegZFiniteDouble] =
    new Generator[NegZFiniteDouble] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZFiniteDouble], rnd: Randomizer): (NegZFiniteDouble, List[NegZFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negZFiniteDouble, nextRnd) = rnd.nextNegZFiniteDouble
            (negZFiniteDouble, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegZFiniteDouble]"
    }

  implicit val negZFloatGenerator: Generator[NegZFloat] =
    new Generator[NegZFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZFloat], rnd: Randomizer): (NegZFloat, List[NegZFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negZFloat, nextRnd) = rnd.nextNegZFloat
            (negZFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegZFloat]"
    }

  implicit val negZFiniteFloatGenerator: Generator[NegZFiniteFloat] =
    new Generator[NegZFiniteFloat] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZFiniteFloat], rnd: Randomizer): (NegZFiniteFloat, List[NegZFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negZFiniteFloat, nextRnd) = rnd.nextNegZFiniteFloat
            (negZFiniteFloat, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegZFiniteFloat]"
    }

  implicit val negZIntGenerator: Generator[NegZInt] =
    new Generator[NegZInt] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZInt], rnd: Randomizer): (NegZInt, List[NegZInt], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negZInt, nextRnd) = rnd.nextNegZInt
            (negZInt, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegZInt]"
    }

  implicit val negZLongGenerator: Generator[NegZLong] =
    new Generator[NegZLong] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZLong], rnd: Randomizer): (NegZLong, List[NegZLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (negZLong, nextRnd) = rnd.nextNegZLong
            (negZLong, Nil, nextRnd)
        }
      }
      override def toString = "Generator[NegZLong]"
    }

  implicit val numericCharGenerator: Generator[NumericChar] =
    new Generator[NumericChar] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NumericChar], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(numericCharEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NumericChar], rnd: Randomizer): (NumericChar, List[NumericChar], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (posZInt, nextRnd) = rnd.choosePosZInt(PosZInt.ensuringValid(0), PosZInt.ensuringValid(9))
            (NumericChar.ensuringValid((posZInt.value + 48).toChar), Nil, nextRnd)
        }
      }
      override def toString = "Generator[NumericChar]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  implicit val stringGenerator: Generator[String] =
    new Generator[String] {
      private val stringEdges = List("")
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[String], Randomizer) = {
        (stringEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[String], rnd: Randomizer): (String, List[String], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (s, nextRnd) = rnd.nextString(szp.size)
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
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = {
        (listEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (List[T], List[List[T]], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (listOfT, nextRnd) = rnd.nextList[T](szp.size)
            (listOfT, Nil, nextRnd)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = {
        val (canonicalsOfT, rnd1) = genOfT.canonicals(rnd)
        (canonicalsOfT.map(t => List(t)), rnd1)
      }
      override def shrink(xs: List[T], rnd: Randomizer): (Iterator[List[T]], Randomizer) = {

        if (xs.isEmpty) (Iterator.empty, rnd)
        else {
          val (canonicalTsIt, rnd1) = genOfT.canonicals(rnd)
          val canonicalTs = canonicalTsIt.toList
          // Start with Lists of length one each of which contain one of the canonical values
          // of the element type.
          val canonicalListOfTsIt: Iterator[List[T]] = canonicalTs.map(t => List(t)).toIterator

          // Only include distinctListsOfTs if the list to shrink (xs) does not contain
          // just one element itself. If it does, then xs will appear in the output, which
          // we don't need, since we already know it fails.
          val distinctListOfTsIt: Iterator[List[T]] =
            if (xs.nonEmpty && xs.tail.nonEmpty) {
              val distinctListOfTs: List[List[T]] =
                for (x <- xs if !canonicalTs.contains(x)) yield List(x)
              distinctListOfTs.iterator
            }
            else Iterator.empty

          // The last batch of candidate shrunken values are just slices of the list starting at
          // 0 with size doubling each time.
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

          (Iterator(Nil) ++ canonicalListOfTsIt ++ distinctListOfTsIt ++ lastBatch, rnd1)
        }
      }
      override def toString = "Generator[List[T]]"
      def havingSize(size: PosZInt): Generator[List[T]] = // TODO: add with HavingLength again
        // No edges and no shrinking. Since they said they want a list of a particular length,
        // that is what they'll get.
        new Generator[List[T]] {
          override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd) // TODO: filter lists's edges by valid size
          def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (List[T], List[List[T]], Randomizer) =
            outerGenOfListOfT.next(SizeParam(PosZInt(0), szp.maxSize, size), edges, rnd) // TODO: SizeParam(size, size, size)?
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def shrink(xs: List[T], rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def toString = s"Generator[List[T] /* having length $size */]"
        }
      def havingSizesBetween(from: PosZInt, to: PosZInt): Generator[List[T]] = { // TODO: add with HavingLength again
        require(from != to, Resources.fromEqualToToHavingSizesBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingSizesBetween(from, to))
        new Generator[List[T]] {
          // I don't think edges should have one list each of length from and to, because they would
          // need to have random contents, and that doesn't seem like an edge.
          override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd) // TODO: filter lists's edges by valid size
          // Specify how size is used.
          def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (List[T], List[List[T]], Randomizer) = {
            val nextSize = {
              val candidate: Int = (from + (szp.size.toFloat * (to - from).toFloat / (szp.maxSize + 1).toFloat)).round
              if (candidate > to) to
              else if (candidate < from) from
              else PosZInt.ensuringValid(candidate)
            }
            // TODO: should minSize not be from from now on.
            outerGenOfListOfT.next(SizeParam(PosZInt(0), to, nextSize), edges, rnd) // This assumes from < to, and i'm not guaranteeing that yet
          }
          // If from is either 0 or 1, return the canonicals of the outer Generator.
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) =
            if (from <= 1) outerGenOfListOfT.canonicals(rnd) else (Iterator.empty, rnd)
          // TODO: Shrink can go from from up to xs length
          override def shrink(xs: List[T], rnd: Randomizer): (Iterator[List[T]], Randomizer) = outerGenOfListOfT.shrink(xs, rnd)
          override def toString = s"Generator[List[T] /* having lengths between $from and $to (inclusive) */]"
        }
      }
      def havingSizesDeterminedBy(f: SizeParam => SizeParam): Generator[List[T]] = // TODO: add with HavingLength again
        new Generator[List[T]] {
          override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd)
          def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (List[T], List[List[T]], Randomizer) =
            outerGenOfListOfT.next(f(szp), edges, rnd)
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def shrink(xs: List[T], rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def toString = s"Generator[List[T] /* having lengths determined by a function */]"
        }
    }

  implicit def function0Generator[T](implicit genOfT: Generator[T]): Generator[() => T] = {
    new Generator[() => T] { thisGeneratorOfFunction0 =>
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[() => T], Randomizer) = {
        val (edgesOfT, nextRnd) = genOfT.initEdges(maxLength, rnd)
        val edges = edgesOfT.map(t => PrettyFunction0(t))
        (edges, nextRnd)
      }
      def next(szp: SizeParam, edges: List[() => T], rnd: Randomizer): (() => T, List[() => T], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextT, _, nextRnd) = genOfT.next(szp, Nil, rnd)
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
    object IntToIntIdentity extends (Int => Int) {
      def apply(i: Int): Int = i
      override def toString = "(i: Int) => i"
    }
    object IntToIntIncr extends (Int => Int) {
      def apply(i: Int): Int = i + 1
      override def toString = "(i: Int) => i + 1"
    }
    object IntToIntIncrBy2 extends (Int => Int) {
      def apply(i: Int): Int = i + 2
      override def toString = "(i: Int) => i + 2"
    }
    object IntToIntIncrBy3 extends (Int => Int) {
      def apply(i: Int): Int = i + 3
      override def toString = "(i: Int) => i + 3"
    }
    object IntToIntIncrByMax extends (Int => Int) {
      def apply(i: Int): Int = i + Int.MaxValue
      override def toString = "(i: Int) => i + Int.MaxValue"
    }
    object IntToIntIncrByMin extends (Int => Int) {
      def apply(i: Int): Int = i + Int.MinValue
      override def toString = "(i: Int) => i + Int.MinValue"
    }
    object IntToIntDecr extends (Int => Int) {
      def apply(i: Int): Int = i - 1
      override def toString = "(i: Int) => i - 1"
    }
    object IntToIntDecrBy2 extends (Int => Int) {
      def apply(i: Int): Int = i - 2
      override def toString = "(i: Int) => i - 2"
    }
    object IntToIntDecrBy3 extends (Int => Int) {
      def apply(i: Int): Int = i - 3
      override def toString = "(i: Int) => i - 3"
    }
    object IntToIntDecrByMax extends (Int => Int) {
      def apply(i: Int): Int = i - Int.MaxValue
      override def toString = "(i: Int) => i - Int.MaxValue"
    }
    object IntToIntDecrByMin extends (Int => Int) {
      def apply(i: Int): Int = i - Int.MinValue
      override def toString = "(i: Int) => i - Int.MinValue"
    }
    object IntToIntSquare extends (Int => Int) {
      def apply(i: Int): Int = i * i
      override def toString = "(i: Int) => i * i"
    }
    object IntToIntCube extends (Int => Int) {
      def apply(i: Int): Int = i * i * i
      override def toString = "(i: Int) => i * i * i"
    }
    object IntToIntHalf extends (Int => Int) {
      def apply(i: Int): Int = i / 2
      override def toString = "(i: Int) => i / 2"
    }
    object IntToIntThird extends (Int => Int) {
      def apply(i: Int): Int = i / 3
      override def toString = "(i: Int) => i / 3"
    }
    object IntToIntFourth extends (Int => Int) {
      def apply(i: Int): Int = i / 3
      override def toString = "(i: Int) => i / 4"
    }
    object IntToIntNegate extends (Int => Int) {
      def apply(i: Int): Int = -i
      override def toString = "(i: Int) => -i"
    }
    object IntToIntComplement extends (Int => Int) {
      def apply(i: Int): Int = ~i
      override def toString = "(i: Int) => ~i"
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
      def next(szp: SizeParam, edges: List[Int => Int], rnd: Randomizer): (Int => Int, List[Int => Int], Randomizer) = {
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

  implicit def function1Generator[A, B](implicit genOfB: Generator[B], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B]): Generator[A => B] = {
    new Generator[A => B] {
      def next(szp: SizeParam, edges: List[A => B], rnd: Randomizer): (A => B, List[A => B], Randomizer) = {

        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object AToB extends (A => B) {
          def apply(a: A): B = org.scalatest.prop.valueOf[B](a, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            s"(o: $typeOfA) => org.scalatest.prop.valueOf[$typeOfB](o, $multiplier)"
          }
        }

        (AToB, Nil, rnd1)
      }
    }
  }

  implicit def function2Generator[A, B, C](implicit genOfC: Generator[C], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C]): Generator[(A, B) => C] = {
    new Generator[(A, B) => C] {
      def next(szp: SizeParam, edges: List[(A, B) => C], rnd: Randomizer): ((A, B) => C, List[(A, B) => C], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABToC extends ((A, B) => C) {
          def apply(a: A, b: B): C = org.scalatest.prop.valueOf[C](a, b, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            s"(a: $typeOfA, b: $typeOfB) => org.scalatest.prop.valueOf[$typeOfC](a, b, $multiplier)"
          }
        }

        (ABToC, Nil, rnd1)
      }
    }
  }

  implicit def function3Generator[A, B, C, D](implicit genOfD: Generator[D], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D]): Generator[(A, B, C) => D] = {
    new Generator[(A, B, C) => D] {
      def next(szp: SizeParam, edges: List[(A, B, C) => D], rnd: Randomizer): ((A, B, C) => D, List[(A, B, C) => D], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCToD extends ((A, B, C) => D) {
          def apply(a: A, b: B, c: C): D = org.scalatest.prop.valueOf[D](a, b, c, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC) => org.scalatest.prop.valueOf[$typeOfD](a, b, c, $multiplier)"
          }
        }

        (ABCToD, Nil, rnd1)
      }
    }
  }

  implicit def function4Generator[A, B, C, D, E](implicit genOfE: Generator[E], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E]): Generator[(A, B, C, D) => E] = {
    new Generator[(A, B, C, D) => E] {
      def next(szp: SizeParam, edges: List[(A, B, C, D) => E], rnd: Randomizer): ((A, B, C, D) => E, List[(A, B, C, D) => E], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDToE extends ((A, B, C, D) => E) {
          def apply(a: A, b: B, c: C, d: D): E = org.scalatest.prop.valueOf[E](a, b, c, d, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD) => org.scalatest.prop.valueOf[$typeOfE](a, b, c, d, $multiplier)"
          }
        }

        (ABCDToE, Nil, rnd1)
      }
    }
  }

  implicit def function5Generator[A, B, C, D, E, F](implicit genOfF: Generator[F], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F]): Generator[(A, B, C, D, E) => F] = {
    new Generator[(A, B, C, D, E) => F] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E) => F], rnd: Randomizer): ((A, B, C, D, E) => F, List[(A, B, C, D, E) => F], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEToF extends ((A, B, C, D, E) => F) {
          def apply(a: A, b: B, c: C, d: D, e: E): F = org.scalatest.prop.valueOf[F](a, b, c, d, e, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE) => org.scalatest.prop.valueOf[$typeOfF](a, b, c, d, e, $multiplier)"
          }
        }

        (ABCDEToF, Nil, rnd1)
      }
    }
  }

  implicit def function6Generator[A, B, C, D, E, F, G](implicit genOfG: Generator[G], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G]): Generator[(A, B, C, D, E, F) => G] = {
    new Generator[(A, B, C, D, E, F) => G] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F) => G], rnd: Randomizer): ((A, B, C, D, E, F) => G, List[(A, B, C, D, E, F) => G], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFToG extends ((A, B, C, D, E, F) => G) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F): G = org.scalatest.prop.valueOf[G](a, b, c, d, e, f, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF) => org.scalatest.prop.valueOf[$typeOfG](a, b, c, d, e, f, $multiplier)"
          }
        }

        (ABCDEFToG, Nil, rnd1)
      }
    }
  }

  implicit def function7Generator[A, B, C, D, E, F, G, H](implicit genOfH: Generator[H], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H]): Generator[(A, B, C, D, E, F, G) => H] = {
    new Generator[(A, B, C, D, E, F, G) => H] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G) => H], rnd: Randomizer): ((A, B, C, D, E, F, G) => H, List[(A, B, C, D, E, F, G) => H], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGToH extends ((A, B, C, D, E, F, G) => H) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G): H = org.scalatest.prop.valueOf[H](a, b, c, d, e, f, g, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG) => org.scalatest.prop.valueOf[$typeOfH](a, b, c, d, e, f, g, $multiplier)"
          }
        }

        (ABCDEFGToH, Nil, rnd1)
      }
    }
  }

  implicit def function8Generator[A, B, C, D, E, F, G, H, I](implicit genOfI: Generator[I], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I]): Generator[(A, B, C, D, E, F, G, H) => I] = {
    new Generator[(A, B, C, D, E, F, G, H) => I] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H) => I], rnd: Randomizer): ((A, B, C, D, E, F, G, H) => I, List[(A, B, C, D, E, F, G, H) => I], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHToI extends ((A, B, C, D, E, F, G, H) => I) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H): I = org.scalatest.prop.valueOf[I](a, b, c, d, e, f, g, h, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH) => org.scalatest.prop.valueOf[$typeOfI](a, b, c, d, e, f, g, h, $multiplier)"
          }
        }

        (ABCDEFGHToI, Nil, rnd1)
      }
    }
  }

  implicit def function9Generator[A, B, C, D, E, F, G, H, I, J](implicit genOfJ: Generator[J], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J]): Generator[(A, B, C, D, E, F, G, H, I) => J] = {
    new Generator[(A, B, C, D, E, F, G, H, I) => J] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I) => J], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I) => J, List[(A, B, C, D, E, F, G, H, I) => J], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIToJ extends ((A, B, C, D, E, F, G, H, I) => J) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I): J = org.scalatest.prop.valueOf[J](a, b, c, d, e, f, g, h, i, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI) => org.scalatest.prop.valueOf[$typeOfJ](a, b, c, d, e, f, g, h, i, $multiplier)"
          }
        }

        (ABCDEFGHIToJ, Nil, rnd1)
      }
    }
  }

  implicit def function10Generator[A, B, C, D, E, F, G, H, I, J, K](implicit genOfK: Generator[K], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K]): Generator[(A, B, C, D, E, F, G, H, I, J) => K] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J) => K] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J) => K], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J) => K, List[(A, B, C, D, E, F, G, H, I, J) => K], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJToK extends ((A, B, C, D, E, F, G, H, I, J) => K) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J): K = org.scalatest.prop.valueOf[K](a, b, c, d, e, f, g, h, i, j, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ) => org.scalatest.prop.valueOf[$typeOfK](a, b, c, d, e, f, g, h, i, j, $multiplier)"
          }
        }

        (ABCDEFGHIJToK, Nil, rnd1)
      }
    }
  }

  implicit def function11Generator[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfL: Generator[L], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K) => L], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K) => L, List[(A, B, C, D, E, F, G, H, I, J, K) => L], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKToL extends ((A, B, C, D, E, F, G, H, I, J, K) => L) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K): L = org.scalatest.prop.valueOf[L](a, b, c, d, e, f, g, h, i, j, k, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK) => org.scalatest.prop.valueOf[$typeOfL](a, b, c, d, e, f, g, h, i, j, k, $multiplier)"
          }
        }

        (ABCDEFGHIJKToL, Nil, rnd1)
      }
    }
  }

  implicit def function12Generator[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfM: Generator[M], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L) => M], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L) => M, List[(A, B, C, D, E, F, G, H, I, J, K, L) => M], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLToM extends ((A, B, C, D, E, F, G, H, I, J, K, L) => M) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L): M = org.scalatest.prop.valueOf[M](a, b, c, d, e, f, g, h, i, j, k, l, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL) => org.scalatest.prop.valueOf[$typeOfM](a, b, c, d, e, f, g, h, i, j, k, l, $multiplier)"
          }
        }

        (ABCDEFGHIJKLToM, Nil, rnd1)
      }
    }
  }

  implicit def function13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfN: Generator[N], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M) => N, List[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMToN extends ((A, B, C, D, E, F, G, H, I, J, K, L, M) => N) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M): N = org.scalatest.prop.valueOf[N](a, b, c, d, e, f, g, h, i, j, k, l, m, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM) => org.scalatest.prop.valueOf[$typeOfN](a, b, c, d, e, f, g, h, i, j, k, l, m, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMToN, Nil, rnd1)
      }
    }
  }

  implicit def function14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfO: Generator[O], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNToO extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N): O = org.scalatest.prop.valueOf[O](a, b, c, d, e, f, g, h, i, j, k, l, m, n, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN) => org.scalatest.prop.valueOf[$typeOfO](a, b, c, d, e, f, g, h, i, j, k, l, m, n, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNToO, Nil, rnd1)
      }
    }
  }

  implicit def function15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfP: Generator[P], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOToP extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O): P = org.scalatest.prop.valueOf[P](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO) => org.scalatest.prop.valueOf[$typeOfP](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOToP, Nil, rnd1)
      }
    }
  }

  implicit def function16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfQ: Generator[Q], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOPToQ extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P): Q = org.scalatest.prop.valueOf[Q](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP) => org.scalatest.prop.valueOf[$typeOfQ](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOPToQ, Nil, rnd1)
      }
    }
  }

  implicit def function17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfR: Generator[R], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOPQToR extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q): R = org.scalatest.prop.valueOf[R](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ) => org.scalatest.prop.valueOf[$typeOfR](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOPQToR, Nil, rnd1)
      }
    }
  }

  implicit def function18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfS: Generator[S], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOPQRToS extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R): S = org.scalatest.prop.valueOf[S](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR) => org.scalatest.prop.valueOf[$typeOfS](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOPQRToS, Nil, rnd1)
      }
    }
  }

  implicit def function19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfT: Generator[T], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOPQRSToT extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S): T = org.scalatest.prop.valueOf[T](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS) => org.scalatest.prop.valueOf[$typeOfT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOPQRSToT, Nil, rnd1)
      }
    }
  }

  implicit def function20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfU: Generator[U], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOPQRSTToU extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T): U = org.scalatest.prop.valueOf[U](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            val typeOfU = typeInfoU.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS, t: $typeOfT) => org.scalatest.prop.valueOf[$typeOfU](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOPQRSTToU, Nil, rnd1)
      }
    }
  }

  implicit def function21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfV: Generator[V], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOPQRSTUToV extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U): V = org.scalatest.prop.valueOf[V](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            val typeOfU = typeInfoU.name
            val typeOfV = typeInfoV.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS, t: $typeOfT, u: $typeOfU) => org.scalatest.prop.valueOf[$typeOfV](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOPQRSTUToV, Nil, rnd1)
      }
    }
  }

  implicit def function22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit genOfW: Generator[W], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V], typeInfoW: TypeInfo[W]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W], rnd: Randomizer): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W, List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime == 2) 1 else prime

        object ABCDEFGHIJKLMNOPQRSTUVToW extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V): W = org.scalatest.prop.valueOf[W](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            val typeOfU = typeInfoU.name
            val typeOfV = typeInfoV.name
            val typeOfW = typeInfoW.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS, t: $typeOfT, u: $typeOfU, v: $typeOfV) => org.scalatest.prop.valueOf[$typeOfW](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, $multiplier)"
          }
        }

        (ABCDEFGHIJKLMNOPQRSTUVToW, Nil, rnd1)
      }
    }
  }

  implicit def optionGenerator[T](implicit genOfT: Generator[T]): Generator[Option[T]] =
    new Generator[Option[T]] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Option[T]], Randomizer) = {
        // Subtract one from length, and we'll wrap those in Somes. Subtract one so that None can be the first edge.
        val (edgesOfT, nextRnd) = genOfT.initEdges(if (maxLength > 0) PosZInt.ensuringValid((maxLength - 1)) else 0, rnd)
        val edges = None :: edgesOfT.map(t => Some(t))
        (edges, nextRnd)
      }
      def next(szp: SizeParam, edges: List[Option[T]], rnd: Randomizer): (Option[T], List[Option[T]], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            if (nextInt % 10 == 0)
              (None, Nil, nextRnd)
            else {
              val (nextT, _, nextNextRnd) = genOfT.next(szp, Nil, nextRnd)
              (Some(nextT), Nil, nextNextRnd)
            }
        }
      }
    }

  implicit def orGenerator[G, B](implicit genOfG: Generator[G], genOfB: Generator[B]): Generator[G Or B] =
    new Generator[G Or B] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[G Or B], Randomizer) = {
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
      def next(szp: SizeParam, edges: List[G Or B], rnd: Randomizer): (G Or B, List[G Or B], Randomizer) = {
        edges match {
          case head :: tail => 
            (head, tail, rnd)
          case _ => 
            val (nextInt, nextRnd) = rnd.nextInt
            if (nextInt % 4 == 0) {
              val (nextB, _, nextRnd) = genOfB.next(szp, Nil, rnd)
              (Bad(nextB), Nil, nextRnd)
            }
            else {
              val (nextG, _, nextRnd) = genOfG.next(szp, Nil, rnd)
              (Good(nextG), Nil, nextRnd)
            }
        }
      }
    }
  implicit def tuple2Generator[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] =
    new GeneratorFor2[A, B, (A, B)]((a: A, b: B) => (a, b), (c: (A, B)) => c)(genOfA, genOfB)

  implicit def tuple3Generator[A, B, C](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[(A, B, C)] = {
    new GeneratorFor3[A, B, C, (A, B, C)]((a: A, b: B, c: C) => (a, b, c), (d: (A, B, C)) => d)(genOfA, genOfB, genOfC)
  }

  implicit def tuple4Generator[A, B, C, D](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[(A, B, C, D)] = {
    new GeneratorFor4[A, B, C, D, (A, B, C, D)]((a: A, b: B, c: C, d: D) => (a, b, c, d), (d: (A, B, C, D)) => d)(genOfA, genOfB, genOfC, genOfD)
  }

  implicit def tuple5Generator[A, B, C, D, E](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[(A, B, C, D, E)] =
    new GeneratorFor5[A, B, C, D, E, (A, B, C, D, E)]((a: A, b: B, c: C, d: D, e: E) => (a, b, c, d, e), (f: (A, B, C, D, E)) => f)(genOfA, genOfB, genOfC, genOfD, genOfE)

  implicit def tuple6Generator[A, B, C, D, E, F](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[(A, B, C, D, E, F)] =
    new GeneratorFor6[A, B, C, D, E, F, (A, B, C, D, E, F)]((a: A, b: B, c: C, d: D, e: E, f: F) => (a, b, c, d, e, f), (g: (A, B, C, D, E, F)) => g)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF)

  implicit def tuple7Generator[A, B, C, D, E, F, G](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G]): Generator[(A, B, C, D, E, F, G)] =
    new GeneratorFor7[A, B, C, D, E, F, G, (A, B, C, D, E, F, G)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G) => (a, b, c, d, e, f, g), (h: (A, B, C, D, E, F, G)) => h)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG)

  implicit def tuple8Generator[A, B, C, D, E, F, G, H](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H]): Generator[(A, B, C, D, E, F, G, H)] =
    new GeneratorFor8[A, B, C, D, E, F, G, H, (A, B, C, D, E, F, G, H)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => (a, b, c, d, e, f, g, h), (i: (A, B, C, D, E, F, G, H)) => i)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH)

  implicit def tuple9Generator[A, B, C, D, E, F, G, H, I](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[(A, B, C, D, E, F, G, H, I)] =
    new GeneratorFor9[A, B, C, D, E, F, G, H, I, (A, B, C, D, E, F, G, H, I)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) => (a, b, c, d, e, f, g, h, i), (j: (A, B, C, D, E, F, G, H, I)) => j)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI)

  implicit def tuple10Generator[A, B, C, D, E, F, G, H, I, J](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J]): Generator[(A, B, C, D, E, F, G, H, I, J)] =
    new GeneratorFor10[A, B, C, D, E, F, G, H, I, J, (A, B, C, D, E, F, G, H, I, J)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) => (a, b, c, d, e, f, g, h, i, j), (k: (A, B, C, D, E, F, G, H, I, J)) => k)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ)

  implicit def tuple11Generator[A, B, C, D, E, F, G, H, I, J, K](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K]): Generator[(A, B, C, D, E, F, G, H, I, J, K)] =
    new GeneratorFor11[A, B, C, D, E, F, G, H, I, J, K, (A, B, C, D, E, F, G, H, I, J, K)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) => (a, b, c, d, e, f, g, h, i, j, k), (l: (A, B, C, D, E, F, G, H, I, J, K)) => l)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK)

  implicit def tuple12Generator[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new GeneratorFor12[A, B, C, D, E, F, G, H, I, J, K, L, (A, B, C, D, E, F, G, H, I, J, K, L)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L) => (a, b, c, d, e, f, g, h, i, j, k, l), (m: (A, B, C, D, E, F, G, H, I, J, K, L)) => m)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL)

  implicit def tuple13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new GeneratorFor13[A, B, C, D, E, F, G, H, I, J, K, L, M, (A, B, C, D, E, F, G, H, I, J, K, L, M)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M) => (a, b, c, d, e, f, g, h, i, j, k, l, m), (n: (A, B, C, D, E, F, G, H, I, J, K, L, M)) => n)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM)

  implicit def tuple14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new GeneratorFor14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, (A, B, C, D, E, F, G, H, I, J, K, L, M, N)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n), (o: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)) => o)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN)

  implicit def tuple15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new GeneratorFor15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), (p: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) => p)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO)

  implicit def tuple16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new GeneratorFor16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), (q: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) => q)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP)

  implicit def tuple17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new GeneratorFor17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q), (r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) => r)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ)

  implicit def tuple18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new GeneratorFor18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r), (s: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) => s)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR)

  implicit def tuple19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new GeneratorFor19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s), (t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) => t)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS)

  implicit def tuple20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new GeneratorFor20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t), (u: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) => u)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT)

  implicit def tuple21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T], genOfU: Generator[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new GeneratorFor21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u), (v: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) => v)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU)

  implicit def tuple22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T], genOfU: Generator[U], genOfV: Generator[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new GeneratorFor22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v), (w: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) => w)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU, genOfV)

}


