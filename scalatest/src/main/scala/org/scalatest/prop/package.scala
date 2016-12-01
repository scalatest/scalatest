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
package org.scalatest

import org.scalactic.anyvals._
import scala.annotation.tailrec

package object prop {

  def intsBetween(from: Int, to: Int): Generator[Int] =
    new Generator[Int] { thisIntGenerator =>
      private val fromToEdges = List(from, to).distinct // distinct in case from equals to
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Int], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[Int], rnd: Randomizer): (Int, List[Int], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(from, to)
            (nextInt, Nil, nextRandomizer)
        }
      }
    }

  def posIntsBetween(from: PosInt, to: PosInt): Generator[PosInt] =
    new Generator[PosInt] { thisIntGenerator =>
      private val fromToEdges = List(from, to).distinct // distinct in case from equals to
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosInt], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosInt], rnd: Randomizer): (PosInt, List[PosInt], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosInt, nextRandomizer) = rnd.choosePosInt(from, to)
            (nextPosInt, Nil, nextRandomizer)
        }
      }
    }

  def values[T](first: T, second: T, rest: T*): Generator[T] =
    new Generator[T] {
      private val seq: Seq[T] = first +: second +: rest
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

  val bytes: Generator[Byte] = Generator.byteGenerator
  val shorts: Generator[Short] = Generator.shortGenerator
  val ints: Generator[Int] = Generator.intGenerator
  val longs: Generator[Long] = Generator.longGenerator
  val chars: Generator[Char] = Generator.charGenerator
  val floats: Generator[Float] = Generator.floatGenerator
  val doubles: Generator[Double] = Generator.doubleGenerator
  val strings: Generator[String] = Generator.stringGenerator
  def lists[T](implicit genOfT: Generator[T]): Generator[List[T]] = Generator.listGenerator[T]
  def tuple2s[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] = Generator.tuple2Generator[A, B]
  def function0s[A](implicit genOfA: Generator[A]): Generator[() => A] = Generator.function0Generator[A]

  val posInts: Generator[PosInt] = Generator.posIntGenerator
  val posZInts: Generator[PosZInt] = Generator.posZIntGenerator
  val posLongs: Generator[PosLong] = Generator.posLongGenerator
  val posZLongs: Generator[PosZLong] = Generator.posZLongGenerator
  val posFloats: Generator[PosFloat] = Generator.posFloatGenerator
  val posZFloats: Generator[PosZFloat] = Generator.posZFloatGenerator
  val posDoubles: Generator[PosDouble] = Generator.posDoubleGenerator
  val posZDoubles: Generator[PosZDouble] = Generator.posZDoubleGenerator

  val posIntValues: Generator[Int] = Generator.posIntGenerator.map(_.value)
  val posZIntValues: Generator[Int] = Generator.posZIntGenerator.map(_.value)
  val posLongValues: Generator[Long] = Generator.posLongGenerator.map(_.value)
  val posZLongValues: Generator[Long] = Generator.posZLongGenerator.map(_.value)
  val posFloatValues: Generator[Float] = Generator.posFloatGenerator.map(_.value)
  val posZFloatValues: Generator[Float] = Generator.posZFloatGenerator.map(_.value)
  val posDoubleValues: Generator[Double] = Generator.posDoubleGenerator.map(_.value)
  val posZDoubleValues: Generator[Double] = Generator.posZDoubleGenerator.map(_.value)

  def gen[A, B, C](make: (A, B) => C)(unmake: C => (A, B))(implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[C] =
    new Generator2[A, B, C](make, unmake)(genOfA, genOfB)

  // classify will need to use the same sizing algo as forAll, and same edges approach
  def classify[A](count: PosInt, genOfA: Generator[A])(pf: PartialFunction[A, String]): Classification = {

    val (initEdges, rnd1) = genOfA.initEdges(100, Randomizer.default())
    @tailrec
    def loop(currentCount: Int, edges: List[A], rnd: Randomizer, acc: Map[String, Int]): Map[String, Int] = {
      if (currentCount >= count) acc
      else {
        val (nextA, nextEdges, nextRnd) = genOfA.next(100, edges, rnd)
        if (pf.isDefinedAt(nextA)) {
          val category = pf(nextA)
          val prevTotal = acc.getOrElse(category, 0)
          val nextAcc = acc + (category -> (prevTotal + 1))
          loop(currentCount + 1, nextEdges, nextRnd, nextAcc)
        }
        else {
          loop(currentCount + 1, nextEdges, nextRnd, acc)
        }
      }
    }
    val theMap = loop(0, initEdges, rnd1, Map.empty)
    Classification(count, theMap)
  }
  /*
    maybe shrink can be growTo(target: T
    grower(value: T, ...)
    grow(value: T, ...)
    or simplify(value: T, ...)
  */
}


