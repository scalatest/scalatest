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

import org.scalactic.anyvals._
import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag
import org.scalactic.Requirements._

trait CommonGenerators {

  def bytesBetween(from: Byte, to: Byte): Generator[Byte] = {
    require(from <= to)
    new Generator[Byte] { thisByteGenerator =>
      private val byteEdges = Generator.byteEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: byteEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Byte], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Byte], rnd: Randomizer): (Byte, List[Byte], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextByte, nextRandomizer) = rnd.chooseByte(from, to)
            (nextByte, Nil, nextRandomizer)
        }
      }
    }
  }

  def shortsBetween(from: Short, to: Short): Generator[Short] = {
    require(from <= to)
    new Generator[Short] { thisShortGenerator =>
      private val shortEdges = List(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue).filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: shortEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Short], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Short], rnd: Randomizer): (Short, List[Short], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextShort, nextRandomizer) = rnd.chooseShort(from, to)
            (nextShort, Nil, nextRandomizer)
        }
      }
    }
  }

  def intsBetween(from: Int, to: Int): Generator[Int] = {
    require(from <= to)
    new Generator[Int] { thisIntGenerator =>
      private val intEdges = Generator.intEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: intEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Int], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Int], rnd: Randomizer): (Int, List[Int], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(from, to)
            (nextInt, Nil, nextRandomizer)
        }
      }
    }
  }

  def longsBetween(from: Long, to: Long): Generator[Long] = {
    require(from <= to)
    new Generator[Long] { thisLongGenerator =>
      private val longEdges = Generator.longEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: longEdges).distinct // distinct in case from equals to, and/or overlaps an edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Long], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Long], rnd: Randomizer): (Long, List[Long], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextLong, nextRandomizer) = rnd.chooseLong(from, to)
            (nextLong, Nil, nextRandomizer)
        }
      }
    }
  }

  def charsBetween(from: Char, to: Char): Generator[Char] = {
    require(from <= to)
    new Generator[Char] { thisCharGenerator =>
      private val charEdges = Generator.charEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: charEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Char], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Char], rnd: Randomizer): (Char, List[Char], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextChar, nextRandomizer) = rnd.chooseChar(from, to)
            (nextChar, Nil, nextRandomizer)
        }
      }
    }
  }

  def floatsBetween(from: Float, to: Float): Generator[Float] = {
    require(from <= to)
    new Generator[Float] { thisFloatGenerator =>
      private val floatEdges = Generator.floatEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: floatEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Float], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Float], rnd: Randomizer): (Float, List[Float], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextFloat, nextRandomizer) = rnd.chooseFloat(from, to)
            (nextFloat, Nil, nextRandomizer)
        }
      }
    }
  }

  def doublesBetween(from: Double, to: Double): Generator[Double] = {
    require(from <= to)
    new Generator[Double] { thisDoubleGenerator =>
      private val doubleEdges = Generator.doubleEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: doubleEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Double], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Double], rnd: Randomizer): (Double, List[Double], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextDouble, nextRandomizer) = rnd.chooseDouble(from, to)
            (nextDouble, Nil, nextRandomizer)
        }
      }
    }
  }

  def posIntsBetween(from: PosInt, to: PosInt): Generator[PosInt] =
    new Generator[PosInt] { thisPosIntGenerator =>
      private val intEdges = Generator.posIntEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: intEdges).distinct // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosInt], rnd: Randomizer): (PosInt, List[PosInt], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosInt, nextRandomizer) = rnd.choosePosInt(from, to)
            (nextPosInt, Nil, nextRandomizer)
        }
      }
    }

  def posLongsBetween(from: PosLong, to: PosLong): Generator[PosLong] = {
    require(from <= to)
    new Generator[PosLong] { thisPosLongGenerator =>
      private val posLongEdges = List(PosLong(1L), PosLong.MaxValue).filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: posLongEdges).distinct // distinct in case from equals to, and/or overlaps an edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosLong], rnd: Randomizer): (PosLong, List[PosLong], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosLong, nextRandomizer) = rnd.choosePosLong(from, to)
            (nextPosLong, Nil, nextRandomizer)
        }
      }
    }
  }

  def posFloatsBetween(from: PosFloat, to: PosFloat): Generator[PosFloat] = {
    require(from <= to)
    new Generator[PosFloat] { thisPosFloatGenerator =>
      private val posFloatEdges = List(PosFloat(1.0f), PosFloat.MaxValue).filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: posFloatEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosFloat], rnd: Randomizer): (PosFloat, List[PosFloat], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosFloat, nextRandomizer) = rnd.choosePosFloat(from, to)
            (nextPosFloat, Nil, nextRandomizer)
        }
      }
    }
  }

  def posDoublesBetween(from: PosDouble, to: PosDouble): Generator[PosDouble] = {
    require(from <= to)
    new Generator[PosDouble] { thisPosDoubleGenerator =>
      private val posDoubleEdges = Generator.posDoubleEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: posDoubleEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosDouble], rnd: Randomizer): (PosDouble, List[PosDouble], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosDouble, nextRandomizer) = rnd.choosePosDouble(from, to)
            (nextPosDouble, Nil, nextRandomizer)
        }
      }
    }
  }

  def posZIntsBetween(from: PosZInt, to: PosZInt): Generator[PosZInt] =
    // Probably disallow from >= to, and if =, then say use some alternative? constantValues(x) ?
    new Generator[PosZInt] { thisPosZIntGenerator =>
      private val intEdges = Generator.posZIntEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: intEdges).distinct // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosZInt], rnd: Randomizer): (PosZInt, List[PosZInt], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosZInt, nextRandomizer) = rnd.choosePosZInt(from, to)
            (nextPosZInt, Nil, nextRandomizer)
        }
      }
    }

  def posZLongsBetween(from: PosZLong, to: PosZLong): Generator[PosZLong] = {
    require(from <= to)
    new Generator[PosZLong] { thisPosZLongGenerator =>
      private val posZLongEdges = Generator.posZLongEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: posZLongEdges).distinct // distinct in case from equals to, and/or overlaps an edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosZLong], rnd: Randomizer): (PosZLong, List[PosZLong], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosZLong, nextRandomizer) = rnd.choosePosZLong(from, to)
            (nextPosZLong, Nil, nextRandomizer)
        }
      }
    }
  }

  def posZFloatsBetween(from: PosZFloat, to: PosZFloat): Generator[PosZFloat] = {
    require(from <= to)
    new Generator[PosZFloat] { thisPosZFloatGenerator =>
      private val posZFloatEdges = Generator.posZFloatEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: posZFloatEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosZFloat], rnd: Randomizer): (PosZFloat, List[PosZFloat], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosZFloat, nextRandomizer) = rnd.choosePosZFloat(from, to)
            (nextPosZFloat, Nil, nextRandomizer)
        }
      }
    }
  }

  def posZDoublesBetween(from: PosZDouble, to: PosZDouble): Generator[PosZDouble] = {
    require(from <= to)
    new Generator[PosZDouble] { thisPosZDoubleGenerator =>
      private val posZDoubleEdges = Generator.posZDoubleEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: posZDoubleEdges).distinct // distinct in case from equals to, and/or overlaps an Int edge
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[PosZDouble], rnd: Randomizer): (PosZDouble, List[PosZDouble], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosZDouble, nextRandomizer) = rnd.choosePosZDouble(from, to)
            (nextPosZDouble, Nil, nextRandomizer)
        }
      }
    }
  }

  def negIntsBetween(from: NegInt, to: NegInt): Generator[NegInt] = {
    require(from <= to)
    new Generator[NegInt] {
      thisNegIntGenerator =>
      private val intEdges = Generator.negIntEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: intEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegInt], rnd: Randomizer): (NegInt, List[NegInt], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegInt, nextRandomizer) = rnd.chooseNegInt(from, to)
            (nextNegInt, Nil, nextRandomizer)
        }
      }
    }
  }

  def negLongsBetween(from: NegLong, to: NegLong): Generator[NegLong] = {
    require(from <= to)
    new Generator[NegLong] {
      thisNegLongGenerator =>
      private val longEdges = Generator.negLongEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: longEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegLong], rnd: Randomizer): (NegLong, List[NegLong], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegLong, nextRandomizer) = rnd.chooseNegLong(from, to)
            (nextNegLong, Nil, nextRandomizer)
        }
      }
    }
  }

  def negFloatsBetween(from: NegFloat, to: NegFloat): Generator[NegFloat] = {
    require(from <= to)
    new Generator[NegFloat] {
      thisNegFloatGenerator =>
      private val floatEdges = Generator.negFloatEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: floatEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegFloat], rnd: Randomizer): (NegFloat, List[NegFloat], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegFloat, nextRandomizer) = rnd.chooseNegFloat(from, to)
            (nextNegFloat, Nil, nextRandomizer)
        }
      }
    }
  }

  def negDoublesBetween(from: NegDouble, to: NegDouble): Generator[NegDouble] = {
    require(from <= to)
    new Generator[NegDouble] {
      thisNegDoubleGenerator =>
      private val doubleEdges = Generator.negDoubleEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: doubleEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegDouble], rnd: Randomizer): (NegDouble, List[NegDouble], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegDouble, nextRandomizer) = rnd.chooseNegDouble(from, to)
            (nextNegDouble, Nil, nextRandomizer)
        }
      }
    }
  }

  def negZIntsBetween(from: NegZInt, to: NegZInt): Generator[NegZInt] = {
    require(from <= to)
    new Generator[NegZInt] {
      thisNegZIntGenerator =>
      private val intEdges = Generator.negZIntEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: intEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegZInt], rnd: Randomizer): (NegZInt, List[NegZInt], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegZInt, nextRandomizer) = rnd.chooseNegZInt(from, to)
            (nextNegZInt, Nil, nextRandomizer)
        }
      }
    }
  }

  def negZLongsBetween(from: NegZLong, to: NegZLong): Generator[NegZLong] = {
    require(from <= to)
    new Generator[NegZLong] {
      thisNegZLongGenerator =>
      private val longEdges = Generator.negZLongEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: longEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegZLong], rnd: Randomizer): (NegZLong, List[NegZLong], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegZLong, nextRandomizer) = rnd.chooseNegZLong(from, to)
            (nextNegZLong, Nil, nextRandomizer)
        }
      }
    }
  }

  def negZFloatsBetween(from: NegZFloat, to: NegZFloat): Generator[NegZFloat] = {
    require(from <= to)
    new Generator[NegZFloat] {
      thisNegZFloatGenerator =>
      private val floatEdges = Generator.negZFloatEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: floatEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegZFloat], rnd: Randomizer): (NegZFloat, List[NegZFloat], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegZFloat, nextRandomizer) = rnd.chooseNegZFloat(from, to)
            (nextNegZFloat, Nil, nextRandomizer)
        }
      }
    }
  }

  def negZDoublesBetween(from: NegZDouble, to: NegZDouble): Generator[NegZDouble] = {
    require(from <= to)
    new Generator[NegZDouble] {
      thisNegZDoubleGenerator =>
      private val doubleEdges = Generator.negZDoubleEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: doubleEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NegZDouble], rnd: Randomizer): (NegZDouble, List[NegZDouble], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNegZDouble, nextRandomizer) = rnd.chooseNegZDouble(from, to)
            (nextNegZDouble, Nil, nextRandomizer)
        }
      }
    }
  }

  def nonZeroIntsBetween(from: NonZeroInt, to: NonZeroInt): Generator[NonZeroInt] = {
    require(from <= to)
    new Generator[NonZeroInt] {
      thisNonZeroIntGenerator =>
      private val intEdges = Generator.nonZeroIntEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: intEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NonZeroInt], rnd: Randomizer): (NonZeroInt, List[NonZeroInt], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNonZeroInt, nextRandomizer) = rnd.chooseNonZeroInt(from, to)
            (nextNonZeroInt, Nil, nextRandomizer)
        }
      }
    }
  }

  def nonZeroLongsBetween(from: NonZeroLong, to: NonZeroLong): Generator[NonZeroLong] = {
    require(from <= to)
    new Generator[NonZeroLong] {
      thisNonZeroLongGenerator =>
      private val longEdges = Generator.nonZeroLongEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: longEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NonZeroLong], rnd: Randomizer): (NonZeroLong, List[NonZeroLong], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNonZeroLong, nextRandomizer) = rnd.chooseNonZeroLong(from, to)
            (nextNonZeroLong, Nil, nextRandomizer)
        }
      }
    }
  }

  def nonZeroFloatsBetween(from: NonZeroFloat, to: NonZeroFloat): Generator[NonZeroFloat] = {
    require(from <= to)
    new Generator[NonZeroFloat] {
      thisNonZeroFloatGenerator =>
      private val floatEdges = Generator.nonZeroFloatEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: floatEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NonZeroFloat], rnd: Randomizer): (NonZeroFloat, List[NonZeroFloat], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNonZeroFloat, nextRandomizer) = rnd.chooseNonZeroFloat(from, to)
            (nextNonZeroFloat, Nil, nextRandomizer)
        }
      }
    }
  }

  def nonZeroDoublesBetween(from: NonZeroDouble, to: NonZeroDouble): Generator[NonZeroDouble] = {
    require(from <= to)
    new Generator[NonZeroDouble] {
      thisNonZeroDoubleGenerator =>
      private val doubleEdges = Generator.nonZeroDoubleEdges.filter(i => i >= from && i <= to)
      private val fromToEdges = (from :: to :: doubleEdges).distinct

      // distinct in case from equals to
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(size: PosZInt, maxSize: PosZInt, edges: List[NonZeroDouble], rnd: Randomizer): (NonZeroDouble, List[NonZeroDouble], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextNonZeroDouble, nextRandomizer) = rnd.chooseNonZeroDouble(from, to)
            (nextNonZeroDouble, Nil, nextRandomizer)
        }
      }
    }
  }

  def specificValues[T](first: T, second: T, rest: T*): Generator[T] =
    new Generator[T] {
      private val seq: Seq[T] = first +: second +: rest
      def next(size: PosZInt, maxSize: PosZInt, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
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

  def specificValue[T](theValue: T): Generator[T] =
    new Generator[T] {
      def next(size: PosZInt, maxSize: PosZInt, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            (theValue, Nil, rnd)
        }
      }
    }

  // TODO: I wonder if I could get rid of the edges pattern match
  // by moving this to a different method. Then next is just next
  // in the distributed stuff. I could then do the pattern match
  // once and forall in a final method, nextEdge.
  def frequency[T](first: (Int, Generator[T]), second: (Int, Generator[T]), rest: (Int, Generator[T])*): Generator[T] = {
    val distribution: Vector[(Int, Generator[T])] = (first +: second +: rest).toVector
    // Take Int not PosInt, because Scala won't apply  multiple implicit
    // conversions, such as one for PosInt => Int, and another for Int => Generator[Int].
    // So just do a require.
/*
    TODO:

     org.scalactic.Requirements.require {
       distribution forall { case (w, _) => w >= 1 }
     }

[error] /Users/bv/nobkp/delus/st-algebra-and-laws-2/scalatest/src/main/scala/org/scalatest/prop/package.scala:154: exception during macro expansion: 
[error] scala.reflect.macros.TypecheckException: not found: value requirementsHelper
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$typecheck$2$$anonfun$apply$1.apply(Typers.scala:34)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$typecheck$2$$anonfun$apply$1.apply(Typers.scala:28)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$3.apply(Typers.scala:24)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$3.apply(Typers.scala:24)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$withContext$1$1.apply(Typers.scala:25)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$withContext$1$1.apply(Typers.scala:25)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$1.apply(Typers.scala:23)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$1.apply(Typers.scala:23)
[error] 	at scala.reflect.macros.contexts.Typers$class.withContext$1(Typers.scala:25)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$typecheck$2.apply(Typers.scala:28)

*/
    // I think we actually need to say org.scalactic.Requirements.requirementsHelper in the thing not requirementsHelper
    // Oh, maybe that won't work. Anyway, see what's up.
    import org.scalactic.Requirements._
    require {
      distribution forall { case (w, _) => w >= 1 }
    }
    new Generator[T] {
      private val totalWeight: Int = distribution.toMap.keys.sum
      // gens contains, for each distribution pair, weight generators.
      private val gens: Vector[Generator[T]] =
        // TODO: Try dropping toVector. distribution is already a Vector
        distribution.toVector flatMap { case (w, g) =>
          Vector.fill(w)(g)
        }
      def next(size: PosZInt, maxSize: PosZInt, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(0, gens.length - 1)
            val nextGen = gens(nextInt)
            nextGen.next(size, maxSize, Nil, nextRandomizer)
        }
      }
    }
  }

  def evenly[T](first: Generator[T], second: Generator[T], rest: Generator[T]*): Generator[T] = {
    val distributees: Vector[Generator[T]] = (first +: second +: rest).toVector
    new Generator[T] {
      // gens contains, for each distribution pair, weight generators.
      def next(size: PosZInt, maxSize: PosZInt, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(0, distributees.length - 1)
            val nextGen = distributees(nextInt)
            nextGen.next(size, maxSize, Nil, nextRandomizer) // TODO: Is it correct to pass size and maxSize here?
        }
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
  def lists[T](implicit genOfT: Generator[T]): Generator[List[T]] with HavingLength[List[T]] = Generator.listGenerator[T]
  def tuple2s[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] = Generator.tuple2Generator[A, B]
  def tuple3s[A, B, C](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[(A, B, C)] = Generator.tuple3Generator[A, B, C]
  def tuple4s[A, B, C, D](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[(A, B, C, D)] = Generator.tuple4Generator[A, B, C, D]
  def tuple5s[A, B, C, D, E](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[(A, B, C, D, E)] = Generator.tuple5Generator[A, B, C, D, E]
  def tuple6s[A, B, C, D, E, F](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[(A, B, C, D, E, F)] = Generator.tuple6Generator[A, B, C, D, E, F]
  def tuple7s[A, B, C, D, E, F, G](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G]): Generator[(A, B, C, D, E, F, G)] = Generator.tuple7Generator[A, B, C, D, E, F, G]
  def tuple8s[A, B, C, D, E, F, G, H](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H]): Generator[(A, B, C, D, E, F, G, H)] = Generator.tuple8Generator[A, B, C, D, E, F, G, H]
  def tuple9s[A, B, C, D, E, F, G, H, I](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[(A, B, C, D, E, F, G, H, I)] = Generator.tuple9Generator[A, B, C, D, E, F, G, H, I]
  def tuple10s[A, B, C, D, E, F, G, H, I, J](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                             genOfJ: Generator[J]): Generator[(A, B, C, D, E, F, G, H, I, J)] = Generator.tuple10Generator[A, B, C, D, E, F, G, H, I, J]
  def tuple11s[A, B, C, D, E, F, G, H, I, J, K](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                             genOfJ: Generator[J], genOfK: Generator[K]): Generator[(A, B, C, D, E, F, G, H, I, J, K)] = Generator.tuple11Generator[A, B, C, D, E, F, G, H, I, J, K]
  def tuple12s[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L)] = Generator.tuple12Generator[A, B, C, D, E, F, G, H, I, J, K, L]
  def tuple13s[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                   genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = Generator.tuple13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M]
  def tuple14s[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                      genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = Generator.tuple14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
  def tuple15s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                         genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = Generator.tuple15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
  def tuple16s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                            genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = Generator.tuple16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
  def tuple17s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                               genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = Generator.tuple17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
  def tuple18s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                  genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = Generator.tuple18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
  def tuple19s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                     genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = Generator.tuple19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  def tuple20s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                           genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                           genOfT: Generator[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = Generator.tuple20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  def tuple21s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                           genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                           genOfT: Generator[T], genOfU: Generator[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = Generator.tuple21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  def tuple22s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                              genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                              genOfT: Generator[T], genOfU: Generator[U], genOfV: Generator[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = Generator.tuple22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
  def function0s[A](implicit genOfA: Generator[A]): Generator[() => A] = Generator.function0Generator[A]
  def function1s[A, B](implicit genOfB: Generator[B], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B]): Generator[A => B] =
    Generator.function1Generator[A, B]
  def function2s[A, B, C](implicit genOfC: Generator[C], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C]): Generator[(A, B) => C] =
    Generator.function2Generator[A, B, C]
  def function3s[A, B, C, D](implicit genOfD: Generator[D], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D]): Generator[(A, B, C) => D] =
    Generator.function3Generator[A, B, C, D]
  def function4s[A, B, C, D, E](implicit genOfE: Generator[E], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E]): Generator[(A, B, C, D) => E] =
    Generator.function4Generator[A, B, C, D, E]
  def function5s[A, B, C, D, E, F](implicit genOfF: Generator[F], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F]): Generator[(A, B, C, D, E) => F] =
    Generator.function5Generator[A, B, C, D, E, F]
  def function6s[A, B, C, D, E, F, G](implicit genOfG: Generator[G], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G]): Generator[(A, B, C, D, E, F) => G] =
    Generator.function6Generator[A, B, C, D, E, F, G]
  def function7s[A, B, C, D, E, F, G, H](implicit genOfH: Generator[H], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H]): Generator[(A, B, C, D, E, F, G) => H] =
    Generator.function7Generator[A, B, C, D, E, F, G, H]
  def function8s[A, B, C, D, E, F, G, H, I](implicit genOfI: Generator[I], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I]): Generator[(A, B, C, D, E, F, G, H) => I] =
    Generator.function8Generator[A, B, C, D, E, F, G, H, I]
  def function9s[A, B, C, D, E, F, G, H, I, J](implicit genOfJ: Generator[J], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J]): Generator[(A, B, C, D, E, F, G, H, I) => J] =
    Generator.function9Generator[A, B, C, D, E, F, G, H, I, J]
  def function10s[A, B, C, D, E, F, G, H, I, J, K](implicit genOfK: Generator[K], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K]): Generator[(A, B, C, D, E, F, G, H, I, J) => K] =
    Generator.function10Generator[A, B, C, D, E, F, G, H, I, J, K]
  def function11s[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfL: Generator[L], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] =
    Generator.function11Generator[A, B, C, D, E, F, G, H, I, J, K, L]
  def function12s[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfM: Generator[M], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] =
    Generator.function12Generator[A, B, C, D, E, F, G, H, I, J, K, L, M]
  def function13s[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfN: Generator[N], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] =
    Generator.function13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
  def function14s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfO: Generator[O], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] =
    Generator.function14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
  def function15s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfP: Generator[P], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] =
    Generator.function15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
  def function16s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfQ: Generator[Q], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] =
    Generator.function16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
  def function17s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfR: Generator[R], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] =
    Generator.function17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
  def function18s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfS: Generator[S], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] =
    Generator.function18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  def function19s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfT: Generator[T], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] =
    Generator.function19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  def function20s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfU: Generator[U], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T], typeTagOfU: TypeTag[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] =
    Generator.function20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  def function21s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfV: Generator[V], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T], typeTagOfU: TypeTag[U], typeTagOfV: TypeTag[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] =
    Generator.function21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
  def function22s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit genOfW: Generator[W], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T], typeTagOfU: TypeTag[U], typeTagOfV: TypeTag[V], typeTagOfW: TypeTag[W]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] =
    Generator.function22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]

  val posInts: Generator[PosInt] = Generator.posIntGenerator
  val posZInts: Generator[PosZInt] = Generator.posZIntGenerator
  val posLongs: Generator[PosLong] = Generator.posLongGenerator
  val posZLongs: Generator[PosZLong] = Generator.posZLongGenerator
  val posFloats: Generator[PosFloat] = Generator.posFloatGenerator
  val posZFloats: Generator[PosZFloat] = Generator.posZFloatGenerator
  val posDoubles: Generator[PosDouble] = Generator.posDoubleGenerator
  val posZDoubles: Generator[PosZDouble] = Generator.posZDoubleGenerator
  val negInts: Generator[NegInt] = Generator.negIntGenerator
  val negZInts: Generator[NegZInt] = Generator.negZIntGenerator
  val negLongs: Generator[NegLong] = Generator.negLongGenerator
  val negZLongs: Generator[NegZLong] = Generator.negZLongGenerator
  val negFloats: Generator[NegFloat] = Generator.negFloatGenerator
  val negZFloats: Generator[NegZFloat] = Generator.negZFloatGenerator
  val negDoubles: Generator[NegDouble] = Generator.negDoubleGenerator
  val negZDoubles: Generator[NegZDouble] = Generator.negZDoubleGenerator
  val nonZeroInts: Generator[NonZeroInt] = Generator.nonZeroIntGenerator
  val nonZeroLongs: Generator[NonZeroLong] = Generator.nonZeroLongGenerator
  val nonZeroFloats: Generator[NonZeroFloat] = Generator.nonZeroFloatGenerator
  val nonZeroDoubles: Generator[NonZeroDouble] = Generator.nonZeroDoubleGenerator

  val posIntValues: Generator[Int] = Generator.posIntGenerator.map(_.value)
  val posZIntValues: Generator[Int] = Generator.posZIntGenerator.map(_.value)
  val posLongValues: Generator[Long] = Generator.posLongGenerator.map(_.value)
  val posZLongValues: Generator[Long] = Generator.posZLongGenerator.map(_.value)
  val posFloatValues: Generator[Float] = Generator.posFloatGenerator.map(_.value)
  val posZFloatValues: Generator[Float] = Generator.posZFloatGenerator.map(_.value)
  val posDoubleValues: Generator[Double] = Generator.posDoubleGenerator.map(_.value)
  val posZDoubleValues: Generator[Double] = Generator.posZDoubleGenerator.map(_.value)
  val negIntValues: Generator[Int] = Generator.negIntGenerator.map(_.value)
  val negZIntValues: Generator[Int] = Generator.negZIntGenerator.map(_.value)
  val negLongValues: Generator[Long] = Generator.negLongGenerator.map(_.value)
  val negZLongValues: Generator[Long] = Generator.negZLongGenerator.map(_.value)
  val negFloatValues: Generator[Float] = Generator.negFloatGenerator.map(_.value)
  val negZFloatValues: Generator[Float] = Generator.negZFloatGenerator.map(_.value)
  val negDoubleValues: Generator[Double] = Generator.negDoubleGenerator.map(_.value)
  val negZDoubleValues: Generator[Double] = Generator.negZDoubleGenerator.map(_.value)
  val nonZeroIntValues: Generator[Int] = Generator.nonZeroIntGenerator.map(_.value)
  val nonZeroLongValues: Generator[Long] = Generator.nonZeroLongGenerator.map(_.value)
  val nonZeroFloatValues: Generator[Float] = Generator.nonZeroFloatGenerator.map(_.value)
  val nonZeroDoubleValues: Generator[Double] = Generator.nonZeroDoubleGenerator.map(_.value)

  def instancesOf[A, B](construct: A => B)(deconstruct: B => A)(implicit genOfA: Generator[A]): Generator[B] =
    new GeneratorFor1[A, B](construct, deconstruct)(genOfA)

  def instancesOf[A, B, C](construct: (A, B) => C)(deconstruct: C => (A, B))(implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[C] =
    new GeneratorFor2[A, B, C](construct, deconstruct)(genOfA, genOfB)

  def instancesOf[A, B, C, D](construct: (A, B, C) => D)(deconstruct: D => (A, B, C))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[D] =
    new GeneratorFor3[A, B, C, D](construct, deconstruct)(genOfA, genOfB, genOfC)

  def instancesOf[A, B, C, D, E](construct: (A, B, C, D) => E)(deconstruct: E => (A, B, C, D))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[E] =
    new GeneratorFor4[A, B, C, D, E](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD)

  def instancesOf[A, B, C, D, E, F](construct: (A, B, C, D, E) => F)(deconstruct: F => (A, B, C, D, E))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[F] =
    new GeneratorFor5[A, B, C, D, E, F](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE)

  def instancesOf[A, B, C, D, E, F, G](construct: (A, B, C, D, E, F) => G)(deconstruct: G => (A, B, C, D, E, F))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[G] =
    new GeneratorFor6[A, B, C, D, E, F, G](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF)

  def instancesOf[A, B, C, D, E, F, G, H](construct: (A, B, C, D, E, F, G) => H)(deconstruct: H => (A, B, C, D, E, F, G))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                  genOfG: Generator[G]): Generator[H] =
    new GeneratorFor7[A, B, C, D, E, F, G, H](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG)

  def instancesOf[A, B, C, D, E, F, G, H, I](construct: (A, B, C, D, E, F, G, H) => I)(deconstruct: I => (A, B, C, D, E, F, G, H))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                  genOfG: Generator[G], genOfH: Generator[H]): Generator[I] =
    new GeneratorFor8[A, B, C, D, E, F, G, H, I](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH)

  def instancesOf[A, B, C, D, E, F, G, H, I, J](construct: (A, B, C, D, E, F, G, H, I) => J)(deconstruct: J => (A, B, C, D, E, F, G, H, I))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                           genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[J] =
    new GeneratorFor9[A, B, C, D, E, F, G, H, I, J](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K](construct: (A, B, C, D, E, F, G, H, I, J) => K)(deconstruct: K => (A, B, C, D, E, F, G, H, I, J))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                    genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J]): Generator[K] =
    new GeneratorFor10[A, B, C, D, E, F, G, H, I, J, K](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L](construct: (A, B, C, D, E, F, G, H, I, J, K) => L)(deconstruct: L => (A, B, C, D, E, F, G, H, I, J, K))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                             genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K]): Generator[L] =
    new GeneratorFor11[A, B, C, D, E, F, G, H, I, J, K, L](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M](construct: (A, B, C, D, E, F, G, H, I, J, K, L) => M)(deconstruct: M => (A, B, C, D, E, F, G, H, I, J, K, L))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                      genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[M] =
    new GeneratorFor12[A, B, C, D, E, F, G, H, I, J, K, L, M](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N)(deconstruct: N => (A, B, C, D, E, F, G, H, I, J, K, L, M))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                               genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[N] =
    new GeneratorFor13[A, B, C, D, E, F, G, H, I, J, K, L, M, N](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O)(deconstruct: O => (A, B, C, D, E, F, G, H, I, J, K, L, M, N))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                 genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                 genOfN: Generator[N]): Generator[O] =
    new GeneratorFor14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P)(deconstruct: P => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                 genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                 genOfN: Generator[N], genOfO: Generator[O]): Generator[P] =
    new GeneratorFor15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q)(deconstruct: Q => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                          genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                          genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[Q] =
    new GeneratorFor16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R)(deconstruct: R => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                   genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                   genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[R] =
    new GeneratorFor17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S)(deconstruct: S => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                            genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                            genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[S] =
    new GeneratorFor18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T)(deconstruct: T => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                     genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                     genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[T] =
    new GeneratorFor19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U)(deconstruct: U => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                              genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                              genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T]): Generator[U] =
    new GeneratorFor20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V)(deconstruct: V => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                       genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                       genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T],
                                                                                                                                                                                                                                       genOfU: Generator[U]): Generator[V] =
    new GeneratorFor21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU)

  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W)(deconstruct: W => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                                genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                                genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T],
                                                                                                                                                                                                                                                genOfU: Generator[U], genOfV: Generator[V]): Generator[W] =
    new GeneratorFor22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU, genOfV)

  // classify will need to use the same sizing algo as forAll, and same edges approach
  def classify[A](count: PosInt, genOfA: Generator[A])(pf: PartialFunction[A, String]): Classification = {

    val (initEdges, rnd1) = genOfA.initEdges(100, Randomizer.default())
    @tailrec
    def loop(currentCount: Int, edges: List[A], rnd: Randomizer, acc: Map[String, PosZInt]): Map[String, PosZInt] = {
      if (currentCount >= count) acc
      else {
        val (nextA, nextEdges, nextRnd) = genOfA.next(100, 100, edges, rnd) // TODO: I think this need to mimic forAll.
        if (pf.isDefinedAt(nextA)) {
          val category = pf(nextA)
          val prevTotal = acc.getOrElse(category, PosZInt(0))
          val nextAcc = acc + (category -> PosZInt.ensuringValid(prevTotal + 1))
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

  def first1000Primes: Generator[Int] =
    new Generator[Int] { thisIntGenerator =>
      def next(size: PosZInt, maxSize: PosZInt, edges: List[Int], rnd: Randomizer): (Int, List[Int], Randomizer) = {
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            import CommonGenerators.primeNumbers
            val (index, nextRandomizer) = rnd.chooseInt(0, primeNumbers.length - 1)
            (primeNumbers(index), Nil, nextRandomizer)
        }
      }
    }
}

object CommonGenerators extends CommonGenerators {
  private val primeNumbers =
    Vector(
         2,     3,     5,     7,    11,    13,    17,    19,    23,    29,
        31,    37,    41,    43,    47,    53,    59,    61,    67,    71,
        73,    79,    83,    89,    97,   101,   103,   107,   109,   113,
       127,   131,   137,   139,   149,   151,   157,   163,   167,   173,
       179,   181,   191,   193,   197,   199,   211,   223,   227,   229,
       233,   239,   241,   251,   257,   263,   269,   271,   277,   281,
       283,   293,   307,   311,   313,   317,   331,   337,   347,   349,
       353,   359,   367,   373,   379,   383,   389,   397,   401,   409,
       419,   421,   431,   433,   439,   443,   449,   457,   461,   463,
       467,   479,   487,   491,   499,   503,   509,   521,   523,   541,
       547,   557,   563,   569,   571,   577,   587,   593,   599,   601,
       607,   613,   617,   619,   631,   641,   643,   647,   653,   659,
       661,   673,   677,   683,   691,   701,   709,   719,   727,   733,
       739,   743,   751,   757,   761,   769,   773,   787,   797,   809,
       811,   821,   823,   827,   829,   839,   853,   857,   859,   863,
       877,   881,   883,   887,   907,   911,   919,   929,   937,   941,
       947,   953,   967,   971,   977,   983,   991,   997,  1009,  1013,
      1019,  1021,  1031,  1033,  1039,  1049,  1051,  1061,  1063,  1069,
      1087,  1091,  1093,  1097,  1103,  1109,  1117,  1123,  1129,  1151,
      1153,  1163,  1171,  1181,  1187,  1193,  1201,  1213,  1217,  1223,
      1229,  1231,  1237,  1249,  1259,  1277,  1279,  1283,  1289,  1291,
      1297,  1301,  1303,  1307,  1319,  1321,  1327,  1361,  1367,  1373,
      1381,  1399,  1409,  1423,  1427,  1429,  1433,  1439,  1447,  1451,
      1453,  1459,  1471,  1481,  1483,  1487,  1489,  1493,  1499,  1511,
      1523,  1531,  1543,  1549,  1553,  1559,  1567,  1571,  1579,  1583,
      1597,  1601,  1607,  1609,  1613,  1619,  1621,  1627,  1637,  1657,
      1663,  1667,  1669,  1693,  1697,  1699,  1709,  1721,  1723,  1733,
      1741,  1747,  1753,  1759,  1777,  1783,  1787,  1789,  1801,  1811,
      1823,  1831,  1847,  1861,  1867,  1871,  1873,  1877,  1879,  1889,
      1901,  1907,  1913,  1931,  1933,  1949,  1951,  1973,  1979,  1987,
      1993,  1997,  1999,  2003,  2011,  2017,  2027,  2029,  2039,  2053,
      2063,  2069,  2081,  2083,  2087,  2089,  2099,  2111,  2113,  2129,
      2131,  2137,  2141,  2143,  2153,  2161,  2179,  2203,  2207,  2213,
      2221,  2237,  2239,  2243,  2251,  2267,  2269,  2273,  2281,  2287,
      2293,  2297,  2309,  2311,  2333,  2339,  2341,  2347,  2351,  2357,
      2371,  2377,  2381,  2383,  2389,  2393,  2399,  2411,  2417,  2423,
      2437,  2441,  2447,  2459,  2467,  2473,  2477,  2503,  2521,  2531,
      2539,  2543,  2549,  2551,  2557,  2579,  2591,  2593,  2609,  2617,
      2621,  2633,  2647,  2657,  2659,  2663,  2671,  2677,  2683,  2687,
      2689,  2693,  2699,  2707,  2711,  2713,  2719,  2729,  2731,  2741,
      2749,  2753,  2767,  2777,  2789,  2791,  2797,  2801,  2803,  2819,
      2833,  2837,  2843,  2851,  2857,  2861,  2879,  2887,  2897,  2903,
      2909,  2917,  2927,  2939,  2953,  2957,  2963,  2969,  2971,  2999,
      3001,  3011,  3019,  3023,  3037,  3041,  3049,  3061,  3067,  3079,
      3083,  3089,  3109,  3119,  3121,  3137,  3163,  3167,  3169,  3181,
      3187,  3191,  3203,  3209,  3217,  3221,  3229,  3251,  3253,  3257,
      3259,  3271,  3299,  3301,  3307,  3313,  3319,  3323,  3329,  3331,
      3343,  3347,  3359,  3361,  3371,  3373,  3389,  3391,  3407,  3413,
      3433,  3449,  3457,  3461,  3463,  3467,  3469,  3491,  3499,  3511,
      3517,  3527,  3529,  3533,  3539,  3541,  3547,  3557,  3559,  3571,
      3581,  3583,  3593,  3607,  3613,  3617,  3623,  3631,  3637,  3643,
      3659,  3671,  3673,  3677,  3691,  3697,  3701,  3709,  3719,  3727,
      3733,  3739,  3761,  3767,  3769,  3779,  3793,  3797,  3803,  3821,
      3823,  3833,  3847,  3851,  3853,  3863,  3877,  3881,  3889,  3907,
      3911,  3917,  3919,  3923,  3929,  3931,  3943,  3947,  3967,  3989,
      4001,  4003,  4007,  4013,  4019,  4021,  4027,  4049,  4051,  4057,
      4073,  4079,  4091,  4093,  4099,  4111,  4127,  4129,  4133,  4139,
      4153,  4157,  4159,  4177,  4201,  4211,  4217,  4219,  4229,  4231,
      4241,  4243,  4253,  4259,  4261,  4271,  4273,  4283,  4289,  4297,
      4327,  4337,  4339,  4349,  4357,  4363,  4373,  4391,  4397,  4409,
      4421,  4423,  4441,  4447,  4451,  4457,  4463,  4481,  4483,  4493,
      4507,  4513,  4517,  4519,  4523,  4547,  4549,  4561,  4567,  4583,
      4591,  4597,  4603,  4621,  4637,  4639,  4643,  4649,  4651,  4657,
      4663,  4673,  4679,  4691,  4703,  4721,  4723,  4729,  4733,  4751,
      4759,  4783,  4787,  4789,  4793,  4799,  4801,  4813,  4817,  4831,
      4861,  4871,  4877,  4889,  4903,  4909,  4919,  4931,  4933,  4937,
      4943,  4951,  4957,  4967,  4969,  4973,  4987,  4993,  4999,  5003,
      5009,  5011,  5021,  5023,  5039,  5051,  5059,  5077,  5081,  5087,
      5099,  5101,  5107,  5113,  5119,  5147,  5153,  5167,  5171,  5179,
      5189,  5197,  5209,  5227,  5231,  5233,  5237,  5261,  5273,  5279,
      5281,  5297,  5303,  5309,  5323,  5333,  5347,  5351,  5381,  5387,
      5393,  5399,  5407,  5413,  5417,  5419,  5431,  5437,  5441,  5443,
      5449,  5471,  5477,  5479,  5483,  5501,  5503,  5507,  5519,  5521,
      5527,  5531,  5557,  5563,  5569,  5573,  5581,  5591,  5623,  5639,
      5641,  5647,  5651,  5653,  5657,  5659,  5669,  5683,  5689,  5693,
      5701,  5711,  5717,  5737,  5741,  5743,  5749,  5779,  5783,  5791,
      5801,  5807,  5813,  5821,  5827,  5839,  5843,  5849,  5851,  5857,
      5861,  5867,  5869,  5879,  5881,  5897,  5903,  5923,  5927,  5939,
      5953,  5981,  5987,  6007,  6011,  6029,  6037,  6043,  6047,  6053,
      6067,  6073,  6079,  6089,  6091,  6101,  6113,  6121,  6131,  6133,
      6143,  6151,  6163,  6173,  6197,  6199,  6203,  6211,  6217,  6221,
      6229,  6247,  6257,  6263,  6269,  6271,  6277,  6287,  6299,  6301,
      6311,  6317,  6323,  6329,  6337,  6343,  6353,  6359,  6361,  6367,
      6373,  6379,  6389,  6397,  6421,  6427,  6449,  6451,  6469,  6473,
      6481,  6491,  6521,  6529,  6547,  6551,  6553,  6563,  6569,  6571,
      6577,  6581,  6599,  6607,  6619,  6637,  6653,  6659,  6661,  6673,
      6679,  6689,  6691,  6701,  6703,  6709,  6719,  6733,  6737,  6761,
      6763,  6779,  6781,  6791,  6793,  6803,  6823,  6827,  6829,  6833,
      6841,  6857,  6863,  6869,  6871,  6883,  6899,  6907,  6911,  6917,
      6947,  6949,  6959,  6961,  6967,  6971,  6977,  6983,  6991,  6997,
      7001,  7013,  7019,  7027,  7039,  7043,  7057,  7069,  7079,  7103,
      7109,  7121,  7127,  7129,  7151,  7159,  7177,  7187,  7193,  7207,
      7211,  7213,  7219,  7229,  7237,  7243,  7247,  7253,  7283,  7297,
      7307,  7309,  7321,  7331,  7333,  7349,  7351,  7369,  7393,  7411,
      7417,  7433,  7451,  7457,  7459,  7477,  7481,  7487,  7489,  7499,
      7507,  7517,  7523,  7529,  7537,  7541,  7547,  7549,  7559,  7561,
      7573,  7577,  7583,  7589,  7591,  7603,  7607,  7621,  7639,  7643,
      7649,  7669,  7673,  7681,  7687,  7691,  7699,  7703,  7717,  7723,
      7727,  7741,  7753,  7757,  7759,  7789,  7793,  7817,  7823,  7829,
      7841,  7853,  7867,  7873,  7877,  7879,  7883,  7901,  7907,  7919
    )
}
