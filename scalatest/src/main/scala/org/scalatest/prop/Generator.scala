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
      def next(size: Int, rnd: Randomizer): (Int => List[String], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (String => List[Long], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (Int => Int, Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (Int => Option[Int], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (Int => String, Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (String => Int, Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (String => String, Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (String => Option[String], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (String => Option[Long], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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
      def next(size: Int, rnd: Randomizer): (String => Long, Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (funs(idx), nextRnd)
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

  implicit def orGenerator[G, B](implicit genOfG: Generator[G], genOfB: Generator[B]): Generator[G Or B] =
    new Generator[G Or B] {
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

