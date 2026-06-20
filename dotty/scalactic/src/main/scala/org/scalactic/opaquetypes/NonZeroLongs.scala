/*
 * Copyright 2001-2026 Artima, Inc.
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
 * and limitations under the License.
 */
package org.scalactic.opaquetypes

import org.scalactic.Resources
import scala.compiletime.{constValueOpt, error}
import scala.util.{Failure, Success, Try}

import org.scalactic.{Bad, Fail, Good, Or, Pass, Validation}

object NonZeroLongs {
  /** Opaque type representing non-zero Long values (!= 0). */
  opaque type NonZeroLong = Long

  // Import opaque types we need for extension methods
  import PosLongs.{PosLong, PosZLong}
  import PosInts.{PosInt, PosZInt}
  import NegInts.{NegInt, NegZInt}
  import NonZeroInts.NonZeroInt
  import NegLongs.{NegLong, NegZLong}
  import PosFloats.{PosFloat, PosZFloat, PosFiniteFloat, PosZFiniteFloat}
  import NegFloats.{NegFloat, NegZFloat, NegFiniteFloat, NegZFiniteFloat}
  import NonZeroFloats.{NonZeroFloat, NonZeroFiniteFloat}
  import Finites.{FiniteFloat, FiniteDouble}
  import PosDoubles.{PosDouble, PosZDouble, PosFiniteDouble, PosZFiniteDouble}
  import NegDoubles.{NegDouble, NegZDouble, NegFiniteDouble, NegZFiniteDouble}
  import NonZeroDoubles.{NonZeroDouble, NonZeroFiniteDouble}

  /** Companion object for [[NonZeroLong]] with construction and validation helpers. */
  object NonZeroLong {
    /** Compile-time factory for creating a [[NonZeroLong]] from a long literal.
      *
      * Rejects zero literals at compile time.
      */
    inline def apply[L <: Long & Singleton](inline l: L): NonZeroLong =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v == 0L then
            error("NonZeroLong cannot be instantiated with zero")
          else
            v.asInstanceOf[NonZeroLong]
        case None =>
          error("NonZeroLong.apply requires a long literal")
      }

    /** Compile-time factory for creating a [[NonZeroLong]] from an integer literal. */
    inline def apply[I <: Int & Singleton](inline i: I): NonZeroLong =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v == 0 then
            error("NonZeroLong cannot be instantiated with zero")
          else
            v.toLong.asInstanceOf[NonZeroLong]
        case None =>
          error("NonZeroLong.apply requires an integer or long literal")
      }

    /** Construct a [[NonZeroLong]] from a runtime Long if it is non-zero.
      *
      * @return Some(NonZeroLong) when l != 0, else None
      */
    def from(l: Long): Option[NonZeroLong] =
      if (l != 0L) Some(l) else None

    /** Construct a [[NonZeroLong]] from a runtime Int if it is non-zero.
      *
      * @return Some(NonZeroLong) when i != 0, else None
      */
    def from(i: Int): Option[NonZeroLong] =
      if (i != 0) Some(i.toLong) else None

    /** Validate and return the given Long as [[NonZeroLong]].
      *
      * @throws AssertionError if l is zero
      */
    def ensuringValid(l: Long): NonZeroLong =
      if (l == 0L)
        throw new AssertionError(Resources.invalidNonZeroLong)
      else l

    /** Runtime factory that returns Success for valid input, Failure otherwise. */
    def tryingValid(value: Long): Try[NonZeroLong] =
      if (value != 0L) Success(value)
      else Failure(new AssertionError(Resources.invalidNonZeroLong))

    /** Predicate indicating whether the given Long is valid for [[NonZeroLong]]. */
    def isValid(value: Long): Boolean = value != 0L

    /** Validate a value and return Pass, else Fail(f(value)). */
    def passOrElse[E](value: Long)(f: Long => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(NonZeroLong), else Bad(f(value)). */
    def goodOrElse[B](value: Long)(f: Long => B): NonZeroLong Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Validate a value and return Right(NonZeroLong), else Left(f(value)). */
    def rightOrElse[L](value: Long)(f: Long => L): Either[L, NonZeroLong] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid. */
    def fromOrElse(value: Long, default: => NonZeroLong): NonZeroLong =
      if (isValid(value)) value else default

    /** Largest valid [[NonZeroLong]] value. */
    val MaxValue: NonZeroLong = Long.MaxValue

    /** Smallest valid [[NonZeroLong]] value. */
    val MinValue: NonZeroLong = Long.MinValue
  }

  // All extension methods are now at NonZeroLongs level to avoid ambiguity
  // with implicit conversions defined in the same object

  extension (x: NonZeroLong) {
    /** Return the underlying Long value. */
    def value: Long = x

    /** Unary plus returns this value unchanged. */
    def unary_+ : NonZeroLong = x

    /** Numeric negation as NonZeroLong. */
    def unary_- : NonZeroLong = NonZeroLong.ensuringValid(-x)

    /** Greater of this and that value. */
    def max(that: NonZeroLong): NonZeroLong = if (math.max(x, that) == x) x else that

    /** Lesser of this and that value. */
    def min(that: NonZeroLong): NonZeroLong = if (math.min(x, that) == x) x else that

    /** Apply `f` and require the result to remain a valid [[NonZeroLong]].
      *
      * @throws AssertionError if f(x) is zero
      */
    def ensuringValid(f: Long => Long): NonZeroLong = {
      val res = f(x)
      if (res == 0L)
        throw new AssertionError(Resources.invalidNonZeroLong)
      else res
    }

    /** Shift left by n bits. */
    def <<(n: Long): NonZeroLong = NonZeroLong.ensuringValid(x << n)

    /** Shift right by n bits. */
    def >>(n: Long): NonZeroLong = NonZeroLong.ensuringValid(x >> n)

    /** Unsigned shift right by n bits. */
    def >>>(n: Long): NonZeroLong = NonZeroLong.ensuringValid(x >>> n)

    // Arithmetic operations returning Long
    @annotation.targetName("plusByte")
    def +(y: Byte): Long = x + y
    @annotation.targetName("plusShort")
    def +(y: Short): Long = x + y
    @annotation.targetName("plusChar")
    def +(y: Char): Long = x + y
    @annotation.targetName("plusInt")
    def +(y: Int): Long = x + y
    @annotation.targetName("plusLong")
    def +(y: Long): Long = x + y

    @annotation.targetName("minusByte")
    def -(y: Byte): Long = x - y
    @annotation.targetName("minusShort")
    def -(y: Short): Long = x - y
    @annotation.targetName("minusChar")
    def -(y: Char): Long = x - y
    @annotation.targetName("minusInt")
    def -(y: Int): Long = x - y
    @annotation.targetName("minusLong")
    def -(y: Long): Long = x - y

    @annotation.targetName("timesByte")
    def *(y: Byte): Long = x * y
    @annotation.targetName("timesShort")
    def *(y: Short): Long = x * y
    @annotation.targetName("timesChar")
    def *(y: Char): Long = x * y
    @annotation.targetName("timesInt")
    def *(y: Int): Long = x * y
    @annotation.targetName("timesLong")
    def *(y: Long): Long = x * y

    @annotation.targetName("divByte")
    def /(y: Byte): Long = x / y
    @annotation.targetName("divShort")
    def /(y: Short): Long = x / y
    @annotation.targetName("divChar")
    def /(y: Char): Long = x / y
    @annotation.targetName("divInt")
    def /(y: Int): Long = x / y
    @annotation.targetName("divLong")
    def /(y: Long): Long = x / y

    @annotation.targetName("modByte")
    def %(y: Byte): Long = x % y
    @annotation.targetName("modShort")
    def %(y: Short): Long = x % y
    @annotation.targetName("modChar")
    def %(y: Char): Long = x % y
    @annotation.targetName("modInt")
    def %(y: Int): Long = x % y
    @annotation.targetName("modLong")
    def %(y: Long): Long = x % y

    // Arithmetic operations returning Float, Double
    @annotation.targetName("toFloat")
    def toFloat: Float = x.toFloat
    @annotation.targetName("toDouble")
    def toDouble: Double = x.toDouble

    @annotation.targetName("plusFloat")
    def +(y: Float): Float = x.toFloat + y
    @annotation.targetName("plusDouble")
    def +(y: Double): Double = x.toDouble + y

    @annotation.targetName("minusFloat")
    def -(y: Float): Float = x.toFloat - y
    @annotation.targetName("minusDouble")
    def -(y: Double): Double = x.toDouble - y

    @annotation.targetName("timesFloat")
    def *(y: Float): Float = x.toFloat * y
    @annotation.targetName("timesDouble")
    def *(y: Double): Double = x.toDouble * y

    @annotation.targetName("divFloat")
    def /(y: Float): Float = x.toFloat / y
    @annotation.targetName("divDouble")
    def /(y: Double): Double = x.toDouble / y

    @annotation.targetName("modFloat")
    def %(y: Float): Float = x.toFloat % y
    @annotation.targetName("modDouble")
    def %(y: Double): Double = x.toDouble % y

    // Comparison operations
    def <(y: Byte): Boolean = x < y
    def <(y: Short): Boolean = x < y
    def <(y: Char): Boolean = x < y
    def <(y: Int): Boolean = x < y
    def <(y: Long): Boolean = x < y
    def <(y: Float): Boolean = x < y
    def <(y: Double): Boolean = x < y

    def <=(y: Byte): Boolean = x <= y
    def <=(y: Short): Boolean = x <= y
    def <=(y: Char): Boolean = x <= y
    def <=(y: Int): Boolean = x <= y
    def <=(y: Long): Boolean = x <= y
    def <=(y: Float): Boolean = x <= y
    def <=(y: Double): Boolean = x <= y

    def >(y: Byte): Boolean = x > y
    def >(y: Short): Boolean = x > y
    def >(y: Char): Boolean = x > y
    def >(y: Int): Boolean = x > y
    def >(y: Long): Boolean = x > y
    def >(y: Float): Boolean = x > y
    def >(y: Double): Boolean = x > y

    def >=(y: Byte): Boolean = x >= y
    def >=(y: Short): Boolean = x >= y
    def >=(y: Char): Boolean = x >= y
    def >=(y: Int): Boolean = x >= y
    def >=(y: Long): Boolean = x >= y
    def >=(y: Float): Boolean = x >= y
    def >=(y: Double): Boolean = x >= y

    // Bitwise operations
    def |(y: Byte): Long = x | y
    def |(y: Short): Long = x | y
    def |(y: Char): Long = x | y
    def |(y: Int): Long = x | y
    def |(y: Long): Long = x | y

    def &(y: Byte): Long = x & y
    def &(y: Short): Long = x & y
    def &(y: Char): Long = x & y
    def &(y: Int): Long = x & y
    def &(y: Long): Long = x & y

    def ^(y: Byte): Long = x ^ y
    def ^(y: Short): Long = x ^ y
    def ^(y: Char): Long = x ^ y
    def ^(y: Int): Long = x ^ y
    def ^(y: Long): Long = x ^ y

    def unary_~ : Long = ~x
  }

  // Extension methods for arithmetic operations with other opaque types
  // These use @targetName to avoid JVM erasure conflicts with primitive type methods

  extension (x: NonZeroLong) {
    @annotation.targetName("plusPosLong")
    def +(y: PosLong): Long = x + y.value
    @annotation.targetName("plusPosZLong")
    def +(y: PosZLong): Long = x + y.value
    @annotation.targetName("plusNegLong")
    def +(y: NegLong): Long = x + y.value
    @annotation.targetName("plusNegZLong")
    def +(y: NegZLong): Long = x + y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("plusPosInt")
    def +(y: PosInt): Long = x + y.value
    @annotation.targetName("plusPosZInt")
    def +(y: PosZInt): Long = x + y.value
    @annotation.targetName("plusNonZeroInt")
    def +(y: NonZeroInt): Long = x + y.value
    @annotation.targetName("plusNegInt")
    def +(y: NegInt): Long = x + y.value
    @annotation.targetName("plusNegZInt")
    def +(y: NegZInt): Long = x + y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("plusPosFloat")
    def +(y: PosFloat): Float = x.toFloat + y.value
    @annotation.targetName("plusPosZFloat")
    def +(y: PosZFloat): Float = x.toFloat + y.value
    @annotation.targetName("plusPosZFiniteFloat")
    def +(y: PosZFiniteFloat): Float = x.toFloat + y.value
    @annotation.targetName("plusPosFiniteFloat")
    def +(y: PosFiniteFloat): Float = x.toFloat + y.value
    @annotation.targetName("plusFiniteFloat")
    def +(y: FiniteFloat): Float = x.toFloat + y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("plusNonZeroFloat")
    def +(y: NonZeroFloat): Float = x.toFloat + y  // implicit conversion to Float
    @annotation.targetName("plusNonZeroFiniteFloat")
    def +(y: NonZeroFiniteFloat): Float = x.toFloat + y  // implicit conversion to Float
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("plusNegFloat")
    def +(y: NegFloat): Float = x.toFloat + y.value
    @annotation.targetName("plusNegZFloat")
    def +(y: NegZFloat): Float = x.toFloat + y.value
    @annotation.targetName("plusNegZFiniteFloat")
    def +(y: NegZFiniteFloat): Float = x.toFloat + y.value
    @annotation.targetName("plusNegFiniteFloat")
    def +(y: NegFiniteFloat): Float = x.toFloat + y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("plusPosDouble")
    def +(y: PosDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusPosZDouble")
    def +(y: PosZDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusPosZFiniteDouble")
    def +(y: PosZFiniteDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusPosFiniteDouble")
    def +(y: PosFiniteDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusFiniteDouble")
    def +(y: FiniteDouble): Double = x.toDouble + y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("plusNonZeroDouble")
    def +(y: NonZeroDouble): Double = x.toDouble + y  // implicit conversion to Double
    @annotation.targetName("plusNonZeroFiniteDouble")
    def +(y: NonZeroFiniteDouble): Double = x.toDouble + y  // implicit conversion to Double
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("plusNegDouble")
    def +(y: NegDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusNegZDouble")
    def +(y: NegZDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusNegZFiniteDouble")
    def +(y: NegZFiniteDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusNegFiniteDouble")
    def +(y: NegFiniteDouble): Double = x.toDouble + y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusPosLong")
    def -(y: PosLong): Long = x - y.value
    @annotation.targetName("minusPosZLong")
    def -(y: PosZLong): Long = x - y.value
    @annotation.targetName("minusNegLong")
    def -(y: NegLong): Long = x - y.value
    @annotation.targetName("minusNegZLong")
    def -(y: NegZLong): Long = x - y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusPosInt")
    def -(y: PosInt): Long = x - y.value
    @annotation.targetName("minusPosZInt")
    def -(y: PosZInt): Long = x - y.value
    @annotation.targetName("minusNonZeroInt")
    def -(y: NonZeroInt): Long = x - y.value
    @annotation.targetName("minusNegInt")
    def -(y: NegInt): Long = x - y.value
    @annotation.targetName("minusNegZInt")
    def -(y: NegZInt): Long = x - y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusPosFloat")
    def -(y: PosFloat): Float = x.toFloat - y.value
    @annotation.targetName("minusPosZFloat")
    def -(y: PosZFloat): Float = x.toFloat - y.value
    @annotation.targetName("minusPosZFiniteFloat")
    def -(y: PosZFiniteFloat): Float = x.toFloat - y.value
    @annotation.targetName("minusPosFiniteFloat")
    def -(y: PosFiniteFloat): Float = x.toFloat - y.value
    @annotation.targetName("minusFiniteFloat")
    def -(y: FiniteFloat): Float = x.toFloat - y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusNonZeroFloat")
    def -(y: NonZeroFloat): Float = x.toFloat - y  // implicit conversion to Float
    @annotation.targetName("minusNonZeroFiniteFloat")
    def -(y: NonZeroFiniteFloat): Float = x.toFloat - y  // implicit conversion to Float
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusNegFloat")
    def -(y: NegFloat): Float = x.toFloat - y.value
    @annotation.targetName("minusNegZFloat")
    def -(y: NegZFloat): Float = x.toFloat - y.value
    @annotation.targetName("minusNegZFiniteFloat")
    def -(y: NegZFiniteFloat): Float = x.toFloat - y.value
    @annotation.targetName("minusNegFiniteFloat")
    def -(y: NegFiniteFloat): Float = x.toFloat - y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusPosDouble")
    def -(y: PosDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusPosZDouble")
    def -(y: PosZDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusPosZFiniteDouble")
    def -(y: PosZFiniteDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusPosFiniteDouble")
    def -(y: PosFiniteDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusFiniteDouble")
    def -(y: FiniteDouble): Double = x.toDouble - y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusNonZeroDouble")
    def -(y: NonZeroDouble): Double = x.toDouble - y  // implicit conversion to Double
    @annotation.targetName("minusNonZeroFiniteDouble")
    def -(y: NonZeroFiniteDouble): Double = x.toDouble - y  // implicit conversion to Double
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("minusNegDouble")
    def -(y: NegDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusNegZDouble")
    def -(y: NegZDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusNegZFiniteDouble")
    def -(y: NegZFiniteDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusNegFiniteDouble")
    def -(y: NegFiniteDouble): Double = x.toDouble - y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesPosLong")
    def *(y: PosLong): Long = x * y.value
    @annotation.targetName("timesPosZLong")
    def *(y: PosZLong): Long = x * y.value
    @annotation.targetName("timesNegLong")
    def *(y: NegLong): Long = x * y.value
    @annotation.targetName("timesNegZLong")
    def *(y: NegZLong): Long = x * y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesPosInt")
    def *(y: PosInt): Long = x * y.value
    @annotation.targetName("timesPosZInt")
    def *(y: PosZInt): Long = x * y.value
    @annotation.targetName("timesNonZeroInt")
    def *(y: NonZeroInt): Long = x * y.value
    @annotation.targetName("timesNegInt")
    def *(y: NegInt): Long = x * y.value
    @annotation.targetName("timesNegZInt")
    def *(y: NegZInt): Long = x * y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesPosFloat")
    def *(y: PosFloat): Float = x.toFloat * y.value
    @annotation.targetName("timesPosZFloat")
    def *(y: PosZFloat): Float = x.toFloat * y.value
    @annotation.targetName("timesPosZFiniteFloat")
    def *(y: PosZFiniteFloat): Float = x.toFloat * y.value
    @annotation.targetName("timesPosFiniteFloat")
    def *(y: PosFiniteFloat): Float = x.toFloat * y.value
    @annotation.targetName("timesFiniteFloat")
    def *(y: FiniteFloat): Float = x.toFloat * y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesNonZeroFloat")
    def *(y: NonZeroFloat): Float = x.toFloat * y  // implicit conversion to Float
    @annotation.targetName("timesNonZeroFiniteFloat")
    def *(y: NonZeroFiniteFloat): Float = x.toFloat * y  // implicit conversion to Float
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesNegFloat")
    def *(y: NegFloat): Float = x.toFloat * y.value
    @annotation.targetName("timesNegZFloat")
    def *(y: NegZFloat): Float = x.toFloat * y.value
    @annotation.targetName("timesNegZFiniteFloat")
    def *(y: NegZFiniteFloat): Float = x.toFloat * y.value
    @annotation.targetName("timesNegFiniteFloat")
    def *(y: NegFiniteFloat): Float = x.toFloat * y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesPosDouble")
    def *(y: PosDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesPosZDouble")
    def *(y: PosZDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesPosZFiniteDouble")
    def *(y: PosZFiniteDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesPosFiniteDouble")
    def *(y: PosFiniteDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesFiniteDouble")
    def *(y: FiniteDouble): Double = x.toDouble * y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesNonZeroDouble")
    def *(y: NonZeroDouble): Double = x.toDouble * y  // implicit conversion to Double
    @annotation.targetName("timesNonZeroFiniteDouble")
    def *(y: NonZeroFiniteDouble): Double = x.toDouble * y  // implicit conversion to Double
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("timesNegDouble")
    def *(y: NegDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesNegZDouble")
    def *(y: NegZDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesNegZFiniteDouble")
    def *(y: NegZFiniteDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesNegFiniteDouble")
    def *(y: NegFiniteDouble): Double = x.toDouble * y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divPosLong")
    def /(y: PosLong): Long = x / y.value
    @annotation.targetName("divPosZLong")
    def /(y: PosZLong): Long = x / y.value
    @annotation.targetName("divNegLong")
    def /(y: NegLong): Long = x / y.value
    @annotation.targetName("divNegZLong")
    def /(y: NegZLong): Long = x / y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divPosInt")
    def /(y: PosInt): Long = x / y.value
    @annotation.targetName("divPosZInt")
    def /(y: PosZInt): Long = x / y.value
    @annotation.targetName("divNonZeroInt")
    def /(y: NonZeroInt): Long = x / y.value
    @annotation.targetName("divNegInt")
    def /(y: NegInt): Long = x / y.value
    @annotation.targetName("divNegZInt")
    def /(y: NegZInt): Long = x / y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divPosFloat")
    def /(y: PosFloat): Float = x.toFloat / y.value
    @annotation.targetName("divPosZFloat")
    def /(y: PosZFloat): Float = x.toFloat / y.value
    @annotation.targetName("divPosZFiniteFloat")
    def /(y: PosZFiniteFloat): Float = x.toFloat / y.value
    @annotation.targetName("divPosFiniteFloat")
    def /(y: PosFiniteFloat): Float = x.toFloat / y.value
    @annotation.targetName("divFiniteFloat")
    def /(y: FiniteFloat): Float = x.toFloat / y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divNonZeroFloat")
    def /(y: NonZeroFloat): Float = x.toFloat / y  // implicit conversion to Float
    @annotation.targetName("divNonZeroFiniteFloat")
    def /(y: NonZeroFiniteFloat): Float = x.toFloat / y  // implicit conversion to Float
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divNegFloat")
    def /(y: NegFloat): Float = x.toFloat / y.value
    @annotation.targetName("divNegZFloat")
    def /(y: NegZFloat): Float = x.toFloat / y.value
    @annotation.targetName("divNegZFiniteFloat")
    def /(y: NegZFiniteFloat): Float = x.toFloat / y.value
    @annotation.targetName("divNegFiniteFloat")
    def /(y: NegFiniteFloat): Float = x.toFloat / y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divPosDouble")
    def /(y: PosDouble): Double = x.toDouble / y.value
    @annotation.targetName("divPosZDouble")
    def /(y: PosZDouble): Double = x.toDouble / y.value
    @annotation.targetName("divPosZFiniteDouble")
    def /(y: PosZFiniteDouble): Double = x.toDouble / y.value
    @annotation.targetName("divPosFiniteDouble")
    def /(y: PosFiniteDouble): Double = x.toDouble / y.value
    @annotation.targetName("divFiniteDouble")
    def /(y: FiniteDouble): Double = x.toDouble / y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divNonZeroDouble")
    def /(y: NonZeroDouble): Double = x.toDouble / y  // implicit conversion to Double
    @annotation.targetName("divNonZeroFiniteDouble")
    def /(y: NonZeroFiniteDouble): Double = x.toDouble / y  // implicit conversion to Double
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("divNegDouble")
    def /(y: NegDouble): Double = x.toDouble / y.value
    @annotation.targetName("divNegZDouble")
    def /(y: NegZDouble): Double = x.toDouble / y.value
    @annotation.targetName("divNegZFiniteDouble")
    def /(y: NegZFiniteDouble): Double = x.toDouble / y.value
    @annotation.targetName("divNegFiniteDouble")
    def /(y: NegFiniteDouble): Double = x.toDouble / y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modPosLong")
    def %(y: PosLong): Long = x % y.value
    @annotation.targetName("modPosZLong")
    def %(y: PosZLong): Long = x % y.value
    @annotation.targetName("modNegLong")
    def %(y: NegLong): Long = x % y.value
    @annotation.targetName("modNegZLong")
    def %(y: NegZLong): Long = x % y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modPosInt")
    def %(y: PosInt): Long = x % y.value
    @annotation.targetName("modPosZInt")
    def %(y: PosZInt): Long = x % y.value
    @annotation.targetName("modNonZeroInt")
    def %(y: NonZeroInt): Long = x % y.value
    @annotation.targetName("modNegInt")
    def %(y: NegInt): Long = x % y.value
    @annotation.targetName("modNegZInt")
    def %(y: NegZInt): Long = x % y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modPosFloat")
    def %(y: PosFloat): Float = x.toFloat % y.value
    @annotation.targetName("modPosZFloat")
    def %(y: PosZFloat): Float = x.toFloat % y.value
    @annotation.targetName("modPosZFiniteFloat")
    def %(y: PosZFiniteFloat): Float = x.toFloat % y.value
    @annotation.targetName("modPosFiniteFloat")
    def %(y: PosFiniteFloat): Float = x.toFloat % y.value
    @annotation.targetName("modFiniteFloat")
    def %(y: FiniteFloat): Float = x.toFloat % y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modNonZeroFloat")
    def %(y: NonZeroFloat): Float = x.toFloat % y  // implicit conversion to Float
    @annotation.targetName("modNonZeroFiniteFloat")
    def %(y: NonZeroFiniteFloat): Float = x.toFloat % y  // implicit conversion to Float
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modNegFloat")
    def %(y: NegFloat): Float = x.toFloat % y.value
    @annotation.targetName("modNegZFloat")
    def %(y: NegZFloat): Float = x.toFloat % y.value
    @annotation.targetName("modNegZFiniteFloat")
    def %(y: NegZFiniteFloat): Float = x.toFloat % y.value
    @annotation.targetName("modNegFiniteFloat")
    def %(y: NegFiniteFloat): Float = x.toFloat % y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modPosDouble")
    def %(y: PosDouble): Double = x.toDouble % y.value
    @annotation.targetName("modPosZDouble")
    def %(y: PosZDouble): Double = x.toDouble % y.value
    @annotation.targetName("modPosZFiniteDouble")
    def %(y: PosZFiniteDouble): Double = x.toDouble % y.value
    @annotation.targetName("modPosFiniteDouble")
    def %(y: PosFiniteDouble): Double = x.toDouble % y.value
    @annotation.targetName("modFiniteDouble")
    def %(y: FiniteDouble): Double = x.toDouble % y.value
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modNonZeroDouble")
    def %(y: NonZeroDouble): Double = x.toDouble % y  // implicit conversion to Double
    @annotation.targetName("modNonZeroFiniteDouble")
    def %(y: NonZeroFiniteDouble): Double = x.toDouble % y  // implicit conversion to Double
  }

  extension (x: NonZeroLong) {
    @annotation.targetName("modNegDouble")
    def %(y: NegDouble): Double = x.toDouble % y.value
    @annotation.targetName("modNegZDouble")
    def %(y: NegZDouble): Double = x.toDouble % y.value
    @annotation.targetName("modNegZFiniteDouble")
    def %(y: NegZFiniteDouble): Double = x.toDouble % y.value
    @annotation.targetName("modNegFiniteDouble")
    def %(y: NegFiniteDouble): Double = x.toDouble % y.value
  }

  /** Widen [[NonZeroLong]] to `Long`.
    *
    * @param x the wrapped value
    * @return the underlying `Long`
    */
  given Conversion[NonZeroLong, Long] with {
    def apply(x: NonZeroLong): Long = x
  }

  /** Widen [[NonZeroLong]] to [[NonZeroFloat]]. */
  given Conversion[NonZeroLong, NonZeroFloats.NonZeroFloat] with {
    def apply(x: NonZeroLong): NonZeroFloats.NonZeroFloat = NonZeroFloats.NonZeroFloat.ensuringValid(x.toFloat)
  }

  /** Widen [[NonZeroLong]] to [[NonZeroDouble]]. */
  given Conversion[NonZeroLong, NonZeroDoubles.NonZeroDouble] with {
    def apply(x: NonZeroLong): NonZeroDoubles.NonZeroDouble = NonZeroDoubles.NonZeroDouble.ensuringValid(x.toDouble)
  }

  /** Convert Long to [[NonZeroLong]] via compile-time or runtime validation. */
  given Conversion[Long, NonZeroLong] with {
    inline def apply[L <: Long & Singleton](inline x: L): NonZeroLong =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v == 0L then
            error("NonZeroLong cannot be instantiated with zero")
          else
            v.asInstanceOf[NonZeroLong]
        case None =>
          error("NonZeroLong conversion requires a long literal")
      }

    def apply(x: Long): NonZeroLong = NonZeroLong.ensuringValid(x)
  }

  /** Ordering instance based on underlying numeric Long ordering. */
  given Ordering[NonZeroLong] with {
    def compare(x: NonZeroLong, y: NonZeroLong): Int = x.compareTo(y)
  }

}
