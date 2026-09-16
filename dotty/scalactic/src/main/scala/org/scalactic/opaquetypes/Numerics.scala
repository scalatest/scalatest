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
 * limitations under the License.
 */
package org.scalactic.opaquetypes

import org.scalactic.Resources
import scala.compiletime.{ constValueOpt, error }
import scala.quoted.*
import scala.util.{Try, Success, Failure}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}

import PosInts.{PosInt, PosZInt}
import PosLongs.{PosLong, PosZLong}
import PosFloats.{PosFloat, PosZFloat, PosZFiniteFloat, PosFiniteFloat}
import PosDoubles.{PosDouble, PosZDouble, PosZFiniteDouble, PosFiniteDouble}
import NegLongs.{NegLong, NegZLong}
import NegDoubles.{NegDouble, NegZDouble}
import NegFloats.{NegFloat, NegZFloat, NegZFiniteFloat, NegFiniteFloat}
import NegInts.{NegInt, NegZInt}
import NonZeroLongs.{NonZeroLong}
import NonZeroFloats.{NonZeroFloat, NonZeroFiniteFloat}
import NonZeroDoubles.{NonZeroDouble, NonZeroFiniteDouble}
import NonZeroInts.NonZeroInt
import Finites.{FiniteFloat, FiniteDouble}

import scala.annotation.targetName

/** Factory object for numeric opaque types that enforce value constraints at
  * compile time or runtime.
  *
  * Currently provides the [[NumericChar]] opaque type, which restricts `Char`
  * values to the digit characters `'0'` through `'9'`.
  */
object Numerics {

  /** Opaque type representing numeric Char values ('0' to '9').
    *
    * Instances of this type are guaranteed to represent a digit character.
    * Use the compile-time apply method to construct instances from literals,
    * or the runtime factory methods for values known only at runtime.
    */
  opaque type NumericChar = Char

  /** Companion object for [[NumericChar]] with construction and validation helpers.
    *
    * Provides factory methods for compile-time-checked construction from Char
    * literals, runtime validation helpers, given conversions, and extension
    * methods for common operations.
    */
  object NumericChar {
    /** Compile-time factory for creating a [[NumericChar]] from a Char literal.
      *
      * Rejects non-numeric character literals at compile time.
      */
    inline def apply[C <: Char & Singleton](inline c: C): NumericChar =
      inline constValueOpt[C] match {
        case Some(v: Char) =>
          inline if v >= '0' && v <= '9' then
            v.asInstanceOf[NumericChar]
          else
            error("NumericChar.apply can only be invoked on Char literals that are numeric, like NumericChar('8').")
        case None =>
          error("NumericChar.apply can only be invoked on Char literals that are numeric, like NumericChar('8').")
      }

    /** Construct a [[NumericChar]] from a runtime Char if it is numeric.
      *
      * @param c the Char to validate
      * @return Some(NumericChar) if c is between '0' and '9', else None
      */
    def from(c: Char): Option[NumericChar] =
      if (c >= '0' && c <= '9') Some(c) else None

    /** Validate and return the given Char as [[NumericChar]].
      *
      * @throws AssertionError if c is not a numeric character
      */
    def ensuringValid(c: Char): NumericChar =
      if (c >= '0' && c <= '9')
        c.asInstanceOf[NumericChar]
      else
        throw new AssertionError(Resources.invalidNumericChar)

    /** Runtime factory that returns Success for valid input, Failure otherwise.
      *
      * @param value the Char to validate
      * @return Success(NumericChar) if value is between '0' and '9',
      *   else Failure(AssertionError)
      */
    def tryingValid(value: Char): Try[NumericChar] =
      if (value >= '0' && value <= '9')
        Success(value.asInstanceOf[NumericChar])
      else
        Failure(new AssertionError(Resources.invalidNumericChar))

    /** Predicate indicating whether the given Char is valid for [[NumericChar]].
      *
      * @param value the Char to validate
      * @return true if value is between '0' and '9', else false
      */
    def isValid(value: Char): Boolean = value >= '0' && value <= '9'

    /** Validate a value and return Pass, else Fail(f(value)).
      *
      * @param value the Char to validate
      * @param f function to produce an error value when validation fails
      * @return Pass if value is a valid NumericChar, else Fail(f(value))
      */
    def passOrElse[E](value: Char)(f: Char => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(NumericChar), else Bad(f(value)).
      *
      * @param value the Char to validate
      * @param f function to produce an error value when validation fails
      * @return Good(NumericChar) if value is valid, else Bad(f(value))
      */
    def goodOrElse[B](value: Char)(f: Char => B): NumericChar Or B =
      if (isValid(value)) Good(value.asInstanceOf[NumericChar]) else Bad(f(value))

    /** Validate a value and return Right(NumericChar), else Left(f(value)).
      *
      * @param value the Char to validate
      * @param f function to produce an error value when validation fails
      * @return Right(NumericChar) if value is valid, else Left(f(value))
      */
    def rightOrElse[L](value: Char)(f: Char => L): Either[L, NumericChar] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid.
      *
      * @param value the Char to validate
      * @param default the NumericChar to return if value is not valid
      * @return value as NumericChar if valid, else default
      */
    def fromOrElse(value: Char, default: => NumericChar): NumericChar =
      if (isValid(value)) value.asInstanceOf[NumericChar] else default

    /** Smallest valid NumericChar value (which is '0'). */
    val MinValue: NumericChar = '0'

    /** Largest valid NumericChar value (which is '9'). */
    val MaxValue: NumericChar = '9'

    extension (x: NumericChar) {
      /** Return the underlying Char value. */
      def value: Char = x

      /** Return the Char value (identity). */
      def toChar: Char = x

      /** Convert to Byte (the numeric ASCII value). */
      def toByte: Byte = x.toByte

      /** Convert to Short (the numeric ASCII value). */
      def toShort: Short = x.toShort

      /** Convert to Int (the numeric ASCII value). */
      def toInt: Int = x.toInt

      /** Convert to Long (the numeric ASCII value). */
      def toLong: Long = x.toLong

      /** Convert to Float (the numeric ASCII value). */
      def toFloat: Float = x.toFloat

      /** Convert to Double (the numeric ASCII value). */
      def toDouble: Double = x.toDouble

      /** Unary plus returns this value unchanged. */
      def unary_+ : NumericChar = x

      /** Unary minus returns the negation as NegZInt. */
      def unary_- : NegZInt = NegZInt.ensuringValid(-x.toInt)

      /** Bitwise complement returns the negation as an Int. */
      def unary_~ : Int = ~x.toInt

      /** The numeric digit this character represents (0-9). */
      def asDigit: Int = Character.digit(x, Character.MAX_RADIX)

      /** The numeric digit this character represents as a PosZInt. */
      def asDigitPosZInt: PosZInt = PosZInt.ensuringValid(asDigit)

      /** Greater of this and that value. */
      def max(that: NumericChar): NumericChar =
        if (x > that) x else that

      /** Lesser of this and that value. */
      def min(that: NumericChar): NumericChar =
        if (x < that) x else that

      // Bit shift operations - return Int
      def <<(shift: Int): Int = x.toInt << shift
      def <<(shift: Long): Int = x.toInt << shift
      def >>(shift: Int): Int = x.toInt >> shift
      def >>(shift: Long): Int = x.toInt >> shift
      def >>>(shift: Int): Int = x.toInt >>> shift
      def >>>(shift: Long): Int = x.toInt >>> shift

      // Bitwise operations - return Int
      def |(y: Byte): Int = x.toInt | y
      def |(y: Short): Int = x.toInt | y
      def |(y: Char): Int = x.toInt | y
      def |(y: Int): Int = x.toInt | y
      def |(y: Long): Long = x.toLong | y

      def &(y: Byte): Int = x.toInt & y
      def &(y: Short): Int = x.toInt & y
      def &(y: Char): Int = x.toInt & y
      def &(y: Int): Int = x.toInt & y
      def &(y: Long): Long = x.toLong & y

      def ^(y: Byte): Int = x.toInt ^ y
      def ^(y: Short): Int = x.toInt ^ y
      def ^(y: Char): Int = x.toInt ^ y
      def ^(y: Int): Int = x.toInt ^ y
      def ^(y: Long): Long = x.toLong ^ y

      // Comparison operations
      def <(y: Byte): Boolean = x.toInt < y
      def <(y: Short): Boolean = x.toInt < y
      def <(y: Char): Boolean = x < y
      def <(y: Int): Boolean = x.toInt < y
      def <(y: Long): Boolean = x.toLong < y
      def <(y: Float): Boolean = x.toFloat < y
      def <(y: Double): Boolean = x.toDouble < y

      def <=(y: Byte): Boolean = x.toInt <= y
      def <=(y: Short): Boolean = x.toInt <= y
      def <=(y: Char): Boolean = x <= y
      def <=(y: Int): Boolean = x.toInt <= y
      def <=(y: Long): Boolean = x.toLong <= y
      def <=(y: Float): Boolean = x.toFloat <= y
      def <=(y: Double): Boolean = x.toDouble <= y

      def >(y: Byte): Boolean = x.toInt > y
      def >(y: Short): Boolean = x.toInt > y
      def >(y: Char): Boolean = x > y
      def >(y: Int): Boolean = x.toInt > y
      def >(y: Long): Boolean = x.toLong > y
      def >(y: Float): Boolean = x.toFloat > y
      def >(y: Double): Boolean = x.toDouble > y

      def >=(y: Byte): Boolean = x.toInt >= y
      def >=(y: Short): Boolean = x.toInt >= y
      def >=(y: Char): Boolean = x >= y
      def >=(y: Int): Boolean = x.toInt >= y
      def >=(y: Long): Boolean = x.toLong >= y
      def >=(y: Float): Boolean = x.toFloat >= y
      def >=(y: Double): Boolean = x.toDouble >= y
    }

    /** Convert Char to [[NumericChar]] via compile-time or runtime validation.
      *
      * The inline overload checks Char literals at compile time; the runtime
      * overload validates and throws for non-numeric characters.
      */
    given Conversion[Char, NumericChar] with {
      inline def apply[C <: Char & Singleton](inline x: C): NumericChar =
        inline constValueOpt[C] match {
          case Some(v: Char) =>
            inline if v >= '0' && v <= '9' then
              v.asInstanceOf[NumericChar]
            else
              error("NumericChar conversion requires a numeric Char literal, like NumericChar('8').")
          case None =>
            error("NumericChar conversion requires a numeric Char literal, like NumericChar('8').")
        }

      def apply(x: Char): NumericChar = NumericChar.ensuringValid(x)
    }

    /** Ordering instance based on underlying Char ordering. */
    given Ordering[NumericChar] with {
      def compare(x: NumericChar, y: NumericChar): Int = x.compare(y)
    }
  }

  // String concatenation - must be in a separate extension block to avoid ambiguity
  extension (x: NumericChar) {
    /** Prepends this [[NumericChar]]'s value to a string. */
    def +(s: String): String = x.toString + s
  }

  // Extension methods for arithmetic with primitive types
  extension (x: NumericChar) {
    @annotation.targetName("plusByte")
    def +(y: Byte): Int = x.toInt + y

    @annotation.targetName("plusShort")
    def +(y: Short): Int = x.toInt + y

    @annotation.targetName("plusChar")
    def +(y: Char): Int = x.toInt + y

    @annotation.targetName("plusInt")
    def +(y: Int): Int = x.toInt + y

    @annotation.targetName("plusLong")
    def +(y: Long): Long = x.toLong + y

    @annotation.targetName("plusFloat")
    def +(y: Float): Float = x.toFloat + y

    @annotation.targetName("plusDouble")
    def +(y: Double): Double = x.toDouble + y
  }

  extension (x: NumericChar) {
    @annotation.targetName("minusByte")
    def -(y: Byte): Int = x.toInt - y

    @annotation.targetName("minusShort")
    def -(y: Short): Int = x.toInt - y

    @annotation.targetName("minusChar")
    def -(y: Char): Int = x.toInt - y

    @annotation.targetName("minusInt")
    def -(y: Int): Int = x.toInt - y

    @annotation.targetName("minusLong")
    def -(y: Long): Long = x.toLong - y

    @annotation.targetName("minusFloat")
    def -(y: Float): Float = x.toFloat - y

    @annotation.targetName("minusDouble")
    def -(y: Double): Double = x.toDouble - y
  }

  extension (x: NumericChar) {
    @annotation.targetName("timesByte")
    def *(y: Byte): Int = x.toInt * y

    @annotation.targetName("timesShort")
    def *(y: Short): Int = x.toInt * y

    @annotation.targetName("timesChar")
    def *(y: Char): Int = x.toInt * y

    @annotation.targetName("timesInt")
    def *(y: Int): Int = x.toInt * y

    @annotation.targetName("timesLong")
    def *(y: Long): Long = x.toLong * y

    @annotation.targetName("timesFloat")
    def *(y: Float): Float = x.toFloat * y

    @annotation.targetName("timesDouble")
    def *(y: Double): Double = x.toDouble * y
  }

  extension (x: NumericChar) {
    @annotation.targetName("divByte")
    def /(y: Byte): Int = x.toInt / y

    @annotation.targetName("divShort")
    def /(y: Short): Int = x.toInt / y

    @annotation.targetName("divChar")
    def /(y: Char): Int = x.toInt / y

    @annotation.targetName("divInt")
    def /(y: Int): Int = x.toInt / y

    @annotation.targetName("divLong")
    def /(y: Long): Long = x.toLong / y

    @annotation.targetName("divFloat")
    def /(y: Float): Float = x.toFloat / y

    @annotation.targetName("divDouble")
    def /(y: Double): Double = x.toDouble / y
  }

  extension (x: NumericChar) {
    @annotation.targetName("modByte")
    def %(y: Byte): Int = x.toInt % y

    @annotation.targetName("modShort")
    def %(y: Short): Int = x.toInt % y

    @annotation.targetName("modChar")
    def %(y: Char): Int = x.toInt % y

    @annotation.targetName("modInt")
    def %(y: Int): Int = x.toInt % y

    @annotation.targetName("modLong")
    def %(y: Long): Long = x.toLong % y

    @annotation.targetName("modFloat")
    def %(y: Float): Float = x.toFloat % y

    @annotation.targetName("modDouble")
    def %(y: Double): Double = x.toDouble % y
  }

  // Extension methods for arithmetic with opaque Int-like types (PosInt, PosZInt, NonZeroInt, NegInt, NegZInt)
  extension (x: NumericChar) {
    @annotation.targetName("plusPosInt")
    def +(y: PosInt): Int = x.toInt + y.value

    @annotation.targetName("plusPosZInt")
    def +(y: PosZInt): Int = x.toInt + y.value

    @annotation.targetName("plusNonZeroInt")
    def +(y: NonZeroInt): Int = x.toInt + y.value

    @annotation.targetName("plusNegInt")
    def +(y: NegInt): Int = x.toInt + y.value

    @annotation.targetName("plusNegZInt")
    def +(y: NegZInt): Int = x.toInt + y.value

    @annotation.targetName("minusPosInt")
    def -(y: PosInt): Int = x.toInt - y.value

    @annotation.targetName("minusPosZInt")
    def -(y: PosZInt): Int = x.toInt - y.value

    @annotation.targetName("minusNonZeroInt")
    def -(y: NonZeroInt): Int = x.toInt - y.value

    @annotation.targetName("minusNegInt")
    def -(y: NegInt): Int = x.toInt - y.value

    @annotation.targetName("minusNegZInt")
    def -(y: NegZInt): Int = x.toInt - y.value

    @annotation.targetName("timesPosInt")
    def *(y: PosInt): Int = x.toInt * y.value

    @annotation.targetName("timesPosZInt")
    def *(y: PosZInt): Int = x.toInt * y.value

    @annotation.targetName("timesNonZeroInt")
    def *(y: NonZeroInt): Int = x.toInt * y.value

    @annotation.targetName("timesNegInt")
    def *(y: NegInt): Int = x.toInt * y.value

    @annotation.targetName("timesNegZInt")
    def *(y: NegZInt): Int = x.toInt * y.value

    @annotation.targetName("divPosInt")
    def /(y: PosInt): Int = x.toInt / y.value

    @annotation.targetName("divPosZInt")
    def /(y: PosZInt): Int = x.toInt / y.value

    @annotation.targetName("divNonZeroInt")
    def /(y: NonZeroInt): Int = x.toInt / y.value

    @annotation.targetName("divNegInt")
    def /(y: NegInt): Int = x.toInt / y.value

    @annotation.targetName("divNegZInt")
    def /(y: NegZInt): Int = x.toInt / y.value

    @annotation.targetName("modPosInt")
    def %(y: PosInt): Int = x.toInt % y.value

    @annotation.targetName("modPosZInt")
    def %(y: PosZInt): Int = x.toInt % y.value

    @annotation.targetName("modNonZeroInt")
    def %(y: NonZeroInt): Int = x.toInt % y.value

    @annotation.targetName("modNegInt")
    def %(y: NegInt): Int = x.toInt % y.value

    @annotation.targetName("modNegZInt")
    def %(y: NegZInt): Int = x.toInt % y.value
  }

  // Extension methods for arithmetic with opaque Long-like types (PosLong, PosZLong, NonZeroLong, NegLong, NegZLong)
  extension (x: NumericChar) {
    @annotation.targetName("plusPosLong")
    def +(y: PosLong): Long = x.toLong + y.value

    @annotation.targetName("plusPosZLong")
    def +(y: PosZLong): Long = x.toLong + y.value

    @annotation.targetName("plusNonZeroLong")
    def +(y: NonZeroLong): Long = x.toLong + y.value

    @annotation.targetName("plusNegLong")
    def +(y: NegLong): Long = x.toLong + y.value

    @annotation.targetName("plusNegZLong")
    def +(y: NegZLong): Long = x.toLong + y.value

    @annotation.targetName("minusPosLong")
    def -(y: PosLong): Long = x.toLong - y.value

    @annotation.targetName("minusPosZLong")
    def -(y: PosZLong): Long = x.toLong - y.value

    @annotation.targetName("minusNonZeroLong")
    def -(y: NonZeroLong): Long = x.toLong - y.value

    @annotation.targetName("minusNegLong")
    def -(y: NegLong): Long = x.toLong - y.value

    @annotation.targetName("minusNegZLong")
    def -(y: NegZLong): Long = x.toLong - y.value

    @annotation.targetName("timesPosLong")
    def *(y: PosLong): Long = x.toLong * y.value

    @annotation.targetName("timesPosZLong")
    def *(y: PosZLong): Long = x.toLong * y.value

    @annotation.targetName("timesNonZeroLong")
    def *(y: NonZeroLong): Long = x.toLong * y.value

    @annotation.targetName("timesNegLong")
    def *(y: NegLong): Long = x.toLong * y.value

    @annotation.targetName("timesNegZLong")
    def *(y: NegZLong): Long = x.toLong * y.value

    @annotation.targetName("divPosLong")
    def /(y: PosLong): Long = x.toLong / y.value

    @annotation.targetName("divPosZLong")
    def /(y: PosZLong): Long = x.toLong / y.value

    @annotation.targetName("divNonZeroLong")
    def /(y: NonZeroLong): Long = x.toLong / y.value

    @annotation.targetName("divNegLong")
    def /(y: NegLong): Long = x.toLong / y.value

    @annotation.targetName("divNegZLong")
    def /(y: NegZLong): Long = x.toLong / y.value

    @annotation.targetName("modPosLong")
    def %(y: PosLong): Long = x.toLong % y.value

    @annotation.targetName("modPosZLong")
    def %(y: PosZLong): Long = x.toLong % y.value

    @annotation.targetName("modNonZeroLong")
    def %(y: NonZeroLong): Long = x.toLong % y.value

    @annotation.targetName("modNegLong")
    def %(y: NegLong): Long = x.toLong % y.value

    @annotation.targetName("modNegZLong")
    def %(y: NegZLong): Long = x.toLong % y.value
  }

  // Extension methods for arithmetic with opaque Float-like types
  extension (x: NumericChar) {
    @annotation.targetName("plusPosFloat")
    def +(y: PosFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusNonZeroFloat")
    def +(y: NonZeroFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusNegFloat")
    def +(y: NegFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusPosZFiniteFloat")
    def +(y: PosZFiniteFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusPosZFloat")
    def +(y: PosZFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusNegZFiniteFloat")
    def +(y: NegZFiniteFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusPosFiniteFloat")
    def +(y: PosFiniteFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusNegFiniteFloat")
    def +(y: NegFiniteFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusNonZeroFiniteFloat")
    def +(y: NonZeroFiniteFloat): Float = x.toFloat + y.value

    @annotation.targetName("plusFiniteFloat")
    def +(y: FiniteFloat): Float = x.toFloat + y.value

    @annotation.targetName("minusPosFloat")
    def -(y: PosFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusNonZeroFloat")
    def -(y: NonZeroFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusNegFloat")
    def -(y: NegFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusPosZFiniteFloat")
    def -(y: PosZFiniteFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusPosZFloat")
    def -(y: PosZFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusNegZFiniteFloat")
    def -(y: NegZFiniteFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusPosFiniteFloat")
    def -(y: PosFiniteFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusNegFiniteFloat")
    def -(y: NegFiniteFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusNonZeroFiniteFloat")
    def -(y: NonZeroFiniteFloat): Float = x.toFloat - y.value

    @annotation.targetName("minusFiniteFloat")
    def -(y: FiniteFloat): Float = x.toFloat - y.value

    @annotation.targetName("timesPosFloat")
    def *(y: PosFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesNonZeroFloat")
    def *(y: NonZeroFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesNegFloat")
    def *(y: NegFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesPosZFiniteFloat")
    def *(y: PosZFiniteFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesPosZFloat")
    def *(y: PosZFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesNegZFiniteFloat")
    def *(y: NegZFiniteFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesPosFiniteFloat")
    def *(y: PosFiniteFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesNegFiniteFloat")
    def *(y: NegFiniteFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesNonZeroFiniteFloat")
    def *(y: NonZeroFiniteFloat): Float = x.toFloat * y.value

    @annotation.targetName("timesFiniteFloat")
    def *(y: FiniteFloat): Float = x.toFloat * y.value

    @annotation.targetName("divPosFloat")
    def /(y: PosFloat): Float = x.toFloat / y.value

    @annotation.targetName("divNonZeroFloat")
    def /(y: NonZeroFloat): Float = x.toFloat / y.value

    @annotation.targetName("divNegFloat")
    def /(y: NegFloat): Float = x.toFloat / y.value

    @annotation.targetName("divPosZFiniteFloat")
    def /(y: PosZFiniteFloat): Float = x.toFloat / y.value

    @annotation.targetName("divPosZFloat")
    def /(y: PosZFloat): Float = x.toFloat / y.value

    @annotation.targetName("divNegZFiniteFloat")
    def /(y: NegZFiniteFloat): Float = x.toFloat / y.value

    @annotation.targetName("divPosFiniteFloat")
    def /(y: PosFiniteFloat): Float = x.toFloat / y.value

    @annotation.targetName("divNegFiniteFloat")
    def /(y: NegFiniteFloat): Float = x.toFloat / y.value

    @annotation.targetName("divNonZeroFiniteFloat")
    def /(y: NonZeroFiniteFloat): Float = x.toFloat / y.value

    @annotation.targetName("divFiniteFloat")
    def /(y: FiniteFloat): Float = x.toFloat / y.value

    @annotation.targetName("modPosFloat")
    def %(y: PosFloat): Float = x.toFloat % y.value

    @annotation.targetName("modNonZeroFloat")
    def %(y: NonZeroFloat): Float = x.toFloat % y.value

    @annotation.targetName("modNegFloat")
    def %(y: NegFloat): Float = x.toFloat % y.value

    @annotation.targetName("modPosZFiniteFloat")
    def %(y: PosZFiniteFloat): Float = x.toFloat % y.value

    @annotation.targetName("modPosZFloat")
    def %(y: PosZFloat): Float = x.toFloat % y.value

    @annotation.targetName("modNegZFiniteFloat")
    def %(y: NegZFiniteFloat): Float = x.toFloat % y.value

    @annotation.targetName("modPosFiniteFloat")
    def %(y: PosFiniteFloat): Float = x.toFloat % y.value

    @annotation.targetName("modNegFiniteFloat")
    def %(y: NegFiniteFloat): Float = x.toFloat % y.value

    @annotation.targetName("modNonZeroFiniteFloat")
    def %(y: NonZeroFiniteFloat): Float = x.toFloat % y.value

    @annotation.targetName("modFiniteFloat")
    def %(y: FiniteFloat): Float = x.toFloat % y.value
  }

  // Extension methods for arithmetic with opaque Double-like types
  extension (x: NumericChar) {
    @annotation.targetName("plusPosDouble")
    def +(y: PosDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusNonZeroDouble")
    def +(y: NonZeroDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusNegDouble")
    def +(y: NegDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusPosZFiniteDouble")
    def +(y: PosZFiniteDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusPosZDouble")
    def +(y: PosZDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusNegZFiniteDouble")
    def +(y: NegZFiniteDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusPosFiniteDouble")
    def +(y: PosFiniteDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusNegFiniteDouble")
    def +(y: NegFiniteDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusNonZeroFiniteDouble")
    def +(y: NonZeroFiniteDouble): Double = x.toDouble + y.value

    @annotation.targetName("plusFiniteDouble")
    def +(y: FiniteDouble): Double = x.toDouble + y.value

    @annotation.targetName("minusPosDouble")
    def -(y: PosDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusNonZeroDouble")
    def -(y: NonZeroDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusNegDouble")
    def -(y: NegDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusPosZFiniteDouble")
    def -(y: PosZFiniteDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusPosZDouble")
    def -(y: PosZDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusNegZFiniteDouble")
    def -(y: NegZFiniteDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusPosFiniteDouble")
    def -(y: PosFiniteDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusNegFiniteDouble")
    def -(y: NegFiniteDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusNonZeroFiniteDouble")
    def -(y: NonZeroFiniteDouble): Double = x.toDouble - y.value

    @annotation.targetName("minusFiniteDouble")
    def -(y: FiniteDouble): Double = x.toDouble - y.value

    @annotation.targetName("timesPosDouble")
    def *(y: PosDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesNonZeroDouble")
    def *(y: NonZeroDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesNegDouble")
    def *(y: NegDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesPosZFiniteDouble")
    def *(y: PosZFiniteDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesPosZDouble")
    def *(y: PosZDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesNegZFiniteDouble")
    def *(y: NegZFiniteDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesPosFiniteDouble")
    def *(y: PosFiniteDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesNegFiniteDouble")
    def *(y: NegFiniteDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesNonZeroFiniteDouble")
    def *(y: NonZeroFiniteDouble): Double = x.toDouble * y.value

    @annotation.targetName("timesFiniteDouble")
    def *(y: FiniteDouble): Double = x.toDouble * y.value

    @annotation.targetName("divPosDouble")
    def /(y: PosDouble): Double = x.toDouble / y.value

    @annotation.targetName("divNonZeroDouble")
    def /(y: NonZeroDouble): Double = x.toDouble / y.value

    @annotation.targetName("divNegDouble")
    def /(y: NegDouble): Double = x.toDouble / y.value

    @annotation.targetName("divPosZFiniteDouble")
    def /(y: PosZFiniteDouble): Double = x.toDouble / y.value

    @annotation.targetName("divPosZDouble")
    def /(y: PosZDouble): Double = x.toDouble / y.value

    @annotation.targetName("divNegZFiniteDouble")
    def /(y: NegZFiniteDouble): Double = x.toDouble / y.value

    @annotation.targetName("divPosFiniteDouble")
    def /(y: PosFiniteDouble): Double = x.toDouble / y.value

    @annotation.targetName("divNegFiniteDouble")
    def /(y: NegFiniteDouble): Double = x.toDouble / y.value

    @annotation.targetName("divNonZeroFiniteDouble")
    def /(y: NonZeroFiniteDouble): Double = x.toDouble / y.value

    @annotation.targetName("divFiniteDouble")
    def /(y: FiniteDouble): Double = x.toDouble / y.value

    @annotation.targetName("modPosDouble")
    def %(y: PosDouble): Double = x.toDouble % y.value

    @annotation.targetName("modNonZeroDouble")
    def %(y: NonZeroDouble): Double = x.toDouble % y.value

    @annotation.targetName("modNegDouble")
    def %(y: NegDouble): Double = x.toDouble % y.value

    @annotation.targetName("modPosZFiniteDouble")
    def %(y: PosZFiniteDouble): Double = x.toDouble % y.value

    @annotation.targetName("modPosZDouble")
    def %(y: PosZDouble): Double = x.toDouble % y.value

    @annotation.targetName("modNegZFiniteDouble")
    def %(y: NegZFiniteDouble): Double = x.toDouble % y.value

    @annotation.targetName("modPosFiniteDouble")
    def %(y: PosFiniteDouble): Double = x.toDouble % y.value

    @annotation.targetName("modNegFiniteDouble")
    def %(y: NegFiniteDouble): Double = x.toDouble % y.value

    @annotation.targetName("modNonZeroFiniteDouble")
    def %(y: NonZeroFiniteDouble): Double = x.toDouble % y.value

    @annotation.targetName("modFiniteDouble")
    def %(y: FiniteDouble): Double = x.toDouble % y.value
  }

  // Widening conversions to compatible AnyVal targets
  // We provide direct conversions to specific types. Subtype widening conversions
  // from PosFloat to PosZFloat, PosZFiniteFloat to PosZFloat, etc. are defined
  // in PosFloats.scala and PosDoubles.scala.

  /** Convert [[NumericChar]] to [[PosInt]]. */
  given posIntConversion: Conversion[NumericChar, PosInt] = 
    (x: NumericChar) => PosInt.ensuringValid(x.toInt)

  /** Convert [[NumericChar]] to [[PosLong]]. */
  given posLongConversion: Conversion[NumericChar, PosLong] = 
    (x: NumericChar) => PosLong.ensuringValid(x.toLong)

  /** Convert [[NumericChar]] to [[PosZInt]]. */
  given posZIntConversion: Conversion[NumericChar, PosZInt] = 
    (x: NumericChar) => PosZInt.ensuringValid(x.toInt)

  /** Convert [[NumericChar]] to [[PosZLong]]. */
  given posZLongConversion: Conversion[NumericChar, PosZLong] = 
    (x: NumericChar) => PosZLong.ensuringValid(x.toLong)

  /** Convert [[NumericChar]] to [[PosFloat]]. */
  given posFloatConversion: Conversion[NumericChar, PosFloat] = 
    (x: NumericChar) => PosFloat.ensuringValid(x.toFloat)

  /** Convert [[NumericChar]] to [[PosFiniteFloat]]. */
  given posFiniteFloatConversion: Conversion[NumericChar, PosFiniteFloat] = 
    (x: NumericChar) => PosFiniteFloat.ensuringValid(x.toFloat)

  /** Convert [[NumericChar]] to [[PosDouble]]. */
  given posDoubleConversion: Conversion[NumericChar, PosDouble] = 
    (x: NumericChar) => PosDouble.ensuringValid(x.toDouble)

  /** Convert [[NumericChar]] to [[PosFiniteDouble]]. */
  given posFiniteDoubleConversion: Conversion[NumericChar, PosFiniteDouble] = 
    (x: NumericChar) => PosFiniteDouble.ensuringValid(x.toDouble)

  /** Convert [[NumericChar]] to [[FiniteFloat]]. */
  given finiteFloatConversion: Conversion[NumericChar, FiniteFloat] = 
    (x: NumericChar) => FiniteFloat.ensuringValid(x.toFloat)

  /** Convert [[NumericChar]] to [[FiniteDouble]]. */
  given finiteDoubleConversion: Conversion[NumericChar, FiniteDouble] = 
    (x: NumericChar) => FiniteDouble.ensuringValid(x.toDouble)

  /** Convert Char to [[NumericChar]] via compile-time or runtime validation.
    *
    * The inline overload checks Char literals at compile time; the runtime
    * overload validates and throws for non-numeric characters.
    */
  given Conversion[Char, NumericChar] with {
    inline def apply[C <: Char & Singleton](inline x: C): NumericChar =
      inline constValueOpt[C] match {
        case Some(v: Char) =>
          inline if v >= '0' && v <= '9' then
            v.asInstanceOf[NumericChar]
          else
            error("NumericChar conversion requires a numeric Char literal, like NumericChar('8').")
        case None =>
          error("NumericChar conversion requires a numeric Char literal, like NumericChar('8').")
      }

    def apply(x: Char): NumericChar = NumericChar.ensuringValid(x)
  }

  /** Ordering instance based on underlying Char ordering. */
  given Ordering[NumericChar] with {
    def compare(x: NumericChar, y: NumericChar): Int = x.compare(y)
  }

  /** Opaque type representing a non-empty String consisting only of digit
    * characters ('0' to '9').
    *
    * Instances of this type are guaranteed to be non-empty and contain only
    * numeric digit characters. Use the compile-time apply method to construct
    * instances from string literals, or the runtime factory methods for values
    * known only at runtime.
    */
  opaque type NumericString = String

  /** Companion object for [[NumericString]] with construction and validation helpers.
    *
    * Provides factory methods for compile-time-checked construction from String
    * literals, runtime validation helpers, given conversions, and extension
    * methods for common string operations.
    */
  object NumericString {

    /** Compile-time factory for creating a [[NumericString]] from a String literal.
      *
      * Rejects empty strings and strings containing non-digit characters at
      * compile time.
      */
    transparent inline def apply(inline s: String): NumericString =
      ${ NumericString.applyImpl('s) }

    private def applyImpl(s: Expr[String])(using Quotes): Expr[NumericString] = {
      import quotes.reflect.*
      val v = s.valueOrAbort
      // At this point v is guaranteed to be a non-empty digit-only string
      // since the error case was handled above.
      // Cast the string literal to the opaque type.
      s.asExprOf[NumericString]
    }

    /** Construct a [[NumericString]] from a runtime String if it is numeric.
      *
      * @param s the String to validate
      * @return Some(NumericString) if s is non-empty and contains only digits, else None
      */
    def from(s: String): Option[NumericString] =
      if (isNumericString(s)) Some(s.asInstanceOf[NumericString]) else None

    /** Validate and return the given String as [[NumericString]].
      *
      * @throws AssertionError if s is empty or contains non-digit characters
      */
    def ensuringValid(s: String): NumericString =
      if (isNumericString(s))
        s.asInstanceOf[NumericString]
      else
        throw new AssertionError(Resources.invalidNumericString)

    /** Runtime factory that returns Success for valid input, Failure otherwise.
      *
      * @param value the String to validate
      * @return Success(NumericString) if value is non-empty and contains only digits,
      *   else Failure(AssertionError)
      */
    def tryingValid(value: String): Try[NumericString] =
      if (isNumericString(value))
        Success(value.asInstanceOf[NumericString])
      else
        Failure(new AssertionError(Resources.invalidNumericString))

    /** Predicate indicating whether the given String is valid for [[NumericString]].
      *
      * @param value the String to validate
      * @return true if value is non-empty and contains only digit characters, else false
      */
    def isValid(value: String): Boolean = isNumericString(value)

    private[opaquetypes] def isNumericString(s: String): Boolean =
      !s.isEmpty && s.forall(c => c >= '0' && c <= '9')

    /** Validate a value and return Pass, else Fail(f(value)).
      *
      * @param value the String to validate
      * @param f function to produce an error value when validation fails
      * @return Pass if value is a valid NumericString, else Fail(f(value))
      */
    def passOrElse[E](value: String)(f: String => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(NumericString), else Bad(f(value)).
      *
      * @param value the String to validate
      * @param f function to produce an error value when validation fails
      * @return Good(NumericString) if value is valid, else Bad(f(value))
      */
    def goodOrElse[B](value: String)(f: String => B): NumericString Or B =
      if (isValid(value)) Good(value.asInstanceOf[NumericString]) else Bad(f(value))

    /** Validate a value and return Right(NumericString), else Left(f(value)).
      *
      * @param value the String to validate
      * @param f function to produce an error value when validation fails
      * @return Right(NumericString) if value is valid, else Left(f(value))
      */
    def rightOrElse[L](value: String)(f: String => L): Either[L, NumericString] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid.
      *
      * @param value the String to validate
      * @param default the NumericString to return if value is not valid
      * @return value as NumericString if valid, else default
      */
    def fromOrElse(value: String, default: => NumericString): NumericString =
      if (isValid(value)) value.asInstanceOf[NumericString] else default

    /** Smallest valid NumericString value (which is "0"). */
    val MinValue: NumericString = "0".asInstanceOf[NumericString]

    /** Largest valid NumericString value (which is "9"). */
    val MaxValue: NumericString = "9".asInstanceOf[NumericString]

    /** Convert [[NumericString]] to [[String]] for interoperability. */
    given numericStringToStringConversion: Conversion[NumericString, String] with {
      def apply(x: NumericString): String = x
    }

    /** Ordering instance based on underlying String ordering. */
    given Ordering[NumericString] with {
      def compare(x: NumericString, y: NumericString): Int = x.compareTo(y)
    }
  }
  
  extension (x: NumericString) {
    /** Return the underlying String value. */
    def value: String = x

    /** Length of this NumericString. */
    def length: Int = x.length

    /** Character at the given index. */
    def apply(idx: Int): Char = (x: String).charAt(idx)

    /** Concatenate with another NumericString, returning NumericString. */
    @targetName("plusPlusNumericString")  
    def ++(other: NumericString): NumericString = {
      val s: String = x
      val o: String = other
      s.concat(o)
    }

    def ++(other: String): String =
      // Cast to String explicitly so the compiler uses String's ++,
      // not a recursive call to this very extension
      (x: String).concat(other: String)

    /** Extract a substring as NumericString (caller must ensure validity). */
    def slice(from: Int, until: Int): NumericString = 
      (x: String).substring(from, until)

    /** Reverse the characters, returning a NumericString. */
    def reverse: NumericString = new scala.collection.immutable.StringOps(x: String).reverse
  }
}
