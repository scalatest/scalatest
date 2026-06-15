/*
 * Copyright 2001-2025 Artima, Inc.
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
import scala.util.{Try, Success, Failure}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}

object NonZeroFloats {

  // Import opaque types we need for extension methods
  import PosLongs.{PosLong, PosZLong}
  import PosInts.{PosInt, PosZInt}
  import NegInts.{NegInt, NegZInt}
  import NonZeroInts.NonZeroInt
  import NegLongs.{NegLong, NegZLong}
  import PosFloats.{PosFloat, PosZFloat, PosZFiniteFloat, PosFiniteFloat}
  import NegFloats.{NegFloat, NegZFloat, NegZFiniteFloat, NegFiniteFloat}
  import NonZeroDoubles.{NonZeroDouble, NonZeroFiniteDouble}
  import PosDoubles.{PosDouble, PosZDouble, PosZFiniteDouble, PosFiniteDouble}
  import NegDoubles.{NegDouble, NegZDouble, NegZFiniteDouble, NegFiniteDouble}
  import Finites.{FiniteFloat, FiniteDouble}

  opaque type NonZeroFloat = Float

  object NonZeroFloat {
    /** Compile-time factory for creating a [[NonZeroFloat]] from a float literal.
      *
      * Rejects zero literals at compile time.
      */
    inline def apply[F <: Float & Singleton](inline f: F): NonZeroFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v != 0.0f then
            v.asInstanceOf[NonZeroFloat]
          else
            error("NonZeroFloat cannot be instantiated with zero")
        case None =>
          error("NonZeroFloat.apply requires a float literal")
      }

    /** Compile-time factory for creating a [[NonZeroFloat]] from an integer literal. */
    inline def apply[I <: Int & Singleton](inline i: I): NonZeroFloat =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v != 0 then
            v.toFloat.asInstanceOf[NonZeroFloat]
          else
            error("NonZeroFloat cannot be instantiated with zero")
        case None =>
          error("NonZeroFloat.apply requires an integer or float literal")
      }

    def ensuringValid(f: Float): NonZeroFloat =
      if (f == 0.0f)
        throw new AssertionError(Resources.invalidNonZeroFloat)
      else f
    def from(f: Float): Option[NonZeroFloat] =
      if (f == 0.0f) None else Some(f)

    /** Implicitly widens a [[NonZeroFloat]] to a plain <code>Float</code>. */
    given Conversion[NonZeroFloat, Float] with {
      def apply(x: NonZeroFloat): Float = x
    }

    /** Convert Float to [[NonZeroFloat]] via compile-time or runtime validation. */
    given Conversion[Float, NonZeroFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): NonZeroFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v == 0.0f then
              error("NonZeroFloat cannot be instantiated with zero")
            else
              v.asInstanceOf[NonZeroFloat]
          case None =>
            error("NonZeroFloat conversion requires a float literal")
        }

      def apply(x: Float): NonZeroFloat = NonZeroFloat.ensuringValid(x)
    }

    /** Widen [[NonZeroFloat]] to [[NonZeroDouble]]. */
    given Conversion[NonZeroFloat, NonZeroDoubles.NonZeroDouble] with {
      def apply(x: NonZeroFloat): NonZeroDoubles.NonZeroDouble = NonZeroDoubles.NonZeroDouble.ensuringValid(x.toDouble)
    }
  }

  // Extension methods at NonZeroFloats level for NonZeroFloat
  extension (x: NonZeroFloat) {
    /** Return the underlying Float value. */
    def value: Float = x

    /** Return the underlying Float value. */
    def toFloat: Float = x

    /** Return the underlying Double value. */
    def toDouble: Double = x.toDouble

    /** Unary plus returns this value unchanged. */
    def unary_+ : NonZeroFloat = x

    /** Numeric negation (returns Double to handle infinity). */
    def unary_- : Double = -x

    /** Greater of this and that value. */
    def max(that: NonZeroFloat): NonZeroFloat = if (x >= that) x else that

    /** Lesser of this and that value. */
    def min(that: NonZeroFloat): NonZeroFloat = if (x <= that) x else that

    // Arithmetic operations - return appropriate types
    @annotation.targetName("plusByte")
    def +(y: Byte): Float = x + y
    @annotation.targetName("plusShort")
    def +(y: Short): Float = x + y
    @annotation.targetName("plusChar")
    def +(y: Char): Float = x + y
    @annotation.targetName("plusInt")
    def +(y: Int): Float = x + y
    @annotation.targetName("plusLong")
    def +(y: Long): Float = x + y
    @annotation.targetName("plusFloat")
    def +(y: Float): Float = x + y
    @annotation.targetName("plusDouble")
    def +(y: Double): Double = x + y

    @annotation.targetName("minusByte")
    def -(y: Byte): Float = x - y
    @annotation.targetName("minusShort")
    def -(y: Short): Float = x - y
    @annotation.targetName("minusChar")
    def -(y: Char): Float = x - y
    @annotation.targetName("minusInt")
    def -(y: Int): Float = x - y
    @annotation.targetName("minusLong")
    def -(y: Long): Float = x - y
    @annotation.targetName("minusFloat")
    def -(y: Float): Float = x - y
    @annotation.targetName("minusDouble")
    def -(y: Double): Double = x - y

    @annotation.targetName("timesByte")
    def *(y: Byte): Float = x * y
    @annotation.targetName("timesShort")
    def *(y: Short): Float = x * y
    @annotation.targetName("timesChar")
    def *(y: Char): Float = x * y
    @annotation.targetName("timesInt")
    def *(y: Int): Float = x * y
    @annotation.targetName("timesLong")
    def *(y: Long): Float = x * y
    @annotation.targetName("timesFloat")
    def *(y: Float): Float = x * y
    @annotation.targetName("timesDouble")
    def *(y: Double): Double = x * y

    @annotation.targetName("divByte")
    def /(y: Byte): Float = x / y
    @annotation.targetName("divShort")
    def /(y: Short): Float = x / y
    @annotation.targetName("divChar")
    def /(y: Char): Float = x / y
    @annotation.targetName("divInt")
    def /(y: Int): Float = x / y
    @annotation.targetName("divLong")
    def /(y: Long): Float = x / y
    @annotation.targetName("divFloat")
    def /(y: Float): Float = x / y
    @annotation.targetName("divDouble")
    def /(y: Double): Double = x / y

    @annotation.targetName("modByte")
    def %(y: Byte): Float = x % y
    @annotation.targetName("modShort")
    def %(y: Short): Float = x % y
    @annotation.targetName("modChar")
    def %(y: Char): Float = x % y
    @annotation.targetName("modInt")
    def %(y: Int): Float = x % y
    @annotation.targetName("modLong")
    def %(y: Long): Float = x % y
    @annotation.targetName("modFloat")
    def %(y: Float): Float = x % y
    @annotation.targetName("modDouble")
    def %(y: Double): Double = x % y

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
  }

  // Extension methods for arithmetic operations with other opaque Int types
  // These use @targetName to avoid JVM erasure conflicts with primitive type methods

  extension (x: NonZeroFloat) {
    @annotation.targetName("plusPosInt")
    def +(y: PosInts.PosInt): Float = x + y.value
    @annotation.targetName("plusPosZInt")
    def +(y: PosInts.PosZInt): Float = x + y.value
    @annotation.targetName("plusNonZeroInt")
    def +(y: NonZeroInts.NonZeroInt): Float = x + y.value
    @annotation.targetName("plusNegInt")
    def +(y: NegInts.NegInt): Float = x + y.value
    @annotation.targetName("plusNegZInt")
    def +(y: NegInts.NegZInt): Float = x + y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("plusPosLong")
    def +(y: PosLongs.PosLong): Float = x + y.value
    @annotation.targetName("plusPosZLong")
    def +(y: PosLongs.PosZLong): Float = x + y.value
    @annotation.targetName("plusNonZeroLong")
    def +(y: NonZeroLongs.NonZeroLong): Float = x + y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("plusPosFloat")
    def +(y: PosFloats.PosFloat): Float = x + y.value
    @annotation.targetName("plusPosZFloat")
    def +(y: PosFloats.PosZFloat): Float = x + y.value
    @annotation.targetName("plusPosZFiniteFloat")
    def +(y: PosFloats.PosZFiniteFloat): Float = x + y.value
    @annotation.targetName("plusPosFiniteFloat")
    def +(y: PosFloats.PosFiniteFloat): Float = x + y.value
    @annotation.targetName("plusFiniteFloat")
    def +(y: Finites.FiniteFloat): Float = x + y.value
    @annotation.targetName("plusNonZeroFloat")
    def +(y: NonZeroFloat): Float = x + y
    @annotation.targetName("plusNonZeroFiniteFloat")
    def +(y: NonZeroFiniteFloat): Float = x + y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("plusNegFloat")
    def +(y: NegFloats.NegFloat): Float = x + y.value
    @annotation.targetName("plusNegZFloat")
    def +(y: NegFloats.NegZFloat): Float = x + y.value
    @annotation.targetName("plusNegZFiniteFloat")
    def +(y: NegFloats.NegZFiniteFloat): Float = x + y.value
    @annotation.targetName("plusNegFiniteFloat")
    def +(y: NegFloats.NegFiniteFloat): Float = x + y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("plusPosDouble")
    def +(y: PosDoubles.PosDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusPosZDouble")
    def +(y: PosDoubles.PosZDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusPosZFiniteDouble")
    def +(y: PosDoubles.PosZFiniteDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusPosFiniteDouble")
    def +(y: PosDoubles.PosFiniteDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusFiniteDouble")
    def +(y: Finites.FiniteDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusNonZeroDouble")
    def +(y: NonZeroDoubles.NonZeroDouble): Double = x.toDouble + y
    @annotation.targetName("plusNonZeroFiniteDouble")
    def +(y: NonZeroDoubles.NonZeroFiniteDouble): Double = x.toDouble + y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("plusNegDouble")
    def +(y: NegDoubles.NegDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusNegZDouble")
    def +(y: NegDoubles.NegZDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusNegZFiniteDouble")
    def +(y: NegDoubles.NegZFiniteDouble): Double = x.toDouble + y.value
    @annotation.targetName("plusNegFiniteDouble")
    def +(y: NegDoubles.NegFiniteDouble): Double = x.toDouble + y.value
  }

  // Subtraction methods
  extension (x: NonZeroFloat) {
    @annotation.targetName("minusPosInt")
    def -(y: PosInts.PosInt): Float = x - y.value
    @annotation.targetName("minusPosZInt")
    def -(y: PosInts.PosZInt): Float = x - y.value
    @annotation.targetName("minusNonZeroInt")
    def -(y: NonZeroInts.NonZeroInt): Float = x - y.value
    @annotation.targetName("minusNegInt")
    def -(y: NegInts.NegInt): Float = x - y.value
    @annotation.targetName("minusNegZInt")
    def -(y: NegInts.NegZInt): Float = x - y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("minusPosLong")
    def -(y: PosLongs.PosLong): Float = x - y.value
    @annotation.targetName("minusPosZLong")
    def -(y: PosLongs.PosZLong): Float = x - y.value
    @annotation.targetName("minusNonZeroLong")
    def -(y: NonZeroLongs.NonZeroLong): Float = x - y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("minusPosFloat")
    def -(y: PosFloats.PosFloat): Float = x - y.value
    @annotation.targetName("minusPosZFloat")
    def -(y: PosFloats.PosZFloat): Float = x - y.value
    @annotation.targetName("minusPosZFiniteFloat")
    def -(y: PosFloats.PosZFiniteFloat): Float = x - y.value
    @annotation.targetName("minusPosFiniteFloat")
    def -(y: PosFloats.PosFiniteFloat): Float = x - y.value
    @annotation.targetName("minusFiniteFloat")
    def -(y: Finites.FiniteFloat): Float = x - y.value
    @annotation.targetName("minusNonZeroFloat")
    def -(y: NonZeroFloat): Float = x - y
    @annotation.targetName("minusNonZeroFiniteFloat")
    def -(y: NonZeroFiniteFloat): Float = x - y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("minusNegFloat")
    def -(y: NegFloats.NegFloat): Float = x - y.value
    @annotation.targetName("minusNegZFloat")
    def -(y: NegFloats.NegZFloat): Float = x - y.value
    @annotation.targetName("minusNegZFiniteFloat")
    def -(y: NegFloats.NegZFiniteFloat): Float = x - y.value
    @annotation.targetName("minusNegFiniteFloat")
    def -(y: NegFloats.NegFiniteFloat): Float = x - y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("minusPosDouble")
    def -(y: PosDoubles.PosDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusPosZDouble")
    def -(y: PosDoubles.PosZDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusPosZFiniteDouble")
    def -(y: PosDoubles.PosZFiniteDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusPosFiniteDouble")
    def -(y: PosDoubles.PosFiniteDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusFiniteDouble")
    def -(y: Finites.FiniteDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusNonZeroDouble")
    def -(y: NonZeroDoubles.NonZeroDouble): Double = x.toDouble - y
    @annotation.targetName("minusNonZeroFiniteDouble")
    def -(y: NonZeroDoubles.NonZeroFiniteDouble): Double = x.toDouble - y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("minusNegDouble")
    def -(y: NegDoubles.NegDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusNegZDouble")
    def -(y: NegDoubles.NegZDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusNegZFiniteDouble")
    def -(y: NegDoubles.NegZFiniteDouble): Double = x.toDouble - y.value
    @annotation.targetName("minusNegFiniteDouble")
    def -(y: NegDoubles.NegFiniteDouble): Double = x.toDouble - y.value
  }

  // Multiplication methods
  extension (x: NonZeroFloat) {
    @annotation.targetName("timesPosInt")
    def *(y: PosInts.PosInt): Float = x * y.value
    @annotation.targetName("timesPosZInt")
    def *(y: PosInts.PosZInt): Float = x * y.value
    @annotation.targetName("timesNonZeroInt")
    def *(y: NonZeroInts.NonZeroInt): Float = x * y.value
    @annotation.targetName("timesNegInt")
    def *(y: NegInts.NegInt): Float = x * y.value
    @annotation.targetName("timesNegZInt")
    def *(y: NegInts.NegZInt): Float = x * y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("timesPosLong")
    def *(y: PosLongs.PosLong): Float = x * y.value
    @annotation.targetName("timesPosZLong")
    def *(y: PosLongs.PosZLong): Float = x * y.value
    @annotation.targetName("timesNonZeroLong")
    def *(y: NonZeroLongs.NonZeroLong): Float = x * y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("timesPosFloat")
    def *(y: PosFloats.PosFloat): Float = x * y.value
    @annotation.targetName("timesPosZFloat")
    def *(y: PosFloats.PosZFloat): Float = x * y.value
    @annotation.targetName("timesPosZFiniteFloat")
    def *(y: PosFloats.PosZFiniteFloat): Float = x * y.value
    @annotation.targetName("timesPosFiniteFloat")
    def *(y: PosFloats.PosFiniteFloat): Float = x * y.value
    @annotation.targetName("timesFiniteFloat")
    def *(y: Finites.FiniteFloat): Float = x * y.value
    @annotation.targetName("timesNonZeroFloat")
    def *(y: NonZeroFloat): Float = x * y
    @annotation.targetName("timesNonZeroFiniteFloat")
    def *(y: NonZeroFiniteFloat): Float = x * y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("timesNegFloat")
    def *(y: NegFloats.NegFloat): Float = x * y.value
    @annotation.targetName("timesNegZFloat")
    def *(y: NegFloats.NegZFloat): Float = x * y.value
    @annotation.targetName("timesNegZFiniteFloat")
    def *(y: NegFloats.NegZFiniteFloat): Float = x * y.value
    @annotation.targetName("timesNegFiniteFloat")
    def *(y: NegFloats.NegFiniteFloat): Float = x * y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("timesPosDouble")
    def *(y: PosDoubles.PosDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesPosZDouble")
    def *(y: PosDoubles.PosZDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesPosZFiniteDouble")
    def *(y: PosDoubles.PosZFiniteDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesPosFiniteDouble")
    def *(y: PosDoubles.PosFiniteDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesFiniteDouble")
    def *(y: Finites.FiniteDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesNonZeroDouble")
    def *(y: NonZeroDoubles.NonZeroDouble): Double = x.toDouble * y
    @annotation.targetName("timesNonZeroFiniteDouble")
    def *(y: NonZeroDoubles.NonZeroFiniteDouble): Double = x.toDouble * y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("timesNegDouble")
    def *(y: NegDoubles.NegDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesNegZDouble")
    def *(y: NegDoubles.NegZDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesNegZFiniteDouble")
    def *(y: NegDoubles.NegZFiniteDouble): Double = x.toDouble * y.value
    @annotation.targetName("timesNegFiniteDouble")
    def *(y: NegDoubles.NegFiniteDouble): Double = x.toDouble * y.value
  }

  // Division methods
  extension (x: NonZeroFloat) {
    @annotation.targetName("divPosInt")
    def /(y: PosInts.PosInt): Float = x / y.value
    @annotation.targetName("divPosZInt")
    def /(y: PosInts.PosZInt): Float = x / y.value
    @annotation.targetName("divNonZeroInt")
    def /(y: NonZeroInts.NonZeroInt): Float = x / y.value
    @annotation.targetName("divNegInt")
    def /(y: NegInts.NegInt): Float = x / y.value
    @annotation.targetName("divNegZInt")
    def /(y: NegInts.NegZInt): Float = x / y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("divPosLong")
    def /(y: PosLongs.PosLong): Float = x / y.value
    @annotation.targetName("divPosZLong")
    def /(y: PosLongs.PosZLong): Float = x / y.value
    @annotation.targetName("divNonZeroLong")
    def /(y: NonZeroLongs.NonZeroLong): Float = x / y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("divPosFloat")
    def /(y: PosFloats.PosFloat): Float = x / y.value
    @annotation.targetName("divPosZFloat")
    def /(y: PosFloats.PosZFloat): Float = x / y.value
    @annotation.targetName("divPosZFiniteFloat")
    def /(y: PosFloats.PosZFiniteFloat): Float = x / y.value
    @annotation.targetName("divPosFiniteFloat")
    def /(y: PosFloats.PosFiniteFloat): Float = x / y.value
    @annotation.targetName("divFiniteFloat")
    def /(y: Finites.FiniteFloat): Float = x / y.value
    @annotation.targetName("divNonZeroFloat")
    def /(y: NonZeroFloat): Float = x / y
    @annotation.targetName("divNonZeroFiniteFloat")
    def /(y: NonZeroFiniteFloat): Float = x / y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("divNegFloat")
    def /(y: NegFloats.NegFloat): Float = x / y.value
    @annotation.targetName("divNegZFloat")
    def /(y: NegFloats.NegZFloat): Float = x / y.value
    @annotation.targetName("divNegZFiniteFloat")
    def /(y: NegFloats.NegZFiniteFloat): Float = x / y.value
    @annotation.targetName("divNegFiniteFloat")
    def /(y: NegFloats.NegFiniteFloat): Float = x / y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("divPosDouble")
    def /(y: PosDoubles.PosDouble): Double = x.toDouble / y.value
    @annotation.targetName("divPosZDouble")
    def /(y: PosDoubles.PosZDouble): Double = x.toDouble / y.value
    @annotation.targetName("divPosZFiniteDouble")
    def /(y: PosDoubles.PosZFiniteDouble): Double = x.toDouble / y.value
    @annotation.targetName("divPosFiniteDouble")
    def /(y: PosDoubles.PosFiniteDouble): Double = x.toDouble / y.value
    @annotation.targetName("divFiniteDouble")
    def /(y: Finites.FiniteDouble): Double = x.toDouble / y.value
    @annotation.targetName("divNonZeroDouble")
    def /(y: NonZeroDoubles.NonZeroDouble): Double = x.toDouble / y
    @annotation.targetName("divNonZeroFiniteDouble")
    def /(y: NonZeroDoubles.NonZeroFiniteDouble): Double = x.toDouble / y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("divNegDouble")
    def /(y: NegDoubles.NegDouble): Double = x.toDouble / y.value
    @annotation.targetName("divNegZDouble")
    def /(y: NegDoubles.NegZDouble): Double = x.toDouble / y.value
    @annotation.targetName("divNegZFiniteDouble")
    def /(y: NegDoubles.NegZFiniteDouble): Double = x.toDouble / y.value
    @annotation.targetName("divNegFiniteDouble")
    def /(y: NegDoubles.NegFiniteDouble): Double = x.toDouble / y.value
  }

  // Modulo methods
  extension (x: NonZeroFloat) {
    @annotation.targetName("modPosInt")
    def %(y: PosInts.PosInt): Float = x % y.value
    @annotation.targetName("modPosZInt")
    def %(y: PosInts.PosZInt): Float = x % y.value
    @annotation.targetName("modNonZeroInt")
    def %(y: NonZeroInts.NonZeroInt): Float = x % y.value
    @annotation.targetName("modNegInt")
    def %(y: NegInts.NegInt): Float = x % y.value
    @annotation.targetName("modNegZInt")
    def %(y: NegInts.NegZInt): Float = x % y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("modPosLong")
    def %(y: PosLongs.PosLong): Float = x % y.value
    @annotation.targetName("modPosZLong")
    def %(y: PosLongs.PosZLong): Float = x % y.value
    @annotation.targetName("modNonZeroLong")
    def %(y: NonZeroLongs.NonZeroLong): Float = x % y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("modPosFloat")
    def %(y: PosFloats.PosFloat): Float = x % y.value
    @annotation.targetName("modPosZFloat")
    def %(y: PosFloats.PosZFloat): Float = x % y.value
    @annotation.targetName("modPosZFiniteFloat")
    def %(y: PosFloats.PosZFiniteFloat): Float = x % y.value
    @annotation.targetName("modPosFiniteFloat")
    def %(y: PosFloats.PosFiniteFloat): Float = x % y.value
    @annotation.targetName("modFiniteFloat")
    def %(y: Finites.FiniteFloat): Float = x % y.value
    @annotation.targetName("modNonZeroFloat")
    def %(y: NonZeroFloat): Float = x % y
    @annotation.targetName("modNonZeroFiniteFloat")
    def %(y: NonZeroFiniteFloat): Float = x % y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("modNegFloat")
    def %(y: NegFloats.NegFloat): Float = x % y.value
    @annotation.targetName("modNegZFloat")
    def %(y: NegFloats.NegZFloat): Float = x % y.value
    @annotation.targetName("modNegZFiniteFloat")
    def %(y: NegFloats.NegZFiniteFloat): Float = x % y.value
    @annotation.targetName("modNegFiniteFloat")
    def %(y: NegFloats.NegFiniteFloat): Float = x % y.value
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("modPosDouble")
    def %(y: PosDoubles.PosDouble): Double = x.toDouble % y.value
    @annotation.targetName("modPosZDouble")
    def %(y: PosDoubles.PosZDouble): Double = x.toDouble % y.value
    @annotation.targetName("modPosZFiniteDouble")
    def %(y: PosDoubles.PosZFiniteDouble): Double = x.toDouble % y.value
    @annotation.targetName("modPosFiniteDouble")
    def %(y: PosDoubles.PosFiniteDouble): Double = x.toDouble % y.value
    @annotation.targetName("modFiniteDouble")
    def %(y: Finites.FiniteDouble): Double = x.toDouble % y.value
    @annotation.targetName("modNonZeroDouble")
    def %(y: NonZeroDoubles.NonZeroDouble): Double = x.toDouble % y
    @annotation.targetName("modNonZeroFiniteDouble")
    def %(y: NonZeroDoubles.NonZeroFiniteDouble): Double = x.toDouble % y
  }

  extension (x: NonZeroFloat) {
    @annotation.targetName("modNegDouble")
    def %(y: NegDoubles.NegDouble): Double = x.toDouble % y.value
    @annotation.targetName("modNegZDouble")
    def %(y: NegDoubles.NegZDouble): Double = x.toDouble % y.value
    @annotation.targetName("modNegZFiniteDouble")
    def %(y: NegDoubles.NegZFiniteDouble): Double = x.toDouble % y.value
    @annotation.targetName("modNegFiniteDouble")
    def %(y: NegDoubles.NegFiniteDouble): Double = x.toDouble % y.value
  }

  /** Opaque type representing a non-zero, finite <code>Float</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy both <code>!= 0.0f</code>
    * and <code>isFinite</code> (i.e. neither <code>Float.PositiveInfinity</code> nor
    * <code>Float.NegativeInfinity</code> nor <code>Float.NaN</code>).
    * </p>
    */
  opaque type NonZeroFiniteFloat <: NonZeroFloat = Float

  object NonZeroFiniteFloat {
    /** Implicitly widens a [[NonZeroFiniteFloat]] to a plain <code>Float</code>. */
    given Conversion[NonZeroFiniteFloat, Float] with {
      def apply(x: NonZeroFiniteFloat): Float = x
    }

    /** Compile-time factory for creating a [[NonZeroFiniteFloat]] from a float literal. */
    inline def apply[F <: Float & Singleton](inline f: F): NonZeroFiniteFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v != 0.0f && v != Float.PositiveInfinity && v != Float.NegativeInfinity && v != Float.NaN then
            v
          else
            error("NonZeroFiniteFloat cannot be instantiated with zero, infinity, or NaN")
        case None =>
          error("NonZeroFiniteFloat.apply requires a float literal")
      }

    /** Returns <code>true</code> if the provided <code>Float</code> is a valid [[NonZeroFiniteFloat]]
      * value — that is, if it is both <code>!= 0.0f</code> and finite (<code>isFinite</code>).
      */
    def isValid(value: Float): Boolean = value != 0.0f && value != Float.PositiveInfinity && value != Float.NegativeInfinity && value != Float.NaN

    /** Returns <code>Some(NonZeroFiniteFloat)</code> if the given <code>Float</code> is a valid
      * [[NonZeroFiniteFloat]] (non-zero and finite), or <code>None</code> otherwise.
      */
    def from(f: Float): Option[NonZeroFiniteFloat] =
      if (isValid(f)) Some(f) else None

    /** Returns the given <code>Float</code> as a [[NonZeroFiniteFloat]] if it is valid,
      * or throws <code>AssertionError</code> if it is not.
      */
    def ensuringValid(f: Float): NonZeroFiniteFloat =
      if (isValid(f))
        f
      else
        throw new AssertionError(Resources.notValidNonZeroFiniteFloat)

    /** Returns the given <code>Float</code> as a [[NonZeroFiniteFloat]] if it is valid,
      * or the given <code>default</code> value otherwise.
      */
    def fromOrElse(value: Float, default: => NonZeroFiniteFloat): NonZeroFiniteFloat =
      if (isValid(value)) value else default

    /** A factory/validation method that produces a <code>NonZeroFiniteFloat</code> wrapped
      * in a <code>Success</code> if the given <code>Float</code> is valid, or an
      * <code>AssertionError</code> wrapped in a <code>Failure</code> if it is not.
      */
    def tryingValid(value: Float): Try[NonZeroFiniteFloat] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.notValidNonZeroFiniteFloat))

    /** A validation method that produces a <code>Pass</code> given a valid <code>Float</code>
      * value, or a <code>Fail</code> containing an error value produced by passing the
      * invalid <code>Float</code> to the function <code>f</code>.
      */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** A factory/validation method that produces a <code>NonZeroFiniteFloat</code> wrapped
      * in a <code>Good</code> if the given <code>Float</code> is valid, or an error
      * value produced by passing the invalid <code>Float</code> to <code>f</code>
      * wrapped in a <code>Bad</code>.
      */
    def goodOrElse[B](value: Float)(f: Float => B): NonZeroFiniteFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** A factory/validation method that produces a <code>NonZeroFiniteFloat</code> wrapped
      * in a <code>Right</code> if the given <code>Float</code> is valid, or an error
      * value produced by passing the invalid <code>Float</code> to <code>f</code>
      * wrapped in a <code>Left</code>.
      */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, NonZeroFiniteFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** The largest value representable as a [[NonZeroFiniteFloat]], which is
      * <code>NonZeroFiniteFloat(Float.MaxValue)</code>.
      */
    val MaxValue: NonZeroFiniteFloat = Float.MaxValue

    /** The smallest value representable as a positive and finite [[NonZeroFiniteFloat]], which is
      * <code>NonZeroFiniteFloat(Float.MinPositiveValue)</code>.
      */
    val MinValue: NonZeroFiniteFloat = Float.MinPositiveValue

    /** The smallest positive value representable as a [[NonZeroFiniteFloat]], which is
      * <code>NonZeroFiniteFloat(Float.MinPositiveValue)</code>.
      */
    val MinPositiveValue: NonZeroFiniteFloat = Float.MinPositiveValue

    extension (p: NonZeroFiniteFloat) {
      /** Applies the given <code>Float =&gt; Float</code> function to the underlying
        * <code>Float</code> value, and returns the result as a [[NonZeroFiniteFloat]] if
        * it is valid, or throws <code>AssertionError</code> if it is not.
        */
      def ensuringValid(f: Float => Float): NonZeroFiniteFloat = {
        val candidateResult: Float = f(p)
        if (NonZeroFiniteFloat.isValid(candidateResult)) NonZeroFiniteFloat.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid NonZeroFiniteFloat")
      }
    }
  }
}