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

  /** Opaque type alias for <code>Float</code> that represents any non-zero <code>Float</code> value.
    *
    *  <p>
    *  Instances of this type are guaranteed to satisfy <code>!= 0.0f</code>.
    *  Unlike [[NonZeroFiniteFloat]], this type permits infinite values
    *  (<code>Float.PositiveInfinity</code> and <code>Float.NegativeInfinity</code>)
    *  as well as <code>Float.NaN</code>.
    *  </p>
    *
    *  @see [[NonZeroFloat]] companion object for factory methods and conversions.
    */
  opaque type NonZeroFloat = Float

  /** Companion object for [[NonZeroFloat]].
    *
    *  Provides factory methods, validation utilities, and conversions
    *  for the [[NonZeroFloat]] opaque type, which wraps a <code>Float</code>
    *  value guaranteed to be non-zero.
    *
    *  @see [[NonZeroFloat]] opaque type definition.
    */
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
      error("NonZeroFloat.apply from Int is not supported due to potential precision loss. Use explicit toFloat: NonZeroFloat(i.toFloat)")

    /** Ensure the given <code>Float</code> is non-zero and return it as a [[NonZeroFloat]].
      *
      * @param f the <code>Float</code> to validate
      * @return the given float as a [[NonZeroFloat]] if it is non-zero
      * @throws AssertionError if the given Float is zero
      */
    /** Validate and return the given `Float` as [[NonZeroFloat]].
      *
      * @param f the `Float` to validate
      * @return the validated value as a [[NonZeroFloat]]
      * @throws AssertionError if `f` is zero
      */
    def ensuringValid(f: Float): NonZeroFloat =
      if (f == 0.0f)
        throw new AssertionError(Resources.invalidNonZeroFloat)
      else f

    /** Construct a [[NonZeroFloat]] from a runtime `Float` if it is non-zero.
      *
      * @param f the `Float` to validate
      * @return `Some(NonZeroFloat)` when `f != 0.0f`, else `None`
      */
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

  /** Companion object for [[NonZeroFiniteFloat]].
    *
    * Provides factory methods, validation utilities, conversions, and constants
    * for the [[NonZeroFiniteFloat]] opaque type, which wraps a <code>Float</code>
    * value guaranteed to be both non-zero and finite.
    *
    * @see [[NonZeroFiniteFloat]] opaque type definition.
    */
  object NonZeroFiniteFloat {
    /** Implicitly widens a [[NonZeroFiniteFloat]] to a plain <code>Float</code>. */
    given Conversion[NonZeroFiniteFloat, Float] with {
      def apply(x: NonZeroFiniteFloat): Float = x
    }

    /** Compile-time factory for creating a [[NonZeroFiniteFloat]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects zero, infinity, and NaN literals. Use it as:
      * <code>NonZeroFiniteFloat(5.0f)</code>. For non-literal values, use
      * [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the Float literal
      * @return a [[NonZeroFiniteFloat]] representing the given valid literal
      * @throws a compile-time error if the literal is zero, infinite, NaN, or not a literal
      */
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

    /** Check whether the provided <code>Float</code> is a valid [[NonZeroFiniteFloat]].
      *
      * A valid value is both non-zero (<code>!= 0.0f</code>) and finite
      * (neither <code>Float.PositiveInfinity</code>, <code>Float.NegativeInfinity</code>,
      * nor <code>Float.NaN</code>).
      *
      * @param value the Float to validate
      * @return <code>true</code> if the specified Float is a valid NonZeroFiniteFloat
      */
    def isValid(value: Float): Boolean = value != 0.0f && value != Float.PositiveInfinity && value != Float.NegativeInfinity && value != Float.NaN

    /** Construct a [[NonZeroFiniteFloat]] from a runtime <code>Float</code> if valid.
      *
      * @param f runtime Float to validate
      * @return <code>Some(NonZeroFiniteFloat)</code> if <code>f</code> is non-zero and finite, otherwise <code>None</code>
      */
    def from(f: Float): Option[NonZeroFiniteFloat] =
      if (isValid(f)) Some(f) else None

    /** Ensure the runtime <code>Float</code> is a valid [[NonZeroFiniteFloat]] and return it.
      *
      * @param f runtime Float to check
      * @return the given float as a [[NonZeroFiniteFloat]] if valid
      * @throws AssertionError if the given Float is zero, infinite, or NaN
      */
    def ensuringValid(f: Float): NonZeroFiniteFloat =
      if (isValid(f))
        f
      else
        throw new AssertionError(Resources.notValidNonZeroFiniteFloat)

    /** A factory/validation method that produces a <code>NonZeroFiniteFloat</code>, wrapped
      * in a <code>Success</code>, given a valid <code>Float</code> value, or if the
      * given <code>Float</code> is invalid, an <code>AssertionError</code>, wrapped
      * in a <code>Failure</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a [[NonZeroFiniteFloat]] <code>Float</code>, it will return a
      * <code>NonZeroFiniteFloat</code> representing that value, wrapped in a
      * <code>Success</code>. Otherwise, if the passed <code>Float</code> value is not
      * a [[NonZeroFiniteFloat]], this method will return an <code>AssertionError</code>,
      * wrapped in a <code>Failure</code>.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @param value the <code>Float</code> to inspect, and if a valid NonZeroFiniteFloat, return
      *     wrapped in a <code>Success(NonZeroFiniteFloat)</code>.
      * @return the specified <code>Float</code> value wrapped
      *     in a <code>Success(NonZeroFiniteFloat)</code>, if it is valid, else a <code>Failure(AssertionError)</code>.
      */
    def tryingValid(value: Float): Try[NonZeroFiniteFloat] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.notValidNonZeroFiniteFloat))

    /** A validation method that produces a <code>Pass</code>
      * given a valid <code>Float</code> value, or
      * an error value of type <code>E</code> produced by passing the
      * given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a [[NonZeroFiniteFloat]] <code>Float</code>, it will return a <code>Pass</code>.
      * Otherwise, the passed <code>Float</code> value is not a [[NonZeroFiniteFloat]], so this
      * method will return a result of type <code>E</code> obtained by passing
      * the invalid <code>Float</code> value to the given function <code>f</code>,
      * wrapped in a `Fail`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam E error type produced by f
      * @param value the `Float` to validate that it is a [[NonZeroFiniteFloat]].
      * @param f function to produce an error when value is invalid
      * @return a `Pass` if the specified `Float` value is a [[NonZeroFiniteFloat]],
      *   else a `Fail` containing an error value produced by passing the
      *   specified `Float` to the given function `f`.
      */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** A factory/validation method that produces a <code>NonZeroFiniteFloat</code>, wrapped
      * in a <code>Good</code>, given a valid <code>Float</code> value, or if the
      * given <code>Float</code> is invalid, an error value of type <code>B</code>
      * produced by passing the given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a [[NonZeroFiniteFloat]] <code>Float</code>, it will return a
      * <code>NonZeroFiniteFloat</code> representing that value, wrapped in a
      * <code>Good</code>. Otherwise, the passed <code>Float</code> value is not
      * a [[NonZeroFiniteFloat]], so this method will return a result of type
      * <code>B</code> obtained by passing the invalid <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a `Bad`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam B error type produced by f
      * @param value the <code>Float</code> to inspect, and if [[NonZeroFiniteFloat]], return
      *     wrapped in a <code>Good(NonZeroFiniteFloat)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Float</code> value wrapped
      *     in a <code>Good(NonZeroFiniteFloat)</code>, if it is [[NonZeroFiniteFloat]], else a <code>Bad(f(value))</code>.
      */
    def goodOrElse[B](value: Float)(f: Float => B): NonZeroFiniteFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** A factory/validation method that produces a <code>NonZeroFiniteFloat</code>, wrapped
      * in a <code>Right</code>, given a valid <code>Float</code> value, or if the
      * given <code>Float</code> is invalid, an error value of type <code>L</code>
      * produced by passing the given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Left</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a [[NonZeroFiniteFloat]] <code>Float</code>, it will return a
      * <code>NonZeroFiniteFloat</code> representing that value, wrapped in a
      * <code>Right</code>. Otherwise, the passed <code>Float</code> value is not
      * a [[NonZeroFiniteFloat]], so this method will return a result of type
      * <code>L</code> obtained by passing the invalid <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a `Left`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam L error type produced by f
      * @param value the <code>Float</code> to inspect, and if [[NonZeroFiniteFloat]], return
      *     wrapped in a <code>Right(NonZeroFiniteFloat)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Float</code> value wrapped
      *     in a <code>Right(NonZeroFiniteFloat)</code>, if it is [[NonZeroFiniteFloat]], else a <code>Left(f(value))</code>.
      */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, NonZeroFiniteFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** A factory method that produces a <code>NonZeroFiniteFloat</code> given a
      * <code>Float</code> value and a default <code>NonZeroFiniteFloat</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a valid [[NonZeroFiniteFloat]] <code>Float</code> (non-zero and finite),
      * it will return a <code>NonZeroFiniteFloat</code> representing that value.
      * Otherwise, the passed <code>Float</code> value is zero, infinite, or NaN, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas <code>fromOrElse</code>
      * inspects <code>Float</code> values at run time.
      * </p>
      *
      * @param value the <code>Float</code> to inspect, and if valid, return.
      * @param default the <code>NonZeroFiniteFloat</code> to return if the passed
      *     <code>Float</code> value is not valid.
      * @return the specified <code>Float</code> value wrapped in a
      *     <code>NonZeroFiniteFloat</code>, if it is valid, else the
      *     <code>default</code> <code>NonZeroFiniteFloat</code> value.
      */
    def fromOrElse(value: Float, default: => NonZeroFiniteFloat): NonZeroFiniteFloat =
      if (isValid(value)) value else default

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
