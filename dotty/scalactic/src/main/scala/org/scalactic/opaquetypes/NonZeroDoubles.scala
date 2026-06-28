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

object NonZeroDoubles {

  /** Opaque type representing a non-zero <code>Double</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy <code>!= 0.0</code>, but may
    * be <code>Double.PositiveInfinity</code>, <code>Double.NegativeInfinity</code>,
    * or <code>Double.NaN</code>.
    * </p>
    *
    * <p>
    * Use the compile-time <code>apply</code> overloads to construct instances from
    * literals, or the runtime factory methods [[NonZeroDouble.from]],
    * [[NonZeroDouble.ensuringValid]], and [[NonZeroDouble.fromOrElse]] for
    * values known only at runtime.
    * </p>
    */
  opaque type NonZeroDouble = Double

  /** Companion object for the [[NonZeroDouble]] opaque type.
    *
    * Provides compile-time <code>apply</code> overloads, runtime factory and
    * validation methods, implicit widening conversions, and an extension method
    * for post-computation validation.
    */
  object NonZeroDouble {
    /** Compile-time factory for creating a [[NonZeroDouble]] from a double literal.
      *
      * Rejects zero literals at compile time.
      */
    inline def apply[D <: Double & Singleton](inline d: D): NonZeroDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v != 0.0 then
            v.asInstanceOf[NonZeroDouble]
          else
            error("NonZeroDouble cannot be instantiated with zero")
        case None =>
          error("NonZeroDouble.apply requires a double literal")
      }

    /** Compile-time factory for creating a [[NonZeroDouble]] from an integer literal. */
    inline def apply[I <: Int & Singleton](inline i: I): NonZeroDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v != 0 then
            v.toDouble.asInstanceOf[NonZeroDouble]
          else
            error("NonZeroDouble cannot be instantiated with zero")
        case None =>
          error("NonZeroDouble.apply requires an integer or double literal")
      }

    /** Compile-time factory for creating a [[NonZeroDouble]] from a long literal. */
    inline def apply[L <: Long & Singleton](inline l: L): NonZeroDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v != 0L then
            v.toDouble.asInstanceOf[NonZeroDouble]
          else
            error("NonZeroDouble cannot be instantiated with zero")
        case None =>
          error("NonZeroDouble.apply requires a long or double literal")
      }

    /** Compile-time factory for creating a [[NonZeroDouble]] from a float literal. */
    inline def apply[F <: Float & Singleton](inline f: F): NonZeroDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v != 0.0f then
            v.toDouble.asInstanceOf[NonZeroDouble]
          else
            error("NonZeroDouble cannot be instantiated with zero")
        case None =>
          error("NonZeroDouble.apply requires a float or double literal")
      }

    /** Returns the given <code>Double</code> as a [[NonZeroDouble]] if it is non-zero,
      * or throws <code>AssertionError</code> if it is zero.
      *
      * <p>
      * This method is appropriate when you are certain the value is non-zero; use
      * [[from]] when you want to handle the invalid case gracefully.
      * </p>
      *
      * @param d the <code>Double</code> to return as a [[NonZeroDouble]]
      * @return <code>d</code> as a [[NonZeroDouble]] if non-zero
      * @throws AssertionError if <code>d</code> is zero
      */
    def ensuringValid(d: Double): NonZeroDouble =
      if (d == 0.0)
        throw new AssertionError(Resources.invalidNonZeroDouble)
      else d

    /** Returns <code>Some(NonZeroDouble)</code> if the given <code>Double</code> is non-zero,
      * or <code>None</code> otherwise.
      *
      * <p>
      * This factory method inspects the value at runtime.  Use the compile-time
      * <code>apply</code> overloads when constructing from literals.
      * </p>
      *
      * @param d the <code>Double</code> to inspect
      * @return <code>Some(NonZeroDouble)</code> if <code>d != 0.0</code>, else <code>None</code>
      */
    def from(d: Double): Option[NonZeroDouble] =
      if (d == 0.0) None else Some(d)

    /** Implicitly widens a [[NonZeroDouble]] to a plain <code>Double</code>. */
    given Conversion[NonZeroDouble, Double] with {
      def apply(x: NonZeroDouble): Double = x
    }

    /** Convert Double to [[NonZeroDouble]] via compile-time or runtime validation. */
    given Conversion[Double, NonZeroDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NonZeroDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v == 0.0 then
              error("NonZeroDouble cannot be instantiated with zero")
            else
              v.asInstanceOf[NonZeroDouble]
          case None =>
            error("NonZeroDouble conversion requires a double literal")
        }

      def apply(x: Double): NonZeroDouble = NonZeroDouble.ensuringValid(x)
    }

    /** Convert Int to [[NonZeroDouble]] via compile-time or runtime validation. */
    given Conversion[Int, NonZeroDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): NonZeroDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v == 0 then
              error("NonZeroDouble cannot be instantiated with zero")
            else
              v.toDouble.asInstanceOf[NonZeroDouble]
          case None =>
            error("NonZeroDouble conversion requires an integer literal")
        }

      def apply(x: Int): NonZeroDouble = NonZeroDouble.ensuringValid(x.toDouble)
    }

    /** Convert Long to [[NonZeroDouble]] via compile-time or runtime validation. */
    given Conversion[Long, NonZeroDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): NonZeroDouble =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v == 0L then
              error("NonZeroDouble cannot be instantiated with zero")
            else
              v.toDouble.asInstanceOf[NonZeroDouble]
          case None =>
            error("NonZeroDouble conversion requires a long literal")
        }

      def apply(x: Long): NonZeroDouble = NonZeroDouble.ensuringValid(x.toDouble)
    }

    /** Convert Float to [[NonZeroDouble]] via compile-time or runtime validation. */
    given Conversion[Float, NonZeroDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): NonZeroDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v == 0.0f then
              error("NonZeroDouble cannot be instantiated with zero")
            else
              v.toDouble.asInstanceOf[NonZeroDouble]
          case None =>
            error("NonZeroDouble conversion requires a float literal")
        }

      def apply(x: Float): NonZeroDouble = NonZeroDouble.ensuringValid(x.toDouble)
    }

    /** Ordering instance for NonZeroDouble that orders by numeric value. */
    given Ordering[NonZeroDouble] with {
      def compare(x: NonZeroDouble, y: NonZeroDouble): Int = x.compareTo(y)
    }

    /** The positive infinity value, which is <code>NonZeroDouble.ensuringValid(Double.PositiveInfinity)</code>. */
    val PositiveInfinity: NonZeroDouble = NonZeroDouble.ensuringValid(Double.PositiveInfinity)

    /** The negative infinity value, which is <code>NonZeroDouble.ensuringValid(Double.NegativeInfinity)</code>. */
    val NegativeInfinity: NonZeroDouble = NonZeroDouble.ensuringValid(Double.NegativeInfinity)

    /** The smallest positive value greater than 0.0d representable as a <code>NonZeroDouble</code>, which is NonZeroDouble(4.9E-324). */
    val MinPositiveValue: NonZeroDouble = NonZeroDouble.ensuringValid(Double.MinPositiveValue)
  }

  /** Extension methods for NonZeroDouble at the object level. */
  extension (x: NonZeroDouble) {
    /** Return the underlying Double value. */
    def value: Double = x

    /** Return the underlying Double value. */
    def toDouble: Double = x

    /** Unary plus returns this value unchanged. */
    def unary_+ : NonZeroDouble = x

    /** Numeric negation (returns Double to handle infinity). */
    def unary_- : Double = -x

    /** Greater of this and that value. */
    def max(that: NonZeroDouble): NonZeroDouble = if (math.max(x, that) == x) x else that

    /** Lesser of this and that value. */
    def min(that: NonZeroDouble): NonZeroDouble = if (math.min(x, that) == x) x else that

    /** Indicates whether this `NonZeroDouble` has a value that is a whole number: it is finite and it has no fraction part. */
    def isWhole: Boolean = {
      val longValue = x.toLong
      longValue.toDouble == x || longValue == Long.MaxValue && x < Double.PositiveInfinity || longValue == Long.MinValue && x > Double.NegativeInfinity
    }

    /** Rounds this `NonZeroDouble` value to the nearest whole number, returning the result as a [[NonZeroLong]]. */
    def round: NonZeroLong = NonZeroLong.ensuringValid(math.round(x))

    /** Returns the smallest (closest to negative infinity) <code>NonZeroDouble</code> that is greater than or equal to this value
      * and represents a mathematical integer.
      */
    def ceil: NonZeroDouble = NonZeroDouble.ensuringValid(math.ceil(x))

    /** Returns the greatest (closest to positive infinity) <code>NonZeroDouble</code> that is less than or equal to this value
      * and represents a mathematical integer.
      */
    def floor: NonZeroDouble = NonZeroDouble.ensuringValid(math.floor(x))

    /** Converts an angle measured in degrees to an approximately equivalent angle measured in radians. */
    def toRadians: Double = math.toRadians(x)

    /** Converts an angle measured in radians to an approximately equivalent angle measured in degrees. */
    def toDegrees: Double = math.toDegrees(x)

    /** True if this <code>NonZeroDouble</code> value represents positive infinity, else false. */
    def isPosInfinity: Boolean = Double.PositiveInfinity == x

    /** True if this <code>NonZeroDouble</code> value represents negative infinity, else false. */
    def isNegInfinity: Boolean = Double.NegativeInfinity == x

    /** True if this <code>NonZeroDouble</code> value represents positive or negative infinity, else false. */
    def isInfinite: Boolean = java.lang.Double.isInfinite(x)

    /** True if this <code>NonZeroDouble</code> value is any finite value (i.e., it is neither positive nor negative infinity), else false. */
    def isFinite: Boolean = !java.lang.Double.isInfinite(x)
  }

  // Extension methods for arithmetic operations with other opaque Int types
  extension (x: NonZeroDouble) {
    @annotation.targetName("plusPosInt")
    def +(y: PosInt): Double = x + y.value
    @annotation.targetName("plusPosZInt")
    def +(y: PosZInt): Double = x + y.value
    @annotation.targetName("plusNonZeroInt")
    def +(y: NonZeroInt): Double = x + y.value
    @annotation.targetName("plusNegInt")
    def +(y: NegInt): Double = x + y.value
    @annotation.targetName("plusNegZInt")
    def +(y: NegZInt): Double = x + y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("plusPosLong")
    def +(y: PosLong): Double = x + y.value
    @annotation.targetName("plusPosZLong")
    def +(y: PosZLong): Double = x + y.value
    @annotation.targetName("plusNonZeroLong")
    def +(y: NonZeroLong): Double = x + y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("plusPosFloat")
    def +(y: PosFloat): Double = x + y.value
    @annotation.targetName("plusPosZFloat")
    def +(y: PosZFloat): Double = x + y.value
    @annotation.targetName("plusPosZFiniteFloat")
    def +(y: PosZFiniteFloat): Double = x + y.value
    @annotation.targetName("plusPosFiniteFloat")
    def +(y: PosFiniteFloat): Double = x + y.value
    @annotation.targetName("plusFiniteFloat")
    def +(y: FiniteFloat): Double = x + y.value
    @annotation.targetName("plusNonZeroFloat")
    def +(y: NonZeroFloat): Double = x + y
    @annotation.targetName("plusNonZeroFiniteFloat")
    def +(y: NonZeroFiniteFloat): Double = x + y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("plusNegFloat")
    def +(y: NegFloat): Double = x + y.value
    @annotation.targetName("plusNegZFloat")
    def +(y: NegZFloat): Double = x + y.value
    @annotation.targetName("plusNegZFiniteFloat")
    def +(y: NegZFiniteFloat): Double = x + y.value
    @annotation.targetName("plusNegFiniteFloat")
    def +(y: NegFiniteFloat): Double = x + y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("plusPosDouble")
    def +(y: PosDouble): Double = x + y.value
    @annotation.targetName("plusPosZDouble")
    def +(y: PosZDouble): Double = x + y.value
    @annotation.targetName("plusPosZFiniteDouble")
    def +(y: PosZFiniteDouble): Double = x + y.value
    @annotation.targetName("plusPosFiniteDouble")
    def +(y: PosFiniteDouble): Double = x + y.value
    @annotation.targetName("plusFiniteDouble")
    def +(y: FiniteDouble): Double = x + y.value
    @annotation.targetName("plusNonZeroDouble")
    def +(y: NonZeroDouble): Double = x + y
    @annotation.targetName("plusNonZeroFiniteDouble")
    def +(y: NonZeroFiniteDouble): Double = x + y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("plusNegDouble")
    def +(y: NegDouble): Double = x + y.value
    @annotation.targetName("plusNegZDouble")
    def +(y: NegZDouble): Double = x + y.value
    @annotation.targetName("plusNegZFiniteDouble")
    def +(y: NegZFiniteDouble): Double = x + y.value
    @annotation.targetName("plusNegFiniteDouble")
    def +(y: NegFiniteDouble): Double = x + y.value
  }

  // Subtraction methods
  extension (x: NonZeroDouble) {
    @annotation.targetName("minusPosInt")
    def -(y: PosInt): Double = x - y.value
    @annotation.targetName("minusPosZInt")
    def -(y: PosZInt): Double = x - y.value
    @annotation.targetName("minusNonZeroInt")
    def -(y: NonZeroInt): Double = x - y.value
    @annotation.targetName("minusNegInt")
    def -(y: NegInt): Double = x - y.value
    @annotation.targetName("minusNegZInt")
    def -(y: NegZInt): Double = x - y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("minusPosLong")
    def -(y: PosLong): Double = x - y.value
    @annotation.targetName("minusPosZLong")
    def -(y: PosZLong): Double = x - y.value
    @annotation.targetName("minusNonZeroLong")
    def -(y: NonZeroLong): Double = x - y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("minusPosFloat")
    def -(y: PosFloat): Double = x - y.value
    @annotation.targetName("minusPosZFloat")
    def -(y: PosZFloat): Double = x - y.value
    @annotation.targetName("minusPosZFiniteFloat")
    def -(y: PosZFiniteFloat): Double = x - y.value
    @annotation.targetName("minusPosFiniteFloat")
    def -(y: PosFiniteFloat): Double = x - y.value
    @annotation.targetName("minusFiniteFloat")
    def -(y: FiniteFloat): Double = x - y.value
    @annotation.targetName("minusNonZeroFloat")
    def -(y: NonZeroFloat): Double = x - y
    @annotation.targetName("minusNonZeroFiniteFloat")
    def -(y: NonZeroFiniteFloat): Double = x - y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("minusNegFloat")
    def -(y: NegFloat): Double = x - y.value
    @annotation.targetName("minusNegZFloat")
    def -(y: NegZFloat): Double = x - y.value
    @annotation.targetName("minusNegZFiniteFloat")
    def -(y: NegZFiniteFloat): Double = x - y.value
    @annotation.targetName("minusNegFiniteFloat")
    def -(y: NegFiniteFloat): Double = x - y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("minusPosDouble")
    def -(y: PosDouble): Double = x - y.value
    @annotation.targetName("minusPosZDouble")
    def -(y: PosZDouble): Double = x - y.value
    @annotation.targetName("minusPosZFiniteDouble")
    def -(y: PosZFiniteDouble): Double = x - y.value
    @annotation.targetName("minusPosFiniteDouble")
    def -(y: PosFiniteDouble): Double = x - y.value
    @annotation.targetName("minusFiniteDouble")
    def -(y: FiniteDouble): Double = x - y.value
    @annotation.targetName("minusNonZeroDouble")
    def -(y: NonZeroDouble): Double = x - y
    @annotation.targetName("minusNonZeroFiniteDouble")
    def -(y: NonZeroFiniteDouble): Double = x - y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("minusNegDouble")
    def -(y: NegDouble): Double = x - y.value
    @annotation.targetName("minusNegZDouble")
    def -(y: NegZDouble): Double = x - y.value
    @annotation.targetName("minusNegZFiniteDouble")
    def -(y: NegZFiniteDouble): Double = x - y.value
    @annotation.targetName("minusNegFiniteDouble")
    def -(y: NegFiniteDouble): Double = x - y.value
  }

  // Multiplication methods
  extension (x: NonZeroDouble) {
    @annotation.targetName("timesPosInt")
    def *(y: PosInt): Double = x * y.value
    @annotation.targetName("timesPosZInt")
    def *(y: PosZInt): Double = x * y.value
    @annotation.targetName("timesNonZeroInt")
    def *(y: NonZeroInt): Double = x * y.value
    @annotation.targetName("timesNegInt")
    def *(y: NegInt): Double = x * y.value
    @annotation.targetName("timesNegZInt")
    def *(y: NegZInt): Double = x * y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("timesPosLong")
    def *(y: PosLong): Double = x * y.value
    @annotation.targetName("timesPosZLong")
    def *(y: PosZLong): Double = x * y.value
    @annotation.targetName("timesNonZeroLong")
    def *(y: NonZeroLong): Double = x * y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("timesPosFloat")
    def *(y: PosFloat): Double = x * y.value
    @annotation.targetName("timesPosZFloat")
    def *(y: PosZFloat): Double = x * y.value
    @annotation.targetName("timesPosZFiniteFloat")
    def *(y: PosZFiniteFloat): Double = x * y.value
    @annotation.targetName("timesPosFiniteFloat")
    def *(y: PosFiniteFloat): Double = x * y.value
    @annotation.targetName("timesFiniteFloat")
    def *(y: FiniteFloat): Double = x * y.value
    @annotation.targetName("timesNonZeroFloat")
    def *(y: NonZeroFloat): Double = x * y
    @annotation.targetName("timesNonZeroFiniteFloat")
    def *(y: NonZeroFiniteFloat): Double = x * y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("timesNegFloat")
    def *(y: NegFloat): Double = x * y.value
    @annotation.targetName("timesNegZFloat")
    def *(y: NegZFloat): Double = x * y.value
    @annotation.targetName("timesNegZFiniteFloat")
    def *(y: NegZFiniteFloat): Double = x * y.value
    @annotation.targetName("timesNegFiniteFloat")
    def *(y: NegFiniteFloat): Double = x * y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("timesPosDouble")
    def *(y: PosDouble): Double = x * y.value
    @annotation.targetName("timesPosZDouble")
    def *(y: PosZDouble): Double = x * y.value
    @annotation.targetName("timesPosZFiniteDouble")
    def *(y: PosZFiniteDouble): Double = x * y.value
    @annotation.targetName("timesPosFiniteDouble")
    def *(y: PosFiniteDouble): Double = x * y.value
    @annotation.targetName("timesFiniteDouble")
    def *(y: FiniteDouble): Double = x * y.value
    @annotation.targetName("timesNonZeroDouble")
    def *(y: NonZeroDouble): Double = x * y
    @annotation.targetName("timesNonZeroFiniteDouble")
    def *(y: NonZeroFiniteDouble): Double = x * y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("timesNegDouble")
    def *(y: NegDouble): Double = x * y.value
    @annotation.targetName("timesNegZDouble")
    def *(y: NegZDouble): Double = x * y.value
    @annotation.targetName("timesNegZFiniteDouble")
    def *(y: NegZFiniteDouble): Double = x * y.value
    @annotation.targetName("timesNegFiniteDouble")
    def *(y: NegFiniteDouble): Double = x * y.value
  }

  // Division methods
  extension (x: NonZeroDouble) {
    @annotation.targetName("divPosInt")
    def /(y: PosInt): Double = x / y.value
    @annotation.targetName("divPosZInt")
    def /(y: PosZInt): Double = x / y.value
    @annotation.targetName("divNonZeroInt")
    def /(y: NonZeroInt): Double = x / y.value
    @annotation.targetName("divNegInt")
    def /(y: NegInt): Double = x / y.value
    @annotation.targetName("divNegZInt")
    def /(y: NegZInt): Double = x / y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("divPosLong")
    def /(y: PosLong): Double = x / y.value
    @annotation.targetName("divPosZLong")
    def /(y: PosZLong): Double = x / y.value
    @annotation.targetName("divNonZeroLong")
    def /(y: NonZeroLong): Double = x / y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("divPosFloat")
    def /(y: PosFloat): Double = x / y.value
    @annotation.targetName("divPosZFloat")
    def /(y: PosZFloat): Double = x / y.value
    @annotation.targetName("divPosZFiniteFloat")
    def /(y: PosZFiniteFloat): Double = x / y.value
    @annotation.targetName("divPosFiniteFloat")
    def /(y: PosFiniteFloat): Double = x / y.value
    @annotation.targetName("divFiniteFloat")
    def /(y: FiniteFloat): Double = x / y.value
    @annotation.targetName("divNonZeroFloat")
    def /(y: NonZeroFloat): Double = x / y
    @annotation.targetName("divNonZeroFiniteFloat")
    def /(y: NonZeroFiniteFloat): Double = x / y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("divNegFloat")
    def /(y: NegFloat): Double = x / y.value
    @annotation.targetName("divNegZFloat")
    def /(y: NegZFloat): Double = x / y.value
    @annotation.targetName("divNegZFiniteFloat")
    def /(y: NegZFiniteFloat): Double = x / y.value
    @annotation.targetName("divNegFiniteFloat")
    def /(y: NegFiniteFloat): Double = x / y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("divPosDouble")
    def /(y: PosDouble): Double = x / y.value
    @annotation.targetName("divPosZDouble")
    def /(y: PosZDouble): Double = x / y.value
    @annotation.targetName("divPosZFiniteDouble")
    def /(y: PosZFiniteDouble): Double = x / y.value
    @annotation.targetName("divPosFiniteDouble")
    def /(y: PosFiniteDouble): Double = x / y.value
    @annotation.targetName("divFiniteDouble")
    def /(y: FiniteDouble): Double = x / y.value
    @annotation.targetName("divNonZeroDouble")
    def /(y: NonZeroDouble): Double = x / y
    @annotation.targetName("divNonZeroFiniteDouble")
    def /(y: NonZeroFiniteDouble): Double = x / y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("divNegDouble")
    def /(y: NegDouble): Double = x / y.value
    @annotation.targetName("divNegZDouble")
    def /(y: NegZDouble): Double = x / y.value
    @annotation.targetName("divNegZFiniteDouble")
    def /(y: NegZFiniteDouble): Double = x / y.value
    @annotation.targetName("divNegFiniteDouble")
    def /(y: NegFiniteDouble): Double = x / y.value
  }

  // Modulo methods
  extension (x: NonZeroDouble) {
    @annotation.targetName("modPosInt")
    def %(y: PosInt): Double = x % y.value
    @annotation.targetName("modPosZInt")
    def %(y: PosZInt): Double = x % y.value
    @annotation.targetName("modNonZeroInt")
    def %(y: NonZeroInt): Double = x % y.value
    @annotation.targetName("modNegInt")
    def %(y: NegInt): Double = x % y.value
    @annotation.targetName("modNegZInt")
    def %(y: NegZInt): Double = x % y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("modPosLong")
    def %(y: PosLong): Double = x % y.value
    @annotation.targetName("modPosZLong")
    def %(y: PosZLong): Double = x % y.value
    @annotation.targetName("modNonZeroLong")
    def %(y: NonZeroLong): Double = x % y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("modPosFloat")
    def %(y: PosFloat): Double = x % y.value
    @annotation.targetName("modPosZFloat")
    def %(y: PosZFloat): Double = x % y.value
    @annotation.targetName("modPosZFiniteFloat")
    def %(y: PosZFiniteFloat): Double = x % y.value
    @annotation.targetName("modPosFiniteFloat")
    def %(y: PosFiniteFloat): Double = x % y.value
    @annotation.targetName("modFiniteFloat")
    def %(y: FiniteFloat): Double = x % y.value
    @annotation.targetName("modNonZeroFloat")
    def %(y: NonZeroFloat): Double = x % y
    @annotation.targetName("modNonZeroFiniteFloat")
    def %(y: NonZeroFiniteFloat): Double = x % y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("modNegFloat")
    def %(y: NegFloat): Double = x % y.value
    @annotation.targetName("modNegZFloat")
    def %(y: NegZFloat): Double = x % y.value
    @annotation.targetName("modNegZFiniteFloat")
    def %(y: NegZFiniteFloat): Double = x % y.value
    @annotation.targetName("modNegFiniteFloat")
    def %(y: NegFiniteFloat): Double = x % y.value
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("modPosDouble")
    def %(y: PosDouble): Double = x % y.value
    @annotation.targetName("modPosZDouble")
    def %(y: PosZDouble): Double = x % y.value
    @annotation.targetName("modPosZFiniteDouble")
    def %(y: PosZFiniteDouble): Double = x % y.value
    @annotation.targetName("modPosFiniteDouble")
    def %(y: PosFiniteDouble): Double = x % y.value
    @annotation.targetName("modFiniteDouble")
    def %(y: FiniteDouble): Double = x % y.value
    @annotation.targetName("modNonZeroDouble")
    def %(y: NonZeroDouble): Double = x % y
    @annotation.targetName("modNonZeroFiniteDouble")
    def %(y: NonZeroFiniteDouble): Double = x % y
  }

  extension (x: NonZeroDouble) {
    @annotation.targetName("modNegDouble")
    def %(y: NegDouble): Double = x % y.value
    @annotation.targetName("modNegZDouble")
    def %(y: NegZDouble): Double = x % y.value
    @annotation.targetName("modNegZFiniteDouble")
    def %(y: NegZFiniteDouble): Double = x % y.value
    @annotation.targetName("modNegFiniteDouble")
    def %(y: NegFiniteDouble): Double = x % y.value
  }

  // Comparison operations
  extension (x: NonZeroDouble) {
    def <(y: Byte): Boolean = x < y
    def <(y: Short): Boolean = x < y
    def <(y: Char): Boolean = x < y
    def <(y: Int): Boolean = x < y
    def <(y: Long): Boolean = x < y
    def <(y: Float): Boolean = x < y
    def <(y: Double): Boolean = x < y
  }

  extension (x: NonZeroDouble) {
    def <=(y: Byte): Boolean = x <= y
    def <=(y: Short): Boolean = x <= y
    def <=(y: Char): Boolean = x <= y
    def <=(y: Int): Boolean = x <= y
    def <=(y: Long): Boolean = x <= y
    def <=(y: Float): Boolean = x <= y
    def <=(y: Double): Boolean = x <= y
  }

  extension (x: NonZeroDouble) {
    def >(y: Byte): Boolean = x > y
    def >(y: Short): Boolean = x > y
    def >(y: Char): Boolean = x > y
    def >(y: Int): Boolean = x > y
    def >(y: Long): Boolean = x > y
    def >(y: Float): Boolean = x > y
    def >(y: Double): Boolean = x > y
  }

  extension (x: NonZeroDouble) {
    def >=(y: Byte): Boolean = x >= y
    def >=(y: Short): Boolean = x >= y
    def >=(y: Char): Boolean = x >= y
    def >=(y: Int): Boolean = x >= y
    def >=(y: Long): Boolean = x >= y
    def >=(y: Float): Boolean = x >= y
    def >=(y: Double): Boolean = x >= y
  }

  // Arithmetic with primitive types (for the primitive operation tests)
  extension (x: NonZeroDouble) {
    def +(y: Byte): Double = x + y
    def +(y: Short): Double = x + y
    def +(y: Char): Double = x + y
    def +(y: Int): Double = x + y
    def +(y: Long): Double = x + y
    def +(y: Float): Double = x + y
    def +(y: Double): Double = x + y
  }

  extension (x: NonZeroDouble) {
    def -(y: Byte): Double = x - y
    def -(y: Short): Double = x - y
    def -(y: Char): Double = x - y
    def -(y: Int): Double = x - y
    def -(y: Long): Double = x - y
    def -(y: Float): Double = x - y
    def -(y: Double): Double = x - y
  }

  extension (x: NonZeroDouble) {
    def *(y: Byte): Double = x * y
    def *(y: Short): Double = x * y
    def *(y: Char): Double = x * y
    def *(y: Int): Double = x * y
    def *(y: Long): Double = x * y
    def *(y: Float): Double = x * y
    def *(y: Double): Double = x * y
  }

  extension (x: NonZeroDouble) {
    def /(y: Byte): Double = x / y
    def /(y: Short): Double = x / y
    def /(y: Char): Double = x / y
    def /(y: Int): Double = x / y
    def /(y: Long): Double = x / y
    def /(y: Float): Double = x / y
    def /(y: Double): Double = x / y
  }

  extension (x: NonZeroDouble) {
    def %(y: Byte): Double = x % y
    def %(y: Short): Double = x % y
    def %(y: Char): Double = x % y
    def %(y: Int): Double = x % y
    def %(y: Long): Double = x % y
    def %(y: Float): Double = x % y
    def %(y: Double): Double = x % y
  }

  /** Opaque type representing a non-zero, finite <code>Double</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy both <code>!= 0.0</code>
    * and <code>isFinite</code> (i.e. neither <code>Double.PositiveInfinity</code> nor
    * <code>Double.NegativeInfinity</code> nor <code>Double.NaN</code>).
    * </p>
    */
  opaque type NonZeroFiniteDouble <: NonZeroDouble = Double

  object NonZeroFiniteDouble {
    /** Implicitly widens a [[NonZeroFiniteDouble]] to a plain <code>Double</code>. */
    given Conversion[NonZeroFiniteDouble, Double] with {
      def apply(x: NonZeroFiniteDouble): Double = x
    }

    /** Compile-time factory for creating a [[NonZeroFiniteDouble]] from a double literal. */
    inline def apply[D <: Double & Singleton](inline d: D): NonZeroFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v != 0.0 && v != Double.PositiveInfinity && v != Double.NegativeInfinity && v != Double.NaN then
            v
          else
            error("NonZeroFiniteDouble cannot be instantiated with zero, infinity, or NaN")
        case None =>
          error("NonZeroFiniteDouble.apply requires a double literal")
      }

    /** Returns <code>true</code> if the provided <code>Double</code> is a valid [[NonZeroFiniteDouble]]
      * value — that is, if it is both <code>!= 0.0</code> and finite (<code>isFinite</code>).
      */
    def isValid(value: Double): Boolean = value != 0.0 && value != Double.PositiveInfinity && value != Double.NegativeInfinity && value != Double.NaN

    /** Returns <code>Some(NonZeroFiniteDouble)</code> if the given <code>Double</code> is a valid
      * [[NonZeroFiniteDouble]] (non-zero and finite), or <code>None</code> otherwise.
      */
    def from(d: Double): Option[NonZeroFiniteDouble] =
      if (isValid(d)) Some(d) else None

    /** Returns the given <code>Double</code> as a [[NonZeroFiniteDouble]] if it is valid,
      * or throws <code>AssertionError</code> if it is not.
      */
    def ensuringValid(d: Double): NonZeroFiniteDouble =
      if (isValid(d))
        d
      else
        throw new AssertionError(Resources.notValidNonZeroFiniteDouble)

    /** Returns the given <code>Double</code> as a [[NonZeroFiniteDouble]] if it is valid,
      * or the given <code>default</code> value otherwise.
      */
    def fromOrElse(value: Double, default: => NonZeroFiniteDouble): NonZeroFiniteDouble =
      if (isValid(value)) value else default

    /** A factory/validation method that produces a <code>NonZeroFiniteDouble</code> wrapped
      * in a <code>Success</code> if the given <code>Double</code> is valid, or an
      * <code>AssertionError</code> wrapped in a <code>Failure</code> if it is not.
      */
    def tryingValid(value: Double): Try[NonZeroFiniteDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.notValidNonZeroFiniteDouble))

    /** A validation method that produces a <code>Pass</code> given a valid <code>Double</code>
      * value, or a <code>Fail</code> containing an error value produced by passing the
      * invalid <code>Double</code> to the function <code>f</code>.
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** A factory/validation method that produces a <code>NonZeroFiniteDouble</code> wrapped
      * in a <code>Good</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Bad</code>.
      */
    def goodOrElse[B](value: Double)(f: Double => B): NonZeroFiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** A factory/validation method that produces a <code>NonZeroFiniteDouble</code> wrapped
      * in a <code>Right</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Left</code>.
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NonZeroFiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** The largest value representable as a [[NonZeroFiniteDouble]], which is
      * <code>NonZeroFiniteDouble(Double.MaxValue)</code>.
      */
    val MaxValue: NonZeroFiniteDouble = Double.MaxValue

    /** The smallest value representable as a positive and finite [[NonZeroFiniteDouble]], which is
      * <code>NonZeroFiniteDouble(Double.MinPositiveValue)</code>.
      */
    val MinValue: NonZeroFiniteDouble = Double.MinPositiveValue

    /** The smallest positive value representable as a [[NonZeroFiniteDouble]], which is
      * <code>NonZeroFiniteDouble(Double.MinPositiveValue)</code>.
      */
    val MinPositiveValue: NonZeroFiniteDouble = Double.MinPositiveValue

    extension (p: NonZeroFiniteDouble) {
      /** Applies the given <code>Double => Double</code> function to the underlying
        * <code>Double</code> value, and returns the result as a [[NonZeroFiniteDouble]] if
        * it is valid, or throws <code>AssertionError</code> if it is not.
        */
      def ensuringValid(f: Double => Double): NonZeroFiniteDouble = {
        val candidateResult: Double = f(p)
        if (NonZeroFiniteDouble.isValid(candidateResult)) NonZeroFiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid NonZeroFiniteDouble")
      }
    }
  }
}
