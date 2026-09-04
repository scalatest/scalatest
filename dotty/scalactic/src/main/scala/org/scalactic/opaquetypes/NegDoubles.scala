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
import scala.compiletime.{constValueOpt, error}
import scala.util.NotGiven
import scala.util.{Failure, Success, Try}

import org.scalactic.{Bad, Fail, Good, Or, Pass, Validation}

object NegDoubles {

  /** Opaque type representing any `Double` value ≤ 0.0 (non-positive doubles, including zero and negative infinity).
    *
    * This is the widest of the negative-double types. Values that are strictly negative
    * or finite can be represented by narrower subtypes: [[NegDouble]], [[NegZFiniteDouble]],
    * and [[NegFiniteDouble]].
    */
  opaque type NegZDouble = Double

  /** Companion object for [[NegZDouble]] with compile-time and runtime factory methods,
    * validation helpers, implicit conversions, and extension methods.
    */
  object NegZDouble {

    /** Compile-time factory for creating a [[NegZDouble]] from a `Double` literal.
      *
      * Rejects positive literals at compile time. For runtime values, use
      * [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton `Double` literal type
      * @param d the `Double` literal to validate
      * @return the validated literal as a [[NegZDouble]]
      * @throws scala.compiletime.error if the literal is positive or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): NegZDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v > 0.0 then
            error(Resources.notValidNegZDouble)
          else
            v.asInstanceOf[NegZDouble]
        case None =>
          error(Resources.notLiteralNegZDouble)
      }

    /** Compile-time factory for creating a [[NegZDouble]] from an `Int` literal.
      *
      * Rejects positive literals at compile time. For runtime values, use
      * [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton `Int` literal type
      * @param d the `Int` literal to validate
      * @return the validated literal as a [[NegZDouble]] widened to `Double`
      * @throws scala.compiletime.error if the literal is positive or not a literal
      */
    inline def apply[I <: Int & Singleton](inline d: I): NegZDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v > 0 then
            error(Resources.notValidNegZDouble)
          else
            v.toDouble.asInstanceOf[NegZDouble]
        case None =>
          error(Resources.notLiteralNegZDouble)
      }

    /** Compile-time factory for creating a [[NegZDouble]] from a `Long` literal.
      *
      * This overload always produces a compile-time error to prevent precision loss
      * when converting large `Long` values to `Double` (values beyond 2^53).
      * Use explicit widening: `NegZDouble(x.toDouble)`.
      *
      * @tparam L the singleton `Long` literal type
      * @param d the `Long` literal
      * @throws scala.compiletime.error unconditionally
      */
    inline def apply[L <: Long & Singleton](inline d: L): NegZDouble =
      error("NegZDouble.apply from Long is not supported due to potential precision loss. Use explicit toDouble: NegZDouble(x.toDouble)")

    inline def apply[F <: Float & Singleton](inline d: F): NegZDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v > 0.0f then
            error(Resources.notValidNegZDouble)
          else
            v.toDouble.asInstanceOf[NegZDouble]
        case None =>
          error(Resources.notLiteralNegZDouble)
      }

    def from(d: Double): Option[NegZDouble] =
      if (isValid(d)) Some(d) else None

    def ensuringValid(d: Double): NegZDouble =
      if (d > 0.0)
        throw new AssertionError(Resources.invalidNegZDouble)
      else d

    def tryingValid(value: Double): Try[NegZDouble] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegZDouble))

    def isValid(value: Double): Boolean = value <= 0.0

    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    def goodOrElse[B](value: Double)(f: Double => B): NegZDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NegZDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    def fromOrElse(value: Double, default: => NegZDouble): NegZDouble =
      if (isValid(value)) value else default

    /** Largest valid [[NegZDouble]] value, equal to `0.0`. */
    val MaxValue: NegZDouble = 0.0

    /** Smallest valid [[NegZDouble]] value, equal to `Double.MinValue`. */
    val MinValue: NegZDouble = Double.MinValue

    /** Negative infinity as a [[NegZDouble]], equal to `Double.NegativeInfinity`. */
    val NegativeInfinity: NegZDouble = Double.NegativeInfinity

    extension [A <: NegZDouble](p: A) {
      /** Return the underlying `Double` value. */
      def value: Double = p

      /** Return `true` if this value is negative infinity, `false` otherwise.
        *
        * Requires the receiver to be a finite type (not `NegInfinity`).
        */
      def isNegInfinity(using NotGiven[A <:< NegFiniteDouble]): Boolean = p == Double.NegativeInfinity

      /** Return `true` if this value is finite (not positive or negative infinity). */
      def isFinite: Boolean = !p.isInfinite

      /** Unary plus returns this value unchanged. */
      def unary_+ : NegZDouble = p

      /** Add another [[NegZDouble]] and revalidate the result.
        *
        * @param x the addend
        * @return the validated sum
        * @throws AssertionError if the sum is positive
        */
      def plus(x: NegZDouble): NegZDouble = NegZDouble.ensuringValid(value + x)

      /** Return the greater of this and that value.
        *
        * @param that the comparison value
        * @return the greater of the two values
        */
      def max(that: NegZDouble): NegZDouble = math.max(p, that)

      /** Return the lesser of this and that value.
        *
        * @param that the comparison value
        * @return the lesser of the two values
        */
      def min(that: NegZDouble): NegZDouble = math.min(p, that)

      /** Return `true` if this value is a mathematical integer (has no fractional part). */
      def isWhole: Boolean = {
        val longValue = p.toLong
        longValue.toDouble == p || longValue == Long.MaxValue && p < Double.PositiveInfinity || longValue == Long.MinValue && p > Double.NegativeInfinity
      }

      /** Round to the nearest whole number as a [[NegZLong]]. */
      def round: NegZLong = NegZLong.ensuringValid(math.round(value))

      /** Return the smallest whole-number value greater than or equal to this value. */
      def ceil: NegZDouble = NegZDouble.ensuringValid(math.ceil(value))

      /** Return the greatest whole-number value less than or equal to this value. */
      def floor: NegZDouble = NegZDouble.ensuringValid(math.floor(value))

      /** Convert degrees to radians.
        *
        * @return the approximate radian value
        */
      def toRadians: Double = math.toRadians(value)

      /** Convert radians to degrees.
        *
        * @return the approximate degree value
        */
      def toDegrees: Double = math.toDegrees(value)

      /** Apply a transformation and revalidate the result as a [[NegZDouble]].
        *
        * @param f transformation applied to the wrapped value
        * @return the validated transformed value
        * @throws AssertionError if the transformed value is positive
        */
      def ensuringValid(f: Double => Double): NegZDouble = {
        val candidateResult: Double = f(value)
        if (NegZDouble.isValid(candidateResult)) NegZDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${value.toString()}, was not a valid NegZDouble")
      }
    }

    /** Implicitly widens a [[NegZDouble]] to a plain <code>Double</code>. */
    given Conversion[NegZDouble, Double] with {
      def apply(x: NegZDouble): Double = x.value
    }

    /** Convert a <code>Double</code> to [[NegZDouble]] via compile-time or runtime validation. */
    given Conversion[Double, NegZDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegZDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v > 0.0 then
              error(Resources.notValidNegZDouble)
            else
              v.asInstanceOf[NegZDouble]
          case None =>
            error(Resources.notLiteralNegZDouble)
        }

      def apply(x: Double): NegZDouble = NegZDouble.ensuringValid(x)
    }

    /** Convert an <code>Int</code> to [[NegZDouble]] via compile-time or runtime validation.
      *
      * Rejects positive literals at compile time. For runtime values, use
      * [[ensuringValid]] or [[from]].
      */
    given Conversion[Int, NegZDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): NegZDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v > 0 then
              error(Resources.notValidNegZDouble)
            else
              v.toDouble.asInstanceOf[NegZDouble]
          case None =>
            error(Resources.notLiteralNegZDouble)
        }

      def apply(x: Int): NegZDouble = NegZDouble.ensuringValid(x.toDouble)
    }

    /** Blocking Long conversion to prevent precision loss. Long to Double can lose precision for values > 2^53. */
    given Conversion[Long, NegZDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): NegZDouble =
        error("NegZDouble conversion from Long is not supported due to potential precision loss. Use explicit toDouble: NegZDouble(8L.toDouble)")

      def apply(x: Long): NegZDouble =
        throw new AssertionError("NegZDouble conversion from Long is not supported due to potential precision loss. Use explicit toDouble: NegZDouble(x.toDouble)")
    }

    /** Convert a <code>Float</code> to [[NegZDouble]] via compile-time or runtime validation. */
    given Conversion[Float, NegZDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegZDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v > 0.0f then
              error(Resources.notValidNegZDouble)
            else
              v.toDouble.asInstanceOf[NegZDouble]
          case None =>
            error(Resources.notLiteralNegZDouble)
        }

      def apply(x: Float): NegZDouble = NegZDouble.ensuringValid(x.toDouble)
    }

    given [A <: NegZDouble](using NotGiven[A =:= NegDouble], NotGiven[A =:= NegZFiniteDouble], NotGiven[A =:= NegFiniteDouble]): Ordering[A] with {
      def compare(x: A, y: A): Int = java.lang.Double.compare(x.value, y.value)
    }

  }

  /** Opaque type representing any strictly negative `Double` value (< 0.0, including negative infinity).
    *
    * This is a strict subtype of [[NegZDouble]]; zero is excluded. Values that are also
    * finite can be represented by [[NegFiniteDouble]].
    */
  opaque type NegDouble <: NegZDouble = Double

  /** Companion object for [[NegDouble]] with compile-time and runtime factory methods,
    * validation helpers, implicit conversions, and extension methods.
    */
  object NegDouble {

    /** Compile-time factory for creating a [[NegDouble]] from an `Int` literal.
      *
      * Rejects non-negative literals at compile time. For runtime values, use
      * [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton `Int` literal type
      * @param d the `Int` literal to validate
      * @return the validated literal as a [[NegDouble]] widened to `Double`
      * @throws scala.compiletime.error if the literal is non-negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline d: I): NegDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v >= 0 then
            error("NegDouble cannot be instantiated with a non-negative integer literal")
          else
            v.toDouble.asInstanceOf[NegDouble]
        case None =>
          error("NegDouble.apply requires an integer literal")
      }

    /** Blocking Long literal apply to prevent precision loss. Long to Double can lose precision for values > 2^53. */
    inline def apply[L <: Long & Singleton](inline d: L): NegDouble =
      error("NegDouble.apply from Long is not supported due to potential precision loss. Use explicit toDouble: NegDouble(x.toDouble)")

    /** Compile-time factory for creating a [[NegDouble]] from a `Float` literal.
      *
      * Rejects non-negative literals at compile time. For runtime values, use
      * [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton `Float` literal type
      * @param d the `Float` literal to validate
      * @return the validated literal as a [[NegDouble]] widened to `Double`
      * @throws scala.compiletime.error if the literal is non-negative or not a literal
      */
    inline def apply[F <: Float & Singleton](inline d: F): NegDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v >= 0.0f then
            error("NegDouble cannot be instantiated with a non-negative float literal")
          else
            v.toDouble.asInstanceOf[NegDouble]
        case None =>
          error("NegDouble.apply requires a float literal")
      }

    /** Compile-time factory for creating a [[NegDouble]] from a `Double` literal.
      *
      * Rejects non-negative literals at compile time. For runtime values, use
      * [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton `Double` literal type
      * @param d the `Double` literal to validate
      * @return the validated literal as a [[NegDouble]]
      * @throws scala.compiletime.error if the literal is non-negative or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): NegDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v >= 0.0 then
            error("NegDouble cannot be instantiated with a non-negative double literal")
          else
            v.asInstanceOf[NegDouble]
        case None =>
          error("NegDouble.apply requires a double literal")
      }

    def apply(d: Int): NegDouble = ensuringValid(d.toDouble)
    /** Blocking Long apply to prevent precision loss. Long to Double can lose precision for values > 2^53. */
    def apply(d: Long): NegDouble =
      throw new AssertionError("NegDouble.apply from Long is not supported due to potential precision loss. Use explicit toDouble: NegDouble(x.toDouble)")
    def apply(d: Float): NegDouble = ensuringValid(d.toDouble)
    def apply(d: Double): NegDouble = ensuringValid(d)

    /** Construct a [[NegDouble]] from a runtime `Double` if it is strictly negative.
      *
      * @param d the `Double` to validate
      * @return `Some(NegDouble)` when `d < 0.0`, else `None`
      */
    def from(d: Double): Option[NegDouble] =
      if (isValid(d)) Some(d) else None

    /** Validate and return the given `Double` as [[NegDouble]].
      *
      * @param d the `Double` to validate
      * @return the validated value as a [[NegDouble]]
      * @throws AssertionError if `d` is non-negative
      */
    def ensuringValid(d: Double): NegDouble =
      if (d >= 0.0)
        throw new AssertionError(Resources.invalidNegDouble)
      else d

    /** Runtime factory that returns `Success` for valid input, `Failure` otherwise.
      *
      * @param value the `Double` to validate
      * @return `Success(NegDouble)` if `value < 0.0`, else `Failure(AssertionError)`
      */
    def tryingValid(value: Double): Try[NegDouble] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegDouble))

    /** Predicate indicating whether the given `Double` is valid for [[NegDouble]].
      *
      * @param value the `Double` to inspect
      * @return `true` if `value < 0.0`, otherwise `false`
      */
    def isValid(value: Double): Boolean = value < 0.0

    /** Validate a value and return `Pass`, else `Fail(f(value))`.
      *
      * @tparam E the error type produced by `f`
      * @param value the `Double` to validate
      * @param f function used to compute an error value when `value` is invalid
      * @return `Pass` for valid input, else `Fail(f(value))`
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return `Good(NegDouble)`, else `Bad(f(value))`.
      *
      * @tparam B the error type produced by `f`
      * @param value the `Double` to validate
      * @param f function used to compute an error value when `value` is invalid
      * @return `Good(NegDouble)` for valid input, else `Bad(f(value))`
      */
    def goodOrElse[B](value: Double)(f: Double => B): NegDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Validate a value and return `Right(NegDouble)`, else `Left(f(value))`.
      *
      * @tparam L the error type produced by `f`
      * @param value the `Double` to validate
      * @param f function used to compute an error value when `value` is invalid
      * @return `Right(NegDouble)` for valid input, else `Left(f(value))`
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NegDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    def fromOrElse(value: Double, default: => NegDouble): NegDouble =
      if (isValid(value)) value else default

    /** Largest valid [[NegDouble]] value, equal to `-Double.MinPositiveValue`. */
    val MaxValue: NegDouble = -Double.MinPositiveValue

    /** Smallest valid [[NegDouble]] value, equal to `Double.MinValue`. */
    val MinValue: NegDouble = Double.MinValue

    /** Negative infinity as a [[NegDouble]], equal to `Double.NegativeInfinity`. */
    val NegativeInfinity: NegDouble = Double.NegativeInfinity

    extension (p: NegDouble) {
      /** Return the underlying `Double` value. */
      def value: Double = p

      /** Return `true` if this value is negative infinity, `false` otherwise. */
      def isNegInfinity: Boolean = p == Double.NegativeInfinity

      /** Return `true` if this value is finite (not positive or negative infinity). */
      def isFinite: Boolean = !p.isInfinite

      /** Unary plus returns this value unchanged. */
      def unary_+ : NegDouble = p

      /** Numeric negation, returning a positive [[PosDoubles.PosDouble]]. */
      def unary_- : PosDoubles.PosDouble = PosDoubles.PosDouble.ensuringValid(-p.toDouble)

      /** Add another [[NegZDouble]] and revalidate the result.
        *
        * @param x the addend
        * @return the validated sum
        * @throws AssertionError if the sum is non-negative
        */
      def plus(x: NegZDouble): NegDouble = NegDouble.ensuringValid(value + x)
      def plus(x: NegInts.NegInt): NegDouble = NegDouble.ensuringValid(value + x.value.toDouble)
      def max(that: NegDouble): NegDouble = math.max(p, that)
      def min(that: NegDouble): NegDouble = math.min(p, that)
      def isWhole: Boolean = {
        val longValue = p.toLong
        longValue.toDouble == p || longValue == Long.MaxValue && p < Double.PositiveInfinity || longValue == Long.MinValue && p > Double.NegativeInfinity
      }
      def round: NegZLong = NegZLong.ensuringValid(math.round(value))
      def ceil: NegZDouble = NegZDouble.ensuringValid(math.ceil(value))
      def floor: NegDouble = NegDouble.ensuringValid(math.floor(value))
      def toRadians: Double = math.toRadians(value)
      def toDegrees: Double = math.toDegrees(value)
      def ensuringValid(f: Double => Double): NegDouble = {
        val candidateResult: Double = f(value)
        if (NegDouble.isValid(candidateResult)) NegDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${value.toString()}, was not a valid NegDouble")
      }
    }

    /** Implicitly widens a [[NegDouble]] to a plain <code>Double</code>. */
    given Conversion[NegDouble, Double] with {
      def apply(x: NegDouble): Double = x.value
    }

    /** Widens a [[NegDouble]] to a [[NonZeroDoubles.NonZeroDouble]]. */
    given Conversion[NegDouble, NonZeroDoubles.NonZeroDouble] with {
      def apply(x: NegDouble): NonZeroDoubles.NonZeroDouble = NonZeroDoubles.NonZeroDouble.ensuringValid(x.toDouble)
    }

    given Conversion[Double, NegDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v >= 0.0 then
              error("NegDouble cannot be instantiated with a non-negative double literal")
            else
              v.asInstanceOf[NegDouble]
          case None =>
            error("NegDouble conversion requires a double literal")
        }

      def apply(x: Double): NegDouble = NegDouble.ensuringValid(x)

            given Conversion[Int, NegZDouble] with {
              inline def apply[I <: Int & Singleton](inline x: I): NegZDouble =
                inline constValueOpt[I] match {
                  case Some(v: Int) =>
                    inline if v > 0 then
                      error("NegZDouble cannot be instantiated with a positive integer literal")
                    else
                      v.toDouble.asInstanceOf[NegZDouble]
                  case None =>
                    error("NegZDouble conversion requires an integer literal")
                }

              def apply(x: Int): NegZDouble = NegZDouble.ensuringValid(x.toDouble)
            }

            given Conversion[Long, NegZDouble] with {
              inline def apply[L <: Long & Singleton](inline x: L): NegZDouble =
                inline constValueOpt[L] match {
                  case Some(v: Long) =>
                    inline if v > 0L then
                      error("NegZDouble cannot be instantiated with a positive long literal")
                    else
                      v.toDouble.asInstanceOf[NegZDouble]
                  case None =>
                    error("NegZDouble conversion requires a long literal")
                }

              def apply(x: Long): NegZDouble = NegZDouble.ensuringValid(x.toDouble)
            }

            given Conversion[Float, NegZDouble] with {
              inline def apply[F <: Float & Singleton](inline x: F): NegZDouble =
                inline constValueOpt[F] match {
                  case Some(v: Float) =>
                    inline if v > 0.0f then
                      error("NegZDouble cannot be instantiated with a positive float literal")
                    else
                      v.toDouble.asInstanceOf[NegZDouble]
                  case None =>
                    error("NegZDouble conversion requires a float literal")
                }

              def apply(x: Float): NegZDouble = NegZDouble.ensuringValid(x.toDouble)
            }
    }

    given Conversion[Int, NegDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): NegDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v >= 0 then
              error("NegDouble cannot be instantiated with a non-negative integer literal")
            else
              v.toDouble.asInstanceOf[NegDouble]
          case None =>
            error("NegDouble conversion requires an integer literal")
        }

      def apply(x: Int): NegDouble = NegDouble.ensuringValid(x.toDouble)
    }

    /** Blocking Long conversion to prevent precision loss. Long to Double can lose precision for values > 2^53. */
    given Conversion[Long, NegDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): NegDouble =
        error("NegDouble conversion from Long is not supported due to potential precision loss. Use explicit toDouble: NegDouble(x.toDouble)")

      def apply(x: Long): NegDouble =
        throw new AssertionError("NegDouble conversion from Long is not supported due to potential precision loss. Use explicit toDouble: NegDouble(x.toDouble)")
    }

    given Conversion[Float, NegDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v >= 0.0f then
              error("NegDouble cannot be instantiated with a non-negative float literal")
            else
              v.toDouble.asInstanceOf[NegDouble]
          case None =>
            error("NegDouble conversion requires a float literal")
        }

      def apply(x: Float): NegDouble = NegDouble.ensuringValid(x.toDouble)
    }

    /** Ordering instance based on underlying numeric `Double` ordering. */
    given Ordering[NegDouble] with {
      def compare(x: NegDouble, y: NegDouble): Int = x.compareTo(y)
    }
  }

  /** Opaque type representing any finite `Double` value ≤ 0.0 (non-positive, finite doubles).
    *
    * Excludes infinities and NaN. This is a strict subtype of [[NegZDouble]].
    * Values that are also strictly negative can be represented by [[NegFiniteDouble]].
    */
  opaque type NegZFiniteDouble <: NegZDouble = Double

  /** Companion object for [[NegZFiniteDouble]] with compile-time and runtime factory methods,
    * validation helpers, implicit conversions, and extension methods.
    */
  object NegZFiniteDouble {
    inline def apply[I <: Int & Singleton](inline d: I): NegZFiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v > 0 then
            error(Resources.notValidNegZFiniteDouble)
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error(Resources.notLiteralNegZFiniteDouble)
      }

    /** Compile-time factory for creating a [[NegZFiniteDouble]] from a `Float` literal.
      *
      * Rejects positive, infinite, and NaN literals at compile time.
      *
      * @tparam F the singleton `Float` literal type
      * @param d the `Float` literal to validate
      * @return the validated literal as a [[NegZFiniteDouble]] widened to `Double`
      * @throws scala.compiletime.error if the literal is positive, infinite, or NaN
      */
    inline def apply[F <: Float & Singleton](inline d: F): NegZFiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v > 0.0f then
            error(Resources.notValidNegZFiniteDouble)
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error(Resources.notLiteralNegZFiniteDouble)
      }

    /** Compile-time factory for creating a [[NegZFiniteDouble]] from a `Double` literal.
      *
      * Rejects positive, infinite, and NaN literals at compile time.
      *
      * @tparam D the singleton `Double` literal type
      * @param d the `Double` literal to validate
      * @return the validated literal as a [[NegZFiniteDouble]]
      * @throws scala.compiletime.error if the literal is positive, infinite, or NaN
      */
    inline def apply[D <: Double & Singleton](inline d: D): NegZFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v > 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
            error(Resources.notValidNegZFiniteDouble)
          else
            v.asInstanceOf[NegZFiniteDouble]
        case None =>
          error(Resources.notLiteralNegZFiniteDouble)
      }

    def from(d: Double): Option[NegZFiniteDouble] =
      if (isValid(d)) Some(d) else None

    def ensuringValid(d: Double): NegZFiniteDouble =
      if (d > 0.0 || d == Double.NegativeInfinity || d == Double.PositiveInfinity)
        throw new AssertionError(Resources.invalidNegZDouble)
      else d

    def tryingValid(value: Double): Try[NegZFiniteDouble] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegZDouble))

    def isValid(value: Double): Boolean = value <= 0.0 && value.isFinite

    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    def goodOrElse[B](value: Double)(f: Double => B): NegZFiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NegZFiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    def fromOrElse(value: Double, default: => NegZFiniteDouble): NegZFiniteDouble =
      if (isValid(value)) value else default

    extension (p: NegZFiniteDouble) {
      /** Return the underlying `Double` value. */
      def value: Double = p

      /** Apply a transformation and revalidate the result as a [[NegZFiniteDouble]].
        *
        * @param f transformation applied to the wrapped value
        * @return the validated transformed value
        * @throws AssertionError if the transformed value is invalid
        */
      def ensuringValid(f: Double => Double): NegZFiniteDouble = {
        val candidateResult: Double = f(p)
        if (NegZFiniteDouble.isValid(candidateResult)) NegZFiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid NegZFiniteDouble")
      }
    }

    /** Largest valid [[NegZFiniteDouble]] value, equal to `0.0`. */
    val MaxValue: NegZFiniteDouble = 0.0

    /** Smallest valid [[NegZFiniteDouble]] value, equal to `Double.MinValue`. */
    val MinValue: NegZFiniteDouble = Double.MinValue

    /** Implicitly widens a [[NegZFiniteDouble]] to a plain <code>Double</code>. */
    given Conversion[NegZFiniteDouble, Double] with {
      def apply(x: NegZFiniteDouble): Double = x.value
    }

    /** Widens a [[NegZFiniteDouble]] to a [[NegZDouble]]. */
    given Conversion[NegZFiniteDouble, NegZDouble] with {
      def apply(x: NegZFiniteDouble): NegZDouble = x.toDouble
    }

    /** Widens a [[NegZFiniteDouble]] to a [[NonZeroDoubles.NonZeroDouble]]. */
    given Conversion[NegZFiniteDouble, NonZeroDoubles.NonZeroDouble] with {
      def apply(x: NegZFiniteDouble): NonZeroDoubles.NonZeroDouble = NonZeroDoubles.NonZeroDouble.ensuringValid(x.toDouble)
    }

    /** Convert an <code>Int</code> to [[NegZFiniteDouble]] via compile-time or runtime validation. */
    given Conversion[Int, NegZFiniteDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): NegZFiniteDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v > 0 then
              error(Resources.notValidNegZFiniteDouble)
            else
              v.toDouble.asInstanceOf[NegZFiniteDouble]
          case None =>
            error(Resources.notLiteralNegZFiniteDouble)
        }

      def apply(x: Int): NegZFiniteDouble = NegZFiniteDouble.ensuringValid(x.toDouble)
    }

    given Conversion[Float, NegZFiniteDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegZFiniteDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v > 0.0f then
              error(Resources.notValidNegZFiniteDouble)
            else
              v.toDouble.asInstanceOf[NegZFiniteDouble]
          case None =>
            error(Resources.notLiteralNegZFiniteDouble)
        }

      def apply(x: Float): NegZFiniteDouble = NegZFiniteDouble.ensuringValid(x.toDouble)
    }

    /** Convert a <code>Double</code> to [[NegZFiniteDouble]] via compile-time or runtime validation. */
    given Conversion[Double, NegZFiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegZFiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v > 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
              error(Resources.notValidNegZFiniteDouble)
            else
              v.asInstanceOf[NegZFiniteDouble]
          case None =>
            error(Resources.notLiteralNegZFiniteDouble)
        }

      def apply(x: Double): NegZFiniteDouble = NegZFiniteDouble.ensuringValid(x)
    }

    /** Ordering instance based on underlying numeric `Double` ordering. */
    given Ordering[NegZFiniteDouble] with {
      def compare(x: NegZFiniteDouble, y: NegZFiniteDouble): Int = x.compareTo(y)
    }
  }

  /** Opaque type representing any strictly negative, finite `Double` value (< 0.0, excludes infinities and NaN).
    *
    * This is a strict subtype of [[NegZFiniteDouble]]. Zero is excluded.
    */
  opaque type NegFiniteDouble <: NegZFiniteDouble = Double

  /** Companion object for [[NegFiniteDouble]] with compile-time and runtime factory methods,
    * validation helpers, implicit conversions, and extension methods.
    */
  object NegFiniteDouble {
    inline def apply[I <: Int & Singleton](inline d: I): NegFiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v >= 0 then
            error("NegFiniteDouble cannot be instantiated with a non-negative integer literal")
          else
            v.toDouble.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires an integer literal")
      }

    /** Compile-time factory for creating a [[NegFiniteDouble]] from a `Float` literal.
      *
      * Rejects non-negative, infinite, and NaN literals at compile time.
      *
      * @tparam F the singleton `Float` literal type
      * @param d the `Float` literal to validate
      * @return the validated literal as a [[NegFiniteDouble]] widened to `Double`
      * @throws scala.compiletime.error if the literal is non-negative, infinite, or NaN
      */
    inline def apply[F <: Float & Singleton](inline d: F): NegFiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v >= 0.0f then
            error("NegFiniteDouble cannot be instantiated with a non-negative float literal")
          else
            v.toDouble.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires a float literal")
      }

    /** Compile-time factory for creating a [[NegFiniteDouble]] from a `Double` literal.
      *
      * Rejects non-negative, infinite, and NaN literals at compile time.
      *
      * @tparam D the singleton `Double` literal type
      * @param d the `Double` literal to validate
      * @return the validated literal as a [[NegFiniteDouble]]
      * @throws scala.compiletime.error if the literal is non-negative, infinite, or NaN
      */
    inline def apply[D <: Double & Singleton](inline d: D): NegFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v >= 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
            error("NegFiniteDouble cannot be instantiated with a non-negative double literal or infinity")
          else
            v.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires a double literal")
      }

    def from(d: Double): Option[NegFiniteDouble] =
      if (isValid(d)) Some(d) else None

    def ensuringValid(d: Double): NegFiniteDouble =
      if (d >= 0.0 || d == Double.NegativeInfinity || d == Double.PositiveInfinity)
        throw new AssertionError(Resources.invalidNegDouble)
      else d

    def tryingValid(value: Double): Try[NegFiniteDouble] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegDouble))

    def isValid(value: Double): Boolean = value < 0.0 && value.isFinite

    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    def goodOrElse[B](value: Double)(f: Double => B): NegFiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NegFiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    def fromOrElse(value: Double, default: => NegFiniteDouble): NegFiniteDouble =
      if (isValid(value)) value else default

    extension (p: NegFiniteDouble) {
      /** Return the underlying `Double` value. */
      def value: Double = p

      /** Apply a transformation and revalidate the result as a [[NegFiniteDouble]].
        *
        * @param f transformation applied to the wrapped value
        * @return the validated transformed value
        * @throws AssertionError if the transformed value is invalid
        */
      def ensuringValid(f: Double => Double): NegFiniteDouble = {
        val candidateResult: Double = f(p)
        if (NegFiniteDouble.isValid(candidateResult)) NegFiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid NegFiniteDouble")
      }
    }

    /** Largest valid [[NegFiniteDouble]] value, equal to `-Double.MinPositiveValue`. */
    val MaxValue: NegFiniteDouble = -Double.MinPositiveValue

    /** Smallest valid [[NegFiniteDouble]] value, equal to `Double.MinValue`. */
    val MinValue: NegFiniteDouble = Double.MinValue

    /** Implicitly widens a [[NegFiniteDouble]] to a plain <code>Double</code>. */
    given Conversion[NegFiniteDouble, Double] with {
      def apply(x: NegFiniteDouble): Double = x.value
    }

    /** Widens a [[NegFiniteDouble]] to a [[NegDouble]]. */
    given Conversion[NegFiniteDouble, NegDouble] with {
      def apply(x: NegFiniteDouble): NegDouble = x.toDouble
    }

    /** Widens a [[NegFiniteDouble]] to a [[NonZeroDoubles.NonZeroDouble]]. */
    given Conversion[NegFiniteDouble, NonZeroDoubles.NonZeroDouble] with {
      def apply(x: NegFiniteDouble): NonZeroDoubles.NonZeroDouble = NonZeroDoubles.NonZeroDouble.ensuringValid(x.toDouble)
    }

    given Conversion[Int, NegFiniteDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): NegFiniteDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v >= 0 then
              error(Resources.notValidNegFiniteDouble)
            else
              v.toDouble.asInstanceOf[NegFiniteDouble]
          case None =>
            error(Resources.notLiteralNegFiniteDouble)
        }

      def apply(x: Int): NegFiniteDouble = NegFiniteDouble.ensuringValid(x.toDouble)
    }

    /** Convert a <code>Float</code> to [[NegFiniteDouble]] via compile-time or runtime validation. */
    given Conversion[Float, NegFiniteDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegFiniteDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v >= 0.0f || v == Float.PositiveInfinity || v == Float.NegativeInfinity then
              error(Resources.notValidNegFiniteDouble)
            else
              v.toDouble.asInstanceOf[NegFiniteDouble]
          case None =>
            error(Resources.notLiteralNegFiniteDouble)
        }

      def apply(x: Float): NegFiniteDouble = NegFiniteDouble.ensuringValid(x.toDouble)
    }

    /** Convert a <code>Double</code> to [[NegFiniteDouble]] via compile-time or runtime validation. */
    given Conversion[Double, NegFiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegFiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v >= 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
              error(Resources.notValidNegFiniteDouble)
            else
              v.asInstanceOf[NegFiniteDouble]
          case None =>
            error(Resources.notLiteralNegFiniteDouble)
        }

      def apply(x: Double): NegFiniteDouble = NegFiniteDouble.ensuringValid(x)
    }
  }
}