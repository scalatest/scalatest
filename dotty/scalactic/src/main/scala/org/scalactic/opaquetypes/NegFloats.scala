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
import scala.compiletime.{constValueOpt, error}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

import org.scalactic.{Bad, Fail, Good, Or, Pass, Validation}

/** Opaque negative-float types and companion helpers.
  *
  * This family mirrors the negative anyvals surface for `Float`, including
  * the strictly negative and negative-or-zero variants, along with finite
  * counterparts.
  */
object NegFloats {
  /** Opaque type for negative-or-zero `Float` values. */
  opaque type NegZFloat = Float

  /** Companion for [[NegZFloat]] with construction, validation, and math helpers. */
  object NegZFloat {
    /** Compile-time factory for a negative-or-zero float literal.
      *
      * @tparam F singleton float literal type
      * @param f the candidate float literal
      * @return a validated [[NegZFloat]]
      */
    inline def apply[F <: Float & Singleton](inline f: F): NegZFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v > 0.0f then
            error("NegZFloat cannot be instantiated with a positive float literal")
          else
            v.asInstanceOf[NegZFloat]
        case None =>
          error("NegZFloat.apply requires a float literal")
      }

    /** Return `Some` when the passed value is a valid [[NegZFloat]], otherwise `None`.
      *
      * @param f the candidate value
      * @return `Some` when `f <= 0.0f`, else `None`
      */
    def from(f: Float): Option[NegZFloat] =
      if (isValid(f)) Some(f) else None

    /** Validate and return a [[NegZFloat]], throwing when the value is positive.
      *
      * @param f the candidate value
      * @return the validated [[NegZFloat]]
      * @throws AssertionError if `f > 0.0f`
      */
    def ensuringValid(f: Float): NegZFloat =
      if (f > 0.0f)
        throw new AssertionError(Resources.invalidNegZFloat)
      else f

    /** Return `Success` for a valid value or `Failure` with an assertion error otherwise.
      *
      * @param value the candidate value
      * @return `Success` when the value is valid, otherwise `Failure`
      */
    def tryingValid(value: Float): Try[NegZFloat] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegZFloat))

    /** Test whether a value is a valid [[NegZFloat]].
      *
      * @param value the candidate value
      * @return `true` when the value is less than or equal to `0.0f`
      */
    def isValid(value: Float): Boolean = value <= 0.0f

    /** Return `Pass` for a valid value, or `Fail` with an error value otherwise.
      *
      * @param value the candidate value
      * @param f function used to produce the failure payload when invalid
      * @return `Pass` when the value is valid, otherwise `Fail`
      */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Return `Good` for a valid value, or `Bad` with an error value otherwise.
      *
      * @param value the candidate value
      * @param f function used to produce the failure payload when invalid
      * @return `Good` when the value is valid, otherwise `Bad`
      */
    def goodOrElse[B](value: Float)(f: Float => B): NegZFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Return `Right` for a valid value, or `Left` with an error value otherwise.
      *
      * @param value the candidate value
      * @param f function used to produce the failure payload when invalid
      * @return `Right` when the value is valid, otherwise `Left`
      */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, NegZFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return the validated value or a provided default when invalid.
      *
      * @param value the candidate value
      * @param default fallback value used when `value` is invalid
      * @return the validated value or `default`
      */
    def fromOrElse(value: Float, default: => NegZFloat): NegZFloat =
      if (isValid(value)) value else default

    /** Largest valid [[NegZFloat]] value, equal to `0.0f`. */
    val MaxValue: NegZFloat = 0.0f
    /** Smallest valid [[NegZFloat]] value, equal to `Float.MinValue`. */
    val MinValue: NegZFloat = Float.MinValue
    /** Negative infinity as a [[NegZFloat]]. */
    val NegativeInfinity: NegZFloat = Float.NegativeInfinity

    extension (p: NegZFloat) {
      /** Return the underlying `Float` value.
        *
        * @return the wrapped `Float`
        */
      def value: Float = p
      /** Return `true` when this value is negative infinity.
        *
        * @return `true` when `p == Float.NegativeInfinity`
        */
      def isNegInfinity: Boolean = p == Float.NegativeInfinity
      /** Return `true` when this value is finite.
        *
        * @return `true` when the wrapped value is neither positive nor negative infinity
        */
      def isFinite: Boolean = !p.isInfinite
      /** Unary plus returns this value unchanged.
        *
        * @return this value
        */
      def unary_+ : NegZFloat = p
      /** Add another negative-or-zero float and revalidate the result.
        *
        * @param x the addend
        * @return the validated sum
        * @throws AssertionError if the sum is positive
        */
      def plus(x: NegZFloat): NegZFloat = NegZFloat.ensuringValid(value + x)
      /** Return the greater of this and that value.
        *
        * @param that the comparison value
        * @return the greater of the two values
        */
      def max(that: NegZFloat): NegZFloat = math.max(p, that)
      /** Return the lesser of this and that value.
        *
        * @param that the comparison value
        * @return the lesser of the two values
        */
      def min(that: NegZFloat): NegZFloat = math.min(p, that)
      /** Return `true` when the value is mathematically whole.
        *
        * @return `true` when the wrapped value has no fractional component
        */
      def isWhole: Boolean = {
        val longValue = p.toLong
        longValue.toFloat == p || longValue == Long.MaxValue && p < Float.PositiveInfinity || longValue == Long.MinValue && p > Float.NegativeInfinity
      }
      /** Round to the nearest whole number as a [[NegZInt]].
        *
        * @return the rounded value as a [[NegZInt]]
        */
      def round: NegZInt = NegZInt.ensuringValid(math.round(value))
      /** Return the smallest whole-number value greater than or equal to this value.
        *
        * @return the ceiling value as a [[NegZFloat]]
        */
      def ceil: NegZFloat = NegZFloat.ensuringValid(math.ceil(value).toFloat)
      /** Return the greatest whole-number value less than or equal to this value.
        *
        * @return the floor value as a [[NegZFloat]]
        */
      def floor: NegZFloat = NegZFloat.ensuringValid(math.floor(value).toFloat)
      /** Convert degrees to radians.
        *
        * @return the approximate radian value
        */
      def toRadians: Float = math.toRadians(value.toDouble).toFloat
      /** Convert radians to degrees.
        *
        * @return the approximate degree value
        */
      def toDegrees: Float = math.toDegrees(value.toDouble).toFloat
      /** Apply a transformation and revalidate the result as a [[NegZFloat]].
        *
        * @param f transformation applied to the wrapped value
        * @return the validated transformed value
        * @throws AssertionError if the transformed value is positive
        */
      def ensuringValid(f: Float => Float): NegZFloat = {
        val candidateResult: Float = f(value)
        if (NegZFloat.isValid(candidateResult)) NegZFloat.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${value.toString()}, was not a valid NegZFloat")
      }
    }

    given Conversion[NegZFloat, Float] with {
      def apply(x: NegZFloat): Float = x.toFloat
    }

    given Conversion[Float, NegZFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegZFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v > 0.0f then
              error("NegZFloat cannot be instantiated with a positive float literal")
            else
              v.asInstanceOf[NegZFloat]
          case None =>
            error("NegZFloat conversion requires a float literal")
        }

      def apply(x: Float): NegZFloat = NegZFloat.ensuringValid(x)
    }

    given Ordering[NegZFloat] with {
      def compare(x: NegZFloat, y: NegZFloat): Int = x.compareTo(y)
    }
  }

  /** Opaque type for strictly negative `Float` values. */
  opaque type NegFloat <: NegZFloat = Float

  /** Companion for [[NegFloat]] with construction, validation, and math helpers. */
  object NegFloat {
    /** Compile-time factory for a strictly negative float literal.
      *
      * @tparam F singleton float literal type
      * @param f the candidate float literal
      * @return a validated [[NegFloat]]
      */
    inline def apply[F <: Float & Singleton](inline f: F): NegFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v >= 0.0f then
            error("NegFloat cannot be instantiated with a non-negative float literal")
          else
            v.asInstanceOf[NegFloat]
        case None =>
          error("NegFloat.apply requires a float literal")
      }

    /** Return `Some` when the passed value is a valid [[NegFloat]], otherwise `None`.
      *
      * @param f the candidate value
      * @return `Some` when `f < 0.0f`, else `None`
      */
    def from(f: Float): Option[NegFloat] =
      if (isValid(f)) Some(f) else None

    /** Validate and return a [[NegFloat]], throwing when the value is non-negative.
      *
      * @param f the candidate value
      * @return the validated [[NegFloat]]
      * @throws AssertionError if `f >= 0.0f`
      */
    def ensuringValid(f: Float): NegFloat =
      if (f >= 0.0f)
        throw new AssertionError(Resources.invalidNegFloat)
      else f

    /** Return `Success` for a valid value or `Failure` with an assertion error otherwise.
      *
      * @param value the candidate value
      * @return `Success` when the value is valid, otherwise `Failure`
      */
    def tryingValid(value: Float): Try[NegFloat] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegFloat))

    /** Test whether a value is a valid [[NegFloat]].
      *
      * @param value the candidate value
      * @return `true` when the value is strictly less than `0.0f`
      */
    def isValid(value: Float): Boolean = value < 0.0f

    /** Return `Pass` for a valid value, or `Fail` with an error value otherwise.
      *
      * @param value the candidate value
      * @param f function used to produce the failure payload when invalid
      * @return `Pass` when the value is valid, otherwise `Fail`
      */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Return `Good` for a valid value, or `Bad` with an error value otherwise.
      *
      * @param value the candidate value
      * @param f function used to produce the failure payload when invalid
      * @return `Good` when the value is valid, otherwise `Bad`
      */
    def goodOrElse[B](value: Float)(f: Float => B): NegFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Return `Right` for a valid value, or `Left` with an error value otherwise.
      *
      * @param value the candidate value
      * @param f function used to produce the failure payload when invalid
      * @return `Right` when the value is valid, otherwise `Left`
      */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, NegFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return the validated value or a provided default when invalid.
      *
      * @param value the candidate value
      * @param default fallback value used when `value` is invalid
      * @return the validated value or `default`
      */
    def fromOrElse(value: Float, default: => NegFloat): NegFloat =
      if (isValid(value)) value else default

    /** Largest valid [[NegFloat]] value, equal to `-Float.MinPositiveValue`. */
    val MaxValue: NegFloat = -Float.MinPositiveValue
    /** Smallest valid [[NegFloat]] value, equal to `Float.MinValue`. */
    val MinValue: NegFloat = Float.MinValue
    /** Negative infinity as a [[NegFloat]]. */
    val NegativeInfinity: NegFloat = Float.NegativeInfinity

    extension (p: NegFloat) {
      /** Return the underlying `Float` value.
        *
        * @return the wrapped `Float`
        */
      def value: Float = p
      /** Return `true` when this value is negative infinity.
        *
        * @return `true` when `p == Float.NegativeInfinity`
        */
      def isNegInfinity: Boolean = p == Float.NegativeInfinity
      /** Return `true` when this value is finite.
        *
        * @return `true` when the wrapped value is neither positive nor negative infinity
        */
      def isFinite: Boolean = !p.isInfinite
      /** Unary plus returns this value unchanged.
        *
        * @return this value
        */
      def unary_+ : NegFloat = p
      /** Unary minus converts this negative float to a positive [[PosFloat]].
        *
        * @return the positive counterpart of this value
        */
      def unary_- : PosFloat = PosFloats.PosFloat.ensuringValid(-p.toFloat)
      /** Add another negative-or-zero float and revalidate the result.
        *
        * @param x the addend
        * @return the validated sum
        * @throws AssertionError if the sum is non-negative
        */
      def plus(x: NegZFloat): NegFloat = NegFloat.ensuringValid(value + x)
      /** Add a negative integer and revalidate the result.
        *
        * @param x the addend
        * @return the validated sum
        * @throws AssertionError if the sum is non-negative
        */
      def plus(x: NegInts.NegInt): NegFloat = NegFloat.ensuringValid(value + x.value.toFloat)
      /** Return the greater of this and that value.
        *
        * @param that the comparison value
        * @return the greater of the two values
        */
      def max(that: NegFloat): NegFloat = math.max(p, that)
      /** Return the lesser of this and that value.
        *
        * @param that the comparison value
        * @return the lesser of the two values
        */
      def min(that: NegFloat): NegFloat = math.min(p, that)
      /** Return `true` when the value is mathematically whole.
        *
        * @return `true` when the wrapped value has no fractional component
        */
      def isWhole: Boolean = {
        val longValue = p.toLong
        longValue.toFloat == p || longValue == Long.MaxValue && p < Float.PositiveInfinity || longValue == Long.MinValue && p > Float.NegativeInfinity
      }
      /** Round to the nearest whole number as a [[NegZInt]].
        *
        * @return the rounded value as a [[NegZInt]]
        */
      def round: NegZInt = NegZInt.ensuringValid(math.round(value))
      /** Return the smallest whole-number value greater than or equal to this value.
        *
        * @return the ceiling value as a [[NegZFloat]]
        */
      def ceil: NegZFloat = NegZFloat.ensuringValid(math.ceil(value).toFloat)
      /** Return the greatest whole-number value less than or equal to this value.
        *
        * @return the floor value as a [[NegFloat]]
        */
      def floor: NegFloat = NegFloat.ensuringValid(math.floor(value).toFloat)
      /** Convert degrees to radians.
        *
        * @return the approximate radian value
        */
      def toRadians: Float = math.toRadians(value.toDouble).toFloat
      /** Convert radians to degrees.
        *
        * @return the approximate degree value
        */
      def toDegrees: Float = math.toDegrees(value.toDouble).toFloat
      /** Apply a transformation and revalidate the result as a [[NegFloat]].
        *
        * @param f transformation applied to the wrapped value
        * @return the validated transformed value
        * @throws AssertionError if the transformed value is non-negative
        */
      def ensuringValid(f: Float => Float): NegFloat = {
        val candidateResult: Float = f(value)
        if (NegFloat.isValid(candidateResult)) NegFloat.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${value.toString()}, was not a valid NegFloat")
      }
    }

    given Conversion[NegFloat, Float] with {
      def apply(x: NegFloat): Float = x.toFloat
    }

    given Conversion[Float, NegFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v >= 0.0f then
              error("NegFloat cannot be instantiated with a non-negative float literal")
            else
              v.asInstanceOf[NegFloat]
          case None =>
            error("NegFloat conversion requires a float literal")
        }

      def apply(x: Float): NegFloat = NegFloat.ensuringValid(x)
    }

  }

  /** Opaque type for finite negative-or-zero `Float` values. */
  opaque type NegZFiniteFloat <: NegZFloat = Float

  /** Companion for [[NegZFiniteFloat]] with validation helpers. */
  object NegZFiniteFloat {
    /** Compile-time factory for a finite negative-or-zero float literal.
      *
      * @tparam F singleton float literal type
      * @param f the candidate float literal
      * @return a validated [[NegZFiniteFloat]]
      */
    inline def apply[F <: Float & Singleton](inline f: F): NegZFiniteFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v > 0.0f || v == Float.PositiveInfinity || v == Float.NegativeInfinity then
            error("NegZFiniteFloat cannot be instantiated with a positive float literal or infinity")
          else
            v.asInstanceOf[NegZFiniteFloat]
        case None =>
          error("NegZFiniteFloat.apply requires a float literal")
      }

    /** Return `Some` when the passed value is a valid [[NegZFiniteFloat]], otherwise `None`.
      *
      * @param f the candidate value
      * @return `Some` when the value is finite and non-positive, else `None`
      */
    def from(f: Float): Option[NegZFiniteFloat] =
      if (isValid(f)) Some(f) else None

    /** Validate and return a [[NegZFiniteFloat]], throwing when the value is invalid.
      *
      * @param f the candidate value
      * @return the validated [[NegZFiniteFloat]]
      * @throws AssertionError if `f` is positive or infinite
      */
    def ensuringValid(f: Float): NegZFiniteFloat =
      if (f > 0.0f || f == Float.NegativeInfinity || f == Float.PositiveInfinity)
        throw new AssertionError(Resources.invalidNegZFloat)
      else f

    /** Return `Success` for a valid value or `Failure` with an assertion error otherwise.
      *
      * @param value the candidate value
      * @return `Success` when the value is valid, otherwise `Failure`
      */
    def tryingValid(value: Float): Try[NegZFiniteFloat] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegZFloat))

    /** Test whether a value is a valid [[NegZFiniteFloat]].
      *
      * @param value the candidate value
      * @return `true` when the value is finite and less than or equal to `0.0f`
      */
    def isValid(value: Float): Boolean = value <= 0.0f && value.isFinite

    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    def goodOrElse[B](value: Float)(f: Float => B): NegZFiniteFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    def rightOrElse[L](value: Float)(f: Float => L): Either[L, NegZFiniteFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    def fromOrElse(value: Float, default: => NegZFiniteFloat): NegZFiniteFloat =
      if (isValid(value)) value else default

    /** Largest valid [[NegZFiniteFloat]] value, equal to `0.0f`.
      *
      * @return the greatest valid value
      */
    val MaxValue: NegZFiniteFloat = 0.0f
    /** Smallest valid [[NegZFiniteFloat]] value, equal to `Float.MinValue`.
      *
      * @return the least valid finite value
      */
    val MinValue: NegZFiniteFloat = Float.MinValue

    given Conversion[NegZFiniteFloat, Float] with {
      def apply(x: NegZFiniteFloat): Float = x.toFloat
    }

    given Conversion[Float, NegZFiniteFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegZFiniteFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v > 0.0f || v == Float.PositiveInfinity || v == Float.NegativeInfinity then
              error("NegZFiniteFloat cannot be instantiated with a positive float literal or infinity")
            else
              v.asInstanceOf[NegZFiniteFloat]
          case None =>
            error("NegZFiniteFloat conversion requires a float literal")
        }

      def apply(x: Float): NegZFiniteFloat = NegZFiniteFloat.ensuringValid(x)
    }

    given Ordering[NegZFiniteFloat] with {
      def compare(x: NegZFiniteFloat, y: NegZFiniteFloat): Int = x.compareTo(y)
    }
  }

  /** Opaque type for finite strictly negative `Float` values. */
  opaque type NegFiniteFloat <: NegFloat = Float

  /** Companion for [[NegFiniteFloat]] with validation helpers. */
  object NegFiniteFloat {
    /** Compile-time factory for a finite strictly negative float literal.
      *
      * @tparam F singleton float literal type
      * @param f the candidate float literal
      * @return a validated [[NegFiniteFloat]]
      */
    inline def apply[F <: Float & Singleton](inline f: F): NegFiniteFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v >= 0.0f || v == Float.PositiveInfinity || v == Float.NegativeInfinity then
            error("NegFiniteFloat cannot be instantiated with a non-negative float literal or infinity")
          else
            v.asInstanceOf[NegFiniteFloat]
        case None =>
          error("NegFiniteFloat.apply requires a float literal")
      }

    /** Return `Some` when the passed value is a valid [[NegFiniteFloat]], otherwise `None`.
      *
      * @param f the candidate value
      * @return `Some` when the value is finite and negative, else `None`
      */
    def from(f: Float): Option[NegFiniteFloat] =
      if (isValid(f)) Some(f) else None

    /** Validate and return a [[NegFiniteFloat]], throwing when the value is invalid.
      *
      * @param f the candidate value
      * @return the validated [[NegFiniteFloat]]
      * @throws AssertionError if `f` is non-negative or infinite
      */
    def ensuringValid(f: Float): NegFiniteFloat =
      if (f >= 0.0f || f == Float.NegativeInfinity || f == Float.PositiveInfinity)
        throw new AssertionError(Resources.invalidNegFloat)
      else f

    /** Return `Success` for a valid value or `Failure` with an assertion error otherwise.
      *
      * @param value the candidate value
      * @return `Success` when the value is valid, otherwise `Failure`
      */
    def tryingValid(value: Float): Try[NegFiniteFloat] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegFloat))

    /** Test whether a value is a valid [[NegFiniteFloat]].
      *
      * @param value the candidate value
      * @return `true` when the value is finite and strictly less than `0.0f`
      */
    def isValid(value: Float): Boolean = value < 0.0f && value.isFinite

    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    def goodOrElse[B](value: Float)(f: Float => B): NegFiniteFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    def rightOrElse[L](value: Float)(f: Float => L): Either[L, NegFiniteFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    def fromOrElse(value: Float, default: => NegFiniteFloat): NegFiniteFloat =
      if (isValid(value)) value else default

    /** Largest valid [[NegFiniteFloat]] value, equal to `-Float.MinPositiveValue`.
      *
      * @return the greatest valid value
      */
    val MaxValue: NegFiniteFloat = -Float.MinPositiveValue
    /** Smallest valid [[NegFiniteFloat]] value, equal to `Float.MinValue`.
      *
      * @return the least valid finite value
      */
    val MinValue: NegFiniteFloat = Float.MinValue

    given Conversion[NegFiniteFloat, Float] with {
      def apply(x: NegFiniteFloat): Float = x.toFloat
    }

    given Conversion[Float, NegFiniteFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegFiniteFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v >= 0.0f || v == Float.PositiveInfinity || v == Float.NegativeInfinity then
              error("NegFiniteFloat cannot be instantiated with a non-negative float literal or infinity")
            else
              v.asInstanceOf[NegFiniteFloat]
          case None =>
            error("NegFiniteFloat conversion requires a float literal")
        }

      def apply(x: Float): NegFiniteFloat = NegFiniteFloat.ensuringValid(x)
    }

    given Ordering[NegFiniteFloat] with {
      def compare(x: NegFiniteFloat, y: NegFiniteFloat): Int = x.compareTo(y)
    }
  }
}