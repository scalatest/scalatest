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
import org.scalactic.{Bad, Fail, Good, Or, Pass, Validation}
import scala.compiletime.{constValueOpt, error}
import scala.util.{Failure, Success, Try}

object Finites {

  /** Opaque type representing any finite <code>Float</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy <code>isFinite</code>, meaning
    * they are neither positive infinity, negative infinity, nor NaN.
    * </p>
    *
    * <p>
    * Use the compile-time <code>apply</code> overload to construct instances from
    * literals, or the runtime factory methods [[FiniteFloat.from]],
    * [[FiniteFloat.ensuringValid]], and [[FiniteFloat.fromOrElse]] for values
    * known only at runtime.
    * </p>
    */
  opaque type FiniteFloat = Float

  /** Companion object for the [[FiniteFloat]] opaque type. */
  object FiniteFloat {

    /** Implicitly widens a [[FiniteFloat]] to a plain <code>Float</code>. */
    given Conversion[FiniteFloat, Float] with {
      def apply(x: FiniteFloat): Float = x.toFloat
  }

    /** Converts a compile-time <code>Float</code> literal to a [[FiniteFloat]]. */
    given Conversion[Float, FiniteFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): FiniteFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v == Float.PositiveInfinity || v == Float.NegativeInfinity || v != v then
              error("FiniteFloat cannot be instantiated with infinity or NaN")
            else
              v.asInstanceOf[FiniteFloat]
          case None =>
            error("FiniteFloat.apply requires a float literal")
        }

      def apply(x: Float): FiniteFloat = FiniteFloat.ensuringValid(x)
    }

    /** Compile-time factory for creating a [[FiniteFloat]] from a float literal. */
    inline def apply[F <: Float & Singleton](inline f: F): FiniteFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v == Float.PositiveInfinity || v == Float.NegativeInfinity || v != v then
            error("FiniteFloat cannot be instantiated with infinity or NaN")
          else
            v.asInstanceOf[FiniteFloat]
        case None =>
          error("FiniteFloat.apply requires a float literal")
      }

    /** Test whether a value is a valid [[FiniteFloat]]. */
    def isValid(value: Float): Boolean = !(value.isInfinite || value.isNaN)

    /** Return `Some` when the passed value is finite, otherwise `None`. */
    def from(value: Float): Option[FiniteFloat] =
      if (isValid(value)) Some(value) else None

    /** Validate and return a [[FiniteFloat]], throwing when the value is not finite. */
    def ensuringValid(value: Float): FiniteFloat =
      if (isValid(value)) value
      else throw new AssertionError(Resources.invalidPosFiniteFloat)

    /** Return `Success` for a finite value or `Failure` with an assertion error otherwise. */
    def tryingValid(value: Float): Try[FiniteFloat] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidPosFiniteFloat))

    /** Return `Pass` for a finite value, or `Fail` with an error value otherwise. */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Return `Good` for a finite value, or `Bad` with an error value otherwise. */
    def goodOrElse[B](value: Float)(f: Float => B): FiniteFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Return `Right` for a finite value, or `Left` with an error value otherwise. */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, FiniteFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return the validated value or a provided default when invalid. */
    def fromOrElse(value: Float, default: => FiniteFloat): FiniteFloat =
      if (isValid(value)) value else default

    /** Largest valid [[FiniteFloat]] value. */
    val MaxValue: FiniteFloat = Float.MaxValue

    /** Smallest valid [[FiniteFloat]] value. */
    val MinValue: FiniteFloat = Float.MinValue

    /** Smallest positive valid [[FiniteFloat]] value. */
    val MinPositiveValue: FiniteFloat = Float.MinPositiveValue

    /** Ordering instance for FiniteFloat that orders by numeric value. */
    given Ordering[FiniteFloat] with {
      def compare(x: FiniteFloat, y: FiniteFloat): Int = java.lang.Float.compare(x, y)
    }

    extension (p: FiniteFloat) {
      /** Return the underlying Float value. */
      def value: Float = p

      /** Indicates whether this FiniteFloat has no fraction part. */
      def isWhole: Boolean = {
        val longValue = p.toLong
        longValue.toFloat == p || longValue == Long.MaxValue && p < Float.PositiveInfinity || longValue == Long.MinValue && p > Float.NegativeInfinity
      }

      /** Rounds this FiniteFloat to the nearest whole number. */
      def round: Int = math.round(value)

      /** Returns the smallest FiniteFloat that is >= this value and is a mathematical integer. */
      def ceil: FiniteFloat = FiniteFloat.ensuringValid(math.ceil(value).toFloat)

      /** Returns the greatest FiniteFloat that is <= this value and is a mathematical integer. */
      def floor: FiniteFloat = FiniteFloat.ensuringValid(math.floor(value).toFloat)

      /** Converts an angle measured in degrees to an approximately equivalent angle measured in radians. */
      def toRadians: Float = math.toRadians(value.toDouble).toFloat

      /** Converts an angle measured in radians to an approximately equivalent angle measured in degrees. */
      def toDegrees: Float = math.toDegrees(value.toDouble).toFloat

      /** Returns the larger of this value and `other`. */
      def max(other: FiniteFloat): FiniteFloat =
        if (p >= other) p else other

      /** Returns the smaller of this value and `other`. */
      def min(other: FiniteFloat): FiniteFloat =
        if (p <= other) p else other

      /** Returns the negated value as another [[FiniteFloat]]. */
      def unary_- : FiniteFloat = FiniteFloat.ensuringValid(-p)

      /** Applies the passed Float => Float function and ensures the result is a FiniteFloat. */
      def ensuringValid(f: Float => Float): FiniteFloat = {
        val candidateResult: Float = f(p)
        if (FiniteFloat.isValid(candidateResult)) FiniteFloat.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid FiniteFloat")
      }
    }

  }
  /** Opaque type representing any finite <code>Double</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy <code>isFinite</code>, meaning
    * they are neither positive infinity, negative infinity, nor NaN.
    * </p>
    *
    * <p>
    * Use the compile-time <code>apply</code> overloads to construct instances from
    * literals, or the runtime factory methods [[FiniteDouble.from]],
    * [[FiniteDouble.ensuringValid]], and [[FiniteDouble.fromOrElse]] for values
    * known only at runtime.
    * </p>
    */
  opaque type FiniteDouble = Double

  /** Companion object for the [[FiniteDouble]] opaque type. */
  object FiniteDouble {

    /** Implicitly widens a [[FiniteDouble]] to a plain <code>Double</code>. */
    given Conversion[FiniteDouble, Double] with {
      def apply(x: FiniteDouble): Double = x.asInstanceOf[Double]
    }

    /** Converts a compile-time <code>Int</code> literal to a [[FiniteDouble]]. */
    given Conversion[Int, FiniteDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): FiniteDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) => v.toDouble.asInstanceOf[FiniteDouble]
          case None => error("FiniteDouble conversion requires an integer literal")
        }
      def apply(x: Int): FiniteDouble = x.toDouble
    }

    /** Converts a compile-time <code>Float</code> literal to a [[FiniteDouble]]. */
    given Conversion[Float, FiniteDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): FiniteDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v == v && v != Float.PositiveInfinity && v != Float.NegativeInfinity then
              v.toDouble.asInstanceOf[FiniteDouble]
            else
              error("FiniteDouble cannot be instantiated with infinity or NaN")
          case Some(_) => error("FiniteDouble cannot be instantiated with infinity or NaN")
          case None => error("FiniteDouble conversion requires a float literal")
        }
      def apply(x: Float): FiniteDouble = x.toDouble
    }

    /** Converts a compile-time <code>Double</code> literal to a [[FiniteDouble]]. */
    given Conversion[Double, FiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): FiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v == v && v != Double.PositiveInfinity && v != Double.NegativeInfinity then
              v.asInstanceOf[FiniteDouble]
            else
              error("FiniteDouble cannot be instantiated with infinity or NaN")
          case Some(_) => error("FiniteDouble cannot be instantiated with infinity or NaN")
          case None => error("FiniteDouble conversion requires a double literal")
        }
      def apply(x: Double): FiniteDouble = x
    }

    /** Compile-time factory for creating a [[FiniteDouble]] from an <code>Int</code> literal. */
    inline def apply[I <: Int & Singleton](inline i: I): FiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) => v.toDouble.asInstanceOf[FiniteDouble]
        case None => error("FiniteDouble.apply requires an integer literal")
      }

    /** Compile-time factory for creating a [[FiniteDouble]] from a <code>Float</code> literal. */
    inline def apply[F <: Float & Singleton](inline f: F): FiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v == v && v != Float.PositiveInfinity && v != Float.NegativeInfinity then
            v.toDouble.asInstanceOf[FiniteDouble]
          else
            error("FiniteDouble cannot be instantiated with infinity or NaN")
        case Some(_) => error("FiniteDouble cannot be instantiated with infinity or NaN")
        case None => error("FiniteDouble.apply requires a float literal")
      }

    /** Compile-time factory for creating a [[FiniteDouble]] from a <code>Double</code> literal. */
    inline def apply[D <: Double & Singleton](inline d: D): FiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v == v && v != Double.PositiveInfinity && v != Double.NegativeInfinity then
            v.asInstanceOf[FiniteDouble]
          else
            error("FiniteDouble cannot be instantiated with infinity or NaN")
        case Some(_) => error("FiniteDouble cannot be instantiated with infinity or NaN")
        case None => error("FiniteDouble.apply requires a double literal")
      }

    /** Test whether a value is a valid [[FiniteDouble]]. */
    def isValid(value: Double): Boolean = !value.isNaN && !value.isInfinite

    /** Return `Some` when the passed value is finite, otherwise `None`. */
    def from(value: Double): Option[FiniteDouble] =
      if (isValid(value)) Some(value) else None

    /** Validate and return a [[FiniteDouble]], throwing when the value is not finite. */
    def ensuringValid(value: Double): FiniteDouble =
      if (isValid(value)) value
      else throw new AssertionError(Resources.invalidPosFiniteDouble)

    /** Return `Success` for a finite value or `Failure` with an assertion error otherwise. */
    def tryingValid(value: Double): Try[FiniteDouble] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidPosFiniteDouble))

    /** Return `Pass` for a finite value, or `Fail` with an error value otherwise. */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Return `Good` for a finite value, or `Bad` with an error value otherwise. */
    def goodOrElse[B](value: Double)(f: Double => B): FiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Return `Right` for a finite value, or `Left` with an error value otherwise. */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, FiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return the validated value or a provided default when invalid. */
    def fromOrElse(value: Double, default: => FiniteDouble): FiniteDouble =
      if (isValid(value)) value else default

    /** Largest valid [[FiniteDouble]] value. */
    val MaxValue: FiniteDouble = Double.MaxValue

    /** Smallest valid [[FiniteDouble]] value. */
    val MinValue: FiniteDouble = Double.MinValue

    /** Smallest positive valid [[FiniteDouble]] value. */
    val MinPositiveValue: FiniteDouble = Double.MinPositiveValue

    /** Ordering instance for FiniteDouble that orders by numeric value. */
    given Ordering[FiniteDouble] with {
      def compare(x: FiniteDouble, y: FiniteDouble): Int = java.lang.Double.compare(x, y)
    }

    extension (p: FiniteDouble) {
      /** Return the underlying Double value. */
      def value: Double = p.asInstanceOf[Double]

      /** Indicates whether this FiniteDouble has no fraction part. */
      def isWhole: Boolean = {
        val underlying = p.asInstanceOf[Double]
        !underlying.isNaN && !underlying.isInfinite && underlying == math.rint(underlying)
      }

      /** Rounds this FiniteDouble to the nearest whole number. */
      def round: Long = math.round(value)

      /** Returns the smallest FiniteDouble that is >= this value and is a mathematical integer. */
      def ceil: FiniteDouble = FiniteDouble.ensuringValid(math.ceil(value).toDouble)

      /** Returns the greatest FiniteDouble that is <= this value and is a mathematical integer. */
      def floor: FiniteDouble = FiniteDouble.ensuringValid(math.floor(value).toDouble)

      /** Returns the larger of this value and `other`. */
      def max(other: FiniteDouble): FiniteDouble =
        if (p >= other) p else other

      /** Returns the smaller of this value and `other`. */
      def min(other: FiniteDouble): FiniteDouble =
        if (p <= other) p else other

      /** Returns this value in radians. */
      def toRadians: Double = math.toRadians(value)

      /** Returns this value in degrees. */
      def toDegrees: Double = math.toDegrees(value)

      /** Returns the negated value as another [[FiniteDouble]]. */
      def unary_- : FiniteDouble = FiniteDouble.ensuringValid(-p)

      /** Applies the passed Double => Double function and ensures the result is a FiniteDouble. */
      def ensuringValid(f: Double => Double): FiniteDouble = {
        val candidateResult: Double = f(p)
        if (FiniteDouble.isValid(candidateResult)) FiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid FiniteDouble")
      }
    }
}
  }