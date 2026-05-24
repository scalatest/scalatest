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

object NegDoubles {
  opaque type NegZDouble = Double

  object NegZDouble {
    inline def apply[D <: Double & Singleton](inline d: D): NegZDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v > 0.0 then
            error("NegZDouble cannot be instantiated with a positive double literal")
          else
            v.asInstanceOf[NegZDouble]
        case None =>
          error("NegZDouble.apply requires a double literal")
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

    val MaxValue: NegZDouble = 0.0
    val MinValue: NegZDouble = Double.MinValue
    val NegativeInfinity: NegZDouble = Double.NegativeInfinity

    extension (p: NegZDouble) {
      def value: Double = p
      def isNegInfinity: Boolean = p == Double.NegativeInfinity
      def isFinite: Boolean = !p.isInfinite
      def unary_+ : NegZDouble = p
      def plus(x: NegZDouble): NegZDouble = NegZDouble.ensuringValid(value + x)
      def max(that: NegZDouble): NegZDouble = math.max(p, that)
      def min(that: NegZDouble): NegZDouble = math.min(p, that)
      def isWhole: Boolean = {
        val longValue = p.toLong
        longValue.toDouble == p || longValue == Long.MaxValue && p < Double.PositiveInfinity || longValue == Long.MinValue && p > Double.NegativeInfinity
      }
      def round: NegZLong = NegZLong.ensuringValid(math.round(value))
      def ceil: NegZDouble = NegZDouble.ensuringValid(math.ceil(value))
      def floor: NegZDouble = NegZDouble.ensuringValid(math.floor(value))
      def toRadians: Double = math.toRadians(value)
      def toDegrees: Double = math.toDegrees(value)
      def ensuringValid(f: Double => Double): NegZDouble = {
        val candidateResult: Double = f(value)
        if (NegZDouble.isValid(candidateResult)) NegZDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${value.toString()}, was not a valid NegZDouble")
      }
    }

    given Conversion[NegZDouble, Double] with {
      def apply(x: NegZDouble): Double = x.toDouble
    }

    given Conversion[Double, NegZDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegZDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v > 0.0 then
              error("NegZDouble cannot be instantiated with a positive double literal")
            else
              v.asInstanceOf[NegZDouble]
          case None =>
            error("NegZDouble conversion requires a double literal")
        }

      def apply(x: Double): NegZDouble = NegZDouble.ensuringValid(x)
    }

  }

  opaque type NegDouble <: NegZDouble = Double

  object NegDouble {
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

    inline def apply[L <: Long & Singleton](inline d: L): NegDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v >= 0L then
            error("NegDouble cannot be instantiated with a non-negative long literal")
          else
            v.toDouble.asInstanceOf[NegDouble]
        case None =>
          error("NegDouble.apply requires a long literal")
      }

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
    def apply(d: Long): NegDouble = ensuringValid(d.toDouble)
    def apply(d: Float): NegDouble = ensuringValid(d.toDouble)
    def apply(d: Double): NegDouble = ensuringValid(d)

    def from(d: Double): Option[NegDouble] =
      if (isValid(d)) Some(d) else None

    def ensuringValid(d: Double): NegDouble =
      if (d >= 0.0)
        throw new AssertionError(Resources.invalidNegDouble)
      else d

    def tryingValid(value: Double): Try[NegDouble] =
      if (isValid(value)) Success(value)
      else Failure(new AssertionError(Resources.invalidNegDouble))

    def isValid(value: Double): Boolean = value < 0.0

    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    def goodOrElse[B](value: Double)(f: Double => B): NegDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NegDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    def fromOrElse(value: Double, default: => NegDouble): NegDouble =
      if (isValid(value)) value else default

    val MaxValue: NegDouble = -Double.MinPositiveValue
    val MinValue: NegDouble = Double.MinValue
    val NegativeInfinity: NegDouble = Double.NegativeInfinity

    extension (p: NegDouble) {
      def value: Double = p
      def isNegInfinity: Boolean = p == Double.NegativeInfinity
      def isFinite: Boolean = !p.isInfinite
      def unary_+ : NegDouble = p
        def unary_- : PosDoubles.PosDouble = PosDoubles.PosDouble.ensuringValid(-p.toDouble)
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

    given Conversion[NegDouble, Double] with {
      def apply(x: NegDouble): Double = x.toDouble
    }

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

    given Conversion[Long, NegDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): NegDouble =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v >= 0L then
              error("NegDouble cannot be instantiated with a non-negative long literal")
            else
              v.toDouble.asInstanceOf[NegDouble]
          case None =>
            error("NegDouble conversion requires a long literal")
        }

      def apply(x: Long): NegDouble = NegDouble.ensuringValid(x.toDouble)
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

    given Ordering[NegDouble] with {
      def compare(x: NegDouble, y: NegDouble): Int = x.compareTo(y)
    }
  }

  opaque type NegZFiniteDouble <: NegZDouble = Double

  object NegZFiniteDouble {
    inline def apply[I <: Int & Singleton](inline d: I): NegZFiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v > 0 then
            error("NegZFiniteDouble cannot be instantiated with a positive integer literal")
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires an integer literal")
      }

    inline def apply[L <: Long & Singleton](inline d: L): NegZFiniteDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v > 0L then
            error("NegZFiniteDouble cannot be instantiated with a positive long literal")
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires a long literal")
      }

    inline def apply[F <: Float & Singleton](inline d: F): NegZFiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v > 0.0f then
            error("NegZFiniteDouble cannot be instantiated with a positive float literal")
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires a float literal")
      }

    inline def apply[D <: Double & Singleton](inline d: D): NegZFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v > 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
            error("NegZFiniteDouble cannot be instantiated with a positive double literal or infinity")
          else
            v.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires a double literal")
      }

    def apply(d: Int): NegZFiniteDouble = ensuringValid(d.toDouble)
    def apply(d: Long): NegZFiniteDouble = ensuringValid(d.toDouble)
    def apply(d: Float): NegZFiniteDouble = ensuringValid(d.toDouble)
    def apply(d: Double): NegZFiniteDouble = ensuringValid(d)

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

    val MaxValue: NegZFiniteDouble = 0.0
    val MinValue: NegZFiniteDouble = Double.MinValue

    given Conversion[NegZFiniteDouble, Double] with {
      def apply(x: NegZFiniteDouble): Double = x.toDouble
    }

    given Conversion[Double, NegZFiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegZFiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v > 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
              error("NegZFiniteDouble cannot be instantiated with a positive double literal or infinity")
            else
              v.asInstanceOf[NegZFiniteDouble]
          case None =>
            error("NegZFiniteDouble conversion requires a double literal")
        }

      def apply(x: Double): NegZFiniteDouble = NegZFiniteDouble.ensuringValid(x)
    }

    given Ordering[NegZFiniteDouble] with {
      def compare(x: NegZFiniteDouble, y: NegZFiniteDouble): Int = x.compareTo(y)
    }
  }

  opaque type NegFiniteDouble <: NegDouble = Double

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

    inline def apply[L <: Long & Singleton](inline d: L): NegFiniteDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v >= 0L then
            error("NegFiniteDouble cannot be instantiated with a non-negative long literal")
          else
            v.toDouble.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires a long literal")
      }

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

    def apply(d: Int): NegFiniteDouble = ensuringValid(d.toDouble)
    def apply(d: Long): NegFiniteDouble = ensuringValid(d.toDouble)
    def apply(d: Float): NegFiniteDouble = ensuringValid(d.toDouble)
    def apply(d: Double): NegFiniteDouble = ensuringValid(d)

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

    val MaxValue: NegFiniteDouble = -Double.MinPositiveValue
    val MinValue: NegFiniteDouble = Double.MinValue

    given Conversion[NegFiniteDouble, Double] with {
      def apply(x: NegFiniteDouble): Double = x.toDouble
    }

    given Conversion[NegFiniteDouble, NonZeroDoubles.NonZeroDouble] with {
      def apply(x: NegFiniteDouble): NonZeroDoubles.NonZeroDouble = NonZeroDoubles.NonZeroDouble.ensuringValid(x.toDouble)
    }

    given Conversion[Double, NegFiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegFiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v >= 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
              error("NegFiniteDouble cannot be instantiated with a non-negative double literal or infinity")
            else
              v.asInstanceOf[NegFiniteDouble]
          case None =>
            error("NegFiniteDouble conversion requires a double literal")
        }

      def apply(x: Double): NegFiniteDouble = NegFiniteDouble.ensuringValid(x)
    }

    given Ordering[NegFiniteDouble] with {
      def compare(x: NegFiniteDouble, y: NegFiniteDouble): Int = x.compareTo(y)
    }
  }
}