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

object NonZeroDoubles {

  opaque type NonZeroDouble = Double

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

    def ensuringValid(d: Double): NonZeroDouble =
      if (d == 0.0)
        throw new AssertionError(Resources.invalidNonZeroDouble)
      else d
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
          inline if v != 0.0 && v.isFinite then
            v
          else
            error("NonZeroFiniteDouble cannot be instantiated with zero, infinity, or NaN")
        case None =>
          error("NonZeroFiniteDouble.apply requires a double literal")
      }

    /** Returns <code>true</code> if the provided <code>Double</code> is a valid [[NonZeroFiniteDouble]]
      * value — that is, if it is both <code>!= 0.0</code> and finite (<code>isFinite</code>).
      */
    def isValid(value: Double): Boolean = value != 0.0 && value.isFinite

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
      /** Return the underlying Double value. */
      def toDouble: Double = p

      /** Applies the given <code>Double =&gt; Double</code> function to the underlying
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