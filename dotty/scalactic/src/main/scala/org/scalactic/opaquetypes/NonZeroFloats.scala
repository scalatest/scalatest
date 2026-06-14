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

    /** Implicitly widens a [[NonZeroFloat]] to a plain <code>Double</code>. */
    given Conversion[NonZeroFloat, Double] with {
      def apply(x: NonZeroFloat): Double = x
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
          inline if v != 0.0f && v.isFinite then
            v
          else
            error("NonZeroFiniteFloat cannot be instantiated with zero, infinity, or NaN")
        case None =>
          error("NonZeroFiniteFloat.apply requires a float literal")
      }

    /** Returns <code>true</code> if the provided <code>Float</code> is a valid [[NonZeroFiniteFloat]]
      * value — that is, if it is both <code>!= 0.0f</code> and finite (<code>isFinite</code>).
      */
    def isValid(value: Float): Boolean = value != 0.0f && value.isFinite

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
      /** Return the underlying Float value. */
      def toFloat: Float = p

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