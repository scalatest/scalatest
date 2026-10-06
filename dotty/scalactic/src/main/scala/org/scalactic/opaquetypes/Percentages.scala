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
import scala.util.{ Try, Success, Failure }
import org.scalactic.{ Validation, Pass, Fail }
import org.scalactic.{ Or, Good, Bad }

/** Factory object for the [[Percentages.PercentageInt]] opaque type, which
  * restricts `Int` values to the inclusive percent range 0 through 100.
  */
object Percentages {

  /** Opaque type representing an `Int` percentage value between 0 and 100 inclusive.
    *
    * Instances of this type are guaranteed to satisfy both `>= 0` and `<= 100`.
    * Use the compile-time [[Percentages.PercentageInt.apply]] method to construct
    * instances from integer literals, or the runtime factory methods for values
    * known only at runtime.
    */
  private[scalactic] opaque type PercentageInt = Int

  /** Companion object for [[Percentages.PercentageInt]] with construction and
    * validation helpers.
    *
    * Provides a compile-time-checked factory for integer literals, runtime
    * validation helpers, and extension methods for common operations.
    */
  private[scalactic] object PercentageInt {

    /** Compile-time factory for creating a [[Percentages.PercentageInt]] from an integer literal.
      *
      * Rejects literals outside the range 0 to 100 at compile time.
      */
    inline def apply[I <: Int & Singleton](inline i: I): PercentageInt =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v >= 0 && v <= 100 then
            v.asInstanceOf[PercentageInt]
          else
            error("PercentageInt.apply can only be invoked on Int literals between 0 and 100, inclusive, like PercentageInt(8).")
        case None =>
          error("PercentageInt.apply can only be invoked on Int literals, like PercentageInt(8). Please use PercentageInt.from instead.")
      }

    /** Construct a [[Percentages.PercentageInt]] from a runtime Int if it is in range.
      *
      * @param value the Int to validate
      * @return Some(PercentageInt) if value is between 0 and 100 inclusive, else None
      */
    def from(value: Int): Option[PercentageInt] =
      if (isValid(value)) Some(value) else None

    /** Validate and return the given Int as [[Percentages.PercentageInt]].
      *
      * @throws AssertionError if value is not between 0 and 100 inclusive
      */
    def ensuringValid(value: Int): PercentageInt =
      if (isValid(value))
        value
      else
        throw new AssertionError(Resources.invalidPercentageInt)

    /** Runtime factory that returns Success for valid input, Failure otherwise.
      *
      * @param value the Int to validate
      * @return Success(PercentageInt) if value is between 0 and 100 inclusive,
      *   else Failure(AssertionError)
      */
    def tryingValid(value: Int): Try[PercentageInt] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPercentageInt))

    /** Predicate indicating whether the given Int is valid for [[Percentages.PercentageInt]].
      *
      * @param value the Int to validate
      * @return true if value is between 0 and 100 inclusive, else false
      */
    def isValid(value: Int): Boolean = value >= 0 && value <= 100

    /** Validate a value and return Pass, else Fail(f(value)).
      *
      * @param value the Int to validate
      * @param f function to produce an error value when validation fails
      * @return Pass if value is a valid PercentageInt, else Fail(f(value))
      */
    def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(PercentageInt), else Bad(f(value)).
      *
      * @param value the Int to validate
      * @param f function to produce an error value when validation fails
      * @return Good(PercentageInt) if value is valid, else Bad(f(value))
      */
    def goodOrElse[B](value: Int)(f: Int => B): PercentageInt Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Validate a value and return Right(PercentageInt), else Left(f(value)).
      *
      * @param value the Int to validate
      * @param f function to produce an error value when validation fails
      * @return Right(PercentageInt) if value is valid, else Left(f(value))
      */
    def rightOrElse[L](value: Int)(f: Int => L): Either[L, PercentageInt] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid.
      *
      * @param value the Int to validate
      * @param default the PercentageInt to return if value is not valid
      * @return value as PercentageInt if valid, else default
      */
    def fromOrElse(value: Int, default: => PercentageInt): PercentageInt =
      if (isValid(value)) value else default

    /** Smallest valid PercentageInt value (which is 0). */
    val MinValue: PercentageInt = 0

    /** Largest valid PercentageInt value (which is 100). */
    val MaxValue: PercentageInt = 100

    /** Convert [[Percentages.PercentageInt]] to [[Int]] for interoperability. */
    given Conversion[PercentageInt, Int] with {
      def apply(x: PercentageInt): Int = x
    }

    /** Ordering instance based on underlying Int ordering. */
    given Ordering[PercentageInt] with {
      def compare(x: PercentageInt, y: PercentageInt): Int = x.compareTo(y)
    }
  }

  extension (x: PercentageInt) {

    /** Return the underlying Int value. */
    def value: Int = x

    /** Return the underlying Int value. */
    def toInt: Int = x

    /** True if this value is zero. */
    def isZero: Boolean = x == 0

    /** True if this value is the maximum, 100. */
    def isMax: Boolean = x == 100

    /** True if this value is the minimum, 0. */
    def isMin: Boolean = x == 0
  }
}