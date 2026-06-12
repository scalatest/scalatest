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
 * and limitations under the License.
 */
package org.scalactic.opaquetypes

import org.scalactic.Resources
import scala.compiletime.{constValueOpt, error}
import scala.collection.immutable.Range
import scala.util.{Failure, Success, Try}

import org.scalactic.{Bad, Fail, Good, Or, Pass, Validation}

object NonZeroInts {
  /** Opaque type representing non-zero Int values (!= 0). */
  opaque type NonZeroInt = Int

  /** Companion object for [[NonZeroInt]] with construction and validation helpers. */
  object NonZeroInt {
    /** Compile-time factory for creating a [[NonZeroInt]] from an integer literal.
      *
      * Rejects zero literals at compile time.
      */
    inline def apply[I <: Int & Singleton](inline i: I): NonZeroInt =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v == 0 then
            error("NonZeroInt cannot be instantiated with zero")
          else
            v.asInstanceOf[NonZeroInt]
        case None =>
          error("NonZeroInt.apply requires an integer literal")
      }

    /** Construct a [[NonZeroInt]] from a runtime Int if it is non-zero.
      *
      * @return Some(NonZeroInt) when i != 0, else None
      */
    def from(i: Int): Option[NonZeroInt] =
      if (i != 0) Some(i) else None

    /** Validate and return the given Int as [[NonZeroInt]].
      *
      * @throws AssertionError if i is zero
      */
    def ensuringValid(i: Int): NonZeroInt =
      if (i == 0)
        throw new AssertionError(Resources.invalidNonZeroInt)
      else i

    /** Runtime factory that returns Success for valid input, Failure otherwise. */
    def tryingValid(value: Int): Try[NonZeroInt] =
      if (value != 0) Success(value)
      else Failure(new AssertionError(Resources.invalidNonZeroInt))

    /** Predicate indicating whether the given Int is valid for [[NonZeroInt]]. */
    def isValid(value: Int): Boolean = value != 0

    /** Validate a value and return Pass, else Fail(f(value)). */
    def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(NonZeroInt), else Bad(f(value)). */
    def goodOrElse[B](value: Int)(f: Int => B): NonZeroInt Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Validate a value and return Right(NonZeroInt), else Left(f(value)). */
    def rightOrElse[L](value: Int)(f: Int => L): Either[L, NonZeroInt] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid. */
    def fromOrElse(value: Int, default: => NonZeroInt): NonZeroInt =
      if (isValid(value)) value else default

    /** Largest valid [[NonZeroInt]] value. */
    val MaxValue: NonZeroInt = Int.MaxValue

    /** Smallest valid [[NonZeroInt]] value. */
    val MinValue: NonZeroInt = Int.MinValue

    extension (x: NonZeroInt) {
      /** Return the underlying Int value. */
      def value: Int = x

      /** Numeric negation as NonZeroInt. */
      def unary_- : NonZeroInt = NonZeroInt.ensuringValid(-x)

      /** Greater of this and that value. */
      def max(that: NonZeroInt): NonZeroInt = if (math.max(x, that) == x) x else that

      /** Lesser of this and that value. */
      def min(that: NonZeroInt): NonZeroInt = if (math.min(x, that) == x) x else that

      /** Unsigned binary string representation. */
      def toBinaryString: String = java.lang.Integer.toBinaryString(x)

      /** Unsigned hexadecimal string representation. */
      def toHexString: String = java.lang.Integer.toHexString(x)

      /** Unsigned octal string representation. */
      def toOctalString: String = java.lang.Integer.toOctalString(x)

      /** Inclusive range from this value to `end` with step 1. */
      def to(end: Int): Range.Inclusive = Range.inclusive(x, end)

      /** Inclusive range from this value to `end` with custom step. */
      def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(x, end, step)

      /** Range from this value up to but excluding `end` with step 1. */
      def until(end: Int): Range = Range(x, end)

      /** Range from this value up to but excluding `end` with custom step. */
      def until(end: Int, step: Int): Range = Range(x, end, step)

      /** Apply `f` and require the result to remain a valid [[NonZeroInt]].
        *
        * @throws AssertionError if f(x) is zero
        */
      def ensuringValid(f: Int => Int): NonZeroInt = {
        val res = f(x)
        if (res == 0)
          throw new AssertionError(Resources.invalidNonZeroInt)
        else res
      }
    }

    /** Widen [[NonZeroInt]] to `Int`.
      *
      * @param x the wrapped value
      * @return the underlying `Int`
      */
    given Conversion[NonZeroInt, Int] with {
      def apply(x: NonZeroInt): Int = x
    }

    /** Convert Int to [[NonZeroInt]] via compile-time or runtime validation. */
    given Conversion[Int, NonZeroInt] with {
      inline def apply[I <: Int & Singleton](inline x: I): NonZeroInt =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v == 0 then
              error("NonZeroInt cannot be instantiated with zero")
            else
              v.asInstanceOf[NonZeroInt]
          case None =>
            error("NonZeroInt conversion requires an integer literal")
        }

      def apply(x: Int): NonZeroInt = NonZeroInt.ensuringValid(x)
    }

    /** Ordering instance based on underlying numeric Int ordering. */
    given Ordering[NonZeroInt] with {
      def compare(x: NonZeroInt, y: NonZeroInt): Int = x.compareTo(y)
    }

  }
}
