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
import scala.collection.immutable.Range
import scala.util.{Failure, Success, Try}

import org.scalactic.{Bad, Fail, Good, Or, Pass, Validation}

object NegInts {
  /** Opaque type representing strictly negative Int values (< 0). */
  opaque type NegInt = Int

  /** Companion object for [[NegInt]] with construction and validation helpers. */
  object NegInt {
    /** Compile-time factory for creating a [[NegInt]] from an integer literal.
      *
      * Rejects non-negative literals at compile time.
      */
    inline def apply[I <: Int & Singleton](inline i: I): NegInt =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v >= 0 then
            error("NegInt cannot be instantiated with a non-negative integer literal")
          else
            v.asInstanceOf[NegInt]
        case None =>
          error("NegInt.apply requires an integer literal")
      }

    /** Construct a [[NegInt]] from a runtime Int if it is negative.
      *
      * @return Some(NegInt) when i < 0, else None
      */
    def from(i: Int): Option[NegInt] =
      if (i < 0) Some(i) else None

    /** Validate and return the given Int as [[NegInt]].
      *
      * @throws AssertionError if i is non-negative
      */
    def ensuringValid(i: Int): NegInt =
      if (i >= 0)
        throw new AssertionError(Resources.invalidNegInt)
      else i

    /** Runtime factory that returns Success for valid input, Failure otherwise. */
    def tryingValid(value: Int): Try[NegInt] =
      if (value < 0) Success(value)
      else Failure(new AssertionError(Resources.invalidNegInt))

    /** Predicate indicating whether the given Int is valid for [[NegInt]]. */
    def isValid(value: Int): Boolean = value < 0

    /** Validate a value and return Pass, else Fail(f(value)). */
    def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(NegInt), else Bad(f(value)). */
    def goodOrElse[B](value: Int)(f: Int => B): NegInt Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Validate a value and return Right(NegInt), else Left(f(value)). */
    def rightOrElse[L](value: Int)(f: Int => L): Either[L, NegInt] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid. */
    def fromOrElse(value: Int, default: => NegInt): NegInt =
      if (isValid(value)) value else default

    /** Largest valid [[NegInt]] value, equal to -1. */
    val MaxValue: NegInt = -1

    /** Smallest valid [[NegInt]] value, equal to Int.MinValue. */
    val MinValue: NegInt = Int.MinValue

    extension (x: NegInt) {
      /** Return the underlying Int value. */
      def value: Int = x
      /** Bitwise complement of the underlying value. */
      def unary_~ : Int = ~x
      /** Unary plus returns this value unchanged. */
      def unary_+ : NegInt = x
      /** Numeric negation as Int. */
      def unary_- : Int = -x
      /** Greater of this and that value. */
      def max(that: NegInt): NegInt = if (math.max(x, that) == x) x else that
      /** Lesser of this and that value. */
      def min(that: NegInt): NegInt = if (math.min(x, that) == x) x else that
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

      /** Apply `f` and require the result to remain a valid [[NegInt]].
        *
        * @throws AssertionError if f(x) is non-negative
        */
      def ensuringValid(f: Int => Int): NegInt = {
        val res = f(x)
        if (res >= 0)
          throw new AssertionError(Resources.invalidNegInt)
        else res
      }
    }

    /** Widen [[NegInt]] to Int. */
    given Conversion[NegInt, Int] with {
      def apply(x: NegInt): Int = x
    }

    /** Convert Int to [[NegInt]] via compile-time or runtime validation. */
    given Conversion[Int, NegInt] with {
      inline def apply[I <: Int & Singleton](inline x: I): NegInt =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v >= 0 then
              error("NegInt cannot be instantiated with a non-negative integer literal")
            else
              v.asInstanceOf[NegInt]
          case None =>
            error("NegInt conversion requires an integer literal")
        }

      def apply(x: Int): NegInt = NegInt.ensuringValid(x)
    }

    /** Ordering instance based on underlying numeric Int ordering. */
    given Ordering[NegInt] with {
      def compare(x: NegInt, y: NegInt): Int = x.compareTo(y)
    }
  }

  /** Opaque type representing negative-or-zero Int values (<= 0). */
  opaque type NegZInt = Int

  /** Companion object for [[NegZInt]] validation helpers. */
  object NegZInt {
    /** Validate and return the given Int as [[NegZInt]].
      *
      * @throws AssertionError if i is positive
      */
    def ensuringValid(i: Int): NegZInt = 
      if (i > 0) 
        throw new AssertionError(Resources.invalidNegZInt)
      else i
  }
}