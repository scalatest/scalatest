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

    /** Widen [[NegInt]] to `Int`.
      *
      * @param x the wrapped value
      * @return the underlying `Int`
      */
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
    /** Compile-time factory for creating a [[NegZInt]] from an integer literal.
      *
      * Rejects positive literals at compile time.
      *
      * @tparam I the singleton integer literal type
      * @param i the integer literal to validate
      * @return the validated literal as a [[NegZInt]]
      * @throws scala.compiletime.error if the literal is positive
      */
    inline def apply[I <: Int & Singleton](inline i: I): NegZInt =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v > 0 then
            error("NegZInt cannot be instantiated with a positive integer literal")
          else
            v.asInstanceOf[NegZInt]
        case None =>
          error("NegZInt.apply requires an integer literal")
      }

    /** Construct a [[NegZInt]] from a runtime Int if it is non-positive.
      *
      * @param i the integer to validate
      * @return Some(NegZInt) when `i <= 0`, else None
      */
    def from(i: Int): Option[NegZInt] =
      if (i <= 0) Some(i) else None

    /** Validate and return the given Int as [[NegZInt]].
      *
      * @param i the integer to validate
      * @return the validated value as a [[NegZInt]]
      * @throws AssertionError if `i` is positive
      */
    def ensuringValid(i: Int): NegZInt =
      if (i > 0)
        throw new AssertionError(Resources.invalidNegZInt)
      else i

    /** Runtime factory that returns Success for valid input, Failure otherwise.
      *
      * @param value the integer to validate
      * @return Success(NegZInt) if `value <= 0`, else Failure(AssertionError)
      */
    def tryingValid(value: Int): Try[NegZInt] =
      if (value <= 0) Success(value)
      else Failure(new AssertionError(Resources.invalidNegZInt))

    /** Predicate indicating whether the given Int is valid for [[NegZInt]].
      *
      * @param value the integer to inspect
      * @return true if `value <= 0`, otherwise false
      */
    def isValid(value: Int): Boolean = value <= 0

    /** Validate a value and return Pass, else Fail(f(value)).
      *
      * @tparam E the error type produced by `f`
      * @param value the integer to validate
      * @param f function used to compute an error value when `value` is invalid
      * @return Pass for valid input, else Fail(f(value))
      */
    def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(NegZInt), else Bad(f(value)).
      *
      * @tparam B the error type produced by `f`
      * @param value the integer to validate
      * @param f function used to compute an error value when `value` is invalid
      * @return Good(NegZInt) for valid input, else Bad(f(value))
      */
    def goodOrElse[B](value: Int)(f: Int => B): NegZInt Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Validate a value and return Right(NegZInt), else Left(f(value)).
      *
      * @tparam L the error type produced by `f`
      * @param value the integer to validate
      * @param f function used to compute an error value when `value` is invalid
      * @return Right(NegZInt) for valid input, else Left(f(value))
      */
    def rightOrElse[L](value: Int)(f: Int => L): Either[L, NegZInt] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid.
      *
      * @param value the integer to validate
      * @param default the [[NegZInt]] to return if `value` is positive
      * @return the validated value if valid, else `default`
      */
    def fromOrElse(value: Int, default: => NegZInt): NegZInt =
      if (isValid(value)) value else default

    /** Largest valid [[NegZInt]] value, equal to 0. */
    val MaxValue: NegZInt = 0

    /** Smallest valid [[NegZInt]] value, equal to Int.MinValue. */
    val MinValue: NegZInt = Int.MinValue

    extension (x: NegZInt) {
      /** Return the underlying Int value. */
      def value: Int = x

      /** Greater of this and that value.
        *
        * @param that the comparison value
        * @return the larger of the two values
        */
      def max(that: NegZInt): NegZInt = math.max(x, that).asInstanceOf[NegZInt]

      /** Lesser of this and that value.
        *
        * @param that the comparison value
        * @return the smaller of the two values
        */
      def min(that: NegZInt): NegZInt = math.min(x, that).asInstanceOf[NegZInt]

      /** Return the unsigned binary string representation of the underlying Int. */
      def toBinaryString: String = java.lang.Integer.toBinaryString(x)

      /** Return the unsigned hexadecimal string representation of the underlying Int. */
      def toHexString: String = java.lang.Integer.toHexString(x)

      /** Return the unsigned octal string representation of the underlying Int. */
      def toOctalString: String = java.lang.Integer.toOctalString(x)

      /**
        * Create an inclusive [[Range]] from this [[NegZInt]] value to the specified `end` with step value 1.
        *
        * @param end the final bound of the range to make
        * @return a [[scala.collection.immutable.Range]] from `this` up to and including `end`
        */
      def to(end: Int): Range.Inclusive = Range.inclusive(x, end)

      /**
        * Create an inclusive [[Range]] from this [[NegZInt]] value to the specified `end` with the specified `step`.
        *
        * @param end the final bound of the range to make
        * @param step the amount to increase by for each step of the range
        * @return a [[scala.collection.immutable.Range]] from `this` up to and including `end`
        */
      def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(x, end, step)

      /**
        * Create a [[Range]] from this [[NegZInt]] value until the specified `end` (exclusive) with step value 1.
        *
        * @param end the final bound of the range to make
        * @return a [[scala.collection.immutable.Range]] from `this` up to but not including `end`
        */
      def until(end: Int): Range = Range(x, end)

      /**
        * Create a [[Range]] from this [[NegZInt]] value until the specified `end` (exclusive) with the specified `step`.
        *
        * @param end the final bound of the range to make
        * @param step the amount to increase by for each step of the range
        * @return a [[scala.collection.immutable.Range]] from `this` up to but not including `end`
        */
      def until(end: Int, step: Int): Range = Range(x, end, step)

      /** Apply a transformation and ensure the result is a valid [[NegZInt]].
        *
        * @param f function to transform the underlying Int
        * @return the transformed value as a [[NegZInt]] if valid
        * @throws AssertionError if the result of `f` is positive
        */
      def ensuringValid(f: Int => Int): NegZInt = {
        val res = f(x)
        if (res > 0)
          throw new AssertionError(Resources.invalidNegZInt)
        else res
      }
    }

    /** Convert a [[NegZInt]] to a plain Int (unwrap).
      *
      * @param x the wrapped value
      * @return the underlying Int
      */
    given Conversion[NegZInt, Int] with {
      def apply(x: NegZInt): Int = x
    }

    /** Convert an Int to [[NegZInt]] via compile-time or runtime validation.
      *
      * @tparam I the singleton integer literal type
      * @param x the integer literal or runtime Int to validate
      * @return the validated value as a [[NegZInt]]
      * @throws AssertionError if the runtime value is positive
      */
    given Conversion[Int, NegZInt] with {
      inline def apply[I <: Int & Singleton](inline x: I): NegZInt =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v > 0 then
              error("NegZInt cannot be instantiated with a positive integer literal")
            else
              v.asInstanceOf[NegZInt]
          case None =>
            error("NegZInt conversion requires an integer literal")
        }

      def apply(x: Int): NegZInt = NegZInt.ensuringValid(x)
    }

    /** Ordering instance based on underlying numeric Int ordering. */
    given Ordering[NegZInt] with {
      def compare(x: NegZInt, y: NegZInt): Int = x.compareTo(y)
    }
  }
}