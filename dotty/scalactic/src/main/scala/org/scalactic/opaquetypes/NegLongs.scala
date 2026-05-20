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
import scala.collection.immutable.NumericRange
import scala.compiletime.{constValueOpt, error}
import scala.util.{Failure, Success, Try}

import org.scalactic.{Bad, Fail, Good, Or, Pass, Validation}

object NegLongs {
  /** Opaque type representing strictly negative Long values (< 0). */
  opaque type NegLong = Long

  /** Companion object for [[NegLong]] with construction and validation helpers. */
  object NegLong {
    /** Compile-time factory for creating a [[NegLong]] from a Long literal. */
    inline def apply[I <: Long & Singleton](inline i: I): NegLong =
      inline constValueOpt[I] match {
        case Some(v: Long) =>
          inline if v >= 0L then
            error("NegLong cannot be instantiated with a non-negative long literal")
          else
            v.asInstanceOf[NegLong]
        case None =>
          error("NegLong.apply requires a long literal")
      }

    /** Compile-time factory for creating a [[NegLong]] from an Int literal. */
    inline def apply[I <: Int & Singleton](inline i: I): NegLong =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v >= 0 then
            error("NegLong cannot be instantiated with a non-negative integer literal")
          else
            v.toLong.asInstanceOf[NegLong]
        case None =>
          error("NegLong.apply requires an integer or long literal")
      }

    /** Construct a [[NegLong]] from a runtime Long if it is negative. */
    def from(value: Long): Option[NegLong] =
      if (value < 0L) Some(value) else None

    /** Validate and return the given Long as [[NegLong]]. */
    def ensuringValid(value: Long): NegLong =
      if (value >= 0L)
        throw new AssertionError(Resources.invalidNegLong)
      else value

    /** Runtime factory that returns Success for valid input, Failure otherwise. */
    def tryingValid(value: Long): Try[NegLong] =
      if (value < 0L) Success(value)
      else Failure(new AssertionError(Resources.invalidNegLong))

    /** Predicate indicating whether the given Long is valid for [[NegLong]]. */
    def isValid(value: Long): Boolean = value < 0L

    /** Validate a value and return Pass, else Fail(f(value)). */
    def passOrElse[E](value: Long)(f: Long => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(NegLong), else Bad(f(value)). */
    def goodOrElse[B](value: Long)(f: Long => B): NegLong Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** Validate a value and return Right(NegLong), else Left(f(value)). */
    def rightOrElse[L](value: Long)(f: Long => L): Either[L, NegLong] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid. */
    def fromOrElse(value: Long, default: => NegLong): NegLong =
      if (isValid(value)) value else default

    /** Largest valid [[NegLong]] value, equal to -1L. */
    val MaxValue: NegLong = -1L

    /** Smallest valid [[NegLong]] value, equal to Long.MinValue. */
    val MinValue: NegLong = Long.MinValue

    extension (x: NegLong) {
      /** Return the underlying Long value. */
      def value: Long = x
      /** Bitwise complement of the underlying value. */
      def unary_~ : Long = ~x
      /** Unary plus returns this value unchanged. */
      def unary_+ : NegLong = x
      /** Numeric negation as Long. */
      def unary_- : Long = -x
      /** Concatenate the string form of this value with the provided suffix. */
      def +(suffix: String): String = s"${x.toString()}$suffix"
      /** Left shift by an Int amount. */
      def <<(shift: Int): Long = x << shift
      /** Left shift by a Long amount. */
      def <<(shift: Long): Long = x << shift
      /** Unsigned right shift by an Int amount. */
      def >>>(shift: Int): Long = x >>> shift
      /** Unsigned right shift by a Long amount. */
      def >>>(shift: Long): Long = x >>> shift
      /** Signed right shift by an Int amount. */
      def >>(shift: Int): Long = x >> shift
      /** Signed right shift by a Long amount. */
      def >>(shift: Long): Long = x >> shift
      /** Bitwise OR with a Byte. */
      def |(other: Byte): Long = x | other
      /** Bitwise OR with a Short. */
      def |(other: Short): Long = x | other
      /** Bitwise OR with a Char. */
      def |(other: Char): Long = x | other
      /** Bitwise OR with an Int. */
      def |(other: Int): Long = x | other
      /** Bitwise OR with a Long. */
      def |(other: Long): Long = x | other
      /** Bitwise AND with a Byte. */
      def &(other: Byte): Long = x & other
      /** Bitwise AND with a Short. */
      def &(other: Short): Long = x & other
      /** Bitwise AND with a Char. */
      def &(other: Char): Long = x & other
      /** Bitwise AND with an Int. */
      def &(other: Int): Long = x & other
      /** Bitwise AND with a Long. */
      def &(other: Long): Long = x & other
      /** Bitwise XOR with a Byte. */
      def ^(other: Byte): Long = x ^ other
      /** Bitwise XOR with a Short. */
      def ^(other: Short): Long = x ^ other
      /** Bitwise XOR with a Char. */
      def ^(other: Char): Long = x ^ other
      /** Bitwise XOR with an Int. */
      def ^(other: Int): Long = x ^ other
      /** Bitwise XOR with a Long. */
      def ^(other: Long): Long = x ^ other
      /** Return the greater of this and that value. */
      def max(that: NegLong): NegLong = if (math.max(x, that) == x) x else that
      /** Return the lesser of this and that value. */
      def min(that: NegLong): NegLong = if (math.min(x, that) == x) x else that
      /** Unsigned binary string representation. */
      def toBinaryString: String = java.lang.Long.toBinaryString(x)
      /** Unsigned hexadecimal string representation. */
      def toHexString: String = java.lang.Long.toHexString(x)
      /** Unsigned octal string representation. */
      def toOctalString: String = java.lang.Long.toOctalString(x)
      /** Exclusive range from this value up to but excluding `end` with step 1. */
      def until(end: Long): NumericRange.Exclusive[Long] = NumericRange.Exclusive(x, end, 1L)
      /** Exclusive range from this value up to but excluding `end` with custom step. */
      def until(end: Long, step: Long): NumericRange.Exclusive[Long] = NumericRange.Exclusive(x, end, step)
      /** Inclusive range from this value to `end` with step 1. */
      def to(end: Long): NumericRange.Inclusive[Long] = NumericRange.Inclusive(x, end, 1L)
      /** Inclusive range from this value to `end` with custom step. */
      def to(end: Long, step: Long): NumericRange.Inclusive[Long] = NumericRange.Inclusive(x, end, step)

      /** Apply `f` and require the result to remain a valid [[NegLong]]. */
      def ensuringValid(f: Long => Long): NegLong = {
        val res = f(x)
        if (res >= 0L)
          throw new AssertionError(Resources.invalidNegLong)
        else res
      }
    }

    /** Widen [[NegLong]] to Long. */
    given Conversion[NegLong, Long] with {
      def apply(x: NegLong): Long = x
    }

    /** Ordering instance based on underlying numeric Long ordering. */
    given Ordering[NegLong] with {
      def compare(x: NegLong, y: NegLong): Int = x.compareTo(y)
    }
  }

  /** Opaque type representing negative-or-zero Long values (<= 0). */
  opaque type NegZLong = Long

  /** Companion object for [[NegZLong]] validation helpers. */
  object NegZLong {
    /** Validate and return the given Long as [[NegZLong]]. */
    def ensuringValid(l: Long): NegZLong = 
      if (l > 0) 
        throw new AssertionError(Resources.invalidNegZLong)
      else l
  }
}