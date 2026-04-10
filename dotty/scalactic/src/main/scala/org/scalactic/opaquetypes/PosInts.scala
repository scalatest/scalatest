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
import PosLongs.PosZLong
import PosDoubles.PosZDouble

object PosInts {

  /** Opaque type representing non-negative (zero or positive) Int values.
   *
   * Instances of this type are guaranteed to be >= 0. Use the factory and
   * validation methods in the companion object to create or validate values.
   */
  opaque type PosZInt = Int

  /** Lower-priority given conversions for PosZInt.
    *
    * These conversions are provided at low priority to avoid 
    * conflict resolution in the presence of other numeric conversions.
    */
  trait PosZIntConversionsLowPriority {
    /** Convert a [[PosZInt]] to a plain Long with the same numeric value. */
    given Conversion[PosZInt, Long] with {
      def apply(pos: PosZInt): Long = pos.toLong
    }
    /** Convert a [[PosZInt]] to a Double preserving its numeric value. */
    given Conversion[PosZInt, Double] with {
      def apply(pos: PosZInt): Double = pos.toDouble
    }
    /** Convert a [[PosZInt]] to a [[PosZLong]] with the same numeric value. */
    given Conversion[PosZInt, PosZLong] with {
      def apply(pos: PosZInt): PosZLong = PosZLong.ensuringValid(pos.toLong)
    }
    /** Convert a [[PosZInt]] to a [[PosZDouble]] with the same numeric value. */
    given Conversion[PosZInt, PosZDouble] with {
      def apply(pos: PosZInt): PosZDouble = PosZDouble.ensuringValid(pos.toDouble)
    }
  }

  /** Companion object for the [[PosZInt]] opaque type.
    *
    * Provides factory and validation methods, given conversions, extension
    * methods, and useful constants (e.g. MaxValue, MinValue). Prefer the
    * inline [[apply]] overload for compile-time checked construction from
    * integer literals; use [[ensuringValid]], [[from]], or other helpers for
    * runtime values.
    */
  object PosZInt extends PosZIntConversionsLowPriority {
    
    /** Compile-time factory for creating a [[PosZInt]] from an integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative literals. Use it as: `PosZInt(5)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton Int literal type
      * @param i the Int literal
      * @return a [[PosZInt]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosZInt =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v < 0 then
            error("PosZInt cannot be instantiated with a negative integer literal")
          else
            v.asInstanceOf[PosZInt]
        case None =>
          error("PosZInt.apply requires an integer literal")
      }

    /** Construct a [[PosZInt]] from a runtime Int if it is non-negative.
      *
      * @param i runtime Int to validate
      * @return Some(PosZInt) if i >= 0, otherwise None
      */
    def from(i: Int): Option[PosZInt] =
      if (i >= 0) Some(i) else None

    /** Ensure the runtime Int is non-negative and return it as a [[PosZInt]].
      *
      * @param i runtime Int to check
      * @return the given integer as a [[PosZInt]] if valid
      * @throws AssertionError if the given Int is negative
      */
    def ensuringValid(i: Int): PosZInt =
      if (i < 0)
        throw new AssertionError(Resources.invalidPosZInt)
      else i

    /**
    * A factory/validation method that produces a <code>PosZInt</code>, wrapped
    * in a <code>Success</code>, given a valid <code>Int</code> value, or if the
    * given <code>Int</code> is invalid, an <code>AssertionError</code>, wrapped
    * in a <code>Failure</code>.
    *
    * <p>
    * This method will inspect the passed <code>Int</code> value and if
    * it is a PosZInt <code>Int</code>, it will return a <code>PosZInt</code>
    * representing that value, wrapped in a <code>Success</code>.
    * Otherwise, if the passed <code>Int</code> value is not PosZInt, this
    * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Int</code> literals at compile time, whereas this method inspects
    * <code>Int</code> values at run time.
    * </p>
    *
    * @param value the <code>Int</code> to inspect, and if a non-negative integer, return
    *     wrapped in a <code>Success(PosZInt)</code>.
    * @return the specified <code>Int</code> value wrapped
    *     in a <code>Success(PosZInt)</code>, if it is a non-negative integer, else a <code>Failure(AssertionError)</code>.
    */
    def tryingValid(value: Int): Try[PosZInt] =
      if (value >= 0)
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZInt))

    /** 
    * Return true when the provided Int is a valid [[PosZInt]] value (>= 0). 
    *
    * @param value the Int to validate
    * @return true if the specified Int is a non-negative integer, else false
    */
    def isValid(value: Int): Boolean = value >= 0

    /**
    * A validation method that produces a <code>Pass</code>
    * given a valid <code>Int</code> value, or
    * an error value of type <code>E</code> produced by passing the
    * given <em>invalid</em> <code>Int</code> value
    * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
    *
    * <p>
    * This method will inspect the passed <code>Int</code> value and if
    * it is a non-negative integer <code>Int</code>, it will return a <code>Pass</code>.
    * Otherwise, the passed <code>Int</code> value is not a non-negative integer, so this
    * method will return a result of type <code>E</code> obtained by passing
    * the invalid <code>Int</code> value to the given function <code>f</code>,
    * wrapped in a `Fail`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Int</code> literals at compile time, whereas this method inspects
    * <code>Int</code> values at run time.
    * </p>
    *
    * @tparam E error type produced by f
    * @param value the `Int` to validate that it is a non-negative integer.
    * @param f function to produce an error when value is invalid
    * @return a `Pass` if the specified `Int` value is a non-negative integer,
    *   else a `Fail` containing an error value produced by passing the
    *   specified `Int` to the given function `f`.
    */
    def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))


    /**
    * A factory/validation method that produces a <code>PosZInt</code>, wrapped
    * in a <code>Good</code>, given a valid <code>Int</code> value, or if the
    * given <code>Int</code> is invalid, an error value of type <code>B</code>
    * produced by passing the given <em>invalid</em> <code>Int</code> value
    * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
    *
    * <p>
    * This method will inspect the passed <code>Int</code> value and if
    * it is a PosZInt <code>Int</code>, it will return a <code>PosZInt</code>
    * representing that value, wrapped in a <code>Good</code>.
    * Otherwise, the passed <code>Int</code> value is not PosZInt, so this
    * method will return a result of type <code>B</code> obtained by passing
    * the invalid <code>Int</code> value to the given function <code>f</code>,
    * wrapped in a `Bad`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Int</code> literals at compile time, whereas this method inspects
    * <code>Int</code> values at run time.
    * </p>
    *
    * @tparam B error type produced by f
    * @param value the <code>Int</code> to inspect, and if PosZInt, return
    *     wrapped in a <code>Good(PosZInt)</code>.
    * @param f function to produce an error when value is invalid
    * @return the specified <code>Int</code> value wrapped
    *     in a <code>Good(PosZInt)</code>, if it is PosZInt, else a <code>Bad(f(value))</code>.
    */
    def goodOrElse[B](value: Int)(f: Int => B): PosZInt Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /**
    * A factory/validation method that produces a <code>PosZInt</code>, wrapped
    * in a <code>Right</code>, given a valid <code>Int</code> value, or if the
    * given <code>Int</code> is invalid, an error value of type <code>L</code>
    * produced by passing the given <em>invalid</em> <code>Int</code> value
    * to the given function <code>f</code>, wrapped in a <code>Left</code>.
    *
    * <p>
    * This method will inspect the passed <code>Int</code> value and if
    * it is a PosZInt <code>Int</code>, it will return a <code>PosZInt</code>
    * representing that value, wrapped in a <code>Right</code>.
    * Otherwise, the passed <code>Int</code> value is not PosZInt, so this
    * method will return a result of type <code>L</code> obtained by passing
    * the invalid <code>Int</code> value to the given function <code>f</code>,
    * wrapped in a `Left`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Int</code> literals at compile time, whereas this method inspects
    * <code>Int</code> values at run time.
    * </p>
    *
    * @tparam L error type produced by f
    * @param value the <code>Int</code> to inspect, and if PosZInt, return
    *     wrapped in a <code>Right(PosZInt)</code>.
    * @param f function to produce an error when value is invalid
    * @return the specified <code>Int</code> value wrapped
    *     in a <code>Right(PosZInt)</code>, if it is PosZInt, else a <code>Left(f(value))</code>.
    */
    def rightOrElse[L](value: Int)(f: Int => L): Either[L, PosZInt] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /**
    * A factory method that produces a <code>PosZInt</code> given a
    * <code>Int</code> value and a default <code>PosZInt</code>.
    *
    * <p>
    * This method will inspect the passed <code>Int</code> value and if
    * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
    * than 0.0, it will return a <code>PosZInt</code> representing that value.
    * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
    * method will return the passed <code>default</code> value.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code>
    * factory method in that <code>apply</code> is implemented
    * via a macro that inspects <code>Int</code> literals at
    * compile time, whereas <code>from</code> inspects
    * <code>Int</code> values at run time.
    * </p>
    *
    * @param value the <code>Int</code> to inspect, and if positive, return.
    * @param default the <code>PosZInt</code> to return if the passed
    *     <code>Int</code> value is not positive.
    * @return the specified <code>Int</code> value wrapped in a
    *     <code>PosZInt</code>, if it is positive, else the
    *     <code>default</code> <code>PosZInt</code> value.
    */
    def fromOrElse(value: Int, default: => PosZInt): PosZInt =
      if (isValid(value)) value else default

    /**
    * The largest value representable as a non-negative integer <code>Int</code>, which is <code>PosZInt(2147483647)</code>.
    */
    val MaxValue: PosZInt = Int.MaxValue

    /**
    * The smallest value representable as a non-negative integer <code>Int</code>, which is <code>PosZInt(0)</code>.
    */
    val MinValue: PosZInt = 0
    
    extension (x: PosZInt) {
      /** Return the underlying Int value. */
      def value: Int = x
      /** Absolute value (no-op for non-negative integers). */
      def abs: PosZInt = x
      /**
        * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
        */
      /** Return the greater of this and that. */
      def max(that: PosZInt): PosZInt = math.max(x, that)

      /**
        * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
        */
      /** Return the lesser of this and that. */
      def min(that: PosZInt): PosZInt = math.min(x, that)

      /** Return the unsigned binary string representation of the underlying Int. */
      def toBinaryString: String = java.lang.Integer.toBinaryString(x)

      /** Return the unsigned hexadecimal string representation of the underlying Int. */
      def toHexString: String = java.lang.Integer.toHexString(x)

      /** Return the unsigned octal string representation of the underlying Int. */
      def toOctalString: String = java.lang.Integer.toOctalString(x)

      /**
        * Create an inclusive <code>Range</code> from this <code>PosZInt</code> value
        * to the specified <code>end</code> with step value 1.
        *
        * @param end The final bound of the range to make.
        * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
        * and including `end`.
        */
      def to(end: Int): Range.Inclusive = Range.inclusive(x, end)

      /**
        * Create an inclusive <code>Range</code> from this <code>PosZInt</code> value
        * to the specified <code>end</code> with the specified <code>step</code> value.
        *
        * @param end The final bound of the range to make.
        * @param step The number to increase by for each step of the range.
        * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
        * and including `end`.
        */
      def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(x, end, step)

      /**
        * Create a <code>Range</code> from this <code>PosZInt</code> value
        * until the specified <code>end</code> (exclusive) with step value 1.
        *
        * @param end The final bound of the range to make.
        * @return A [[scala.collection.immutable.Range]] from `this` up to but
        * not including `end`.
        */
      def until(end: Int): Range = Range(value, end)

      /**
        * Create a <code>Range</code> from this <code>PosZInt</code> value
        * until the specified <code>end</code> (exclusive) with the specified <code>step</code> value.
        *
        * @param end The final bound of the range to make.
        * @param step The number to increase by for each step of the range.
        * @return A [[scala.collection.immutable.Range]] from `this` up to but
        * not including `end`.
        */
      def until(end: Int, step: Int): Range = Range(value, end, step)

      /** Apply a transformation and ensure the result is a valid [[PosZInt]].
        *
        * @param f function to transform the underlying Int
        * @return the transformed value as PosZInt if valid
        * @throws AssertionError if the result of f is negative
        */
      def ensuringValid(f: Int => Int): PosZInt = {
        val res = f(x)
        if (res < 0)
          throw new AssertionError(Resources.invalidPosZInt)
        else res
      }
    }
    
    /** Convert a [[PosZInt]] to a plain Int (unwrap). */
    given Conversion[PosZInt, Int] with {
      def apply(x: PosZInt): Int = x
    }
  
    /** Convert a compile-time Int literal or runtime Int to a [[PosZInt]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Int, PosZInt] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosZInt =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v < 0 then
              error("PosZInt cannot be instantiated with a negative integer literal")
            else
              v.asInstanceOf[PosZInt]
          case None =>
            error("PosZInt conversion requires an integer literal")
        }
      def apply(x: Int): PosZInt = PosZInt.ensuringValid(x)
    }
  
    /** Ordering instance for PosZInt that orders by numeric value. */
    given Ordering[PosZInt] with {
      def compare(x: PosZInt, y: PosZInt): Int = x.compareTo(y)
    }
  }

  /** Opaque type representing positive (greater than zero) Int values.
   *
   * Instances of this type are guaranteed to be > 0. Use the factory and
   * validation methods in the companion object to create or validate values.
   */
  opaque type PosInt <: PosZInt = Int

  /** Companion object for the [[PosInt]] opaque type.
   *
   * Provides factory and validation methods, given conversions, extension
   * methods, and useful constants (e.g. MaxValue, MinValue). Prefer the
   * inline [[apply]] overload for compile-time checked construction from
   * integer literals; use [[ensuringValid]], [[from]], or other helpers for
   * runtime values.
   */
  object PosInt {
    /** Compile-time factory for creating a [[PosInt]] from an integer literal.
     *
     * This inline method inspects the provided integer literal at compile time
     * and rejects non-positive literals. Use it as: `PosInt(5)`. For non-literal
     * values, use [[ensuringValid]] or [[from]].
     *
     * @tparam I the singleton Int literal type
     * @param i the Int literal
     * @return a [[PosInt]] representing the given positive literal
     * @throws a compile-time error if the literal is negative, zero or not a literal
     */
    inline def apply[I <: Int & Singleton](inline i: I): PosInt =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v <= 0 then
            error("PosInt cannot be instantiated with a negative or zero integer literal")
          else
            v.asInstanceOf[PosInt]
        case None =>
          error("PosInt.apply requires an integer literal")
      }

    /** 
     * Return true when the provided Int is a valid [[PosInt]] value (> 0). 
     *
     * @param value the Int to validate
     * @return true if the specified Int is a positive integer, else false
     */
    def isValid(value: Int): Boolean = value > 0  

    /** Create a [[PosInt]] if the given Int is valid.
      *
      * @param i the Int to inspect
      * @return Some([[PosInt]]) if the given Int is greater than 0, else None
      */
    def from(i: Int): Option[PosInt] =
      if (i > 0) Some(i) else None

    /** Create a [[PosInt]], throwing an AssertionError if the given Int is invalid.
     *
     * @param i the Int to inspect
     * @return the [[PosInt]] if the given Int is greater than 0
     * @throws AssertionError if the given Int is less than or equal to 0
     */
    def ensuringValid(i: Int): PosInt =
      if (i <= 0)
        throw new AssertionError(Resources.invalidPosInt)
      else i

    /**
     * A factory/validation method that produces a <code>PosInt</code>, wrapped
     * in a <code>Success</code>, given a valid <code>Int</code> value, or if the
     * given <code>Int</code> is invalid, an <code>AssertionError</code>, wrapped
     * in a <code>Failure</code>.
     *
     * <p>
     * This method will inspect the passed <code>Int</code> value and if
     * it is a PosInt <code>Int</code>, it will return a <code>PosInt</code>
     * representing that value, wrapped in a <code>Success</code>.
     * Otherwise, if the passed <code>Int</code> value is not PosInt, this
     * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
     * </p>
     *
     * <p>
     * This factory method differs from the <code>apply</code> factory method
     * in that <code>apply</code> is implemented via a macro that inspects
     * <code>Int</code> literals at compile time, whereas this method inspects
     * <code>Int</code> values at run time.
     * </p>
     *
     * @param value the <code>Int</code> to inspect, and if a positive integer, return
     *     wrapped in a <code>Success(PosInt)</code>.
     * @return the specified <code>Int</code> value wrapped
     *     in a <code>Success(PosInt)</code>, if it is a positive integer, else a <code>Failure(AssertionError)</code>.
     */
    def tryingValid(value: Int): Try[PosInt] =
      if (value > 0)
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosInt))

    /**
      * A validation method that produces a <code>Pass</code>
      * given a valid <code>Int</code> value, or
      * an error value of type <code>E</code> produced by passing the
      * given <em>invalid</em> <code>Int</code> value
      * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
      *
      * <p>
      * This method will inspect the passed <code>Int</code> value and if
      * it is a positive integer <code>Int</code>, it will return a <code>Pass</code>.
      * Otherwise, the passed <code>Int</code> value is not a positive integer, so this
      * method will return a result of type <code>E</code> obtained by passing
      * the invalid <code>Int</code> value to the given function <code>f</code>,
      * wrapped in a `Fail`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Int</code> literals at compile time, whereas this method inspects
      * <code>Int</code> values at run time.
      * </p>
      *
      * @tparam E error type produced by f
      * @param value the `Int` to validate that it is a positive integer.
      * @param f function to produce an error when value is invalid
      * @return a `Pass` if the specified `Int` value is a positive integer,
      *   else a `Fail` containing an error value produced by passing the
      *   specified `Int` to the given function `f`.
      */
    def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /**
      * A factory/validation method that produces a <code>PosInt</code>, wrapped
      * in a <code>Good</code>, given a valid <code>Int</code> value, or if the
      * given <code>Int</code> is invalid, an error value of type <code>B</code>
      * produced by passing the given <em>invalid</em> <code>Int</code> value
      * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
      *
      * <p>
      * This method will inspect the passed <code>Int</code> value and if
      * it is a PosInt <code>Int</code>, it will return a <code>PosInt</code>
      * representing that value, wrapped in a <code>Good</code>.
      * Otherwise, the passed <code>Int</code> value is not PosInt, so this
      * method will return a result of type <code>B</code> obtained by passing
      * the invalid <code>Int</code> value to the given function <code>f</code>,
      * wrapped in a `Bad`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Int</code> literals at compile time, whereas this method inspects
      * <code>Int</code> values at run time.
      * </p>
      *
      * @tparam B error type produced by f
      * @param value the <code>Int</code> to inspect, and if PosInt, return
      *     wrapped in a <code>Good(PosInt)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Int</code> value wrapped
      *     in a <code>Good(PosInt)</code>, if it is PosInt, else a <code>Bad(f(value))</code>.
      */
    def goodOrElse[B](value: Int)(f: Int => B): PosInt Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /**
      * A factory/validation method that produces a <code>PosInt</code>, wrapped
      * in a <code>Right</code>, given a valid <code>Int</code> value, or if the
      * given <code>Int</code> is invalid, an error value of type <code>L</code>
      * produced by passing the given <em>invalid</em> <code>Int</code> value
      * to the given function <code>f</code>, wrapped in a <code>Left</code>.
      *
      * <p>
      * This method will inspect the passed <code>Int</code> value and if
      * it is a PosInt <code>Int</code>, it will return a <code>PosInt</code>
      * representing that value, wrapped in a <code>Right</code>.
      * Otherwise, the passed <code>Int</code> value is not PosInt, so this
      * method will return a result of type <code>L</code> obtained by passing
      * the invalid <code>Int</code> value to the given function <code>f</code>,
      * wrapped in a `Left`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Int</code> literals at compile time, whereas this method inspects
      * <code>Int</code> values at run time.
      * </p>
      *
      * @tparam L error type produced by f
      * @param value the <code>Int</code> to inspect, and if PosInt, return
      *     wrapped in a <code>Right(PosInt)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Int</code> value wrapped
      *     in a <code>Right(PosInt)</code>, if it is PosInt, else a <code>Left(f(value))</code>.
      */
    def rightOrElse[L](value: Int)(f: Int => L): Either[L, PosInt] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))  
  
    /**
      * A factory method that produces a <code>PosInt</code> given a
      * <code>Int</code> value and a default <code>PosInt</code>.
      *
      * <p>
      * This method will inspect the passed <code>Int</code> value and if
      * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
      * than 0.0, it will return a <code>PosInt</code> representing that value.
      * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code>
      * factory method in that <code>apply</code> is implemented
      * via a macro that inspects <code>Int</code> literals at
      * compile time, whereas <code>from</code> inspects
      * <code>Int</code> values at run time.
      * </p>
      *
      * @param value the <code>Int</code> to inspect, and if positive, return.
      * @param default the <code>PosInt</code> to return if the passed
      *     <code>Int</code> value is not positive.
      * @return the specified <code>Int</code> value wrapped in a
      *     <code>PosInt</code>, if it is positive, else the
      *     <code>default</code> <code>PosInt</code> value.
      */
    def fromOrElse(value: Int, default: => PosInt): PosInt =
      if (isValid(value)) value else default

    /**
      * The largest value representable as a positive integer <code>Int</code>, which is <code>PosInt(2147483647)</code>.
      */
    val MaxValue: PosInt = Int.MaxValue

    /**
      * The smallest value representable as a positive integer <code>Int</code>, which is <code>PosInt(1)</code>.
      */
    val MinValue: PosInt = 1  

    /** Convert a [[PosInt]] to a plain Int (unwrap). */
    given Conversion[PosInt, Int] with {
      def apply(x: PosInt): Int = x
    }
  
    /** Convert a compile-time Int literal or runtime Int to a [[PosInt]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Int, PosInt] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosInt =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v <= 0 then
              error("PosInt cannot be instantiated with a positive integer literal")
            else
              v.asInstanceOf[PosInt]
          case None =>
            error("PosInt conversion requires an integer literal")
        }
      def apply(x: Int): PosInt = PosInt.ensuringValid(x)
    }
  }

}