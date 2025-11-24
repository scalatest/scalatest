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
import scala.collection.immutable.NumericRange

import PosFloats.PosZFloat
import PosDoubles.PosZDouble

object PosLongs {

  opaque type PosZLong = Long

  trait PosZLongConversionsLowPriority {
    /** Convert a [[PosZLong]] to a Float preserving its numeric value. */
    given Conversion[PosZLong, Float] with {
      def apply(pos: PosZLong): Float = pos.toFloat
    }
    /** Convert a [[PosZLong]] to a Double preserving its numeric value. */
    given Conversion[PosZLong, Double] with {
      def apply(pos: PosZLong): Double = pos.toDouble
    }
    /** Convert a [[PosZLong]] to a [[PosZFloat]] with the same numeric value. */
    given Conversion[PosZLong, PosZFloat] with {
      def apply(pos: PosZLong): PosZFloat = PosZFloat.ensuringValid(pos.toFloat)
    }
    /** Convert a [[PosZLong]] to a [[PosZDouble]] with the same numeric value. */
    given Conversion[PosZLong, PosZDouble] with {
      def apply(pos: PosZLong): PosZDouble = PosZDouble.ensuringValid(pos.toDouble)
    }
  }

  object PosZLong extends PosZLongConversionsLowPriority {

    /** Compile-time factory for creating a [[PosZLong]] from an integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative literals. Use it as: `PosZLong(5)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton Int literal type
      * @param i the Int literal
      * @return a [[PosZLong]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosZLong =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v < 0 then
            error("PosZLong cannot be instantiated with a negative int literal")
          else
            v.asInstanceOf[PosZLong]
        case None =>
          error("PosZLong.apply requires an int or long literal")
      }
    
    /** Compile-time factory for creating a [[PosZLong]] from an long literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects negative literals. Use it as: `PosZLong(5L)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton Long literal type
      * @param l the Long literal
      * @return a [[PosZLong]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosZLong =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v < 0L then
            error("PosZLong cannot be instantiated with a negative long literal")
          else
            v.asInstanceOf[PosZLong]
        case None =>
          error("PosZLong.apply requires an int or long literal")
      }

    /** 
    * Return true when the provided Long is a valid [[PosZLong]] value (>= 0). 
    *
    * @param value the Long to validate
    * @return true if the specified Long is a non-negative long, else false
    */
    def isValid(value: Long): Boolean = value >= 0  

    def from(l: Long): Option[PosZLong] =
      if (isValid(l)) Some(l) else None

    def ensuringValid(l: Long): PosZLong = 
      if (!isValid(l)) 
        throw new AssertionError(Resources.invalidPosZLong)
      else l  

    /**
      * A factory/validation method that produces a <code>PosZLong</code>, wrapped
      * in a <code>Success</code>, given a valid <code>Long</code> value, or if the
      * given <code>Long</code> is invalid, an <code>AssertionError</code>, wrapped
      * in a <code>Failure</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a PosZLong <code>Long</code>, it will return a <code>PosZLong</code>
      * representing that value, wrapped in a <code>Success</code>.
      * Otherwise, if the passed <code>Long</code> value is not PosZLong, this
      * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Long</code> literals at compile time, whereas this method inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @param value the <code>Long</code> to inspect, and if a zero or non-negative long, return
      *     wrapped in a <code>Success(PosZLong)</code>.
      * @return the specified <code>Long</code> value wrapped
      *     in a <code>Success(PosZLong)</code>, if it is a non-negative long, else a <code>Failure(AssertionError)</code>.
      */
    def tryingValid(value: Long): Try[PosZLong] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZLong))

    /**
      * A validation method that produces a <code>Pass</code>
      * given a valid <code>Long</code> value, or
      * an error value of type <code>E</code> produced by passing the
      * given <em>invalid</em> <code>Long</code> value
      * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a non-negative <code>Long</code>, it will return a <code>Pass</code>.
      * Otherwise, the passed <code>Long</code> value is not a non-negative long, so this
      * method will return a result of type <code>E</code> obtained by passing
      * the invalid <code>Long</code> value to the given function <code>f</code>,
      * wrapped in a `Fail`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Long</code> literals at compile time, whereas this method inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @tparam E error type produced by f
      * @param value the `Long` to validate that it is a non-negative long.
      * @param f function to produce an error when value is invalid
      * @return a `Pass` if the specified `Long` value is a non-negative long,
      *   else a `Fail` containing an error value produced by passing the
      *   specified `Long` to the given function `f`.
      */
    def passOrElse[E](value: Long)(f: Long => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))          

    /**
      * A factory/validation method that produces a <code>PosZLong</code>, wrapped
      * in a <code>Good</code>, given a valid <code>Long</code> value, or if the
      * given <code>Long</code> is invalid, an error value of type <code>B</code>
      * produced by passing the given <em>invalid</em> <code>Long</code> value
      * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a PosZLong <code>Long</code>, it will return a <code>PosZLong</code>
      * representing that value, wrapped in a <code>Good</code>.
      * Otherwise, the passed <code>Long</code> value is not PosZLong, so this
      * method will return a result of type <code>B</code> obtained by passing
      * the invalid <code>Long</code> value to the given function <code>f</code>,
      * wrapped in a `Bad`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Long</code> literals at compile time, whereas this method inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @tparam B error type produced by f
      * @param value the <code>Long</code> to inspect, and if PosZLong, return
      *     wrapped in a <code>Good(PosZLong)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Long</code> value wrapped
      *     in a <code>Good(PosZLong)</code>, if it is PosZLong, else a <code>Bad(f(value))</code>.
      */
    def goodOrElse[B](value: Long)(f: Long => B): PosZLong Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /**
    * A factory/validation method that produces a <code>PosZLong</code>, wrapped
    * in a <code>Right</code>, given a valid <code>Long</code> value, or if the
    * given <code>Long</code> is invalid, an error value of type <code>L</code>
    * produced by passing the given <em>invalid</em> <code>Long</code> value
    * to the given function <code>f</code>, wrapped in a <code>Left</code>.
    *
    * <p>
    * This method will inspect the passed <code>Long</code> value and if
    * it is a PosZLong <code>Long</code>, it will return a <code>PosZLong</code>
    * representing that value, wrapped in a <code>Right</code>.
    * Otherwise, the passed <code>Long</code> value is not PosZLong, so this
    * method will return a result of type <code>L</code> obtained by passing
    * the invalid <code>Long</code> value to the given function <code>f</code>,
    * wrapped in a `Left`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Long</code> literals at compile time, whereas this method inspects
    * <code>Long</code> values at run time.
    * </p>
    *
    * @tparam L error type produced by f
    * @param value the <code>Long</code> to inspect, and if PosZLong, return
    *     wrapped in a <code>Right(PosZLong)</code>.
    * @param f function to produce an error when value is invalid
    * @return the specified <code>Long</code> value wrapped
    *     in a <code>Right(PosZLong)</code>, if it is PosZLong, else a <code>Left(f(value))</code>.
    */
    def rightOrElse[L](value: Long)(f: Long => L): Either[L, PosZLong] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /**
      * A factory method that produces a <code>PosZLong</code> given a
      * <code>Long</code> value and a default <code>PosZLong</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a positive <code>Long</code>, <em>i.e.</em>, a value greater
      * than 0.0, it will return a <code>PosZLong</code> representing that value.
      * Otherwise, the passed <code>Long</code> value is 0 or negative, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code>
      * factory method in that <code>apply</code> is implemented
      * via a macro that inspects <code>Long</code> literals at
      * compile time, whereas <code>from</code> inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @param value the <code>Long</code> to inspect, and if positive, return.
      * @param default the <code>PosZLong</code> to return if the passed
      *     <code>Long</code> value is not positive.
      * @return the specified <code>Long</code> value wrapped in a
      *     <code>PosZLong</code>, if it is positive, else the
      *     <code>default</code> <code>PosZLong</code> value.
      */
    def fromOrElse(value: Long, default: => PosZLong): PosZLong =
      if (isValid(value)) value else default    

    /**
    * The largest value representable as a zero and positive long <code>Long</code>, which is <code>PosZLong(9223372036854775807)</code>.
    */
    val MaxValue: PosZLong = PosZLong.ensuringValid(Long.MaxValue)

    /**
    * The smallest value representable as a zero and positive long <code>Long</code>, which is <code>PosZLong(0)</code>.
    */
    val MinValue: PosZLong = PosZLong.ensuringValid(0L)

    /** Convert a [[PosZInt]] to a plain Int (unwrap). */
    given Conversion[PosZLong, Long] with {
      def apply(x: PosZLong): Long = x
    }

    /** Convert a compile-time Int literal or runtime Int to a [[PosZLong]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Int, PosZLong] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosZLong =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v < 0 then
              error("PosZLong cannot be instantiated with a negative integer literal")
            else
              v.toLong.asInstanceOf[PosZLong]
          case None =>
            error("PosZLong conversion requires a integer literal")
        }
      def apply(x: Int): PosZLong = x.toLong
    }

    /** Convert a compile-time Long literal or runtime Long to a [[PosZLong]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Long, PosZLong] with {
      inline def apply[L <: Long & Singleton](inline x: L): PosZLong =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v < 0 then
              error("PosZLong cannot be instantiated with a negative long literal")
            else
              v.asInstanceOf[PosZLong]
          case None =>
            error("PosZLong conversion requires a long literal")
        }
      def apply(x: Long): PosZLong = PosZLong.ensuringValid(x)
    }

    /** Ordering instance for PosZLong that orders by numeric value. */
    given Ordering[PosZLong] with {
      def compare(x: PosZLong, y: PosZLong): Int = x.compareTo(y)
    }

    extension (x: PosZLong) {
      /** Return the underlying Long value. */
      def value: Long = x
      /**
        * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
        */
      /** Return the greater of this and that. */
      def max(that: PosZLong): PosZLong = math.max(x, that)

      /**
        * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
        */
      /** Return the lesser of this and that. */
      def min(that: PosZLong): PosZLong = math.min(x, that)

      /** Return the unsigned binary string representation of the underlying Long. */
      def toBinaryString: String = java.lang.Long.toBinaryString(x)

      /** Return the unsigned hexadecimal string representation of the underlying Long. */
      def toHexString: String = java.lang.Long.toHexString(x)

      /** Return the unsigned octal string representation of the underlying Long. */
      def toOctalString: String = java.lang.Long.toOctalString(x)

      /**
        * Create a <code>Range</code> from this <code>PosZInt</code> value
        * until the specified <code>end</code> (exclusive) with step value 1.
        *
        * @param end The final bound of the range to make.
        * @return A [[scala.collection.immutable.Range]] from `this` up to but
        * not including `end`.
        */
      def until(end: Long): NumericRange.Exclusive[Long] = NumericRange.Exclusive(x, end, 1L)

      /**
        * Create a <code>Range</code> from this <code>PosZInt</code> value
        * until the specified <code>end</code> (exclusive) with the specified <code>step</code> value.
        *
        * @param end The final bound of the range to make.
        * @param step The number to increase by for each step of the range.
        * @return A [[scala.collection.immutable.Range]] from `this` up to but
        * not including `end`.
        */
      def until(end: Long, step: Long): NumericRange.Exclusive[Long] = NumericRange.Exclusive(x, end, step)

      /**
        * Create an inclusive <code>Range</code> from this <code>PosZInt</code> value
        * to the specified <code>end</code> with step value 1.
        *
        * @param end The final bound of the range to make.
        * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
        * and including `end`.
        */
      def to(end: Long): NumericRange.Inclusive[Long] = NumericRange.Inclusive(x, end, 1L)

      /**
        * Create an inclusive <code>Range</code> from this <code>PosZInt</code> value
        * to the specified <code>end</code> with the specified <code>step</code> value.
        *
        * @param end The final bound of the range to make.
        * @param step The number to increase by for each step of the range.
        * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
        * and including `end`.
        */
      def to(end: Long, step: Long): NumericRange.Inclusive[Long] = NumericRange.Inclusive(x, end, step)

      /** Apply a transformation and ensure the result is a valid [[PosZLong]].
        *
        * @param f function to transform the underlying Long
        * @return the transformed value as PosZLong if valid
        * @throws AssertionError if the result of f is negative
        */
      def ensuringValid(f: Long => Long): PosZLong = {
        val res = f(x)
        if (res < 0)
          throw new AssertionError(Resources.invalidPosZLong)
        else res
      }
    }

  }

  opaque type PosLong <: PosZLong = Long

  object PosLong {

    /** Convert a [[PosLong]] to a plain Long (unwrap). */
    given Conversion[PosLong, Long] with {
      def apply(x: PosLong): Long = x
    }

    /** Compile-time factory for creating a [[PosLong]] from an long literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects negative literals. Use it as: `PosLong(5L)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton Long literal type
      * @param l the Long literal
      * @return a [[PosLong]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosLong =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v <= 0L then
            error("PosLong cannot be instantiated with a negative long literal")
          else
            v.asInstanceOf[PosLong]
        case None =>
          error("PosLong.apply requires an int or long literal")
      }

    /** 
     * Return true when the provided Int is a valid [[PosLong]] value (> 0L). 
     *
     * @param value the Long to validate
     * @return true if the specified Long is a positive long, else false
     */
    def isValid(value: Long): Boolean = value > 0L  

    /** Create a [[PosLong]] if the given Int is valid.
      *
      * @param l the Long to inspect
      * @return Some([[PosLong]]) if the given Long is greater than 0, else None
      */
    def from(l: Long): Option[PosLong] =
      if (isValid(l)) Some(l) else None  

    /** Create a [[PosLong]], throwing an AssertionError if the given Int is invalid.
     *
     * @param l the Long to inspect
     * @return the [[PosLong]] if the given Long is greater than 0
     * @throws AssertionError if the given Long is less than or equal to 0
     */
    def ensuringValid(l: Long): PosLong = 
      if (!isValid(l)) 
        throw new AssertionError(Resources.invalidPosLong)
      else l  

    /**
      * A factory/validation method that produces a <code>PosLong</code>, wrapped
      * in a <code>Success</code>, given a valid <code>Long</code> value, or if the
      * given <code>Long</code> is invalid, an <code>AssertionError</code>, wrapped
      * in a <code>Failure</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a PosLong <code>Long</code>, it will return a <code>PosLong</code>
      * representing that value, wrapped in a <code>Success</code>.
      * Otherwise, if the passed <code>Long</code> value is not PosLong, this
      * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Long</code> literals at compile time, whereas this method inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @param value the <code>Long</code> to inspect, and if a non-negative long, return
      *     wrapped in a <code>Success(PosLong)</code>.
      * @return the specified <code>Long</code> value wrapped
      *     in a <code>Success(PosLong)</code>, if it is a non-negative long, else a <code>Failure(AssertionError)</code>.
      */
    def tryingValid(value: Long): Try[PosLong] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosLong))

    /**
      * A validation method that produces a <code>Pass</code>
      * given a valid <code>Long</code> value, or
      * an error value of type <code>E</code> produced by passing the
      * given <em>invalid</em> <code>Long</code> value
      * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a non-negative <code>Long</code>, it will return a <code>Pass</code>.
      * Otherwise, the passed <code>Long</code> value is not a non-negative long, so this
      * method will return a result of type <code>E</code> obtained by passing
      * the invalid <code>Long</code> value to the given function <code>f</code>,
      * wrapped in a `Fail`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Long</code> literals at compile time, whereas this method inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @tparam E error type produced by f
      * @param value the `Long` to validate that it is a non-negative long.
      * @param f function to produce an error when value is invalid
      * @return a `Pass` if the specified `Long` value is a non-negative long,
      *   else a `Fail` containing an error value produced by passing the
      *   specified `Long` to the given function `f`.
      */
    def passOrElse[E](value: Long)(f: Long => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /**
      * A factory/validation method that produces a <code>PosLong</code>, wrapped
      * in a <code>Good</code>, given a valid <code>Long</code> value, or if the
      * given <code>Long</code> is invalid, an error value of type <code>B</code>
      * produced by passing the given <em>invalid</em> <code>Long</code> value
      * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a PosLong <code>Long</code>, it will return a <code>PosLong</code>
      * representing that value, wrapped in a <code>Good</code>.
      * Otherwise, the passed <code>Long</code> value is not PosLong, so this
      * method will return a result of type <code>B</code> obtained by passing
      * the invalid <code>Long</code> value to the given function <code>f</code>,
      * wrapped in a `Bad`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Long</code> literals at compile time, whereas this method inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @tparam B error type produced by f
      * @param value the <code>Long</code> to inspect, and if PosLong, return
      *     wrapped in a <code>Good(PosLong)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Long</code> value wrapped
      *     in a <code>Good(PosLong)</code>, if it is PosLong, else a <code>Bad(f(value))</code>.
      */
    def goodOrElse[B](value: Long)(f: Long => B): PosLong Or B =
      if (isValid(value)) Good(value) else Bad(f(value))  

    /**
    * A factory/validation method that produces a <code>PosLong</code>, wrapped
    * in a <code>Right</code>, given a valid <code>Long</code> value, or if the
    * given <code>Long</code> is invalid, an error value of type <code>L</code>
    * produced by passing the given <em>invalid</em> <code>Long</code> value
    * to the given function <code>f</code>, wrapped in a <code>Left</code>.
    *
    * <p>
    * This method will inspect the passed <code>Long</code> value and if
    * it is a PosLong <code>Long</code>, it will return a <code>PosLong</code>
    * representing that value, wrapped in a <code>Right</code>.
    * Otherwise, the passed <code>Long</code> value is not PosLong, so this
    * method will return a result of type <code>L</code> obtained by passing
    * the invalid <code>Long</code> value to the given function <code>f</code>,
    * wrapped in a `Left`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Long</code> literals at compile time, whereas this method inspects
    * <code>Long</code> values at run time.
    * </p>
    *
    * @tparam L error type produced by f
    * @param value the <code>Long</code> to inspect, and if PosLong, return
    *     wrapped in a <code>Right(PosLong)</code>.
    * @param f function to produce an error when value is invalid
    * @return the specified <code>Long</code> value wrapped
    *     in a <code>Right(PosLong)</code>, if it is PosLong, else a <code>Left(f(value))</code>.
    */
    def rightOrElse[L](value: Long)(f: Long => L): Either[L, PosLong] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /**
      * A factory method that produces a <code>PosLong</code> given a
      * <code>Long</code> value and a default <code>PosLong</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a positive <code>Long</code>, <em>i.e.</em>, a value greater
      * than 0.0, it will return a <code>PosLong</code> representing that value.
      * Otherwise, the passed <code>Long</code> value is 0 or negative, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code>
      * factory method in that <code>apply</code> is implemented
      * via a macro that inspects <code>Long</code> literals at
      * compile time, whereas <code>from</code> inspects
      * <code>Long</code> values at run time.
      * </p>
      *
      * @param value the <code>Long</code> to inspect, and if positive, return.
      * @param default the <code>PosLong</code> to return if the passed
      *     <code>Long</code> value is not positive.
      * @return the specified <code>Long</code> value wrapped in a
      *     <code>PosLong</code>, if it is positive, else the
      *     <code>default</code> <code>PosLong</code> value.
      */
    def fromOrElse(value: Long, default: => PosLong): PosLong =
      if (isValid(value)) value else default    

    /**
    * The largest value representable as a non-negative long <code>Long</code>, which is <code>PosLong(9223372036854775807)</code>.
    */
    val MaxValue: PosLong = PosLong.ensuringValid(Long.MaxValue)

    /**
    * The smallest value representable as a non-negative long <code>Long</code>, which is <code>PosZLong(1)</code>.
    */
    val MinValue: PosLong = PosLong.ensuringValid(1L)  
  }

}
