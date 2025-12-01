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

import PosInts.PosZInt
import PosDoubles.{PosZDouble, PosDouble}
import NonZeroFloats.NonZeroFloat
import NonZeroDoubles.NonZeroDouble

object PosFloats {

  opaque type PosZFloat = Float

  /** Lower-priority given conversions for PosZFloat.
    *
    * These conversions are provided at low priority to avoid 
    * conflict resolution in the presence of other numeric conversions.
    */
  trait PosZFloatConversionsLowPriority {
    /** Convert a [[PosZFloat]] to a plain Double (unwrap). */
    given Conversion[PosZFloat, Double] with {
      def apply(x: PosZFloat): Double = x.toDouble
    }
    /** Convert a [[PosZFloat]] to a plain PosZDouble (unwrap). */
    given Conversion[PosZFloat, PosZDouble] with {
      def apply(x: PosZFloat): PosZDouble = PosZDouble.ensuringValid(x.toDouble)
    }
  }

  object PosZFloat extends PosZFloatConversionsLowPriority {

    /** Convert a [[PosZFloat]] to a plain Float (unwrap). */
    given Conversion[PosZFloat, Float] with {
      def apply(x: PosZFloat): Float = x.toFloat
    }

    /** Convert a compile-time Int literal or runtime Int to a [[PosZFloat]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Int, PosZFloat] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosZFloat =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v < 0 then
              error("PosZFloat cannot be instantiated with a negative integer literal")
            else
              v.toFloat.asInstanceOf[PosZFloat]
          case None =>
            error("PosZFloat conversion requires a integer literal")
        }
      def apply(x: Int): PosZFloat = x.toFloat
    }

    /** Convert a compile-time Long literal or runtime Long to a [[PosZFloat]].
      *
      * The inline overload checks long literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Long, PosZFloat] with {
      inline def apply[L <: Long & Singleton](inline x: L): PosZFloat =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v < 0L then
              error("PosZFloat cannot be instantiated with a negative long literal")
            else
              v.toFloat.asInstanceOf[PosZFloat]
          case None =>
            error("PosZFloat conversion requires a long literal")
        }
      def apply(x: Long): PosZFloat = x.toFloat
    }

    /** Convert a compile-time Float literal or runtime Float to a [[PosZFloat]].
      *
      * The inline overload checks float literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Float, PosZFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): PosZFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v < 0.0f then
              error("PosZFloat cannot be instantiated with a negative float literal")
            else
              v.toFloat.asInstanceOf[PosZFloat]
          case None =>
            error("PosZFloat conversion requires a float literal")
        }
      def apply(x: Float): PosZFloat = x
    }

    /** Compile-time factory for creating a [[PosZFloat]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects negative literals. Use it as: `PosZFloat(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the Float literal
      * @return a [[PosZFloat]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosZFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v < 0 then
            error("PosZFloat cannot be instantiated with a negative float literal")
          else
            v.asInstanceOf[PosZFloat]
        case None =>
          error("PosZFloat.apply requires a integer, long or float literal")
      }

    /** Compile-time factory for creating a [[PosZFloat]] from a integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative literals. Use it as: `PosZFloat(5)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton Int literal type
      * @param i the Int literal
      * @return a [[PosZFloat]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosZFloat =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v < 0 then
            error("PosZFloat cannot be instantiated with a negative integer literal")
          else
            v.asInstanceOf[PosZFloat]
        case None =>
          error("PosZFloat.apply requires an integer, long or float literal")
      }

    /** Compile-time factory for creating a [[PosZFloat]] from a long literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects negative literals. Use it as: `PosZFloat(5L)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton Long literal type
      * @param l the Long literal
      * @return a [[PosZFloat]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosZFloat =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v < 0L then
            error("PosZFloat cannot be instantiated with a negative long literal")
          else
            v.asInstanceOf[PosZFloat]
        case None =>
          error("PosZFloat.apply requires an integer, long or float literal")
      }    

    /** 
      * Return true when the provided Float is a valid [[PosZFloat]] value (>= 0). 
      *
      * @param value the Float to validate
      * @return true if the specified Float is a non-negative float, else false
      */
    def isValid(value: Float): Boolean = value >= 0.0f  
    
    /** Construct a [[PosZFloat]] from a runtime Float if it is non-negative.
      *
      * @param f runtime Float to validate
      * @return Some(PosZFloat) if f >= 0, otherwise None
      */
    def from(f: Float): Option[PosZFloat] =
      if (isValid(f)) Some(f) else None

    /** Ensure the runtime Float is non-negative and return it as a [[PosZFloat]].
      *
      * @param f runtime Float to check
      * @return the given float as a [[PosZFloat]] if valid
      * @throws AssertionError if the given Float is negative
      */
    def ensuringValid(f: Float): PosZFloat = 
      if (isValid(f)) 
        f
      else   
        throw new AssertionError(Resources.invalidPosZFloat)

    /**
     * A factory/validation method that produces a <code>PosZFloat</code>, wrapped
     * in a <code>Success</code>, given a valid <code>Float</code> value, or if the
     * given <code>Float</code> is invalid, an <code>AssertionError</code>, wrapped
     * in a <code>Failure</code>.
     *
     * <p>
     * This method will inspect the passed <code>Float</code> value and if
     * it is a PosZFloat <code>Float</code>, it will return a <code>PosZFloat</code>
     * representing that value, wrapped in a <code>Success</code>.
     * Otherwise, if the passed <code>Float</code> value is not PosZFloat, this
     * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
     * </p>
     *
     * <p>
     * This factory method differs from the <code>apply</code> factory method
     * in that <code>apply</code> is implemented via a macro that inspects
     * <code>Float</code> literals at compile time, whereas this method inspects
     * <code>Float</code> values at run time.
     * </p>
     *
     * @param value the <code>Float</code> to inspect, and if a non-negative float, return
     *     wrapped in a <code>Success(PosZFloat)</code>.
     * @return the specified <code>Float</code> value wrapped
     *     in a <code>Success(PosZFloat)</code>, if it is a non-negative float, else a <code>Failure(AssertionError)</code>.
     */
    def tryingValid(value: Float): Try[PosZFloat] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZFloat))

    /**
    * A validation method that produces a <code>Pass</code>
    * given a valid <code>Float</code> value, or
    * an error value of type <code>E</code> produced by passing the
    * given <em>invalid</em> <code>Float</code> value
    * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
    *
    * <p>
    * This method will inspect the passed <code>Float</code> value and if
    * it is a non-negative float <code>Float</code>, it will return a <code>Pass</code>.
    * Otherwise, the passed <code>Float</code> value is not a non-negative float, so this
    * method will return a result of type <code>E</code> obtained by passing
    * the invalid <code>Float</code> value to the given function <code>f</code>,
    * wrapped in a `Fail`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Float</code> literals at compile time, whereas this method inspects
    * <code>Float</code> values at run time.
    * </p>
    *
    * @tparam E error type produced by f
    * @param value the `Float` to validate that it is a non-negative float.
    * @param f function to produce an error when value is invalid
    * @return a `Pass` if the specified `Float` value is a non-negative float,
    *   else a `Fail` containing an error value produced by passing the
    *   specified `Float` to the given function `f`.
    */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))    

    /**
    * A factory/validation method that produces a <code>PosZFloat</code>, wrapped
    * in a <code>Good</code>, given a valid <code>Float</code> value, or if the
    * given <code>Float</code> is invalid, an error value of type <code>B</code>
    * produced by passing the given <em>invalid</em> <code>Float</code> value
    * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
    *
    * <p>
    * This method will inspect the passed <code>Float</code> value and if
    * it is a PosZFloat <code>Float</code>, it will return a <code>PosZFloat</code>
    * representing that value, wrapped in a <code>Good</code>.
    * Otherwise, the passed <code>Float</code> value is not PosZFloat, so this
    * method will return a result of type <code>B</code> obtained by passing
    * the invalid <code>Float</code> value to the given function <code>f</code>,
    * wrapped in a `Bad`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Float</code> literals at compile time, whereas this method inspects
    * <code>Float</code> values at run time.
    * </p>
    *
    * @tparam B error type produced by f
    * @param value the <code>Float</code> to inspect, and if PosZFloat, return
    *     wrapped in a <code>Good(PosZFloat)</code>.
    * @param f function to produce an error when value is invalid
    * @return the specified <code>Float</code> value wrapped
    *     in a <code>Good(PosZFloat)</code>, if it is PosZFloat, else a <code>Bad(f(value))</code>.
    */
    def goodOrElse[B](value: Float)(f: Float => B): PosZFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))  

    /**
      * A factory/validation method that produces a <code>PosZFloat</code>, wrapped
      * in a <code>Right</code>, given a valid <code>Float</code> value, or if the
      * given <code>Float</code> is invalid, an error value of type <code>L</code>
      * produced by passing the given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Left</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a PosZFloat <code>Float</code>, it will return a <code>PosZFloat</code>
      * representing that value, wrapped in a <code>Right</code>.
      * Otherwise, the passed <code>Float</code> value is not PosZFloat, so this
      * method will return a result of type <code>L</code> obtained by passing
      * the invalid <code>Float</code> value to the given function <code>f</code>,
      * wrapped in a `Left`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam L error type produced by f
      * @param value the <code>Float</code> to inspect, and if PosZFloat, return
      *     wrapped in a <code>Right(PosZFloat)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Float</code> value wrapped
      *     in a <code>Right(PosZFloat)</code>, if it is PosZFloat, else a <code>Left(f(value))</code>.
      */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, PosZFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /**
    * A factory method that produces a <code>PosZFloat</code> given a
    * <code>Float</code> value and a default <code>PosZFloat</code>.
    *
    * <p>
    * This method will inspect the passed <code>Float</code> value and if
    * it is a non-negative <code>Float</code>, <em>i.e.</em>, a value greater
    * than 0.0, it will return a <code>PosZFloat</code> representing that value.
    * Otherwise, the passed <code>Float</code> value is 0 or negative, so this
    * method will return the passed <code>default</code> value.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code>
    * factory method in that <code>apply</code> is implemented
    * via a macro that inspects <code>Float</code> literals at
    * compile time, whereas <code>from</code> inspects
    * <code>Float</code> values at run time.
    * </p>
    *
    * @param value the <code>Float</code> to inspect, and if positive, return.
    * @param default the <code>PosZFloat</code> to return if the passed
    *     <code>Float</code> value is not positive.
    * @return the specified <code>Float</code> value wrapped in a
    *     <code>PosZFloat</code>, if it is positive, else the
    *     <code>default</code> <code>PosZFloat</code> value.
    */
    def fromOrElse(value: Float, default: => PosZFloat): PosZFloat =
      if (isValid(value)) value else default

    /**
      * The largest value representable as a non-negative <code>Float</code>, which is <code>PosZFloat(Float.MaxValue)</code>.
      */
    val MaxValue: PosZFloat = Float.MaxValue

    /**
      * The smallest value representable as a non-negative <code>Float</code>, which is <code>PosZFloat(0.0f)</code>.
      */
    val MinValue: PosZFloat = 0.0f

    /**
      * Positive infinity as a <code>PosZFloat</code>, which is
      * <code>PosZFloat(Float.PositiveInfinity)</code>.
      */
    val PositiveInfinity: PosZFloat = Float.PositiveInfinity

    /**
      * The smallest positive value greater than 0.0f representable
      * as a <code>PosZFloat</code>, which is <code>PosZFloat(Float.MinPositiveValue)</code>.
      */
    val MinPositiveValue: PosZFloat = Float.MinPositiveValue

    /** Ordering instance for PosZFloat that orders by numeric value. */
    given Ordering[PosZFloat] with {
      def compare(x: PosZFloat, y: PosZFloat): Int = x.compareTo(y)
    }

    extension (p: PosZFloat) {
      /** Return the underlying Float value. */
      def value: Float = p
      /** Return true if this PosZFloat is positive infinity. */
      def isPosInfinity: Boolean = p == Float.PositiveInfinity
      /**
        * Returns the <code>PosZFloat</code> sum of this value and `x`.
        *
        * <p>
        * This method will always succeed (not throw an exception) because
        * adding a non-negative Float to another non-negative Float
        * will always result in another non-negative Float
        * value (though the result may be infinity).
        * </p>
        */
      def plus(x: PosZFloat): PosZFloat = PosZFloat.ensuringValid(value + x)
      /**
        * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
        */
      def max(that: PosZFloat): PosZFloat = math.max(p, that)
      /**
        * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
        */
      def min(that: PosZFloat): PosZFloat = math.min(p, that)
      /**
        * Indicates whether this `PosZFloat` has a value that is a whole number: it is finite and it has no fraction part.
        */
      def isWhole = {
        val longValue = p.toLong
        longValue.toFloat == p || longValue == Long.MaxValue && p < Float.PositiveInfinity || longValue == Long.MinValue && p > Float.NegativeInfinity
      }
      /**
        * Rounds this `PosZFloat` value to the nearest whole number value that can be expressed as an `PosZInt`, returning the result as a `PosZInt`.
        */
      def round: PosZInt = PosZInt.ensuringValid(math.round(value))

      /**
        * Returns the smallest (closest to 0) `PosZFloat` that is greater than or equal to this `PosZFloat`
        * and represents a mathematical integer.
        */
      def ceil: PosZFloat = PosZFloat.ensuringValid(math.ceil(value).toFloat)

      /**
        * Returns the greatest (closest to infinity) `PosZFloat` that is less than or equal to
        * this `PosZFloat` and represents a mathematical integer.
        */
      def floor: PosZFloat = PosZFloat.ensuringValid(math.floor(value).toFloat)

      /** Converts an angle measured in degrees to an approximately equivalent
        * angle measured in radians.
        *
        * @return the measurement of the angle x in radians.
        */
      def toRadians: Float = math.toRadians(value.toDouble).toFloat

      /** Converts an angle measured in radians to an approximately equivalent
        * angle measured in degrees.
        * @return the measurement of the angle x in degrees.
        */
      def toDegrees: Float = math.toDegrees(value.toDouble).toFloat

      /**
        * Applies the passed <code>Float =&gt; Float</code> function to the underlying <code>Float</code>
        * value, and if the result is positive, returns the result wrapped in a <code>PosZFloat</code>,
        * else throws <code>AssertionError</code>.
        *
        * <p>
        * This method will inspect the result of applying the given function to this
        * <code>PosZFloat</code>'s underlying <code>Float</code> value and if the result
        * is non-negative, it will return a <code>PosZFloat</code> representing that value.
        * Otherwise, the <code>Float</code> value returned by the given function is
        * not non-negative, so this method will throw <code>AssertionError</code>.
        * </p>
        *
        * <p>
        * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
        * call in that you get something you didn't already have if the assertion
        * succeeds: a <em>type</em> that promises an <code>Float</code> is non-negative.
        * With this method, you are asserting that you are convinced the result of
        * the computation represented by applying the given function to this <code>PosZFloat</code>'s
        * value will not produce invalid value.
        * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
        * </p>
        *
        * @param f the <code>Float =&gt; Float</code> function to apply to this <code>PosZFloat</code>'s
        *     underlying <code>Float</code> value.
        * @return the result of applying this <code>PosZFloat</code>'s underlying <code>Float</code> value to
        *     to the passed function, wrapped in a <code>PosZFloat</code> if it is non-negative (else throws <code>AssertionError</code>).
        * @throws AssertionError if the result of applying this <code>PosZFloat</code>'s underlying <code>Float</code> value to
        *     to the passed function is not non-negative.
        */
      def ensuringValid(f: Float => Float): PosZFloat = {
        val candidateResult: Float = f(value)
        if (PosZFloat.isValid(candidateResult)) PosZFloat.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${value.toString()}, was not a valid PosZFloat")
      }

      /**
        * True if this <code>PosZFloat</code> value is any finite value (i.e., it is neither positive nor negative infinity), else false.
        */
      def isFinite: Boolean = !value.isInfinite
    }  

  }

  opaque type PosFloat <: PosZFloat = Float

  /** Lower-priority given conversions for PosFloat.
    *
    * These conversions are provided at low priority to avoid 
    * conflict resolution in the presence of other numeric conversions.
    */
  trait PosFloatConversionsLowPriority extends PosZFloatConversionsLowPriority {
    /** Convert a [[PosFloat]] to a plain Double (unwrap). */
    given Conversion[PosFloat, Double] with {
      def apply(x: PosFloat): Double = x.toDouble
    }
    /** Convert a [[PosFloat]] to a plain PosDouble (unwrap). */
    given Conversion[PosFloat, PosDouble] with {
      def apply(x: PosFloat): PosDouble = PosDouble.ensuringValid(x.toDouble)
    }
    /** Convert a [[PosFloat]] to a plain PosZDouble (unwrap). */
    given Conversion[PosFloat, PosZDouble] with {
      def apply(x: PosFloat): PosZDouble = PosZDouble.ensuringValid(x.toDouble)
    }
    /** Convert a [[PosFloat]] to a plain NonZeroFloat (unwrap). */
    given Conversion[PosFloat, NonZeroFloat] with {
      def apply(x: PosFloat): NonZeroFloat = NonZeroFloat.ensuringValid(x.toFloat)
    }
    /** Convert a [[PosFloat]] to a plain NonZeroDouble (unwrap). */
    given Conversion[PosFloat, NonZeroDouble] with {
      def apply(x: PosFloat): NonZeroDouble = NonZeroDouble.ensuringValid(x.toDouble)
    }
  }

  object PosFloat extends PosFloatConversionsLowPriority {

    /** Convert a [[PosFloat]] to a plain Float (unwrap). */
    given Conversion[PosFloat, Float] with {
      def apply(x: PosFloat): Float = x.toFloat
    }

    /** Convert a compile-time Int literal or runtime Int to a [[PosFloat]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Int, PosFloat] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosFloat =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v <= 0 then
              error("PosFloat cannot be instantiated with a negative or zero integer literal")
            else
              v.toFloat.asInstanceOf[PosFloat]
          case None =>
            error("PosFloat conversion requires a integer literal")
        }
      def apply(x: Int): PosFloat = x.toFloat
    }

    /** Convert a compile-time Long literal or runtime Long to a [[PosFloat]].
      *
      * The inline overload checks long literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Long, PosFloat] with {
      inline def apply[L <: Long & Singleton](inline x: L): PosFloat =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v <= 0L then
              error("PosFloat cannot be instantiated with a negative or zero long literal")
            else
              v.toFloat.asInstanceOf[PosFloat]
          case None =>
            error("PosFloat conversion requires a long literal")
        }
      def apply(x: Long): PosFloat = x.toFloat
    }

    /** Convert a compile-time Float literal or runtime Float to a [[PosFloat]].
      *
      * The inline overload checks float literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Float, PosFloat] with {
      inline def apply[F <: Float & Singleton](inline x: F): PosFloat =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v <= 0.0f then
              error("PosFloat cannot be instantiated with a negative or zero float literal")
            else
              v.toFloat.asInstanceOf[PosFloat]
          case None =>
            error("PosFloat conversion requires a float literal")
        }
      def apply(x: Float): PosFloat = x
    }

    /** Compile-time factory for creating a [[PosFloat]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects non-positive literals. Use it as: `PosFloat(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the Float literal
      * @return a [[PosFloat]] representing the given positive literal
      * @throws a compile-time error if the literal is non-positive or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v <= 0.0f then
            error("PosFloat cannot be instantiated with a negative or zero float literal")
          else
            v.asInstanceOf[PosFloat]
        case None =>
          error("PosFloat.apply requires a integer, long or float literal")
      }

    /** Compile-time factory for creating a [[PosFloat]] from a integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects non-positive literals. Use it as: `PosFloat(5)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the float literal
      * @return a [[PosFloat]] representing the given positive literal
      * @throws a compile-time error if the literal is non-positive or not a literal
      */
    inline def apply[I <: Int & Singleton](inline f: I): PosFloat =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v <= 0 then
            error("PosFloat cannot be instantiated with a negative or zero integer literal")
          else
            v.toFloat.asInstanceOf[PosFloat]
        case None =>
          error("PosFloat.apply requires a integer, long or float literal")
      }

    /** Compile-time factory for creating a [[PosFloat]] from a long literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects non-positive literals. Use it as: `PosFloat(5L)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton Long literal type
      * @param l the long literal
      * @return a [[PosLong]] representing the given positive literal
      * @throws a compile-time error if the literal is non-positive or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosFloat =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v <= 0L then
            error("PosFloat cannot be instantiated with a negative or zero long literal")
          else
            v.toFloat.asInstanceOf[PosFloat]
        case None =>
          error("PosFloat.apply requires a integer, long or float literal")
      }    

    /** 
      * Return true when the provided Float is a valid [[PosFloat]] value (> 0). 
      *
      * @param value the Float to validate
      * @return true if the specified Float is a positive float, else false
      */
    def isValid(value: Float): Boolean = value > 0.0f  
    
    def from(f: Float): Option[PosFloat] =
      if (isValid(f)) Some(f) else None

    def ensuringValid(f: Float): PosFloat = 
      if (isValid(f))
        f
      else 
        throw new AssertionError(Resources.invalidPosFloat)

    /**
     * A factory/validation method that produces a <code>PosFloat</code>, wrapped
     * in a <code>Success</code>, given a valid <code>Float</code> value, or if the
     * given <code>Float</code> is invalid, an <code>AssertionError</code>, wrapped
     * in a <code>Failure</code>.
     *
     * <p>
     * This method will inspect the passed <code>Float</code> value and if
     * it is a PosFloat <code>Float</code>, it will return a <code>PosFloat</code>
     * representing that value, wrapped in a <code>Success</code>.
     * Otherwise, if the passed <code>Float</code> value is not PosFloat, this
     * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
     * </p>
     *
     * <p>
     * This factory method differs from the <code>apply</code> factory method
     * in that <code>apply</code> is implemented via a macro that inspects
     * <code>Float</code> literals at compile time, whereas this method inspects
     * <code>Float</code> values at run time.
     * </p>
     *
     * @param value the <code>Float</code> to inspect, and if a non-negative float, return
     *     wrapped in a <code>Success(PosFloat)</code>.
     * @return the specified <code>Float</code> value wrapped
     *     in a <code>Success(PosFloat)</code>, if it is a non-negative float, else a <code>Failure(AssertionError)</code>.
     */
    def tryingValid(value: Float): Try[PosFloat] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosFloat))  

    /**
      * A validation method that produces a <code>Pass</code>
      * given a valid <code>Float</code> value, or
      * an error value of type <code>E</code> produced by passing the
      * given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a non-negative float <code>Float</code>, it will return a <code>Pass</code>.
      * Otherwise, the passed <code>Float</code> value is not a non-negative float, so this
      * method will return a result of type <code>E</code> obtained by passing
      * the invalid <code>Float</code> value to the given function <code>f</code>,
      * wrapped in a `Fail`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam E error type produced by f
      * @param value the `Float` to validate that it is a non-negative float.
      * @param f function to produce an error when value is invalid
      * @return a `Pass` if the specified `Float` value is a non-negative float,
      *   else a `Fail` containing an error value produced by passing the
      *   specified `Float` to the given function `f`.
      */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /**
      * A factory/validation method that produces a <code>PosFloat</code>, wrapped
      * in a <code>Good</code>, given a valid <code>Float</code> value, or if the
      * given <code>Float</code> is invalid, an error value of type <code>B</code>
      * produced by passing the given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a PosFloat <code>Float</code>, it will return a <code>PosFloat</code>
      * representing that value, wrapped in a <code>Good</code>.
      * Otherwise, the passed <code>Float</code> value is not PosFloat, so this
      * method will return a result of type <code>B</code> obtained by passing
      * the invalid <code>Float</code> value to the given function <code>f</code>,
      * wrapped in a `Bad`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam B error type produced by f
      * @param value the <code>Float</code> to inspect, and if PosFloat, return
      *     wrapped in a <code>Good(PosFloat)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Float</code> value wrapped
      *     in a <code>Good(PosFloat)</code>, if it is PosFloat, else a <code>Bad(f(value))</code>.
      */
    def goodOrElse[B](value: Float)(f: Float => B): PosFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /**
      * A factory/validation method that produces a <code>PosFloat</code>, wrapped
      * in a <code>Right</code>, given a valid <code>Float</code> value, or if the
      * given <code>Float</code> is invalid, an error value of type <code>L</code>
      * produced by passing the given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Left</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a PosFloat <code>Float</code>, it will return a <code>PosFloat</code>
      * representing that value, wrapped in a <code>Right</code>.
      * Otherwise, the passed <code>Float</code> value is not PosFloat, so this
      * method will return a result of type <code>L</code> obtained by passing
      * the invalid <code>Float</code> value to the given function <code>f</code>,
      * wrapped in a `Left`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam L error type produced by f
      * @param value the <code>Float</code> to inspect, and if PosFloat, return
      *     wrapped in a <code>Right(PosFloat)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Float</code> value wrapped
      *     in a <code>Right(PosFloat)</code>, if it is PosFloat, else a <code>Left(f(value))</code>.
      */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, PosFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))        

    /**
      * A factory method that produces a <code>PosFloat</code> given a
      * <code>Float</code> value and a default <code>PosFloat</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a non-negative <code>Float</code>, <em>i.e.</em>, a value greater
      * than 0.0, it will return a <code>PosFloat</code> representing that value.
      * Otherwise, the passed <code>Float</code> value is 0 or negative, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code>
      * factory method in that <code>apply</code> is implemented
      * via a macro that inspects <code>Float</code> literals at
      * compile time, whereas <code>from</code> inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @param value the <code>Float</code> to inspect, and if positive, return.
      * @param default the <code>PosFloat</code> to return if the passed
      *     <code>Float</code> value is not positive.
      * @return the specified <code>Float</code> value wrapped in a
      *     <code>PosFloat</code>, if it is positive, else the
      *     <code>default</code> <code>PosFloat</code> value.
      */
    def fromOrElse(value: Float, default: => PosFloat): PosFloat =
      if (isValid(value)) value else default

    /**
      * The largest value representable as a positive <code>Float</code>, which is <code>PosFloat(Float.MaxValue)</code>.
      */
    val MaxValue: PosFloat = Float.MaxValue

    /**
      * The smallest value representable as a positive <code>Float</code>, which is <code>PosFloat(Float.MinPositiveValue)</code>.
      */
    val MinValue: PosFloat = Float.MinPositiveValue

    /**
      * Positive infinity as a <code>PosFloat</code>, which is
      * <code>PosFloat(Float.PositiveInfinity)</code>.
      */
    val PositiveInfinity: PosFloat = Float.PositiveInfinity

    extension (p: PosFloat) {
      /**
        * Applies the passed <code>Float =&gt; Float</code> function to the underlying <code>Float</code>
        * value, and if the result is positive, returns the result wrapped in a <code>PosFloat</code>,
        * else throws <code>AssertionError</code>.
        *
        * <p>
        * This method will inspect the result of applying the given function to this
        * <code>PosFloat</code>'s underlying <code>Float</code> value and if the result
        * is non-negative, it will return a <code>PosFloat</code> representing that value.
        * Otherwise, the <code>Float</code> value returned by the given function is
        * not non-negative, so this method will throw <code>AssertionError</code>.
        * </p>
        *
        * <p>
        * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
        * call in that you get something you didn't already have if the assertion
        * succeeds: a <em>type</em> that promises an <code>Float</code> is non-negative.
        * With this method, you are asserting that you are convinced the result of
        * the computation represented by applying the given function to this <code>PosFloat</code>'s
        * value will not produce invalid value.
        * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
        * </p>
        *
        * @param f the <code>Float =&gt; Float</code> function to apply to this <code>PosFloat</code>'s
        *     underlying <code>Float</code> value.
        * @return the result of applying this <code>PosFloat</code>'s underlying <code>Float</code> value to
        *     to the passed function, wrapped in a <code>PosFloat</code> if it is non-negative (else throws <code>AssertionError</code>).
        * @throws AssertionError if the result of applying this <code>PosFloat</code>'s underlying <code>Float</code> value to
        *     to the passed function is not non-negative.
        */
      def ensuringValid(f: Float => Float): PosFloat = {
        val candidateResult: Float = f(p)
        if (PosFloat.isValid(candidateResult)) PosFloat.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid PosFloat")
      }
    }

  }

  opaque type PosZFiniteFloat <: PosZFloat = Float

  object PosZFiniteFloat {
    
    /** Compile-time factory for creating a [[PosZFiniteFloat]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects negative literals. Use it as: `PosZFiniteFloat(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the Float literal
      * @return a [[PosZFiniteFloat]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosZFiniteFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v < 0.0f && v != Float.PositiveInfinity then
            error("PosZFiniteFloat cannot be instantiated with a negative float literal or positive infinity")
          else
            v.asInstanceOf[PosZFiniteFloat]
        case None =>
          error("PosZFiniteFloat.apply requires a integer, long or float literal")
      }

    /** Compile-time factory for creating a [[PosZFiniteFloat]] from a integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative literals. Use it as: `PosZFiniteFloat(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton Integer literal type
      * @param i the Integer literal
      * @return a [[PosZFiniteFloat]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosZFiniteFloat =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v < 0 then
            error("PosZFiniteFloat cannot be instantiated with a negative integer literal")
          else
            v.asInstanceOf[PosZFiniteFloat]
        case None =>
          error("PosZFiniteFloat.apply requires a integer, long or float literal")
      }

    /** Compile-time factory for creating a [[PosZFiniteFloat]] from a long literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects negative literals. Use it as: `PosZFiniteFloat(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton Long literal type
      * @param l the Long literal
      * @return a [[PosZFiniteFloat]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosZFiniteFloat =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v < 0L then
            error("PosZFiniteFloat cannot be instantiated with a negative long literal")
          else
            v.asInstanceOf[PosZFiniteFloat]
        case None =>
          error("PosZFiniteFloat.apply requires a integer, long or float literal")
      }  

    /** 
      * Return true when the provided Float is a valid [[PosZFiniteFloat]] value (>= 0 and != Float.PositiveInfinity). 
      *
      * @param value the Float to validate
      * @return true if the specified Float is a non-negative float, else false
      */
    def isValid(value: Float): Boolean = value >= 0.0f && value != Float.PositiveInfinity
    
    /** Construct a [[PosZFiniteFloat]] from a runtime Float if it is positive and finite.
      *
      * @param f runtime Float to validate
      * @return Some(PosZFiniteFloat) if f >= 0 and finite, otherwise None
      */
    def from(f: Float): Option[PosZFiniteFloat] =
      if (isValid(f)) Some(f) else None

    /** Ensure the runtime Float is positive and not positive infinity, return it as a [[PosZFiniteFloat]].
      *
      * @param f runtime Float to check
      * @return the given float as a [[PosZFiniteFloat]] if valid
      * @throws AssertionError if the given Float is negative
      */
    def ensuringValid(f: Float): PosZFiniteFloat = 
      if (isValid(f)) 
        f
      else   
        throw new AssertionError(Resources.invalidPosZFiniteFloat)

    /**
     * A factory/validation method that produces a <code>PosZFiniteFloat</code>, wrapped
     * in a <code>Success</code>, given a valid <code>Float</code> value, or if the
     * given <code>Float</code> is invalid, an <code>AssertionError</code>, wrapped
     * in a <code>Failure</code>.
     *
     * <p>
     * This method will inspect the passed <code>Float</code> value and if
     * it is a PosZFiniteFloat <code>Float</code>, it will return a <code>PosZFiniteFloat</code>
     * representing that value, wrapped in a <code>Success</code>.
     * Otherwise, if the passed <code>Float</code> value is not PosZFiniteFloat, this
     * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
     * </p>
     *
     * <p>
     * This factory method differs from the <code>apply</code> factory method
     * in that <code>apply</code> is implemented via a macro that inspects
     * <code>Float</code> literals at compile time, whereas this method inspects
     * <code>Float</code> values at run time.
     * </p>
     *
     * @param value the <code>Float</code> to inspect, and if a non-negative float, return
     *     wrapped in a <code>Success(PosZFiniteFloat)</code>.
     * @return the specified <code>Float</code> value wrapped
     *     in a <code>Success(PosZFiniteFloat)</code>, if it is a non-negative float, else a <code>Failure(AssertionError)</code>.
     */
    def tryingValid(value: Float): Try[PosZFiniteFloat] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZFiniteFloat))

    /**
    * A validation method that produces a <code>Pass</code>
    * given a valid <code>Float</code> value, or
    * an error value of type <code>E</code> produced by passing the
    * given <em>invalid</em> <code>Float</code> value
    * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
    *
    * <p>
    * This method will inspect the passed <code>Float</code> value and if
    * it is a positive finite float <code>Float</code>, it will return a <code>Pass</code>.
    * Otherwise, the passed <code>Float</code> value is not a positive finite float, so this
    * method will return a result of type <code>E</code> obtained by passing
    * the invalid <code>Float</code> value to the given function <code>f</code>,
    * wrapped in a `Fail`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Float</code> literals at compile time, whereas this method inspects
    * <code>Float</code> values at run time.
    * </p>
    *
    * @tparam E error type produced by f
    * @param value the `Float` to validate that it is a positive finite float.
    * @param f function to produce an error when value is invalid
    * @return a `Pass` if the specified `Float` value is a positive finite float,
    *   else a `Fail` containing an error value produced by passing the
    *   specified `Float` to the given function `f`.
    */
    def passOrElse[E](value: Float)(f: Float => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))
    /**
    * A factory/validation method that produces a <code>PosZFFiniteloat</code>, wrapped
    * in a <code>Good</code>, given a valid <code>Float</code> value, or if the
    * given <code>Float</code> is invalid, an error value of type <code>B</code>
    * produced by passing the given <em>invalid</em> <code>Float</code> value
    * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
    *
    * <p>
    * This method will inspect the passed <code>Float</code> value and if
    * it is a PosZFiniteFloat <code>Float</code>, it will return a <code>PosZFiniteFloat</code>
    * representing that value, wrapped in a <code>Good</code>.
    * Otherwise, the passed <code>Float</code> value is not PosZFiniteFloat, so this
    * method will return a result of type <code>B</code> obtained by passing
    * the invalid <code>Float</code> value to the given function <code>f</code>,
    * wrapped in a `Bad`.
    * </p>
    *
    * <p>
    * This factory method differs from the <code>apply</code> factory method
    * in that <code>apply</code> is implemented via a macro that inspects
    * <code>Float</code> literals at compile time, whereas this method inspects
    * <code>Float</code> values at run time.
    * </p>
    *
    * @tparam B error type produced by f
    * @param value the <code>Float</code> to inspect, and if PosZFiniteFloat, return
    *     wrapped in a <code>Good(PosZFiniteFloat)</code>.
    * @param f function to produce an error when value is invalid
    * @return the specified <code>Float</code> value wrapped
    *     in a <code>Good(PosZFiniteFloat)</code>, if it is PosZFiniteFloat, else a <code>Bad(f(value))</code>.
    */
    def goodOrElse[B](value: Float)(f: Float => B): PosZFiniteFloat Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /**
      * A factory/validation method that produces a <code>PosZFiniteFloat</code>, wrapped
      * in a <code>Right</code>, given a valid <code>Float</code> value, or if the
      * given <code>Float</code> is invalid, an error value of type <code>L</code>
      * produced by passing the given <em>invalid</em> <code>Float</code> value
      * to the given function <code>f</code>, wrapped in a <code>Left</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a PosZFiniteFloat <code>Float</code>, it will return a <code>PosZFiniteFloat</code>
      * representing that value, wrapped in a <code>Right</code>.
      * Otherwise, the passed <code>Float</code> value is not PosZFiniteFloat, so this
      * method will return a result of type <code>L</code> obtained by passing
      * the invalid <code>Float</code> value to the given function <code>f</code>,
      * wrapped in a `Left`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @tparam L error type produced by f
      * @param value the <code>Float</code> to inspect, and if PosZFiniteFloat, return
      *     wrapped in a <code>Right(PosZFiniteFloat)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Float</code> value wrapped
      *     in a <code>Right(PosZFiniteFloat)</code>, if it is PosZFiniteFloat, else a <code>Left(f(value))</code>.
      */
    def rightOrElse[L](value: Float)(f: Float => L): Either[L, PosZFiniteFloat] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /**
      * A factory method that produces a <code>PosZFiniteFloat</code> given a
      * <code>Float</code> value and a default <code>PosZFiniteFloat</code>.
      *
      * <p>
      * This method will inspect the passed <code>Float</code> value and if
      * it is a non-negative <code>Float</code>, <em>i.e.</em>, a value greater
      * than 0.0, it will return a <code>PosZFiniteFloat</code> representing that value.
      * Otherwise, the passed <code>Float</code> value is 0 or negative, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code>
      * factory method in that <code>apply</code> is implemented
      * via a macro that inspects <code>Float</code> literals at
      * compile time, whereas <code>from</code> inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @param value the <code>Float</code> to inspect, and if positive and finite, return.
      * @param default the <code>PosZFiniteFloat</code> to return if the passed
      *     <code>Float</code> value is not positive.
      * @return the specified <code>Float</code> value wrapped in a
      *     <code>PosZFiniteFloat</code>, if it is positive, else the
      *     <code>default</code> <code>PosZFiniteFloat</code> value.
      */
    def fromOrElse(value: Float, default: => PosZFiniteFloat): PosZFiniteFloat =
      if (isValid(value)) value else default

    /**
      * The largest value representable as a positive and finite <code>Float</code>, which is <code>PosZFiniteFloat(Float.MaxValue)</code>.
      */
    val MaxValue: PosZFiniteFloat = Float.MaxValue

    /**
      * The smallest value representable as a positive and finite <code>Float</code>, which is <code>PosZFiniteFloat(0.0f)</code>.
      */
    val MinValue: PosZFiniteFloat = Float.MinPositiveValue              
  }

}
