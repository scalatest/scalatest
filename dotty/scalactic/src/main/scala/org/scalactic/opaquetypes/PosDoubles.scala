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
import NonZeroDoubles.NonZeroDouble

object PosDoubles {

  opaque type PosZDouble = Double

  object PosZDouble {

    /** Convert a [[PosZDouble]] to a plain Double (unwrap). */
    given Conversion[PosZDouble, Double] with {
      def apply(x: PosZDouble): Double = x.toDouble
    }

    /** Convert a compile-time Int literal or runtime Int to a [[PosZDouble]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Int, PosZDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosZDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v < 0 then
              error("PosZDouble cannot be instantiated with a negative integer literal")
            else
              v.toDouble.asInstanceOf[PosZDouble]
          case None =>
            error("PosZDouble conversion requires a integer literal")
        }
      def apply(x: Int): PosZDouble = x.toDouble
    }

    /** Convert a compile-time Long literal or runtime Long to a [[PosZDouble]].
      *
      * The inline overload checks long literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Long, PosZDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): PosZDouble =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v < 0L then
              error("PosZDouble cannot be instantiated with a negative long literal")
            else
              v.toDouble.asInstanceOf[PosZDouble]
          case None =>
            error("PosZDouble conversion requires a long literal")
        }
      def apply(x: Long): PosZDouble = x.toDouble
    }

    /** Convert a compile-time Float literal or runtime Float to a [[PosZDouble]].
      *
      * The inline overload checks float literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Float, PosZDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): PosZDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v < 0.0f then
              error("PosZDouble cannot be instantiated with a negative float literal")
            else
              v.toDouble.asInstanceOf[PosZDouble]
          case None =>
            error("PosZDouble conversion requires a float literal")
        }
      def apply(x: Float): PosZDouble = x.toDouble
    }

    /** Convert a compile-time Double literal or runtime Double to a [[PosZDouble]].
      *
      * The inline overload checks double literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Double, PosZDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): PosZDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v < 0.0 then
              error("PosZDouble cannot be instantiated with a negative double literal")
            else
              v.toDouble.asInstanceOf[PosZDouble]
          case None =>
            error("PosZDouble conversion requires a double literal")
        }
      def apply(x: Double): PosZDouble = x
    }

    /** Compile-time factory for creating a [[PosZDouble]] from a double literal.
      *
      * This inline method inspects the provided double literal at compile time
      * and rejects negative literals. Use it as: `PosZDouble(5.0)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton Double literal type
      * @param d the Double literal
      * @return a [[PosZDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): PosZDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v < 0.0 then
            error("PosZDouble cannot be instantiated with a negative double literal")
          else
            v.asInstanceOf[PosZDouble]
        case None =>
          error("PosZDouble.apply requires a integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosZDouble]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects negative literals. Use it as: `PosZDouble(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the Float literal
      * @return a [[PosZDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosZDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v < 0.0f then
            error("PosZDouble cannot be instantiated with a negative float literal")
          else
            v.toDouble.asInstanceOf[PosZDouble]
        case None =>
          error("PosZDouble.apply requires a integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosZDouble]] from a long literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects negative literals. Use it as: `PosZDouble(5L)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton Long literal type
      * @param l the Long literal
      * @return a [[PosZDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosZDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v < 0L then
            error("PosZDouble cannot be instantiated with a negative long literal")
          else
            v.toDouble.asInstanceOf[PosZDouble]
        case None =>
          error("PosZDouble.apply requires a integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosZDouble]] from a integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative literals. Use it as: `PosZDouble(5)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton Int literal type
      * @param i the Int literal
      * @return a [[PosZDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosZDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v < 0 then
            error("PosZDouble cannot be instantiated with a negative integer literal")
          else
            v.toDouble.asInstanceOf[PosZDouble]
        case None =>
          error("PosZDouble.apply requires a integer, long, float or double literal")
      }      

    /** 
      * Return true when the provided Double is a valid [[PosZDouble]] value (>= 0). 
      *
      * @param value the Double to validate
      * @return true if the specified Double is a non-negative double, else false
      */
    def isValid(value: Double): Boolean = value >= 0.0  

    /**
      * A factory/validation method that produces a <code>PosZDouble</code>, wrapped
      * in a <code>Success</code>, given a valid <code>Double</code> value, or if the
      * given <code>Double</code> is invalid, an <code>AssertionError</code>, wrapped
      * in a <code>Failure</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a PosZDouble <code>Double</code>, it will return a <code>PosZDouble</code>
      * representing that value, wrapped in a <code>Success</code>.
      * Otherwise, if the passed <code>Double</code> value is not PosZDouble, this
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
      * @param value the <code>Double</code> to inspect, and if a non-negative double, return
      *     wrapped in a <code>Success(PosZDouble)</code>.
      * @return the specified <code>Double</code> value wrapped
      *     in a <code>Success(PosZDouble)</code>, if it is a non-negative double, else a <code>Failure(AssertionError)</code>.
      */
    def tryingValid(value: Double): Try[PosZDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZDouble))

    /**
      * A validation method that produces a <code>Pass</code>
      * given a valid <code>Double</code> value, or
      * an error value of type <code>E</code> produced by passing the
      * given <em>invalid</em> <code>Double</code> value
      * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a non-negative double <code>Double</code>, it will return a <code>Pass</code>.
      * Otherwise, the passed <code>Double</code> value is not a non-negative double, so this
      * method will return a result of type <code>E</code> obtained by passing
      * the invalid <code>Double</code> value to the given function <code>f</code>,
      * wrapped in a `Fail`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Double</code> literals at compile time, whereas this method inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @tparam E error type produced by f
      * @param value the `Double` to validate that it is a non-negative double.
      * @param f function to produce an error when value is invalid
      * @return a `Pass` if the specified `Double` value is a non-negative double,
      *   else a `Fail` containing an error value produced by passing the
      *   specified `Double` to the given function `f`.
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /**
      * A factory/validation method that produces a <code>PosZDouble</code>, wrapped
      * in a <code>Good</code>, given a valid <code>Double</code> value, or if the
      * given <code>Double</code> is invalid, an error value of type <code>B</code>
      * produced by passing the given <em>invalid</em> <code>Double</code> value
      * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a PosZDouble <code>Double</code>, it will return a <code>PosZDouble</code>
      * representing that value, wrapped in a <code>Good</code>.
      * Otherwise, the passed <code>Double</code> value is not PosZDouble, so this
      * method will return a result of type <code>B</code> obtained by passing
      * the invalid <code>Double</code> value to the given function <code>f</code>,
      * wrapped in a `Bad`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Double</code> literals at compile time, whereas this method inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @tparam B error type produced by f
      * @param value the <code>Double</code> to inspect, and if PosZDouble, return
      *     wrapped in a <code>Good(PosZDouble)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Double</code> value wrapped
      *     in a <code>Good(PosZDouble)</code>, if it is PosZDouble, else a <code>Bad(f(value))</code>.
      */
    def goodOrElse[B](value: Double)(f: Double => B): PosZDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /**
      * A factory/validation method that produces a <code>PosZDouble</code>, wrapped
      * in a <code>Right</code>, given a valid <code>Double</code> value, or if the
      * given <code>Double</code> is invalid, an error value of type <code>L</code>
      * produced by passing the given <em>invalid</em> <code>Double</code> value
      * to the given function <code>f</code>, wrapped in a <code>Left</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a PosZDouble <code>Double</code>, it will return a <code>PosZDouble</code>
      * representing that value, wrapped in a <code>Right</code>.
      * Otherwise, the passed <code>Double</code> value is not PosZDouble, so this
      * method will return a result of type <code>L</code> obtained by passing
      * the invalid <code>Double</code> value to the given function <code>f</code>,
      * wrapped in a `Left`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Double</code> literals at compile time, whereas this method inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @tparam L error type produced by f
      * @param value the <code>Double</code> to inspect, and if PosZDouble, return
      *     wrapped in a <code>Right(PosZDouble)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Double</code> value wrapped
      *     in a <code>Right(PosZDouble)</code>, if it is PosZDouble, else a <code>Left(f(value))</code>.
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, PosZDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /**
      * A factory method that produces a <code>PosZDouble</code> given a
      * <code>Double</code> value and a default <code>PosZDouble</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a non-negative <code>Double</code>, <em>i.e.</em>, a value greater
      * than 0.0, it will return a <code>PosZDouble</code> representing that value.
      * Otherwise, the passed <code>Double</code> value is 0 or negative, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code>
      * factory method in that <code>apply</code> is implemented
      * via a macro that inspects <code>Double</code> literals at
      * compile time, whereas <code>from</code> inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @param value the <code>Double</code> to inspect, and if positive, return.
      * @param default the <code>PosZDouble</code> to return if the passed
      *     <code>Double</code> value is not positive.
      * @return the specified <code>Double</code> value wrapped in a
      *     <code>PosZDouble</code>, if it is positive, else the
      *     <code>default</code> <code>PosZDouble</code> value.
      */
    def fromOrElse(value: Double, default: => PosZDouble): PosZDouble =
      if (isValid(value)) value else default          
    
    def from(d: Double): Option[PosZDouble] =
      if (d >= 0.0) Some(d) else None

    def ensuringValid(d: Double): PosZDouble = 
      if (d < 0.0) 
        throw new AssertionError(Resources.invalidPosZDouble)
      else d

    /**
      * The largest value representable as a positive <code>Double</code>, which is <code>PosDouble(Double.MaxValue)</code>.
      */
    val MaxValue: PosZDouble = Double.MaxValue

    /**
      * The smallest value representable as a positive <code>Double</code>, which is <code>PosDouble(Double.MinPositiveValue)</code>.
      */
    val MinValue: PosZDouble = 0.0

    /**
      * Positive infinity as a <code>PosDouble</code>, which is
      * <code>PosDouble(Double.PositiveInfinity)</code>.
      */
    val PositiveInfinity: PosZDouble = Double.PositiveInfinity  

    /**
      * The smallest positive value greater than 0.0f representable
      * as a <code>PosZDouble</code>, which is <code>PosZDouble(Double.MinPositiveValue)</code>.
      */
    val MinPositiveValue: PosZDouble = Double.MinPositiveValue

    /** Ordering instance for PosZDouble that orders by numeric value. */
    given Ordering[PosZDouble] with {
      def compare(x: PosZDouble, y: PosZDouble): Int = x.compareTo(y)
    }

    extension (p: PosZDouble) {
      /** Return the underlying Double value. */
      def value: Double = p
      /** Return true if this PosZDouble is positive infinity. */
      def isPosInfinity: Boolean = p == Double.PositiveInfinity
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
      def plus(x: PosZDouble): PosZDouble = PosZDouble.ensuringValid(value + x)
      /**
        * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
        */
      def max(that: PosZDouble): PosZDouble = math.max(p, that)
      /**
        * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
        */
      def min(that: PosZDouble): PosZDouble = math.min(p, that)
      /**
        * Indicates whether this `PosZFloat` has a value that is a whole number: it is finite and it has no fraction part.
        */
      def isWhole = {
        val longValue = p.toLong
        longValue.toDouble == p || longValue == Long.MaxValue && p < Double.PositiveInfinity || longValue == Long.MinValue && p > Double.NegativeInfinity
      }
      /**
        * Rounds this `PosZDouble` value to the nearest whole number value that can be expressed as an `PosZInt`, returning the result as a `PosZInt`.
        */
      def round: PosZLong = PosZLong.ensuringValid(math.round(value))
      /**
        * Returns the smallest (closest to 0) `PosZFloat` that is greater than or equal to this `PosZFloat`
        * and represents a mathematical integer.
        */
      def ceil: PosZDouble = PosZDouble.ensuringValid(math.ceil(value))
      /**
        * Returns the greatest (closest to infinity) `PosZFloat` that is less than or equal to
        * this `PosZFloat` and represents a mathematical integer.
        */
      def floor: PosZDouble = PosZDouble.ensuringValid(math.floor(value))
      /** Converts an angle measured in degrees to an approximately equivalent
        * angle measured in radians.
        *
        * @return the measurement of the angle x in radians.
        */
      def toRadians: Double = math.toRadians(value.toDouble)
      /** Converts an angle measured in radians to an approximately equivalent
        * angle measured in degrees.
        * @return the measurement of the angle x in degrees.
        */
      def toDegrees: Double = math.toDegrees(value.toDouble)
      /**
        * Applies the passed <code>Double =&gt; Double</code> function to the underlying <code>Double</code>
        * value, and if the result is positive, returns the result wrapped in a <code>PosZDouble</code>,
        * else throws <code>AssertionError</code>.
        *
        * <p>
        * This method will inspect the result of applying the given function to this
        * <code>PosZDouble</code>'s underlying <code>Double</code> value and if the result
        * is non-negative, it will return a <code>PosZDouble</code> representing that value.
        * Otherwise, the <code>Double</code> value returned by the given function is
        * not non-negative, so this method will throw <code>AssertionError</code>.
        * </p>
        *
        * <p>
        * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
        * call in that you get something you didn't already have if the assertion
        * succeeds: a <em>type</em> that promises an <code>Double</code> is non-negative.
        * With this method, you are asserting that you are convinced the result of
        * the computation represented by applying the given function to this <code>PosZDouble</code>'s
        * value will not produce invalid value.
        * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
        * </p>
        *
        * @param f the <code>Double =&gt; Double</code> function to apply to this <code>PosZDouble</code>'s
        *     underlying <code>Double</code> value.
        * @return the result of applying this <code>PosZDouble</code>'s underlying <code>Double</code> value to
        *     to the passed function, wrapped in a <code>PosZDouble</code> if it is non-negative (else throws <code>AssertionError</code>).
        * @throws AssertionError if the result of applying this <code>PosZDouble</code>'s underlying <code>Double</code> value to
        *     to the passed function is not non-negative.
        */
      def ensuringValid(f: Double => Double): PosZDouble = {
        val candidateResult: Double = f(value)
        if (PosZDouble.isValid(candidateResult)) PosZDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${value.toString()}, was not a valid PosZDouble")
      }
      /**
        * True if this <code>PosZDouble</code> value is any finite value (i.e., it is neither positive nor negative infinity), else false.
        */
      def isFinite: Boolean = !value.isInfinite
    }
  }

  opaque type PosDouble <: PosZDouble = Double

  object PosDouble {

    /** Convert a [[PosDouble]] to a plain Double (unwrap). */
    given Conversion[PosDouble, Double] with {
      def apply(x: PosDouble): Double = x.toDouble
    }

    /** Convert a [[PosDouble]] to a plain NonZeroDouble (unwrap). */
    given Conversion[PosDouble, NonZeroDouble] with {
      def apply(x: PosDouble): NonZeroDouble = NonZeroDouble.ensuringValid(x.toDouble)
    }

    /** Convert a compile-time Int literal or runtime Int to a [[PosDouble]].
      *
      * The inline overload checks integer literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Int, PosDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v <= 0 then
              error("PosZDouble cannot be instantiated with a zero or negative integer literal")
            else
              v.toDouble.asInstanceOf[PosDouble]
          case None =>
            error("PosDouble conversion requires a integer literal")
        }
      def apply(x: Int): PosDouble = x.toDouble
    }

    /** Convert a compile-time Long literal or runtime Long to a [[PosDouble]].
      *
      * The inline overload checks long literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Long, PosDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): PosDouble =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v <= 0L then
              error("PosZDouble cannot be instantiated with a zero or negative long literal")
            else
              v.toDouble.asInstanceOf[PosDouble]
          case None =>
            error("PosDouble conversion requires a long literal")
        }
      def apply(x: Long): PosDouble = x.toDouble
    }

    /** Convert a compile-time Float literal or runtime Float to a [[PosDouble]].
      *
      * The inline overload checks float literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Float, PosDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): PosDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v <= 0.0f then
              error("PosZDouble cannot be instantiated with a zero or negative float literal")
            else
              v.toDouble.asInstanceOf[PosDouble]
          case None =>
            error("PosDouble conversion requires a float literal")
        }
      def apply(x: Float): PosDouble = x.toDouble
    }

    /** Convert a compile-time Double literal or runtime Double to a [[PosDouble]].
      *
      * The inline overload checks double literals at compile time; the runtime
      * overload validates and throws for negative values.
      */
    given Conversion[Double, PosDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): PosDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v <= 0.0 then
              error("PosZDouble cannot be instantiated with a zero or negative double literal")
            else
              v.asInstanceOf[PosDouble]
          case None =>
            error("PosDouble conversion requires a double literal")
        }
      def apply(x: Double): PosDouble = x.toDouble
    }

    /** Compile-time factory for creating a [[PosDouble]] from a double literal.
      *
      * This inline method inspects the provided double literal at compile time
      * and rejects negative literals. Use it as: `PosDouble(5.0)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton Double literal type
      * @param d the Double literal
      * @return a [[PosDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): PosDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v <= 0.0 then
            error("PosDouble cannot be instantiated with a non-positive double literal")
          else
            v.asInstanceOf[PosDouble]
        case None =>
          error("PosDouble.apply requires a integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosDouble]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects negative literals. Use it as: `PosDouble(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the Float literal
      * @return a [[PosDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v <= 0.0f then
            error("PosDouble cannot be instantiated with a non-positive float literal")
          else
            v.toDouble.asInstanceOf[PosDouble]
        case None =>
          error("PosDouble.apply requires a integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosDouble]] from a long literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects negative literals. Use it as: `PosDouble(5L)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton Long literal type
      * @param l the Long literal
      * @return a [[PosDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v <= 0L then
            error("PosDouble cannot be instantiated with a non-positive long literal")
          else
            v.toDouble.asInstanceOf[PosDouble]
        case None =>
          error("PosDouble.apply requires a integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosDouble]] from a integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative literals. Use it as: `PosDouble(5)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton Integer literal type
      * @param i the Integer literal
      * @return a [[PosDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v <= 0 then
            error("PosDouble cannot be instantiated with a non-positive integer literal")
          else
            v.toDouble.asInstanceOf[PosDouble]
        case None =>
          error("PosDouble.apply requires a integer, long, float or double literal")
      }      
    
    def from(d: Double): Option[PosDouble] =
      if (isValid(d)) Some(d) else None

    /** 
      * Return true when the provided Double is a valid [[PosDouble]] value (> 0.0). 
      *
      * @param value the Double to validate
      * @return true if the specified Double is a non-negative double, else false
      */
    def isValid(value: Double): Boolean = value > 0.0    

    def ensuringValid(d: Double): PosDouble = 
      if (isValid(d))
        d
      else  
        throw new AssertionError(Resources.invalidPosDouble)

    /**
      * A factory/validation method that produces a <code>PosDouble</code>, wrapped
      * in a <code>Success</code>, given a valid <code>Double</code> value, or if the
      * given <code>Double</code> is invalid, an <code>AssertionError</code>, wrapped
      * in a <code>Failure</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a PosDouble <code>Double</code>, it will return a <code>PosDouble</code>
      * representing that value, wrapped in a <code>Success</code>.
      * Otherwise, if the passed <code>Double</code> value is not PosDouble, this
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
      * @param value the <code>Double</code> to inspect, and if a non-negative double, return
      *     wrapped in a <code>Success(PosDouble)</code>.
      * @return the specified <code>Double</code> value wrapped
      *     in a <code>Success(PosDouble)</code>, if it is a non-negative double, else a <code>Failure(AssertionError)</code>.
      */
    def tryingValid(value: Double): Try[PosDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosDouble))

    /**
      * A validation method that produces a <code>Pass</code>
      * given a valid <code>Double</code> value, or
      * an error value of type <code>E</code> produced by passing the
      * given <em>invalid</em> <code>Double</code> value
      * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a non-negative double <code>Double</code>, it will return a <code>Pass</code>.
      * Otherwise, the passed <code>Double</code> value is not a non-negative double, so this
      * method will return a result of type <code>E</code> obtained by passing
      * the invalid <code>Double</code> value to the given function <code>f</code>,
      * wrapped in a `Fail`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Double</code> literals at compile time, whereas this method inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @tparam E error type produced by f
      * @param value the `Double` to validate that it is a non-negative double.
      * @param f function to produce an error when value is invalid
      * @return a `Pass` if the specified `Double` value is a non-negative double,
      *   else a `Fail` containing an error value produced by passing the
      *   specified `Double` to the given function `f`.
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))      

    /**
      * A factory/validation method that produces a <code>PosDouble</code>, wrapped
      * in a <code>Good</code>, given a valid <code>Double</code> value, or if the
      * given <code>Double</code> is invalid, an error value of type <code>B</code>
      * produced by passing the given <em>invalid</em> <code>Double</code> value
      * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a PosDouble <code>Double</code>, it will return a <code>PosDouble</code>
      * representing that value, wrapped in a <code>Good</code>.
      * Otherwise, the passed <code>Double</code> value is not PosDouble, so this
      * method will return a result of type <code>B</code> obtained by passing
      * the invalid <code>Double</code> value to the given function <code>f</code>,
      * wrapped in a `Bad`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Double</code> literals at compile time, whereas this method inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @tparam B error type produced by f
      * @param value the <code>Double</code> to inspect, and if PosDouble, return
      *     wrapped in a <code>Good(PosDouble)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Double</code> value wrapped
      *     in a <code>Good(PosDouble)</code>, if it is PosDouble, else a <code>Bad(f(value))</code>.
      */
    def goodOrElse[B](value: Double)(f: Double => B): PosDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /**
      * A factory/validation method that produces a <code>PosDouble</code>, wrapped
      * in a <code>Right</code>, given a valid <code>Double</code> value, or if the
      * given <code>Double</code> is invalid, an error value of type <code>L</code>
      * produced by passing the given <em>invalid</em> <code>Double</code> value
      * to the given function <code>f</code>, wrapped in a <code>Left</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a PosDouble <code>Double</code>, it will return a <code>PosDouble</code>
      * representing that value, wrapped in a <code>Right</code>.
      * Otherwise, the passed <code>Double</code> value is not PosDouble, so this
      * method will return a result of type <code>L</code> obtained by passing
      * the invalid <code>Double</code> value to the given function <code>f</code>,
      * wrapped in a `Left`.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Double</code> literals at compile time, whereas this method inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @tparam L error type produced by f
      * @param value the <code>Double</code> to inspect, and if PosDouble, return
      *     wrapped in a <code>Right(PosDouble)</code>.
      * @param f function to produce an error when value is invalid
      * @return the specified <code>Double</code> value wrapped
      *     in a <code>Right(PosDouble)</code>, if it is PosDouble, else a <code>Left(f(value))</code>.
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, PosDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /**
      * A factory method that produces a <code>PosDouble</code> given a
      * <code>Double</code> value and a default <code>PosDouble</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a non-negative <code>Double</code>, <em>i.e.</em>, a value greater
      * than 0.0, it will return a <code>PosDouble</code> representing that value.
      * Otherwise, the passed <code>Double</code> value is 0 or negative, so this
      * method will return the passed <code>default</code> value.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code>
      * factory method in that <code>apply</code> is implemented
      * via a macro that inspects <code>Double</code> literals at
      * compile time, whereas <code>from</code> inspects
      * <code>Double</code> values at run time.
      * </p>
      *
      * @param value the <code>Double</code> to inspect, and if positive, return.
      * @param default the <code>PosDouble</code> to return if the passed
      *     <code>Double</code> value is not positive.
      * @return the specified <code>Double</code> value wrapped in a
      *     <code>PosDouble</code>, if it is positive, else the
      *     <code>default</code> <code>PosDouble</code> value.
      */
    def fromOrElse(value: Double, default: => PosDouble): PosDouble =
      if (isValid(value)) value else default

    /**
      * The largest value representable as a positive <code>Double</code>, which is <code>PosDouble(Double.MaxValue)</code>.
      */
    val MaxValue: PosDouble = Double.MaxValue

    /**
      * The smallest value representable as a positive <code>Double</code>, which is <code>PosDouble(Double.MinPositiveValue)</code>.
      */
    val MinValue: PosDouble = Double.MinPositiveValue 

    /**
      * The smallest positive value greater than 0.0f representable
      * as a <code>PosDouble</code>, which is <code>PosDouble(Double.MinPositiveValue)</code>.
      */
    val MinPositiveValue: PosDouble = Double.MinPositiveValue

    /**
      * Positive infinity as a <code>PosDouble</code>, which is
      * <code>PosDouble(Double.PositiveInfinity)</code>.
      */
    val PositiveInfinity: PosDouble = Double.PositiveInfinity

    extension (p: PosDouble) {
      /**
        * Applies the passed <code>Double =&gt; Double</code> function to the underlying <code>Double</code>
        * value, and if the result is positive, returns the result wrapped in a <code>PosDouble</code>,
        * else throws <code>AssertionError</code>.
        *
        * <p>
        * This method will inspect the result of applying the given function to this
        * <code>PosDouble</code>'s underlying <code>Double</code> value and if the result
        * is non-negative, it will return a <code>PosDouble</code> representing that value.
        * Otherwise, the <code>Double</code> value returned by the given function is
        * not non-negative, so this method will throw <code>AssertionError</code>.
        * </p>
        *
        * <p>
        * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
        * call in that you get something you didn't already have if the assertion
        * succeeds: a <em>type</em> that promises an <code>Double</code> is non-negative.
        * With this method, you are asserting that you are convinced the result of
        * the computation represented by applying the given function to this <code>PosDouble</code>'s
        * value will not produce invalid value.
        * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
        * </p>
        *
        * @param f the <code>Double =&gt; Double</code> function to apply to this <code>PosDouble</code>'s
        *     underlying <code>Double</code> value.
        * @return the result of applying this <code>PosDouble</code>'s underlying <code>Double</code> value to
        *     to the passed function, wrapped in a <code>PosDouble</code> if it is non-negative (else throws <code>AssertionError</code>).
        * @throws AssertionError if the result of applying this <code>PosDouble</code>'s underlying <code>Double</code> value to
        *     to the passed function is not non-negative.
        */
      def ensuringValid(f: Double => Double): PosDouble = {
        val candidateResult: Double = f(p)
        if (PosDouble.isValid(candidateResult)) PosDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid PosDouble")
      }
    }

  }

  /** Opaque type representing a non-negative, finite <code>Double</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy both <code>&gt;= 0.0</code>
    * and <code>isFinite</code> (i.e. neither <code>Double.PositiveInfinity</code> nor
    * <code>Double.NegativeInfinity</code> nor <code>Double.NaN</code>).
    * </p>
    *
    * <p>
    * <code>PosZFiniteDouble</code> is a strict subtype of [[PosZDouble]], so anywhere a
    * <code>PosZDouble</code> is accepted a <code>PosZFiniteDouble</code> may be passed
    * without an explicit conversion.
    * </p>
    *
    * <p>
    * Use the compile-time <code>apply</code> overloads to construct instances from
    * literals, or the runtime factory methods [[PosZFiniteDouble.from]],
    * [[PosZFiniteDouble.ensuringValid]], and [[PosZFiniteDouble.fromOrElse]] for
    * values known only at runtime.
    * </p>
    */
  opaque type PosZFiniteDouble <: PosZDouble = Double

  /** Companion object for the [[PosZFiniteDouble]] opaque type.
    *
    * Provides compile-time <code>apply</code> overloads, runtime factory and
    * validation methods, implicit widening conversions, and an extension method
    * for post-computation validation.
    */
  object PosZFiniteDouble {

    /** Implicitly widens a [[PosZFiniteDouble]] to a plain <code>Double</code>. */
    given Conversion[PosZFiniteDouble, Double] with {
      def apply(x: PosZFiniteDouble): Double = x.toDouble
    }

    /** Converts a compile-time non-negative <code>Int</code> literal to a [[PosZFiniteDouble]].
      *
      * The inline overload is checked at compile time; the runtime overload
      * performs no validation (all <code>Int</code> values widen safely to a
      * non-negative, finite <code>Double</code> when the source is non-negative).
      *
      * @throws compile-time error if the literal is negative or not a literal
      */
    given Conversion[Int, PosZFiniteDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosZFiniteDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v < 0 then
              error("PosZFiniteDouble cannot be instantiated with a negative integer literal")
            else
              v.toDouble.asInstanceOf[PosZFiniteDouble]
          case None =>
            error("PosZFiniteDouble conversion requires an integer literal")
        }
      def apply(x: Int): PosZFiniteDouble = x.toDouble
    }

    /** Converts a compile-time non-negative <code>Long</code> literal to a [[PosZFiniteDouble]].
      *
      * The inline overload is checked at compile time; the runtime overload
      * performs no validation (all non-negative <code>Long</code> values widen
      * safely to a finite <code>Double</code>).
      *
      * @throws compile-time error if the literal is negative or not a literal
      */
    given Conversion[Long, PosZFiniteDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): PosZFiniteDouble =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v < 0L then
              error("PosZFiniteDouble cannot be instantiated with a negative long literal")
            else
              v.toDouble.asInstanceOf[PosZFiniteDouble]
          case None =>
            error("PosZFiniteDouble conversion requires a long literal")
        }
      def apply(x: Long): PosZFiniteDouble = x.toDouble
    }

    /** Converts a compile-time non-negative, finite <code>Float</code> literal to a [[PosZFiniteDouble]].
      *
      * The inline overload is checked at compile time.  The runtime overload
      * performs no validation — callers must ensure the <code>Float</code>
      * value is non-negative and finite before using it.
      *
      * @throws compile-time error if the literal is negative or not a literal
      */
    given Conversion[Float, PosZFiniteDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): PosZFiniteDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v < 0.0f then
              error("PosZFiniteDouble cannot be instantiated with a negative float literal")
            else
              v.toDouble.asInstanceOf[PosZFiniteDouble]
          case None =>
            error("PosZFiniteDouble conversion requires a float literal")
        }
      def apply(x: Float): PosZFiniteDouble = x.toDouble
    }

    /** Converts a compile-time non-negative, finite <code>Double</code> literal to a [[PosZFiniteDouble]].
      *
      * The inline overload is checked at compile time and rejects negative values
      * and positive infinity.  The runtime overload performs no validation —
      * callers must ensure the value is non-negative and finite before using it.
      *
      * @throws compile-time error if the literal is negative, infinite, or not a literal
      */
    given Conversion[Double, PosZFiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): PosZFiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v < 0.0 || v == Double.PositiveInfinity then
              error("PosZFiniteDouble cannot be instantiated with a negative double literal or positive infinity")
            else
              v.asInstanceOf[PosZFiniteDouble]
          case None =>
            error("PosZFiniteDouble conversion requires a double literal")
        }
      def apply(x: Double): PosZFiniteDouble = x
    }

    /** Compile-time factory for creating a [[PosZFiniteDouble]] from a <code>Double</code> literal.
      *
      * This inline method inspects the provided double literal at compile time
      * and rejects negative values, <code>Double.PositiveInfinity</code>, and
      * <code>Double.NegativeInfinity</code>.  Use it as: <code>PosZFiniteDouble(5.0)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton <code>Double</code> literal type
      * @param d the <code>Double</code> literal
      * @return a [[PosZFiniteDouble]] representing the given non-negative, finite literal
      * @throws compile-time error if the literal is negative, infinite, or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): PosZFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v < 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
            error("PosZFiniteDouble cannot be instantiated with a negative double literal or positive infinity")
          else
            v.asInstanceOf[PosZFiniteDouble]
        case None =>
          error("PosZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosZFiniteDouble]] from a <code>Float</code> literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects negative values.  Use it as: <code>PosZFiniteDouble(5.0f)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton <code>Float</code> literal type
      * @param f the <code>Float</code> literal
      * @return a [[PosZFiniteDouble]] representing the given non-negative literal widened to <code>Double</code>
      * @throws compile-time error if the literal is negative or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosZFiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v < 0.0f then
            error("PosZFiniteDouble cannot be instantiated with a negative float literal")
          else
            v.toDouble.asInstanceOf[PosZFiniteDouble]
        case None =>
          error("PosZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosZFiniteDouble]] from a <code>Long</code> literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects negative values.  Use it as: <code>PosZFiniteDouble(5L)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton <code>Long</code> literal type
      * @param l the <code>Long</code> literal
      * @return a [[PosZFiniteDouble]] representing the given non-negative literal widened to <code>Double</code>
      * @throws compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosZFiniteDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v < 0L then
            error("PosZFiniteDouble cannot be instantiated with a negative long literal")
          else
            v.toDouble.asInstanceOf[PosZFiniteDouble]
        case None =>
          error("PosZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosZFiniteDouble]] from an <code>Int</code> literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative values.  Use it as: <code>PosZFiniteDouble(5)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton <code>Int</code> literal type
      * @param i the <code>Int</code> literal
      * @return a [[PosZFiniteDouble]] representing the given non-negative literal widened to <code>Double</code>
      * @throws compile-time error if the literal is negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosZFiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v < 0 then
            error("PosZFiniteDouble cannot be instantiated with a negative integer literal")
          else
            v.toDouble.asInstanceOf[PosZFiniteDouble]
        case None =>
          error("PosZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Returns <code>true</code> if the provided <code>Double</code> is a valid [[PosZFiniteDouble]]
      * value — that is, if it is both <code>&gt;= 0.0</code> and finite (<code>isFinite</code>).
      *
      * @param value the <code>Double</code> to validate
      * @return <code>true</code> if <code>value &gt;= 0.0 &amp;&amp; value.isFinite</code>, <code>false</code> otherwise
      */
    def isValid(value: Double): Boolean = value >= 0.0 && value.isFinite

    /** Returns <code>Some(PosZFiniteDouble)</code> if the given <code>Double</code> is a valid
      * [[PosZFiniteDouble]] (non-negative and finite), or <code>None</code> otherwise.
      *
      * <p>
      * This factory method inspects the value at runtime.  Use the compile-time
      * <code>apply</code> overloads when constructing from literals.
      * </p>
      *
      * @param d the <code>Double</code> to inspect
      * @return <code>Some(PosZFiniteDouble)</code> if <code>d &gt;= 0.0</code> and finite, else <code>None</code>
      */
    def from(d: Double): Option[PosZFiniteDouble] =
      if (isValid(d)) Some(d) else None

    /** Returns the given <code>Double</code> as a [[PosZFiniteDouble]] if it is valid,
      * or throws <code>AssertionError</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&gt;= 0.0</code> and finite.  This method is
      * appropriate when you are certain the value satisfies the constraint; use
      * [[from]] when you want to handle the invalid case gracefully.
      * </p>
      *
      * @param d the <code>Double</code> to return as a [[PosZFiniteDouble]]
      * @return <code>d</code> as a [[PosZFiniteDouble]] if valid
      * @throws AssertionError if <code>d</code> is negative or infinite
      */
    def ensuringValid(d: Double): PosZFiniteDouble =
      if (isValid(d))
        d
      else
        throw new AssertionError(Resources.invalidPosZFiniteDouble)

    /** Returns the given <code>Double</code> as a [[PosZFiniteDouble]] if it is valid,
      * or the given <code>default</code> value otherwise.
      *
      * <p>
      * A value is valid if it is <code>&gt;= 0.0</code> and finite.
      * </p>
      *
      * @param value the <code>Double</code> to inspect
      * @param default the [[PosZFiniteDouble]] to return if <code>value</code> is invalid
      * @return <code>value</code> as a [[PosZFiniteDouble]] if valid, else <code>default</code>
      */
    def fromOrElse(value: Double, default: => PosZFiniteDouble): PosZFiniteDouble =
      if (isValid(value)) value else default

    /** A factory/validation method that produces a <code>PosZFiniteDouble</code> wrapped
      * in a <code>Success</code> if the given <code>Double</code> is valid, or an
      * <code>AssertionError</code> wrapped in a <code>Failure</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&gt;= 0.0</code> and finite.
      * </p>
      *
      * <p>
      * This factory method differs from the compile-time <code>apply</code> overloads
      * in that it inspects values at runtime.
      * </p>
      *
      * @param value the <code>Double</code> to inspect
      * @return <code>Success(PosZFiniteDouble)</code> if valid, else <code>Failure(AssertionError)</code>
      */
    def tryingValid(value: Double): Try[PosZFiniteDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZFiniteDouble))

    /** A validation method that produces a <code>Pass</code> given a valid <code>Double</code>
      * value, or a <code>Fail</code> containing an error value produced by passing the
      * invalid <code>Double</code> to the function <code>f</code>.
      *
      * <p>
      * A value is valid if it is <code>&gt;= 0.0</code> and finite.
      * </p>
      *
      * @tparam E the error type produced by <code>f</code>
      * @param value the <code>Double</code> to validate
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Pass</code> if valid, else <code>Fail(f(value))</code>
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** A factory/validation method that produces a <code>PosZFiniteDouble</code> wrapped
      * in a <code>Good</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Bad</code>.
      *
      * <p>
      * A value is valid if it is <code>&gt;= 0.0</code> and finite.
      * </p>
      *
      * @tparam B the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Good(PosZFiniteDouble)</code> if valid, else <code>Bad(f(value))</code>
      */
    def goodOrElse[B](value: Double)(f: Double => B): PosZFiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** A factory/validation method that produces a <code>PosZFiniteDouble</code> wrapped
      * in a <code>Right</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Left</code>.
      *
      * <p>
      * A value is valid if it is <code>&gt;= 0.0</code> and finite.
      * </p>
      *
      * @tparam L the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Right(PosZFiniteDouble)</code> if valid, else <code>Left(f(value))</code>
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, PosZFiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** The largest value representable as a [[PosZFiniteDouble]], which is
      * <code>PosZFiniteDouble(Double.MaxValue)</code>.
      */
    val MaxValue: PosZFiniteDouble = Double.MaxValue

    /** The smallest positive value representable as a [[PosZFiniteDouble]], which is
      * <code>PosZFiniteDouble(Double.MinPositiveValue)</code>.
      */
    val MinValue: PosZFiniteDouble = Double.MinPositiveValue

    extension (p: PosZFiniteDouble) {
      /** Applies the given <code>Double =&gt; Double</code> function to the underlying
        * <code>Double</code> value, and returns the result as a [[PosZFiniteDouble]] if
        * it is valid, or throws <code>AssertionError</code> if it is not.
        *
        * <p>
        * A result is valid if it is <code>&gt;= 0.0</code> and finite.  This method
        * is useful for asserting that a derived value still satisfies the
        * <code>PosZFiniteDouble</code> constraint without leaving the opaque type.
        * </p>
        *
        * @param f the <code>Double =&gt; Double</code> function to apply
        * @return the result of <code>f(p)</code> as a [[PosZFiniteDouble]] if valid
        * @throws AssertionError if the result of <code>f(p)</code> is negative or infinite
        */
      def ensuringValid(f: Double => Double): PosZFiniteDouble = {
        val candidateResult: Double = f(p)
        if (PosZFiniteDouble.isValid(candidateResult)) PosZFiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid PosZFiniteDouble")
      }
    }
  }

  /** Opaque type representing a strictly positive, finite <code>Double</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy both <code>&gt; 0.0</code>
    * and <code>isFinite</code> (i.e. neither <code>Double.PositiveInfinity</code> nor
    * <code>Double.NegativeInfinity</code> nor <code>Double.NaN</code>).
    * </p>
    *
    * <p>
    * <code>PosFiniteDouble</code> is a strict subtype of both [[PosZFiniteDouble]] and
    * [[PosZDouble]], so it can be widened to either without an explicit conversion.
    * </p>
    *
    * <p>
    * Use the compile-time <code>apply</code> overloads to construct instances from
    * literals, or the runtime factory methods [[PosFiniteDouble.from]],
    * [[PosFiniteDouble.ensuringValid]], and [[PosFiniteDouble.fromOrElse]] for values
    * known only at runtime.
    * </p>
    */
  opaque type PosFiniteDouble <: PosZFiniteDouble = Double

  /** Companion object for the [[PosFiniteDouble]] opaque type.
    *
    * Provides compile-time <code>apply</code> overloads, runtime factory and
    * validation methods, and an extension method for post-computation validation.
    * No implicit widening conversions from primitive types are provided because
    * zero and negative values must be rejected, making a blanket conversion unsafe.
    */
  object PosFiniteDouble {

    /** Implicitly widens a [[PosFiniteDouble]] to a plain <code>Double</code>. */
    given Conversion[PosFiniteDouble, Double] with {
      def apply(x: PosFiniteDouble): Double = x.toDouble
    }

    /** Converts a compile-time positive <code>Int</code> literal to a [[PosFiniteDouble]].
      *
      * The inline overload validates literals at compile time. The runtime overload
      * widens the value to <code>Double</code> without additional checks.
      *
      * @throws compile-time error if the literal is non-positive or not a literal
      */
    given Conversion[Int, PosFiniteDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): PosFiniteDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v <= 0 then
              error("PosFiniteDouble cannot be instantiated with a non-positive integer literal")
            else
              v.toDouble.asInstanceOf[PosFiniteDouble]
          case None =>
            error("PosFiniteDouble conversion requires an integer literal")
        }
      def apply(x: Int): PosFiniteDouble = x.toDouble
    }

    /** Converts a compile-time positive <code>Long</code> literal to a [[PosFiniteDouble]].
      *
      * The inline overload validates literals at compile time. The runtime overload
      * widens the value to <code>Double</code> without additional checks.
      *
      * @throws compile-time error if the literal is non-positive or not a literal
      */
    given Conversion[Long, PosFiniteDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): PosFiniteDouble =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v <= 0L then
              error("PosFiniteDouble cannot be instantiated with a non-positive long literal")
            else
              v.toDouble.asInstanceOf[PosFiniteDouble]
          case None =>
            error("PosFiniteDouble conversion requires a long literal")
        }
      def apply(x: Long): PosFiniteDouble = x.toDouble
    }

    /** Converts a compile-time positive, finite <code>Float</code> literal to a [[PosFiniteDouble]].
      *
      * The inline overload validates literals at compile time. The runtime overload
      * widens the value to <code>Double</code> without additional checks.
      *
      * @throws compile-time error if the literal is non-positive, infinite, or not a literal
      */
    given Conversion[Float, PosFiniteDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): PosFiniteDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v <= 0.0f || v == Float.PositiveInfinity then
              error("PosFiniteDouble cannot be instantiated with a non-positive float literal or infinity")
            else
              v.toDouble.asInstanceOf[PosFiniteDouble]
          case None =>
            error("PosFiniteDouble conversion requires a float literal")
        }
      def apply(x: Float): PosFiniteDouble = x.toDouble
    }

    /** Converts a compile-time positive, finite <code>Double</code> literal to a [[PosFiniteDouble]].
      *
      * The inline overload validates literals at compile time. The runtime overload
      * returns the value without additional checks.
      *
      * @throws compile-time error if the literal is non-positive, infinite, or not a literal
      */
    given Conversion[Double, PosFiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): PosFiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v <= 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
              error("PosFiniteDouble cannot be instantiated with a non-positive double literal or infinity")
            else
              v.asInstanceOf[PosFiniteDouble]
          case None =>
            error("PosFiniteDouble conversion requires a double literal")
        }
      def apply(x: Double): PosFiniteDouble = x
    }

    /** Compile-time factory for creating a [[PosFiniteDouble]] from a <code>Double</code> literal.
      *
      * This inline method inspects the provided double literal at compile time
      * and rejects zero, negative values, and infinities.
      * Use it as: <code>PosFiniteDouble(5.0)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton <code>Double</code> literal type
      * @param d the <code>Double</code> literal
      * @return a [[PosFiniteDouble]] representing the given strictly positive, finite literal
      * @throws compile-time error if the literal is non-positive, infinite, or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): PosFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v <= 0.0 || v == Double.PositiveInfinity || v == Double.NegativeInfinity then
            error("PosFiniteDouble cannot be instantiated with a non-positive double literal or infinity")
          else
            v.asInstanceOf[PosFiniteDouble]
        case None =>
          error("PosFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosFiniteDouble]] from a <code>Float</code> literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects zero, negative values, and positive infinity.
      * Use it as: <code>PosFiniteDouble(5.0f)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton <code>Float</code> literal type
      * @param f the <code>Float</code> literal
      * @return a [[PosFiniteDouble]] representing the given strictly positive literal widened to <code>Double</code>
      * @throws compile-time error if the literal is non-positive, infinite, or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosFiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v <= 0.0f || v == Float.PositiveInfinity then
            error("PosFiniteDouble cannot be instantiated with a non-positive float literal or infinity")
          else
            v.toDouble.asInstanceOf[PosFiniteDouble]
        case None =>
          error("PosFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosFiniteDouble]] from a <code>Long</code> literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects zero and negative values.
      * Use it as: <code>PosFiniteDouble(5L)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam L the singleton <code>Long</code> literal type
      * @param l the <code>Long</code> literal
      * @return a [[PosFiniteDouble]] representing the given strictly positive literal widened to <code>Double</code>
      * @throws compile-time error if the literal is non-positive or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosFiniteDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v <= 0L then
            error("PosFiniteDouble cannot be instantiated with a non-positive long literal")
          else
            v.toDouble.asInstanceOf[PosFiniteDouble]
        case None =>
          error("PosFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[PosFiniteDouble]] from an <code>Int</code> literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects zero and negative values.
      * Use it as: <code>PosFiniteDouble(5)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton <code>Int</code> literal type
      * @param i the <code>Int</code> literal
      * @return a [[PosFiniteDouble]] representing the given strictly positive literal widened to <code>Double</code>
      * @throws compile-time error if the literal is non-positive or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): PosFiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v <= 0 then
            error("PosFiniteDouble cannot be instantiated with a non-positive integer literal")
          else
            v.toDouble.asInstanceOf[PosFiniteDouble]
        case None =>
          error("PosFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Returns <code>true</code> if the provided <code>Double</code> is a valid [[PosFiniteDouble]]
      * value — that is, if it is both <code>&gt; 0.0</code> and finite (<code>isFinite</code>).
      *
      * @param value the <code>Double</code> to validate
      * @return <code>true</code> if <code>value &gt; 0.0 &amp;&amp; value.isFinite</code>, <code>false</code> otherwise
      */
    def isValid(value: Double): Boolean = value > 0.0 && value.isFinite

    /** Returns <code>Some(PosFiniteDouble)</code> if the given <code>Double</code> is a valid
      * [[PosFiniteDouble]] (strictly positive and finite), or <code>None</code> otherwise.
      *
      * <p>
      * This factory method inspects the value at runtime.  Use the compile-time
      * <code>apply</code> overloads when constructing from literals.
      * </p>
      *
      * @param d the <code>Double</code> to inspect
      * @return <code>Some(PosFiniteDouble)</code> if <code>d &gt; 0.0</code> and finite, else <code>None</code>
      */
    def from(d: Double): Option[PosFiniteDouble] =
      if (isValid(d)) Some(d) else None

    /** Returns the given <code>Double</code> as a [[PosFiniteDouble]] if it is valid,
      * or throws <code>AssertionError</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&gt; 0.0</code> and finite.  This method is
      * appropriate when you are certain the value satisfies the constraint; use
      * [[from]] when you want to handle the invalid case gracefully.
      * </p>
      *
      * @param d the <code>Double</code> to return as a [[PosFiniteDouble]]
      * @return <code>d</code> as a [[PosFiniteDouble]] if valid
      * @throws AssertionError if <code>d</code> is zero, negative, or infinite
      */
    def ensuringValid(d: Double): PosFiniteDouble =
      if (isValid(d))
        d
      else
        throw new AssertionError(Resources.invalidPosFiniteDouble)

    /** Returns the given <code>Double</code> as a [[PosFiniteDouble]] if it is valid,
      * or the given <code>default</code> value otherwise.
      *
      * <p>
      * A value is valid if it is <code>&gt; 0.0</code> and finite.
      * </p>
      *
      * @param value the <code>Double</code> to inspect
      * @param default the [[PosFiniteDouble]] to return if <code>value</code> is invalid
      * @return <code>value</code> as a [[PosFiniteDouble]] if valid, else <code>default</code>
      */
    def fromOrElse(value: Double, default: => PosFiniteDouble): PosFiniteDouble =
      if (isValid(value)) value else default

    /** A factory/validation method that produces a <code>PosFiniteDouble</code> wrapped
      * in a <code>Success</code> if the given <code>Double</code> is valid, or an
      * <code>AssertionError</code> wrapped in a <code>Failure</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&gt; 0.0</code> and finite.
      * </p>
      *
      * <p>
      * This factory method differs from the compile-time <code>apply</code> overloads
      * in that it inspects values at runtime.
      * </p>
      *
      * @param value the <code>Double</code> to inspect
      * @return <code>Success(PosFiniteDouble)</code> if valid, else <code>Failure(AssertionError)</code>
      */
    def tryingValid(value: Double): Try[PosFiniteDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosFiniteDouble))

    /** A validation method that produces a <code>Pass</code> given a valid <code>Double</code>
      * value, or a <code>Fail</code> containing an error value produced by passing the
      * invalid <code>Double</code> to the function <code>f</code>.
      *
      * <p>
      * A value is valid if it is <code>&gt; 0.0</code> and finite.
      * </p>
      *
      * @tparam E the error type produced by <code>f</code>
      * @param value the <code>Double</code> to validate
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Pass</code> if valid, else <code>Fail(f(value))</code>
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** A factory/validation method that produces a <code>PosFiniteDouble</code> wrapped
      * in a <code>Good</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Bad</code>.
      *
      * <p>
      * A value is valid if it is <code>&gt; 0.0</code> and finite.
      * </p>
      *
      * @tparam B the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Good(PosFiniteDouble)</code> if valid, else <code>Bad(f(value))</code>
      */
    def goodOrElse[B](value: Double)(f: Double => B): PosFiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** A factory/validation method that produces a <code>PosFiniteDouble</code> wrapped
      * in a <code>Right</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Left</code>.
      *
      * <p>
      * A value is valid if it is <code>&gt; 0.0</code> and finite.
      * </p>
      *
      * @tparam L the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Right(PosFiniteDouble)</code> if valid, else <code>Left(f(value))</code>
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, PosFiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** The largest value representable as a [[PosFiniteDouble]], which is
      * <code>PosFiniteDouble(Double.MaxValue)</code>.
      */
    val MaxValue: PosFiniteDouble = Double.MaxValue

    /** The smallest positive value representable as a [[PosFiniteDouble]], which is
      * <code>PosFiniteDouble(Double.MinPositiveValue)</code>.
      */
    val MinValue: PosFiniteDouble = Double.MinPositiveValue

    extension (p: PosFiniteDouble) {
      /** Applies the given <code>Double =&gt; Double</code> function to the underlying
        * <code>Double</code> value, and returns the result as a [[PosFiniteDouble]] if
        * it is valid, or throws <code>AssertionError</code> if it is not.
        *
        * <p>
        * A result is valid if it is <code>&gt; 0.0</code> and finite.  This method
        * is useful for asserting that a derived value still satisfies the
        * <code>PosFiniteDouble</code> constraint without leaving the opaque type.
        * </p>
        *
        * @param f the <code>Double =&gt; Double</code> function to apply
        * @return the result of <code>f(p)</code> as a [[PosFiniteDouble]] if valid
        * @throws AssertionError if the result of <code>f(p)</code> is zero, negative, or infinite
        */
      def ensuringValid(f: Double => Double): PosFiniteDouble = {
        val candidateResult: Double = f(p)
        if (PosFiniteDouble.isValid(candidateResult)) PosFiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid PosFiniteDouble")
      }
    }
  }

}

