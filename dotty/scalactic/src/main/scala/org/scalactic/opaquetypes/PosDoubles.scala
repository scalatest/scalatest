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

  } 

}

