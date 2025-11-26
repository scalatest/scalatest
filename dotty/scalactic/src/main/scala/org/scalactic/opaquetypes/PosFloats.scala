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

object PosFloats {

  opaque type PosZFloat = Float

  object PosZFloat {

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
      * The inline overload checks integer literals at compile time; the runtime
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

    /** Ordering instance for PosZFloat that orders by numeric value. */
    given Ordering[PosZFloat] with {
      def compare(x: PosZFloat, y: PosZFloat): Int = x.compareTo(y)
    }

    extension (p: PosZFloat) {
      /** Return the underlying Float value. */
      def value: Float = p
      /** Return true if this PosZFloat is positive infinity. */
      def isPosInfinity: Boolean = p == Float.PositiveInfinity
    }  

  }

  opaque type PosFloat = Float

  object PosFloat {
    
    def from(f: Float): Option[PosFloat] =
      if (f >= 0.0f) Some(f) else None

    def ensuringValid(f: Float): PosFloat = 
      if (f < 0.0f) 
        throw new AssertionError(Resources.invalidPosFloat)
      else f

  } 

}
