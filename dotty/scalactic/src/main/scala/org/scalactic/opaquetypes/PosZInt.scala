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
import scala.util.{Try, Success, Failure}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}
import scala.compiletime.{ constValueOpt, error }

opaque type PosZInt = Int

trait PosZIntConversionsLowPriority {
  given Conversion[PosZInt, Long] with {
    def apply(pos: PosZInt): Long = pos.value.toLong
  }
  given Conversion[PosZInt, Float] with {
    def apply(pos: PosZInt): Float = pos.value.toFloat
  }
  given Conversion[PosZInt, Double] with {
    def apply(pos: PosZInt): Double = pos.value.toDouble
  }
}

object PosZInt extends PosZIntConversionsLowPriority {
  
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

  def from(i: Int): Option[PosZInt] =
    if (i >= 0) Some(i) else None

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
   * Otherwise, the passed <code>Int</code> value is not PosZInt, so this
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
   * @param value the <code>Int</code> to inspect, and if $typeDesc$, return
   *     wrapped in a <code>Success(PosZInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Success(PosZInt)</code>, if it is $typeDesc$, else a <code>Failure(AssertionError)</code>.
   */
   def tryingValid(value: Int): Try[PosZInt] =
     if (value >= 0)
       Success(value)
     else
       Failure(new AssertionError(Resources.invalidPosZInt))

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
   * it is a $typeDesc$ <code>Int</code>, it will return a <code>Pass</code>.
   * Otherwise, the passed <code>Int</code> value is $typeDesc$, so this
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
   * @param value the `Int` to validate that it is $typeDesc$.
   * @return a `Pass` if the specified `Int` value is $typeDesc$,
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
   * @param value the <code>Int</code> to inspect, and if PosZInt, return
   *     wrapped in a <code>Good(PosZInt)</code>.
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
   * @param value the <code>Int</code> to inspect, and if PosZInt, return
   *     wrapped in a <code>Right(PosZInt)</code>.
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
   * The largest value representable as a $typeDesc$ <code>Int</code>, which is <code>PosZInt($typeMaxValueNumber$)</code>.
   */
  val MaxValue: PosZInt = Int.MaxValue

  /**
   * The smallest value representable as a $typeDesc$ <code>Int</code>, which is <code>PosZInt($typeMinValueNumber$)</code>.
   */
  val MinValue: PosZInt = 0   
  
  extension (x: PosZInt) {
    def value: Int = x
    def abs: PosZInt = x
    /**
      * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
      */
    def max(that: PosZInt): PosZInt = math.max(x, that)

    /**
      * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
      */
    def min(that: PosZInt): PosZInt = math.min(x, that)

    /**
      * Returns a string representation of this <code>PosZInt</code>'s underlying <code>Int</code> as an
      * unsigned integer in base&nbsp;2.
      *
      * <p>
      * The unsigned integer value is the argument plus 2<sup>32</sup>
      * if this <code>PosZInt</code>'s underlying <code>Int</code> is negative; otherwise it is equal to the
      * underlying <code>Int</code>.  This value is converted to a string of ASCII digits
      * in binary (base&nbsp;2) with no extra leading <code>0</code>s.
      * If the unsigned magnitude is zero, it is represented by a
      * single zero character <code>'0'</code>
      * (<code>'&#92;u0030'</code>); otherwise, the first character of
      * the representation of the unsigned magnitude will not be the
      * zero character. The characters <code>'0'</code>
      * (<code>'&#92;u0030'</code>) and <code>'1'</code>
      * (<code>'&#92;u0031'</code>) are used as binary digits.
      * </p>
      *
      * @return  the string representation of the unsigned integer value
      *          represented by this <code>PosZInt</code>'s underlying <code>Int</code> in binary (base&nbsp;2).
      */
    def toBinaryString: String = java.lang.Integer.toBinaryString(x)

    /**
      * Returns a string representation of this <code>PosZInt</code>'s underlying <code>Int</code> as an
      * unsigned integer in base&nbsp;16.
      *
      * <p>
      * The unsigned integer value is the argument plus 2<sup>32</sup>
      * if this <code>PosZInt</code>'s underlying <code>Int</code> is negative; otherwise, it is equal to the
      * this <code>PosZInt</code>'s underlying <code>Int</code>  This value is converted to a string of ASCII digits
      * in hexadecimal (base&nbsp;16) with no extra leading
      * <code>0</code>s. If the unsigned magnitude is zero, it is
      * represented by a single zero character <code>'0'</code>
      * (<code>'&#92;u0030'</code>); otherwise, the first character of
      * the representation of the unsigned magnitude will not be the
      * zero character. The following characters are used as
      * hexadecimal digits:
      * </p>
      *
      * <blockquote>
      *  <code>0123456789abcdef</code>
      * </blockquote>
      *
      * These are the characters <code>'&#92;u0030'</code> through
      * <code>'&#92;u0039'</code> and <code>'&#92;u0061'</code> through
      * <code>'&#92;u0066'</code>. If uppercase letters are
      * desired, the <code>toUpperCase</code> method may
      * be called on the result.
      *
      * @return  the string representation of the unsigned integer value
      *          represented by this <code>PosZInt</code>'s underlying <code>Int</code> in hexadecimal (base&nbsp;16).
      */
    def toHexString: String = java.lang.Integer.toHexString(x)

    /**
      * Returns a string representation of this <code>PosZInt</code>'s underlying <code>Int</code> as an
      * unsigned integer in base&nbsp;8.
      *
      * <p>The unsigned integer value is this <code>PosZInt</code>'s underlying <code>Int</code> plus 2<sup>32</sup>
      * if the underlying <code>Int</code> is negative; otherwise, it is equal to the
      * underlying <code>Int</code>.  This value is converted to a string of ASCII digits
      * in octal (base&nbsp;8) with no extra leading <code>0</code>s.
      *
      * <p>If the unsigned magnitude is zero, it is represented by a
      * single zero character <code>'0'</code>
      * (<code>'&#92;u0030'</code>); otherwise, the first character of
      * the representation of the unsigned magnitude will not be the
      * zero character. The following characters are used as octal
      * digits:
      *
      * <blockquote>
      * <code>01234567</code>
      * </blockquote>
      *
      * These are the characters <code>'&#92;u0030'</code> through
      * <code>'&#92;u0037'</code>.
      *
      * @return  the string representation of the unsigned integer value
      *          represented by this <code>PosZInt</code>'s underlying <code>Int</code> in octal (base&nbsp;8).
      */
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

    def ensuringValid(f: Int => Int): PosZInt = {
      val res = f(x)
      if (res < 0) 
        throw new AssertionError(Resources.invalidPosZInt)
      else res
    }
  }
  
  given Conversion[PosZInt, Int] with {
    def apply(x: PosZInt): Int = x
  }
 
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
 
  given Ordering[PosZInt] with {
    def compare(x: PosZInt, y: PosZInt): Int = x.compareTo(y)
  }
}