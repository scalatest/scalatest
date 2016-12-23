/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.anyvals

import scala.collection.immutable.NumericRange

/**
 * An <code>AnyVal</code> for non-negative <code>Float</code>s.
 *
 * <p>
 * Note: A <code>PosFloat</code> may have value <code>Float.PositiveInfinity</code>,
 * but cannot have value <code>Float.NegativeInfinity</code> or <code>Float.NaN</code>.
 * </p>
 *
 * <p>
 * Because <code>PosZFloat</code> is an <code>AnyVal</code> it will usually be
 * as efficient as an <code>Float</code>, being boxed only when a
 * <code>Float</code> would have been boxed.
 * </p>
 * 
 * <p>
 * The <code>PosZFloat.apply</code> factory method is
 * implemented in terms of a macro that checks literals for
 * validity at compile time. Calling
 * <code>PosZFloat.apply</code> with a literal
 * <code>Float</code> value will either produce a valid
 * <code>PosZFloat</code> instance at run time or an error at
 * compile time. Here's an example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import anyvals._
 * import anyvals._
 *
 * scala&gt; PosZFloat(1.1F)
 * res0: org.scalactic.anyvals.PosZFloat = PosZFloat(1.1)
 *
 * scala&gt; PosZFloat(0.0F)
 * res1: org.scalactic.anyvals.PosZFloat = PosZFloat(0.0)
 *
 * scala&gt; PosZFloat(-1.1F)
 * &lt;console&gt;:14: error: PosZFloat.apply can only be invoked on a non-negative (i &gt;= 0.0F) floating point literal, like PosZFloat(42.0F).
 *               PosZFloat(-1.1F)
 *                        ^
 * </pre>
 *
 * <p>
 * <code>PosZFloat.apply</code> cannot be used if the value
 * being passed is a variable (<em>i.e.</em>, not a literal),
 * because the macro cannot determine the validity of variables
 * at compile time (just literals). If you try to pass a
 * variable to <code>PosZFloat.apply</code>, you'll get a
 * compiler error that suggests you use a different factor
 * method, <code>PosZFloat.from</code>, instead:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val x = 1.1F
 * x: Float = 1.1
 *
 * scala&gt; PosZFloat(x)
 * &lt;console&gt;:15: error: PosZFloat.apply can only be invoked on a floating point literal, like PosZFloat(42.0F). Please use PosZFloat.from instead.
 *               PosZFloat(x)
 *                        ^
 * </pre>
 *
 * <p>
 * The <code>PosZFloat.from</code> factory method will inspect
 * the value at runtime and return an
 * <code>Option[PosZFloat]</code>. If the value is valid,
 * <code>PosZFloat.from</code> will return a
 * <code>Some[PosZFloat]</code>, else it will return a
 * <code>None</code>.  Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; PosZFloat.from(x)
 * res4: Option[org.scalactic.anyvals.PosZFloat] = Some(PosZFloat(1.1))
 *
 * scala&gt; val y = -1.1F
 * y: Float = -1.1
 *
 * scala&gt; PosZFloat.from(y)
 * res5: Option[org.scalactic.anyvals.PosZFloat] = None
 * </pre>
 * 
 * <p>
 * The <code>PosZFloat.apply</code> factory method is marked implicit, so that
 * you can pass literal <code>Float</code>s into methods that require
 * <code>PosZFloat</code>, and get the same compile-time checking you get when
 * calling <code>PosZFloat.apply</code> explicitly. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; def invert(pos: PosZFloat): Float = Float.MaxValue - pos
 * invert: (pos: org.scalactic.anyvals.PosZFloat)Float
 *
 * scala&gt; invert(0.0F)
 * res6: Float = 3.4028235E38
 *
 * scala&gt; invert(Float.MaxValue)
 * res7: Float = 0.0
 *
 * scala&gt; invert(-1.1F)
 * &lt;console&gt;:15: error: PosZFloat.apply can only be invoked on a non-negative (i &gt;= 0.0F) floating point literal, like PosZFloat(42.0F).
   *             invert(-1.1F)
   *                     ^
 * </pre>
 *
 * <p>
 * This example also demonstrates that the
 * <code>PosZFloat</code> companion object also defines
 * implicit widening conversions when a similar conversion is
 * provided in Scala. This makes it convenient to use a
 * <code>PosZFloat</code> where a <code>Float</code> or wider
 * type is needed. An example is the subtraction in the body of
 * the <code>invert</code> method defined above,
 * <code>Float.MaxValue - pos</code>. Although
 * <code>Float.MaxValue</code> is an <code>Float</code>, which
 * has no <code>-</code> method that takes a
 * <code>PosZFloat</code> (the type of <code>pos</code>), you
 * can still subtract <code>pos</code>, because the
 * <code>PosZFloat</code> will be implicitly widened to
 * <code>Float</code>.
 * </p>
 *
 * @param value The <code>Float</code> value underlying this <code>PosZFloat</code>.
 */ 
final class PosZFloat private (val value: Float) extends AnyVal {

  /**
   * A string representation of this <code>PosZFloat</code>.
   */
  override def toString: String = s"PosZFloat($value)"

  /**
   * Converts this <code>PosZFloat</code> to a <code>Byte</code>.
   */
  def toByte: Byte = value.toByte

  /**
   * Converts this <code>PosZFloat</code> to a <code>Short</code>.
   */
  def toShort: Short = value.toShort

  /**
   * Converts this <code>PosZFloat</code> to a <code>Char</code>.
   */
  def toChar: Char = value.toChar

  /**
   * Converts this <code>PosZFloat</code> to an <code>Int</code>.
   */
  def toInt: Int = value.toInt

  /**
   * Converts this <code>PosZFloat</code> to a <code>Long</code>.
   */
  def toLong: Long = value.toLong

  /**
   * Converts this <code>PosZFloat</code> to a <code>Float</code>.
   */
  def toFloat: Float = value.toFloat

  /**
   * Converts this <code>PosZFloat</code> to a <code>Double</code>.
   */
  def toDouble: Double = value.toDouble

  /** Returns this value, unmodified. */
  def unary_+ : PosZFloat = this
  /** Returns the negation of this value. */
  def unary_- : Float = -value

  /**
   * Converts this <code>PosZFloat</code>'s value to a string then concatenates the given string.
   */
  def +(x: String): String = value + x

  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Byte): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Short): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Char): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Int): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Long): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Float): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Double): Boolean = value < x

  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Byte): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Short): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Char): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Int): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Long): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Float): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Double): Boolean = value <= x

  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Byte): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Short): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Char): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Int): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Long): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Float): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Double): Boolean = value > x

  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Byte): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Short): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Char): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Int): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Long): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Float): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Double): Boolean = value >= x

  /** Returns the sum of this value and `x`. */
  def +(x: Byte): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double = value + x

  /**
   * Returns the <code>PosZFloat</code> sum of this value and `x`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive or zero Float to another positive or zero Float
   * will always result in another positive or zero Float
   * value (though the result may be positive infinity).
   * </p>
   */
  def plus(x: PosZFloat): PosZFloat = PosZFloat.ensuringValid(value + x)

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double = value - x

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Short): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Char): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Int): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Long): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Float): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double = value * x

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double = value / x

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double = value % x

  // Stuff from RichFloat
  def isPosInfinity: Boolean = Float.PositiveInfinity == value

  /**
  * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
  */
  def max(that: PosZFloat): PosZFloat = if (math.max(value, that.value) == value) this else that

  /**
  * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
  */
  def min(that: PosZFloat): PosZFloat = if (math.min(value, that.value) == value) this else that

  /**
   * Indicates whether this `PosZFloat` has a value that is a whole number: it is finite and it has no fraction part.
   */
  def isWhole = {
    val longValue = value.toLong
    longValue.toFloat == value || longValue == Long.MaxValue && value < Float.PositiveInfinity || longValue == Long.MinValue && value > Float.NegativeInfinity
  }

  /**
   * Rounds this `PosZFloat` value to the nearest whole number value that can be expressed as an `Int`, returning the result as a `PosZInt`.
   */
  def round: PosZInt = PosZInt.ensuringValid(math.round(value))

  /**
   * Returns the smallest (closest to 0) `PosZFloat` that is greater than or equal to this `PosZFloat`
   * and represents a mathematical integer.
   */
  def ceil: PosZFloat = PosZFloat.ensuringValid(math.ceil(value).toFloat)

  /**
   * Returns the greatest (closest to positive infinity) `PosZFloat` that is less than or equal to
   * this `PosZFloat` and represents a mathematical integer.
   */
  def floor: PosZFloat = PosZFloat.ensuringValid(math.floor(value).toFloat)

  /** Converts an angle measured in degrees to an approximately equivalent
  * angle measured in radians.
  *
  * @return the measurement of the angle x in radians.
  */
  def toRadians: PosZFloat = PosZFloat.ensuringValid(math.toRadians(value).toFloat)

  /** Converts an angle measured in radians to an approximately equivalent
  * angle measured in degrees.
  * @return the measurement of the angle x in degrees.
  */
  def toDegrees: PosZFloat = PosZFloat.ensuringValid(math.toDegrees(value).toFloat)

  // adapted from RichInt:
  /**
  * Create a <code>Range</code> from this <code>PosZFloat</code> value
  * until the specified <code>end</code> (exclusive) with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Float, NumericRange[Float]]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Float): Range.Partial[Float, NumericRange[Float]] =
    value.until(end)

  /**
  * Create a <code>Range</code> from this <code>PosZFloat</code> value
  * until the specified <code>end</code> (exclusive) with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Exclusive[Float]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Float, step: Float): NumericRange.Exclusive[Float] =
    value.until(end, step)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosZFloat</code> value
  * to the specified <code>end</code> with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Float, NumericRange[Float]]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Float): Range.Partial[Float, NumericRange[Float]] =
    value.to(end)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosZFloat</code> value
  * to the specified <code>end</code> with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Inclusive[Float]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Float, step: Float): NumericRange.Inclusive[Float] =
    value.to(end, step)

  /**
   * Applies the passed <code>Float =&gt; Float</code> function to the underlying <code>Float</code>
   * value, and if the result is positive or zero, returns the result wrapped in a <code>PosZFloat</code>,
   * else throws <code>AssertionError</code>.
   *
   * <p>
   * This method will inspect the result of applying the given function to this
   * <code>PosZFloat</code>'s underlying <code>Float</code> value and if the result
   * is positive or zero, <em>i.e.</em>, a value greater
   * than or equal to <code>0.0f</code>, it will return a <code>PosZFloat</code> representing that value.
   * Otherwise, the <code>Float</code> value returned by the given function is
   * negative, so this method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises an <code>Float</code> is positive or zero. 
   * With this method, you are asserting that you are convinced the result of
   * the computation represented by applying the given function to this <code>PosZFloat</code>'s
   * value will not produce a negative number, including <code>Float.NegativeInfinity</code>, or <code>Float.NaN</code>.
   * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
   * </p>
   *
   * @param f the <code>Float =&gt; Float</code> function to apply to this <code>PosZFloat</code>'s
   *     underlying <code>Float</code> value.
   * @return the result of applying this <code>PosZFloat</code>'s underlying <code>Float</code> value to
   *     to the passed function, wrapped in a <code>PosZFloat</code> if it is positive or zero (else throws <code>AssertionError</code>).
   * @throws AssertionError if the result of applying this <code>PosZFloat</code>'s underlying <code>Float</code> value to
   *     to the passed function is not positive or zero.
   */
  def ensuringValid(f: Float => Float): PosZFloat = {
    val candidateResult: Float = f(value)
    if (PosZFloatMacro.isValid(candidateResult)) new PosZFloat(candidateResult)
    else throw new AssertionError(s"$candidateResult, the result of applying the passed function to $value, was not a valid PosZFloat")
  }
}

/**
 * The companion object for <code>PosZFloat</code> that offers
 * factory methods that produce <code>PosZFloat</code>s, implicit
 * widening conversions from <code>PosZFloat</code> to other
 * numeric types, and maximum and minimum constant values for
 * <code>PosZFloat</code>.
 */
object PosZFloat {
  /**
   * The largest value representable as a non-negative <code>Float</code>,
   * which is <code>PosZFloat(3.4028235E38)</code>.
   */
  final val MaxValue: PosZFloat = PosZFloat.ensuringValid(Float.MaxValue)

  /**
   * The smallest value representable as a non-negative <code>Float</code>,
   * which is <code>PosZFloat(0.0F)</code>.
   */
  final val MinValue: PosZFloat = PosZFloat.ensuringValid(0.0f) // Can't use the macro here

  /**
   * The smallest value representable as a positive
   * <code>Float</code>, which is <code>PosFloat(1.4E-45)</code>.
   */
  final val MinPositiveValue: PosZFloat = PosZFloat.ensuringValid(Float.MinPositiveValue) // Can't use the macro here

  /**
   * The positive infinity value, which is <code>PosZFloat.ensuringValid(Float.PositiveInfinity)</code>.
   */
  final val PositiveInfinity: PosZFloat = PosZFloat.ensuringValid(Float.PositiveInfinity) // Can't use the macro here

  /**
   * A factory method that produces an <code>Option[PosZFloat]</code> given a
   * <code>Float</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Float</code> value
   * and if it is a non-negative <code>Float</code>,
   * <em>i.e.</em>, a value greater than or equal to 0, it will
   * return a <code>PosZFloat</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed
   * <code>Float</code> value is negative, so this method
   * will return <code>None</code>.
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
   * @param value the <code>Float</code> to inspect, and if non-negative, return
   *     wrapped in a <code>Some[PosZFloat]</code>.
   * @return the specified <code>Float</code> value wrapped
   *     in a <code>Some[PosZFloat]</code>, if it is positive, else
   *     <code>None</code>.
   */
  def from(value: Float): Option[PosZFloat] =
    if (PosZFloatMacro.isValid(value)) Some(new PosZFloat(value)) else None

  /**
   * A factory/assertion method that produces an <code>PosZFloat</code> given a
   * valid <code>Float</code> value, or throws <code>AssertionError</code>,
   * if given an invalid <code>Float</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Float</code> value
   * and if it is a non-negative <code>Float</code>,
   * <em>i.e.</em>, a value greater than or equal to 0, it will
   * return a <code>PosZFloat</code> representing that value.
   * Otherwise, the passed * <code>Float</code> value is negative, so this method
   * will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Float</code> literals at
   * compile time, whereas <code>from</code> inspects
   * <code>Float</code> values at run time.
   * It differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises a <code>Float</code> is positive or zero.
   * </p>
   *
   * @param value the <code>Float</code> to inspect, and if non-negative, return
   *     wrapped in a <code>PosZFloat</code>.
   * @return the specified <code>Float</code> value wrapped
   *     in a <code>PosZFloat</code>, if it is positive, else
   *     throws <code>AssertionError</code>.
   * @throws AssertionError if the passed value is not zero or positive
   */
  def ensuringValid(value: Float): PosZFloat =
    if (PosZFloatMacro.isValid(value)) new PosZFloat(value) else {
      throw new AssertionError(s"$value was not a valid PosZFloat")
    }

  /**
   * A predicate method that returns true if a given 
   * <code>Float</code> value is positive or zero.
   *
   * @param value the <code>Float</code> to inspect, and if positive or zero, return true.
   * @return true if the specified <code>Float</code> is positive or zero, else false.
   */
  def isValid(value: Float): Boolean = PosZFloatMacro.isValid(value)

  /**
   * A factory method that produces a <code>PosZFloat</code> given a
   * <code>Float</code> value and a default <code>PosZFloat</code>.
   *
   * <p>
   * This method will inspect the passed <code>Float</code> value and if
   * it is a positive or zero <code>Float</code>, <em>i.e.</em>, a value greater
   * than or equal to 0.0f, it will return a <code>PosZFloat</code> representing that value.
   * Otherwise, the passed <code>Float</code> value is negative, so this
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
   * @param value the <code>Float</code> to inspect, and if positive or zero, return.
   * @param default the <code>PosZFloat</code> to return if the passed
   *     <code>Float</code> value is not positive or zero.
   * @return the specified <code>Float</code> value wrapped in a
   *     <code>PosZFloat</code>, if it is positive or zero, else the
   *     <code>default</code> <code>PosZFloat</code> value.
   */
  def fromOrElse(value: Float, default: => PosZFloat): PosZFloat =
    if (PosZFloatMacro.isValid(value)) new PosZFloat(value) else default

  import language.experimental.macros
  import scala.language.implicitConversions

  /**
   * A factory method, implemented via a macro, that produces a
   * <code>PosZFloat</code> if passed a valid <code>Float</code>
   * literal, otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the
   * specified <code>Float</code> expression at compile time. If
   * the expression is a non-negative <code>Float</code> literal,
   * <em>i.e.</em>, with a value greater than or equal to 0, it will return
   * a <code>PosZFloat</code> representing that value.  Otherwise,
   * the passed <code>Float</code> expression is either a literal
   * that is negative, or is not a literal, so this method
   * will give a compiler error.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>from</code>
   * factory method in that this method is implemented via a
   * macro that inspects <code>Float</code> literals at compile
   * time, whereas <code>from</code> inspects <code>Float</code>
   * values at run time.
   * </p>
   *
   * @param value the <code>Float</code> literal expression to inspect at
   *     compile time, and if non-negative, to return wrapped in a
   *     <code>PosZFloat</code> at run time.
   * @return the specified, valid <code>Float</code> literal
   *     value wrapped in a <code>PosZFloat</code>. (If the
   *     specified expression is not a valid <code>Float</code>
   *     literal, the invocation of this method will not
   *     compile.)
   */
  implicit def apply(value: Float): PosZFloat = macro PosZFloatMacro.apply

  /**
   * Implicit widening conversion from <code>PosZFloat</code> to
   * <code>Float</code>.
   *
   * @param pos the <code>PosZFloat</code> to widen
   * @return the <code>Float</code> value underlying the specified
   *     <code>PosZFloat</code>.
   */
  implicit def widenToFloat(poz: PosZFloat): Float = poz.value

  /**
   * Implicit widening conversion from <code>PosZFloat</code> to
   * <code>Double</code>.
   *
   * @param pos the <code>PosZFloat</code> to widen
   * @return the <code>Float</code> value underlying the specified
   *     <code>PosZFloat</code>, widened to <code>Double</code>.
   */
  implicit def widenToDouble(poz: PosZFloat): Double = poz.value

  /**
   * Implicit widening conversion from <code>PosZFloat</code> to
   * <code>PosZDouble</code>.
   *
   * @param pos the <code>PosZFloat</code> to widen
   * @return the <code>Float</code> value underlying the specified
   *     <code>PosZFloat</code>, widened to <code>Double</code>
   *     and wrapped in a <code>PosZDouble</code>.
   */
  implicit def widenToPosZDouble(poz: PosZFloat): PosZDouble = PosZDouble.ensuringValid(poz.value)

  /**
   * Returns the <code>PosZFloat</code> sum of the passed <code>PosZFloat</code> values `x` and `y`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive or zero Float to another positive or zero Float
   * will always result in another positive or zero Float
   * value (though the result may be positive infinity).
   * </p>
   *
   * <p>
   * This overloaded form of the method is used when there are just two arguments so that
   * boxing is avoided. The overloaded <code>sumOf</code> that takes a varargs of
   * <code>PosZFloat</code> starting at the third parameter can sum more than two
   * values, but will entail boxing and may therefore be less efficient.
   * </p>
   */
  def sumOf(x: PosZFloat, y: PosZFloat): PosZFloat = PosZFloat.ensuringValid(x.value + y.value)

  /**
   * Returns the <code>PosZFloat</code> sum of the passed <code>PosZFloat</code> values `first` and 
   * value `second`, and the <code>PosZFloat</code> values passed as varargs `rest`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive or zero Float to another positive or zero Float
   * will always result in another positive or zero Float
   * value (though the result may be positive infinity).
   * </p>
   *
   * <p>
   * This overloaded form of the <code>sumOf</code> method can sum more than two
   * values, but unlike its two-arg sibling, will entail boxing.
   * </p>
   */
  def sumOf(first: PosZFloat, second: PosZFloat, rest: PosZFloat*): PosZFloat =
    PosZFloat.ensuringValid(first.value + second.value + rest.map(_.value).sum)


  /**
   * Implicit Ordering instance.
   */
  implicit val posZFloatOrd: Ordering[PosZFloat] =
    new Ordering[PosZFloat] {
      def compare(x: PosZFloat, y: PosZFloat): Int = x.toFloat.compare(y)
    } 
}

