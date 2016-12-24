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
 * An <code>AnyVal</code> for non-negative <code>Double</code>s.
 *
 * <p>
 * Note: A <code>PosZDouble</code> may have value <code>Double.PositiveInfinity</code>,
 * but cannot have value <code>Double.NegativeInfinity</code> or <code>Double.NaN</code>.
 * </p>
 *
 * <p>
 * Because <code>PosZDouble</code> is an <code>AnyVal</code> it will usually be
 * as efficient as an <code>Double</code>, being boxed only when a
 * <code>Double</code> would have been boxed.
 * </p>
 * 
 * <p>
 * The <code>PosZDouble.apply</code> factory method is
 * implemented in terms of a macro that checks literals for
 * validity at compile time. Calling
 * <code>PosZDouble.apply</code> with a literal
 * <code>Double</code> value will either produce a valid
 * <code>PosZDouble</code> instance at run time or an error at
 * compile time. Here's an example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import anyvals._
 * import anyvals._
 *
 * scala&gt; PosZDouble(1.1)
 * res0: org.scalactic.anyvals.PosZDouble = PosZDouble(1.1)
 *
 * scala&gt; PosZDouble(0.0)
 * res1: org.scalactic.anyvals.PosZDouble = PosZDouble(0.0)
 *
 * scala&gt; PosZDouble(-1.1)
 * &lt;console&gt;:14: error: PosZDouble.apply can only be invoked on a non-negative (i &gt;= 0.0) floating point literal, like PosZDouble(42.0).
 *               PosZDouble(-1.1)
 *                         ^
 * </pre>
 *
 * <p>
 * <code>PosZDouble.apply</code> cannot be used if the value
 * being passed is a variable (<em>i.e.</em>, not a literal),
 * because the macro cannot determine the validity of variables
 * at compile time (just literals). If you try to pass a
 * variable to <code>PosZDouble.apply</code>, you'll get a
 * compiler error that suggests you use a different factor
 * method, <code>PosZDouble.from</code>, instead:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val x = 1.1
 * x: Double = 1.1
 *
 * scala&gt; PosZDouble(x)
 * &lt;console&gt;:15: error: PosZDouble.apply can only be invoked on a floating point literal, like PosZDouble(42.0). Please use PosZDouble.from instead.
 *               PosZDouble(x)
 *                         ^
 * </pre>
 *
 * <p>
 * The <code>PosZDouble.from</code> factory method will inspect
 * the value at runtime and return an
 * <code>Option[PosZDouble]</code>. If the value is valid,
 * <code>PosZDouble.from</code> will return a
 * <code>Some[PosZDouble]</code>, else it will return a
 * <code>None</code>.  Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; PosZDouble.from(x)
 * res4: Option[org.scalactic.anyvals.PosZDouble] = Some(PosZDouble(1.1))
 *
 * scala&gt; val y = -1.1
 * y: Double = -1.1
 *
 * scala&gt; PosZDouble.from(y)
 * res5: Option[org.scalactic.anyvals.PosZDouble] = None
 * </pre>
 * 
 * <p>
 * The <code>PosZDouble.apply</code> factory method is marked implicit, so that
 * you can pass literal <code>Double</code>s into methods that require
 * <code>PosZDouble</code>, and get the same compile-time checking you get when
 * calling <code>PosZDouble.apply</code> explicitly. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; def invert(pos: PosZDouble): Double = Double.MaxValue - pos
 * invert: (pos: org.scalactic.anyvals.PosZDouble)Double
 *
 * scala&gt; invert(0.0)
 * res6: Double = 1.7976931348623157E308
 *
 * scala&gt; invert(Double.MaxValue)
 * res7: Double = 0.0
 *
 * scala&gt; invert(-1.1)
 * &lt;console&gt;:15: error: PosZDouble.apply can only be invoked on a non-negative (i &gt;= 0.0) floating point literal, like PosZDouble(42.0).
 *               invert(-1.1)
 *                       ^
 * </pre>
 *
 * <p>
 * This example also demonstrates that the
 * <code>PosZDouble</code> companion object also defines
 * implicit widening conversions when a similar conversion is
 * provided in Scala. This makes it convenient to use a
 * <code>PosZDouble</code> where a <code>Double</code> or wider
 * type is needed. An example is the subtraction in the body of
 * the <code>invert</code> method defined above,
 * <code>Double.MaxValue - pos</code>. Although
 * <code>Double.MaxValue</code> is a <code>Double</code>, which
 * has no <code>-</code> method that takes a
 * <code>PosZDouble</code> (the type of <code>pos</code>), you
 * can still subtract <code>pos</code>, because the
 * <code>PosZDouble</code> will be implicitly widened to
 * <code>Double</code>.
 * </p>
 *
 * @param value The <code>Double</code> value underlying this
 *              <code>PosZDouble</code>.
 */ 
final class PosZDouble private (val value: Double) extends AnyVal {

  /**
   * A string representation of this <code>PosZDouble</code>.
   */
  override def toString: String = s"PosZDouble($value)"

  /**
   * Converts this <code>PosZDouble</code> to a <code>Byte</code>.
   */
  def toByte: Byte = value.toByte

  /**
   * Converts this <code>PosZDouble</code> to a <code>Short</code>.
   */
  def toShort: Short = value.toShort

  /**
   * Converts this <code>PosZDouble</code> to a <code>Char</code>.
   */
  def toChar: Char = value.toChar

  /**
   * Converts this <code>PosZDouble</code> to an <code>Int</code>.
   */
  def toInt: Int = value.toInt

  /**
   * Converts this <code>PosZDouble</code> to a <code>Long</code>.
   */
  def toLong: Long = value.toLong

  /**
   * Converts this <code>PosZDouble</code> to a <code>Float</code>.
   */
  def toFloat: Float = value.toFloat

  /**
   * Converts this <code>PosZDouble</code> to a <code>Double</code>.
   */
  def toDouble: Double = value.toDouble

  /** Returns this value, unmodified. */
  def unary_+ : PosZDouble = this
  /** Returns the negation of this value. */
  def unary_- : Double = -value

  /**
   * Converts this <code>PosZDouble</code>'s value to a string then concatenates the given string.
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
  def +(x: Byte): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double = value + x

  /**
   * Returns the <code>PosZDouble</code> sum of this value and `x`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive or zero Double to another positive or zero Double
   * will always result in another positive or zero Double
   * value (though the result may be positive infinity).
   * </p>
   */
  def plus(x: PosZDouble): PosZDouble = PosZDouble.ensuringValid(value + x)

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double = value - x

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Short): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Char): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Int): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Long): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Float): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double = value * x

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double = value / x

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double = value % x

  // Stuff from RichDouble
  def isPosInfinity: Boolean = Double.PositiveInfinity == value

  /**
  * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
  */
  def max(that: PosZDouble): PosZDouble = if (math.max(value, that.value) == value) this else that

  /**
  * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
  */
  def min(that: PosZDouble): PosZDouble = if (math.min(value, that.value) == value) this else that

  /**
   * Indicates whether this `PosZDouble` has a value that is a whole number: it is finite and it has no fraction part.
   */
  def isWhole = {
    val longValue = value.toLong
    longValue.toDouble == value || longValue == Long.MaxValue && value < Double.PositiveInfinity || longValue == Long.MinValue && value > Double.NegativeInfinity
  }

  /**
   * Rounds this `PosZDouble` value to the nearest whole number value that can be expressed as a `Long`, returning the result as a `PosZLong`.
   */
  def round: PosZLong = PosZLong.ensuringValid(math.round(value))

  /**
   * Returns the smallest (closest to 0) `PosZDouble` that is greater than or equal to this `PosZDouble`
   * and represents a mathematical integer.
   */
  def ceil: PosZDouble = PosZDouble.ensuringValid(math.ceil(value))

  /**
   * Returns the greatest (closest to positive infinity) `PosZDouble` that is less than or equal to
   * this `PosZDouble` and represents a mathematical integer.
   */
  def floor: PosZDouble = PosZDouble.ensuringValid(math.floor(value))

  /**
  * Converts an angle measured in degrees to an approximately equivalent
  * angle measured in radians.
  *
  * @return the measurement of the angle x in radians.
  */
  def toRadians: Double = math.toRadians(value)

  /** Converts an angle measured in radians to an approximately equivalent
  * angle measured in degrees.
  * @return the measurement of the angle x in degrees.
  */
  def toDegrees: Double = math.toDegrees(value)

  // adapted from RichInt:
  /**
  * Create a <code>Range</code> from this <code>PosZDouble</code> value
  * until the specified <code>end</code> (exclusive) with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Double, NumericRange[Double]]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Double): Range.Partial[Double, NumericRange[Double]] =
    value.until(end)

  /**
  * Create a <code>Range</code> from this <code>PosZDouble</code> value
  * until the specified <code>end</code> (exclusive) with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Exclusive[Double]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Double, step: Double): NumericRange.Exclusive[Double] =
    value.until(end, step)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosZDouble</code> value
  * to the specified <code>end</code> with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Double, NumericRange[Double]]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Double): Range.Partial[Double, NumericRange[Double]] =
    value.to(end)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosZDouble</code> value
  * to the specified <code>end</code> with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Inclusive[Double]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Double, step: Double): NumericRange.Inclusive[Double] =
    value.to(end, step)

  /**
   * Applies the passed <code>Double =&gt; Double</code> function to the underlying <code>Double</code>
   * value, and if the result is positive or zero, returns the result wrapped in a <code>PosZDouble</code>,
   * else throws <code>AssertionError</code>.
   *
   * Note: you should use this method only when you are convinced that it will
   * always succeed, i.e., never throw an exception. It is good practice to
   * add a comment near the invocation of this method indicating ''why'' you think
   * it will always succeed to document your reasoning. If you are not sure an
   * `ensuringValid` call will always succeed, you should use a different method
   * on this class that returns a `Double` result instead.
   *
   * <p>
   * This method will inspect the result of applying the given function to this
   * <code>PosZDouble</code>'s underlying <code>Double</code> value and if the result
   * is positive or zero, <em>i.e.</em>, a value greater
   * than or equal to <code>0.0</code>, it will return a <code>PosZDouble</code> representing that value.
   * Otherwise, the <code>Double</code> value returned by the given function is
   * negative, so this method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises an <code>Double</code> is positive or zero. 
   * With this method, you are asserting that you are convinced the result of
   * the computation represented by applying the given function to this <code>PosZDouble</code>'s
   * value will not produce a negative number, including <code>Double.NegativeInfinity</code>, or <code>Double.NaN</code>.
   * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
   * </p>
   *
   * @param f the <code>Double =&gt; Double</code> function to apply to this <code>PosZDouble</code>'s
   *     underlying <code>Double</code> value.
   * @return the result of applying this <code>PosZDouble</code>'s underlying <code>Double</code> value to
   *     to the passed function, wrapped in a <code>PosZDouble</code> if it is positive or zero (else throws <code>AssertionError</code>).
   * @throws AssertionError if the result of applying this <code>PosZDouble</code>'s underlying <code>Double</code> value to
   *     to the passed function is not positive or zero.
   */
  def ensuringValid(f: Double => Double): PosZDouble = {
    val candidateResult: Double = f(value)
    if (PosZDoubleMacro.isValid(candidateResult)) new PosZDouble(candidateResult)
    else throw new AssertionError(s"$candidateResult, the result of applying the passed function to $value, was not a valid PosZDouble")
  }
}

/**
 * The companion object for <code>PosZDouble</code> that offers
 * factory methods that produce <code>PosZDouble</code>s, implicit
 * widening conversions from <code>PosZDouble</code> to other
 * numeric types, and maximum and minimum constant values for
 * <code>PosZDouble</code>.
 */
object PosZDouble {
  /**
   * The largest value representable as a non-negative <code>Double</code>,
   * which is <code>PosZDouble(1.7976931348623157E308)</code>.
   */
  final val MaxValue: PosZDouble = PosZDouble.ensuringValid(Double.MaxValue)

  /**
   * The smallest value representable as a non-negative <code>Double</code>,
   * which is <code>PosZDouble(0.0)</code>.
   */
  final val MinValue: PosZDouble = PosZDouble.ensuringValid(0.0) // Can't use the macro here

  /**
   * The smallest value representable as a positive
   * <code>Double</code>, which is <code>PosDouble(4.9E-324)</code>.
   */
  final val MinPositiveValue: PosZDouble = PosZDouble.ensuringValid(Double.MinPositiveValue) // Can't use the macro here

  /**
   * The positive infinity value, which is <code>PosZDouble.ensuringValid(Double.PositiveInfinity)</code>.
   */
  final val PositiveInfinity: PosZDouble = PosZDouble.ensuringValid(Double.PositiveInfinity) // Can't use the macro here

  /**
   * A factory method that produces an <code>Option[PosZDouble]</code> given a
   * <code>Double</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Double</code> value
   * and if it is a non-negative <code>Double</code>,
   * <em>i.e.</em>, a value greater than or equal to 0, it will
   * return a <code>PosZDouble</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed
   * <code>Double</code> value is negative, so this method
   * will return <code>None</code>.
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
   * @param value the <code>Double</code> to inspect, and if
   *     non-negative, return wrapped in a
   *     <code>Some[PosZDouble]</code>.
   * @return the specified <code>Double</code> value wrapped
   *     in a <code>Some[PosZDouble]</code>, if it is positive, else
   *     <code>None</code>.
   */
  def from(value: Double): Option[PosZDouble] =
    if (PosZDoubleMacro.isValid(value)) Some(new PosZDouble(value)) else None

  /**
   * A factory/assertion method that produces an <code>PosZDouble</code> given a
   * valid <code>Double</code> value, or throws <code>AssertionError</code>,
   * if given an invalid <code>Double</code> value.
   *
   * Note: you should use this method only when you are convinced that it will
   * always succeed, i.e., never throw an exception. It is good practice to
   * add a comment near the invocation of this method indicating ''why'' you think
   * it will always succeed to document your reasoning. If you are not sure an
   * `ensuringValid` call will always succeed, you should use one of the other
   * factory or validation methods provided on this object instead: `isValid`, 
   * `tryingValid`, `passOrElse`, `goodOrElse`, or `rightOrElse`.
   *
   * <p>
   * This method will inspect the passed <code>Double</code> value
   * and if it is a non-negative <code>Double</code>,
   * <em>i.e.</em>, a value greater than or equal to 0, it will
   * return a <code>PosZDouble</code> representing that value.
   * Otherwise, the passed <code>Double</code> value is negative,
   * so this method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Double</code> literals at
   * compile time, whereas this method inspects
   * <code>Double</code> values at run time.
   * It differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises a <code>Double</code> is positive or zero.
   * </p>
   *
   * @param value the <code>Double</code> to inspect, and if
   *     non-negative, return wrapped in a
   *     <code>PosZDouble</code>.
   * @return the specified <code>Double</code> value wrapped
   *     in a <code>PosZDouble</code>, if it is positive, else
   *     throws <code>AssertionError</code>.
   * @throws AssertionError if the passed value is not zero or positive
   */
  def ensuringValid(value: Double): PosZDouble =
    if (PosZDoubleMacro.isValid(value)) new PosZDouble(value) else {
      throw new AssertionError(s"$value was not a valid PosZDouble")
    }

  /**
   * A predicate method that returns true if a given 
   * <code>Double</code> value is positive or zero.
   *
   * @param value the <code>Double</code> to inspect, and if positive or zero, return true.
   * @return true if the specified <code>Double</code> is positive or zero, else false.
   */
  def isValid(value: Double): Boolean = PosZDoubleMacro.isValid(value)

  /**
   * A factory method that produces a <code>PosZDouble</code> given a
   * <code>Double</code> value and a default <code>PosZDouble</code>.
   *
   * <p>
   * This method will inspect the passed <code>Double</code> value and if
   * it is a positive or zero <code>Double</code>, <em>i.e.</em>, a value greater
   * than or equal to 0.0, it will return a <code>PosZDouble</code> representing that value.
   * Otherwise, the passed <code>Double</code> value is negative, so this
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
   * @param value the <code>Double</code> to inspect, and if positive or zero, return.
   * @param default the <code>PosZDouble</code> to return if the passed
   *     <code>Double</code> value is not positive or zero.
   * @return the specified <code>Double</code> value wrapped in a
   *     <code>PosZDouble</code>, if it is positive or zero, else the
   *     <code>default</code> <code>PosZDouble</code> value.
   */
  def fromOrElse(value: Double, default: => PosZDouble): PosZDouble =
    if (PosZDoubleMacro.isValid(value)) new PosZDouble(value) else default

  import language.experimental.macros
  import scala.language.implicitConversions

  /**
   * A factory method, implemented via a macro, that produces a
   * <code>PosZDouble</code> if passed a valid <code>Double</code>
   * literal, otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the
   * specified <code>Double</code> expression at compile time. If
   * the expression is a non-negative <code>Double</code> literal,
   * <em>i.e.</em>, with a value greater than or equal to 0, it will return
   * a <code>PosZDouble</code> representing that value.  Otherwise,
   * the passed <code>Double</code> expression is either a literal
   * that is negative, or is not a literal, so this method
   * will give a compiler error.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>from</code>
   * factory method in that this method is implemented via a
   * macro that inspects <code>Double</code> literals at compile
   * time, whereas <code>from</code> inspects <code>Double</code>
   * values at run time.
   * </p>
   *
   * @param value the <code>Double</code> literal expression to inspect at
   *     compile time, and if non-negative, to return wrapped in a
   *     <code>PosZDouble</code> at run time.
   * @return the specified, valid <code>Double</code> literal
   *     value wrapped in a <code>PosZDouble</code>. (If the
   *     specified expression is not a valid <code>Double</code>
   *     literal, the invocation of this method will not
   *     compile.)
   */
  implicit def apply(value: Double): PosZDouble = macro PosZDoubleMacro.apply

  /**
   * Implicit widening conversion from <code>PosZDouble</code> to
   * <code>Double</code>.
   *
   * @param pos the <code>PosZDouble</code> to widen
   * @return the <code>Double</code> value underlying the specified
   *     <code>PosZDouble</code>.
   */
  implicit def widenToDouble(poz: PosZDouble): Double = poz.value

  /**
   * Returns the <code>PosZDouble</code> sum of the passed <code>PosZDouble</code> values `x` and `y`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive or zero Double to another positive or zero Double
   * will always result in another positive or zero Double
   * value (though the result may be positive infinity).
   * </p>
   *
   * <p>
   * This overloaded form of the method is used when there are just two arguments so that
   * boxing is avoided. The overloaded <code>sumOf</code> that takes a varargs of
   * <code>PosZDouble</code> starting at the third parameter can sum more than two
   * values, but will entail boxing and may therefore be less efficient.
   * </p>
   */
  def sumOf(x: PosZDouble, y: PosZDouble): PosZDouble = PosZDouble.ensuringValid(x.value + y.value)

  /**
   * Returns the <code>PosZDouble</code> sum of the passed <code>PosZDouble</code> values `first` and 
   * value `second`, and the <code>PosZDouble</code> values passed as varargs `rest`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive or zero Double to another positive or zero Double
   * will always result in another positive or zero Double
   * value (though the result may be positive infinity).
   * </p>
   *
   * <p>
   * This overloaded form of the <code>sumOf</code> method can sum more than two
   * values, but unlike its two-arg sibling, will entail boxing.
   * </p>
   */
  def sumOf(first: PosZDouble, second: PosZDouble, rest: PosZDouble*): PosZDouble =
    PosZDouble.ensuringValid(first.value + second.value + rest.map(_.value).sum)

  /**
   * Implicit Ordering instance.
   */
  implicit val posZDoubleOrd: Ordering[PosZDouble] =
    new Ordering[PosZDouble] {
      def compare(x: PosZDouble, y: PosZDouble): Int = x.toDouble.compare(y)
    } 
}

