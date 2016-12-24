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
import scala.language.implicitConversions

/**
 * An <code>AnyVal</code> for positive <code>Float</code>s.
 *
 * <p>
 * Note: a <code>PosFloat</code> may not equal 0. If you want positive
 * number or 0, use [[PosZFloat]].
 * A <code>PosFloat</code> may have value <code>Float.PositiveInfinity</code>,
 * but cannot have value <code>Float.NegativeInfinity</code> or <code>Float.NaN</code>.
 * </p>
 *
 * <p>
 * Because <code>PosFloat</code> is an <code>AnyVal</code> it
 * will usually be as efficient as an <code>Float</code>, being
 * boxed only when an <code>Float</code> would have been boxed.
 * </p>
 *
 * <p>
 * The <code>PosFloat.apply</code> factory method is implemented
 * in terms of a macro that checks literals for validity at
 * compile time. Calling <code>PosFloat.apply</code> with a
 * literal <code>Float</code> value will either produce a valid
 * <code>PosFloat</code> instance at run time or an error at
 * compile time. Here's an example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import anyvals._
 * import anyvals._
 *
 * scala&gt; PosFloat(1.0F)
 * res0: org.scalactic.anyvals.PosFloat = PosFloat(1.0)
 *
 * scala&gt; PosFloat(0.0F)
 * &lt;console&gt;:14: error: PosFloat.apply can only be invoked on a positive (i &gt; 0.0F) floating point literal, like PosFloat(42.0F).
 *               PosFloat(0.0F)
 *                       ^
 * </pre>
 *
 * <p>
 * <code>PosFloat.apply</code> cannot be used if the value being
 * passed is a variable (<em>i.e.</em>, not a literal), because
 * the macro cannot determine the validity of variables at
 * compile time (just literals). If you try to pass a variable
 * to <code>PosFloat.apply</code>, you'll get a compiler error
 * that suggests you use a different factor method,
 * <code>PosFloat.from</code>, instead:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val x = 1.0F
 * x: Float = 1.0
 *
 * scala&gt; PosFloat(x)
 * &lt;console&gt;:15: error: PosFloat.apply can only be invoked on a floating point literal, like PosFloat(42.0F). Please use PosFloat.from instead.
 *               PosFloat(x)
 *                       ^
 * </pre>
 *
 * <p>
 * The <code>PosFloat.from</code> factory method will inspect
 * the value at runtime and return an
 * <code>Option[PosFloat]</code>. If the value is valid,
 * <code>PosFloat.from</code> will return a
 * <code>Some[PosFloat]</code>, else it will return a
 * <code>None</code>.  Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; PosFloat.from(x)
 * res3: Option[org.scalactic.anyvals.PosFloat] = Some(PosFloat(1.0))
 *
 * scala&gt; val y = 0.0F
 * y: Float = 0.0
 *
 * scala&gt; PosFloat.from(y)
 * res4: Option[org.scalactic.anyvals.PosFloat] = None
 * </pre>
 * 
 * <p>
 * The <code>PosFloat.apply</code> factory method is marked
 * implicit, so that you can pass literal <code>Float</code>s
 * into methods that require <code>PosFloat</code>, and get the
 * same compile-time checking you get when calling
 * <code>PosFloat.apply</code> explicitly. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; def invert(pos: PosFloat): Float = Float.MaxValue - pos
 * invert: (pos: org.scalactic.anyvals.PosFloat)Float
 *
 * scala&gt; invert(1.1F)
 * res5: Float = 3.4028235E38
 *
 * scala&gt; invert(Float.MaxValue)
 * res6: Float = 0.0
 *
 * scala&gt; invert(0.0F)
 * &lt;console&gt;:15: error: PosFloat.apply can only be invoked on a positive (i &gt; 0.0F) floating point literal, like PosFloat(42.0F).
 *               invert(0.0F)
 *                      ^
 *
 * scala&gt; invert(-1.1F)
 * &lt;console&gt;:15: error: PosFloat.apply can only be invoked on a positive (i &gt; 0.0F) floating point literal, like PosFloat(42.0F).
 *               invert(-1.1F)
 *                       ^
 *
 * </pre>
 *
 * <p>
 * This example also demonstrates that the <code>PosFloat</code>
 * companion object also defines implicit widening conversions
 * when no loss of precision will occur. This makes it convenient to use a
 * <code>PosFloat</code> where a <code>Float</code> or wider
 * type is needed. An example is the subtraction in the body of
 * the <code>invert</code> method defined above,
 * <code>Float.MaxValue - pos</code>. Although
 * <code>Float.MaxValue</code> is a <code>Float</code>, which
 * has no <code>-</code> method that takes a
 * <code>PosFloat</code> (the type of <code>pos</code>), you can
 * still subtract <code>pos</code>, because the
 * <code>PosFloat</code> will be implicitly widened to
 * <code>Float</code>.
 * </p>
 *
 * @param value The <code>Float</code> value underlying this <code>PosFloat</code>.
 */ 
final class PosFloat private (val value: Float) extends AnyVal {

  /**
   * A string representation of this <code>PosFloat</code>.
   */
  override def toString: String = s"PosFloat($value)"

  /**
   * Converts this <code>PosFloat</code> to a <code>Byte</code>.
   */
  def toByte: Byte = value.toByte

  /**
   * Converts this <code>PosFloat</code> to a <code>Short</code>.
   */
  def toShort: Short = value.toShort

  /**
   * Converts this <code>PosFloat</code> to a <code>Char</code>.
   */
  def toChar: Char = value.toChar

  /**
   * Converts this <code>PosFloat</code> to an <code>Int</code>.
   */
  def toInt: Int = value.toInt

  /**
   * Converts this <code>PosFloat</code> to a <code>Long</code>.
   */
  def toLong: Long = value.toLong

  /**
   * Converts this <code>PosFloat</code> to a <code>Float</code>.
   */
  def toFloat: Float = value.toFloat

  /**
   * Converts this <code>PosFloat</code> to a <code>Double</code>.
   */
  def toDouble: Double = value.toDouble

  /** Returns this value, unmodified. */
  def unary_+ : PosFloat = this
  /** Returns the negation of this value. */
  def unary_- : Float = -value

  /**
   * Converts this <code>PosFloat</code>'s value to a string then concatenates the given string.
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
   * Returns the <code>PosFloat</code> sum of this <code>PosFloat</code>'s value and the given <code>PosZFloat</code> value.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive Float and zero or a positive Float and another
   * positive Float will always result in another positive Float
   * value (though the result may be positive infinity).
   * </p>
   */
  def plus(x: PosZFloat): PosFloat = PosFloat.ensuringValid(value + x.value)

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
  def max(that: PosFloat): PosFloat = if (math.max(value, that.value) == value) this else that

  /**
  * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
  */
  def min(that: PosFloat): PosFloat = if (math.min(value, that.value) == value) this else that

  /**
   * Indicates whether this `PosFloat` has a value that is a whole number: it is finite and it has no fraction part.
   */
  def isWhole = {
    val longValue = value.toLong
    longValue.toFloat == value || longValue == Long.MaxValue && value < Float.PositiveInfinity || longValue == Long.MinValue && value > Float.NegativeInfinity
  }

  /**
   * Rounds this `PosFloat` value to the nearest whole number value that can be expressed as an `Int`, returning the result as a `PosZInt`.
   */
  def round: PosZInt = PosZInt.ensuringValid(math.round(value))
/*
  // Why did I do this?
  def round: PosZInt = {
    import scala.util.Failure
    import scala.util.Success
    import scala.util.Try
    val roundedInt: Int = math.round(value)
    val result = Try(PosZInt.ensuringValid(math.round(value)))
    result match {
      case Failure(ex) => println("PosZInt round failed")
        println(s"value was $value")
        println(s"result was $result")
        throw ex
      case Success(v) => v
    }
  }
*/

  /**
   * Returns the smallest (closest to 0) `PosFloat` that is greater than or equal to this `PosFloat`
   * and represents a mathematical integer.
   */
  def ceil: PosFloat = PosFloat.ensuringValid(math.ceil(value.toDouble).toFloat) // I think this one is safe, but try NaN

  /**
   * Returns the greatest (closest to positive infinity) `PosFloat` that is less than or equal to
   * this `PosFloat` and represents a mathematical integer.
   */
  def floor: PosZFloat = PosZFloat.ensuringValid(math.floor(value.toDouble).toFloat) // Could be zero.

  /**
  * Converts an angle measured in degrees to an approximately equivalent
  * angle measured in radians.
  *
  * @return the measurement of the angle x in radians.
  */
  def toRadians: Float = math.toRadians(value.toDouble).toFloat

  /**
  * Converts an angle measured in radians to an approximately equivalent
  * angle measured in degrees.
  * @return the measurement of the angle x in degrees.
  */
  def toDegrees: Float = math.toDegrees(value.toDouble).toFloat

  // adapted from RichInt:
  /**
  * Create a <code>Range</code> from this <code>PosFloat</code> value
  * until the specified <code>end</code> (exclusive) with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Float, NumericRange[Float]]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Float): Range.Partial[Float, NumericRange[Float]] =
    value.until(end)

  /**
  * Create a <code>Range</code> (exclusive) from this <code>PosFloat</code> value
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
  * Create an inclusive <code>Range</code> from this <code>PosFloat</code> value
  * to the specified <code>end</code> with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Float], NumericRange[Float]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Float): Range.Partial[Float, NumericRange[Float]] =
    value.to(end)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosFloat</code> value
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
   * value, and if the result is positive, returns the result wrapped in a <code>PosFloat</code>,
   * else throws <code>AssertionError</code>.
   *
   * <p>
   * This method will inspect the result of applying the given function to this
   * <code>PosFloat</code>'s underlying <code>Float</code> value and if the result
   * is greater than <code>0.0f</code>, it will return a <code>PosFloat</code> representing that value.
   * Otherwise, the <code>Float</code> value returned by the given function is
   * <code>0.0f</code> or negative, so this method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises an <code>Float</code> is positive. 
   * With this method, you are asserting that you are convinced the result of
   * the computation represented by applying the given function to this <code>PosFloat</code>'s
   * value will not produce zero, a negative number, including <code>Float.NegativeInfinity</code>, or <code>Float.NaN</code>.
   * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
   * </p>
   *
   * @param f the <code>Float =&gt; Float</code> function to apply to this <code>PosFloat</code>'s
   *     underlying <code>Float</code> value.
   * @return the result of applying this <code>PosFloat</code>'s underlying <code>Float</code> value to
   *     to the passed function, wrapped in a <code>PosFloat</code> if it is positive (else throws <code>AssertionError</code>).
   * @throws AssertionError if the result of applying this <code>PosFloat</code>'s underlying <code>Float</code> value to
   *     to the passed function is not positive.
   */
  def ensuringValid(f: Float => Float): PosFloat = {
    val candidateResult: Float = f(value)
    if (PosFloatMacro.isValid(candidateResult)) new PosFloat(candidateResult)
    else throw new AssertionError(s"$candidateResult, the result of applying the passed function to $value, was not a valid PosFloat")
  }
}

/**
 * The companion object for <code>PosFloat</code> that offers
 * factory methods that produce <code>PosFloat</code>s,
 * implicit widening conversions from <code>PosFloat</code> to
 * other numeric types, and maximum and minimum constant values
 * for <code>PosFloat</code>.
 */
object PosFloat {
  /**
   * The largest value representable as a positive <code>Float</code>,
   * which is <code>PosFloat(3.4028235E38)</code>.
   */
  final val MaxValue: PosFloat = PosFloat.ensuringValid(Float.MaxValue)

  /**
   * The smallest value representable as a positive
   * <code>Float</code>, which is <code>PosFloat(1.4E-45)</code>.
   */
  final val MinValue: PosFloat = PosFloat.ensuringValid(Float.MinPositiveValue) // Can't use the macro here

  /**
   * The positive infinity value, which is <code>PosFloat.ensuringValid(Float.PositiveInfinity)</code>.
   */
  final val PositiveInfinity: PosFloat = PosFloat.ensuringValid(Float.PositiveInfinity) // Can't use the macro here

  /**
   * A factory method that produces an <code>Option[PosFloat]</code> given a
   * <code>Float</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Float</code> value and if
   * it is a positive <code>Float</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosFloat</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed <code>Float</code>
   * value is 0.0f or negative, so this method will return <code>None</code>.
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
   * @param value the <code>Float</code> to inspect, and if positive, return
   *     wrapped in a <code>Some[PosFloat]</code>.
   * @return the specified <code>Float</code> value wrapped in a
   *     <code>Some[PosFloat]</code>, if it is positive, else
   *     <code>None</code>.
   */
  def from(value: Float): Option[PosFloat] =
    if (PosFloatMacro.isValid(value)) Some(new PosFloat(value)) else None

  /**
   * A factory/assertion method that produces a <code>PosFloat</code> given a
   * valid <code>Float</code> value, or throws <code>AssertionError</code>,
   * if given an invalid <code>Float</code> value.
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
   * This method will inspect the passed <code>Float</code> value and if
   * it is a positive <code>Float</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosFloat</code> representing that value.
   * Otherwise, the passed <code>Float</code> value is 0.0f or negative, so
   * this method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Float</code> literals at
   * compile time, whereas this method inspects
   * <code>Float</code> values at run time.
   * It differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises a <code>Float</code> is positive.
   * </p>
   *
   * @param value the <code>Float</code> to inspect, and if positive, return
   *     wrapped in a <code>PosFloat</code>.
   * @return the specified <code>Float</code> value wrapped in a
   *     <code>PosFloat</code>, if it is positive, else
   *     throws <code>AssertionError</code>.
   * @throws AssertionError if the passed value is not positive
   */
  def ensuringValid(value: Float): PosFloat =
    if (PosFloatMacro.isValid(value)) new PosFloat(value) else {
      throw new AssertionError(s"$value was not a valid PosFloat")
    }

  /**
   * A predicate method that returns true if a given 
   * <code>Float</code> value is positive.
   *
   * @param value the <code>Float</code> to inspect, and if positive, return true.
   * @return true if the specified <code>Float</code> is positive, else false.
   */
  def isValid(value: Float): Boolean = PosFloatMacro.isValid(value)

  /**
   * A factory method that produces a <code>PosFloat</code> given a
   * <code>Float</code> value and a default <code>PosFloat</code>.
   *
   * <p>
   * This method will inspect the passed <code>Float</code> value and if
   * it is a positive <code>Float</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosFloat</code> representing that value.
   * Otherwise, the passed <code>Float</code> value is 0.0f or negative, so this
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
    if (PosFloatMacro.isValid(value)) new PosFloat(value) else default

  import language.experimental.macros
  import scala.language.implicitConversions

  /**
   * A factory method, implemented via a macro, that produces a
   * <code>PosFloat</code> if passed a valid <code>Float</code>
   * literal, otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the
   * specified <code>Float</code> expression at compile time. If
   * the expression is a positive <code>Float</code> literal,
   * <em>i.e.</em>, with a value greater than 0.0, it will return
   * a <code>PosFloat</code> representing that value.  Otherwise,
   * the passed <code>Float</code> expression is either a literal
   * that is 0.0f or negative, or is not a literal, so this method
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
   * @param value the <code>Float</code> literal expression to
   *     inspect at compile time, and if positive, to return
   *     wrapped in a <code>PosFloat</code> at run time.
   * @return the specified, valid <code>Float</code> literal
   *     value wrapped in a <code>PosFloat</code>. (If the
   *     specified expression is not a valid <code>Float</code>
   *     literal, the invocation of this method will not
   *     compile.)
   */
  implicit def apply(value: Float): PosFloat = macro PosFloatMacro.apply

  /**
   * Implicit widening conversion from <code>PosFloat</code> to
  * <code>Float</code>.
  *
  * @param pos the <code>PosFloat</code> to widen
  * @return the <code>Float</code> value underlying the
  *     specified <code>PosFloat</code>
  */
 implicit def widenToFloat(pos: PosFloat): Float = pos.value

 /**
  * Implicit widening conversion from <code>PosFloat</code> to
   * <code>Double</code>.
   *
   * @param pos the <code>PosFloat</code> to widen
   * @return the <code>Float</code> value underlying the
   *     specified <code>PosFloat</code>, widened to
   *     <code>Double</code>.
   */
  implicit def widenToDouble(pos: PosFloat): Double = pos.value

  /**
   * Implicit widening conversion from <code>PosFloat</code> to
   * <code>PosDouble</code>.
   *
   * @param pos the <code>PosFloat</code> to widen
   * @return the <code>Float</code> value underlying the
   *     specified <code>PosFloat</code>, widened to
   *     <code>Double</code> and wrapped in a
   *     <code>PosDouble</code>.
   */
  implicit def widenToPosDouble(pos: PosFloat): PosDouble = PosDouble.ensuringValid(pos.value)

  /**
   * Implicit widening conversion from <code>PosFloat</code> to
   * <code>PosZFloat</code>.
   *
   * @param pos the <code>PosFloat</code> to widen
   * @return the <code>Float</code> value underlying the
   * specified <code>PosFloat</code> wrapped in a
   * <code>PosZFloat</code>.
   */
  implicit def widenToPosZFloat(pos: PosFloat): PosZFloat = PosZFloat.ensuringValid(pos.value)

  /**
   * Implicit widening conversion from <code>PosFloat</code> to
   * <code>PosZDouble</code>.
   *
   * @param pos the <code>PosFloat</code> to widen
   * @return the <code>Float</code> value underlying the
   *     specified <code>PosFloat</code>, widened to
   *     <code>Double</code> and wrapped in a
   *     <code>PosZDouble</code>.
   */
  implicit def widenToPosZDouble(pos: PosFloat): PosZDouble = PosZDouble.ensuringValid(pos.value)

  /**
   * Returns the <code>PosFloat</code> sum of the passed <code>PosFloat</code> value `x` and <code>PosZFloat</code> value `y`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive Float and zero or a positive Float and another
   * positive Float will always result in another positive Float
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
  def sumOf(x: PosFloat, y: PosZFloat): PosFloat = PosFloat.ensuringValid(x.value + y.value)

  /**
   * Returns the <code>PosFloat</code> sum of the passed <code>PosFloat</code> value `first`, the <code>PosZFloat</code>
   * value `second`, and the <code>PosFloat</code> values passed as varargs `rest`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive Float and one or more zeros or positive Floats
   * will always result in another positive Float
   * value (though the result may be positive infinity).
   * </p>
   *
   * <p>
   * This overloaded form of the <code>sumOf</code> method can sum more than two
   * values, but unlike its two-arg sibling, will entail boxing.
   * </p>
   */
  def sumOf(first: PosFloat, second: PosZFloat, rest: PosZFloat*): PosFloat =
    PosFloat.ensuringValid(first.value + second.value + rest.map(_.value).sum)

  /**
   * Implicit Ordering instance.
   */
  implicit val posFloatOrd: Ordering[PosFloat] =
    new Ordering[PosFloat] {
      def compare(x: PosFloat, y: PosFloat): Int = x.toFloat.compare(y)
    } 
}

