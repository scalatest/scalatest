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

import scala.language.implicitConversions
import scala.collection.immutable.NumericRange

/**
 * An <code>AnyVal</code> for positive <code>Float</code>s.
 *
 * Note: a <code>PosFloat</code> may not equal 0. If you want positive
 * number or 0, use [[PosZFloat]].
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
 * <pre>
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
 * <pre>
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
 * <pre>
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
 * <pre>
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

  def isWhole = {
    val longValue = value.toLong
    longValue.toFloat == value || longValue == Long.MaxValue && value < Float.PositiveInfinity || longValue == Long.MinValue && value > Float.NegativeInfinity
  }

  def round: PosZInt = {
    import scala.util.Try
    import scala.util.Success
    import scala.util.Failure
    val roundedInt: Int = math.round(value)
    val result = Try(PosZInt.from(math.round(value)).get)
    result match {
      case Failure(ex) => println("PosZInt round failed")
        println(s"value was $value")
        println(s"result was $result")
        throw ex
      case Success(v) => v
    }
    // PosZInt.from(math.round(value)).get // Also could be zero.
  }
  def ceil: PosFloat = PosFloat.from(math.ceil(value.toDouble).toFloat).get // I think this one is safe, but try NaN
  def floor: PosZFloat = PosZFloat.from(math.floor(value.toDouble).toFloat).get // Could be zero.

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
  final val MaxValue: PosFloat = PosFloat.from(Float.MaxValue).get

  /**
   * The smallest value representable as a positive
   * <code>Float</code>, which is <code>PosFloat(1.4E-45)</code>.
   */
  final val MinValue: PosFloat = PosFloat.from(Math.nextAfter(0.0F, 1.0F)).get // Can't use the macro here

  /**
   * A factory method that produces an <code>Option[PosFloat]</code> given a
   * <code>Float</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Float</code> value and if
   * it is a positive <code>Float</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosFloat</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed <code>Float</code>
   * value is 0.0 or negative, so this method will return <code>None</code>.
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
    if (value > 0.0F) Some(new PosFloat(value)) else None

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
   * that is 0.0 or negative, or is not a literal, so this method
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
  implicit def widenToPosDouble(pos: PosFloat): PosDouble = PosDouble.from(pos.value).get

  /**
   * Implicit widening conversion from <code>PosFloat</code> to
   * <code>PosZFloat</code>.
   *
   * @param pos the <code>PosFloat</code> to widen
   * @return the <code>Float</code> value underlying the
   * specified <code>PosFloat</code> wrapped in a
   * <code>PosZFloat</code>.
   */
  implicit def widenToPosZFloat(pos: PosFloat): PosZFloat = PosZFloat.from(pos.value).get

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
  implicit def widenToPosZDouble(pos: PosFloat): PosZDouble = PosZDouble.from(pos.value).get
}

