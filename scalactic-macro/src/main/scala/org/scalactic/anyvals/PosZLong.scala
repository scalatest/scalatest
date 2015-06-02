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
 * An <code>AnyVal</code> for non-negative <code>Long</code>s.
 *
 * <p>
 * Because <code>PosZLong</code> is an <code>AnyVal</code> it will usually be
 * as efficient as an <code>Long</code>, being boxed only when an
 * <code>Long</code> would have been boxed.
 * </p>
 * 
 * <p>
 * The <code>PosZLong.apply</code> factory method is implemented in terms of a
 * macro that checks literals for validity at compile time. Calling
 * <code>PosZLong.apply</code> with a literal <code>Long</code> value will
 * either produce a valid <code>PosZLong</code> instance at run time or an
 * error at compile time. Here's an example:
 * </p>
 * 
 * <pre>
 * scala&gt; import anyvals._
 * import anyvals._
 *
 * scala&gt; PosZLong(1L)
 * res0: org.scalactic.anyvals.PosZLong = PosZLong(1)
 *
 * scala&gt; PosZLong(0L)
 * res1: org.scalactic.anyvals.PosZLong = PosZLong(0)
 *
 * scala&gt; PosZLong(-1L)
 * &lt;console&gt;:14: error: PosZLong.apply can only be invoked on a non-negative (i &gt;= 0L) integer literal, like PosZLong(42L).
 *               PosZLong(-1L)
 *                       ^
 * </pre>
 *
 * <p>
 * <code>PosZLong.apply</code> cannot be used if the value being passed is a
 * variable (<em>i.e.</em>, not a literal), because the macro cannot determine
 * the validity of variables at compile time (just literals). If you try to
 * pass a variable to <code>PosZLong.apply</code>, you'll get a compiler error
 * that suggests you use a different factor method, <code>PosZLong.from</code>,
 * instead:
 * </p>
 *
 * <pre>
 * scala&gt; val x = 1L
 * x: Long = 1
 *
 * scala&gt; PosZLong(x)
 * &lt;console&gt;:15: error: PosZLong.apply can only be invoked on an integer literal, like PosZLong(42L). Please use PosZLong.from instead.
 *               PosZLong(x)
 *                       ^
 * </pre>
 *
 * <p>
 * The <code>PosZLong.from</code> factory method will inspect the value at runtime and return an <code>Option[PosZLong]</code>. If
 * the value is valid, <code>PosZLong.from</code> will return a <code>Some[PosZLong]</code>, else it will return a <code>None</code>.
 * Here's an example:
 * </p>
 *
 * <pre>
 * scala&gt; PosZLong.from(x)
 * res4: Option[org.scalactic.anyvals.PosZLong] = Some(PosZLong(1))
 *
 * scala&gt; val y = -1L
 * y: Long = -1
 *
 * scala&gt; PosZLong.from(y)
 * res5: Option[org.scalactic.anyvals.PosZLong] = None
 * </pre>
 * 
 * <p>
 * The <code>PosZLong.apply</code> factory method is marked implicit, so that
 * you can pass literal <code>Long</code>s into methods that require
 * <code>PosZLong</code>, and get the same compile-time checking you get when
 * calling <code>PosZLong.apply</code> explicitly. Here's an example:
 * </p>
 *
 * <pre>
 * scala&gt; def invert(pos: PosZLong): Long = Long.MaxValue - pos
 * invert: (pos: org.scalactic.anyvals.PosZLong)Long
 *
 * scala&gt; invert(0L)
 * res6: Long = 9223372036854775807
 *
 * scala&gt; invert(Long.MaxValue)
 * res7: Long = 0
 *
 * scala&gt; invert(-1L)
 * &lt;console&gt;:15: error: PosZLong.apply can only be invoked on a non-negative (i &gt;= 0L) integer literal, like PosZLong(42L).
 *               invert(-1L)
 *                       ^
 * </pre>
 *
 * <p>
 * This example also demonstrates that the <code>PosZLong</code>
 * companion object also defines implicit widening conversions
 * when either no loss of precision will occur or a similar
 * conversion is provided in Scala. (For example, the implicit
 * conversion from <code>Long</code> to </code>Double</code> in
 * Scala can lose precision.) This makes it convenient to use a
 * <code>PosZLong</code> where a <code>Long</code> or wider type
 * is needed. An example is the subtraction in the body of the
 * <code>invert</code> method defined above, <code>Long.MaxValue
 * - pos</code>. Although <code>Long.MaxValue</code> is a
 * <code>Long</code>, which has no <code>-</code> method that
 * takes a <code>PosZLong</code> (the type of <code>pos</code>),
 * you can still subtract <code>pos</code>, because the
 * <code>PosZLong</code> will be implicitly widened to
 * <code>Long</code>.
 * </p>
 *
 * @param value The <code>Long</code> value underlying this <code>PosZLong</code>.
 */ 
final class PosZLong private (val value: Long) extends AnyVal {

  /**
   * A string representation of this <code>PosZLong</code>.
   */
  override def toString: String = s"PosZLong($value)"

  /**
   * Converts this <code>PosZLong</code> to a <code>Byte</code>.
   */
  def toByte: Byte = value.toByte

  /**
   * Converts this <code>PosZLong</code> to a <code>Short</code>.
   */
  def toShort: Short = value.toShort

  /**
   * Converts this <code>PosZLong</code> to a <code>Char</code>.
   */
  def toChar: Char = value.toChar

  /**
   * Converts this <code>PosZLong</code> to an <code>Int</code>.
   */
  def toInt: Int = value.toInt

  /**
   * Converts this <code>PosZLong</code> to a <code>Long</code>.
   */
  def toLong: Long = value.toLong

  /**
   * Converts this <code>PosZLong</code> to a <code>Float</code>.
   */
  def toFloat: Float = value.toFloat

  /**
   * Converts this <code>PosZLong</code> to a <code>Double</code>.
   */
  def toDouble: Double = value.toDouble

 /**
 * Returns the bitwise negation of this value.
 * @example {{{
 * ~5 == -6
 * // in binary: ~00000101 ==
 * //             11111010
 * }}}
 */
  def unary_~ : Long = ~value
  /** Returns this value, unmodified. */
  def unary_+ : PosZLong = this
  /** Returns the negation of this value. */
  def unary_- : Long = -value

  /**
   * Converts this <code>PosZLong</code>'s value to a string then concatenates the given string.
   */
  def +(x: String): String = value + x

  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: Int): Long = value << x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: Long): Long = value << x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: Int): Long = value >>> x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: Long): Long = value >>> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: Int): Long = value >> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: Long): Long = value >> x

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

  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Byte): Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Short): Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Char): Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Int): Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Long): Long = value | x

  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Byte): Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Short): Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Char): Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Int): Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Long): Long = value & x

  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Byte): Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Short): Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Char): Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Int): Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Long): Long = value ^ x

  /** Returns the sum of this value and `x`. */
  def +(x: Byte): Long = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Long = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Long = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Long = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Long = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double = value + x

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Long = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Long = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Long = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Long = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Long = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double = value - x

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Long = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Short): Long = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Char): Long = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Int): Long = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Long): Long = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Float): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double = value * x

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Long = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Long = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Long = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Long = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Long = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double = value / x

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Long = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Long = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Long = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Long = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Long = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double = value % x

  // Stuff from RichLong:
  /**
   * Returns a string representation of this <code>PosLong</code>'s underlying <code>Long</code>
   * as an unsigned integer in base&nbsp;2.
   *
   * <p>
   * The unsigned <code>long</code> value is this <code>PosLong</code>'s underlying <code>Long</code> plus
   * 2<sup>64</sup> if the underlying <code>Long</code> is negative; otherwise, it is
   * equal to the underlying <code>Long</code>.  This value is converted to a string of
   * ASCII digits in binary (base&nbsp;2) with no extra leading
   * <code>0</code>s.  If the unsigned magnitude is zero, it is
   * represented by a single zero character <code>'0'</code>
   * (<code>'&#92;u0030'</code>); otherwise, the first character of
   * the representation of the unsigned magnitude will not be the
   * zero character. The characters <code>'0'</code>
   * (<code>'&#92;u0030'</code>) and <code>'1'</code>
   * (<code>'&#92;u0031'</code>) are used as binary digits.
   * </p>
   *
   * @return  the string representation of the unsigned <code>long</code>
   *          value represented by this <code>PosLong</code>'s underlying <code>Long</code> in binary (base&nbsp;2).
   */
  def toBinaryString: String = java.lang.Long.toBinaryString(value)

  /**
   * Returns a string representation of this <code>PosLong</code>'s underlying <code>Long</code>
   * as an unsigned integer in base&nbsp;16.
   *
   * <p>
   * The unsigned <code>long</code> value is this <code>PosLong</code>'s underlying <code>Long</code> plus
   * 2<sup>64</sup> if the underlying <code>Long</code> is negative; otherwise, it is
   * equal to the underlying <code>Long</code>.  This value is converted to a string of
   * ASCII digits in hexadecimal (base&nbsp;16) with no extra
   * leading <code>0</code>s.  If the unsigned magnitude is zero, it
   * is represented by a single zero character <code>'0'</code>
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
   * <p>
   * These are the characters <code>'&#92;u0030'</code> through
   * <code>'&#92;u0039'</code> and  <code>'&#92;u0061'</code> through
   * <code>'&#92;u0066'</code>.  If uppercase letters are desired,
   * the <code>toUpperCase</code> method may be called
   * on the result.
   * </p>
   *
   * @return  the string representation of the unsigned <code>long</code>
   *          value represented by this <code>PosLong</code>'s underlying <code>Long</code> in hexadecimal
   *          (base&nbsp;16).
   */
  def toHexString: String = java.lang.Long.toHexString(value)
  def toOctalString: String = java.lang.Long.toOctalString(value)

  // No point to call abs on a PosZLong.
  /**
  * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
  */
  def max(that: PosZLong): PosZLong = if (math.max(value, that.value) == value) this else that

  /**
  * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
  */
  def min(that: PosZLong): PosZLong = if (math.min(value, that.value) == value) this else that

  // adapted from RichInt:
  /**
  * Create a <code>Range</code> from this <code>PosZLong</code> value
  * until the specified <code>end</code> (exclusive) with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.NumericRange.Exclusive[Long]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Long): NumericRange.Exclusive[Long] = value.until(end)

  /**
  * Create a <code>Range</code> from this <code>PosZLong</code> value
  * until the specified <code>end</code> (exclusive) with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Exclusive[Long]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Long, step: Long): NumericRange.Exclusive[Long] =
    value.until(end, step)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosZLong</code> value
  * to the specified <code>end</code> with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.NumericRange.Inclusive[Long]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Long): NumericRange.Inclusive[Long] = value.to(end)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosZLong</code> value
  * to the specified <code>end</code> with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Inclusive[Long]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Long, step: Long): NumericRange.Inclusive[Long] =
    value.to(end, step)
}

/**
 * The companion object for <code>PosZLong</code> that offers
 * factory methods that produce <code>PosZLong</code>s, implicit
 * widening conversions from <code>PosZLong</code> to other
 * numeric types, and maximum and minimum constant values for
 * <code>PosZLong</code>.
 */
object PosZLong {
  /**
   * The largest value representable as a non-negative <code>Long</code>,
   * which is <code>PosZLong(9223372036854775807)</code>.
   */
  final val MaxValue: PosZLong = PosZLong.from(Long.MaxValue).get

  /**
   * The smallest value representable as a non-negative <code>Long</code>,
   * which is <code>PosZLong(0)</code>.
   */
  final val MinValue: PosZLong = PosZLong.from(0L).get // Can't use the macro here

  /**
   * A factory method that produces an <code>Option[PosZLong]</code> given a
   * <code>Long</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Long</code> value
   * and if it is a non-negative <code>Long</code>,
   * <em>i.e.</em>, a value greater than or equal to 0, it will
   * return a <code>PosZLong</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed
   * <code>Long</code> value is negative, so this method
   * will return <code>None</code>.
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
   * @param value the <code>Long</code> to inspect, and if non-negative, return
   *     wrapped in a <code>Some[PosZLong]</code>.
   * @return the specified <code>Long</code> value wrapped
   *     in a <code>Some[PosZLong]</code>, if it is positive, else
   *     <code>None</code>.
   */
  def from(value: Long): Option[PosZLong] =
    if (value >= 0L) Some(new PosZLong(value)) else None

  import language.experimental.macros
  import scala.language.implicitConversions

  /**
   * A factory method, implemented via a macro, that produces a
   * <code>PosZLong</code> if passed a valid <code>Long</code>
   * literal, otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the
   * specified <code>Long</code> expression at compile time. If
   * the expression is a non-negative <code>Long</code> literal,
   * <em>i.e.</em>, with a value greater than or equal to 0, it will return
   * a <code>PosZLong</code> representing that value.  Otherwise,
   * the passed <code>Long</code> expression is either a literal
   * that is negative, or is not a literal, so this method
   * will give a compiler error.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>from</code>
   * factory method in that this method is implemented via a
   * macro that inspects <code>Long</code> literals at compile
   * time, whereas <code>from</code> inspects <code>Long</code>
   * values at run time.
   * </p>
   *
   * @param value the <code>Long</code> literal expression to inspect at
   *     compile time, and if non-negative, to return wrapped in a
   *     <code>PosZLong</code> at run time.
   * @return the specified, valid <code>Long</code> literal value wrapped
   *     in a <code>PosZLong</code>. (If the specified expression is not a valid
   *     <code>Long</code> literal, the invocation of this method will not
   *     compile.)
   */
  implicit def apply(value: Long): PosZLong = macro PosZLongMacro.apply

  /**
   * Implicit widening conversion from <code>PosZLong</code> to
   * <code>Long</code>.
   *
   * @param pos the <code>PosZLong</code> to widen
   * @return the <code>Long</code> value underlying the specified
   *     <code>PosZLong</code>.
   */
  implicit def widenToLong(poz: PosZLong): Long = poz.value

  /**
   * Implicit widening conversion from <code>PosZLong</code> to
   * <code>Float</code>.
   *
   * @param pos the <code>PosZLong</code> to widen
   * @return the <code>Long</code> value underlying the specified
   *     <code>PosZLong</code>, widened to <code>Float</code>.
   */
  implicit def widenToFloat(poz: PosZLong): Float = poz.value

  /**
   * Implicit widening conversion from <code>PosZLong</code> to
   * <code>Double</code>.
   *
   * @param pos the <code>PosZLong</code> to widen
   * @return the <code>Long</code> value underlying the specified
   *     <code>PosZLong</code>, widened to <code>Double</code>.
   */
  implicit def widenToDouble(poz: PosZLong): Double = poz.value

  /**
   * Implicit widening conversion from <code>PosZLong</code> to
   * <code>PosZFloat</code>.
   *
   * @param pos the <code>PosZLong</code> to widen
   * @return the <code>Long</code> value underlying the specified
   *     <code>PosZLong</code>, widened to <code>Float</code> and
   *     wrapped in a <code>PosZFloat</code>.
   */
  implicit def widenToPosZFloat(poz: PosZLong): PosZFloat = PosZFloat.from(poz.value).get

  /**
   * Implicit widening conversion from <code>PosZLong</code> to
   * <code>PosZDouble</code>.
   *
   * @param pos the <code>PosZLong</code> to widen
   * @return the <code>Long</code> value underlying the specified
   *     <code>PosZLong</code>, widened to <code>Double</code>
   *     and wrapped in a <code>PosZDouble</code>.
   */
  implicit def widenToPosZDouble(poz: PosZLong): PosZDouble = PosZDouble.from(poz.value).get
}
