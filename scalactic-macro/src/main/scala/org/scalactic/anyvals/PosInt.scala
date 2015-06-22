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
import scala.collection.immutable.Range

/**
 * An <code>AnyVal</code> for positive <code>Int</code>s.
 *
 * Note: a <code>PosInt</code> may not equal 0. If you want positive
 * number or 0, use [[PosZInt]].
 *
 * <p>
 * Because <code>PosInt</code> is an <code>AnyVal</code> it will usually be
 * as efficient as an <code>Int</code>, being boxed only when an <code>Int</code>
 * would have been boxed.
 * </p>
 * 
 * <p>
 * The <code>PosInt.apply</code> factory method is implemented in terms of a macro that
 * checks literals for validity at compile time. Calling <code>PosInt.apply</code> with
 * a literal <code>Int</code> value will either produce a valid <code>PosInt</code> instance
 * at run time or an error at compile time. Here's an example:
 * </p>
 * 
 * <pre>
 * scala&gt; import anyvals._
 * import anyvals._
 *
 * scala&gt; PosInt(1)
 * res0: org.scalactic.anyvals.PosInt = PosInt(1)
 *
 * scala&gt; PosInt(0)
 * &lt;console&gt;:14: error: PosInt.apply can only be invoked on a positive (i &gt; 0) integer literal, like PosInt(42).
 *               PosInt(0)
 *                     ^
 * </pre>
 *
 * <p>
 * <code>PosInt.apply</code> cannot be used if the value being passed is a variable (<em>i.e.</em>, not a literal), because
 * the macro cannot determine the validity of variables at compile time (just literals). If you try to pass a variable
 * to <code>PosInt.apply</code>, you'll get a compiler error that suggests you use a different factor method,
 * <code>PosInt.from</code>, instead:
 * </p>
 *
 * <pre>
 * scala&gt; val x = 1
 * x: Int = 1
 *
 * scala&gt; PosInt(x)
 * &lt;console&gt;:15: error: PosInt.apply can only be invoked on an integer literal, like PosInt(42). Please use PosInt.from instead.
 *               PosInt(x)
 *                     ^
 * </pre>
 *
 * <p>
 * The <code>PosInt.from</code> factory method will inspect the value at runtime and return an <code>Option[PosInt]</code>. If
 * the value is valid, <code>PosInt.from</code> will return a <code>Some[PosInt]</code>, else it will return a <code>None</code>.
 * Here's an example:
 * </p>
 *
 * <pre>
 * scala&gt; PosInt.from(x)
 * res3: Option[org.scalactic.anyvals.PosInt] = Some(PosInt(1))
 *
 * scala&gt; val y = 0
 * y: Int = 0
 *
 * scala&gt; PosInt.from(y)
 * res4: Option[org.scalactic.anyvals.PosInt] = None
 * </pre>
 * 
 * <p>
 * The <code>PosInt.apply</code> factory method is marked implicit, so that you can pass literal <code>Int</code>s
 * into methods that require <code>PosInt</code>, and get the same compile-time checking you get when calling
 * <code>PosInt.apply</code> explicitly. Here's an example:
 * </p>
 *
 * <pre>
 * scala&gt; def invert(pos: PosInt): Int = Int.MaxValue - pos
 * invert: (pos: org.scalactic.anyvals.PosInt)Int
 *
 * scala&gt; invert(1)
 * res0: Int = 2147483646
 *
 * scala&gt; invert(Int.MaxValue)
 * res1: Int = 0
 *
 * scala&gt; invert(0)
 * &lt;console&gt;:15: error: PosInt.apply can only be invoked on a positive (i &gt; 0) integer literal, like PosInt(42).
 *               invert(0)
 *                      ^
 *
 * scala&gt; invert(-1)
 * &lt;console&gt;:15: error: PosInt.apply can only be invoked on a positive (i &gt; 0) integer literal, like PosInt(42).
 *               invert(-1)
 *                       ^
 *
 * </pre>
 *
 * <p>
 * This example also demonstrates that the <code>PosInt</code> companion object also defines implicit widening conversions
 * when either no loss of precision will occur or a similar conversion is provided in Scala. (For example, the implicit
 * conversion from <code>Int</code> to </code>Float</code> in Scala can lose precision.) This makes it convenient to
 * use a <code>PosInt</code> where an <code>Int</code> or wider type is needed. An example is the subtraction in the body
 * of the <code>invert</code> method defined above, <code>Int.MaxValue - pos</code>. Although <code>Int.MaxValue</code> is
 * an <code>Int</code>, which has no <code>-</code> method that takes a <code>PosInt</code> (the type of <code>pos</code>),
 * you can still subtract <code>pos</code>, because the <code>PosInt</code> will be implicitly widened to <code>Int</code>.
 * </p>
 *
 * @param value The <code>Int</code> value underlying this <code>PosInt</code>.
 */ 
final class PosInt private (val value: Int) extends AnyVal {

  /**
   * A string representation of this <code>PosInt</code>.
   */
  override def toString: String = s"PosInt($value)"

  /**
   * Converts this <code>PosInt</code> to a <code>Byte</code>.
   */
  def toByte: Byte = value.toByte

  /**
   * Converts this <code>PosInt</code> to a <code>Short</code>.
   */
  def toShort: Short = value.toShort

  /**
   * Converts this <code>PosInt</code> to a <code>Char</code>.
   */
  def toChar: Char = value.toChar

  /**
   * Converts this <code>PosInt</code> to an <code>Int</code>.
   */
  def toInt: Int = value.toInt

  /**
   * Converts this <code>PosInt</code> to a <code>Long</code>.
   */
  def toLong: Long = value.toLong

  /**
   * Converts this <code>PosInt</code> to a <code>Float</code>.
   */
  def toFloat: Float = value.toFloat

  /**
   * Converts this <code>PosInt</code> to a <code>Double</code>.
   */
  def toDouble: Double = value.toDouble
  /**
  * Returns the bitwise negation of this value.
  * @example {{{
  * ~5 == -6
  * // in binary: ~00000101 ==
  * // 11111010
  * }}}
  */
  def unary_~ : Int = ~value
  /** Returns this value, unmodified. */
  def unary_+ : PosInt = this
  /** Returns the negation of this value. */
  def unary_- : Int = -value
  /**
   * Converts this <code>PosInt</code>'s value to a string then concatenates the given string.
   */
  def +(x: String): String = value + x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  * filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: Int): Int = value << x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  * filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: Long): Int = value << x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  * filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * // 00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: Int): Int = value >>> x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  * filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * // 00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: Long): Int = value >>> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  * filling in the right bits with the same value as the left-most bit of this.
  * The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * // 11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: Int): Int = value >> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  * filling in the right bits with the same value as the left-most bit of this.
  * The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * // 11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: Long): Int = value >> x
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
  * // in binary: 11110000
  * // | 10101010
  * // --------
  * // 11111010
  * }}}
  */
  def |(x: Byte): Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary: 11110000
  * // | 10101010
  * // --------
  * // 11111010
  * }}}
  */
  def |(x: Short): Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary: 11110000
  * // | 10101010
  * // --------
  * // 11111010
  * }}}
  */
  def |(x: Char): Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary: 11110000
  * // | 10101010
  * // --------
  * // 11111010
  * }}}
  */
  def |(x: Int): Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary: 11110000
  * // | 10101010
  * // --------
  * // 11111010
  * }}}
  */
  def |(x: Long): Long = value | x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary: 11110000
  * // & 10101010
  * // --------
  * // 10100000
  * }}}
  */
  def &(x: Byte): Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary: 11110000
  * // & 10101010
  * // --------
  * // 10100000
  * }}}
  */
  def &(x: Short): Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary: 11110000
  * // & 10101010
  * // --------
  * // 10100000
  * }}}
  */
  def &(x: Char): Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary: 11110000
  * // & 10101010
  * // --------
  * // 10100000
  * }}}
  */
  def &(x: Int): Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary: 11110000
  * // & 10101010
  * // --------
  * // 10100000
  * }}}
  */
  def &(x: Long): Long = value & x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary: 11110000
  * // ^ 10101010
  * // --------
  * // 01011010
  * }}}
  */
  def ^(x: Byte): Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary: 11110000
  * // ^ 10101010
  * // --------
  * // 01011010
  * }}}
  */
  def ^(x: Short): Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary: 11110000
  * // ^ 10101010
  * // --------
  * // 01011010
  * }}}
  */
  def ^(x: Char): Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary: 11110000
  * // ^ 10101010
  * // --------
  * // 01011010
  * }}}
  */
  def ^(x: Int): Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary: 11110000
  * // ^ 10101010
  * // --------
  * // 01011010
  * }}}
  */
  def ^(x: Long): Long = value ^ x
  /** Returns the sum of this value and `x`. */
  def +(x: Byte): Int = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Int = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Int = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Int = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Long = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Float = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double = value + x
  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Int = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Int = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Int = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Int = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Long = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Float = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double = value - x
  /** Returns the product of this value and `x`. */
  def *(x: Byte): Int = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Short): Int = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Char): Int = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Int): Int = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Long): Long = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Float): Float = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double = value * x
  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Int = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Int = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Int = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Int = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Long = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Float = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double = value / x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Int = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Int = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Int = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Int = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Long = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Float = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double = value % x

  // Stuff from RichInt:
  /**
   * Returns a string representation of this <code>PosInt</code>'s underlying <code>Int</code> as an
   * unsigned integer in base&nbsp;2.
   *
   * <p>
   * The unsigned integer value is the argument plus 2<sup>32</sup>
   * if this <code>PosInt</code>'s underlying <code>Int</code> is negative; otherwise it is equal to the
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
   *          represented by this <code>PosInt</code>'s underlying <code>Int</code> in binary (base&nbsp;2).
   */
  def toBinaryString: String = java.lang.Integer.toBinaryString(value)

  /**
   * Returns a string representation of this <code>PosInt</code>'s underlying <code>Int</code> as an
   * unsigned integer in base&nbsp;16.
   *
   * <p>
   * The unsigned integer value is the argument plus 2<sup>32</sup>
   * if this <code>PosInt</code>'s underlying <code>Int</code> is negative; otherwise, it is equal to the
   * this <code>PosInt</code>'s underlying <code>Int</code>  This value is converted to a string of ASCII digits
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
   *          represented by this <code>PosInt</code>'s underlying <code>Int</code> in hexadecimal (base&nbsp;16).
   */
  def toHexString: String = java.lang.Integer.toHexString(value)

  /**
   * Returns a string representation of this <code>PosInt</code>'s underlying <code>Int</code> as an
   * unsigned integer in base&nbsp;8.
   *
   * <p>The unsigned integer value is this <code>PosInt</code>'s underlying <code>Int</code> plus 2<sup>32</sup>
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
   *          represented by this <code>PosInt</code>'s underlying <code>Int</code> in octal (base&nbsp;8).
   */
  def toOctalString: String = java.lang.Integer.toOctalString(value)

  /**
  * Create a <code>Range</code> from this <code>PosInt</code> value
  * until the specified <code>end</code> (exclusive) with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range]] from `this` up to but
  * not including `end`.
  */
  def until(end: Int): Range = Range(value, end)

  /**
  * Create a <code>Range</code> from this <code>PosInt</code> value
  * until the specified <code>end</code> (exclusive) with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.Range]] from `this` up to but
  * not including `end`.
  */
  def until(end: Int, step: Int): Range = Range(value, end, step)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosInt</code> value
  * to the specified <code>end</code> with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Int): Range.Inclusive = Range.inclusive(value, end)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosInt</code> value
  * to the specified <code>end</code> with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(value, end, step)

  // No point to call abs on a PosInt.
  /**
  * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
  */
  def max(that: PosInt): PosInt = if (math.max(value, that.value) == value) this else that

  /**
  * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
  */
  def min(that: PosInt): PosInt = if (math.min(value, that.value) == value) this else that
}

/**
 * The companion object for <code>PosInt</code> that offers factory methods that
 * produce <code>PosInt</code>s, implicit widening conversions from <code>PosInt</code>
 * to other numeric types, and maximum and minimum constant values for <code>PosInt</code>.
 */
object PosInt {
  /**
   * The largest value representable as a positive <code>Int</code>, which is <code>PosInt(2147483647)</code>.
   */
  final val MaxValue: PosInt = PosInt.from(Int.MaxValue).get
  /**
   * The smallest value representable as a positive <code>Int</code>, which is <code>PosInt(1)</code>.
   */
  final val MinValue: PosInt = PosInt.from(1).get // Can't use the macro here

  // TODO: Use one method for validation, as suggested in I think the UK.

  /**
   * A factory method that produces an <code>Option[PosInt]</code> given an
   * <code>Int</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0, it will return a <code>PosInt</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed <code>Int</code>
   * value is 0 or negative, so this method will return <code>None</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas <code>from</code> inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return
   *     wrapped in a <code>Some[PosInt]</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Some[PosInt]</code>, if it is positive, else <code>None</code>.
   */
  def from(value: Int): Option[PosInt] =
    if (PosIntMacro.isValid(value)) Some(new PosInt(value)) else None

  import language.experimental.macros

  /**
   * A factory method, implemented via a macro, that produces a <code>PosInt</code>
   * if passed a valid <code>Int</code> literal, otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the specified <code>Int</code>
   * expression at compile time. If
   * the expression is a positive <code>Int</code> literal, <em>i.e.</em>, with a
   * value greater than 0, it will return a <code>PosInt</code> representing that value.
   * Otherwise, the passed <code>Int</code> 
   * expression is either a literal that is 0 or negative, or is not a literal, so
   * this method will give a compiler error.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>from</code> factory method
   * in that this method is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas <code>from</code> inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the <code>Int</code> literal expression to inspect at compile time,
   *     and if positive, to return wrapped in a <code>PosInt</code> at run time.
   * @return the specified, valid <code>Int</code> literal value wrapped
   *     in a <code>PosInt</code>. (If the specified expression is not a valid
   *     <code>Int</code> literal, the invocation of this method will not
   *     compile.)
   */
  implicit def apply(value: Int): PosInt = macro PosIntMacro.apply

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>Int</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>.
   */
  implicit def widenToInt(pos: PosInt): Int = pos.value

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>Long</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Long</code>.
   */
  implicit def widenToLong(pos: PosInt): Long = pos.value

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>Float</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Float</code>.
   */
  implicit def widenToFloat(pos: PosInt): Float = pos.value

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>Double</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Double</code>.
   */
  implicit def widenToDouble(pos: PosInt): Double = pos.value

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>PosLong</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Long</code> and wrapped in a <code>PosLong</code>.
   */
  implicit def widenToPosLong(pos: PosInt): PosLong = PosLong.from(pos.value).get

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>PosFloat</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Float</code> and wrapped in a <code>PosFloat</code>.
   */
  implicit def widenToPosFloat(pos: PosInt): PosFloat = PosFloat.from(pos.value).get

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>PosDouble</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Double</code> and wrapped in a <code>PosDouble</code>.
   */
  implicit def widenToPosDouble(pos: PosInt): PosDouble = PosDouble.from(pos.value).get

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>PosZInt</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   wrapped in a <code>PosZInt</code>.
   */
  implicit def widenToPosZInt(pos: PosInt): PosZInt = PosZInt.from(pos.value).get

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>PosZLong</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Long</code> and wrapped in a <code>PosZLong</code>.
   */
  implicit def widenToPosZLong(pos: PosInt): PosZLong = PosZLong.from(pos.value).get

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>PosZFloat</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Float</code> and wrapped in a <code>PosZFloat</code>.
   */
  implicit def widenToPosZFloat(pos: PosInt): PosZFloat = PosZFloat.from(pos.value).get

  /**
   * Implicit widening conversion from <code>PosInt</code> to <code>PosZDouble</code>.
   *
   * @param pos the <code>PosInt</code> to widen
   * @return the <code>Int</code> value underlying the specified <code>PosInt</code>,
   *     widened to <code>Double</code> and wrapped in a <code>PosZDouble</code>.
   */
  implicit def widenToPosZDouble(pos: PosInt): PosZDouble = PosZDouble.from(pos.value).get

  /**
   * Implicit Ordering instance.
   */
  implicit val posIntOrd: Ordering[PosInt] =
    new Ordering[PosInt] {
      def compare(x: PosInt, y: PosInt): Int = x - y
    } 
}
