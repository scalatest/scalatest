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

import scala.collection.immutable.Range

//
// Numbers greater than or equal to zero.
//
// (Pronounced like "posey".)
//

final class PozInt private (val value: Int) extends AnyVal {
  override def toString: String = s"PozInt($value)"
  def toByte: Byte = value.toByte
  def toShort: Short = value.toShort
  def toChar: Char = value.toChar
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
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
  def unary_+ : PozInt = this
  /** Returns the negation of this value. */
  def unary_- : Int = -value
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

  // Stuff from Richint:
  def toBinaryString: String = java.lang.Integer.toBinaryString(value)
  def toHexString: String = java.lang.Integer.toHexString(value)
  def toOctalString: String = java.lang.Integer.toOctalString(value)

  /**
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range]] from `this` up to but
  * not including `end`.
  */
  def until(end: Int): Range = Range(value, end)

  /**
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.Range]] from `this` up to but
  * not including `end`.
  */
  def until(end: Int, step: Int): Range = Range(value, end, step)

  /**
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Int): Range.Inclusive = Range.inclusive(value, end)

  /**
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(value, end, step)

  // No point to call abs on a PozInt.
  def max(that: PozInt): PozInt = if (math.max(value, that.value) == value) this else that
  def min(that: PozInt): PozInt = if (math.min(value, that.value) == value) this else that
}

object PozInt {
  def from(value: Int): Option[PozInt] =
    if (value >= 0) Some(new PozInt(value)) else None
  import language.experimental.macros
  import scala.language.implicitConversions
  implicit def apply(value: Int): PozInt = macro PozIntMacro.apply

  implicit def widenToInt(poz: PozInt): Int = poz.value
  implicit def widenToLong(poz: PozInt): Long = poz.value
  implicit def widenToFloat(poz: PozInt): Float = poz.value
  implicit def widenToDouble(poz: PozInt): Double = poz.value

  implicit def widenToPozLong(poz: PozInt): PozLong = PozLong.from(poz.value).get
  implicit def widenToPozFloat(poz: PozInt): PozFloat = PozFloat.from(poz.value).get
  implicit def widenToPozDouble(poz: PozInt): PozDouble = PozDouble.from(poz.value).get
}
