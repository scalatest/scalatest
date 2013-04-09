/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.time

import Span.totalNanosForLongLength
import Span.totalNanosForDoubleLength
import org.scalatest.Resources

/**
 * A time span.
 *
 * <p>
 * A <code>Span</code> is used to express time spans in ScalaTest, in constructs such as the
 * <code>failAfter</code> method of trait <a href="../concurrent/Timeouts.html"><code>Timeouts</code></a>,
 * the <code>timeLimit</code> field of trait
 * <a href="../concurrent/TimeLimitedTests.html"><code>TimeLimitedTests</code></a>, and
 * the timeouts of traits <a href="../concurrent/Eventually.html"><code>Eventually</code></a>,
 * , and
 * <a href="../concurrent/AsyncAssertions.html"><code>AsyncAssertions</code></a>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.time.Span
 * import org.scalatest.time.Millis
 * import org.scalatest.concurrent.Timeouts._
 *
 * failAfter(Span(100, Millis)) {
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * If you prefer you can mix in or import the members of <a href="../time/SpanSugar.html"><code>SpanSugar</code></a> and place a units value after the timeout value.
 * Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.time.SpanSugar._
 * import org.scalatest.concurrent.Timeouts._
 *
 * failAfter(100 millis) {
 *   // ...
 * }
 *
 * failAfter(1 second) {
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * In addition to expression the numeric value with an <code>Int</code> or a <code>Long</code>, you
 * can also express it via a <code>Float</code> or <code>Double</code>. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.time.Span
 * import org.scalatest.time.Seconds
 * import org.scalatest.concurrent.Timeouts._
 *
 * failAfter(Span(1.5, Seconds)) {
 *   // ...
 * }
 *
 * import org.scalatest.time.SpanSugar._
 *
 * failAfter(0.8 seconds) {
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * Internally, a <code>Span</code> is expressed in terms of a <code>Long</code> number of nanoseconds. Thus, the maximum
 * time span that can be represented is <code>Long.MaxValue</code> nanoseconds, or approximately 292 years.
 * Hopefully these won't be "famous last words," but 292 years should be sufficient for software testing purposes.
 * Any attempt to create a <code>Span</code> longer than <code>Long.MaxValue</code> nanoseconds will be met with
 * an <code>IllegalArgumentException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(Long.MaxValue, Nanoseconds) // Produces the longest possible time.Span
 * Span(Long.MaxValue, Seconds)     // Produces an IllegalArgumentException
 * </pre>
 *
 * <p>
 * All of class <code>Span</code>'s constructors are private. The only way you can create a new <code>Span</code> is
 * via one of the two <code>apply</code> factory methods in <a href="Span$.html"><code>Span</code>'s
 * companion object</a>. Here is a table showing one example of each numeric type and unit value:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong><code>Int</code></strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong><code>Long</code></strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong><code>Float</code></strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong><code>Double</code></strong>
 * </th>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1, Nanosecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1L, Nanosecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0F, Nanosecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0, Nanosecond)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Nanoseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Nanoseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Nanoseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Nanoseconds)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1, Microsecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1L, Microsecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0F, Microsecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0, Microsecond)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Microseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Microseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Microseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Microseconds)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1, Millisecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1L, Millisecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0F, Millisecond)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0, Millisecond)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Milliseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Milliseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Milliseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Milliseconds)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Millis)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Millis)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Millis)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Millis)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1, Second)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1L, Second)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0F, Second)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0, Second)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Seconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Seconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Seconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Seconds)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1, Minute)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1L, Minute)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0F, Minute)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0, Minute)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Minutes)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Minutes)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Minutes)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Minutes)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1, Hour)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1L, Hour)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0F, Hour)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0, Hour)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Hours)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Hours)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Hours)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Hours)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1, Day)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1L, Day)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0F, Day)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(1.0, Day)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100, Days)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(100L, Days)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8F, Days)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * Span(99.8, Days)
 * </td>
 * </tr>
 * </table>
 *
 * @author Bill Venners
 */
final class Span private (totNanos: Long, lengthString: String, unitsResource: String, unitsName: String) {

  private[time] def this(length: Long, units: Units) {
    this(
      totalNanosForLongLength(length, units),
      length.toString,
      if (length == 1) units.singularResourceName else units.pluralResourceName,
      units.toString
    )
  }

  private def this(length: Double, units: Units) {
    this(
      totalNanosForDoubleLength(length, units),
      length.toString,
      if (length == 1.0) units.singularResourceName else units.pluralResourceName,
      units.toString
    )
  }

  /**
   * The total number of nanoseconds in this time span.
   *
   * This number will never be negative, but can be zero.
   */
  val totalNanos: Long = totNanos // Didn't use a parametric field becuase don't know how to scaladoc that in a
                                  // private constructor
  /**
   * This time span converted to milliseconds, computed via <code>totalNanos / 1000000</code>, which
   * truncates off any leftover nanoseconds.
   *
   * <p>
   * The <code>millisPart</code> and <code>nanosPart</code> can be used, for example, when invoking
   * <code>Thread.sleep</code>. For example, given a <code>Span</code> named <code>span</code>, you could
   * write:
   * </p>
   *
   * <pre class="stHighlight">
   * Thread.sleep(span.millisPart, span.nanosPart)
   * </pre>
   */
  lazy val millisPart: Long = totalNanos / 1000000

  /**
   * The number of nanoseconds remaining when this time span is converted to milliseconds, computed via
   * <code>(totalNanos % 1000000).toInt</code>.
   *
   * <p>
   * The <code>millisPart</code> and <code>nanosPart</code> can be used, for example, when invoking
   * <code>Thread.sleep</code>. For example, given a <code>Span</code> named <code>span</code>, you could
   * write:
   * </p>
   *
   * <pre class="stHighlight">
   * Thread.sleep(span.millisPart, span.nanosPart)
   * </pre>
   */
  lazy val nanosPart: Int = (totalNanos % 1000000).toInt

  /**
   * Returns a <code>Span</code> representing this <code>Span</code> <em>scaled</em> by the passed factor.
   *
   * <p>
   * The passed <code>factor</code> can be any positive number or zero, including fractional numbers. A number
   * greater than one will scale the <code>Span</code> up to a larger value. A fractional number will scale it
   * down to a smaller value. A factor of 1.0 will cause the exact same <code>Span</code> to be returned. A
   * factor of zero will cause <code>Span.ZeroLength</code> to be returned.
   * </p>
   *
   * <p>
   * If overflow occurs, <code>Span.Max</code> will be returned. If underflow occurs, <code>Span.ZeroLength</code>
   * will be returned.
   * </p>
   *
   * @throws IllegalArgumentException if the passed value is less than zero
   */
  def scaledBy(factor: Double) = {
    require(factor >= 0.0, "factor was less than zero")
    factor match {
      case 0.0 => Span.ZeroLength
      case 1.0 => this
      case _ =>
        val newNanos: Double = totNanos * factor
        newNanos match {
          case n if n > Long.MaxValue => Span.Max
          case 1 => Span(1, Nanosecond)
          case n if n < 1000 =>
            if (n.longValue == n) Span(n.longValue, Nanoseconds) else Span(n, Nanoseconds)
          case 1000 => Span(1, Microsecond)
          case n if n < 1000 * 1000 =>
            val v = n / 1000
            if (v.longValue == v) Span(v.longValue, Microseconds) else Span(v, Microseconds)
          case 1000000 => Span(1, Millisecond)
          case n if n < 1000 * 1000 * 1000 =>
            val v = n / 1000 / 1000
            if (v.longValue == v) Span(v.longValue, Millis) else Span(v, Millis)
          case 1000000000L => Span(1, Second)
          case n if n < 1000L * 1000 * 1000 * 60 =>
            val v = n / 1000 / 1000 / 1000
            if (v.longValue == v) Span(v.longValue, Seconds) else Span(v, Seconds)
          case 60000000000L => Span(1, Minute)
          case n if n < 1000L * 1000 * 1000 * 60 * 60 =>
            val v = n / 1000 / 1000 / 1000 / 60
            if (v.longValue == v) Span(v.longValue, Minutes) else Span(v, Minutes)
          case 3600000000000L => Span(1, Hour)
          case n if n < 1000L * 1000 * 1000 * 60 * 60 * 24 =>
            val v = n / 1000 / 1000 / 1000 / 60 / 60
            if (v.longValue == v) Span(v.longValue, Hours) else Span(v, Hours)
          case 86400000000000L => Span(1, Day)
          case n =>
            val v = n / 1000 / 1000 / 1000 / 60 / 60 / 24
            if (v.longValue == v) Span(v.longValue, Days) else Span(v, Days)
        }
    }
  }

  /**
   * Returns a localized string suitable for presenting to a user that describes the time span represented
   * by this <code>Span</code>.
   *
   * <p>
   * For example, for <code>Span(1, Millisecond)</code>, this method would return <code>"1 millisecond"</code>.
   * For <code>Span(9.99, Seconds)</code>, this method would return <code>"9.9 seconds"</code>.
   * </p>
   *
   * @return a localized string describing this <code>Span</code>
   */
  lazy val prettyString: String = Resources(unitsResource, lengthString)

  /**
   * Returns a string that looks similar to a factory method call that would have produced this <code>Span</code>.
   *
   * <p>
   * For example, for <code>Span(1, Millisecond)</code>, this method would return <code>"Span(1, Millisecond)"</code>.
   * For <code>Span(9.99, Seconds)</code>, this method would return <code>"Span(9.99, Seconds)"</code>.
   * </p>
   *
   * @return a string that looks like a factory method call that would produce this <code>Span</code>
   */
  override def toString = "Span(" + lengthString + ", " + unitsName + ")"

  /**
   * Compares another object for equality.
   *
   * <p>
   * If the passed object is a <code>Span</code>, this method will return <code>true</code> only if the other
   * <code>Span</code> returns the exact same value as this <code>Span</code> for <code>totalNanos</code>.
   * </p>
   *
   * @param other the object to compare with this one for equality
   *
   * @return true if the other object is a <code>Span</code> with the same <code>totalNanos</code> value.
   */
  override def equals(other: Any): Boolean = {
    other match {
      case that: Span => totalNanos == that.totalNanos
      case _ => false
    }
  }

  /**
   * Returns a hash code for this <code>Span</code>.
   *
   * @return a hash code based only on the <code>totalNanos</code> field.
   */
  override def hashCode: Int = totalNanos.hashCode
}

/**
 * Companion object for <code>Span</code> that provides two factory methods for creating <code>Span</code> instances.
 *
 * <p>
 * The first argument to each factory method is a numeric value; the second argument is a <code>Units</code> value.
 * One factory method takes a <code>Long</code>, so it can be invoked with either an <code>Int</code> or
 * </code>Long</code>, as in:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.time._
 *
 * Span(1, Second)
 * Span(1L, Millisecond)
 * </pre>
 *
 * <p>
 * The other factory method takes a <code>Double</code>, so it can be invoked with either a <code>Float</code> or
 * a <code>Double</code>:
 * </p>
 * <pre class="stHighlight">
 * import org.scalatest.time._
 *
 * Span(2.5F, Seconds)
 * Span(99.9, Microseconds)
 * </pre>
 *
 * @author Bill Venners
 */
object Span {

  /**
   * Returns a <code>Span</code> representing the passed <code>Long</code> <code>length</code> of time in the
   * passed <code>units</code>.
   *
   * <p>
   * If the requested time span is less than zero or greater than <code>Long.MaxValue</code> nanoseconds, this method will throw
   * an <code>IllegalArgumentException</code>. (Note: a zero-length time span is allowed, just not a negative or
   * too-large time span.)
   * </p>
   *
   * @param length the length of time denominated by the passed <code>units</code>
   * @param units the units of time for the passed <code>length</code>
   * @return a <code>Span</code> representing the requested time span
   * @throws IllegalArgumentException if the requested time span is greater than <code>Long.MaxValue</code>
   *      nanoseconds, the maximum time span expressible with a <code>Span</code>
   */
  def apply(length: Long, units: Units): Span = new Span(length, units)

  /**
   * Returns a <code>Span</code> representing the passed <code>Double</code> <code>length</code> of time in the
   * passed <code>units</code>.
   *
   * <p>
   * If the requested time span is less than <code>0.0</code> or, when converted to <code>Long</code> number of nanoseconds, would be greater than
   * <code>Long.MaxValue</code> nanoseconds, this method will throw an <code>IllegalArgumentException</code>.
   * (Note: a zero-length time span is allowed, just not a negative or too-large time span.)
   * </p>
   *
   * @param length the length of time denominated by the passed <code>units</code>
   * @param units the units of time for the passed <code>length</code>
   * @return a <code>Span</code> representing the requested time span
   * @throws IllegalArgumentException if the requested time span, when converted to <code>Long</code> number of
   *     nanoseconds, would be  greater than <code>Long.MaxValue</code> nanoseconds, the maximum time span
   *     expressible with a <code>Span</code>
   */
  def apply(length: Double, units: Units): Span = new Span(length, units)

  /**
   * A <code>Span</code> with the maximum expressible value, <code>Span(Long.MaxValue, Nanoseconds)</code>,
   * which is approximately 292 years.
   *
   * <p>
   * One use case for this <code>Span</code> value is to help convert a duration concept from a different library to
   * <code>Span</code> when that library's duration concept includes a notion of infinite durations. An infinite
   * duration can be converted to <code>Span.Max</code>.
   * </p>
   *
   * @return a <code>Span</code> with the maximum expressible value, <code>Long.MaxValue</code> nanoseconds.
   */
  val Max: Span = new Span(Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 / 60 / 24, Days)

  /**
   * A <code>Span</code> with representing a zero-length span of time.
   *
   * @return a zero-length <code>Span</code>.
   */
  val ZeroLength: Span = new Span(0, Nanoseconds)

  private def totalNanosForLongLength(length: Long, units: Units): Long = {

    require(length >= 0, "length must be greater than or equal to zero, but was: " + length)

    val MaxMicroseconds = Long.MaxValue / 1000
    val MaxMilliseconds = Long.MaxValue / 1000 / 1000
    val MaxSeconds = Long.MaxValue / 1000 / 1000 / 1000
    val MaxMinutes = Long.MaxValue / 1000 / 1000 / 1000 / 60
    val MaxHours = Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60
    val MaxDays = Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60 / 24

    require(units != Nanosecond || length == 1, singularErrorMsg("Nanosecond"))
    require(units != Microsecond || length == 1, singularErrorMsg("Microsecond"))
    require(units != Millisecond || length == 1, singularErrorMsg("Millisecond"))
    require(units != Second || length == 1, singularErrorMsg("Second"))
    require(units != Minute || length == 1, singularErrorMsg("Minute"))
    require(units != Hour || length == 1, singularErrorMsg("Hour"))
    require(units != Day || length == 1, singularErrorMsg("Day"))

    require(units != Microseconds || length <= MaxMicroseconds, "Passed length, " + length + ", is larger than the largest expressible number of microseconds: Long.MaxValue / 1000")
    require(units != Milliseconds && units != Millis || length <= MaxMilliseconds, "Passed length, " + length + ", is larger than the largest expressible number of millieconds: Long.MaxValue / 1000 / 1000")
    require(units != Seconds || length <= MaxSeconds, "Passed length, " + length + ", is larger than the largest expressible number of seconds: Long.MaxValue / 1000 / 1000 / 1000")
    require(units != Minutes || length <= MaxMinutes, "Passed length, " + length + ", is larger than the largest expressible number of minutes: Long.MaxValue / 1000 / 1000 / 1000 / 60")
    require(units != Hours || length <= MaxHours, "Passed length, " + length + ", is larger than the largest expressible number of hours: Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60")
    require(units != Days || length <= MaxDays, "Passed length, " + length + ", is larger than the largest expressible number of days: Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60 / 24")

    units match {
      case Nanosecond | Nanoseconds =>
        length
      case Microsecond | Microseconds =>
        length * 1000
      case Millisecond | Milliseconds | Millis =>
        length * 1000 * 1000
      case Second | Seconds =>
        length * 1000 * 1000 * 1000
      case Minute | Minutes =>
        length * 1000 * 1000 * 1000 * 60
      case Hour | Hours =>
        length * 1000 * 1000 * 1000 * 60 * 60
      case Day | Days =>
        length * 1000 * 1000 * 1000 * 60 * 60 * 24
    }
  }

  private def totalNanosForDoubleLength(length: Double, units: Units): Long = {

    require(length >= 0, "length must be greater than or equal to zero, but was: " + length)

    val MaxNanoseconds = (Long.MaxValue).toDouble
    val MaxMicroseconds = Long.MaxValue.toDouble / 1000
    val MaxMilliseconds = Long.MaxValue.toDouble / 1000 / 1000
    val MaxSeconds = Long.MaxValue.toDouble / 1000 / 1000 / 1000
    val MaxMinutes = Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60
    val MaxHours = Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 / 60
    val MaxDays = Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 / 60 / 24

    require(units != Nanosecond || length == 1.0, singularErrorMsg("Nanosecond"))
    require(units != Microsecond || length == 1.0, singularErrorMsg("Microsecond"))
    require(units != Millisecond || length == 1.0, singularErrorMsg("Millisecond"))
    require(units != Second || length == 1.0, singularErrorMsg("Second"))
    require(units != Minute || length == 1.0, singularErrorMsg("Minute"))
    require(units != Hour || length == 1.0, singularErrorMsg("Hour"))
    require(units != Day || length == 1.0, singularErrorMsg("Day"))

    require(units != Nanoseconds || length <= MaxNanoseconds, "Passed length, " + length + ", is larger than the largest expressible number of nanoseconds: Long.MaxValue")
    require(units != Microseconds || length <= MaxMicroseconds, "Passed length, " + length + ", is larger than the largest expressible number of microseconds: Long.MaxValue / 1000")
    require(units != Milliseconds && units != Millis || length <= MaxMilliseconds, "Passed length, " + length + ", is larger than the largest expressible number of millieconds: Long.MaxValue / 1000 / 1000")
    require(units != Seconds || length <= MaxSeconds, "Passed length, " + length + ", is larger than the largest expressible number of seconds: Long.MaxValue / 1000 / 1000 / 1000")
    require(units != Minutes || length <= MaxMinutes, "Passed length, " + length + ", is larger than the largest expressible number of minutes: Long.MaxValue / 1000 / 1000 / 1000 / 60")
    require(units != Hours || length <= MaxHours, "Passed length, " + length + ", is larger than the largest expressible number of hours: Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60")
    require(units != Days || length <= MaxDays, "Passed length, " + length + ", is larger than the largest expressible number of days: Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60 / 24")

    units match {
      case Nanosecond | Nanoseconds =>
        length.toLong
      case Microsecond | Microseconds =>
        (length * 1000).toLong
      case Millisecond | Milliseconds | Millis =>
        (length * 1000 * 1000).toLong
      case Second | Seconds =>
        (length * 1000 * 1000 * 1000).toLong
      case Minute | Minutes =>
        (length * 1000 * 1000 * 1000 * 60).toLong
      case Hour | Hours =>
        (length * 1000 * 1000 * 1000 * 60 * 60).toLong
      case Day | Days =>
        (length * 1000 * 1000 * 1000 * 60 * 60 * 24).toLong
    }
  }

  private def singularErrorMsg(unitsString: String) = {
    "Singular form of " + unitsString +
      " (i.e., without the trailing s) can only be used with the value 1. Use " +
      unitsString + "s (i.e., with an s) instead."
  }
}

