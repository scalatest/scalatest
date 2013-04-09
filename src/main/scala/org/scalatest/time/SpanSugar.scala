/*
 * Copyright 2001-2009 Artima, Inc.
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

/**
 * Trait providing four implicit conversions that allow you to specify spans of time
 * by invoking "units" methods such as <code>millis</code>, <code>seconds</code>, and <code>minutes</code>
 * on <code>Int</code>, <code>Long</code>, <code>Float</code>, and <code>Double</code>.
 * 
 * <p>
 * This trait enables you to specify a span of time in a clear, boilerplate-free way when you
 * need to provide an instance of <a href="Span.html"><code>Span</code></a>. This
 * can be used, for example, with the <code>failAfter</code> method of trait
 * <a href="../concurrent/Timeouts.html"><code>Timeouts</code></a> or the <code>timeLimit</code> field of trait
 * <a href="../concurrent/TimeLimitedTests.html"><code>TimeLimitedTests</code></a>. It can also be used to specify
 * timeouts when using traits <a href="../concurrent/Eventually.html"><code>Eventually</code></a>,
 * <a href="../concurrent/Futures.html"><code>Futures</code></a>,
 * <a href="../concurrent/Waiter.html"><code>Waiter</code></a>. Here are examples of each unit enabled by this trait:
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
 * 1 nanosecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1L nanosecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0F nanosecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0 nanosecond
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 nanoseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L nanoseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F nanoseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 nanoseconds
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1 microsecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1L microsecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0F microsecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0 microsecond
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 microseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L microseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F microseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 microseconds
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1 millisecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1L millisecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0F millisecond
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0 millisecond
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 milliseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L milliseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F milliseconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 milliseconds
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 millis
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L millis
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F millis
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 millis
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1 second
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1L second
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0F second
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0 second
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 seconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L seconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F seconds
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 seconds
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1 minute
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1L minute
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0F minute
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0 minute
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 minutes
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L minutes
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F minutes
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 minutes
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1 hour
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1L hour
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0F hour
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0 hour
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 hours
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L hours
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F hours
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 hours
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1 day
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1L day
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0F day
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 1.0 day
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100 days
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 100L days
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8F days
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 * 99.8 days
 * </td>
 * </tr>
 * </table>
 *
 * <p>
 * This trait is not the default way to specify <code>Span</code>s for two reasons. First, it adds
 * four implicits, which would give the compiler more work to do and may conflict with other implicits the
 * user has in scope. Instead, <code>Span</code> provides a clear, concise default way to specify time
 * spans that requires no implicits. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Second)
 * </pre>
 *
 * <p>
 * If you already have implicit conversions in scope that provide a similar syntax sugar for expression
 * time spans, you can use that by providing an implicit conversion from the result of those expressions
 * to <code>Span</code>. For example, here's how you might use the <code>Duration</code> class from Akka:
 * </p>
 *
 * <pre class="stREPL">
 * $ scala -cp scalatest-1.8-for-scala-2.9.jar:akka-actor-2.0.jar
 * Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_29).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala> import org.scalatest.time._
 * import org.scalatest.time.time._
 *
 * scala> import org.scalatest.concurrent.Eventually._
 * import org.scalatest.concurrent.Eventually._
 * * scala> import org.scalatest.matchers.ShouldMatchers._
 * import org.scalatest.matchers.ShouldMatchers._
 *
 * scala> import akka.util.Duration
 * import akka.util.Duration
 *
 * scala> implicit def convertAkkaDuration(akkaDuration: Duration): Span =
 *      |   if (akkaDuration.isFinite) Span(akkaDuration.toNanos, Nanoseconds) else Span.max
 * convertAkkaDuration: (akkaDuration: akka.util.Duration)org.scalatest.time.Span
 *
 * scala> eventually(timeout(Duration(100, "millis"))) { 1 + 1 should equal (3) }
 * org.scalatest.TestFailedException: The code passed to eventually never returned normally. Attempted 6 times, sleeping 10 milliseconds between each attempt. Last failure message: 2 did not equal 3.
 *     at org.scalatest.concurrent.Eventually$class.tryTryAgain$1(Eventually.scala:346)
 *     at org.scalatest.concurrent.Eventually$class.eventually(Eventually.scala:356)
 *     at org.scalatest.concurrent.Eventually$.eventually(Eventually.scala:396)
 *     ...
 *
 * scala> import akka.util.duration._ // Use Akka's syntax sugar for expressing time spans, not ScalaTest's
 * import akka.util.duration._
 *
 * scala> eventually(timeout(100 millis)) { 1 + 1 should equal (3) }
 * org.scalatest.TestFailedException: The code passed to eventually never returned normally. Attempted 7 times, sleeping 10 milliseconds between each attempt. Last failure message: 2 did not equal 3.
 *     at org.scalatest.concurrent.Eventually$class.tryTryAgain$1(Eventually.scala:346)
 *     at org.scalatest.concurrent.Eventually$class.eventually(Eventually.scala:356)
 *     at org.scalatest.concurrent.Eventually$.eventually(Eventually.scala:396)
 *     ...
 * </pre>
 */
trait SpanSugar {

  /**
   * Class containing methods that return a <code>Span</code> time value calculated from the
   * <code>Long</code> value passed to the <code>GrainOfTime</code> constructor.
   * 
   * @param value the value to be converted
   */
  class GrainOfTime(value: Long) {

    /**
     * A units method for one nanosecond.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in nanoseconds
     */
    def nanosecond: Span = Span(value, Nanosecond)

    /**
     * A units method for nanoseconds.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in nanoseconds
     */
    def nanoseconds: Span = Span(value, Nanoseconds)

    /**
     * A units method for one microsecond.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in microseconds
     */
    def microsecond: Span = Span(value, Microsecond)

    /**
     * A units method for microseconds.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in microseconds
     */
    def microseconds: Span = Span(value, Microseconds)

    /**
     * A units method for one millisecond. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in milliseconds
     */
    def millisecond: Span = Span(value, Millisecond)
    
    /**
     * A units method for milliseconds. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in milliseconds
     */
    def milliseconds: Span = Span(value, Milliseconds)

    /**
     * A shorter units method for milliseconds. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in milliseconds
     */
    def millis: Span = Span(value, Millis)

    /**
     * A units method for one second. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in seconds
     */
    def second: Span = Span(value, Second) 
    
    /**
     * A units method for seconds. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in seconds
     */
    def seconds: Span = Span(value, Seconds)

    /**
     * A units method for one minute. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in minutes
     */
    def minute: Span = Span(value, Minute)

    /**
     * A units method for minutes. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in minutes
     */
    def minutes: Span = Span(value, Minutes)
    
    /**
     * A units method for one hour. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in hours
     */
    def hour: Span = Span(value, Hour)

    /**
     * A units method for hours. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in hours
     */
    def hours: Span = Span(value, Hours)
    
    /**
     * A units method for one day. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor in days
     */
    def day: Span = Span(value, Day)

    /**
     * A units method for days. 
     * 
     * @return A <code>Span</code> representing the value passed to the constructor multiplied in days
     */
    def days: Span = Span(value, Days)
  }

  /**
   * Class containing methods that return a <code>Span</code> time value calculated from the
   * <code>Double</code> value passed to the <code>FloatingGrainOfTime</code> constructor.
   *
   * @param value the value to be converted
   */
  class FloatingGrainOfTime(value: Double) {

    /**
     * A units method for one nanosecond.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in nanoseconds
     */
    def nanosecond: Span = Span(value, Nanosecond)

    /**
     * A units method for nanoseconds.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in nanoseconds
     */
    def nanoseconds: Span = Span(value, Nanoseconds)

    /**
     * A units method for one microsecond.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in microseconds
     */
    def microsecond: Span = Span(value, Microsecond)

    /**
     * A units method for microseconds.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in microseconds
     */
    def microseconds: Span = Span(value, Microseconds)

    /**
     * A units method for one millisecond.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in milliseconds
     */
    def millisecond: Span = Span(value, Millisecond)

    /**
     * A units method for milliseconds.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in milliseconds
     */
    def milliseconds: Span = Span(value, Milliseconds)

    /**
     * A shorter units method for milliseconds.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in milliseconds
     */
    def millis: Span = Span(value, Millis)

    /**
     * A units method for one second.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in seconds
     */
    def second: Span = Span(value, Second)

    /**
     * A units method for seconds.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in seconds
     */
    def seconds: Span = Span(value, Seconds)

    /**
     * A units method for one minute.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in minutes
     */
    def minute: Span = Span(value, Minute)

    /**
     * A units method for minutes.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in minutes
     */
    def minutes: Span = Span(value, Minutes)

    /**
     * A units method for one hour.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in hours
     */
    def hour: Span = Span(value, Hour)

    /**
     * A units method for hours.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in hours
     */
    def hours: Span = Span(value, Hours)

    /**
     * A units method for one day.
     *
     * @return A <code>Span</code> representing the value passed to the constructor in days
     */
    def day: Span = Span(value, Day)

    /**
     * A units method for days.
     *
     * @return A <code>Span</code> representing the value passed to the constructor multiplied in days
     */
    def days: Span = Span(value, Days)
  }

  /**
   * Implicit conversion that adds time units methods to <code>Int</code>s.
   * 
   * @param i: the <code>Int</code> to which to add time units methods
   * @return a <code>GrainOfTime</code> wrapping the passed <code>Int</code>
   */
  implicit def convertIntToGrainOfTime(i: Int) = new GrainOfTime(i)
  
  /**
   * Implicit conversion that adds time units methods to <code>Long</code>s.
   * 
   * @param i: the <code>Long</code> to which to add time units methods
   * @return a <code>GrainOfTime</code> wrapping the passed <code>Long</code>
   */
  implicit def convertLongToGrainOfTime(i: Long) = new GrainOfTime(i)


  /**
   * Implicit conversion that adds time units methods to <code>Float</code>s.
   *
   * @param f: the <code>Float</code> to which to add time units methods
   * @return a <code>FloatingGrainOfTime</code> wrapping the passed <code>Float</code>
   */
  implicit def convertFloatToGrainOfTime(f: Float) = new FloatingGrainOfTime(f)

  /**
   * Implicit conversion that adds time units methods to <code>Double</code>s.
   *
   * @param d: the <code>Double</code> to which to add time units methods
   * @return a <code>FloatingGrainOfTime</code> wrapping the passed <code>Double</code>
   */
  implicit def convertDoubleToGrainOfTime(d: Double) = new FloatingGrainOfTime(d)
}

/**
 * Companion object that facilitates the importing of <code>SpanSugar</code> members as 
 * an alternative to mixing it in. One use case is to import <code>SpanSugar</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_29).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import concurrent.Timeouts._
 * import org.scalatest.concurrent.Timeouts._
 *
 * scala&gt; import time.SpanSugar._
 * import org.scalatest.time.SpanSugar._
 *
 * scala&gt; Thread.sleep(2 seconds) // TODO: Need a new example
 * </pre>
 */
object SpanSugar extends SpanSugar
