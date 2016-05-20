/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest.concurrent

import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalatest.{FailureMessages, UnquotedString}
import org.scalatest.exceptions.{StackDepthException, TestFailedDueToTimeoutException, TestCanceledException}
import java.nio.channels.ClosedByInterruptException
import java.nio.channels.Selector
import java.net.Socket
import org.scalatest.Exceptional
import org.scalatest.time.Span
import org.scalatest.enablers.Timed
import org.scalactic._

/**
 * Trait that provides a <code>failAfter</code> and <code>cancelAfter</code> construct, which allows you to specify a time limit for an
 * operation passed as a by-name parameter, as well as a way to signal/interrupt it if the operation exceeds its time limit.
 *
 * <p>
 * The time limit is passed as the first parameter, as a <a href="../time/Span.html"><code>Span</code></a>. The operation is
 * passed as the second parameter. And an <a href="Signaler.html"><code>Signaler</code></a>, a strategy for signaling/interrupting the operation, is
 * passed as an implicit third parameter.  Here's a simple example of its use:
 * </p>
 *
 * <pre class="stHighlight">
 * failAfter(Span(100, Millis)) {
 *   Thread.sleep(200)
 * }
 * </pre>
 *
 * <p>
 * Using default [[org.scalatest.enablers.Timed Timed]] implementation, the above code, after 100 milliseconds, will produce a <a href="../exceptions/TestFailedDueToTimeoutException.html"><code>TestFailedDueToTimeoutException</code></a> with a message
 * that indicates a timeout expired:
 * </p>
 *
 * <p>
 * <code>The code passed to failAfter did not complete within 100 milliseconds.</code>
 * </p>
 *
 * <p>
 * If you use <code>cancelAfter</code> in place of <code>failAfter</code>, a <a href="../exceptions/TestCanceledException.html"><code>TestCanceledException</code></a> with a message
 * that indicates a timeout expired:
 * </p>
 *
 * <p>
 * <code>The code passed to cancelAfter did not complete within 100 milliseconds.</code>
 * </p>
 *
 * <p>
 * If you prefer you can mix in or import the members of <a href="../time/SpanSugar.html"><code>SpanSugar</code></a> and place a units value after the integer timeout.
 * Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.time.SpanSugar._
 *
 * failAfter(100 millis) {
 *   Thread.sleep(200)
 * }
 *
 * failAfter(1 second) {
 *   Thread.sleep(2000)
 * }
 * </pre>
 *
 * <a name="signalerConfig"></a><h2>Configuring <code>failAfter</code> or <code>cancelAfter</code> with an <code>Signaler</code></h2>
 *
 * <p>
 * <code>failAfter</code> and <code>cancelAfter</code> takes an implicit <code>Signaler</code>,
 * which will be used to signal/interrupt the main thread.  If you wish to use a different <code>Signaler</code>, you can provide an <code>implicit val</code>
 * Here's an example
 * in which the signaling/interruption method is changed to <a href="ThreadSignaler$.html"><code>ThreadSignaler</code></a>, which will attempt to
 * signal/interrupt the main thread when time is up:
 * </p>
 *
 * <pre class="stHighlight">
 * implicit val signaler = ThreadSignaler
 * failAfter(100 millis) {
 *   Thread.sleep(500)
 * }
 * </pre>
 *
 * <p>
 * When the execution of the above code passes through 100 millis, it will attempt to signal/interrupt the main thread to stop and produce a
 * <code>TestFailedDueToTimeoutException</code> with a message that indicates a timeout expired.
 * </p>
 *
 * <p>
 * This illustrates an important feature of <code>failAfter</code> and <code>cancelAfter</code>: it will throw a
 * <code>TestFailedDueToTimeoutException</code> (or <code>TestCanceledException</code> in case of <code>cancelAfter</code>)
 * if the code passed as the by-name parameter takes longer than the specified timeout to execute, even if it
 * is allowed to run to completion beyond the specified timeout and returns normally.
 * </p>
 * 
 * <p>
 * ScalaTest provides the following <code>Signaler</code> implementations:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong><code>Signaler</code> implementation</strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong>Usage</strong>
 * </th>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="ThreadSignaler$.html">ThreadSignaler</a>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Invokes <code>interrupt</code> on the main test thread. This will
 * set the interrupted status for the main test thread and,
 * if the main thread is blocked, will in some cases cause the main thread to complete abruptly with
 * an <code>InterruptedException</code>.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="DoNotInterrupt$.html">DoNotInterrupt</a>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Does not attempt to interrupt the main test thread in any way
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="SelectorSignaler.html">SelectorSignaler</a>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Invokes <code>wakeup</code> on the passed <code>java.nio.channels.Selector</code>, which
 * will cause the main thread, if blocked in <code>Selector.select</code>, to complete abruptly with a
 * <code>ClosedSelectorException</code>.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="SocketSignaler.html">SocketSignaler</a>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Invokes <code>close</code> on the <code>java.io.Socket</code>, which
 * will cause the main thread, if blocked in a read or write of an <code>java.io.InputStream</code> or
 * <code>java.io.OutputStream</code> that uses the <code>Socket</code>, to complete abruptly with a
 * <code>SocketException</code>.
 * </td>
 * </tr>
 * </table>
 *
 * <p>
 * You may wish to create your own <code>Signaler</code> in some situations. For example, if your operation is performing
 * a loop and can check a volatile flag each pass through the loop. You could in that case write an <code>Signaler</code> that
 * sets that flag so that the next time around, the loop would exit.
 * </p>
 * 
 * @author Chua Chee Seng
 * @author Bill Venners
 */
trait TimeLimits {

  /**
   * Executes the passed function, enforcing the passed time limit by attempting to signal the function if the
   * time limit is exceeded, and should response with other action such as throwing <code>TestFailedDueToTimeoutException</code>
   * when the time limit has been exceeded after the function completes.  The exact behavior depends on the passed in
   * [[org.scalatest.enablers.Timed Timed]] implementation .
   *
   * @param timeout the maximimum amount of time allowed for the passed operation
   * @param fun the operation on which to enforce the passed timeout
   * @param signaler a strategy for interrupting the passed operation
   * @param prettifier a <code>Prettifier</code> for prettifying error messages
   * @param pos the <code>Position</code> of the caller site
   * @param timed the <code>Timed</code> type class that provides the behavior implementation of the timing restriction.
   */
  def failAfter[T](timeout: Span)(fun: => T)(implicit signaler: Signaler, prettifier: Prettifier = implicitly[Prettifier], pos: source.Position = implicitly[source.Position], timed: Timed[T] = implicitly[Timed[T]]): T = {
    failAfterImpl(timeout, signaler, prettifier, adj => getStackDepthFun(pos))(fun)(timed)
  }

  private[scalatest] def failAfterImpl[T](timeout: Span, interruptor: Signaler, prettifier: Prettifier, stackDepthFun: Int => (StackDepthException => Int))(fun: => T)(implicit timed: Timed[T]): T = {
    val stackTraceElements = Thread.currentThread.getStackTrace()
    timed.timeoutAfter(
      timeout,
      fun,
      interruptor,
      (cause: Option[Throwable], stackDepthAdjustment: Int) => {
        val e = new TestFailedDueToTimeoutException(
          sde => Some(FailureMessages.timeoutFailedAfter(prettifier, UnquotedString(timeout.prettyString))),
          cause,
          stackDepthFun(stackDepthAdjustment),
          None,
          timeout
        )
        e.setStackTrace(stackTraceElements)
        e
      }
    )
  }

  /**
    * Executes the passed function, enforcing the passed time limit by attempting to signal the function if the
    * time limit is exceeded, and should response with other action such as throwing <code>TestCanceledException</code>
    * when the time limit has been exceeded after the function completes.  The exact behavior depends on the passed in
    * [[org.scalatest.enablers.Timed Timed]] implementation.
    *
    * @param timeout the maximimum amount of time allowed for the passed operation
    * @param fun the operation on which to enforce the passed timeout
    * @param signaler a strategy for interrupting the passed operation
    * @param prettifier a <code>Prettifier</code> for prettifying error messages
    * @param pos the <code>Position</code> of the caller site
    * @param timed the <code>Timed</code> type class that provides the behavior implementation of the timing restriction.
    */
  def cancelAfter[T](timeout: Span)(fun: => T)(implicit signaler: Signaler, prettifier: Prettifier = implicitly[Prettifier], pos: source.Position = implicitly[source.Position], timed: Timed[T] = implicitly[Timed[T]]): T = {
    cancelAfterImpl(timeout, signaler, prettifier, adj => getStackDepthFun(pos))(fun)(timed)
  }

  private[scalatest] def cancelAfterImpl[T](timeout: Span, interruptor: Signaler, prettifier: Prettifier, stackDepthFun: Int => (StackDepthException => Int))(fun: => T)(implicit timed: Timed[T]): T = {
    val stackTraceElements = Thread.currentThread.getStackTrace()
    timed.timeoutAfter(
      timeout,
      fun,
      interruptor,
      (cause: Option[Throwable], stackDepthAdjustment: Int) => {
        val e = new TestCanceledException(
          sde => Some(FailureMessages.timeoutCanceledAfter(prettifier, UnquotedString(timeout.prettyString))),
          cause,
          stackDepthFun(stackDepthAdjustment),
          None
        )
        e.setStackTrace(stackTraceElements)
        e
      }
    )
  }
  
}

/**
 * Companion object that facilitates the importing of <code>TimeLimits</code> members as
 * an alternative to mixing in the trait. One use case is to import <code>TimeLimits</code>'s members so you can use
 * them in the Scala interpreter.
 */
object TimeLimits extends TimeLimits
