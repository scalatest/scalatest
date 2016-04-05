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
import org.scalactic.{Prettifier, SourceInfo}

/**
 * Trait that provides a <code>failAfter</code> and <code>cancelAfter</code> construct, which allows you to specify a time limit for an
 * operation passed as a by-name parameter, as well as a way to interrupt it if the operation exceeds its time limit.
 *
 * <p>
 * The time limit is passed as the first parameter, as a <a href="../time/Span.html"><code>Span</code></a>. The operation is
 * passed as the second parameter. And an <a href="Signaler.html"><code>Signaler</code></a>, a strategy for interrupting the operation, is
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
 * The above code, after 100 milliseconds, will produce a <a href="../exceptions/TestFailedDueToTimeoutException.html"><code>TestFailedDueToTimeoutException</code></a> with a message
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
 * <p>
 * The code passed via the by-name parameter to <code>failAfter</code> or <code>cancelAfter</code> will be executed by the thread that invoked
 * <code>failAfter</code> or <code>cancelAfter</code>, so that no synchronization is necessary to access variables declared outside the by-name.
 * </p>
 *
 * <pre class="stHighlight">
 * var result = -1 // No need to make this volatile
 * failAfter(100 millis) {
 *   result = accessNetService()
 * }
 * result should be (99)
 * </pre>
 *
 * <p>
 * The <code>failAfter</code> or <code>cancelAfter</code> method will create a timer that runs on a different thread than the thread that
 * invoked <code>failAfter</code> or <code>cancelAfter</code>, so that it can detect when the timeout has expired and attempt to <em>interrupt</em>
 * the main thread. Because different operations can require different interruption strategies, the <code>failAfter</code> or <code>cancelAfter</code>
 * method accepts an implicit third parameter of type <code>Signaler</code> that is responsible for interrupting
 * the main thread.
 * </p>
 *
 * <a name="interruptorConfig"></a><h2>Configuring <code>failAfter</code> or <code>cancelAfter</code> with an <code>Signaler</code></h2>
 *
 * <p>
 * This trait declares an implicit <code>val</code> named <code>defaultSignaler</code>,
 * initialized with a <a href="ThreadSignaler$.html"><code>ThreadSignaler</code></a>, which attempts to interrupt the main thread by invoking
 * <code>Thread.interrupt</code>. If you wish to use a different strategy, you can override this <code>val</code> (or hide
 * it, for example if you imported the members of <code>Timeouts</code> rather than mixing it in). Here's an example
 * in which the default interruption method is changed to <a href="DoNotInterrupt$.html"><code>DoNotInterrupt</code></a>, which does not attempt to
 * interrupt the main thread in any way:
 * </p>
 *
 * <pre class="stHighlight">
 * override val defaultSignaler = DoNotInterrupt
 * failAfter(100 millis) {
 *   Thread.sleep(500)
 * }
 * </pre>
 *
 * <p>
 * As with the default <code>Signaler</code>, the above code will eventually produce a 
 * <code>TestFailedDueToTimeoutException</code> with a message that indicates a timeout expired. However, instead
 * of throwing the exception after approximately 100 milliseconds, it will throw it after approximately 500 milliseconds.
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
 * The default interruptor, invokes <code>interrupt</code> on the main test thread. This will
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

   /*
   * <p>
   * To change the default <code>Signaler</code> configuration, define an implicit
   * <code>Signaler</code> in scope.
   * </p>
   */
  // implicit val defaultSignaler: Signaler = ThreadSignaler

  /**
   * Executes the passed function, enforcing the passed time limit by attempting to interrupt the function if the
   * time limit is exceeded, and throwing <code>TestFailedDueToTimeoutException</code> if the time limit has been 
   * exceeded after the function completes.
   *
   * <p>
   * If the function completes <em>before</em> the timeout expires:
   * </p>
   *
   * <ul>
   * <li>If the function returns normally, this method will return normally.</li>
   * <li>If the function completes abruptly with an exception, this method will complete abruptly with that same exception.</li>
   * </ul>
   *
   * <p>
   * If the function completes <em>after</em> the timeout expires:
   * </p>
   *
   * <ul>
   * <li>If the function returns normally, this method will complete abruptly with a <code>TestFailedDueToTimeoutException</code>.</li>
   * <li>If the function completes abruptly with an exception, this method will complete abruptly with a <code>TestFailedDueToTimeoutException</code> that includes the exception thrown by the function as its cause.</li>
   * </ul>
   *
   * <p>
   * If the interrupted status of the main test thread (the thread that invoked <code>failAfter</code>) was not invoked
   * when <code>failAfter</code> was invoked, but is set after the operation times out, it is reset by this method before
   * it completes abruptly with a <code>TestFailedDueToTimeoutException</code>. The interrupted status will be set by
   * <code>ThreadSignaler</code>, the default <code>Signaler</code> implementation.
   * </p>
   *
   * @param timeout the maximimum amount of time allowed for the passed operation
   * @param fun the operation on which to enforce the passed timeout
   * @param interruptor a strategy for interrupting the passed operation
   */
  def failAfter[T](timeout: Span)(fun: => T)(implicit interruptor: Signaler, sourceInfo: SourceInfo = implicitly[SourceInfo], timed: Timed[T] = implicitly[Timed[T]]): T = {
    val stackTraceElements = Thread.currentThread.getStackTrace()
    timed.timeoutAfter(
      timeout,
      fun,
      interruptor,
      (cause: Option[Throwable]) => {
        val e = new TestFailedDueToTimeoutException(
          sde => Some(FailureMessages.timeoutFailedAfter(UnquotedString(timeout.prettyString))),
          cause,
          getStackDepthFun(sourceInfo),
          None,
          timeout
        )
        e.setStackTrace(stackTraceElements)
        e
      }
    )
  }

  // TODO: Consider creating a TestCanceledDueToTimeoutException
  /**
   * Executes the passed function, enforcing the passed time limit by attempting to interrupt the function if the
   * time limit is exceeded, and throwing <code>TestCanceledException</code> if the time limit has been
   * exceeded after the function completes.
   *
   * <p>
   * If the function completes <em>before</em> the timeout expires:
   * </p>
   *
   * <ul>
   * <li>If the function returns normally, this method will return normally.</li>
   * <li>If the function completes abruptly with an exception, this method will complete abruptly with that same exception.</li>
   * </ul>
   *
   * <p>
   * If the function completes <em>after</em> the timeout expires:
   * </p>
   *
   * <ul>
   * <li>If the function returns normally, this method will complete abruptly with a <code>TestCanceledException</code>.</li>
   * <li>If the function completes abruptly with an exception, this method will complete abruptly with a <code>TestCanceledException</code> that includes the exception thrown by the function as its cause.</li>
   * </ul>
   *
   * <p>
   * If the interrupted status of the main test thread (the thread that invoked <code>cancelAfter</code>) was not invoked
   * when <code>cancelAfter</code> was invoked, but is set after the operation times out, it is reset by this method before
   * it completes abruptly with a <code>TestCanceledException</code>. The interrupted status will be set by
   * <code>ThreadSignaler</code>, the default <code>Signaler</code> implementation.
   * </p>
   *
   * @param timeout the maximimum amount of time allowed for the passed operation
   * @param f the operation on which to enforce the passed timeout
   * @param interruptor a strategy for interrupting the passed operation
   */
  def cancelAfter[T](timeout: Span)(fun: => T)(implicit interruptor: Signaler, sourceInfo: SourceInfo = implicitly[SourceInfo], timed: Timed[T] = implicitly[Timed[T]]): T = {
    val stackTraceElements = Thread.currentThread.getStackTrace()
    timed.timeoutAfter(
      timeout,
      fun,
      interruptor,
      (cause: Option[Throwable]) => {
        val e = new TestCanceledException(
          sde => Some(FailureMessages.timeoutCanceledAfter(UnquotedString(timeout.prettyString))),
          cause,
          getStackDepthFun(sourceInfo),
          None
        )
        e.setStackTrace(stackTraceElements)
        e
      }
    )
  }

  /*private def timeoutAfter[T](timeout: Span, f: => T, interruptor: Signaler, exceptionFun: Option[Throwable] => StackDepthException): T = {
    val timer = new Timer()
    val task = new TimeoutTask(Thread.currentThread(), interruptor)
    timer.schedule(task, timeout.totalNanos / 1000 / 1000)
    try {
      val result = f
      timer.cancel()
      if (task.timedOut) {
        if (task.needToResetInterruptedStatus)
          Thread.interrupted() // To reset the flag probably. He only does this if it was not set before and was set after, I think.
        throw exceptionFun(None)
      }
      result
    }
    catch {
      case t: Throwable => 
        timer.cancel() // Duplicate code could be factored out I think. Maybe into a finally? Oh, not that doesn't work. So a method.
        if(task.timedOut) {
          if (task.needToResetInterruptedStatus)
            Thread.interrupted() // Clear the interrupt status (There's a race condition here, but not sure we an do anything about that.)
          throw exceptionFun(Some(t))
        }
        else
          throw t
    }
  }*/
  
}

/**
 * Companion object that facilitates the importing of <code>Timeouts</code> members as 
 * an alternative to mixing in the trait. One use case is to import <code>Timeouts</code>'s members so you can use
 * them in the Scala interpreter.
 */
object TimeLimits extends TimeLimits
