/*
 * Copyright 2001-2025 Artima, Inc.
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
import org.scalatest.exceptions.StackDepthExceptionHelper.posOrElseStackDepthFun
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
 * Trait that provides <code>failAfter</code> and <code>cancelAfter</code> methods, which allow you to specify a time limit for an
 * operation passed as a by-name parameter, as well as a way to signal it if the operation exceeds its time limit.
 *
 * <p>
 * The time limit is passed as the first parameter, as a <a href="../time/Span.html"><code>Span</code></a>. The operation is
 * passed as the second parameter. A <a href="Signaler.html"><code>Signaler</code></a>, a strategy for interrupting the operation, is
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
 * The above code will eventually produce a <a href="../exceptions/TestFailedDueToTimeoutException.html"><code>TestFailedDueToTimeoutException</code></a> with a message
 * that indicates a time limit has been exceeded:
 * </p>
 *
 * <p>
 * <code>The code passed to failAfter did not complete within 100 milliseconds.</code>
 * </p>
 *
 * <p>
 * If you use <code>cancelAfter</code> in place of <code>failAfter</code>, a <a href="../exceptions/TestCanceledException.html"><code>TestCanceledException</code></a> will be thrown
 * instead, also with a message that indicates a time limit has been exceeded:
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
 * invoked <code>failAfter</code> or <code>cancelAfter</code>, so that it can detect when the time limit has been exceeded and attempt to <em>signal</em>
 * the main thread. Because different operations can require different signaling strategies, the <code>failAfter</code> and <code>cancelAfter</code>
 * methods accept an implicit third parameter of type <code>Signaler</code> that is responsible for signaling
 * the main thread.
 * </p>
 *
 * <a name="signalerConfig"></a><h2>Configuring <code>failAfter</code> or <code>cancelAfter</code> with a <code>Signaler</code></h2>
 *
 * <p>
 * The <code>Signaler</code> companion object declares an implicit <code>val</code> of type <code>Signaler</code> that returns
 * a <code>DoNotSignal</code>. This serves as the default signaling strategy.
 * If you wish to use a different strategy, you can declare an implicit <code>val</code> that establishes a different <code>Signaler</code>
 * as the policy.  Here's an example
 * in which the default signaling strategy is changed to <a href="ThreadSignaler.html"><code>ThreadSignaler</code></a>, which does not attempt to
 * interrupt the main thread in any way:
 * </p>
 *
 * <pre class="stHighlight">
 * override val signaler: Signaler = ThreadSignaler
 * failAfter(100 millis) {
 *   Thread.sleep(500)
 * }
 * </pre>
 *
 * <p>
 * As with the default <code>Signaler</code>, the above code will eventually produce a 
 * <code>TestFailedDueToTimeoutException</code> with a message that indicates a timeout expired. However, instead
 * of throwing the exception after approximately 500 milliseconds, it will throw it after approximately 100 milliseconds.
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
 * <a href="DoNotSignal$.html">DoNotSignal</a>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * The default signaler, does not attempt to interrupt the main test thread in any way
 * </td>
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
 * a loop and can check a volatile flag each pass through the loop, you could write a <code>Signaler</code> that
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
   * Executes the passed function, enforcing the passed time limit by attempting to signal the operation if the
   * time limit is exceeded, and "failing" if the time limit has been 
   * exceeded after the function completes, where what it means to "fail" is determined by the implicitly passed <code>Timed[T]</code>
   * instance.
   *
   * <p>
   * The <a href= "../enablers/Timed$.html"><code>Timed</code></a> companion object offers three implicits, one for <code>FutureOutcome</code>, one for <code>Future[U]</code>
   * and one for any other type. The implicit <code>Timed[FutureOutcome]</code> defines failure as failing the <code>FutureOutcome</code> with a <code>TestFailedDueToTimeoutException</code>:
   * no exception will be thrown. The implicit <code>Timed[Future[U]]</code> defines failure as failing the <code>Future[U]</code> with a <code>TestFailedDueToTimeoutException</code>:
   * no exception will be thrown. The implicit for any other type defines failure as throwing
   * <code>TestFailedDueToTimeoutException</code>. For the details, see the Scaladoc of the implicit <code>Timed</code> providers
   * in the <a href= "../enablers/Timed$.html"><code>Timed</code></a> companion object.
   * </p>
   *
   * @param timeout the maximimum amount of time allowed for the passed operation
   * @param fun the operation on which to enforce the passed timeout
   * @param signaler a strategy for signaling the passed operation
   * @param prettifier a <code>Prettifier</code> for prettifying error messages
   * @param pos the <code>Position</code> of the caller site
   * @param timed the <code>Timed</code> type class that provides the behavior implementation of the timing restriction.
   */
  // SKIP-DOTTY-START 
  def failAfter[T](timeout: Span)(fun: => T)(implicit signaler: Signaler, prettifier: Prettifier = implicitly[Prettifier], pos: source.Position = implicitly[source.Position], timed: Timed[T] = implicitly[Timed[T]]): T = {
    TimeLimits.failAfterImpl(timeout, signaler, prettifier, Some(pos), getStackDepthFun(pos))(fun)(timed)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY final inline def failAfter[T](timeout: Span)(fun: => T)(implicit signaler: Signaler, prettifier: Prettifier = implicitly[Prettifier], timed: Timed[T] = implicitly[Timed[T]]): T = 
  //DOTTY-ONLY   ${ TimeLimits.failAfterMacro('{timeout}, '{signaler}, '{prettifier}, '{fun}, '{timed}) }

  // TODO: Consider creating a TestCanceledDueToTimeoutException
  /**
   * Executes the passed function, enforcing the passed time limit by attempting to signal the operation if the
   * time limit is exceeded, and "canceling" if the time limit has been 
   * exceeded after the function completes, where what it means to "cancel" is determined by the implicitly passed <code>Timed[T]</code>
   * instance.
   *
   * <p>
   * The <a href= "../enablers/Timed$.html"><code>Timed</code></a> companion object offers three implicits, one for <code>FutureOutcome</code>, one for <code>Future[U]</code>
   * and one for any other type. The implicit <code>Timed[FutureOutcome]</code> defines cancelation as canceling the <code>FutureOutcome</code>:
   * no exception will be thrown. The implicit <code>Timed[Future[U]]</code> defines canceling as failing the <code>Future[U]</code> with a <code>TestCanceledException</code>:
   * no exception will be thrown. The implicit for any other type defines failure as throwing
   * <code>TestCanceledException</code>. For the details, see the Scaladoc of the implicit <code>Timed</code> providers
   * in the <a href= "../enablers/Timed$.html"><code>Timed</code></a> companion object.
   * </p>
   *
   * @param timeout the maximimum amount of time allowed for the passed operation
   * @param f the operation on which to enforce the passed timeout
   * @param signaler a strategy for signaling the passed operation
   * @param prettifier a <code>Prettifier</code> for prettifying error messages
   * @param pos the <code>Position</code> of the caller site
   * @param timed the <code>Timed</code> type class that provides the behavior implementation of the timing restriction.
   */
  // SKIP-DOTTY-START 
  def cancelAfter[T](timeout: Span)(fun: => T)(implicit signaler: Signaler, prettifier: Prettifier = implicitly[Prettifier], pos: source.Position = implicitly[source.Position], timed: Timed[T] = implicitly[Timed[T]]): T = {
    TimeLimits.cancelAfterImpl(timeout, signaler, prettifier, Some(pos), getStackDepthFun(pos))(fun)(timed)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY final inline def cancelAfter[T](timeout: Span)(fun: => T)(implicit signaler: Signaler, prettifier: Prettifier = implicitly[Prettifier], timed: Timed[T] = implicitly[Timed[T]]): T = 
  //DOTTY-ONLY   ${ TimeLimits.cancelAfterMacro('{timeout}, '{signaler}, '{prettifier}, '{fun}, '{timed}) }
}

/**
 * Companion object that facilitates the importing of <code>Timeouts</code> members as 
 * an alternative to mixing in the trait. One use case is to import <code>Timeouts</code>'s members so you can use
 * them in the Scala interpreter.
 */
object TimeLimits extends TimeLimits {

  private[scalatest] def failAfterImpl[T](timeout: Span, signaler: Signaler, prettifier: Prettifier, pos: Option[source.Position], stackDepthFun: StackDepthException => Int)(fun: => T)(implicit timed: Timed[T]): T = {
    val stackTraceElements = Thread.currentThread.getStackTrace()
    timed.timeoutAfter(
      timeout,
      fun,
      signaler,
      (cause: Option[Throwable]) => {
        val e = new TestFailedDueToTimeoutException(
          (_: StackDepthException) => Some(FailureMessages.timeoutFailedAfter(prettifier, UnquotedString(timeout.prettyString))),
          cause,
          posOrElseStackDepthFun(pos, stackDepthFun),
          None,
          timeout
        )
        e.setStackTrace(stackTraceElements)
        e
      }
    )
  }

  //DOTTY-ONLY import scala.quoted.*

  //DOTTY-ONLY private[concurrent] def failAfterMacro[T](timeout: Expr[Span], signaler: Expr[Signaler], prettifier: Expr[Prettifier], fun: Expr[T], timed: Expr[Timed[T]])(using quotes: Quotes, typeT: Type[T]): Expr[T] = {
  //DOTTY-ONLY   source.Position.withPosition[T]('{(pos: source.Position) => failAfterImpl(${timeout}, ${signaler}, ${prettifier}, Some(pos), getStackDepthFun(pos))(${fun})(${timed}) })
  //DOTTY-ONLY }

  private[scalatest] def cancelAfterImpl[T](timeout: Span, signaler: Signaler, prettifier: Prettifier, pos: Option[source.Position], stackDepthFun: StackDepthException => Int)(fun: => T)(implicit timed: Timed[T]): T = {
    val stackTraceElements = Thread.currentThread.getStackTrace()
    timed.timeoutAfter(
      timeout,
      fun,
      signaler,
      (cause: Option[Throwable]) => {
        val e = new TestCanceledException(
          (_: StackDepthException) => Some(FailureMessages.timeoutCanceledAfter(prettifier, UnquotedString(timeout.prettyString))),
          cause,
          posOrElseStackDepthFun(pos, stackDepthFun),
          None
        )
        e.setStackTrace(stackTraceElements)
        e
      }
    )
  }

  //DOTTY-ONLY import scala.quoted.*

  //DOTTY-ONLY private[concurrent] def cancelAfterMacro[T](timeout: Expr[Span], signaler: Expr[Signaler], prettifier: Expr[Prettifier], fun: Expr[T], timed: Expr[Timed[T]])(using quotes: Quotes, typeT: Type[T]): Expr[T] = {
  //DOTTY-ONLY   source.Position.withPosition[T]('{(pos: source.Position) => cancelAfterImpl(${timeout}, ${signaler}, ${prettifier}, Some(pos), getStackDepthFun(pos))(${fun})(${timed}) })
  //DOTTY-ONLY }

}
