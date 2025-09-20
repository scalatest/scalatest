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

import org.scalatest._
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepth
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import scala.annotation.tailrec
import org.scalatest.time.Span
import exceptions.{TestCanceledException, TestFailedException, TestPendingException, TimeoutField}
import PatienceConfiguration._
import org.scalactic.source
import exceptions.StackDepthException

/**
 * Trait that facilitates testing with futures.
 *
 * <p>
 * This trait defines a <a href="Futures$FutureConcept.html"><code>FutureConcept</code></a> trait that can be used to implicitly wrap
 * different kinds of futures, thereby providing a uniform testing API for futures.
 * The three ways this trait enables you to test futures are:
 * </p>
 *
 * <p>
 * 1. Invoking <code>isReadyWithin</code>, to assert that a future is ready within a a specified time period.
 * Here's an example:
 * </p>
 * 
 * <pre class="stHighlight">
 * assert(result.isReadyWithin(100 millis))
 * </pre>
 * 
 * <p>
 * 2. Invoking <code>futureValue</code>, to obtain a futures result within a specified or implicit time period,
 * like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * assert(result.futureValue === 7)
 *
 * // Or, if you expect the future to fail:
 * assert(result.failed.futureValue.isInstanceOf[ArithmeticException])
 * </pre>
 * 
 * <p>
 * 3. Passing the future to <code>whenReady</code>, and performing assertions on the result value passed
 * to the given function, as in:
 * </p>
 * 
 * <pre class="stHighlight">
 * whenReady(result) { s =&gt;
 *   s should be ("hello")
 * }
 * </pre>
 *
 * <p>
 * The <code>whenReady</code> construct periodically inspects the passed
 * future, until it is either ready or the configured timeout has been surpassed. If the future becomes
 * ready before the timeout, <code>whenReady</code> passes the future's value to the specified function.
 * </p>
 *
 * <p>
 * To make <code>whenReady</code> more broadly applicable, the type of future it accepts is a <code>FutureConcept[T]</code>,
 * where <code>T</code> is the type of value promised by the future. Passing a future to <code>whenReady</code> requires
 * an implicit conversion from the type of future you wish to pass (the <em>modeled type</em>) to
 * <code>FutureConcept[T]</code>. Subtrait <code>JavaFutures</code> provides an implicit conversion from
 * <code>java.util.concurrent.Future[T]</code> to <code>FutureConcept[T]</code>.
 * </p>
 *
 * <p>
 * For example, the following invocation of <code>whenReady</code> would succeed (not throw an exception):
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import Matchers._
 * import concurrent.Futures._
 * import java.util.concurrent._
 * 
 * val exec = Executors.newSingleThreadExecutor
 * val task = new Callable[String] { def call() = { Thread.sleep(50); "hi" } }
 * whenReady(exec.submit(task)) { s =&gt;
 *   s should be ("hi")
 * }
 * </pre>
 *
 * <p>
 * However, because the default timeout is 150 milliseconds, the following invocation of
 * <code>whenReady</code> would ultimately produce a <code>TestFailedException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val task = new Callable[String] { def call() = { Thread.sleep(500); "hi" } }
 * whenReady(exec.submit(task)) { s =&gt;
 *   s should be ("hi")
 * }
 * </pre>
 *
 * <p>
 * Assuming the default configuration parameters, a <code>timeout</code> of 150 milliseconds and an
 * <code>interval</code> of 15 milliseconds,
 * were passed implicitly to <code>whenReady</code>, the detail message of the thrown
 * <code>TestFailedException</code> would look like:
 * </p>
 *
 * <p>
 * <code>The future passed to whenReady was never ready, so whenReady timed out. Queried 95 times, sleeping 10 milliseconds between each query.</code>
 * </p>
 *
 * <a name="defaultPatience"></a><h2>Configuration of <code>whenReady</code></h2>
 *
 * <p>
 * The <code>whenReady</code> methods of this trait can be flexibly configured.
 * The two configuration parameters for <code>whenReady</code> along with their 
 * default values and meanings are described in the following table:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong>Configuration Parameter</strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong>Default Value</strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong>Meaning</strong>
 * </th>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * timeout
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * scaled(150 milliseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * the maximum amount of time to allow unsuccessful queries before giving up and throwing <code>TestFailedException</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * interval
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * scaled(15 milliseconds)
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * the amount of time to sleep between each query
 * </td>
 * </tr>
 * </table>
 *
 * <p>
 * The default values of both timeout and interval are passed to the <code>scaled</code> method, inherited
 * from <code>ScaledTimeSpans</code>, so that the defaults can be scaled up
 * or down together with other scaled time spans. See the documentation for trait <a href="ScaledTimeSpans.html"><code>ScaledTimeSpans</code></a>
 * for more information.
 * </p>
 *
 * <p>
 * The <code>whenReady</code> methods of trait <code>Futures</code> each take a <code>PatienceConfig</code>
 * object as an implicit parameter. This object provides values for the two configuration parameters. Trait
 * <code>Futures</code> provides an implicit <code>val</code> named <code>defaultPatience</code> with each
 * configuration parameter set to its default value. 
 * If you want to set one or more configuration parameters to a different value for all invocations of
 * <code>whenReady</code> in a suite you can override this
 * val (or hide it, for example, if you are importing the members of the <code>Futures</code> companion object rather
 * than mixing in the trait). For example, if
 * you always want the default <code>timeout</code> to be 2 seconds and the default <code>interval</code> to be 5 milliseconds, you
 * can override <code>defaultPatience</code>, like this:
 *
 * <pre class="stHighlight">
 * implicit override val defaultPatience =
 *   PatienceConfig(timeout = Span(2, Seconds), interval = Span(5, Millis))
 * </pre>
 *
 * <p>
 * Or, hide it by declaring a variable of the same name in whatever scope you want the changed values to be in effect:
 * </p>
 *
 * <pre class="stHighlight">
 * implicit val defaultPatience =
 *   PatienceConfig(timeout =  Span(2, Seconds), interval = Span(5, Millis))
 * </pre>
 *
 * <p>
 * In addition to taking a <code>PatienceConfig</code> object as an implicit parameter, the <code>whenReady</code> methods of trait
 * <code>Futures</code> include overloaded forms that take one or two <code>PatienceConfigParam</code>
 * objects that you can use to override the values provided by the implicit <code>PatienceConfig</code> for a single <code>whenReady</code>
 * invocation. For example, if you want to set <code>timeout</code> to 6 seconds for just one particular <code>whenReady</code> invocation,
 * you can do so like this:
 * </p>
 *
 * <pre class="stHighlight">
 * whenReady (exec.submit(task), timeout(Span(6, Seconds))) { s =&gt;
 *   s should be ("hi")
 * }
 * </pre>
 *
 * <p>
 * This invocation of <code>eventually</code> will use 6000 for <code>timeout</code> and whatever value is specified by the 
 * implicitly passed <code>PatienceConfig</code> object for the <code>interval</code> configuration parameter.
 * If you want to set both configuration parameters in this way, just list them separated by commas:
 * </p>
 * 
 * <pre class="stHighlight">
 * whenReady (exec.submit(task), timeout(Span(6, Seconds)), interval(Span(500, Millis))) { s =&gt;
 *   s should be ("hi")
 * }
 * </pre>
 *
 * <p>
 * You can also import or mix in the members of <a href="../time/SpanSugar.html"><code>SpanSugar</code></a> if
 * you want a more concise DSL for expressing time spans:
 * </p>
 *
 * <pre class="stHighlight">
 * whenReady (exec.submit(task), timeout(6 seconds), interval(500 millis)) { s =&gt;
 *   s should be ("hi")
 * }
 * </pre>
 *
 * <p>
 * <em>Note: The <code>whenReady</code> construct was in part inspired by the <code>whenDelivered</code> matcher of the 
 * <a href="http://github.com/jdegoes/blueeyes" target="_blank">BlueEyes</a> project, a lightweight, asynchronous web framework for Scala.</em>
 * </p>
 *
 * @author Bill Venners
 */
trait Futures extends PatienceConfiguration {

  import Futures.FutureConcept

  private[concurrent] val jsAdjustment: Int = 0

  //DOTTY-ONLY import scala.quoted._

  /**
   * Queries the passed future repeatedly until it either is ready, or a configured maximum
   * amount of time has passed, sleeping a configured interval between attempts; and when ready, passes the future's value
   * to the passed function.
   *
   * <p>
   * The maximum amount of time to tolerate unsuccessful queries before giving up and throwing
   * <code>TestFailedException</code> is configured by the value contained in the passed
   * <code>timeout</code> parameter.
   * The interval to sleep between attempts is configured by the value contained in the passed
   * <code>interval</code> parameter.
   * </p>
   *
   * <p>
   * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
   * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
   * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
   * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
   * <code>ExecutionException</code>'s cause is <code>null</code>.
   * </p>
   *
   * @param future the future to query
   * @param timeout the <code>Timeout</code> configuration parameter
   * @param interval the <code>Interval</code> configuration parameter
   * @param fun the function to which pass the future's value when it is ready
   * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
   *          <code>interval</code> parameters that are unused by this method
   * @return the result of invoking the <code>fun</code> parameter
   */
  // SKIP-DOTTY-START 
  final def whenReady[T, U](future: FutureConcept[T], timeout: Timeout, interval: Interval)(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
    Futures.whenReadyImpl(future, fun, timeout.value, interval.value, pos)
  // SKIP-DOTTY-END
  //DOTTY-ONLY final inline def whenReady[T, U](future: FutureConcept[T], timeout: Timeout, interval: Interval)(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
  //DOTTY-ONLY   ${ Futures.whenReadyMacro('{future}, '{fun}, '{timeout.value}, '{interval.value}) }    
    
  /**
   * Queries the passed future repeatedly until it either is ready, or a configured maximum
   * amount of time has passed, sleeping a configured interval between attempts; and when ready, passes the future's value
   * to the passed function.
   *
   * <p>
   * The maximum amount of time in milliseconds to tolerate unsuccessful queries before giving up and throwing
   * <code>TestFailedException</code> is configured by the value contained in the passed
   * <code>timeout</code> parameter.
   * The interval to sleep between attempts is configured by the <code>interval</code> field of
   * the <code>PatienceConfig</code> passed implicitly as the last parameter.
   * </p>
   *
   * <p>
   * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
   * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
   * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
   * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
   * <code>ExecutionException</code>'s cause is <code>null</code>.
   * </p>
   *
   * @param future the future to query
   * @param timeout the <code>Timeout</code> configuration parameter
   * @param fun the function to which pass the future's value when it is ready
   * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
   *          <code>interval</code> parameters that are unused by this method
   * @return the result of invoking the <code>fun</code> parameter
   */
  // SKIP-DOTTY-START 
  final def whenReady[T, U](future: FutureConcept[T], timeout: Timeout)(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
    Futures.whenReadyImpl(future, fun, timeout.value, config.interval, pos)
  // SKIP-DOTTY-END
  //DOTTY-ONLY final inline def whenReady[T, U](future: FutureConcept[T], timeout: Timeout)(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
  //DOTTY-ONLY   ${ Futures.whenReadyMacro('{future}, '{fun}, '{timeout.value}, '{config.interval}) }  

  /**
   * Queries the passed future repeatedly until it either is ready, or a configured maximum
   * amount of time has passed, sleeping a configured interval between attempts; and when ready, passes the future's value
   * to the passed function.
   *
   * <p>
   * The maximum amount of time in milliseconds to tolerate unsuccessful attempts before giving up is configured by the <code>timeout</code> field of
   * the <code>PatienceConfig</code> passed implicitly as the last parameter.
   * The interval to sleep between attempts is configured by the value contained in the passed
   * <code>interval</code> parameter.
   * </p>
   *
   * @param future the future to query
   * @param interval the <code>Interval</code> configuration parameter
   * @param fun the function to which pass the future's value when it is ready
   * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
   *          <code>interval</code> parameters that are unused by this method
   * @return the result of invoking the <code>fun</code> parameter
   */
  // SKIP-DOTTY-START 
  final def whenReady[T, U](future: FutureConcept[T], interval: Interval)(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
    Futures.whenReadyImpl(future, fun, config.timeout, interval.value, pos)
  // SKIP-DOTTY-END
  //DOTTY-ONLY final inline def whenReady[T, U](future: FutureConcept[T], interval: Interval)(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
  //DOTTY-ONLY   ${ Futures.whenReadyMacro('{future}, '{fun}, '{config.timeout}, '{interval.value}) }
  
  /**
   * Queries the passed future repeatedly until it either is ready, or a configured maximum
   * amount of time has passed, sleeping a configured interval between attempts; and when ready, passes the future's value
   * to the passed function.
   *
   * <p>
   * The maximum amount of time in milliseconds to tolerate unsuccessful attempts before giving up is configured by the <code>timeout</code> field of
   * the <code>PatienceConfig</code> passed implicitly as the last parameter.
   * The interval to sleep between attempts is configured by the <code>interval</code> field of
   * the <code>PatienceConfig</code> passed implicitly as the last parameter.
   * </p>
   *
   * <p>
   * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
   * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
   * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
   * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
   * <code>ExecutionException</code>'s cause is <code>null</code>.
   * </p>
   *
   *
   * @param future the future to query
   * @param fun the function to which pass the future's value when it is ready
   * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
   *          <code>interval</code> parameters that are unused by this method
   * @return the result of invoking the <code>fun</code> parameter
   */
  // SKIP-DOTTY-START
  final def whenReady[T, U](future: FutureConcept[T])(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
    Futures.whenReadyImpl(future, fun, config.timeout, config.interval, pos)
  // SKIP-DOTTY-END
  //DOTTY-ONLY final inline def whenReady[T, U](future: FutureConcept[T])(fun: T => U)(implicit config: PatienceConfig, pos: source.Position): U = 
  //DOTTY-ONLY   ${ Futures.whenReadyMacro('{future}, '{fun}, '{config.timeout}, '{config.interval}) }

  /**/
}

object Futures extends Futures {

  /**
   * Concept trait for futures, instances of which are passed to the <code>whenReady</code>
   * methods of trait <a href="Futures.html"><code>Futures</code></a>.
   *
   * <p>
   * See the documentation for trait <a href="Futures.html"><code>Futures</code></a> for the details on the syntax this trait
   * provides for testing with futures.
   * </p>
   *
   * @author Bill Venners
   */
  trait FutureConcept[T] { thisFuture =>

    /**
     * Queries this future for its value.
     *
     * <p>
     * If the future is not ready, this method will return <code>None</code>. If ready, it will either return an exception
     * or a <code>T</code>.
     * </p>
     */
    def eitherValue: Option[Either[Throwable, T]]

    /**
     * Indicates whether this future has expired (timed out).
     *
     * <p>
     * The timeout detected by this method is different from the timeout supported by <code>whenReady</code>. This timeout
     * is a timeout of the underlying future. If the underlying future does not support timeouts, this method must always
     * return <code>false</code>.
     * </p>
     */
    def isExpired: Boolean

    /**
     * Indicates whether this future has been canceled.
     *
     * <p>
     * If the underlying future does not support the concept of cancellation, this method must always return <code>false</code>.
     * </p>
     */
    def isCanceled: Boolean

    /**
     * Indicates whether this future is ready within the specified timeout.
     *
     * <p>
     * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
     * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
     * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
     * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
     * <code>ExecutionException</code>'s cause is <code>null</code>.
     * </p>
     *
     * @param timeout
     * @param config
     * @return
     */
    final def isReadyWithin(timeout: Span)(implicit config: PatienceConfig, pos: source.Position): Boolean = {
      try {
        futureValueImpl(pos)(PatienceConfig(timeout, config.interval))
        true
      }
      catch {
        case e: TimeoutField => false
      }
    }

    /**
     * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
     * exception returned by the future (<em>i.e.</em>, <code>value</code> returned a <code>Left</code>)
     * or <code>TestFailedException</code>.
     *
     * <p>
     * The maximum amount of time to wait for the future to become ready before giving up and throwing
     * <code>TestFailedException</code> is configured by the value contained in the passed
     * <code>timeout</code> parameter.
     * The interval to sleep between queries of the future (used only if the future is polled) is configured by the value contained in the passed
     * <code>interval</code> parameter.
     * </p>
     *
     * <p>
     * This method invokes the overloaded <code>futureValue</code> form with only one (implicit) argument
     * list that contains only one argument, a <code>PatienceConfig</code>, passing a new
     * <code>PatienceConfig</code> with the <code>Timeout</code> specified as <code>timeout</code> and
     * the <code>Interval</code> specified as <code>interval</code>.
     * </p>
     *
     * <p>
     * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
     * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
     * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
     * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
     * <code>ExecutionException</code>'s cause is <code>null</code>.
     * </p>
     *
     * @param timeout the <code>Timeout</code> configuration parameter
     * @param interval the <code>Interval</code> configuration parameter
     * @return the result of the future once it is ready, if <code>value</code> is defined as a <code>Right</code>
     * @throws Throwable if once ready, the <code>value</code> of this future is defined as a
     *       <code>Left</code> (in this case, this method throws that same exception)
     * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
     *     the specified timeout has been exceeded
     */
    final def futureValue(timeout: Timeout, interval: Interval)(implicit pos: source.Position): T = {
      futureValueImpl(pos)(PatienceConfig(timeout.value, interval.value))
    }

    /**
     * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
     * exception returned by the future (<em>i.e.</em>, <code>value</code> returned a <code>Left</code>)
     * or <code>TestFailedException</code>.
     *
     * <p>
     * The maximum amount of time to wait for the future to become ready before giving up and throwing
     * <code>TestFailedException</code> is configured by the value contained in the passed
     * <code>timeout</code> parameter.
     * The interval to sleep between queries of the future (used only if the future is polled) is configured by the <code>interval</code> field of
     * the <code>PatienceConfig</code> passed implicitly as the last parameter.
     * </p>
     *
     * <p>
     * This method invokes the overloaded <code>futureValue</code> form with only one (implicit) argument
     * list that contains only one argument, a <code>PatienceConfig</code>, passing a new
     * <code>PatienceConfig</code> with the <code>Timeout</code> specified as <code>timeout</code> and
     * the <code>Interval</code> specified as <code>config.interval</code>.
     * </p>
     *
     * <p>
     * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
     * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
     * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
     * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
     * <code>ExecutionException</code>'s cause is <code>null</code>.
     * </p>
     *
     * @param timeout the <code>Timeout</code> configuration parameter
     * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
     *          <code>interval</code> parameters that are unused by this method
     * @return the result of the future once it is ready, if <code>eitherValue</code> is defined as a <code>Right</code>
     * @throws Throwable if once ready, the <code>eitherValue</code> of this future is defined as a
     *       <code>Left</code> (in this case, this method throws that same exception)
     * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
     *     the specified timeout has been exceeded
     */
    final def futureValue(timeout: Timeout)(implicit config: PatienceConfig, pos: source.Position): T = {
      futureValueImpl(pos)(PatienceConfig(timeout.value, config.interval))
    }

    /**
     * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
     * exception returned by the future (<em>i.e.</em>, <code>eitherValue</code> returned a <code>Left</code>)
     * or <code>TestFailedException</code>.
     *
     * <p>
     * The maximum amount of time to wait for the future to become ready before giving up and throwing
     * <code>TestFailedException</code> is configured by the <code>timeout</code> field of
     * the <code>PatienceConfig</code> passed implicitly as the last parameter.
     * The interval to sleep between queries of the future (used only if the future is polled) is configured by the value contained in the passed
     * <code>interval</code> parameter.
     * </p>
     *
     * <p>
     * This method invokes the overloaded <code>futureValue</code> form with only one (implicit) argument
     * list that contains only one argument, a <code>PatienceConfig</code>, passing a new
     * <code>PatienceConfig</code> with the <code>Interval</code> specified as <code>interval</code> and
     * the <code>Timeout</code> specified as <code>config.timeout</code>.
     * </p>
     *
     * <p>
     * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
     * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
     * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
     * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
     * <code>ExecutionException</code>'s cause is <code>null</code>.
     * </p>
     *
     * @param interval the <code>Interval</code> configuration parameter
     * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
     *          <code>interval</code> parameters that are unused by this method
     * @return the result of the future once it is ready, if <code>value</code> is defined as a <code>Right</code>
     * @throws Throwable if once ready, the <code>value</code> of this future is defined as a
     *       <code>Left</code> (in this case, this method throws that same exception)
     * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
     *     the specified timeout has been exceeded
     */
    final def futureValue(interval: Interval)(implicit config: PatienceConfig, pos: source.Position): T = {
      futureValueImpl(pos)(PatienceConfig(config.timeout, interval.value))
    }

    /**
     * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
     * exception returned by the future (<em>i.e.</em>, <code>futureValue</code> returned a <code>Left</code>)
     * or <code>TestFailedException</code>.
     *
     * <p>
     * This trait's implementation of this method queries the future repeatedly until it either is
     * ready, or a configured maximum amount of time has passed, sleeping a configured interval between
     * attempts; and when ready, returns the future's value. For greater efficiency, implementations of
     * this trait may override this method so that it blocks the specified timeout while waiting for
     * the result, if the underlying future supports this.
     * </p>
     *
     * <p>
     * The maximum amount of time to wait for the future to become ready before giving up and throwing
     * <code>TestFailedException</code> is configured by the <code>timeout</code> field of
     * the <code>PatienceConfig</code> passed implicitly as the last parameter.
     * The interval to sleep between queries of the future (used only if the future is polled) is configured by the <code>interval</code> field of
     * the <code>PatienceConfig</code> passed implicitly as the last parameter.
     * </p>
     *
     * <p>
     * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
     * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
     * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
     * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
     * <code>ExecutionException</code>'s cause is <code>null</code>.
     * </p>
     *
     * @param config a <code>PatienceConfig</code> object containing <code>timeout</code> and
     *          <code>interval</code> parameters that are unused by this method
     * @return the result of the future once it is ready, if <code>value</code> is defined as a <code>Right</code>
     * @throws Throwable if once ready, the <code>value</code> of this future is defined as a
     *       <code>Left</code> (in this case, this method throws that same exception)
     * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
     *     the specified timeout has been exceeded
     */
    def futureValue(implicit config: PatienceConfig, pos: source.Position): T = 
      futureValueImpl(pos)(config)

    private[concurrent] def futureValueImpl(pos: source.Position)(implicit config: PatienceConfig): T
  }

  final def whenReadyImpl[T, U](future: FutureConcept[T], fun: T => U, timeout: Span, interval: Span, pos: source.Position): U = {
    val result = future.futureValueImpl(pos)(PatienceConfig(timeout, interval))
    fun(result)
  }

  //DOTTY-ONLY import scala.quoted.*

  //DOTTY-ONLY final def workaroundWhenReadyImpl[T, U](future: FutureConcept[T], fun: T => U, timeout: Span, interval: Span, pos: source.Position): U = 
  //DOTTY-ONLY   whenReadyImpl(future.asInstanceOf[FutureConcept[T]], fun, timeout, interval, pos)

  //DOTTY-ONLY // Ideally, we can use future: Expr[FutureConcept[T]] and fun Expr[T => U] here, can't get it to work so we have the above workaroundWhenReadyImpl that takes Any.
  //DOTTY-ONLY private[concurrent] def whenReadyMacro[T, U](future: Expr[FutureConcept[T]], fun: Expr[T => U], timeout: Expr[Span], interval: Expr[Span])(using quotes: Quotes, typeT: Type[T], typeU: Type[U]): Expr[U] = {
  //DOTTY-ONLY   source.Position.withPosition[U]('{(pos: source.Position) => workaroundWhenReadyImpl(${future}, ${fun}, ${timeout}, ${interval}, pos) })
  //DOTTY-ONLY }

}