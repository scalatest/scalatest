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

import org.scalactic.source
import org.scalatest.Resources
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import org.scalatest.exceptions.{StackDepthException, TestCanceledException, TestFailedException, TestPendingException, TimeoutField}
import org.scalatest.time.Span
import scala.concurrent.Await
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Success
import Futures.FutureConcept

/**
 * Provides an implicit conversion from <code>scala.concurrent.Future[T]</code> to
 * <a href="Futures$FutureConcept.html"><code>FutureConcept[T]</code></a>.
 *
 * <p>
 * This trait enables you to invoke the methods defined on <code>FutureConcept</code> on a Scala <code>Future</code>, as well as to pass a Scala future
 * to the <code>whenReady</code> methods of supertrait <code>Futures</code>.
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
trait ScalaFutures extends Futures {

  // SKIP-DOTTY-START
  import scala.language.implicitConversions

  /**
   * Implicitly converts a <code>scala.concurrent.Future[T]</code> to
   * <code>FutureConcept[T]</code>, allowing you to invoke the methods
   * defined on <code>FutureConcept</code> on a Scala <code>Future</code>, as well as to pass a Scala future
   * to the <code>whenReady</code> methods of supertrait <a href="Futures.html"><code>Futures</code></a>.
   *
   * <p>
   * See the documentation for supertrait <a href="Futures.html"><code>Futures</code></a> for the details on the syntax this trait provides
   * for testing with Java futures.
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
   * <p>
   * The <code>isExpired</code> method of the returned <code>FutureConcept</code> will always return <code>false</code>, because
   * the underlying type, <code>scala.concurrent.Future</code>, does not support the notion of expiration. Likewise, the <code>isCanceled</code>
   * method of the returned <code>FutureConcept</code> will always return <code>false</code>, because
   * the underlying type, <code>scala.concurrent.Future</code>, does not support the notion of cancelation.
   * </p>
   *
   * @param scalaFuture a <code>scala.concurrent.Future[T]</code> to convert
   * @return a <code>FutureConcept[T]</code> wrapping the passed <code>scala.concurrent.Future[T]</code>
   */
  implicit def convertScalaFuture[T](scalaFuture: scala.concurrent.Future[T]): FutureConcept[T] =
    new ScalaFutures.ScalaFutureConcept(scalaFuture)
  // SKIP-DOTTY-END  

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Converts a <code>scala.concurrent.Future[T]</code> to
  //DOTTY-ONLY  * <code>FutureConcept[T]</code>, allowing you to invoke the methods
  //DOTTY-ONLY  * defined on <code>FutureConcept</code> on a Scala <code>Future</code>, as well as to pass a Scala future
  //DOTTY-ONLY  * to the <code>whenReady</code> methods of supertrait <a href="Futures.html"><code>Futures</code></a>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * <p>
  //DOTTY-ONLY  * See the documentation for supertrait <a href="Futures.html"><code>Futures</code></a> for the details on the syntax this trait provides
  //DOTTY-ONLY  * for testing with Java futures.
  //DOTTY-ONLY  * </p>
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * <p>
  //DOTTY-ONLY  * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
  //DOTTY-ONLY  * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
  //DOTTY-ONLY  * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
  //DOTTY-ONLY  * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
  //DOTTY-ONLY  * <code>ExecutionException</code>'s cause is <code>null</code>.
  //DOTTY-ONLY  * </p>
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * <p>
  //DOTTY-ONLY  * The <code>isExpired</code> method of the returned <code>FutureConcept</code> will always return <code>false</code>, because
  //DOTTY-ONLY  * the underlying type, <code>scala.concurrent.Future</code>, does not support the notion of expiration. Likewise, the <code>isCanceled</code>
  //DOTTY-ONLY  * method of the returned <code>FutureConcept</code> will always return <code>false</code>, because
  //DOTTY-ONLY  * the underlying type, <code>scala.concurrent.Future</code>, does not support the notion of cancelation.
  //DOTTY-ONLY  * </p>
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param scalaFuture a <code>scala.concurrent.Future[T]</code> to convert
  //DOTTY-ONLY  * @return a <code>FutureConcept[T]</code> wrapping the passed <code>scala.concurrent.Future[T]</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertScalaFuture[T](scalaFuture: scala.concurrent.Future[T]): FutureConcept[T] =
  //DOTTY-ONLY   new ScalaFutures.ScalaFutureConcept(scalaFuture)

  //SCALATESTJS,NATIVE-ONLY override private[concurrent] val jsAdjustment: Int = -1

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that convert a <code>scala.concurrent.Future[T]</code> to the
  //DOTTY-ONLY   * <a href="Futures$FutureConcept.html"><code>FutureConcept[T]</code></a>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * This allows Scala <code>Future</code>s to be passed to the <code>whenReady</code> methods of <code>Futures</code>.
  //DOTTY-ONLY   */
  //DOTTY-ONLY given[T]: Conversion[scala.concurrent.Future[T], FutureConcept[T]] = new ScalaFutures.ScalaFutureConcept(_)
  //DOTTY-ONLY import PatienceConfiguration.{Interval, Timeout}
  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Extension methods for <code>scala.concurrent.Future[T]</code> to support methods of <code>FutureConcept</code>.
  //DOTTY-ONLY   */
  //DOTTY-ONLY extension [T](scalaFuture: scala.concurrent.Future[T]) {
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Queries this future for its value.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the future is not ready, this method will return <code>None</code>. If ready, it will either return an exception
  //DOTTY-ONLY    * or a <code>T</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def eitherValue: Option[Either[Throwable, T]] = convertScalaFuture(scalaFuture).eitherValue
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Indicates whether this future has expired (timed out).
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * The timeout detected by this method is different from the timeout supported by <code>whenReady</code>. This timeout
  //DOTTY-ONLY    * is a timeout of the underlying future. If the underlying future does not support timeouts, this method must always
  //DOTTY-ONLY    * return <code>false</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def isExpired: Boolean = convertScalaFuture(scalaFuture).isExpired
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Indicates whether this future has been canceled.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the underlying future does not support the concept of cancellation, this method must always return <code>false</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def isCanceled: Boolean = convertScalaFuture(scalaFuture).isCanceled
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Indicates whether this future is ready within the specified timeout.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
  //DOTTY-ONLY    * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
  //DOTTY-ONLY    * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
  //DOTTY-ONLY    * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
  //DOTTY-ONLY    * <code>ExecutionException</code>'s cause is <code>null</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * @param timeout
  //DOTTY-ONLY    * @param config
  //DOTTY-ONLY    * @return
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def isReadyWithin(timeout: Span)(using config: PatienceConfig, pos: source.Position): Boolean = convertScalaFuture(scalaFuture).isReadyWithin(timeout)(config, pos)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
  //DOTTY-ONLY    * exception returned by the future (<em>i.e.</em>, <code>value</code> returned a <code>Left</code>)
  //DOTTY-ONLY    * or <code>TestFailedException</code>.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * The maximum amount of time to wait for the future to become ready before giving up and throwing
  //DOTTY-ONLY    * <code>TestFailedException</code> is configured by the value contained in the passed
  //DOTTY-ONLY    * <code>timeout</code> parameter.
  //DOTTY-ONLY    * The interval to sleep between queries of the future (used only if the future is polled) is configured by the value contained in the passed
  //DOTTY-ONLY    * <code>interval</code> parameter.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * This method invokes the overloaded <code>futureValue</code> form with only one (implicit) argument
  //DOTTY-ONLY    * list that contains only one argument, a <code>PatienceConfig</code>, passing a new
  //DOTTY-ONLY    * <code>PatienceConfig</code> with the <code>Timeout</code> specified as <code>timeout</code> and
  //DOTTY-ONLY    * the <code>Interval</code> specified as <code>interval</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
  //DOTTY-ONLY    * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
  //DOTTY-ONLY    * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
  //DOTTY-ONLY    * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
  //DOTTY-ONLY    * <code>ExecutionException</code>'s cause is <code>null</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * @param timeout the <code>Timeout</code> configuration parameter
  //DOTTY-ONLY    * @param interval the <code>Interval</code> configuration parameter
  //DOTTY-ONLY    * @return the result of the future once it is ready, if <code>value</code> is defined as a <code>Right</code>
  //DOTTY-ONLY    * @throws Throwable if once ready, the <code>value</code> of this future is defined as a
  //DOTTY-ONLY    *       <code>Left</code> (in this case, this method throws that same exception)
  //DOTTY-ONLY    * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
  //DOTTY-ONLY    *     the specified timeout has been exceeded
  //DOTTY-ONLY    */
  //DOTTY-ONLY   final def futureValue(timeout: Timeout, interval: Interval)(using pos: source.Position): T = convertScalaFuture(scalaFuture).futureValue(timeout, interval)(pos)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
  //DOTTY-ONLY    * exception returned by the future (<em>i.e.</em>, <code>value</code> returned a <code>Left</code>)
  //DOTTY-ONLY    * or <code>TestFailedException</code>.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * The maximum amount of time to wait for the future to become ready before giving up and throwing
  //DOTTY-ONLY    * <code>TestFailedException</code> is configured by the value contained in the passed
  //DOTTY-ONLY    * <code>timeout</code> parameter.
  //DOTTY-ONLY    * The interval to sleep between queries of the future (used only if the future is polled) is configured by the <code>interval</code> field of
  //DOTTY-ONLY    * the <code>PatienceConfig</code> passed implicitly as the last parameter.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * This method invokes the overloaded <code>futureValue</code> form with only one (implicit) argument
  //DOTTY-ONLY    * list that contains only one argument, a <code>PatienceConfig</code>, passing a new
  //DOTTY-ONLY    * <code>PatienceConfig</code> with the <code>Timeout</code> specified as <code>timeout</code> and
  //DOTTY-ONLY    * the <code>Interval</code> specified as <code>config.interval</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
  //DOTTY-ONLY    * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
  //DOTTY-ONLY    * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
  //DOTTY-ONLY    * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
  //DOTTY-ONLY    * <code>ExecutionException</code>'s cause is <code>null</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * @param timeout the <code>Timeout</code> configuration parameter
  //DOTTY-ONLY    * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
  //DOTTY-ONLY    *          <code>interval</code> parameters that are unused by this method
  //DOTTY-ONLY    * @return the result of the future once it is ready, if <code>eitherValue</code> is defined as a <code>Right</code>
  //DOTTY-ONLY    * @throws Throwable if once ready, the <code>eitherValue</code> of this future is defined as a
  //DOTTY-ONLY    *       <code>Left</code> (in this case, this method throws that same exception)
  //DOTTY-ONLY    * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
  //DOTTY-ONLY    *     the specified timeout has been exceeded
  //DOTTY-ONLY    */
  //DOTTY-ONLY   final def futureValue(timeout: Timeout)(using config: PatienceConfig, pos: source.Position): T = convertScalaFuture(scalaFuture).futureValue(timeout)(config, pos)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
  //DOTTY-ONLY    * exception returned by the future (<em>i.e.</em>, <code>eitherValue</code> returned a <code>Left</code>)
  //DOTTY-ONLY    * or <code>TestFailedException</code>.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * The maximum amount of time to wait for the future to become ready before giving up and throwing
  //DOTTY-ONLY    * <code>TestFailedException</code> is configured by the <code>timeout</code> field of
  //DOTTY-ONLY    * the <code>PatienceConfig</code> passed implicitly as the last parameter.
  //DOTTY-ONLY    * The interval to sleep between queries of the future (used only if the future is polled) is configured by the value contained in the passed
  //DOTTY-ONLY    * <code>interval</code> parameter.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * This method invokes the overloaded <code>futureValue</code> form with only one (implicit) argument
  //DOTTY-ONLY    * list that contains only one argument, a <code>PatienceConfig</code>, passing a new
  //DOTTY-ONLY    * <code>PatienceConfig</code> with the <code>Interval</code> specified as <code>interval</code> and
  //DOTTY-ONLY    * the <code>Timeout</code> specified as <code>config.timeout</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
  //DOTTY-ONLY    * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
  //DOTTY-ONLY    * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
  //DOTTY-ONLY    * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
  //DOTTY-ONLY    * <code>ExecutionException</code>'s cause is <code>null</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * @param interval the <code>Interval</code> configuration parameter
  //DOTTY-ONLY    * @param config an <code>PatienceConfig</code> object containing <code>timeout</code> and
  //DOTTY-ONLY    *          <code>interval</code> parameters that are unused by this method
  //DOTTY-ONLY    * @return the result of the future once it is ready, if <code>value</code> is defined as a <code>Right</code>
  //DOTTY-ONLY    * @throws Throwable if once ready, the <code>value</code> of this future is defined as a
  //DOTTY-ONLY    *       <code>Left</code> (in this case, this method throws that same exception)
  //DOTTY-ONLY    * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
  //DOTTY-ONLY    *     the specified timeout has been exceeded
  //DOTTY-ONLY    */
  //DOTTY-ONLY   final def futureValue(interval: Interval)(using config: PatienceConfig, pos: source.Position): T = convertScalaFuture(scalaFuture).futureValue(interval)(config, pos)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Returns the result of this <code>FutureConcept</code>, once it is ready, or throws either the
  //DOTTY-ONLY    * exception returned by the future (<em>i.e.</em>, <code>futureValue</code> returned a <code>Left</code>)
  //DOTTY-ONLY    * or <code>TestFailedException</code>.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * This trait's implementation of this method queries the future repeatedly until it either is
  //DOTTY-ONLY    * ready, or a configured maximum amount of time has passed, sleeping a configured interval between
  //DOTTY-ONLY    * attempts; and when ready, returns the future's value. For greater efficiency, implementations of
  //DOTTY-ONLY    * this trait may override this method so that it blocks the specified timeout while waiting for
  //DOTTY-ONLY    * the result, if the underlying future supports this.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * The maximum amount of time to wait for the future to become ready before giving up and throwing
  //DOTTY-ONLY    * <code>TestFailedException</code> is configured by the <code>timeout</code> field of
  //DOTTY-ONLY    * the <code>PatienceConfig</code> passed implicitly as the last parameter.
  //DOTTY-ONLY    * The interval to sleep between queries of the future (used only if the future is polled) is configured by the <code>interval</code> field of
  //DOTTY-ONLY    * the <code>PatienceConfig</code> passed implicitly as the last parameter.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the <code>eitherValue</code> method of the underlying Scala future returns a <code>scala.Some</code> containing a
  //DOTTY-ONLY    * <code>scala.util.Failure</code> containing a <code>java.util.concurrent.ExecutionException</code>, and this
  //DOTTY-ONLY    * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The
  //DOTTY-ONLY    * <code>ExecutionException</code> will be be included as the <code>TestFailedException</code>'s cause only if the
  //DOTTY-ONLY    * <code>ExecutionException</code>'s cause is <code>null</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * @param config a <code>PatienceConfig</code> object containing <code>timeout</code> and
  //DOTTY-ONLY    *          <code>interval</code> parameters that are unused by this method
  //DOTTY-ONLY    * @return the result of the future once it is ready, if <code>value</code> is defined as a <code>Right</code>
  //DOTTY-ONLY    * @throws Throwable if once ready, the <code>value</code> of this future is defined as a
  //DOTTY-ONLY    *       <code>Left</code> (in this case, this method throws that same exception)
  //DOTTY-ONLY    * @throws TestFailedException if the future is cancelled, expires, or is still not ready after
  //DOTTY-ONLY    *     the specified timeout has been exceeded
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def futureValue(using config: PatienceConfig, pos: source.Position): T = convertScalaFuture(scalaFuture).futureValue(config, pos)
  //DOTTY-ONLY }

}

/**
 * Companion object that facilitates the importing of <code>ScalaFutures</code> members as
 * an alternative to mixing in the trait. One use case is to import <code>ScalaFutures</code>'s members so you can use
 * them in the Scala interpreter.
 */
object ScalaFutures extends ScalaFutures {
  private final class ScalaFutureConcept[T](scalaFuture: scala.concurrent.Future[T]) extends FutureConcept[T] {
    def eitherValue: Option[Either[Throwable, T]] =
        scalaFuture.value.map {
          case Success(o) => Right(o)
          case Failure(e) => Left(e)
        }
    def isExpired: Boolean = false // Scala Futures themselves don't support the notion of a timeout
    def isCanceled: Boolean = false // Scala Futures don't seem to be cancelable either

    override private[concurrent] def futureValueImpl(pos: source.Position)(implicit config: PatienceConfig): T = {
      try {
        val result: Either[Throwable, T] = 
        // SKIP-SCALATESTJS-START
        if (scalaFuture.isCompleted)
          scalaFuture.value.get.transform(s => Success(Right(s)), f => Success(Left(f))).get
        else
          Await.ready(scalaFuture, Duration.fromNanos(config.timeout.totalNanos)).eitherValue.get
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY scalaFuture.value.getOrElse(throw new TimeoutException("Cannot Await or block in Scala.js.")).transform(s => Success(Right(s)), f => Success(Left(f))).get

        result match {  
          case Right(v) => v
          case Left(tpe: TestPendingException) => throw tpe
          case Left(tce: TestCanceledException) => throw tce
          case Left(e) if anExceptionThatShouldCauseAnAbort(e) => throw e
          case Left(ee: java.util.concurrent.ExecutionException) if ee.getCause != null =>
            val cause = ee.getCause
            cause match {
              case tpe: TestPendingException => throw tpe
              case tce: TestCanceledException => throw tce
              case e if anExceptionThatShouldCauseAnAbort(e) => throw e
              case _ =>
                throw new TestFailedException(
                  (_: StackDepthException) => Some {
                    if (cause.getMessage == null)
                      Resources.futureReturnedAnException(cause.getClass.getName)
                    else
                      Resources.futureReturnedAnExceptionWithMessage(cause.getClass.getName, cause.getMessage)
                  },
                  Some(cause),
                  pos
                )
            }
          case Left(e) =>
            throw new TestFailedException(
              (_: StackDepthException) => Some {
                if (e.getMessage == null)
                  Resources.futureReturnedAnException(e.getClass.getName)
                else
                  Resources.futureReturnedAnExceptionWithMessage(e.getClass.getName, e.getMessage)
              },
              Some(e),
              pos
            )
        }
      }
      catch {
        case e: TimeoutException => 
          throw new TestFailedException(
                (_: StackDepthException) => Some(Resources.wasNeverReady(config.timeout.prettyString)),
                None,
                pos
              ) with TimeoutField {
                val timeout: Span = config.timeout
              }
      }
    }
  }
}
