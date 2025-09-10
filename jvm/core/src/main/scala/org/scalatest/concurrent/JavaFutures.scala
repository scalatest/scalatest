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

import org.scalactic._
import java.util.concurrent.{TimeUnit, Future => FutureOfJava}
import org.scalatest.Resources
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.{TestPendingException, TestFailedException, TimeoutField}
import org.scalatest.time.Span
import Futures.FutureConcept

/**
 * Provides an implicit conversion from <code>java.util.concurrent.Future[T]</code> to
 * <a href="Futures$FutureConcept.html"><code>FutureConcept[T]</code></a>.
 *
 * <p>
 * This trait enables you to invoke the methods defined on <code>FutureConcept</code> on a Java <code>Future</code>, as well as to pass a Java future
 * to the <code>whenReady</code> methods of supertrait <code>Futures</code>.
 * See the documentation for supertrait <a href="Futures.html"><code>Futures</code></a> for the details on the syntax this trait provides
 * for testing with Java futures.
 * </p>
 * 
 * @author Bill Venners
 */
trait JavaFutures extends Futures {

  import JavaFutures._

  // SKIP-DOTTY-START
  import scala.language.implicitConversions

  /**
   * Implicitly converts a <code>java.util.concurrent.Future[T]</code> to
   * <code>FutureConcept[T]</code>, allowing you to invoke the methods
   * defined on <code>FutureConcept</code> on a Java <code>Future</code>, as well as to pass a Java future
   * to the <code>whenReady</code> methods of supertrait <a href="Futures.html"><code>Futures</code></a>.
   *
   * <p>
   * See the documentation for supertrait <a href="Futures.html"><code>Futures</code></a> for the details on the syntax this trait provides
   * for testing with Java futures.
   * </p>
   *
   * <p>If the <code>get</code> method of the underlying Java future throws <code>java.util.concurrent.ExecutionException</code>, and this
   * exception contains a non-<code>null</code> cause, that cause will be included in the <code>TestFailedException</code> as its cause. The <code>ExecutionException</code>
   * will be be included as the <code>TestFailedException</code>'s cause only if the <code>ExecutionException</code>'s cause is <code>null</code>.
   * </p>
   *
   * <p>
   * The <code>isExpired</code> method of the returned <code>FutureConcept</code> will always return <code>false</code>, because
   * the underlying type, <code>java.util.concurrent.Future</code>, does not support the notion of a timeout. The <code>isCanceled</code>
   * method of the returned <code>FutureConcept</code> will return the result of invoking <code>isCancelled</code> on the underlying
   * <code>java.util.concurrent.Future</code>.
   * </p>
   *
   * @param javaFuture a <code>java.util.concurrent.Future[T]</code> to convert
   * @return a <code>FutureConcept[T]</code> wrapping the passed <code>java.util.concurrent.Future[T]</code>
   */
  implicit def convertJavaFuture[T](javaFuture: FutureOfJava[T]): FutureConcept[T] = new FutureConceptImpl[T](javaFuture)
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert <code>java.util.concurrent.Future[T]</code> to <code>FutureConcept[T]</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertJavaFuture[T](javaFuture: FutureOfJava[T]): FutureConcept[T] = new FutureConceptImpl[T](javaFuture)
  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that convert a <code>java.util.concurrent.Future[T]</code> to the
  //DOTTY-ONLY   * <a href="Futures$FutureConcept.html"><code>FutureConcept[T]</code></a>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * This allows Java <code>Future</code>s to be passed to the <code>whenReady</code> methods of <code>Futures</code>.
  //DOTTY-ONLY   */
  //DOTTY-ONLY given[T]: Conversion[FutureOfJava[T], FutureConcept[T]] with {
  //DOTTY-ONLY   def apply(javaFuture: FutureOfJava[T]): FutureConcept[T] = new FutureConceptImpl[T](javaFuture)
  //DOTTY-ONLY }

  //DOTTY-ONLY import PatienceConfiguration.{Interval, Timeout}
  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension methods for <code>java.util.concurrent.Future[T]</code> to support methods of <code>FutureConcept</code>.
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension [T](javaFuture: java.util.concurrent.Future[T]) {
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Queries this future for its value.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the future is not ready, this method will return <code>None</code>. If ready, it will either return an exception
  //DOTTY-ONLY    * or a <code>T</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def eitherValue: Option[Either[Throwable, T]] = convertJavaFuture(javaFuture).eitherValue
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Indicates whether this future has expired (timed out).
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * The timeout detected by this method is different from the timeout supported by <code>whenReady</code>. This timeout
  //DOTTY-ONLY    * is a timeout of the underlying future. If the underlying future does not support timeouts, this method must always
  //DOTTY-ONLY    * return <code>false</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def isExpired: Boolean = convertJavaFuture(javaFuture).isExpired
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Indicates whether this future has been canceled.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the underlying future does not support the concept of cancellation, this method must always return <code>false</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def isCanceled: Boolean = convertJavaFuture(javaFuture).isCanceled
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Indicates whether this future is ready within the specified timeout.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * If the <code>eitherValue</code> method returns a <code>java..Some</code> containing a
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
  //DOTTY-ONLY   def isReadyWithin(timeout: Span)(using config: PatienceConfig, pos: source.Position): Boolean = convertJavaFuture(javaFuture).isReadyWithin(timeout)(config, pos)
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
  //DOTTY-ONLY    * If the <code>eitherValue</code> method returns a <code>scala.Some</code> containing a
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
  //DOTTY-ONLY   final def futureValue(timeout: Timeout, interval: Interval)(using pos: source.Position): T = convertJavaFuture(javaFuture).futureValue(timeout, interval)(pos)
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
  //DOTTY-ONLY    * If the <code>eitherValue</code> method returns a <code>scala.Some</code> containing a
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
  //DOTTY-ONLY   final def futureValue(timeout: Timeout)(using config: PatienceConfig, pos: source.Position): T = convertJavaFuture(javaFuture).futureValue(timeout)(config, pos)
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
  //DOTTY-ONLY    * If the <code>eitherValue</code> method returns a <code>scala.Some</code> containing a
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
  //DOTTY-ONLY   final def futureValue(interval: Interval)(using config: PatienceConfig, pos: source.Position): T = convertJavaFuture(javaFuture).futureValue(interval)(config, pos)
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
  //DOTTY-ONLY    * If the <code>eitherValue</code> method returns a <code>scala.Some</code> containing a
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
  //DOTTY-ONLY   def futureValue(using config: PatienceConfig, pos: source.Position): T = convertJavaFuture(javaFuture).futureValue(config, pos)
  //DOTTY-ONLY }
}

/**
 * Companion object that facilitates the importing of <code>JavaFutures</code> members as
 * an alternative to mixing it in. One use case is to import <code>JavaFutures</code>'s members so you can use
 * them in the Scala interpreter.
 *
 * <pre class="stHighlight">
 * import org.scalatest.concurrent.JavaFutures._
 * </pre>
 */
object JavaFutures extends JavaFutures {
  private class FutureConceptImpl[T](javaFuture: FutureOfJava[T]) extends FutureConcept[T] {
    def eitherValue: Option[Either[Throwable, T]] =
      if (javaFuture.isDone())
        Some(Right(javaFuture.get))
      else
        None
    def isExpired: Boolean = false // Java Futures don't support the notion of a timeout
    def isCanceled: Boolean = javaFuture.isCancelled // Two ll's in Canceled. The verbosity of Java strikes again!
    // TODO: Catch TimeoutException and wrap that in a TFE with ScalaTest's TimeoutException I think.
    // def awaitAtMost(span: Span): T = javaFuture.get(span.totalNanos, TimeUnit.NANOSECONDS)
    override private[concurrent] def futureValueImpl(pos: source.Position)(implicit config: PatienceConfig): T = {
      /*val adjustment =
        if (methodName == "whenReady")
          3
        else
          0*/

      if (javaFuture.isCancelled)
        throw new TestFailedException(
          (_: StackDepthException) => Some(Resources.futureWasCanceled),
          None,
          pos
        )
      try {
        javaFuture.get(config.timeout.totalNanos, TimeUnit.NANOSECONDS)
      }
      catch {
        case e: java.util.concurrent.TimeoutException =>
          throw new TestFailedException(
            (_: StackDepthException) => Some(Resources.wasNeverReady(1, config.interval.prettyString)),
            None,
            pos
          ) with TimeoutField {
            val timeout: Span = config.timeout
          }
        case e: java.util.concurrent.ExecutionException =>
          val cause = e.getCause
          val exToReport = if (cause == null) e else cause 
          if (anExceptionThatShouldCauseAnAbort(exToReport) || exToReport.isInstanceOf[TestPendingException] || exToReport.isInstanceOf[TestCanceledException]) {
            throw exToReport
          }
          throw new TestFailedException(
            (_: StackDepthException) => Some {
              if (exToReport.getMessage == null)
                Resources.futureReturnedAnException(exToReport.getClass.getName)
              else
                Resources.futureReturnedAnExceptionWithMessage(exToReport.getClass.getName, exToReport.getMessage)
            },
            Some(exToReport),
            pos
          )
      }
    }
  }
}
