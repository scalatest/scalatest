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

  //DOTTY-ONLY given[T]: Conversion[FutureOfJava[T], FutureConcept[T]] with {
  //DOTTY-ONLY   def apply(javaFuture: FutureOfJava[T]): FutureConcept[T] = new FutureConceptImpl[T](javaFuture)
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
