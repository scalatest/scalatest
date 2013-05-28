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
package org.scalatest.concurrent

import org.scalatest.time.Span
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import org.scalatest.Resources
import org.scalatest.exceptions.{TestPendingException, TestFailedException, TimeoutField}
import org.scalatest.exceptions.TestCanceledException
import scala.util.Success
import scala.util.Failure

/**
 * Provides an implicit conversion from <code>scala.concurrent.Future[T]</code> to
 * <a href="Futures$FutureConcept.html"><code>FutureConcept[T]</code></a>.
 *
 * <p>
 * This trait enables you to invoke the methods defined on <code>FutureConcept</code> on a Scala <code>Future</code>, as well as to pass a Scala future
 * to the <code>whenReady</code> methods of supertrait <a href="Futures.html"><code>Futures</code></a>.
 * See the documentation for supertrait <a href="Futures.html"><code>Futures</code></a> for the details on the syntax this trait provides
 * for testing with Scala futures.
 * </p>
 * 
 * @author Bill Venners
 */
trait ScalaFutures extends Futures {

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
    new FutureConcept[T] {
      def eitherValue: Option[Either[Throwable, T]] =
         scalaFuture.value.map {
           case Success(o) => Right(o)
           case Failure(e) => Left(e)
         }
      def isExpired: Boolean = false // Scala Futures themselves don't support the notion of a timeout
      def isCanceled: Boolean = false // Scala Futures don't seem to be cancelable either
/*
      def futureValue(implicit config: PatienceConfig): T = {
        try Await.ready(scalaFuture, Duration.fromNanos(config.timeout.totalNanos))
        catch {
          case e: TimeoutException => 
        }
      }
*/
    }
}

