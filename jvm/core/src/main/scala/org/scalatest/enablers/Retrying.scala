/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.enablers

import java.util.concurrent.{Executors, ScheduledExecutorService, ThreadFactory, TimeUnit}

import org.scalactic.source
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import org.scalatest.Resources
import org.scalatest.exceptions.{StackDepthException, TestFailedDueToTimeoutException, TestPendingException}
import org.scalatest.time.{Nanosecond, Nanoseconds, Span}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}
import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * Supertrait for <code>Retrying</code> typeclasses, which are used to implement and determine the behavior
  * of <a href="../Eventually.html"><code>Eventually</code></a> methods.
  *
  * <p>
  * Currently, implementations for anything type <code>T</code> and <code>Future[T]</code> is provided.
  * </p>
  */
trait Retrying[T] {

  /**
    * Retry the passed in function until the given timeout is reached, with the given interval between attempts.
    *
    * @param timeout the timespan to try before giving up
    * @param interval interval between call attempts
    * @param pos the position of the call site
    * @param fun function to be called
    * @return the value returned from the passed in <code>fun</code>.
    */
  def retry(timeout: Span, interval: Span, pos: source.Position)(fun: => T): T

}

/**
  * Companion object that provides <code>Retrying</code> implementations for <code>T</code> and <code>Future[T]</code>.
  */
object Retrying {

  private def createScheduler(): ScheduledExecutorService = {
    val threadFactory = new ThreadFactory {
      val inner = Executors.defaultThreadFactory()
      def newThread(runnable: Runnable) = {
        val thread = inner.newThread(runnable)
        thread.setName("ScalaTest-retrying")
        thread.setDaemon(true)
        thread
      }
    }

    Executors.newSingleThreadScheduledExecutor(threadFactory)
  }

  /**
    * Provides implicit <code>Retrying</code> implementation for <code>Future[T]</code>.
    */
  implicit def retryingNatureOfFutureT[T](implicit execCtx: ExecutionContext): Retrying[Future[T]] =
    new Retrying[Future[T]] {
      def retry(timeout: Span, interval: Span, pos: source.Position)(fun: => Future[T]): Future[T] = {
        val startNanos = System.nanoTime

        val initialInterval = Span(interval.totalNanos * 0.1, Nanoseconds)

        // Can't make this tail recursive. TODO: Document that fact.
        def tryTryAgain(attempt: Int): Future[T] = {
          try {
            fun recoverWith {

              case tpe: TestPendingException => Future.failed(tpe)

              case e: Throwable if !anExceptionThatShouldCauseAnAbort(e) =>

                // Here I want to try again after the duration. So first calculate the duration to
                // wait before retrying. This is front loaded with a simple backoff algo.
                val duration = System.nanoTime - startNanos
                if (duration < timeout.totalNanos) {
                  val chillTime =
                    if (duration < interval.totalNanos) // For first interval, we wake up every 1/10 of the interval.  This is mainly for optimization purpose.
                      initialInterval.millisPart
                    else
                      interval.millisPart

                  // Create a Promise
                  val promise = Promise[T]

                  val task =
                    new Runnable {
                      override def run(): Unit = {
                        val newFut = tryTryAgain(attempt + 1)
                        newFut onComplete {
                          case Success(res) => promise.success(res)
                          case Failure(ex) => promise.failure(ex)
                        }
                      }
                    }

                  val scheduler = createScheduler()
                  scheduler.schedule(task, chillTime, TimeUnit.MILLISECONDS)
                  scheduler.shutdown()

                  promise.future
                }
                else { // Timed out so return a failed Future
                  val durationSpan = Span(1, Nanosecond).scaledBy(duration) // Use scaledBy to get pretty units
                  Future.failed(
                    new TestFailedDueToTimeoutException(
                      (_: StackDepthException) =>
                        Some(
                          if (e.getMessage == null)
                            Resources.didNotUltimatelySucceed(attempt.toString, durationSpan.prettyString)
                          else
                            Resources.didNotUltimatelySucceedBecause(attempt.toString, durationSpan.prettyString, e.getMessage)
                        ),
                      Some(e),
                      Left(pos),
                      None,
                      timeout
                    )
                  )
                }
            }
          }
          catch {
            case tpe: TestPendingException => throw tpe
            case e: Throwable if !anExceptionThatShouldCauseAnAbort(e) => 
              val duration = System.nanoTime - startNanos
              if (duration < timeout.totalNanos) {
                if (duration < interval.totalNanos) // For first interval, we wake up every 1/10 of the interval.  This is mainly for optimization purpose.
                  Thread.sleep(initialInterval.millisPart, initialInterval.nanosPart)
                else
                  Thread.sleep(interval.millisPart, interval.nanosPart)
              }
              else {
                val durationSpan = Span(1, Nanosecond).scaledBy(duration) // Use scaledBy to get pretty units
                throw new TestFailedDueToTimeoutException(
                  (_: StackDepthException) =>
                    Some(
                      if (e.getMessage == null)
                        Resources.didNotEventuallySucceed(attempt.toString, durationSpan.prettyString)
                      else
                        Resources.didNotEventuallySucceedBecause(attempt.toString, durationSpan.prettyString, e.getMessage)
                    ),
                  Some(e),
                  Left(pos),
                  None,
                  timeout
                )
              }
              tryTryAgain(attempt + 1)
          }
        }
        tryTryAgain(1)
      }
    }

  /**
    * Provides implicit <code>Retrying</code> implementation for <code>T</code>.
    */
  implicit def retryingNatureOfT[T]: Retrying[T] =
    new Retrying[T] {
      def retry(timeout: Span, interval: Span, pos: source.Position)(fun: => T): T = {
        val startNanos = System.nanoTime
        def makeAValiantAttempt(): Either[Throwable, T] = {
          try {
            Right(fun)
          }
          catch {
            case tpe: TestPendingException => throw tpe
            case e: Throwable if !anExceptionThatShouldCauseAnAbort(e) => Left(e)
          }
        }

        val initialInterval = Span(interval.totalNanos * 0.1, Nanoseconds)

        @tailrec
        def tryTryAgain(attempt: Int): T = {
          makeAValiantAttempt() match {
            case Right(result) => result
            case Left(e) =>
              val duration = System.nanoTime - startNanos
              if (duration < timeout.totalNanos) {
                if (duration < interval.totalNanos) // For first interval, we wake up every 1/10 of the interval.  This is mainly for optimization purpose.
                  Thread.sleep(initialInterval.millisPart, initialInterval.nanosPart)
                else
                  Thread.sleep(interval.millisPart, interval.nanosPart)
              }
              else {
                val durationSpan = Span(1, Nanosecond).scaledBy(duration) // Use scaledBy to get pretty units
                throw new TestFailedDueToTimeoutException(
                  (_: StackDepthException) =>
                    Some(
                      if (e.getMessage == null)
                        Resources.didNotEventuallySucceed(attempt.toString, durationSpan.prettyString)
                      else
                        Resources.didNotEventuallySucceedBecause(attempt.toString, durationSpan.prettyString, e.getMessage)
                    ),
                  Some(e),
                  Left(pos),
                  None,
                  timeout
                )
              }

              tryTryAgain(attempt + 1)
          }
        }
        tryTryAgain(1)
      }
    }

}
