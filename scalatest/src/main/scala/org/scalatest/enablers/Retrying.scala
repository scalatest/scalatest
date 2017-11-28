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
package org.scalatest.enablers

import org.scalactic.source
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort

import org.scalatest.Resources
import org.scalatest.exceptions.{StackDepthException, TestFailedDueToTimeoutException, TestPendingException}
import org.scalatest.time.{Nanosecond, Nanoseconds, Span}

import scala.annotation.tailrec
import scala.language.higherKinds

trait Retrying[T] {

  def retry(timeout: Span, interval: Span, pos: source.Position)(fun: => T): T

}

object Retrying {

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

        val initialInterval = Span(interval.totalNanos * 0.1, Nanoseconds) // config.interval scaledBy 0.1

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
                val durationSpan = Span(1, Nanosecond) scaledBy duration // Use scaledBy to get pretty units
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
