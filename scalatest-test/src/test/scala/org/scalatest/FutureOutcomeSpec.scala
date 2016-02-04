/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest

import scala.util.{Try, Success, Failure}
import scala.concurrent.Future

/*
class SuiteAbortingException(underlying: Throwable) {
  require(isAnExceptionThatShouldCauseASuiteToAbort)
}

class FutureOutcome(val toFuture: Future[Outcome]) extends AnyVal {
  def value: Option[Outcome Or SuiteAbortingException]
  def isCompleted: Boolean
  def onCompletedThen(f: Outcome Or SuiteAbortingException => Unit): Futuristic
  def onOutcomeThen(f: Outcome => Unit): Futuristic
  def onSucceededThen(f: => Unit): Futuristic
  def onPendingThen(f: => Unit): Futuristic
  def onFailedThen(f: Throwble => Unit): Futuristic
  def onCanceledThen(f: TestCanceledException => Unit): Futuristic
  def onAbortedThen(SuiteAbortingException => Unit): Futuristic
  def map(f: Outcome => Outcome): Futuristic
}
*/

class FutureOutcomeSpec extends AsyncWordSpec with DiagrammedAssertions {
  "A FutureOutcome" when {
    "representing a future outcome that completes with Succeeded" should {
      "execute functions passed to its onCompletedThen method in order" in {
        var paramPassed: Option[Try[Outcome]] = None
        val po = FutureOutcome(Future.successful(Succeeded))
        val po2 = 
          po onCompletedThen { tryOutcome =>
            paramPassed = Some(tryOutcome)
          }
        po2.toFuture map { _ => assert(paramPassed == Some(Success(Succeeded))) }
      }
      "execute functions passed to its onSucceededThen method in order" in {
        pending
      }
      "not execute functions passed to its onFailedThen method" in {
        pending
      }
      "not execute functions passed to its onCanceledThen method" in {
        pending
      }
      "not execute functions passed to its onPendingThen method" in {
        pending
      }
      "not execute functions passed to its onAbortedThen method" in {
        pending
      }
      "execute functions passed to its map method in order" in {
        pending
      }
    }
    "representing a future outcome that completes with Failed" should {
      "execute functions passed to its onCompletedThen method in order" in {
        pending
      }
      "execute functions passed to its onSucceededThen method in order" in {
        pending
      }
      "not execute functions passed to its onFailedThen method" in {
        pending
      }
      "not execute functions passed to its onCanceledThen method" in {
        pending
      }
      "not execute functions passed to its onPendingThen method" in {
        pending
      }
      "not execute functions passed to its onAbortedThen method" in {
        pending
      }
      "execute functions passed to its map method in order" in {
        pending
      }
    }
    "representing a future outcome that completes with Canceled" should {
      "execute functions passed to its onCompletedThen method in order" in {
        pending
      }
      "execute functions passed to its onSucceededThen method in order" in {
        pending
      }
      "not execute functions passed to its onFailedThen method" in {
        pending
      }
      "not execute functions passed to its onCanceledThen method" in {
        pending
      }
      "not execute functions passed to its onPendingThen method" in {
        pending
      }
      "not execute functions passed to its onAbortedThen method" in {
        pending
      }
      "execute functions passed to its map method in order" in {
        pending
      }
    }
    "representing a future outcome that completes with Pending" should {
      "execute functions passed to its onCompletedThen method in order" in {
        pending
      }
      "execute functions passed to its onSucceededThen method in order" in {
        pending
      }
      "not execute functions passed to its onFailedThen method" in {
        pending
      }
      "not execute functions passed to its onCanceledThen method" in {
        pending
      }
      "not execute functions passed to its onPendingThen method" in {
        pending
      }
      "not execute functions passed to its onAbortedThen method" in {
        pending
      }
      "execute functions passed to its map method in order" in {
        pending
      }
    }
    "representing a future outcome that completes with Aborted" should {
      "execute functions passed to its onCompletedThen method in order" in {
        pending
      }
      "execute functions passed to its onSucceededThen method in order" in {
        pending
      }
      "not execute functions passed to its onFailedThen method" in {
        pending
      }
      "not execute functions passed to its onCanceledThen method" in {
        pending
      }
      "not execute functions passed to its onPendingThen method" in {
        pending
      }
      "not execute functions passed to its onAbortedThen method" in {
        pending
      }
      "execute functions passed to its map method in order" in {
        pending
      }
    }
  }
  it should {
    "offer a factory apply method in its companion that takes a Future[Outcome]" in {
      pending
    }
  }
}

