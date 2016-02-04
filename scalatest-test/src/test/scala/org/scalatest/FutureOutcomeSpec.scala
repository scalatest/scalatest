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

import org.scalactic.{Or, Good, Bad}
import scala.util.{Try, Success, Failure}
import scala.concurrent.Future
import scala.concurrent.Promise
import exceptions.TestCanceledException

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
      "execute functions passed to its onCompletedThen method" in {
        val promise = Promise[Outcome]
        var paramPassedToOnCompletedThen: Option[Outcome Or Throwable] = None
        var onSucceededThenFunctionWasInvoked = false
        var paramPassedToOnFailedThen: Option[Throwable] = None
        var paramPassedToOnCanceledThen: Option[TestCanceledException] = None
        var onPendingThenFunctionWasInvoked = false
        var paramPassedToOnOutcomeThen: Option[Outcome] = None
        var paramPassedToOnAbortedThen: Option[Throwable] = None
        val fo = FutureOutcome(promise.future)
        assert(!fo.isCompleted)
        assert(fo.value == None)
        val fo2 = 
          fo onCompletedThen { outcomeOrThrowable =>
            paramPassedToOnCompletedThen = Some(outcomeOrThrowable)
          } onSucceededThen {
            onSucceededThenFunctionWasInvoked = true
          } onFailedThen { ex =>
            paramPassedToOnFailedThen = Some(ex)
          } onCanceledThen { ex =>
            paramPassedToOnCanceledThen = Some(ex)
          } onPendingThen {
            onPendingThenFunctionWasInvoked = true
          } onOutcomeThen { outcome =>
            paramPassedToOnOutcomeThen = Some(outcome)
          } onAbortedThen { ex =>
            paramPassedToOnAbortedThen = Some(ex)
          }
        assert(!fo2.isCompleted)
        assert(fo2.value == None)
        assert(paramPassedToOnCompletedThen == None)
        assert(onSucceededThenFunctionWasInvoked == false)
        assert(paramPassedToOnFailedThen == None)
        assert(paramPassedToOnCanceledThen == None)
        assert(onPendingThenFunctionWasInvoked == false)
        assert(paramPassedToOnAbortedThen == None)
        promise.success(Succeeded)
        fo2.underlying map { _ =>
          assert(fo2.isCompleted)
          assert(fo2.value == Some(Good(Succeeded)))
          assert(paramPassedToOnCompletedThen == Some(Good(Succeeded)))
          assert(onSucceededThenFunctionWasInvoked == true)
          assert(paramPassedToOnFailedThen == None)
          assert(paramPassedToOnCanceledThen == None)
          assert(onPendingThenFunctionWasInvoked == false)
          assert(paramPassedToOnOutcomeThen == Some(Succeeded))
          assert(paramPassedToOnAbortedThen == None)
        }
      }
      "execute functions passed to its onSucceededThen method" in {
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
      "execute functions passed to its map method" in {
        pending
      }
    }
    "representing a future outcome that completes with Failed" should {
      "execute functions passed to its onCompletedThen method" in {
        pending
      }
      "execute functions passed to its onSucceededThen method" in {
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
      "execute functions passed to its map method" in {
        pending
      }
    }
    "representing a future outcome that completes with Canceled" should {
      "execute functions passed to its onCompletedThen method" in {
        pending
      }
      "execute functions passed to its onSucceededThen method" in {
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
      "execute functions passed to its map method" in {
        pending
      }
    }
    "representing a future outcome that completes with Pending" should {
      "execute functions passed to its onCompletedThen method" in {
        pending
      }
      "execute functions passed to its onSucceededThen method" in {
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
      "execute functions passed to its map method" in {
        pending
      }
    }
    "representing a future outcome that completes with Aborted" should {
      "execute functions passed to its onCompletedThen method" in {
        pending
      }
      "execute functions passed to its onSucceededThen method" in {
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
      "execute functions passed to its map method" in {
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
