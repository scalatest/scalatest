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
final case class Possibility(future: Future[Outcome]) {
  def onCompletedThen(f: Try[Outcome] => Unit)(implicit executionContext: ExecutionContext): Possibility
  def onSucceededThen(f: => Unit)(implicit executionContext: ExecutionContext): Possibility
  def onPendingThen(f: => Unit)(implicit executionContext: ExecutionContext): Possibility
  def onFailedThen(f: Throwble => Unit)(implicit executionContext: ExecutionContext): Possibility
  def onCanceledThen(f: TestCanceledException => Unit)(implicit executionContext: ExecutionContext): Possibility
  def onAbortedThen(f: Throwable => Unit)(implicit executionContext: ExecutionContext): Possibility
  def map(f: Outcome => Outcome)(implicit executionContext: ExecutionContext): Possibility
}
*/

class PossibleOutcomeSpec extends AsyncWordSpec with DiagrammedAssertions {
  "A PossibleOutcome" when {
    "representing a future outcome that completes with Succeeded" should {
      "execute functions passed to its onCompletedThen method in order" in {
        var paramPassed: Option[Try[Outcome]] = None
        val po = PossibleOutcome(Future.successful(Succeeded))
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

