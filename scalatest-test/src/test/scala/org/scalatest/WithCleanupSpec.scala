/*
 * Copyright 2001-2015 Artima, Inc.
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

import scala.concurrent.Promise
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Success
import scala.util.Failure
import org.scalactic.Good
import org.scalactic.Bad
// SKIP-SCALATESTJS-START
import org.scalatest.concurrent.Eventually._
// SKIP-SCALATESTJS-END
import org.scalatest.OutcomeOf.outcomeOf
import org.scalatest.concurrent.ScalaFutures._

class WithCleanupSpec extends FunSpec {

  describe("AsyncSuite's withCleanup method") {
    val asyncSuite =
      new AsyncSuite {
// SKIP-SCALATESTJS-START
        override def executionContext: ExecutionContext = ExecutionContext.Implicits.global
// SKIP-SCALATESTJS-END
//SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
      }
    describe("when an exception is immediately thrown") {
      it("should perform the side effect and rethrow the same exception") {
        var sideEffectWasExecuted = false
        val ex = new RuntimeException("oops")
        val thrown = 
          intercept[RuntimeException] {
            asyncSuite.withCleanup {
              throw ex
            } {
              sideEffectWasExecuted = true
            }
          }
        assert(thrown eq ex)
        assert(sideEffectWasExecuted)
      }
      describe("but a second exception is thrown by the cleanup clause") {
        it("should perform the side effect and complete abruptly with the second exception") {
          var sideEffectWasExecuted = false
          val firstEx = new RuntimeException("first")
          val secondEx = new RuntimeException("second")
          val thrown = 
            intercept[RuntimeException] {
              asyncSuite.withCleanup {
                throw firstEx
              } {
                sideEffectWasExecuted = true
                throw secondEx
              }
            }
          assert(thrown eq secondEx)
          assert(sideEffectWasExecuted)
        }
      }
    }
    describe("when a Future[Outcome] is returned") {
      describe("that completes with a Success(Succeeded)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val fut: FutureOutcome = 
            asyncSuite.withCleanup {
              FutureOutcome { promise.future }
            } {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(Succeeded)
          // SKIP-SCALATESTJS-START
          eventually { assert(sideEffectWasExecuted) }
          // SKIP-SCALATESTJS-END
          assert(fut.value == Some(Good(Succeeded))) // TODO: This is a race condition, and therefore a flicker. Fix it.
        }
      }
      describe("that completes with a Success(Pending)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val fut: FutureOutcome = 
            asyncSuite.withCleanup {
              FutureOutcome { promise.future }
            } {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(Pending)
          // SKIP-SCALATESTJS-START
          eventually { assert(sideEffectWasExecuted) }
          // SKIP-SCALATESTJS-END
          assert(fut.value == Some(Good(Pending)))
        }
      }
      describe("that completes with a Success(Canceled)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val canceled = outcomeOf { cancel("nevermind") }
          val fut: FutureOutcome = 
            asyncSuite.withCleanup {
              FutureOutcome { promise.future }
            } {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(canceled)
          // SKIP-SCALATESTJS-START
          eventually { assert(sideEffectWasExecuted) }
          // SKIP-SCALATESTJS-END
          assert(fut.value == Some(Good(canceled)))
        }
      }
      describe("that completes with a Success(Failed)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val failed = outcomeOf { fail("oops") }
          val fut: FutureOutcome = 
            asyncSuite.withCleanup {
              FutureOutcome { promise.future }
            } {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(failed)
          // SKIP-SCALATESTJS-START
          eventually { assert(sideEffectWasExecuted) }
          // SKIP-SCALATESTJS-END
          assert(fut.value == Some(Good(failed)))
        }
      }
      describe("that completes with a Failure(ex)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future with a Faiulre containing the original exception") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val ex = new RuntimeException("I meant to do that!")
          val fut: FutureOutcome = 
            asyncSuite.withCleanup {
              FutureOutcome { promise.future }
            } {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.failure(ex)
          // SKIP-SCALATESTJS-START
          eventually { assert(sideEffectWasExecuted) }
          // SKIP-SCALATESTJS-END
          assert(fut.underlying.failed.futureValue eq ex)
        }
        describe("but a second exception is thrown by the cleanup clause") {
          it("should perform the side effect and fail with the second exception") {
            var sideEffectWasExecuted = false
            val promise = Promise[Outcome]()
            val firstEx = new RuntimeException("first")
            val secondEx = new RuntimeException("second")
            val fut: FutureOutcome = 
              asyncSuite.withCleanup {
                FutureOutcome { promise.future }
              } {
                sideEffectWasExecuted = true
                throw secondEx
              }
            assert(!sideEffectWasExecuted)
            promise.failure(firstEx)
            // SKIP-SCALATESTJS-START
            eventually { assert(sideEffectWasExecuted) }
            // SKIP-SCALATESTJS-END
            assert(fut.value == Some(Good(Failed(secondEx))))
          }
        }
      }
    }
  }
}

