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
package org.scalatest

import org.scalactic.Bad
import org.scalactic.Good
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success
// SKIP-SCALATESTJS,NATIVE-START
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.OutcomeOf.outcomeOf
import org.scalatest.funspec.AsyncFunSpec

class CompleteLastlySpec extends AsyncFunSpec {

  describe("AsyncTestSuite's complete-lastly syntax") {
/*
    val asyncSuite =
      new AsyncSuite {
// SKIP-SCALATESTJS,NATIVE-START
        override def executionContext: ExecutionContext = ExecutionContext.Implicits.global
// SKIP-SCALATESTJS,NATIVE-END
//SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow
      }
*/
    describe("when an exception is immediately thrown") {
      it("should perform the side effect and rethrow the same exception") {
        var sideEffectWasExecuted = false
        val ex = new RuntimeException("oops")
        val thrown = 
          intercept[RuntimeException] {
            complete {
              (throw ex): FutureOutcome
            } lastly {
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
              complete {
                (throw firstEx): FutureOutcome
              } lastly {
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
            complete {
              FutureOutcome { promise.future }
            } lastly {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(Succeeded)
          fut.underlying.map { res =>
            assert(sideEffectWasExecuted)
            assert(res == Succeeded)
          }
        }
      }
      describe("that completes with a Success(Pending)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val fut: FutureOutcome = 
            complete {
              FutureOutcome { promise.future }
            } lastly {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(Pending)
          fut.underlying.map { res => 
            assert(sideEffectWasExecuted)
            assert(res == Pending)
          }
        }
      }
      describe("that completes with a Success(Canceled)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val canceled = outcomeOf { cancel("nevermind") }
          val fut: FutureOutcome = 
            complete {
              FutureOutcome { promise.future }
            } lastly {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(canceled)
          fut.underlying.map { res => 
            assert(sideEffectWasExecuted)
            assert(res == canceled)
          }
        }
      }
      describe("that completes with a Success(Failed)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val failed = outcomeOf { fail("oops") }
          val fut: FutureOutcome = 
            complete {
              FutureOutcome { promise.future }
            } lastly {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.success(failed)
          fut.underlying.map { res => 
            assert(sideEffectWasExecuted)
            assert(res == failed)
          }
        }
      }
      describe("that completes with a Failure(ex)") {
        it("should return a future that on completion of the original future performs the side effect then complete the returned future with a Faiulre containing the original exception") {
          var sideEffectWasExecuted = false
          val promise = Promise[Outcome]()
          val ex = new RuntimeException("I meant to do that!")
          val fut: FutureOutcome = 
            complete {
              FutureOutcome { promise.future }
            } lastly {
              sideEffectWasExecuted = true
            }
          assert(!sideEffectWasExecuted)
          promise.failure(ex)
          fut.underlying.failed.map { actualEx => 
            assert(sideEffectWasExecuted)
            assert(actualEx eq ex)
          }
        }
        describe("but a second exception is thrown by the cleanup clause") {
          it("should perform the side effect and fail with the second exception") {
            var sideEffectWasExecuted = false
            val promise = Promise[Outcome]()
            val firstEx = new RuntimeException("first")
            val secondEx = new RuntimeException("second")
            val fut: FutureOutcome = 
              complete {
                FutureOutcome { promise.future }
              } lastly {
                sideEffectWasExecuted = true
                throw secondEx
              }
            assert(!sideEffectWasExecuted)
            promise.failure(firstEx)
            fut.underlying.map { res => 
              assert(sideEffectWasExecuted)
              assert(res == Failed(secondEx))
            }
          }
        }
      }
    }
  }
}

