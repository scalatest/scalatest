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
import exceptions.TestFailedException
import java.util.concurrent.ExecutionException

/*
class SuiteAbortingException(underlying: Throwable) {
  require(isAnExceptionThatShouldCauseASuiteToAbort)
}
*/

class FutureOutcomeSpec extends AsyncFreeSpec with DiagrammedAssertions {
  "A FutureOutcome" - {
    "when representing a future outcome that completes with Succeeded" - {
      "should execute appropriate callbacks" in {
        val promise = Promise[Outcome]
        var paramPassedToOnCompletedThen: Option[Outcome Or Throwable] = None
        var onSucceededThenFunctionWasInvoked = false
        var paramPassedToOnFailedThen: Option[Throwable] = None
        var paramPassedToOnCanceledThen: Option[TestCanceledException] = None
        var onPendingThenFunctionWasInvoked = false
        var paramPassedToOnOutcomeThen: Option[Outcome] = None
        var paramPassedToOnAbortedThen: Option[Throwable] = None
        var paramPassedToMap: Option[Outcome] = None
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
          } map { outcome =>
            paramPassedToMap = Some(outcome)
            outcome
          }
        assert(!fo2.isCompleted)
        assert(fo2.value == None)
        assert(paramPassedToOnCompletedThen == None)
        assert(onSucceededThenFunctionWasInvoked == false)
        assert(paramPassedToOnFailedThen == None)
        assert(paramPassedToOnCanceledThen == None)
        assert(onPendingThenFunctionWasInvoked == false)
        assert(paramPassedToOnAbortedThen == None)
        assert(paramPassedToMap == None)
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
          assert(paramPassedToMap == Some(Succeeded))
        }
      }
    }
    "representing a future outcome that completes with Failed" - {
      "should execute appropriate callbacks" in {
        val promise = Promise[Outcome]
        var paramPassedToOnCompletedThen: Option[Outcome Or Throwable] = None
        var onSucceededThenFunctionWasInvoked = false
        var paramPassedToOnFailedThen: Option[Throwable] = None
        var paramPassedToOnCanceledThen: Option[TestCanceledException] = None
        var onPendingThenFunctionWasInvoked = false
        var paramPassedToOnOutcomeThen: Option[Outcome] = None
        var paramPassedToOnAbortedThen: Option[Throwable] = None
        var paramPassedToMap: Option[Outcome] = None
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
          } map { outcome =>
            paramPassedToMap = Some(outcome)
            outcome
          }
        assert(!fo2.isCompleted)
        assert(fo2.value == None)
        assert(paramPassedToOnCompletedThen == None)
        assert(onSucceededThenFunctionWasInvoked == false)
        assert(paramPassedToOnFailedThen == None)
        assert(paramPassedToOnCanceledThen == None)
        assert(onPendingThenFunctionWasInvoked == false)
        assert(paramPassedToOnAbortedThen == None)
        assert(paramPassedToMap == None)
        case object MyException extends Throwable
        promise.success(Failed(MyException))
        fo2.underlying map { _ =>
          assert(fo2.isCompleted)
          assert(fo2.value == Some(Good(Failed(MyException))))
          assert(paramPassedToOnCompletedThen == Some(Good(Failed(MyException))))
          assert(onSucceededThenFunctionWasInvoked == false)
          assert(paramPassedToOnFailedThen == Some(MyException))
          assert(paramPassedToOnCanceledThen == None)
          assert(onPendingThenFunctionWasInvoked == false)
          assert(paramPassedToOnOutcomeThen == Some(Failed(MyException)))
          assert(paramPassedToOnAbortedThen == None)
          assert(paramPassedToMap == Some(Failed(MyException)))
        }
      }
    }
    "representing a future outcome that completes with Canceled" - {
      "should execute appropriate callbacks" in {
        val promise = Promise[Outcome]
        var paramPassedToOnCompletedThen: Option[Outcome Or Throwable] = None
        var onSucceededThenFunctionWasInvoked = false
        var paramPassedToOnFailedThen: Option[Throwable] = None
        var paramPassedToOnCanceledThen: Option[TestCanceledException] = None
        var onPendingThenFunctionWasInvoked = false
        var paramPassedToOnOutcomeThen: Option[Outcome] = None
        var paramPassedToOnAbortedThen: Option[Throwable] = None
        var paramPassedToMap: Option[Outcome] = None
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
          } map { outcome =>
            paramPassedToMap = Some(outcome)
            outcome
          }
        assert(!fo2.isCompleted)
        assert(fo2.value == None)
        assert(paramPassedToOnCompletedThen == None)
        assert(onSucceededThenFunctionWasInvoked == false)
        assert(paramPassedToOnFailedThen == None)
        assert(paramPassedToOnCanceledThen == None)
        assert(onPendingThenFunctionWasInvoked == false)
        assert(paramPassedToOnAbortedThen == None)
        assert(paramPassedToMap == None)
        case object MyException extends TestCanceledException(0)
        promise.success(Canceled(MyException))
        fo2.underlying map { _ =>
          assert(fo2.isCompleted)
          assert(fo2.value == Some(Good(Canceled(MyException))))
          assert(paramPassedToOnCompletedThen == Some(Good(Canceled(MyException))))
          assert(onSucceededThenFunctionWasInvoked == false)
          assert(paramPassedToOnFailedThen == None)
          assert(paramPassedToOnCanceledThen == Some(MyException))
          assert(onPendingThenFunctionWasInvoked == false)
          assert(paramPassedToOnOutcomeThen == Some(Canceled(MyException)))
          assert(paramPassedToOnAbortedThen == None)
          assert(paramPassedToMap == Some(Canceled(MyException)))
        }
      }
    }
    "representing a future outcome that completes with Pending" - {
      "should execute appropriate callbacks" in {
        val promise = Promise[Outcome]
        var paramPassedToOnCompletedThen: Option[Outcome Or Throwable] = None
        var onSucceededThenFunctionWasInvoked = false
        var paramPassedToOnFailedThen: Option[Throwable] = None
        var paramPassedToOnCanceledThen: Option[TestCanceledException] = None
        var onPendingThenFunctionWasInvoked = false
        var paramPassedToOnOutcomeThen: Option[Outcome] = None
        var paramPassedToOnAbortedThen: Option[Throwable] = None
        var paramPassedToMap: Option[Outcome] = None
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
          } map { outcome =>
            paramPassedToMap = Some(outcome)
            outcome
          }
        assert(!fo2.isCompleted)
        assert(fo2.value == None)
        assert(paramPassedToOnCompletedThen == None)
        assert(onSucceededThenFunctionWasInvoked == false)
        assert(paramPassedToOnFailedThen == None)
        assert(paramPassedToOnCanceledThen == None)
        assert(onPendingThenFunctionWasInvoked == false)
        assert(paramPassedToOnAbortedThen == None)
        assert(paramPassedToMap == None)
        promise.success(Pending)
        fo2.underlying map { _ =>
          assert(fo2.isCompleted)
          assert(fo2.value == Some(Good(Pending)))
          assert(paramPassedToOnCompletedThen == Some(Good(Pending)))
          assert(onSucceededThenFunctionWasInvoked == false)
          assert(paramPassedToOnFailedThen == None)
          assert(paramPassedToOnCanceledThen == None)
          assert(onPendingThenFunctionWasInvoked == true)
          assert(paramPassedToOnOutcomeThen == Some(Pending))
          assert(paramPassedToOnAbortedThen == None)
          assert(paramPassedToMap == Some(Pending))
        }
      }
    }
    "representing a future outcome that completes with Aborted" - {
      "should execute appropriate callbacks" in {
        val promise = Promise[Outcome]
        var paramPassedToOnCompletedThen: Option[Outcome Or Throwable] = None
        var onSucceededThenFunctionWasInvoked = false
        var paramPassedToOnFailedThen: Option[Throwable] = None
        var paramPassedToOnCanceledThen: Option[TestCanceledException] = None
        var onPendingThenFunctionWasInvoked = false
        var paramPassedToOnOutcomeThen: Option[Outcome] = None
        var paramPassedToOnAbortedThen: Option[Throwable] = None
        var paramPassedToMap: Option[Outcome] = None
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
          } map { outcome =>
            paramPassedToMap = Some(outcome)
            outcome
          }
        assert(!fo2.isCompleted)
        assert(fo2.value == None)
        assert(paramPassedToOnCompletedThen == None)
        assert(onSucceededThenFunctionWasInvoked == false)
        assert(paramPassedToOnFailedThen == None)
        assert(paramPassedToOnCanceledThen == None)
        assert(onPendingThenFunctionWasInvoked == false)
        assert(paramPassedToOnAbortedThen == None)
        assert(paramPassedToMap == None)
        case object MyError extends ExecutionException(new VirtualMachineError {})
        promise.failure(MyError)
        val execEx =
          recoverToExceptionIf[ExecutionException] {
            fo2.underlying
          }
        execEx map { ex =>
          assert(ex.getCause.isInstanceOf[VirtualMachineError])
          assert(fo2.isCompleted)
          assert(fo2.value == Some(Bad(MyError)))
          assert(paramPassedToOnCompletedThen == Some(Bad(MyError)))
          assert(onSucceededThenFunctionWasInvoked == false)
          assert(paramPassedToOnFailedThen == None)
          assert(paramPassedToOnCanceledThen == None)
          assert(onPendingThenFunctionWasInvoked == false)
          assert(paramPassedToOnOutcomeThen == None)
          assert(paramPassedToOnAbortedThen == Some(MyError))
          assert(paramPassedToMap == None)
        }
      }
    }
    "when a function passed to its onSucceededThen method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onSucceededThen {
              fail("I meant to do that!")
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Succeeded)
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Failed(ex) =>
                assert(ex.isInstanceOf[TestFailedException])
                assert(ex.getMessage == "I meant to do that!")
              case _ => fail("Outcome was not a Failed")
            }
          }
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onSucceededThen {
              throw new IllegalArgumentException("I meant to do that!")
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Succeeded)
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Failed(ex) =>
                assert(ex.isInstanceOf[IllegalArgumentException])
                assert(ex.getMessage == "I meant to do that!")
              case _ => fail("Outcome was not a Failed")
            }
          }
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onSucceededThen {
              cancel("I meant to do that!")
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Succeeded)
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Canceled(ex) =>
                assert(ex.isInstanceOf[TestCanceledException])
                assert(ex.getMessage == "I meant to do that!")
              case _ => fail("Outcome was not a Canceled")
            }
          }
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onSucceededThen {
              pending
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Succeeded)
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Pending => succeed
              case _ => fail("Outcome was not a Pending")
            }
          }
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          class MyError extends VirtualMachineError("I meant to do that!")
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onSucceededThen {
              throw new MyError
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Succeeded)
          fo2.underlying.failed map { ex =>
            assert(fo2.isCompleted)
            ex match {
              case ee: ExecutionException => 
                assert(ex.getCause.isInstanceOf[MyError])
                assert(ex.getCause.getMessage == "I meant to do that!")
              case ex => fail("Was not an ExecutionException: " + ex)
            }
          }
        }
      }
    }
    "when a function passed to its onFailedThen method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onFailedThen { ex =>
              fail("I meant to do that!")
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Failed(new IllegalArgumentException("the original one")))
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Failed(ex) =>
                assert(ex.isInstanceOf[TestFailedException])
                assert(ex.getMessage == "I meant to do that!")
              case _ => fail("Outcome was not a Failed")
            }
          }
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onFailedThen { ex =>
              throw new IllegalArgumentException("I meant to do that!")
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Failed(new IllegalArgumentException("the original one")))
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Failed(ex) =>
                assert(ex.isInstanceOf[IllegalArgumentException])
                assert(ex.getMessage == "I meant to do that!")
              case _ => fail("Outcome was not a Failed")
            }
          }
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onFailedThen { ex =>
              cancel("I meant to do that!")
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Failed(new IllegalArgumentException("the original one")))
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Canceled(ex) =>
                assert(ex.isInstanceOf[TestCanceledException])
                assert(ex.getMessage == "I meant to do that!")
              case _ => fail("Outcome was not a Canceled")
            }
          }
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onFailedThen { ex =>
              pending
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Failed(new IllegalArgumentException("the original one")))
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Pending => succeed
              case _ => fail("Outcome was not a Pending")
            }
          }
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          class MyError extends VirtualMachineError("I meant to do that!")
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onFailedThen { ex =>
              throw new MyError
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Failed(new IllegalArgumentException("the original one")))
          fo2.underlying.failed map { ex =>
            assert(fo2.isCompleted)
            ex match {
              case ee: ExecutionException => 
                assert(ex.getCause.isInstanceOf[MyError])
                assert(ex.getCause.getMessage == "I meant to do that!")
              case ex => fail("Was not an ExecutionException: " + ex)
            }
          }
        }
      }
    }
    "when a function passed to its onCanceledThen method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          val promise = Promise[Outcome]
          val fo = FutureOutcome(promise.future)
          assert(!fo.isCompleted)
          assert(fo.value == None)
          val fo2 = 
            fo onCanceledThen { ex =>
              fail("I meant to do that!")
            }
          assert(!fo2.isCompleted)
          assert(fo2.value == None)
          promise.success(Canceled(new TestCanceledException("the original one", 0)))
          fo2.underlying map { outcome =>
            assert(fo2.isCompleted)
            outcome match {
              case Failed(ex) =>
                assert(ex.isInstanceOf[TestFailedException])
                assert(ex.getMessage == "I meant to do that!")
              case _ => fail("Outcome was not a Failed")
            }
          }
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          pending
        }
      }
    }
    "when a function passed to its onPendingThen method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          pending
        }
      }
    }
    "when a function passed to its onAbortedThen method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          pending
        }
      }
    }
    "when a function passed to its onOutcomeThen method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          pending
        }
      }
    }
    "when a function passed to its map method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          pending
        }
      }
    }
    "when a function passed to its onCompletedThen method" - {
      "completes abruptly with a TestFailedException" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly some other test-failing exception" - {
        "should result in a Failed wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestCanceledException" - {
        "should result in a Canceled wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a TestPendingException" - {
        "should result in a Pending wrapping that exception" in {
          pending
        }
      }
      "completes abruptly with a suite-aborting exception" - {
        "should result in a Failed future wrapping that exception" in {
          pending
        }
      }
    }
    "offer a factory apply method in its companion that takes a Future[Outcome]" in {
      pending
    }
  }
}
