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
package org.scalatest

import SharedHelpers.thisLineNumber
import OptionValues._

class OutcomeSpec extends Spec with OptionValues with OutcomeOf {

  val fileName = "OutcomeSpec.scala"

  object `An Outcome` {
    def `can be Succeeded` {
      assert(Succeeded.isSucceeded)
      assert(!Succeeded.isFailed)
      assert(!Succeeded.isCanceled)
      assert(!Succeeded.isPending)
    }
    def `can be Failed` {
      val ex = new Exception
      assert(!Failed(ex).isSucceeded)
      assert(Failed(ex).isFailed)
      assert(!Failed(ex).isCanceled)
      assert(!Failed(ex).isPending)
    }
    def `can be Canceled` {
      val ex = new exceptions.TestCanceledException(0)
      assert(!Canceled(ex).isSucceeded)
      assert(!Canceled(ex).isFailed)
      assert(Canceled(ex).isCanceled)
      assert(!Canceled(ex).isPending)
    }
    def `can be Pending` {
      assert(!Pending.isSucceeded)
      assert(!Pending.isFailed)
      assert(!Pending.isCanceled)
      assert(Pending.isPending)
    }
    val res1: Outcome = Succeeded
    val ex2 = new Exception
    val res2: Outcome = Failed(ex2)
    val ex3 = new exceptions.TestCanceledException(0)
    val res3: Outcome = Canceled(ex3)
    val res4: Outcome = Pending
    def `can be easily pattern matched on based on whether it is Exceptional` {
      def matchesExceptional(res: Outcome): Boolean =
        res match {
          case _: Exceptional => true
          case _ => false
        }
      assert(!matchesExceptional(res1))
      assert(matchesExceptional(res2))
      assert(matchesExceptional(res3))
      assert(!matchesExceptional(res4))
    }
    def `can be easily pattern matched on, extracting the exception, based on whether it is Exceptional` {
      def insideExceptional(res: Outcome): Option[Throwable] =
        res match {
          case Exceptional(ex) => Some(ex)
          case _ => None
        }
      assert(insideExceptional(res1).isEmpty)
      assert(insideExceptional(res2).value eq ex2)
      assert(insideExceptional(res3).value eq ex3)
      assert(insideExceptional(res4).isEmpty)
    }
    def `can be queried to determine whether or not it is "exceptional"` {
      assert(!res1.isExceptional)
      assert(res2.isExceptional)
      assert(res3.isExceptional)
      assert(!res4.isExceptional)
    }
    def `can be transformed into an Option[Throwable]` {
      assert(res1.toOption.isEmpty)
      assert(res2.toOption.value eq ex2)
      assert(res3.toOption.value eq ex3)
      assert(res4.toOption.isEmpty)
    }
    def `can be implicitly converted to an Iterable so it can be flattened` {
      assert(Vector(res1, res2, res3, res4).flatten === Vector(ex2, ex3))
      val succeededs: Vector[Succeeded.type] = Vector(Succeeded, Succeeded, Succeeded)
      assert(succeededs.flatten === Vector.empty)
      val pendings: Vector[Pending.type] = Vector(Pending, Pending, Pending)
      assert(pendings.flatten === Vector.empty)
      val rtEx1 = new RuntimeException
      val rtEx2 = new RuntimeException
      val faileds: Vector[Failed] = Vector(Failed(rtEx1), Failed(ex2), Failed(rtEx2))
      assert(faileds.flatten === Vector(rtEx1, ex2, rtEx2))
      val tceEx1 = new exceptions.TestCanceledException(1)
      val tceEx2 = new exceptions.TestCanceledException(2)
      val canceleds: Vector[Canceled] = Vector(Canceled(tceEx1), Canceled(ex3), Canceled(tceEx2))
      assert(canceleds.flatten === Vector(tceEx1, ex3, tceEx2))
    }
  }
  object `The Failed class` {
    def `should offer a constructor that takes any exception except for TCE, TPE, and TOE and returns it unchanged from its exception field` {
      val ex2 = new exceptions.TestFailedException(0)
      assert(new Failed(ex2).exception eq ex2)
      val ex3 = new RuntimeException
      assert(new Failed(ex3).exception eq ex3)
    }
  }
  object `The Failed companion object` {
    def `should offer an apply factory method that takes no parameters` {
      val failed = Failed()
      failed.exception match {
        case tfe: exceptions.TestFailedException => 
          assert(tfe.message === None)
          assert(tfe.cause === None)
          assert(tfe.failedCodeFileName === Some(fileName))
          assert(tfe.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(failed.exception + " was not a TestFailedException")
      }
    } 
    def `should offer an apply factory method that takes (and simply holds) an exception` {
      val ex = new RuntimeException
      val failed = Failed(ex)
      assert(failed.exception eq ex)
    }
    def `should offer a "here" factory method that takes an exception and wraps it in a TestFailedException` {
      val ex = new RuntimeException("I meant to do that!")
      val failed = Failed.here(ex)
      failed.exception match {
        case tfe: exceptions.TestFailedException => 
          assert(tfe.message === Some("I meant to do that!"))
          assert(tfe.getCause eq ex)
          assert(tfe.failedCodeFileName === Some(fileName))
          assert(tfe.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(failed.exception + " was not a TestFailedException")
      }
    }
    def `should offer an apply factory method that takes a message` {
      val failed = Failed("Oops!")
      failed.exception match {
        case tfe: exceptions.TestFailedException => 
          assert(tfe.message === Some("Oops!"))
          assert(tfe.cause === None)
          assert(tfe.failedCodeFileName === Some(fileName))
          assert(tfe.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(failed.exception + " was not a TestFailedException")
      }
    }
    def `should offer an apply factory method that takes a message and an exception, simply holding the exception` {
      val ex = new RuntimeException
      val failed = Failed("Oops!", ex)
      failed.exception match {
        case tfe: exceptions.TestFailedException => 
          assert(tfe.message === Some("Oops!"))
          assert(tfe.cause === Some(ex))
          assert(tfe.failedCodeFileName === Some(fileName))
          assert(tfe.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(failed.exception + " was not a TestFailedException")
      }
    }
    def `should throw IAE from its apply factory methods if TestCanceledException is passed` {
      val tce = new exceptions.TestCanceledException(0)
      intercept[IllegalArgumentException] {
        Failed(tce)
      }
      intercept[IllegalArgumentException] {
        Failed("Oops!", tce)
      }
      intercept[IllegalArgumentException] {
        new Failed(tce)
      }
      intercept[IllegalArgumentException] {
        Failed.here(tce)
      }
    }
    def `should throw IAE from its apply factory methods if TestPendingException is passed` {
      val tpe = new exceptions.TestPendingException
      intercept[IllegalArgumentException] {
        Failed(tpe)
      }
      intercept[IllegalArgumentException] {
        Failed("Oops!", tpe)
      }
      intercept[IllegalArgumentException] {
        new Failed(tpe)
      }
      intercept[IllegalArgumentException] {
        Failed.here(tpe)
      }
    }
  }
  object `The Canceled class` {
    def `should offer a constructor that takes a TestCanceledException and returns it unchanged from its exception field` {
      val ex1 = new exceptions.TestCanceledException(0)
      assert(new Canceled(ex1).exception eq ex1)
    }
  }
  object `The Canceled companion object` {
    def `should offer an apply factory method that takes no parameters` {
      val canceled = Canceled()
      canceled.exception match {
        case tce: exceptions.TestCanceledException => 
          assert(tce.message === None)
          assert(tce.cause === None)
          assert(tce.failedCodeFileName === Some(fileName))
          assert(tce.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(canceled.exception + " was not a TestCanceledException")
      }
    }
    def `should offer an apply factory method that takes and holds a TCE, but wraps any other exception in a new TCE` {
      val tce = new exceptions.TestCanceledException(1)
      val canceled = Canceled(tce)
      assert(canceled.exception eq tce)

      val tceAsThrowable: Throwable = tce
      val canceled2 = Canceled(tceAsThrowable)
      assert(canceled2.exception eq tce)

      val re = new RuntimeException
      val canceled3 = Canceled(re)
      assert(canceled3.exception.isInstanceOf[exceptions.TestCanceledException])
      assert(canceled3.exception.getCause eq re)
      val ex1 = new exceptions.TestCanceledException(0)
      assert(new Canceled(ex1).exception eq ex1)
      val ex2 = new exceptions.TestFailedException(0)
      assert(Canceled(ex2).exception.getCause eq ex2)
      val ex3 = new RuntimeException
      assert(Canceled(ex3).exception.getCause eq ex3)
    }
    def `should offer a "here" factory method that takes an exception and wraps it in a TestCanceledException` {
      val ex = new RuntimeException("I meant to do that!")
      val canceled = Canceled.here(ex)
      canceled.exception match {
        case tfe: exceptions.TestCanceledException => 
          assert(tfe.message === Some("I meant to do that!"))
          assert(tfe.getCause eq ex)
          assert(tfe.failedCodeFileName === Some(fileName))
          assert(tfe.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(canceled.exception + " was not a TestCanceledException")
      }
    }
    def `should offer an apply factory method that takes a message` {
      val canceled = Canceled("Oops!")
      canceled.exception match {
        case tfe: exceptions.TestCanceledException => 
          assert(tfe.message === Some("Oops!"))
          assert(tfe.cause === None)
          assert(tfe.failedCodeFileName === Some(fileName))
          assert(tfe.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(canceled.exception + " was not a TestCanceledException")
      }
    }
    def `should offer an apply factory method that takes a message and an exception (simply holding the exception)` {
      val ex = new RuntimeException
      val canceled = Canceled("Oops!", ex)
      canceled.exception match {
        case tfe: exceptions.TestCanceledException => 
          assert(tfe.message === Some("Oops!"))
          assert(tfe.cause === Some(ex))
          assert(tfe.failedCodeFileName === Some(fileName))
          assert(tfe.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(canceled.exception + " was not a TestCanceledException")
      }
    }
  }

  object `The outcomeOf method` {
    def `must transform expression evaluations into the appropriate Outcome class` {
      assert(outcomeOf { 99 } == Succeeded)
      val tfe = new exceptions.TestFailedException(0)
      assert(outcomeOf { throw tfe } === Failed(tfe))
      val iae = new IllegalArgumentException
      assert(outcomeOf { throw iae } === Failed(iae))
      val tce = new exceptions.TestCanceledException(0)
      assert(outcomeOf { throw tce } === Canceled(tce))
      val tpe = new exceptions.TestPendingException
      assert(outcomeOf { throw tpe } === Pending)
    }
    def `if UnknownError is thrown, should complete abruptly with that exception` {
      intercept[UnknownError] {
        outcomeOf { throw new UnknownError }
      }
    }
  }
  
  object `The Outcome's toSucceeded method` {
    
    def `should return itself when it is Succeeded` {
      val outcome: Outcome = Succeeded
      val result = outcome.toSucceeded
      assert(result eq outcome)
    }
    
    def `should throw the containing exception when it is Failed` {
      val tfe = new exceptions.TestFailedException("boom!", 3)
      val outcome1: Outcome = Failed(tfe)
      val e1 = intercept[exceptions.TestFailedException] {
        outcome1.toSucceeded
      }
      assert(e1 eq tfe)
      
      val re = new RuntimeException("boom!")
      val outcome2: Outcome = Failed(re)
      val e2 = intercept[RuntimeException] {
        outcome2.toSucceeded
      }
      assert(e2 eq re)
    }

    def `should throw the containing exception when it is Canceled` {
      val tce = new exceptions.TestCanceledException("boom!", 3)
      val outcome1: Outcome = Canceled(tce)
      val e1 = intercept[exceptions.TestCanceledException] {
        outcome1.toSucceeded
      }
      assert(e1 eq tce)
      
      val re = new RuntimeException("boom!")
      val outcome2: Outcome = Canceled(re)
      val e2 = intercept[exceptions.TestCanceledException] {
        outcome2.toSucceeded
      }
      assert(e2.getCause eq re)
    }

    def `should throw TestPendingException when it is Pending` {
      val outcome2: Outcome = Pending
      intercept[exceptions.TestPendingException] {
        outcome2.toSucceeded
      }
    }
  }
}

