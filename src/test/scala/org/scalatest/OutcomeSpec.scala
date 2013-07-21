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
      assert(!Succeeded.isOmitted)
    }
    def `can be Failed` {
      val ex = new Exception
      assert(!Failed(ex).isSucceeded)
      assert(Failed(ex).isFailed)
      assert(!Failed(ex).isCanceled)
      assert(!Failed(ex).isPending)
      assert(!Failed(ex).isOmitted)
    }
    def `can be Canceled` {
      val ex = new exceptions.TestCanceledException(0)
      assert(!Canceled(ex).isSucceeded)
      assert(!Canceled(ex).isFailed)
      assert(Canceled(ex).isCanceled)
      assert(!Canceled(ex).isPending)
      assert(!Canceled(ex).isOmitted)
    }
    def `can be Pending` {
      assert(!Pending().isSucceeded)
      assert(!Pending().isFailed)
      assert(!Pending().isCanceled)
      assert(Pending().isPending)
      assert(!Pending().isOmitted)
    }
    def `can be Omitted` {
      assert(!Omitted.isSucceeded)
      assert(!Omitted.isFailed)
      assert(!Omitted.isCanceled)
      assert(!Omitted.isPending)
      assert(Omitted.isOmitted)
    }
    val res1: Outcome = Succeeded
    val ex2 = new Exception
    val res2: Outcome = Failed(ex2)
    val ex3 = new exceptions.TestCanceledException(0)
    val res3: Outcome = Canceled(ex3)
    val res4: Outcome = Pending()
    val res5: Outcome = Omitted
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
      assert(!matchesExceptional(res5))
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
      assert(insideExceptional(res5).isEmpty)
    }
    def `can be queried to determine whether or not it is "exceptional"` {
      assert(!res1.isExceptional)
      assert(res2.isExceptional)
      assert(res3.isExceptional)
      assert(!res4.isExceptional)
      assert(!res5.isExceptional)
    }
    def `can be transformed into an Option[Throwable]` {
      assert(res1.toOption.isEmpty)
      assert(res2.toOption.value eq ex2)
      assert(res3.toOption.value eq ex3)
      assert(res4.toOption.isEmpty)
      assert(res5.toOption.isEmpty)
    }
    def `can be implicitly converted to an Iterable so it can be flattened` {
      assert(Vector(res1, res2, res3, res4, res5).flatten === Vector(ex2, ex3))
      val succeededs: Vector[Succeeded.type] = Vector(Succeeded, Succeeded, Succeeded)
      assert(succeededs.flatten === Vector.empty)
      val pendings: Vector[Pending] = Vector(Pending(), Pending(), Pending())
      assert(pendings.flatten === Vector.empty)
      val omitteds: Vector[Omitted.type] = Vector(Omitted, Omitted, Omitted)
      assert(omitteds.flatten === Vector.empty)
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
  object `The Failed companion object` {
    def `should offer a factory method that takes no parameters` {
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
    def `should offer a factory method that takes an exception` {
      val ex = new RuntimeException
      val failed = Failed(ex)
      assert(failed.exception eq ex)
    }
    def `should offer a factory method that takes a message` {
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
    def `should offer a factory method that takes a message and an exception` {
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
  }
  object `The Canceled companion object` {
    def `should offer a factory method that takes no parameters` {
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
    def `should offer a factory method that takes a TestCanceledException` {
      val tce = new exceptions.TestCanceledException(1)
      val canceled = Canceled(tce)
      assert(canceled.exception eq tce)
      val e: Throwable = tce
      val canceled2 = Canceled(e)
      assert(canceled2.exception eq tce)
    }
    def `should offer a factory method that takes a non-TestCanceledException and wraps it in a TestCanceledException` {
      val ex = new RuntimeException
      val canceled = Canceled(ex)
      canceled.exception match {
        case tce: exceptions.TestCanceledException => 
          assert(tce.message === None)
          assert(tce.cause === Some(ex))
          assert(tce.failedCodeFileName === Some(fileName))
          assert(tce.failedCodeLineNumber === Some(thisLineNumber - 6))
        case _ => fail(canceled.exception + " was not a TestCanceledException")
      }
    }
    def `should offer a factory method that takes a message` {
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
    def `should offer a factory method that takes a message and a TestCanceledException` {
      val tce = new exceptions.TestCanceledException(1)
      val canceled = Canceled("Oops!", tce)
      assert(canceled.exception eq tce)
    }
    def `should offer a factory method that takes a message and a non-TestCanceledException and wrap it in a TestCanceledException` {
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
      assert(outcomeOf { throw tpe } === Pending(None))
      val toe = new exceptions.TestOmittedException
      assert(outcomeOf { throw toe } === Omitted)
    }
    def `if UnknownError is thrown, should complete abruptly with that exception` {
      intercept[UnknownError] {
        outcomeOf { throw new UnknownError }
      }
    }
  }
}

