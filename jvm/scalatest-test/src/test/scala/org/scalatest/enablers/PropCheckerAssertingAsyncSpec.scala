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

import org.scalatest._
import org.scalactic.Equality
import scala.collection.immutable
import prop.GeneratorDrivenPropertyChecks
import org.scalactic.anyvals.{PosZInt, PosInt}
import exceptions.TestFailedException
import OptionValues._
import scala.concurrent.Future
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers

class PropCheckerAssertingAsyncSpec extends AsyncFunSpec with Matchers with GeneratorDrivenPropertyChecks with LineNumberHelper {

  describe("PropCheckerAsserting") {
    it("The exception reported by an async forAll's Future should include the exception thrown by the property function evaluation as its cause") {
      var innerFutureAssertion: Option[Future[Assertion]] = None
      val forAllFutureAssertion =
        forAll { (i: Int) =>
          val innerFut = Future { i should equal (i + 1) }
          innerFutureAssertion = Some(innerFut)
          innerFut
        }
      forAllFutureAssertion.recoverWith {
        case ex: TestFailedException =>
          innerFutureAssertion.value.recover {
            case innerEx: TestFailedException =>
              ex.cause.value should be theSameInstanceAs (innerEx)
          }
      }
    }

    it("forAll taking a Function1 that result in Future[Assertion] should attempt to shrink the values that cause a property to fail") {
      implicit val stNonZeroIntGen = posInts
       var xs: List[Int] = Nil
       val forAllFutureAssertion =
         forAll { (pi: PosInt) =>
           val i = pi.value
           xs ::= i
           Future { assert(i / i == 1 && (i < 1000 && i != 3)) }
         }
       info("values passed to async forAll: " + xs.toString)
       /*
         3 is one of the canonicals, all of which are less than 1000, so 3 is the smallest
         that would fail the test. The generator in ths case could occasionally produce a 3,
         but that's not one of Int's edges, so getting struck by lightning is more likely, so
         if the following assertion fails, there's a one in (2 ** 32) - 1 chance that the shrink
         values were tried.
       */
      forAllFutureAssertion.recoverWith {
        case tfe: TestFailedException =>
          tfe.cause.value.getMessage should endWith ("3 equaled 3")
      }
    }

    it("should include position and message indicating both that a forAll failed as well as its underlying assertion failure") {
     var innerFutureAssertion: Option[Future[Assertion]] = None
      val forAllFutureAssertion =
        forAll(strings, posZIntsBetween(1, 10)) { (s: String, n: PosZInt) =>
          val s2 = s * n.value
          val innerFut = Future { s2.length should equal (s.length * n.value + 1) }
          innerFutureAssertion = Some(innerFut)
          innerFut
        }
      forAllFutureAssertion.recoverWith {
        case tfe: TestFailedException => 
          info(tfe.toString)
          innerFutureAssertion.value.recover {
            case innerTfe: TestFailedException =>
              val msg = tfe.message.value
              msg should include (Resources.propertyException("") + " (" + tfe.failedCodeFileNameAndLineNumberString.value + ")")
              msg should include (Resources.thrownExceptionsLocation(innerTfe.failedCodeFileNameAndLineNumberString.value))
              msg should include (Resources.thrownExceptionsMessage(innerTfe.message.value))
          }
      }
    }

    it("should include position and message indicating that a forAll failed as well as its underlying exception message if not a StackDepth") {
     var innerFutureAssertion: Option[Future[Assertion]] = None
      val forAllFutureAssertion =
        forAll(strings, posZIntsBetween(1, 10)) { (s: String, n: PosZInt) =>
          val s2 = s * n.value
          val innerFut = Future { throw new Exception("Well, this is embarassing!"); succeed }
          innerFutureAssertion = Some(innerFut)
          innerFut
        }
      forAllFutureAssertion.recoverWith {
        case tfe: TestFailedException =>
          info(tfe.toString)
          innerFutureAssertion.value.recover {
            case innerEx: Throwable =>
              val msg = tfe.message.value
              msg should include (Resources.propertyException("") + " (" + tfe.failedCodeFileNameAndLineNumberString.value + ")")
              msg should include (Resources.thrownExceptionsMessage(innerEx.getMessage))
          }
      }
    }
  }
}

