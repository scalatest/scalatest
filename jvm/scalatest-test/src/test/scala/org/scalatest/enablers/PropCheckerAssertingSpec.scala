/*
 * Copyright 2001-2019 Artima, Inc.
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
import org.scalactic.anyvals.PosZInt
import exceptions.TestFailedException
import OptionValues._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PropCheckerAssertingSpec extends AnyFunSpec with Matchers with GeneratorDrivenPropertyChecks with LineNumberHelper {

  describe("PropCheckerAsserting") {
    it("The exception thrown by forAll should include the exception thrown by the property function evaluation as its cause") {
      var thrownInnerEx: Option[TestFailedException] = None
      val tfe =
        intercept[TestFailedException] {
          forAll { (i: Int) =>
            val innerTfe =
              intercept[TestFailedException] {
                i should equal (i + 1)
              }
              thrownInnerEx = Some(innerTfe)
              throw innerTfe
          }
        }
      tfe.cause.value should be theSameInstanceAs (thrownInnerEx.value)
    }

    ignore("forAll taking a Function1 should attempt to shrink the values that cause a property to fail") {
      // TODO: resurrect this test once we have tuples using the new RoseTree shrink algo
      implicit val stNonZeroIntGen =
        for {
         i <- ints
         j = if (i == 0) 1 else i
       } yield j
       var xs: List[Int] = Nil
       val tfe =
         intercept[TestFailedException] {
           forAll { (i: Int) => xs ::= i; assert(i / i == 1 && (i < 1000 && i != 3)) }
         }
       /*
         3 is one of the canonicals, all of which are less than 1000, so 3 is the smallest
         that would fail the test. The generator in ths case could occasionally produce a 3,
         but that's not one of Int's edges, so getting struck by lightning is more likely, so
         if the following assertion fails, there's a one in (2 ** 32) - 1 chance that the shrink
         values were tried.
       */
       tfe.cause.value.getMessage should endWith ("3 equaled 3")
    }

    it("should include position and message indicating both that a forAll failed as well as its underlying assertion failure") {
      var thrownTfe: Option[TestFailedException] = None
      val tfe =
        intercept[TestFailedException] {
          forAll(strings, posZIntsBetween(1, 10)) { (s: String, n: PosZInt) =>
            val s2 = s * n.value
            val innerTfe =
              intercept[TestFailedException] {
                s2.length should equal (s.length * n.value + 1)
              }
            thrownTfe = Some(innerTfe)
            throw innerTfe
          }
        }
     info(tfe.toString)
     val msg = tfe.message.value
     val innerTfe = thrownTfe.value
     msg should include (Resources.propertyException("") + " (" + tfe.failedCodeFileNameAndLineNumberString.value + ")")
     msg should include (Resources.thrownExceptionsLocation(innerTfe.failedCodeFileNameAndLineNumberString.value))
     msg should include (Resources.thrownExceptionsMessage(innerTfe.message.value))
    }

    it("should include position and message indicating that a forAll failed as well as its underlying exception message if not a StackDepth") {
      var thrownEx: Option[Exception] = None
      val tfe =
        intercept[TestFailedException] {
          forAll(strings, posZIntsBetween(1, 10)) { (s: String, n: PosZInt) =>
            val s2 = s * n.value
            val innerEx =
              intercept[Exception] {
                throw new Exception("Well, this is embarassing!")
              }
            thrownEx = Some(innerEx)
            throw innerEx
          }
        }
     info(tfe.toString)
     val msg = tfe.message.value
     val innerEx = thrownEx.value
     msg should include (Resources.propertyException("") + " (" + tfe.failedCodeFileNameAndLineNumberString.value + ")")
     msg should include (Resources.thrownExceptionsMessage(innerEx.getMessage))
    }
  }
}

