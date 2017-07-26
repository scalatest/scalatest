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

import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import scala.util.Failure
import scala.util.Success
import Assertions.NormalResult
import org.scalatest.exceptions.TestCanceledException
import java.util.Date
import org.scalactic.Prettifier
import org.scalactic.exceptions.NullArgumentException
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.scalatest.concurrent.ScalaFutures

class RecoverMethodsSpec extends FunSpec with RecoverMethods with ScalaFutures {

  // SKIP-SCALATESTJS-START
  implicit val execCtx = scala.concurrent.ExecutionContext.Implicits.global
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY implicit val execCtx = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

  val fileName: String = "RecoverMethodsSpec.scala"

  describe("The recoverToExceptionIf method") {
    it("should recover to subtypes") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      val myEx = new MyException
      val futureMyEx =
        recoverToExceptionIf[MyException] {
          Future { throw myEx }
        }
      assert(futureMyEx.futureValue eq myEx)
      val myExSub = new MyExceptionSubClass
      val futureMyExSub =
        recoverToExceptionIf[MyException] {
          Future { throw myExSub }
        }
      assert(futureMyExSub.futureValue eq myExSub)
      // Try with a trait
      trait MyTrait {
        def someRandomMethod: Int = 42
      }
      class AnotherException extends RuntimeException with MyTrait
      val futureCaught =
        recoverToExceptionIf[MyTrait] {
          Future { throw new AnotherException }
        }
      // Make sure the result type is the type passed in, so I can
      // not cast and still invoke any method on it I want
      val futureInt = futureCaught map { caught => caught.someRandomMethod }
      assert(futureInt.futureValue == 42)
    }

    it("should fail with TFE if no exception is thrown") {
      val futureTfe =
        recoverToExceptionIf[IllegalArgumentException] { Future { "hi" } }
      assert(futureTfe.failed.futureValue.isInstanceOf[TestFailedException])
    }

    it("should return the caught exception") {
      val e = new RuntimeException
      val futureResult =
        recoverToExceptionIf[RuntimeException] {
          Future { throw e }
        }
      assert(futureResult.futureValue eq e)
    }

    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the TFE's cause") {
        val wrongException = new RuntimeException("oops!")
        val futureCaught =
          recoverToExceptionIf[IllegalArgumentException] {
            Future { throw wrongException }
          }
        val caught = futureCaught.failed.futureValue
        assert(caught.isInstanceOf[TestFailedException])
        assert(caught.getCause eq wrongException)
      }
    }
  }

  describe("The recoverToSucceededIf method") {
    it("should recover to subtypes") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      val myEx = new MyException
      val futureMyEx =
        recoverToSucceededIf[MyException] {
          Future { throw myEx }
        }
      assert(futureMyEx.futureValue eq Succeeded)
      val myExSub = new MyExceptionSubClass
      val futureMyExSub =
        recoverToSucceededIf[MyException] {
          Future { throw myExSub }
        }
      assert(futureMyExSub.futureValue eq Succeeded)
      // Try with a trait
      trait MyTrait {
        def someRandomMethod: Int = 42
      }
      class AnotherException extends RuntimeException with MyTrait
      val futureCaught =
        recoverToSucceededIf[MyTrait] {
          Future { throw new AnotherException }
        }
      assert(futureCaught.futureValue eq Succeeded)
    }

    it("should return Succeeded") {
      val e = new RuntimeException
      val futureResult =
        recoverToSucceededIf[RuntimeException] {
          Future { throw e }
        }
      assert(futureResult.futureValue eq Succeeded)
    }

    it("should fail with TFE if no exception is thrown") {
      val futureTfe =
        recoverToSucceededIf[IllegalArgumentException] { Future { "hi" } }
      assert(futureTfe.failed.futureValue.isInstanceOf[TestFailedException])
    }

    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the TFE's cause") {
        val wrongException = new RuntimeException("oops!")
        val futureCaught =
          recoverToSucceededIf[IllegalArgumentException] {
            Future { throw wrongException }
          }
        val caught = futureCaught.failed.futureValue
        assert(caught.isInstanceOf[TestFailedException])
        assert(caught.getCause eq wrongException)
      }
    }
  }
}
