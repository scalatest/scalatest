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
package org.scalatest.junit

import org.scalatest._
import org.scalactic.source
import SharedHelpers.thisLineNumber
import org.junit.Test

class JUnitTestFailedErrorSpec extends /*FunSpec*/ JUnitSuite /*with Matchers*/ {

  //import AssertionsForJUnit._

  val baseLineNumber = thisLineNumber

  //describe("The JUnitTestFailedError") {

    //it("should give the proper line on fail()") {
    @Test def shouldGiveTheProperLineOnFail() {
      try {
        fail()
      }
      catch {
        case e: JUnitTestFailedError =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
            case None => fail("fail() didn't produce a file name and line number string: " + e.failedCodeFileNameAndLineNumberString, e)
          }
        case e: Throwable =>
          fail("fail() didn't produce a JUnitTestFailedError", e)
      }
    }

    @Test def shouldGiveTheProperLineOnFailMessage() {
      try {
        fail("some message")
      }
      catch {
        case e: JUnitTestFailedError =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
            case None => fail("fail(\"some message\") didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          fail("fail(\"some message\") didn't produce a JUnitTestFailedError", e)
      }
    }

    @Test def shouldGiveTheProperLineOnFailThrowable() {
      try {
        fail(new RuntimeException)
      }
      catch {
        case e: JUnitTestFailedError =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
            case None => fail("fail(throwable) didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          fail("fail(throwable) didn't produce a JUnitTestFailedError", e)
      }
    }

  @Test def shouldGiveTheProperLineOnFailSomeMessageThrowable() {
    try {
      fail("some message", new RuntimeException)
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("fail(\"some message\", throwable) didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("fail(\"some message\", throwable) didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnAssertFalse() {
    try {
      assert(false)
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("assert(false) didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("assert(false) didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnAssertFalseSomeMessage() {
    try {
      assert(false, "some message")
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("assert(false, \"some message\") didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("assert(false, \"some message\") didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnAssert1TripleEqual2() {
    try {
      assert(1 === 2)
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("assert(1 === 2) didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("assert(1 === 2) didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnAssert1TripleEqual2SomeMessage() {
    try {
      assert(1 === 2, "some message")
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("assert(1 === 2, \"some message\") didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("assert(1 === 2, \"some message\") didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnAssertResult12() {
    try {
      assertResult(1) { 2 }
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("assertResult(1) { 2 } didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("assertResult(1) { 2 } didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnAssertResult1SomeMessage2() {
    try {
      assertResult(1, "some message") { 2 }
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("assertResult(1, \"some message\") { 2 } didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("assertResult(1, \"some message\") { 2 } didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnInterceptIllegalArgumentException() {
    try {
      intercept[IllegalArgumentException] {}
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("intercept[IllegalArgumentException] {} didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("intercept[IllegalArgumentException] {} didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldGiveTheProperLineOnInterceptIllegalArgumentExceptionThrowNewRuntimeException() {
    try {
      intercept[IllegalArgumentException] { if (false) 1 else throw new RuntimeException }
    }
    catch {
      case e: JUnitTestFailedError =>
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) => assert(s == ("JUnitTestFailedErrorSpec.scala:" + (thisLineNumber - 5)))
          case None => fail("intercept[IllegalArgumentException] { throw new RuntimeException } didn't produce a file name and line number string", e)
        }
      case e: Throwable =>
        fail("intercept[IllegalArgumentException] { throw new RuntimeException } didn't produce a JUnitTestFailedError", e)
    }
  }

  @Test def shouldReturnTheCauseInBothCauseAndGetCause() {
    val theCause = new IllegalArgumentException("howdy")
    val tfe = new JUnitTestFailedError(Some("doody"), Some(theCause), 3, None)
    assert(tfe.cause.isDefined)
    assert(tfe.cause.get === theCause)
    assert(tfe.getCause == theCause)
  }

  @Test def shouldReturnNoneInCauseAndNullInGetCauseIfNoCause() {
    val tfe = new JUnitTestFailedError(Some("doody"), None, 3, None)
    assert(tfe.cause.isEmpty)
    assert(tfe.getCause == null)
  }
  //}
}
