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
package exceptions

import org.scalactic.source
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.time.Second
import org.scalatest.time.Span
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatestplus.junit.JUnitTestFailedError
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StackDepthExceptionSpec extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {

  class FunException(
    messageFun: StackDepthException => Option[String],
    cause: Option[Throwable],
    failedCodeStackDepthFun: StackDepthException => Int
  ) extends StackDepthException(messageFun, cause, Right(failedCodeStackDepthFun)) {
    def severedAtStackDepth: FunException = {
      val truncated = getStackTrace.drop(failedCodeStackDepth)
      val e = new FunException(messageFun, cause, e => 0)
      e.setStackTrace(truncated)
      e
    }
  }

  class NoFunException(
    message: Option[String],
    cause: Option[Throwable],
    failedCodeStackDepth: Int
  ) extends StackDepthException(message, cause, failedCodeStackDepth) {
    def severedAtStackDepth: NoFunException = {
      val truncated = getStackTrace.drop(failedCodeStackDepth)
      val e = new NoFunException(message, cause, 0)
      e.setStackTrace(truncated)
      e
    }
  }

  val invalidFunCombos =
    Table[StackDepthException => Option[String], Option[Throwable], StackDepthException => Int](
      ("messageFun",     "cause",              "failedCodeStackDepthFun"),
      (null,             Some(new Exception),  e => 17),
      (e => Some("hi"),  null,                 e => 17),
      (e => Some("hi"),  Some(null),           e => 17),
      (e => Some("hi"),  Some(new Exception),  null)
    )

  val invalidNoFunCombos =
    Table(
      ("message",   "cause"),
      (null,        Some(new Exception)),
      (Some(null),  Some(new Exception)),
      (Some("hi"),  null),
      (Some("hi"),  Some(null))
    )

  def positionExamples = 
    Table(
      "exception",
      new TestFailedException((_: StackDepthException) => Some("message"), None, Left(source.Position.here), None, Vector.empty),
      // SKIP-SCALATESTJS,NATIVE-START
      new JUnitTestFailedError(Some("message"), None, Left(source.Position.here), None),
      // SKIP-SCALATESTJS,NATIVE-END
      new TestFailedDueToTimeoutException((_: StackDepthException) => Some("message"), None, Left(source.Position.here), None, Span(1, Second)),
      new TableDrivenPropertyCheckFailedException((_: StackDepthException) => "message", None, Left(source.Position.here), None, "undecMsg", List.empty, List.empty, 3),
      new GeneratorDrivenPropertyCheckFailedException((_: StackDepthException) => "message", None, Left(source.Position.here), None, "undecMsg", List.empty, Option(List.empty), List.empty),
      new TestCanceledException((_: StackDepthException) => Some("message"), None, Left(source.Position.here), None),
      new TestRegistrationClosedException("message", Left(source.Position.here)),
      new NotAllowedException("message", None, Left(source.Position.here)),
      new DuplicateTestNameException("the test name", Left(source.Position.here))
   )

  def stackDepthExamples = 
    Table(
      "exception",
      new TestFailedException((_: StackDepthException) => Some("message"), None, Right((_: StackDepthException) => 3), None, Vector.empty),
      // SKIP-SCALATESTJS,NATIVE-START
      new JUnitTestFailedError(Some("message"), None, Right(3), None),
      // SKIP-SCALATESTJS,NATIVE-END
      new TestFailedDueToTimeoutException((_: StackDepthException) => Some("message"), None, Right((_: StackDepthException) => 3), None, Span(1, Second)),
      new TableDrivenPropertyCheckFailedException((_: StackDepthException) => "message", None, Right((_: StackDepthException) => 3), None, "undecMsg", List.empty, List.empty, 3),
      new GeneratorDrivenPropertyCheckFailedException((_: StackDepthException) => "message", None, Right((_: StackDepthException) => 3), None, "undecMsg", List.empty, Option(List.empty), List.empty),
      new TestCanceledException((_: StackDepthException) => Some("message"), None, Right((_: StackDepthException) => 3), None),
      new TestRegistrationClosedException("message", Right((_: StackDepthException) => 3)),
      new NotAllowedException("message", None, Right((_: StackDepthException) => 3)),
      new DuplicateTestNameException("the test name", Right((_: StackDepthException) => 3))
   )

/*
  "The modifyPayload method on TFE" should "return the an exception with an equal message option if passed a function that returns the same option passed to it" in {
    forAll (examples) { e =>
      e.modifyPayload(opt => opt) should equal (e)
    }
  }
*/

  describe("A StackDepth exception") {

    it should behave like aStackDepthExceptionWhenGivenNulls(
      (message, cause, failedCodeStackDepth) => new NoFunException(message, cause, failedCodeStackDepth),
      (messageFun, cause, failedCodeStackDepthFun) => new FunException(messageFun, cause, failedCodeStackDepthFun)
    )

    describe("when created with a stack depth (i.e., not a Position)") {
      it("should return None from its failedCodeFilePathname method") {
        forAll (stackDepthExamples) { ex =>
          ex.failedCodeFilePathname shouldBe None
        }
      }
    }
    describe("when created with a Position (i.e., not a stack depth)") {
      it("should return a Some from its failedCodeFilePathname method") {
        forAll (positionExamples) { ex =>
          ex.failedCodeFilePathname shouldBe defined
        }
      }
    }
  }

  describe("A TestFailedException") {

    it should behave like aStackDepthExceptionWhenGivenNulls(
      (message, cause, failedCodeStackDepth) => new TestFailedException(message, cause, failedCodeStackDepth),
      (messageFun, cause, failedCodeStackDepthFun) => new TestFailedException(messageFun, cause, failedCodeStackDepthFun)
    )
  }

  def aStackDepthExceptionWhenGivenNulls(
    newSDE: (Option[String], Option[Throwable], Int) => StackDepthException,
    newFunSDE: (StackDepthException => Option[String], Option[Throwable], StackDepthException => Int) => StackDepthException
  ): Unit = {

    it("should throw NPE if passed nulls or Some(null)s") {

      forAll (invalidFunCombos) { (msgFun, cause, fcsdFun) =>
        a [IllegalArgumentException] should be thrownBy {
          newFunSDE(msgFun, cause, fcsdFun)
        }
      }

      forAll (invalidNoFunCombos) { (msg, cause) =>
        a [IllegalArgumentException] should be thrownBy {
          newSDE(msg, cause, 17)
        }
      }
    }

    it("should produce the Some(message) from getMessage, or null if message was None") {
    
      val eDefined = newSDE(Some("howdy!"), None, 17)
      eDefined.getMessage should be ("howdy!")
    
      val eEmpty = newSDE(None, None, 17)
      eEmpty.getMessage should be (null)
    }

    it("should produce the Some(cause) from getCause, or null if cause was None") {
    
      val e = new Exception

      val eDefined = newSDE(Some("howdy!"), Some(e), 17)
      eDefined.getCause should be (e)
    
      val eEmpty = newSDE(Some("howdy!"), None, 17)
      eEmpty.getCause should be (null)
    }

    it("should produce the Some(message) from message, or None if message was None") {
    
      val eDefined = newSDE(Some("howdy!"), None, 17)
      eDefined.message should be (Some("howdy!"))
    
      val eEmpty = newSDE(None, None, 17)
      eEmpty.message should be (None)
    }

    it("should produce the Some(cause) from cause, or None if cause was None") {
    
      val e = new Exception

      val eDefined = newSDE(Some("howdy!"), Some(e), 17)
      eDefined.cause should be (Some(e))
    
      val eEmpty = newSDE(Some("howdy!"), None, 17)
      eEmpty.cause should be (None)
    }
  }
}
 
