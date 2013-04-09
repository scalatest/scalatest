/*
 * Copyright 2001-2012 Artima, Inc.
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

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks

class StackDepthExceptionSpec extends FunSpec with ShouldMatchers with TableDrivenPropertyChecks {

  class FunException(
    messageFun: StackDepthException => Option[String],
    cause: Option[Throwable],
    failedCodeStackDepthFun: StackDepthException => Int
  ) extends StackDepthException(messageFun, cause, failedCodeStackDepthFun) {
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

  describe("A StackDepthException") {

    it should behave like aStackDepthExceptionWhenGivenNulls(
      (message, cause, failedCodeStackDepth) => new NoFunException(message, cause, failedCodeStackDepth),
      (messageFun, cause, failedCodeStackDepthFun) => new FunException(messageFun, cause, failedCodeStackDepthFun)
    )
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
  ) {

    it("should throw NPE if passed nulls or Some(null)s") {

      forAll (invalidFunCombos) { (msgFun, cause, fcsdFun) =>
        evaluating {
          newFunSDE(msgFun, cause, fcsdFun)
        } should produce [NullPointerException]
      }

      forAll (invalidNoFunCombos) { (msg, cause) =>
        evaluating {
          newSDE(msg, cause, 17)
        } should produce [NullPointerException]
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
 
