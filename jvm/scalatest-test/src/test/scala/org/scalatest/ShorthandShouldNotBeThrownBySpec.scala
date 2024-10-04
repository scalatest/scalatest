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
package org.scalatest

import SharedHelpers.thisLineNumber
import java.io.FileNotFoundException
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShorthandShouldNotBeThrownBySpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val fileName: String = "ShorthandShouldNotBeThrownBySpec.scala"
    
  def exceptionNotExpected(clz: Class[_]): String = 
    Resources.exceptionNotExpected(clz.getName)
    
  def exceptionExpected(clz: Class[_]): String = 
    Resources.exceptionExpected(clz.getName)
    
  def hadExpectedMessage(left: Throwable, expectedMessage: String): String = 
    FailureMessages.hadExpectedMessage(prettifier, left, expectedMessage)

  class TestException(msg: String) extends Exception(msg)
  
  describe("the [Exception] 'should not have message' syntax should") {
    
    it("do nothing when 'should have message' exception's message not equal expected") {
      the [TestException] thrownBy {
        throw new TestException("purposely")
      } shouldNot have message ("accidentally")
    }
    
    it("throw new TestFailedException with correct message and stack depth when used with 'should not have message' and provided code produced exception that has message equal to expected") {
      val fnfe = 
        the [TestException] thrownBy {
          throw new TestException("purposely")
        }
      val e = intercept[TestFailedException] {
        fnfe shouldNot have message ("purposely")
      }
      assert(e.message === Some(hadExpectedMessage(fnfe, "purposely")))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("throw new TestFailedException with correct message and stack depth when used with 'should not have message' and provided code does not produce any exception") {
      val e = intercept[TestFailedException] {
        the [RuntimeException] thrownBy {
          assert(1 === 1)
        } shouldNot have message ("purposely")
      }
      val offendingLine = thisLineNumber - 4
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      // TODO: should re-enable this when we got the macro-based source location working.
      // SKIP-SCALATESTJS,NATIVE-START
      assert(e.failedCodeLineNumber === Some(offendingLine))
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}
