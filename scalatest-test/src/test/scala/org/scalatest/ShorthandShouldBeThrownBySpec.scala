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
import java.io.FileNotFoundException
import Matchers._
import exceptions.TestFailedException

class ShorthandShouldBeThrownBySpec extends FunSpec {
  
  val fileName: String = "ShorthandShouldBeThrownBySpec.scala"
  
  def exceptionExpected(clz: Class[_]): String = 
    Resources.exceptionExpected(clz.getName)
    
  def wrongException(expectedClz: Class[_], actualClz: Class[_]): String = 
    Resources.wrongException(expectedClz.getName, actualClz.getName)
    
  def exceptionNotExpected(clz: Class[_]): String =
    Resources.exceptionNotExpected(clz.getName)
    
  def hadMessageInsteadOfExpectedMessage(left: Throwable, actualMessage: String, expectedMessage: String) : String = 
    FailureMessages.hadMessageInsteadOfExpectedMessage(left, actualMessage, expectedMessage)
  
  describe("a [Exception] should") {
    
    it("do nothing when provided code produce expected exception") {
      a [RuntimeException] shouldBe thrownBy {
        throw new RuntimeException("purposely")
      }
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce any exception") {
      val e = intercept[TestFailedException] {
        a [RuntimeException] shouldBe thrownBy {
          assert(1 === 1)
        }
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception") {
      val e = intercept[TestFailedException] {
        a [RuntimeException] shouldBe thrownBy {
          throw new CustomException("secret file not found")
        }
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[CustomException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }

  class CustomException(message: String) extends Exception(message)
  
  describe("an [Exception] should") {
    
    it("do nothing when provided code produce expected exception") {
      an [UnsupportedOperationException] shouldBe thrownBy {
        throw new UnsupportedOperationException("purposely")
      }
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce any exception") {
      val e = intercept[TestFailedException] {
        an [RuntimeException] shouldBe thrownBy {
          assert(1 === 1)
        }
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception") {
      val e = intercept[TestFailedException] {
        an [RuntimeException] shouldBe thrownBy {
          throw new CustomException("secret file not found")
        }
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[CustomException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }
  
  describe("noException should") {
    
    it("do nothing when no exception is thrown from the provided code") {
      noException shouldBe thrownBy {
        assert(1 === 1)
      }
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code produces exception") {
      val e = intercept[TestFailedException] {
        noException shouldBe thrownBy {
          throw new RuntimeException("purposely")
        }
      }
      assert(e.message === Some(exceptionNotExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }
}
