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

class ShouldBeThrownBySpec extends FunSpec {
  
  val fileName: String = "ShouldBeThrownBySpec.scala"
  
  def exceptionExpected(clz: Class[_]): String = 
    Resources.exceptionExpected(clz.getName)
    
  def wrongException(expectedClz: Class[_], actualClz: Class[_]): String = 
    Resources.wrongException(expectedClz.getName, actualClz.getName)
    
  def noExceptionExpected(clz: Class[_]): String = 
    Resources.exceptionNotExpected(clz.getName)
    
  def hadMessageInsteadOfExpectedMessage(left: Throwable, actualMessage: String, expectedMessage: String) : String = 
    FailureMessages.hadMessageInsteadOfExpectedMessage(left, actualMessage, expectedMessage)

  class TestException(message: String) extends Exception(message)
  
  describe("a [Exception] should") {
    
    it("do nothing when provided code produce expected exception") {
      a [RuntimeException] should be thrownBy {
        throw new RuntimeException("purposely")
      }
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce any exception") {
      val e = intercept[TestFailedException] {
        a [RuntimeException] should be thrownBy {
          assert(1 === 1)
        }
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception") {
      val e = intercept[TestFailedException] {
        a [RuntimeException] should be thrownBy {
          throw new TestException("secret file not found")
        }
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[TestException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }
  
  describe("an [Exception] should") {
    
    it("do nothing when provided code produce expected exception") {
      an [UnsupportedOperationException] should be thrownBy {
        throw new UnsupportedOperationException("purposely")
      }
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce any exception") {
      val e = intercept[TestFailedException] {
        an [RuntimeException] should be thrownBy {
          assert(1 === 1)
        }
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception") {
      val e = intercept[TestFailedException] {
        an [RuntimeException] should be thrownBy {
          throw new TestException("secret file not found")
        }
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[TestException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }
  
  describe("the [Exception] should") {
    
    it("work return instance of expected exception when provided code produce expected exception") {
      val e = the [TestException] thrownBy {
        throw new TestException("purposely")
      }
      assert(e.isInstanceOf[TestException])
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce any exception") {
      val e = intercept[TestFailedException] {
        val e1 = the [RuntimeException] thrownBy {
          assert(1 === 1)
        }
        assert(e1.isInstanceOf[RuntimeException])
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 7))
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception") {
      val e = intercept[TestFailedException] {
        val e1 = the [RuntimeException] thrownBy {
          throw new TestException("secret file not found")
        }
        assert(e1.isInstanceOf[RuntimeException])
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[TestException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 7))
    }
    
    it("do nothing when 'should have message' check passed") {
      the [TestException] thrownBy {
        throw new TestException("purposely")
      } should have message "purposely"
    }
    
    it("throw new TestFailedException with correct message and stack depth when used with 'should have message' and provided code does not produce any exception") {
      val e = intercept[TestFailedException] {
        the [RuntimeException] thrownBy {
          assert(1 === 1)
        } should have message "purposely"
      }
      val offendingLine = thisLineNumber - 4
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      // TODO: Skipped because stack trace for multiline expression in scala-js is wrong, should re-enable this after we got macro based stack depth working or scala-js fix the problem.
      // SKIP-SCALATESTJS-START
      assert(e.failedCodeLineNumber === Some(offendingLine))
      // SKIP-SCALATESTJS-END
    }
    
    it("throw new TestFailedException with correct message and stack depth when used with 'should have message' and provided code produced expected exception with different message") {
      val fnfe = 
        the [TestException] thrownBy {
          throw new TestException("secret file not found")
        }
      val e = intercept[TestFailedException] {
        fnfe should have message "file not found"
      }
      assert(e.message === Some(hadMessageInsteadOfExpectedMessage(fnfe, "secret file not found", "file not found")))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  describe("noException should") {
    
    it("do nothing when no exception is thrown from the provided code") {
      noException should be thrownBy {
        assert(1 === 1)
      }
    }
    
    it("throw new TestFailedException with correct message and stack depth when provided code produces exception") {
      val e = intercept[TestFailedException] {
        noException should be thrownBy {
          throw new RuntimeException("purposely")
        }
      }
      assert(e.message === Some(noExceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }
}
