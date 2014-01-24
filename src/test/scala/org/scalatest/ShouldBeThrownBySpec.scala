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

class ShouldBeThrownBySpec extends Spec {
  
  val fileName: String = "ShouldBeThrownBySpec.scala"
  
  def exceptionExpected(clz: Class[_]): String = 
    Resources("exceptionExpected", clz.getName)
    
  def wrongException(expectedClz: Class[_], actualClz: Class[_]): String = 
    Resources("wrongException", expectedClz.getName, actualClz.getName)
    
  def noExceptionExpected(clz: Class[_]): String = 
    Resources("exceptionNotExpected", clz.getName)
    
  def hadMessageInsteadOfExpectedMessage(left: Throwable, actualMessage: String, expectedMessage: String) : String = 
    FailureMessages("hadMessageInsteadOfExpectedMessage", left, actualMessage, expectedMessage)
  
  object `a [Exception] should` {
    
    def `do nothing when provided code produce expected exception` {
      a [RuntimeException] should be thrownBy {
        throw new RuntimeException("purposely")
      }
    }
    
    def `throw new TestFailedException with correct message and stack depth when provided code does not produce any exception` {
      val e = intercept[TestFailedException] {
        a [RuntimeException] should be thrownBy {
          assert(1 === 1)
        }
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
    
    def `throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception` {
      val e = intercept[TestFailedException] {
        a [RuntimeException] should be thrownBy {
          throw new FileNotFoundException("secret file not found")
        }
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[FileNotFoundException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }
  
  object `an [Exception] should` {
    
    def `do nothing when provided code produce expected exception` {
      an [UnsupportedOperationException] should be thrownBy {
        throw new UnsupportedOperationException("purposely")
      }
    }
    
    def `throw new TestFailedException with correct message and stack depth when provided code does not produce any exception` {
      val e = intercept[TestFailedException] {
        an [RuntimeException] should be thrownBy {
          assert(1 === 1)
        }
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
    
    def `throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception` {
      val e = intercept[TestFailedException] {
        an [RuntimeException] should be thrownBy {
          throw new FileNotFoundException("secret file not found")
        }
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[FileNotFoundException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
  }
  
  object `the [Exception] should` {
    
    def `work return instance of expected exception when provided code produce expected exception` {
      val e = the [FileNotFoundException] thrownBy {
        throw new FileNotFoundException("purposely")
      }
      assert(e.isInstanceOf[FileNotFoundException])
    }
    
    def `throw new TestFailedException with correct message and stack depth when provided code does not produce any exception` {
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
    
    def `throw new TestFailedException with correct message and stack depth when provided code does not produce expected exception` {
      val e = intercept[TestFailedException] {
        val e1 = the [RuntimeException] thrownBy {
          throw new FileNotFoundException("secret file not found")
        }
        assert(e1.isInstanceOf[RuntimeException])
      }
      assert(e.message === Some(wrongException(classOf[RuntimeException], classOf[FileNotFoundException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 7))
    }
    
    def `do nothing when 'should have message' check passed` {
      the [FileNotFoundException] thrownBy {
        throw new FileNotFoundException("purposely")
      } should have message "purposely"
    }
    
    def `throw new TestFailedException with correct message and stack depth when used with 'should have message' and provided code does not produce any exception` {
      val e = intercept[TestFailedException] {
        the [RuntimeException] thrownBy {
          assert(1 === 1)
        } should have message "purposely"
      }
      assert(e.message === Some(exceptionExpected(classOf[RuntimeException])))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 6))
    }
    
    def `throw new TestFailedException with correct message and stack depth when used with 'should have message' and provided code produced expected exception with different message` {
      val fnfe = 
        the [FileNotFoundException] thrownBy {
          throw new FileNotFoundException("secret file not found")
        }
      val e = intercept[TestFailedException] {
        fnfe should have message "file not found"
      }
      assert(e.message === Some(hadMessageInsteadOfExpectedMessage(fnfe, "secret file not found", "file not found")))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  object `noException should` {
    
    def `do nothing when no exception is thrown from the provided code` {
      noException should be thrownBy {
        assert(1 === 1)
      }
    }
    
    def `throw new TestFailedException with correct message and stack depth when provided code produces exception` {
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
