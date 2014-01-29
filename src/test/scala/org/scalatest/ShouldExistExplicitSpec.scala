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

import java.io.File
import SharedHelpers.{createTempDirectory, thisLineNumber}
import enablers.Existence
import Matchers._

class ShouldExistExplicitSpec extends Spec {
  
  trait Thing {
    def exist: Boolean
  }
  
  val something = new Thing {
    val exist = true
  }
  
  val nothing = new Thing {
    val exist = false
  }
  
  val existence = new Existence[Thing] {
    def exists(thing: Thing): Boolean = thing.exist
  }
  
  val fileName = "ShouldExistExplicitSpec.scala"
  
  def doesNotExist(left: Any): String = 
    FailureMessages("doesNotExist", left)
    
  def exists(left: Any): String = 
    FailureMessages("exists", left)
    
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
  }
  
  object `The exist syntax when used with File` {
    
    def `should do nothing when the file exists` {
      (something should exist) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when the file does not exist` {
      val e = intercept[exceptions.TestFailedException] {
        (nothing should exist) (existence)
      }
      assert(e.message === Some(doesNotExist(nothing)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with not and the file does not exists` {
      (nothing should not (exist)) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when it is used with not and  the file exists` {
      val e = intercept[exceptions.TestFailedException] {
        (something should not (exist)) (existence)
      }
      assert(e.message === Some(exists(something)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with shouldNot and the file does not exists` {
      (nothing shouldNot exist) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when it is used with shouldNot and  the file exists` {
      val e = intercept[exceptions.TestFailedException] {
        (something shouldNot exist) (existence)
      }
      assert(e.message === Some(exists(something)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  object `The exist syntax when used with all(xs)` {
    
    def `should do nothing when the file exists` {
      (all(List(something)) should exist) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when the file does not exist` {
      val left = List(nothing)
      val e = intercept[exceptions.TestFailedException] {
        (all(left) should exist) (existence)
      }
      assert(e.message === Some(allError(left, doesNotExist(nothing), thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with not and the file does not exists` {
      (all(List(nothing)) should not (exist)) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when it is used with not and  the file exists` {
      val left = List(something)
      val e = intercept[exceptions.TestFailedException] {
        (all(left) should not (exist)) (existence)
      }
      assert(e.message === Some(allError(left, exists(something), thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with shouldNot and the file does not exists` {
      (all(List(nothing)) shouldNot exist) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when it is used with shouldNot and  the file exists` {
      val left = List(something)
      val e = intercept[exceptions.TestFailedException] {
        (all(left) shouldNot exist) (existence)
      }
      assert(e.message === Some(allError(left, exists(something), thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
}