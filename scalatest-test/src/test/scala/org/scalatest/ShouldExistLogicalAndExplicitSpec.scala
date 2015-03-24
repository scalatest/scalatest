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

class ShouldExistLogicalAndExplicitSpec extends Spec {
  
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
  
  val fileName = "ShouldExistLogicalAndExplicitSpec.scala"
  
  def doesNotExist(left: Any): String = 
    FailureMessages.doesNotExist(left)
    
  def exists(left: Any): String = 
    FailureMessages.exists(left)
    
  def wasEqualTo(left: Any, right: Any): String =
    FailureMessages.wasEqualTo(left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String =
    FailureMessages.wasNotEqualTo(left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(left, right)
  
  def didNotEqual(left: Any, right: Any): String = 
    FailureMessages.didNotEqual(left, right)
    
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }
    
  object `The exist syntax when used with File` {
    
    def `should do nothing when the file exists` {
      (something should (equal (something) and exist)) (defaultEquality[Thing{val exist: Boolean}], existence)
      (something should (exist and equal (something))) (existence, defaultEquality[Thing{val exist: Boolean}])
      (something should (be_== (something) and exist)) (existence)
      (something should (exist and be_== (something))) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when the file does not exist` {
      val e1 = intercept[exceptions.TestFailedException] {
        (nothing should (equal (nothing) and exist)) (defaultEquality[Thing{val exist: Boolean}], existence)
      }
      assert(e1.message === Some(equaled(nothing, nothing) + ", but " + doesNotExist(nothing)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        (something should (exist and equal (nothing))) (existence, defaultEquality[Thing{val exist: Boolean}])
      }
      assert(e2.message === Some(exists(something) + ", but " + didNotEqual(something, nothing)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        (nothing should (be_== (nothing) and exist)) (existence)
      }
      assert(e3.message === Some(wasEqualTo(nothing, nothing) + ", but " + doesNotExist(nothing)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        (something should (exist and be_== (nothing))) (existence)
      }
      assert(e4.message === Some(exists(something) + ", but " + wasNotEqualTo(something, nothing)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with not and the file does not exists` {
      (nothing should (equal (nothing) and not (exist))) (defaultEquality[Thing{val exist: Boolean}], existence)
      (nothing should (not (exist) and equal (nothing))) (existence, defaultEquality[Thing{val exist: Boolean}])
      (nothing should (be_== (nothing) and not (exist))) (existence)
      (nothing should (not (exist) and be_== (nothing))) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when it is used with not and  the file exists` {
      val e1 = intercept[exceptions.TestFailedException] {
        (something should (equal (something) and not (exist))) (defaultEquality[Thing{val exist: Boolean}], existence)
      }
      assert(e1.message === Some(equaled(something, something) + ", but " + exists(something)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        (nothing should (not (exist) and equal (something))) (existence, defaultEquality[Thing{val exist: Boolean}])
      }
      assert(e2.message === Some(doesNotExist(nothing) + ", but " + didNotEqual(nothing, something)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        (something should (be_== (something) and not (exist))) (existence)
      }
      assert(e3.message === Some(wasEqualTo(something, something) + ", but " + exists(something)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        (nothing should (not (exist) and be_== (something))) (existence)
      }
      assert(e4.message === Some(doesNotExist(nothing) + ", but " + wasNotEqualTo(nothing, something)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  object `The exist syntax when used with all(xs)` {
    
    def `should do nothing when the file exists` {
      (all(List(something)) should (equal (something) and exist)) (defaultEquality[Thing{val exist: Boolean}], existence)
      (all(List(something)) should (exist and equal (something))) (existence, defaultEquality[Thing{val exist: Boolean}])
      (all(List(something)) should (be_== (something) and exist)) (existence)
      (all(List(something)) should (exist and be_== (something))) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when the file does not exist` {
      val left1 = List(nothing)
      val e1 = intercept[exceptions.TestFailedException] {
        (all(left1) should (equal (nothing) and exist)) (defaultEquality[Thing{val exist: Boolean}], existence)
      }
      assert(e1.message === Some(allError(left1, equaled(nothing, nothing) + ", but " + doesNotExist(nothing), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(something)
      val e2 = intercept[exceptions.TestFailedException] {
        (all(left2) should (exist and equal (nothing))) (existence, defaultEquality[Thing{val exist: Boolean}])
      }
      assert(e2.message === Some(allError(left2, exists(something) + ", but " + didNotEqual(something, nothing), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(nothing)
      val e3 = intercept[exceptions.TestFailedException] {
        (all(left3) should (be_== (nothing) and exist)) (existence)
      }
      assert(e3.message === Some(allError(left3, wasEqualTo(nothing, nothing) + ", but " + doesNotExist(nothing), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(something)
      val e4 = intercept[exceptions.TestFailedException] {
        (all(left4) should (exist and be_== (nothing))) (existence)
      }
      assert(e4.message === Some(allError(left4, exists(something) + ", but " + wasNotEqualTo(something, nothing), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with not and the file does not exists` {
      (all(List(nothing)) should (equal (nothing) and not (exist))) (defaultEquality[Thing{val exist: Boolean}], existence)
      (all(List(nothing)) should (not (exist) and equal (nothing))) (existence, defaultEquality[Thing{val exist: Boolean}])
      (all(List(nothing)) should (be_== (nothing) and not (exist))) (existence)
      (all(List(nothing)) should (not (exist) and be_== (nothing))) (existence)
    }
    
    def `should throw TFE with correct stack depth and message when it is used with not and  the file exists` {
      val left1 = List(something)
      val e1 = intercept[exceptions.TestFailedException] {
        (all(left1) should (equal (something) and not (exist))) (defaultEquality[Thing{val exist: Boolean}], existence)
      }
      assert(e1.message === Some(allError(left1, equaled(something, something) + ", but " + exists(something), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(nothing)
      val e2 = intercept[exceptions.TestFailedException] {
        (all(left2) should (not (exist) and equal (something))) (existence, defaultEquality[Thing{val exist: Boolean}])
      }
      assert(e2.message === Some(allError(left2, doesNotExist(nothing) + ", but " + didNotEqual(nothing, something), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(something)
      val e3 = intercept[exceptions.TestFailedException] {
        (all(left3) should (be_== (something) and not (exist))) (existence)
      }
      assert(e3.message === Some(allError(left3, wasEqualTo(something, something) + ", but " + exists(something), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(nothing)
      val e4 = intercept[exceptions.TestFailedException] {
        (all(left4) should (not (exist) and be_== (something))) (existence)
      }
      assert(e4.message === Some(allError(left4, doesNotExist(nothing) + ", but " + wasNotEqualTo(nothing, something), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
  }
}
