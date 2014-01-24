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

class ShouldExistLogicalOrImplicitSpec extends Spec {
  
  trait Thing {
    def exist: Boolean
  }
  
  val something = new Thing {
    val exist = true
  }
  
  val nothing = new Thing {
    val exist = false
  }
  
  implicit def existenceOfThing[T <: Thing]: Existence[T] =
    new Existence[T] {
      def exists(thing: T): Boolean = thing.exist
    }
  
  val fileName = "ShouldExistLogicalOrImplicitSpec.scala"
  
  def doesNotExist(left: Any): String = 
    FailureMessages("doesNotExist", left)
    
  def exists(left: Any): String = 
    FailureMessages("exists", left)
    
  def wasEqualTo(left: Any, right: Any): String =
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String =
    FailureMessages("wasNotEqualTo", left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
  
  def didNotEqual(left: Any, right: Any): String = 
    FailureMessages("didNotEqual", left, right)
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
  }
    
  object `The exist syntax when used with File` {
    
    def `should do nothing when the file exists` {
      something should (equal (something) or exist)
      something should (equal (nothing) or exist)
      nothing should (equal (nothing) or exist)
      
      something should (exist or equal (something))
      nothing should (exist or equal (nothing))
      something should (exist or equal (nothing))
      
      something should (be (something) or exist)
      something should (be (nothing) or exist)
      nothing should (be (nothing) or exist)
      
      something should (exist or be (something))
      nothing should (exist or be (nothing))
      something should (exist or be (nothing))
    }
    
    def `should throw TFE with correct stack depth and message when the file does not exist` {
      val e1 = intercept[exceptions.TestFailedException] {
        nothing should (equal (something) or exist)
      }
      assert(e1.message === Some(didNotEqual(nothing, something) + ", and " + doesNotExist(nothing)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        nothing should (exist or equal (something))
      }
      assert(e2.message === Some(doesNotExist(nothing) + ", and " + didNotEqual(nothing, something)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        nothing should (be (something) or exist)
      }
      assert(e3.message === Some(wasNotEqualTo(nothing, something) + ", and " + doesNotExist(nothing)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        nothing should (exist or be (something))
      }
      assert(e4.message === Some(doesNotExist(nothing) + ", and " + wasNotEqualTo(nothing, something)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with not and the file does not exists` {
      nothing should (equal (nothing) or not (exist))
      nothing should (equal (something) or not (exist))
      something should (equal (something) or not (exist))
      
      nothing should (not (exist) or equal (nothing))
      something should (not (exist) or equal (something))
      nothing should (not (exist) or equal (something))
      
      nothing should (be (nothing) or not (exist))
      nothing should (be (something) or not (exist))
      something should (be (something) or not (exist))
      
      nothing should (not (exist) or be (nothing))
      something should (not (exist) or be (something))
      nothing should (not (exist) or be (something))
    }
    
    def `should throw TFE with correct stack depth and message when it is used with not and  the file exists` {
      val e1 = intercept[exceptions.TestFailedException] {
        something should (equal (nothing) or not (exist))
      }
      assert(e1.message === Some(didNotEqual(something, nothing) + ", and " + exists(something)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        something should (not (exist) or equal (nothing))
      }
      assert(e2.message === Some(exists(something) + ", and " + didNotEqual(something, nothing)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        something should (be (nothing) or not (exist))
      }
      assert(e3.message === Some(wasNotEqualTo(something, nothing) + ", and " + exists(something)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        something should (not (exist) or be (nothing))
      }
      assert(e4.message === Some(exists(something) + ", and " + wasNotEqualTo(something, nothing)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  object `The exist syntax when used with all(xs)` {
    
    def `should do nothing when the file exists` {
      all(List(something)) should (equal (something) or exist)
      all(List(something)) should (equal (nothing) or exist)
      all(List(nothing)) should (equal (nothing) or exist)
      
      all(List(something)) should (exist or equal (something))
      all(List(nothing)) should (exist or equal (nothing))
      all(List(something)) should (exist or equal (nothing))
      
      all(List(something)) should (be (something) or exist)
      all(List(something)) should (be (nothing) or exist)
      all(List(nothing)) should (be (nothing) or exist)
      
      all(List(something)) should (exist or be (something))
      all(List(nothing)) should (exist or be (nothing))
      all(List(something)) should (exist or be (nothing))
    }
    
    def `should throw TFE with correct stack depth and message when the file does not exist` {
      val left1 = List(nothing)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (something) or exist)
      }
      assert(e1.message === Some(allError(left1, didNotEqual(nothing, something) + ", and " + doesNotExist(nothing), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(nothing)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (exist or equal (something))
      }
      assert(e2.message === Some(allError(left2, doesNotExist(nothing) + ", and " + didNotEqual(nothing, something), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(nothing)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (something) or exist)
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(nothing, something) + ", and " + doesNotExist(nothing), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(nothing)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (exist or be (something))
      }
      assert(e4.message === Some(allError(left4, doesNotExist(nothing) + ", and " + wasNotEqualTo(nothing, something), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should do nothing when it is used with not and the file does not exists` {
      all(List(nothing)) should (equal (nothing) or not (exist))
      all(List(nothing)) should (equal (something) or not (exist))
      all(List(something)) should (equal (something) or not (exist))
      
      all(List(nothing)) should (not (exist) or equal (nothing))
      all(List(something)) should (not (exist) or equal (something))
      all(List(nothing)) should (not (exist) or equal (something))
      
      all(List(nothing)) should (be (nothing) or not (exist))
      all(List(nothing)) should (be (something) or not (exist))
      all(List(something)) should (be (something) or not (exist))
      
      all(List(nothing)) should (not (exist) or be (nothing))
      all(List(something)) should (not (exist) or be (something))
      all(List(nothing)) should (not (exist) or be (something))
    }
    
    def `should throw TFE with correct stack depth and message when it is used with not and  the file exists` {
      val left1 = List(something)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (nothing) or not (exist))
      }
      assert(e1.message === Some(allError(left1, didNotEqual(something, nothing) + ", and " + exists(something), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(something)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (not (exist) or equal (nothing))
      }
      assert(e2.message === Some(allError(left2, exists(something) + ", and " + didNotEqual(something, nothing), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(something)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (nothing) or not (exist))
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(something, nothing) + ", and " + exists(something), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(something)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (not (exist) or be (nothing))
      }
      assert(e4.message === Some(allError(left4, exists(something) + ", and " + wasNotEqualTo(something, nothing), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
  }
}