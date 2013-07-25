/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this thing except in compliance with the License.
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

import SharedHelpers.{thisLineNumber, createTempDirectory}
import enablers.Definition

class ShouldBeDefinedLogicalAndExplicitSpec extends Spec with Matchers {
  
  val fileName: String = "ShouldBeDefinedLogicalAndExplicitSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasNotEqualTo", left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages("didNotEqual", left, right)
  
  def wasNotDefined(left: Any): String = 
    FailureMessages("wasNotDefined", left)
    
  def wasDefined(left: Any): String = 
    FailureMessages("wasDefined", left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
  }
    
  trait Thing {
    def isDefined: Boolean
  }
  
  val something = new Thing {
    val isDefined = true
  }
  
  val nothing = new Thing {
    val isDefined = false
  }
  
  val definition = 
    new Definition[Thing] {
      def isDefined(thing: Thing): Boolean = thing.isDefined
    }
  
  object `Definition matcher` {
    
    object `when work with 'thing should be (defined)'` {
      
      def `should do nothing when thing is defined` {
        (something should (equal (something) and be (defined))) (defaultEquality, definition)
        (something should (be (defined) and equal (something))) (definition, defaultEquality)
        
        (something should (be (something) and be (defined))) (definition)
        (something should (be (defined) and be (something))) (definition)
      }
      
      def `should throw TestFailedException with correct stack depth when thing is not defined` {
        val caught1 = intercept[TestFailedException] {
          (nothing should (equal (nothing) and be (defined))) (defaultEquality, definition)
        }
        assert(caught1.message === Some(equaled(nothing, nothing) + ", but " + wasNotDefined(nothing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (nothing should (be (defined) and equal (nothing))) (definition, defaultEquality)
        }
        assert(caught2.message === Some(wasNotDefined(nothing)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (nothing should (be (nothing) and be (defined))) (definition)
        }
        assert(caught3.message === Some(wasEqualTo(nothing, nothing) + ", but " + wasNotDefined(nothing)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (nothing should (be (defined) and be (nothing))) (definition)
        }
        assert(caught4.message === Some(wasNotDefined(nothing)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'thing should not be defined'` {
      
      def `should do nothing when thing is not defined` {
        (nothing should (not equal something and not be defined)) (defaultEquality, definition)
        (nothing should (not be defined and not equal something)) (definition, defaultEquality)
        
        (nothing should (not be something and not be defined)) (definition)
        (nothing should (not be defined and not be something)) (definition)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not defined` {
        val caught1 = intercept[TestFailedException] {
          (something should (not equal nothing and not be defined)) (defaultEquality, definition)
        }
        assert(caught1.message === Some(didNotEqual(something, nothing) + ", but " + wasDefined(something)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (something should (not be defined and not equal nothing)) (definition, defaultEquality)
        }
        assert(caught2.message === Some(wasDefined(something)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (something should (not be nothing and not be defined)) (definition)
        }
        assert(caught3.message === Some(wasNotEqualTo(something, nothing) + ", but " + wasDefined(something)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (something should (not be defined and not be nothing)) (definition)
        }
        assert(caught4.message === Some(wasDefined(something)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (defined)'` {
      
      def `should do nothing when all(xs) is defined` {
        (all(List(something)) should (be (something) and be (defined))) (definition)
        (all(List(something)) should (be (defined) and be (something))) (definition)
        
        (all(List(something)) should (equal (something) and be (defined))) (defaultEquality, definition)
        (all(List(something)) should (be (defined) and equal (something))) (definition, defaultEquality)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not defined` {
        val left1 = List(nothing)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should (be (nothing) and be (defined))) (definition)
        }
        assert(caught1.message === Some(allError(wasEqualTo(nothing, nothing) + ", but " + wasNotDefined(nothing), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(nothing)
        val caught2 = intercept[TestFailedException] {
          (all(left2) should (be (defined) and be (nothing))) (definition)
        }
        assert(caught2.message === Some(allError(wasNotDefined(nothing), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(nothing)
        val caught3 = intercept[TestFailedException] {
          (all(left3) should (equal (nothing) and be (defined))) (defaultEquality, definition)
        }
        assert(caught3.message === Some(allError(equaled(nothing, nothing) + ", but " + wasNotDefined(nothing), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(nothing)
        val caught4 = intercept[TestFailedException] {
          (all(left4) should (be (defined) and equal (nothing))) (definition, defaultEquality)
        }
        assert(caught4.message === Some(allError(wasNotDefined(nothing), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be defined'` {
      def `should do nothing when all(xs) is not defined` {
        (all(List(nothing)) should (not be defined and not be something)) (definition)
        (all(List(nothing)) should (not be something and not be defined)) (definition)
        
        (all(List(nothing)) should (not be defined and not equal something)) (definition, defaultEquality)
        (all(List(nothing)) should (not equal something and not be defined)) (defaultEquality, definition)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is defined` {
        val left1 = List(something)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should (not be nothing and not be defined)) (definition)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(something, nothing) + ", but " + wasDefined(something), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(something)
        val caught2 = intercept[TestFailedException] {
          (all(left2) should (not be defined and not be nothing)) (definition)
        }
        assert(caught2.message === Some(allError(wasDefined(something), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(something)
        val caught3 = intercept[TestFailedException] {
          (all(left3) should (not equal nothing and not be defined)) (defaultEquality, definition)
        }
        assert(caught3.message === Some(allError(didNotEqual(something, nothing) + ", but " + wasDefined(something), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(something)
        val caught4 = intercept[TestFailedException] {
          (all(left4) should (not be defined and not equal nothing)) (definition, defaultEquality)
        }
        assert(caught4.message === Some(allError(wasDefined(something), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
