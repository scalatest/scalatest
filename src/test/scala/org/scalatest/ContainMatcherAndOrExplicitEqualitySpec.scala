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

import org.scalautils.Equality
import org.scalautils.Explicitly
import SharedHelpers._

class ContainMatcherAndOrExplicitEqualitySpec extends Spec with Matchers with Explicitly {

  val equality = new Equality[String] {
    def areEqual(left: String, right: Any) = 
      left.trim == (right match {
        case s: String => s.trim
        case other => other
      })
  }
  
  object `ContainMatcher ` {
    
    object `when use with 'and'` {
      
      def `should pass when both contain passes` {
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 3", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        (left should (contain theSameElementsAs (right1) and contain theSameElementsAs (right2))) (equality, equality)
        (left should (contain theSameElementsAs (right1) and contain theSameElementsInOrderAs (right2))) (equality, equality)
        (left should (contain theSameElementsAs (right1) and contain allOf (" 3", "2 ", " 1"))) (equality, equality)
        (left should (contain theSameElementsAs (right1) and contain inOrder (" 1", "2 ", " 3"))) (equality, equality)
        (left should (contain theSameElementsAs (right1) and contain oneOf (" 1", " 3", "5 "))) (equality, equality)
        (left should (contain theSameElementsAs (right1) and contain only (" 3", " 1", "2 "))) (equality, equality)
        (left should (contain theSameElementsAs (right1) and contain inOrderOnly (" 1", "2 ", " 3"))) (equality, equality)
        (left should (contain theSameElementsAs (right1) and contain noneOf (" 7", "8 ", " 9"))) (equality, equality)
        (left should (contain theSameElementsInOrderAs (right2) and contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain allOf (" 3", " 1", "2 ") and contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain inOrder (" 1", "2 ", " 3") and contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain oneOf (" 1", " 3", "5 ") and contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain only (" 3", " 1", "2 ") and contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain inOrderOnly (" 1", "2 ", " 3") and contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain noneOf (" 7", "8 ", " 9") and contain theSameElementsAs (right1))) (equality, equality)
        
      }
      
      def `should failed with correctly stack depth and message when first contain failed but second contain passed` {
        val left = List("1 ", " 2", "3 ")
        val right = List(" 3", "2 ", " 1")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (contain theSameElementsInOrderAs right and contain theSameElementsAs right)) (equality, equality)
        }
        e.message should be (Some(left + " did not contain the same elements in the same (iterated) order as " + right))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first contain passed but second contain failed` {
        val left = List("1 ", " 2", "3 ")
        val right = List(" 3", "2 ", " 1")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (contain theSameElementsAs right and contain theSameElementsInOrderAs right)) (equality, equality) 
        }
        e.message should be (Some(left + " contained the same elements as " + right + ", but " + left + " did not contain the same elements in the same (iterated) order as " + right))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should pass when not contain and contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        (left should (not contain theSameElementsAs (right1) and contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain theSameElementsInOrderAs (right1) and contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain allOf ("8 ", "2 ", " 1") and contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain inOrder (" 1", "2 ", "8 ") and contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain oneOf (" 6", "8 ", " 5") and contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain only ("8 ", " 1", "2 ") and contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain inOrderOnly (" 1", "2 ", "8 ") and contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain inOrderOnly (" 1", "2 ", "8 ") and contain theSameElementsAs (right2))) (equality, equality)
      }
      
      def `should pass when contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 8", "1 ", " 2")
        val right2 = List(" 1", "2 ", " 3")
        
        (left should ((contain theSameElementsAs (right2)) and not contain theSameElementsAs (right1))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) and not contain theSameElementsInOrderAs (right1))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) and not contain allOf (" 8", " 2", "1 "))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) and not contain inOrder ("1 ", " 2", " 8"))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) and not contain oneOf ("6 ", " 8", "5 "))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) and not contain only (" 8", "1 ", " 2"))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) and not contain inOrderOnly ("1 ", " 2", " 8"))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) and not contain noneOf ("1 ", " 2", " 8"))) (equality, equality)
      }
      
      def `should pass when not contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 8", "1 ", " 2")
        val right2 = List("1 ", " 2", " 8")
        
        (left should (not contain theSameElementsAs (right2) and not contain theSameElementsAs (right1))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) and not contain theSameElementsInOrderAs (right1))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) and not contain allOf (" 8", " 2", "1 "))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) and not contain inOrder ("1 ", " 2", " 8"))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) and not contain oneOf ("6 ", " 8", "5 "))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) and not contain only (" 8", "1 ", " 2"))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) and not contain inOrderOnly ("1 ", " 2", " 8"))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) and not contain noneOf ("1 ", " 2", " 8"))) (equality, equality)
      }
      
      def `should failed with correctly stack depth and message when first not contain failed but second contain passed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 3", "2 ", " 1")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (not contain theSameElementsInOrderAs (right1) and contain theSameElementsAs right2)) (equality, equality) 
        }
        e.message should be (Some(left + " contained the same elements in the same (iterated) order as " + right1))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first contain passed but second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 3", "2 ", " 1")
        val right2 = List(" 1", "2 ", " 3")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should ((contain theSameElementsAs right1) and not contain theSameElementsInOrderAs (right2))) (equality, equality) 
        }
        e.message should be (Some(left + " contained the same elements as " + right1 + ", but " + left + " contained the same elements in the same (iterated) order as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 3", "2 ", " 1")        
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (not contain theSameElementsInOrderAs (right1) and not contain theSameElementsAs (right2))) (equality, equality)
        }
        e.message should be (Some(left + " contained the same elements in the same (iterated) order as " + right1))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
    }
    
    object `when use with 'or'` {
      
      def `should pass when one of contain passes` {
        val left = List("1 ", " 2", "3 ")
        val right1 = List("5 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        (left should (contain theSameElementsAs (right1) or contain theSameElementsAs (right2))) (equality, equality)
        (left should (contain theSameElementsAs (right1) or contain theSameElementsInOrderAs (right2))) (equality, equality)
        (left should (contain theSameElementsAs (right1) or contain allOf (" 3", "2 ", " 1"))) (equality, equality)
        (left should (contain theSameElementsAs (right1) or contain inOrder (" 1", "2 ", " 3"))) (equality, equality)
        (left should (contain theSameElementsAs (right1) or contain oneOf (" 1", " 3", "5 "))) (equality, equality)
        (left should (contain theSameElementsAs (right1) or contain only (" 3", " 1", "2 "))) (equality, equality)
        (left should (contain theSameElementsAs (right1) or contain inOrderOnly (" 1", "2 ", " 3"))) (equality, equality)
        (left should (contain theSameElementsAs (right1) or contain noneOf ("7 ", " 8", "9 "))) (equality, equality)
        (left should (contain theSameElementsInOrderAs (right2) or contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain allOf (" 3", "2 ", " 1") or contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain inOrder (" 1", "2 ", " 3") or contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain oneOf (" 1", " 3", "5 ") or contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain only (" 3", " 1", "2 ") or contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain inOrderOnly (" 1", "2 ", " 3") or contain theSameElementsAs (right1))) (equality, equality)
        (left should (contain noneOf ("7 ", " 8", "9 ") or contain theSameElementsAs (right1))) (equality, equality)
        
      }
      
      def `should failed with correctly stack depth and message when both of contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 3", "8 ", " 1")
        val right2 = List(" 3", "2 ", " 1")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (contain theSameElementsAs right1 or contain theSameElementsInOrderAs right2)) (equality, equality)
        }
        e.message should be (Some(left + " did not contain the same elements as " + right1 + ", and " + left + " did not contain the same elements in the same (iterated) order as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should pass when not contain and contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        (left should (not contain theSameElementsAs (right1) or contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain theSameElementsInOrderAs (right1) or contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain allOf ("8 ", "2 ", " 1") or contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain inOrder (" 1", "2 ", "8 ") or contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain oneOf (" 6", "8 ", " 5") or contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain only ("8 ", " 1", "2 ") or contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain inOrderOnly (" 1", "2 ", "8 ") or contain theSameElementsAs (right2))) (equality, equality)
        (left should (not contain noneOf (" 1", "2 ", "8 ") or contain theSameElementsAs (right2))) (equality, equality)
      }
      
      def `should pass when contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        (left should ((contain theSameElementsAs (right2)) or not contain theSameElementsAs (right1))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) or not contain theSameElementsInOrderAs (right1))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) or not contain allOf ("8 ", "2 ", " 1"))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) or not contain inOrder (" 1", "2 ", "8 "))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) or not contain oneOf (" 6", "8 ", " 5"))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) or not contain only ("8 ", " 1", "2 "))) (equality, equality)
        (left should ((contain theSameElementsAs (right2)) or not contain inOrderOnly (" 1", "2 ", "8 ")))(equality, equality)
        (left should ((contain theSameElementsAs (right2)) or not contain noneOf (" 1", "2 ", "8 "))) (equality, equality)
        
      }
      
      def `should pass when not contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", "8 ")
        
        (left should (not contain theSameElementsAs (right2) or not contain theSameElementsAs (right1))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) or not contain theSameElementsInOrderAs (right1))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) or not contain allOf ("8 ", "2 ", " 1"))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) or not contain inOrder (" 1", "2 ", "8 "))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) or not contain oneOf (" 6", "8 ", " 5"))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) or not contain only ("8 ", " 1", "2 "))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) or not contain inOrderOnly (" 1", "2 ", "8 "))) (equality, equality)
        (left should (not contain theSameElementsAs (right2) or not contain noneOf (" 1", "2 ", "8 "))) (equality, equality)
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 8", "2 ", " 1")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (not contain theSameElementsInOrderAs (right1) or contain theSameElementsAs right2)) (equality, equality)
        }
        e.message should be (Some(left + " contained the same elements in the same (iterated) order as " + right1 + ", and " + left + " did not contain the same elements as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first contain failed and second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right = List(" 3", "2 ", " 1")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (contain theSameElementsInOrderAs (right) or not contain theSameElementsAs (right))) (equality, equality) 
        }
        e.message should be (Some(left + " did not contain the same elements in the same (iterated) order as " + right + ", and " + left + " contained the same elements as " + right))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 3", "2 ", " 1")
        
        val e = intercept[exceptions.TestFailedException] {
          (left should (not contain theSameElementsInOrderAs (right1) or not contain theSameElementsAs (right2))) (equality, equality) 
        }
        e.message should be (Some(left + " contained the same elements in the same (iterated) order as " + right1 + ", and " + left + " contained the same elements as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrExplicitEqualitySpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
    }
    
  }
  
}
