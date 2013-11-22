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
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ContainMatcherAndOrEqualitySpec extends Spec with Matchers {
  
  implicit val equality = new Equality[String] {
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
        
        left should (contain theSameElementsAs (right1) and contain theSameElementsAs (right2)) 
        left should ((contain theSameElementsAs (right1)) and (contain theSameElementsAs (right2)))
        left should (contain theSameElementsAs (right1) and (contain theSameElementsAs (right2))) 
        
        left should (contain theSameElementsAs (right1) and contain theSameElementsInOrderAs (right2)) 
        left should ((contain theSameElementsAs (right1)) and (contain theSameElementsInOrderAs (right2)))
        left should (contain theSameElementsAs (right1) and (contain theSameElementsInOrderAs (right2)))
        
        left should (contain theSameElementsAs (right1) and contain allOf (" 3", "2 ", " 1"))
        left should ((contain theSameElementsAs (right1)) and (contain allOf (" 3", "2 ", " 1")))
        left should (contain theSameElementsAs (right1) and (contain allOf (" 3", "2 ", " 1")))
        
        left should (contain theSameElementsAs (right1) and contain inOrder (" 1", "2 ", " 3"))
        left should ((contain theSameElementsAs (right1)) and (contain inOrder (" 1", "2 ", " 3")))
        left should (contain theSameElementsAs (right1) and (contain inOrder (" 1", "2 ", " 3")))
        
        left should (contain theSameElementsAs (right1) and contain oneOf (" 1", " 4", "5 "))
        left should ((contain theSameElementsAs (right1)) and (contain oneOf (" 1", " 4", "5 ")))
        left should (contain theSameElementsAs (right1) and (contain oneOf (" 1", " 4", "5 ")))
        
        left should (contain theSameElementsAs (right1) and contain atLeastOneOf (" 1", " 3", "5 "))
        left should ((contain theSameElementsAs (right1)) and (contain atLeastOneOf (" 1", " 3", "5 ")))
        left should (contain theSameElementsAs (right1) and (contain atLeastOneOf (" 1", " 3", "5 ")))
        
        left should (contain theSameElementsAs (right1) and contain only (" 3", " 1", "2 "))
        left should ((contain theSameElementsAs (right1)) and (contain only (" 3", " 1", "2 ")))
        left should (contain theSameElementsAs (right1) and (contain only (" 3", " 1", "2 ")))
        
        left should (contain theSameElementsAs (right1) and contain inOrderOnly (" 1", "2 ", " 3"))
        left should ((contain theSameElementsAs (right1)) and (contain inOrderOnly (" 1", "2 ", " 3")))
        left should (contain theSameElementsAs (right1) and (contain inOrderOnly (" 1", "2 ", " 3")))
        
        left should (contain theSameElementsAs (right1) and contain noneOf (" 7", "8 ", " 9"))
        left should ((contain theSameElementsAs (right1)) and (contain noneOf (" 7", "8 ", " 9")))
        left should (contain theSameElementsAs (right1) and (contain noneOf (" 7", "8 ", " 9")))
        
        left should (contain theSameElementsInOrderAs (right2) and contain theSameElementsAs (right1))
        left should ((contain theSameElementsInOrderAs (right2)) and (contain theSameElementsAs (right1)))
        left should (contain theSameElementsInOrderAs (right2) and (contain theSameElementsAs (right1)))
        
        left should (contain allOf (" 3", " 1", "2 ") and contain theSameElementsAs (right1))
        left should ((contain allOf (" 3", " 1", "2 ")) and (contain theSameElementsAs (right1)))
        left should (contain allOf (" 3", " 1", "2 ") and (contain theSameElementsAs (right1)))
        
        left should (contain inOrder (" 1", "2 ", " 3") and contain theSameElementsAs (right1))
        left should ((contain inOrder (" 1", "2 ", " 3")) and (contain theSameElementsAs (right1)))
        left should (contain inOrder (" 1", "2 ", " 3") and (contain theSameElementsAs (right1)))
        
        left should (contain oneOf (" 1", " 4", "5 ") and contain theSameElementsAs (right1))
        left should ((contain oneOf (" 1", " 4", "5 ")) and (contain theSameElementsAs (right1)))
        left should (contain oneOf (" 1", " 4", "5 ") and (contain theSameElementsAs (right1)))
        
        left should (contain atLeastOneOf (" 1", " 3", "5 ") and contain theSameElementsAs (right1))
        left should ((contain atLeastOneOf (" 1", " 3", "5 ")) and (contain theSameElementsAs (right1)))
        left should (contain atLeastOneOf (" 1", " 3", "5 ") and (contain theSameElementsAs (right1)))
        
        left should (contain only (" 3", " 1", "2 ") and contain theSameElementsAs (right1))
        left should ((contain only (" 3", " 1", "2 ")) and (contain theSameElementsAs (right1)))
        left should (contain only (" 3", " 1", "2 ") and (contain theSameElementsAs (right1)))
        
        left should (contain inOrderOnly (" 1", "2 ", " 3") and contain theSameElementsAs (right1))
        left should ((contain inOrderOnly (" 1", "2 ", " 3")) and (contain theSameElementsAs (right1)))
        left should (contain inOrderOnly (" 1", "2 ", " 3") and (contain theSameElementsAs (right1)))
        
        left should (contain noneOf (" 7", "8 ", " 9") and contain theSameElementsAs (right1))
        left should ((contain noneOf (" 7", "8 ", " 9")) and (contain theSameElementsAs (right1)))
        left should (contain noneOf (" 7", "8 ", " 9") and (contain theSameElementsAs (right1)))
      }
      
      def `should failed with correctly stack depth and message when first contain failed but second contain passed` {
        val left = List("1 ", " 2", "3 ")
        val right = List(" 3", "2 ", " 1")
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsInOrderAs right and contain theSameElementsAs right) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsInOrderAs right) and (contain theSameElementsAs right)) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsInOrderAs right and (contain theSameElementsAs right)) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first contain passed but second contain failed` {
        val left = List("1 ", " 2", "3 ")
        val right = List(" 3", "2 ", " 1")
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsAs right and contain theSameElementsInOrderAs right) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right) + ", but " + decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsAs right) and (contain theSameElementsInOrderAs right)) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right) + ", but " + decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsAs right and (contain theSameElementsInOrderAs right)) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right) + ", but " + decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain and contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        left should (not contain theSameElementsAs (right1) and contain theSameElementsAs (right2)) 
        left should ((not contain theSameElementsAs (right1)) and (contain theSameElementsAs (right2)))
        left should (not { contain theSameElementsAs (right1) } and contain theSameElementsAs (right2))
        
        left should (not contain theSameElementsInOrderAs (right1) and contain theSameElementsAs (right2))
        left should ((not contain theSameElementsInOrderAs (right1)) and (contain theSameElementsAs (right2)))
        left should (not { contain theSameElementsInOrderAs (right1) } and contain theSameElementsAs (right2))
        
        left should (not contain allOf ("8 ", "2 ", " 1") and contain theSameElementsAs (right2))
        left should ((not contain allOf ("8 ", "2 ", " 1")) and (contain theSameElementsAs (right2)))
        left should (not { contain allOf ("8 ", "2 ", " 1") } and contain theSameElementsAs (right2))
        
        left should (not contain inOrder (" 1", "2 ", "8 ") and contain theSameElementsAs (right2))
        left should ((not contain inOrder (" 1", "2 ", "8 ")) and (contain theSameElementsAs (right2)))
        left should (not { contain inOrder (" 1", "2 ", "8 ") } and contain theSameElementsAs (right2))
        
        left should (not contain oneOf (" 6", "8 ", " 5") and contain theSameElementsAs (right2))
        left should ((not contain oneOf (" 6", "8 ", " 5")) and (contain theSameElementsAs (right2)))
        left should (not { contain oneOf (" 6", "8 ", " 5") } and contain theSameElementsAs (right2))
        
        left should (not contain only ("8 ", " 1", "2 ") and contain theSameElementsAs (right2))
        left should ((not contain only ("8 ", " 1", "2 ")) and (contain theSameElementsAs (right2)))
        left should (not { contain only ("8 ", " 1", "2 ") } and contain theSameElementsAs (right2))
        
        left should (not contain inOrderOnly (" 1", "2 ", "8 ") and contain theSameElementsAs (right2))
        left should ((not contain inOrderOnly (" 1", "2 ", "8 ")) and (contain theSameElementsAs (right2)))
        left should (not { contain inOrderOnly (" 1", "2 ", "8 ") } and contain theSameElementsAs (right2))
        
        left should (not contain noneOf (" 1", "2 ", "8 ") and contain theSameElementsAs (right2))
        left should ((not contain inOrderOnly (" 1", "2 ", "8 ")) and (contain theSameElementsAs (right2)))
        left should (not { contain inOrderOnly (" 1", "2 ", "8 ") } and contain theSameElementsAs (right2))
      }
      
      def `should pass when contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 8", "1 ", " 2")
        val right2 = List(" 1", "2 ", " 3")
        
        left should (contain theSameElementsAs (right2) and not contain theSameElementsAs (right1))
        left should ((contain theSameElementsAs (right2)) and (not contain theSameElementsAs (right1)))
        left should (contain theSameElementsAs (right2) and not { contain theSameElementsAs (right1) })
        
        left should (contain theSameElementsAs (right2) and not contain theSameElementsInOrderAs (right1))
        left should ((contain theSameElementsAs (right2)) and (not contain theSameElementsInOrderAs (right1)))
        left should ((contain theSameElementsAs (right2)) and not { contain theSameElementsInOrderAs (right1) })
        
        left should (contain theSameElementsAs (right2) and not contain allOf (" 8", " 2", "1 "))
        left should ((contain theSameElementsAs (right2)) and (not contain allOf (" 8", " 2", "1 ")))
        left should (contain theSameElementsAs (right2) and not { contain allOf (" 8", " 2", "1 ") })
        
        left should (contain theSameElementsAs (right2) and not contain inOrder ("1 ", " 2", " 8"))
        left should ((contain theSameElementsAs (right2)) and (not contain inOrder ("1 ", " 2", " 8")))
        left should (contain theSameElementsAs (right2) and not { contain inOrder ("1 ", " 2", " 8") })
        
        left should (contain theSameElementsAs (right2) and not contain oneOf ("6 ", " 8", "5 "))
        left should ((contain theSameElementsAs (right2)) and (not contain oneOf ("6 ", " 8", "5 ")))
        left should (contain theSameElementsAs (right2) and not { contain oneOf ("6 ", " 8", "5 ") })
        
        left should (contain theSameElementsAs (right2) and not contain only (" 8", "1 ", " 2"))
        left should ((contain theSameElementsAs (right2)) and (not contain only (" 8", "1 ", " 2")))
        left should (contain theSameElementsAs (right2) and not { contain only (" 8", "1 ", " 2") })
        
        left should (contain theSameElementsAs (right2) and not contain inOrderOnly ("1 ", " 2", " 8"))
        left should ((contain theSameElementsAs (right2)) and (not contain inOrderOnly ("1 ", " 2", " 8")))
        left should (contain theSameElementsAs (right2) and not { contain inOrderOnly ("1 ", " 2", " 8") })
        
        left should (contain theSameElementsAs (right2) and not contain noneOf ("1 ", " 2", " 8"))
        left should ((contain theSameElementsAs (right2)) and (not contain noneOf ("1 ", " 2", " 8")))
        left should (contain theSameElementsAs (right2) and not { contain noneOf ("1 ", " 2", " 8") })
      }
      
      def `should pass when not contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 8", "1 ", " 2")
        val right2 = List("1 ", " 2", " 8")
        
        left should (not contain theSameElementsAs (right2) and not contain theSameElementsAs (right1)) 
        left should ((not contain theSameElementsAs (right2)) and (not contain theSameElementsAs (right1)))
        left should (not { contain theSameElementsAs (right2) } and not { contain theSameElementsAs (right1) })
        
        left should (not contain theSameElementsAs (right2) and not contain theSameElementsInOrderAs (right1))
        left should ((not contain theSameElementsAs (right2)) and (not contain theSameElementsInOrderAs (right1)))
        left should (not { contain theSameElementsAs (right2) } and not { contain theSameElementsInOrderAs (right1) })
        
        left should (not contain theSameElementsAs (right2) and not contain allOf (" 8", " 2", "1 "))
        left should ((not contain theSameElementsAs (right2)) and (not contain allOf (" 8", " 2", "1 ")))
        left should (not { contain theSameElementsAs (right2) } and not { contain allOf (" 8", " 2", "1 ") })
        
        left should (not contain theSameElementsAs (right2) and not contain inOrder ("1 ", " 2", " 8"))
        left should ((not contain theSameElementsAs (right2)) and (not contain inOrder ("1 ", " 2", " 8")))
        left should (not { contain theSameElementsAs (right2) } and not { contain inOrder ("1 ", " 2", " 8") })
        
        left should (not contain theSameElementsAs (right2) and not contain oneOf ("6 ", " 8", "5 "))
        left should ((not contain theSameElementsAs (right2)) and (not contain oneOf ("6 ", " 8", "5 ")))
        left should (not { contain theSameElementsAs (right2) } and not { contain oneOf ("6 ", " 8", "5 ") })
        
        left should (not contain theSameElementsAs (right2) and not contain only (" 8", "1 ", " 2"))
        left should ((not contain theSameElementsAs (right2)) and (not contain only (" 8", "1 ", " 2")))
        left should (not contain { theSameElementsAs (right2) } and not { contain only (" 8", "1 ", " 2") })
        
        left should (not contain theSameElementsAs (right2) and not contain inOrderOnly ("1 ", " 2", " 8"))
        left should ((not contain theSameElementsAs (right2)) and (not contain inOrderOnly ("1 ", " 2", " 8")))
        left should (not { contain theSameElementsAs (right2) } and not { contain inOrderOnly ("1 ", " 2", " 8") })
        
        left should (not contain theSameElementsAs (right2) and not contain noneOf ("1 ", " 2", " 8"))
        left should ((not contain theSameElementsAs (right2)) and (not contain noneOf ("1 ", " 2", " 8")))
        left should (not { contain theSameElementsAs (right2) } and not { contain noneOf ("1 ", " 2", " 8") })
      }
      
      def `should failed with correctly stack depth and message when first not contain failed but second contain passed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 3", "2 ", " 1")
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain theSameElementsInOrderAs (right1) and contain theSameElementsAs right2) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain theSameElementsInOrderAs (right1)) and (contain theSameElementsAs right2)) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not { contain theSameElementsInOrderAs (right1) } and contain theSameElementsAs right2) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first contain passed but second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 3", "2 ", " 1")
        val right2 = List(" 1", "2 ", " 3")
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsAs right1 and not contain theSameElementsInOrderAs (right2)) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right1) + ", but " + decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right2)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsAs right1) and (not contain theSameElementsInOrderAs (right2))) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right1) + ", but " + decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right2)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsAs right1 and not { contain theSameElementsInOrderAs (right2) }) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right1) + ", but " + decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right2)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 3", "2 ", " 1")        
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain theSameElementsInOrderAs (right1) and not contain theSameElementsAs (right2)) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain theSameElementsInOrderAs (right1)) and (not contain theSameElementsAs (right2))) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not { contain theSameElementsInOrderAs (right1) } and not { contain theSameElementsAs (right2) }) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when use with 'or'` {
      
      def `should pass when one of contain passes` {
        val left = List("1 ", " 2", "3 ")
        val right1 = List("5 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        left should (contain theSameElementsAs (right1) or contain theSameElementsAs (right2)) 
        left should ((contain theSameElementsAs (right1)) or (contain theSameElementsAs (right2)))
        left should (contain theSameElementsAs (right1) or (contain theSameElementsAs (right2)))
        
        left should (contain theSameElementsAs (right1) or contain theSameElementsInOrderAs (right2))
        left should ((contain theSameElementsAs (right1)) or (contain theSameElementsInOrderAs (right2)))
        left should (contain theSameElementsAs (right1) or (contain theSameElementsInOrderAs (right2)))
        
        left should (contain theSameElementsAs (right1) or contain allOf (" 3", "2 ", " 1"))
        left should ((contain theSameElementsAs (right1)) or (contain allOf (" 3", "2 ", " 1")))
        left should (contain theSameElementsAs (right1) or (contain allOf (" 3", "2 ", " 1")))
        
        left should (contain theSameElementsAs (right1) or contain inOrder (" 1", "2 ", " 3"))
        left should ((contain theSameElementsAs (right1)) or (contain inOrder (" 1", "2 ", " 3")))
        left should (contain theSameElementsAs (right1) or (contain inOrder (" 1", "2 ", " 3")))
        
        left should (contain theSameElementsAs (right1) or contain oneOf (" 1", " 4", "5 "))
        left should ((contain theSameElementsAs (right1)) or (contain oneOf (" 1", " 4", "5 ")))
        left should (contain theSameElementsAs (right1) or (contain oneOf (" 1", " 4", "5 ")))
        
        left should (contain theSameElementsAs (right1) or contain atLeastOneOf (" 1", " 3", "5 "))
        left should ((contain theSameElementsAs (right1)) or (contain atLeastOneOf (" 1", " 3", "5 ")))
        left should (contain theSameElementsAs (right1) or (contain atLeastOneOf (" 1", " 3", "5 ")))
        
        left should (contain theSameElementsAs (right1) or contain only (" 3", " 1", "2 "))
        left should ((contain theSameElementsAs (right1)) or (contain only (" 3", " 1", "2 ")))
        left should (contain theSameElementsAs (right1) or (contain only (" 3", " 1", "2 ")))
        
        left should (contain theSameElementsAs (right1) or contain inOrderOnly (" 1", "2 ", " 3"))
        left should ((contain theSameElementsAs (right1)) or (contain inOrderOnly (" 1", "2 ", " 3")))
        left should (contain theSameElementsAs (right1) or (contain inOrderOnly (" 1", "2 ", " 3")))
        
        left should (contain theSameElementsAs (right1) or contain noneOf ("7 ", " 8", "9 "))
        left should ((contain theSameElementsAs (right1)) or (contain noneOf ("7 ", " 8", "9 ")))
        left should (contain theSameElementsAs (right1) or (contain noneOf ("7 ", " 8", "9 ")))
        
        left should (contain theSameElementsInOrderAs (right2) or contain theSameElementsAs (right1))
        left should ((contain theSameElementsInOrderAs (right2)) or (contain theSameElementsAs (right1)))
        left should (contain theSameElementsInOrderAs (right2) or (contain theSameElementsAs (right1)))
        
        left should (contain allOf (" 3", "2 ", " 1") or contain theSameElementsAs (right1))
        left should ((contain allOf (" 3", "2 ", " 1")) or (contain theSameElementsAs (right1)))
        left should (contain allOf (" 3", "2 ", " 1") or (contain theSameElementsAs (right1)))
        
        left should (contain inOrder (" 1", "2 ", " 3") or contain theSameElementsAs (right1))
        left should ((contain inOrder (" 1", "2 ", " 3")) or (contain theSameElementsAs (right1)))
        left should (contain inOrder (" 1", "2 ", " 3") or (contain theSameElementsAs (right1)))
        
        left should (contain oneOf (" 1", " 4", "5 ") or contain theSameElementsAs (right1))
        left should ((contain oneOf (" 1", " 4", "5 ")) or (contain theSameElementsAs (right1)))
        left should (contain oneOf (" 1", " 4", "5 ") or (contain theSameElementsAs (right1)))
        
        left should (contain atLeastOneOf (" 1", " 3", "5 ") or contain theSameElementsAs (right1))
        left should ((contain atLeastOneOf (" 1", " 3", "5 ")) or (contain theSameElementsAs (right1)))
        left should (contain atLeastOneOf (" 1", " 3", "5 ") or (contain theSameElementsAs (right1)))
        
        left should (contain only (" 3", " 1", "2 ") or contain theSameElementsAs (right1))
        left should ((contain only (" 3", " 1", "2 ")) or (contain theSameElementsAs (right1)))
        left should (contain only (" 3", " 1", "2 ") or (contain theSameElementsAs (right1)))
        
        left should (contain inOrderOnly (" 1", "2 ", " 3") or contain theSameElementsAs (right1))
        left should ((contain inOrderOnly (" 1", "2 ", " 3")) or (contain theSameElementsAs (right1)))
        left should (contain inOrderOnly (" 1", "2 ", " 3") or (contain theSameElementsAs (right1)))
        
        left should (contain noneOf ("7 ", " 8", "9 ") or contain theSameElementsAs (right1))
        left should ((contain noneOf ("7 ", " 8", "9 ")) or (contain theSameElementsAs (right1)))
        left should (contain noneOf ("7 ", " 8", "9 ") or (contain theSameElementsAs (right1)))
      }
      
      def `should failed with correctly stack depth and message when both of contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 3", "8 ", " 1")
        val right2 = List(" 3", "2 ", " 1")
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsAs right1 or contain theSameElementsInOrderAs right2) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " did not contain the same elements as " + decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right2)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsAs right1) or (contain theSameElementsInOrderAs right2))
        }
        e2.message should be (Some(decorateToStringValue(left) + " did not contain the same elements as " + decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right2)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsAs right1 or (contain theSameElementsInOrderAs right2))
        }
        e3.message should be (Some(decorateToStringValue(left) + " did not contain the same elements as " + decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right2)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain and contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        left should (not contain theSameElementsAs (right1) or contain theSameElementsAs (right2)) 
        left should ((not contain theSameElementsAs (right1)) or (contain theSameElementsAs (right2))) 
        left should (not { contain theSameElementsAs (right1) } or contain theSameElementsAs (right2)) 
        
        left should (not contain theSameElementsInOrderAs (right1) or contain theSameElementsAs (right2))
        left should ((not contain theSameElementsInOrderAs (right1)) or (contain theSameElementsAs (right2)))
        left should (not { contain theSameElementsInOrderAs (right1) } or contain theSameElementsAs (right2))
        
        left should (not contain allOf ("8 ", "2 ", " 1") or contain theSameElementsAs (right2))
        left should ((not contain allOf ("8 ", "2 ", " 1")) or (contain theSameElementsAs (right2)))
        left should (not { contain allOf ("8 ", "2 ", " 1") } or contain theSameElementsAs (right2))
        
        left should (not contain inOrder (" 1", "2 ", "8 ") or contain theSameElementsAs (right2))
        left should ((not contain inOrder (" 1", "2 ", "8 ")) or (contain theSameElementsAs (right2)))
        left should (not { contain inOrder (" 1", "2 ", "8 ") } or contain theSameElementsAs (right2))
        
        left should (not contain oneOf (" 6", "8 ", " 5") or contain theSameElementsAs (right2))
        left should ((not contain oneOf (" 6", "8 ", " 5")) or (contain theSameElementsAs (right2)))
        left should (not { contain oneOf (" 6", "8 ", " 5") } or contain theSameElementsAs (right2))
        
        left should (not contain only ("8 ", " 1", "2 ") or contain theSameElementsAs (right2))
        left should ((not contain only ("8 ", " 1", "2 ")) or (contain theSameElementsAs (right2)))
        left should (not { contain only ("8 ", " 1", "2 ") } or contain theSameElementsAs (right2))
        
        left should (not contain inOrderOnly (" 1", "2 ", "8 ") or contain theSameElementsAs (right2))
        left should ((not contain inOrderOnly (" 1", "2 ", "8 ")) or (contain theSameElementsAs (right2)))
        left should (not { contain inOrderOnly (" 1", "2 ", "8 ") } or contain theSameElementsAs (right2))
        
        left should (not contain noneOf (" 1", "2 ", "8 ") or contain theSameElementsAs (right2))
        left should ((not contain noneOf (" 1", "2 ", "8 ")) or (contain theSameElementsAs (right2)))
        left should (not { contain noneOf (" 1", "2 ", "8 ") } or contain theSameElementsAs (right2))
      }
      
      def `should pass when contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", " 3")
        
        left should (contain theSameElementsAs (right2) or not contain theSameElementsAs (right1)) 
        left should ((contain theSameElementsAs (right2)) or (not contain theSameElementsAs (right1))) 
        left should (contain theSameElementsAs (right2) or not { contain theSameElementsAs (right1) }) 
        
        left should (contain theSameElementsAs (right2) or not contain theSameElementsInOrderAs (right1))
        left should ((contain theSameElementsAs (right2)) or (not contain theSameElementsInOrderAs (right1)))
        left should (contain theSameElementsAs (right2) or not { contain theSameElementsInOrderAs (right1) })
        
        left should (contain theSameElementsAs (right2) or not contain allOf ("8 ", "2 ", " 1"))
        left should ((contain theSameElementsAs (right2)) or (not contain allOf ("8 ", "2 ", " 1")))
        left should (contain theSameElementsAs (right2) or not { contain allOf ("8 ", "2 ", " 1") })
        
        left should (contain theSameElementsAs (right2) or not contain inOrder (" 1", "2 ", "8 "))
        left should ((contain theSameElementsAs (right2)) or (not contain inOrder (" 1", "2 ", "8 ")))
        left should (contain theSameElementsAs (right2) or not { contain inOrder (" 1", "2 ", "8 ") })
        
        left should (contain theSameElementsAs (right2) or not contain oneOf (" 6", "8 ", " 5"))
        left should ((contain theSameElementsAs (right2)) or (not contain oneOf (" 6", "8 ", " 5")))
        left should (contain theSameElementsAs (right2) or not { contain oneOf (" 6", "8 ", " 5") })
        
        left should (contain theSameElementsAs (right2) or not contain only ("8 ", " 1", "2 "))
        left should ((contain theSameElementsAs (right2)) or (not contain only ("8 ", " 1", "2 ")))
        left should (contain theSameElementsAs (right2) or not { contain only ("8 ", " 1", "2 ") })
        
        left should (contain theSameElementsAs (right2) or not contain inOrderOnly (" 1", "2 ", "8 "))
        left should ((contain theSameElementsAs (right2)) or (not contain inOrderOnly (" 1", "2 ", "8 ")))
        left should (contain theSameElementsAs (right2) or not { contain inOrderOnly (" 1", "2 ", "8 ") })
        
        left should (contain theSameElementsAs (right2) or not contain noneOf (" 1", "2 ", "8 "))
        left should ((contain theSameElementsAs (right2)) or (not contain noneOf (" 1", "2 ", "8 ")))
        left should (contain theSameElementsAs (right2) or not { contain noneOf (" 1", "2 ", "8 ") })
      }
      
      def `should pass when not contain and not contain passes` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List("8 ", " 1", "2 ")
        val right2 = List(" 1", "2 ", "8 ")
        
        left should (not contain theSameElementsAs (right2) or not contain theSameElementsAs (right1)) 
        left should ((not contain theSameElementsAs (right2)) or (not contain theSameElementsAs (right1))) 
        left should (not { contain theSameElementsAs (right2) } or not { contain theSameElementsAs (right1) }) 
        
        left should (not contain theSameElementsAs (right2) or not contain theSameElementsInOrderAs (right1))
        left should ((not contain theSameElementsAs (right2)) or (not contain theSameElementsInOrderAs (right1)))
        left should (not { contain theSameElementsAs (right2) } or not { contain theSameElementsInOrderAs (right1) })
        
        left should (not contain theSameElementsAs (right2) or not contain allOf ("8 ", "2 ", " 1"))
        left should ((not contain theSameElementsAs (right2)) or (not contain allOf ("8 ", "2 ", " 1")))
        left should (not { contain theSameElementsAs (right2) } or not { contain allOf ("8 ", "2 ", " 1") })
        
        left should (not contain theSameElementsAs (right2) or not contain inOrder (" 1", "2 ", "8 "))
        left should ((not contain theSameElementsAs (right2)) or (not contain inOrder (" 1", "2 ", "8 ")))
        left should (not { contain theSameElementsAs (right2) } or not { contain inOrder (" 1", "2 ", "8 ") })
        
        left should (not contain theSameElementsAs (right2) or not contain oneOf (" 6", "8 ", " 5"))
        left should ((not contain theSameElementsAs (right2)) or (not contain oneOf (" 6", "8 ", " 5")))
        left should (not { contain theSameElementsAs (right2) } or { not contain oneOf (" 6", "8 ", " 5") })
        
        left should (not contain theSameElementsAs (right2) or not contain only ("8 ", " 1", "2 "))
        left should ((not contain theSameElementsAs (right2)) or (not contain only ("8 ", " 1", "2 ")))
        left should (not { contain theSameElementsAs (right2) } or not { contain only ("8 ", " 1", "2 ")})
        
        left should (not contain theSameElementsAs (right2) or not contain inOrderOnly (" 1", "2 ", "8 "))
        left should ((not contain theSameElementsAs (right2)) or (not contain inOrderOnly (" 1", "2 ", "8 ")))
        left should (not { contain theSameElementsAs (right2) } or not { contain inOrderOnly (" 1", "2 ", "8 ") })
        
        left should (not contain theSameElementsAs (right2) or not contain noneOf (" 1", "2 ", "8 "))
        left should ((not contain theSameElementsAs (right2)) or (not contain noneOf (" 1", "2 ", "8 ")))
        left should (not { contain theSameElementsAs (right2) } or not { contain noneOf (" 1", "2 ", "8 ") })
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 8", "2 ", " 1")
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain theSameElementsInOrderAs (right1) or contain theSameElementsAs right2) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " did not contain the same elements as " + decorateToStringValue(right2)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain theSameElementsInOrderAs (right1)) or (contain theSameElementsAs right2)) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as "+ decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " did not contain the same elements as " + decorateToStringValue(right2)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not { contain theSameElementsInOrderAs (right1) } or contain theSameElementsAs right2) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as "+ decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " did not contain the same elements as " + decorateToStringValue(right2)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first contain failed and second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right = List(" 3", "2 ", " 1")
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsInOrderAs (right) or not contain theSameElementsAs (right)) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right) + ", and " + decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsInOrderAs (right)) or (not contain theSameElementsAs (right))) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right) + ", and " + decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain theSameElementsInOrderAs (right) or not { contain theSameElementsAs (right) }) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right) + ", and " + decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second not contain failed` {
        
        val left = List("1 ", " 2", "3 ")
        val right1 = List(" 1", "2 ", " 3")
        val right2 = List(" 3", "2 ", " 1")
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain theSameElementsInOrderAs (right1) or not contain theSameElementsAs (right2)) 
        }
        e1.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right2)))
        e1.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain theSameElementsInOrderAs (right1)) or (not contain theSameElementsAs (right2))) 
        }
        e2.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right2)))
        e2.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain theSameElementsInOrderAs (right1) or not { contain theSameElementsAs (right2) }) 
        }
        e3.message should be (Some(decorateToStringValue(left) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right1) + ", and " + decorateToStringValue(left) + " contained the same elements as " + decorateToStringValue(right2)))
        e3.failedCodeFileName should be (Some("ContainMatcherAndOrEqualitySpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
  }
  
}
