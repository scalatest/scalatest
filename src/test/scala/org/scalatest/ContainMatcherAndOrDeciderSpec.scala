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
import org.scalautils.StringNormalizations

class ContainMatcherAndOrDeciderSpec extends Spec with Matchers with Explicitly with StringNormalizations with SharedHelpers {

  val equality = new Equality[String] {
    def areEqual(left: String, right: Any) = 
      left.toLowerCase == (right match {
        case s: String => s.toLowerCase
        case other => other
      })
  }
  
  object `ContainMatcher ` {
    
    object `when use with 'and'` {
      
      def `should pass when both contain passes` {
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" three", " one", "two ")
        val right2 = List(" one", "two ", " three")
        
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain theSameIteratedElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain allOf (" three", "two ", " one")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain inOrder (" one", "two ", " three")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain oneOf (" one", " three", "five ")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain only (" three", " one", "two "))(decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain inOrderOnly (" one", "two ", " three")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) and (contain noneOf (" seven", "eight ", " nine")) (decided by equality afterBeing trimmed))
        left should ((contain theSameIteratedElementsAs (right2)) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain allOf (" three", " one", "two ")) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain inOrder (" one", "two ", " three")) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain oneOf (" one", " three", "five ")) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain only (" three", " one", "two ")) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain inOrderOnly (" one", "two ", " three")) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain noneOf (" seven", "eight ", " nine")) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        
      }
      
      def `should failed with correctly stack depth and message when first contain failed but second contain passed` {
        val left = List("ONE ", " TWO", "THREE ")
        val right = List(" three", "two ", " one")
        
        val e = intercept[exceptions.TestFailedException] {
          left should ((contain theSameIteratedElementsAs right) (decided by equality afterBeing trimmed) and (contain theSameElementsAs right) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " did not contain the same iterated elements as " + right))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first contain passed but second contain failed` {
        val left = List("ONE ", " TWO", "THREE ")
        val right = List(" three", "two ", " one")
        
        val e = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsAs right) (decided by equality afterBeing trimmed) and (contain theSameIteratedElementsAs right) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " contained the same elements as " + right + ", but " + left + " did not contain the same iterated elements as " + right))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should pass when not contain and contain passes` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List("eight ", " one", "two ")
        val right2 = List(" one", "two ", " three")
        
        left should (not contain theSameElementsAs (right1) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain allOf ("eight ", "two ", " one") (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain inOrder (" one", "two ", "eight ") (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain oneOf (" six", "eight ", " five") (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain only ("eight ", " one", "two ") (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain inOrderOnly (" one", "two ", "eight ") (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain inOrderOnly (" one", "two ", "eight ") (decided by equality afterBeing trimmed) and (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
      }
      
      def `should pass when contain and not contain passes` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" eight", "one ", " two")
        val right2 = List(" one", "two ", " three")
        
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain theSameElementsAs (right1) (decided by equality afterBeing trimmed))        
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain allOf (" eight", " two", "one ") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain inOrder ("one ", " two", " eight") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain oneOf ("six ", " eight", "five ") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain only (" eight", "one ", " two") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain inOrderOnly ("one ", " two", " eight") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) and not contain noneOf ("one ", " two", " eight") (decided by equality afterBeing trimmed))
      }
      
      def `should pass when not contain and not contain passes` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" eight", "one ", " two")
        val right2 = List("one ", " two", " eight")
        
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain theSameElementsAs (right1) (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain allOf (" eight", " two", "one ") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain inOrder ("one ", " two", " eight") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain oneOf ("six ", " eight", "five ") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain only (" eight", "one ", " two") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain inOrderOnly ("one ", " two", " eight") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) and not contain noneOf ("one ", " two", " eight") (decided by equality afterBeing trimmed))
      }
      
      def `should failed with correctly stack depth and message when first not contain failed but second contain passed` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" one", "two ", " three")
        val right2 = List(" three", "two ", " one")
        
        val e = intercept[exceptions.TestFailedException] {
          left should (not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed) and (contain theSameElementsAs right2) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " contained the same iterated elements as " + right1))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first contain passed but second not contain failed` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" three", "two ", " one")
        val right2 = List(" one", "two ", " three")
        
        val e = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsAs right1) (decided by equality afterBeing trimmed) and not contain theSameIteratedElementsAs (right2) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " contained the same elements as " + right1 + ", but " + left + " contained the same iterated elements as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second not contain failed` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" one", "two ", " three")
        val right2 = List(" three", "two ", " one")        
        
        val e = intercept[exceptions.TestFailedException] {
          left should (not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed) and not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " contained the same iterated elements as " + right1))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
    }
    
    object `when use with 'or'` {
      
      def `should pass when one of contain passes` {
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List("five ", " one", "two ")
        val right2 = List(" one", "two ", " three")
        
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain theSameIteratedElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain allOf (" three", "two ", " one")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain inOrder (" one", "two ", " three")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain oneOf (" one", " three", "five ")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain only (" three", " one", "two ")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain inOrderOnly (" one", "two ", " three")) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed) or (contain noneOf ("seven ", " eight", "nine ")) (decided by equality afterBeing trimmed))
        left should ((contain theSameIteratedElementsAs (right2)) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain allOf (" three", "two ", " one")) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain inOrder (" one", "two ", " three")) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain oneOf (" one", " three", "five ")) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain only (" three", " one", "two ")) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain inOrderOnly (" one", "two ", " three")) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        left should ((contain noneOf ("seven ", " eight", "nine ")) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right1)) (decided by equality afterBeing trimmed))
        
      }
      
      def `should failed with correctly stack depth and message when both of contain failed` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" three", "eight ", " one")
        val right2 = List(" three", "two ", " one")
        
        val e = intercept[exceptions.TestFailedException] {
          left should ((contain theSameElementsAs right1) (decided by equality afterBeing trimmed) or (contain theSameIteratedElementsAs right2) (decided by equality afterBeing trimmed))
        }
        e.message should be (Some(left + " did not contain the same elements as " + right1 + ", and " + left + " did not contain the same iterated elements as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should pass when not contain and contain passes` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List("eight ", " one", "two ")
        val right2 = List(" one", "two ", " three")
        
        left should (not contain theSameElementsAs (right1) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed)) 
        left should (not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain allOf ("eight ", "two ", " one") (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain inOrder (" one", "two ", "eight ") (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain oneOf (" six", "eight ", " five") (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain only ("eight ", " one", "two ") (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain inOrderOnly (" one", "two ", "eight ") (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
        left should (not contain noneOf (" one", "two ", "eight ") (decided by equality afterBeing trimmed) or (contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed))
      }
      
      def `should pass when contain and not contain passes` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List("eight ", " one", "two ")
        val right2 = List(" one", "two ", " three")
        
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain theSameElementsAs (right1) (decided by equality afterBeing trimmed)) 
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain allOf ("eight ", "two ", " one") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain inOrder (" one", "two ", "eight ") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain oneOf (" six", "eight ", " five") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain only ("eight ", " one", "two ") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain inOrderOnly (" one", "two ", "eight ") (decided by equality afterBeing trimmed))
        left should ((contain theSameElementsAs (right2)) (decided by equality afterBeing trimmed) or not contain noneOf (" one", "two ", "eight ") (decided by equality afterBeing trimmed))
        
      }
      
      def `should pass when not contain and not contain passes` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List("eight ", " one", "two ")
        val right2 = List(" one", "two ", "eight ")
        
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain theSameElementsAs (right1) (decided by equality afterBeing trimmed)) 
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain allOf ("eight ", "two ", " one") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain inOrder (" one", "two ", "eight ") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain oneOf (" six", "eight ", " five") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain only ("eight ", " one", "two ") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain inOrderOnly (" one", "two ", "eight ") (decided by equality afterBeing trimmed))
        left should (not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed) or not contain noneOf (" one", "two ", "eight ") (decided by equality afterBeing trimmed))
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second contain failed` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" one", "two ", " three")
        val right2 = List(" eight", "two ", " one")
        
        val e = intercept[exceptions.TestFailedException] {
          left should (not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed) or (contain theSameElementsAs right2) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " contained the same iterated elements as " + right1 + ", and " + left + " did not contain the same elements as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first contain failed and second not contain failed` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right = List(" three", "two ", " one")
        
        val e = intercept[exceptions.TestFailedException] {
          left should ((contain theSameIteratedElementsAs (right)) (decided by equality afterBeing trimmed) or not contain theSameElementsAs (right) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " did not contain the same iterated elements as " + right + ", and " + left + " contained the same elements as " + right))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
      def `should failed with correctly stack depth and message when first not contain failed and second not contain failed` {
        
        val left = List("ONE ", " TWO", "THREE ")
        val right1 = List(" one", "two ", " three")
        val right2 = List(" three", "two ", " one")
        
        val e = intercept[exceptions.TestFailedException] {
          left should (not contain theSameIteratedElementsAs (right1) (decided by equality afterBeing trimmed) or not contain theSameElementsAs (right2) (decided by equality afterBeing trimmed)) 
        }
        e.message should be (Some(left + " contained the same iterated elements as " + right1 + ", and " + left + " contained the same elements as " + right2))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrDeciderSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
      }
      
    }
    
  }
  
}
