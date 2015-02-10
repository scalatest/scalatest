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
import enablers.Messaging
import exceptions.TestFailedException

class ShouldMessageSpec extends Spec with Matchers {
  
  def hadMessageInsteadOfExpectedMessage(left: Any, actualMessage: String, expectedMessage: String): String = 
    FailureMessages("hadMessageInsteadOfExpectedMessage", left, actualMessage, expectedMessage)
    
  def hadMessage(left: Any, expectedMessage: String): String = 
    FailureMessages("hadExpectedMessage", left, expectedMessage)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String = 
    FailureMessages("didNotEqual", left, right)
    
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasNotEqualTo", left, right)
  
  object `The 'have message (xxx)' syntax` {
    
    object `on Throwable` {
      
      val t = new RuntimeException("We have an error!")
      val t2 = new RuntimeException("This is another error!")
      
      def `should do nothing if message matches the throwable's message` {
        t should have message "We have an error!"
      }
      
      def `should throw TFE with correct stack depth if message does not match the throwable's message` {
        val e =intercept[TestFailedException] { 
          t should have message "We have a boom!"
        }
        e.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if message does not match the throwable's message and used with should not` {
        t should not { have message "We have a boom!" }
        t should not have message ("We have a boom!")
      }
      
      def `should throw TFE with correct stack depth if message matches throwable's message and used with should not` {
        val e1 = intercept[TestFailedException] {
          t should not { have message "We have an error!" }
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should not have message ("We have an error!")
        }
        e2.message should be (Some(hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression` {
        t should (have message ("We have an error!") and (equal (t)))
        t should (equal (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and equal (t))
        t should (equal (t) and have message ("We have an error!"))
        
        t should (have message ("We have an error!") and (be (t)))
        t should (be (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and be (t))
        t should (be (t) and have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-and expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (equal (t)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (have message ("We have a boom!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and equal (t))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t) and have message ("We have a boom!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (be (t)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and be (t))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and have message ("We have a boom!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-and expression and not` {
        t should (not have message ("We have a boom!") and (equal (t)))
        t should (equal (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and equal (t))
        t should (equal (t) and not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") and (be (t)))
        t should (be (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and be (t))
        t should (be (t) and not have message ("We have a boom!"))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression and not` {
        
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (equal (t)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (not have message ("We have an error!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and equal (t))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e4 = intercept[TestFailedException] {
          t should (equal (t) and not have message ("We have an error!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (be (t)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and be (t))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and not have message ("We have an error!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-or expression` {
        t should (have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t))
        t should (equal (t) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t)))
        t should (be (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t))
        t should (be (t) or have message ("We have an error!"))
        
        t should (have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or equal (t))
        t should (equal (t) or have message ("We have a boom!"))
        
        t should (have message ("We have a boom!") or (be (t)))
        t should (be (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or be (t))
        t should (be (t) or have message ("We have a boom!"))
        
        t should (have message ("We have an error!") or (equal (t2)))
        t should (equal (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t2))
        t should (equal (t2) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t2)))
        t should (be (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t2))
        t should (be (t2) or have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-or expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (have message ("We have a boom!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or equal (t2))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or have message ("We have a boom!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (be (t2)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or be (t2))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or have message ("We have a boom!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-or expression and not` {
        t should (not have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t))
        t should (equal (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t)))
        t should (be (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t))
        t should (be (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or equal (t))
        t should (equal (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have an error!") or (be (t)))
        t should (be (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or be (t))
        t should (be (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have a boom!") or (equal (t2)))
        t should (equal (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t2))
        t should (equal (t2) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t2)))
        t should (be (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t2))
        t should (be (t2) or not have message ("We have a boom!"))
      }
      
      def `should throw TFE with correct stack depth if error message matches and used in a logical-or expression and not` {
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (not have message ("We have an error!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or equal (t2))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or not have message ("We have an error!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (be (t2)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or be (t2))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or not have message ("We have an error!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `on an arbitrary object that has an empty-paren String message method` {
      
      class Messenger(theMessage: String) {
        def message(): String = theMessage
        override def toString = "messenger"
      }
      
      val t = new Messenger("We have an error!")
      val t2 = new Messenger("This is another error!")
     
      def `should do nothing if message matches the throwable's message` {
        t should have message "We have an error!"
      }
      
      def `should throw TFE with correct stack depth if message does not match the throwable's message` {
        val e =intercept[TestFailedException] { 
          t should have message "We have a boom!"
        }
        e.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if message does not match the throwable's message and used with should not` {
        t should not { have message "We have a boom!" }
        t should not have message ("We have a boom!")
      }
      
      def `should throw TFE with correct stack depth if message matches throwable's message and used with should not` {
        val e1 = intercept[TestFailedException] {
          t should not { have message "We have an error!" }
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should not have message ("We have an error!")
        }
        e2.message should be (Some(hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression` {
        t should (have message ("We have an error!") and (equal (t)))
        t should (equal (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and equal (t))
        t should (equal (t) and have message ("We have an error!"))
        
        t should (have message ("We have an error!") and (be (t)))
        t should (be (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and be (t))
        t should (be (t) and have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-and expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (equal (t)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (have message ("We have a boom!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and equal (t))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t) and have message ("We have a boom!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (be (t)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and be (t))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and have message ("We have a boom!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-and expression and not` {
        t should (not have message ("We have a boom!") and (equal (t)))
        t should (equal (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and equal (t))
        t should (equal (t) and not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") and (be (t)))
        t should (be (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and be (t))
        t should (be (t) and not have message ("We have a boom!"))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression and not` {
        
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (equal (t)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (not have message ("We have an error!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and equal (t))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e4 = intercept[TestFailedException] {
          t should (equal (t) and not have message ("We have an error!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (be (t)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and be (t))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and not have message ("We have an error!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-or expression` {
        t should (have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t))
        t should (equal (t) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t)))
        t should (be (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t))
        t should (be (t) or have message ("We have an error!"))
        
        t should (have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or equal (t))
        t should (equal (t) or have message ("We have a boom!"))
        
        t should (have message ("We have a boom!") or (be (t)))
        t should (be (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or be (t))
        t should (be (t) or have message ("We have a boom!"))
        
        t should (have message ("We have an error!") or (equal (t2)))
        t should (equal (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t2))
        t should (equal (t2) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t2)))
        t should (be (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t2))
        t should (be (t2) or have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-or expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (have message ("We have a boom!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or equal (t2))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or have message ("We have a boom!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (be (t2)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or be (t2))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or have message ("We have a boom!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-or expression and not` {
        t should (not have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t))
        t should (equal (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t)))
        t should (be (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t))
        t should (be (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or equal (t))
        t should (equal (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have an error!") or (be (t)))
        t should (be (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or be (t))
        t should (be (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have a boom!") or (equal (t2)))
        t should (equal (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t2))
        t should (equal (t2) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t2)))
        t should (be (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t2))
        t should (be (t2) or not have message ("We have a boom!"))
      }
      
      def `should throw TFE with correct stack depth if error message matches and used in a logical-or expression and not` {
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (not have message ("We have an error!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or equal (t2))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or not have message ("We have an error!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (be (t2)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or be (t2))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or not have message ("We have an error!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `on an arbitrary object that has an parameterless String message method` {
      
      class Messenger(theMessage: String) {
        def message: String = theMessage
        override def toString = "messenger"
      }
      
      val t = new Messenger("We have an error!")
      val t2 = new Messenger("This is another error!")
     
      def `should do nothing if message matches the throwable's message` {
        t should have message "We have an error!"
      }
      
      def `should throw TFE with correct stack depth if message does not match the throwable's message` {
        val e =intercept[TestFailedException] { 
          t should have message "We have a boom!"
        }
        e.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if message does not match the throwable's message and used with should not` {
        t should not { have message "We have a boom!" }
        t should not have message ("We have a boom!")
      }
      
      def `should throw TFE with correct stack depth if message matches throwable's message and used with should not` {
        val e1 = intercept[TestFailedException] {
          t should not { have message "We have an error!" }
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should not have message ("We have an error!")
        }
        e2.message should be (Some(hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression` {
        t should (have message ("We have an error!") and (equal (t)))
        t should (equal (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and equal (t))
        t should (equal (t) and have message ("We have an error!"))
        
        t should (have message ("We have an error!") and (be (t)))
        t should (be (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and be (t))
        t should (be (t) and have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-and expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (equal (t)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (have message ("We have a boom!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and equal (t))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t) and have message ("We have a boom!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (be (t)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and be (t))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and have message ("We have a boom!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-and expression and not` {
        t should (not have message ("We have a boom!") and (equal (t)))
        t should (equal (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and equal (t))
        t should (equal (t) and not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") and (be (t)))
        t should (be (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and be (t))
        t should (be (t) and not have message ("We have a boom!"))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression and not` {
        
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (equal (t)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (not have message ("We have an error!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and equal (t))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e4 = intercept[TestFailedException] {
          t should (equal (t) and not have message ("We have an error!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (be (t)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and be (t))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and not have message ("We have an error!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-or expression` {
        t should (have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t))
        t should (equal (t) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t)))
        t should (be (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t))
        t should (be (t) or have message ("We have an error!"))
        
        t should (have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or equal (t))
        t should (equal (t) or have message ("We have a boom!"))
        
        t should (have message ("We have a boom!") or (be (t)))
        t should (be (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or be (t))
        t should (be (t) or have message ("We have a boom!"))
        
        t should (have message ("We have an error!") or (equal (t2)))
        t should (equal (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t2))
        t should (equal (t2) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t2)))
        t should (be (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t2))
        t should (be (t2) or have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-or expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (have message ("We have a boom!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or equal (t2))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or have message ("We have a boom!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (be (t2)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or be (t2))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or have message ("We have a boom!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-or expression and not` {
        t should (not have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t))
        t should (equal (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t)))
        t should (be (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t))
        t should (be (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or equal (t))
        t should (equal (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have an error!") or (be (t)))
        t should (be (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or be (t))
        t should (be (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have a boom!") or (equal (t2)))
        t should (equal (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t2))
        t should (equal (t2) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t2)))
        t should (be (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t2))
        t should (be (t2) or not have message ("We have a boom!"))
      }
      
      def `should throw TFE with correct stack depth if error message matches and used in a logical-or expression and not` {
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (not have message ("We have an error!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or equal (t2))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or not have message ("We have an error!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (be (t2)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or be (t2))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or not have message ("We have an error!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `on an arbitrary object that has an parameterless String message val` {
      
      class Messenger(theMessage: String) {
        val message: String = theMessage
        override def toString = "messenger"
      }
      
      val t = new Messenger("We have an error!")
      val t2 = new Messenger("This is another error!")
     
      def `should do nothing if message matches the throwable's message` {
        t should have message "We have an error!"
      }
      
      def `should throw TFE with correct stack depth if message does not match the throwable's message` {
        val e =intercept[TestFailedException] { 
          t should have message "We have a boom!"
        }
        e.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if message does not match the throwable's message and used with should not` {
        t should not { have message "We have a boom!" }
        t should not have message ("We have a boom!")
      }
      
      def `should throw TFE with correct stack depth if message matches throwable's message and used with should not` {
        val e1 = intercept[TestFailedException] {
          t should not { have message "We have an error!" }
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should not have message ("We have an error!")
        }
        e2.message should be (Some(hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression` {
        t should (have message ("We have an error!") and (equal (t)))
        t should (equal (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and equal (t))
        t should (equal (t) and have message ("We have an error!"))
        
        t should (have message ("We have an error!") and (be (t)))
        t should (be (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and be (t))
        t should (be (t) and have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-and expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (equal (t)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (have message ("We have a boom!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and equal (t))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t) and have message ("We have a boom!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (be (t)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and be (t))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and have message ("We have a boom!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-and expression and not` {
        t should (not have message ("We have a boom!") and (equal (t)))
        t should (equal (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and equal (t))
        t should (equal (t) and not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") and (be (t)))
        t should (be (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and be (t))
        t should (be (t) and not have message ("We have a boom!"))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression and not` {
        
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (equal (t)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (not have message ("We have an error!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and equal (t))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e4 = intercept[TestFailedException] {
          t should (equal (t) and not have message ("We have an error!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (be (t)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and be (t))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and not have message ("We have an error!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-or expression` {
        t should (have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t))
        t should (equal (t) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t)))
        t should (be (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t))
        t should (be (t) or have message ("We have an error!"))
        
        t should (have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or equal (t))
        t should (equal (t) or have message ("We have a boom!"))
        
        t should (have message ("We have a boom!") or (be (t)))
        t should (be (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or be (t))
        t should (be (t) or have message ("We have a boom!"))
        
        t should (have message ("We have an error!") or (equal (t2)))
        t should (equal (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t2))
        t should (equal (t2) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t2)))
        t should (be (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t2))
        t should (be (t2) or have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-or expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (have message ("We have a boom!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or equal (t2))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or have message ("We have a boom!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (be (t2)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or be (t2))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or have message ("We have a boom!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-or expression and not` {
        t should (not have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t))
        t should (equal (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t)))
        t should (be (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t))
        t should (be (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or equal (t))
        t should (equal (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have an error!") or (be (t)))
        t should (be (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or be (t))
        t should (be (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have a boom!") or (equal (t2)))
        t should (equal (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t2))
        t should (equal (t2) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t2)))
        t should (be (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t2))
        t should (be (t2) or not have message ("We have a boom!"))
      }
      
      def `should throw TFE with correct stack depth if error message matches and used in a logical-or expression and not` {
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (not have message ("We have an error!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or equal (t2))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or not have message ("We have an error!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (be (t2)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or be (t2))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or not have message ("We have an error!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `on an arbitrary object that has an empty-paren String getMessage method` {
      
      class Messenger(theMessage: String) {
        def getMessage(): String = theMessage
        override def toString = "messenger"
      }
      
      val t = new Messenger("We have an error!")
      val t2 = new Messenger("This is another error!")
     
      def `should do nothing if message matches the throwable's message` {
        t should have message "We have an error!"
      }
      
      def `should throw TFE with correct stack depth if message does not match the throwable's message` {
        val e =intercept[TestFailedException] { 
          t should have message "We have a boom!"
        }
        e.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if message does not match the throwable's message and used with should not` {
        t should not { have message "We have a boom!" }
        t should not have message ("We have a boom!")
      }
      
      def `should throw TFE with correct stack depth if message matches throwable's message and used with should not` {
        val e1 = intercept[TestFailedException] {
          t should not { have message "We have an error!" }
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should not have message ("We have an error!")
        }
        e2.message should be (Some(hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression` {
        t should (have message ("We have an error!") and (equal (t)))
        t should (equal (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and equal (t))
        t should (equal (t) and have message ("We have an error!"))
        
        t should (have message ("We have an error!") and (be (t)))
        t should (be (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and be (t))
        t should (be (t) and have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-and expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (equal (t)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (have message ("We have a boom!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and equal (t))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t) and have message ("We have a boom!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (be (t)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and be (t))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and have message ("We have a boom!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-and expression and not` {
        t should (not have message ("We have a boom!") and (equal (t)))
        t should (equal (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and equal (t))
        t should (equal (t) and not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") and (be (t)))
        t should (be (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and be (t))
        t should (be (t) and not have message ("We have a boom!"))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression and not` {
        
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (equal (t)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (not have message ("We have an error!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and equal (t))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e4 = intercept[TestFailedException] {
          t should (equal (t) and not have message ("We have an error!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (be (t)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and be (t))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and not have message ("We have an error!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-or expression` {
        t should (have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t))
        t should (equal (t) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t)))
        t should (be (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t))
        t should (be (t) or have message ("We have an error!"))
        
        t should (have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or equal (t))
        t should (equal (t) or have message ("We have a boom!"))
        
        t should (have message ("We have a boom!") or (be (t)))
        t should (be (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or be (t))
        t should (be (t) or have message ("We have a boom!"))
        
        t should (have message ("We have an error!") or (equal (t2)))
        t should (equal (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t2))
        t should (equal (t2) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t2)))
        t should (be (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t2))
        t should (be (t2) or have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-or expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (have message ("We have a boom!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or equal (t2))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or have message ("We have a boom!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (be (t2)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or be (t2))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or have message ("We have a boom!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-or expression and not` {
        t should (not have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t))
        t should (equal (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t)))
        t should (be (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t))
        t should (be (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or equal (t))
        t should (equal (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have an error!") or (be (t)))
        t should (be (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or be (t))
        t should (be (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have a boom!") or (equal (t2)))
        t should (equal (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t2))
        t should (equal (t2) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t2)))
        t should (be (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t2))
        t should (be (t2) or not have message ("We have a boom!"))
      }
      
      def `should throw TFE with correct stack depth if error message matches and used in a logical-or expression and not` {
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (not have message ("We have an error!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or equal (t2))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or not have message ("We have an error!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (be (t2)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or be (t2))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or not have message ("We have an error!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `on an arbitrary object that has an parameterless String getMessage method` {
      
      class Messenger(theMessage: String) {
        def getMessage: String = theMessage
        override def toString = "messenger"
      }
      
      val t = new Messenger("We have an error!")
      val t2 = new Messenger("This is another error!")
     
      def `should do nothing if message matches the throwable's message` {
        t should have message "We have an error!"
      }
      
      def `should throw TFE with correct stack depth if message does not match the throwable's message` {
        val e =intercept[TestFailedException] { 
          t should have message "We have a boom!"
        }
        e.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if message does not match the throwable's message and used with should not` {
        t should not { have message "We have a boom!" }
        t should not have message ("We have a boom!")
      }
      
      def `should throw TFE with correct stack depth if message matches throwable's message and used with should not` {
        val e1 = intercept[TestFailedException] {
          t should not { have message "We have an error!" }
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should not have message ("We have an error!")
        }
        e2.message should be (Some(hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression` {
        t should (have message ("We have an error!") and (equal (t)))
        t should (equal (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and equal (t))
        t should (equal (t) and have message ("We have an error!"))
        
        t should (have message ("We have an error!") and (be (t)))
        t should (be (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and be (t))
        t should (be (t) and have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-and expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (equal (t)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (have message ("We have a boom!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and equal (t))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t) and have message ("We have a boom!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (be (t)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and be (t))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and have message ("We have a boom!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-and expression and not` {
        t should (not have message ("We have a boom!") and (equal (t)))
        t should (equal (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and equal (t))
        t should (equal (t) and not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") and (be (t)))
        t should (be (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and be (t))
        t should (be (t) and not have message ("We have a boom!"))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression and not` {
        
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (equal (t)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (not have message ("We have an error!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and equal (t))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e4 = intercept[TestFailedException] {
          t should (equal (t) and not have message ("We have an error!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (be (t)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and be (t))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and not have message ("We have an error!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-or expression` {
        t should (have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t))
        t should (equal (t) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t)))
        t should (be (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t))
        t should (be (t) or have message ("We have an error!"))
        
        t should (have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or equal (t))
        t should (equal (t) or have message ("We have a boom!"))
        
        t should (have message ("We have a boom!") or (be (t)))
        t should (be (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or be (t))
        t should (be (t) or have message ("We have a boom!"))
        
        t should (have message ("We have an error!") or (equal (t2)))
        t should (equal (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t2))
        t should (equal (t2) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t2)))
        t should (be (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t2))
        t should (be (t2) or have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-or expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (have message ("We have a boom!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or equal (t2))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or have message ("We have a boom!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (be (t2)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or be (t2))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or have message ("We have a boom!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-or expression and not` {
        t should (not have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t))
        t should (equal (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t)))
        t should (be (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t))
        t should (be (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or equal (t))
        t should (equal (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have an error!") or (be (t)))
        t should (be (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or be (t))
        t should (be (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have a boom!") or (equal (t2)))
        t should (equal (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t2))
        t should (equal (t2) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t2)))
        t should (be (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t2))
        t should (be (t2) or not have message ("We have a boom!"))
      }
      
      def `should throw TFE with correct stack depth if error message matches and used in a logical-or expression and not` {
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (not have message ("We have an error!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or equal (t2))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or not have message ("We have an error!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (be (t2)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or be (t2))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or not have message ("We have an error!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `on an arbitrary object that has an parameterless String getMessage val` {
      
      class Messenger(theMessage: String) {
        val getMessage: String = theMessage
        override def toString = "messenger"
      }
      
      val t = new Messenger("We have an error!")
      val t2 = new Messenger("This is another error!")
     
      def `should do nothing if message matches the throwable's message` {
        t should have message "We have an error!"
      }
      
      def `should throw TFE with correct stack depth if message does not match the throwable's message` {
        val e =intercept[TestFailedException] { 
          t should have message "We have a boom!"
        }
        e.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if message does not match the throwable's message and used with should not` {
        t should not { have message "We have a boom!" }
        t should not have message ("We have a boom!")
      }
      
      def `should throw TFE with correct stack depth if message matches throwable's message and used with should not` {
        val e1 = intercept[TestFailedException] {
          t should not { have message "We have an error!" }
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should not have message ("We have an error!")
        }
        e2.message should be (Some(hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression` {
        t should (have message ("We have an error!") and (equal (t)))
        t should (equal (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and equal (t))
        t should (equal (t) and have message ("We have an error!"))
        
        t should (have message ("We have an error!") and (be (t)))
        t should (be (t) and (have message ("We have an error!")))
        
        t should (have message ("We have an error!") and be (t))
        t should (be (t) and have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-and expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (equal (t)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (have message ("We have a boom!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and equal (t))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t) and have message ("We have a boom!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and (be (t)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") and be (t))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and have message ("We have a boom!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-and expression and not` {
        t should (not have message ("We have a boom!") and (equal (t)))
        t should (equal (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and equal (t))
        t should (equal (t) and not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") and (be (t)))
        t should (be (t) and (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") and be (t))
        t should (be (t) and not have message ("We have a boom!"))
      }
      
      def `should do nothing if error message matches and used in a logical-and expression and not` {
        
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (equal (t)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!")))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t) and (not have message ("We have an error!")))
        }
        e2.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and equal (t))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!")))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e4 = intercept[TestFailedException] {
          t should (equal (t) and not have message ("We have an error!"))
        }
        e4.message should be (Some(equaled(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and (be (t)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!")))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t) and (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") and be (t))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!")))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e8 = intercept[TestFailedException] {
          t should (be (t) and not have message ("We have an error!"))
        }
        e8.message should be (Some(wasEqualTo(t, t) + ", but " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message matches and used in a logical-or expression` {
        t should (have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t))
        t should (equal (t) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t)))
        t should (be (t) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t))
        t should (be (t) or have message ("We have an error!"))
        
        t should (have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or equal (t))
        t should (equal (t) or have message ("We have a boom!"))
        
        t should (have message ("We have a boom!") or (be (t)))
        t should (be (t) or (have message ("We have a boom!")))
        
        t should (have message ("We have a boom!") or be (t))
        t should (be (t) or have message ("We have a boom!"))
        
        t should (have message ("We have an error!") or (equal (t2)))
        t should (equal (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or equal (t2))
        t should (equal (t2) or have message ("We have an error!"))
        
        t should (have message ("We have an error!") or (be (t2)))
        t should (be (t2) or (have message ("We have an error!")))
        
        t should (have message ("We have an error!") or be (t2))
        t should (be (t2) or have message ("We have an error!"))
      }
      
      def `should throw TFE with correct stack depth if error message does not match and used in a logical-or expression` {
        val e1 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (have message ("We have a boom!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or equal (t2))
        }
        e3.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or have message ("We have a boom!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or (be (t2)))
        }
        e5.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (have message ("We have a boom!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (have message ("We have a boom!") or be (t2))
        }
        e7.message should be (Some(hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or have message ("We have a boom!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessageInsteadOfExpectedMessage(t, "We have an error!", "We have a boom!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should do nothing if error message does not match and used in a logical-or expression and not` {
        t should (not have message ("We have a boom!") or (equal (t)))
        t should (equal (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t))
        t should (equal (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t)))
        t should (be (t) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t))
        t should (be (t) or not have message ("We have a boom!"))
        
        t should (not have message ("We have an error!") or (equal (t)))
        t should (equal (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or equal (t))
        t should (equal (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have an error!") or (be (t)))
        t should (be (t) or (not have message ("We have an error!")))
        
        t should (not have message ("We have an error!") or be (t))
        t should (be (t) or not have message ("We have an error!"))
        
        t should (not have message ("We have a boom!") or (equal (t2)))
        t should (equal (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or equal (t2))
        t should (equal (t2) or not have message ("We have a boom!"))
        
        t should (not have message ("We have a boom!") or (be (t2)))
        t should (be (t2) or (not have message ("We have a boom!")))
        
        t should (not have message ("We have a boom!") or be (t2))
        t should (be (t2) or not have message ("We have a boom!"))
      }
      
      def `should throw TFE with correct stack depth if error message matches and used in a logical-or expression and not` {
        val e1 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (equal (t2)))
        }
        e1.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e1.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[TestFailedException] {
          t should (equal (t2) or (not have message ("We have an error!")))
        }
        e2.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e2.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or equal (t2))
        }
        e3.message should be (Some(hadMessage(t, "We have an error!") + ", and " + didNotEqual(t, t2)))
        e3.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e4 = intercept[TestFailedException] {
          t should (equal (t2) or not have message ("We have an error!"))
        }
        e4.message should be (Some(didNotEqual(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e4.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e5 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or (be (t2)))
        }
        e5.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e5.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e6 = intercept[TestFailedException] {
          t should (be (t2) or (not have message ("We have an error!")))
        }
        e6.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e6.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e7 = intercept[TestFailedException] {
          t should (not have message ("We have an error!") or be (t2))
        }
        e7.message should be (Some(hadMessage(t, "We have an error!") + ", and " + wasNotEqualTo(t, t2)))
        e7.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

        val e8 = intercept[TestFailedException] {
          t should (be (t2) or not have message ("We have an error!"))
        }
        e8.message should be (Some(wasNotEqualTo(t, t2) + ", and " + hadMessage(t, "We have an error!")))
        e8.failedCodeFileName should be (Some("ShouldMessageSpec.scala"))
        e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
  }
  
}
