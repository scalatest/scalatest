package org.scalatest

import matchers.AMatcher
import matchers.AnMatcher

class ShouldBeAMatcherAndOrSpec extends Spec with Matchers with SharedHelpers {

  object `AMatcher ` {
    
    val passedMarks = AMatcher[Int]("passed marks") { _ >= 40  }
    val validMarks = AMatcher[Int]("valid marks") { mark => mark >= 0 && mark <= 100 }
    
    object `when use with 'and'` {
      
      def `should pass when both be a passes` {
        
        88 should (be a passedMarks and be a validMarks)
        88 should ((be a (passedMarks)) and (be a (validMarks)))
        88 should (be a (passedMarks) and (be a (validMarks))) 
        
      }
      
      def `should failed with correctly stack depth and message when first be a failed but second be a passed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          101 should (be a validMarks and be a passedMarks)
        }
        e1.message should be (Some("101 was not a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          101 should ((be a (validMarks)) and (be a (passedMarks)))
        }
        e2.message should be (Some("101 was not a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          101 should (be a (validMarks) and (be a (passedMarks))) 
        }
        e3.message should be (Some("101 was not a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first be a passed but second be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          101 should (be a passedMarks and be a validMarks)
        }
        e1.message should be (Some("101 was a passed marks, but 101 was not a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          101 should ((be a (passedMarks)) and (be a (validMarks)))
        }
        e2.message should be (Some("101 was a passed marks, but 101 was not a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          101 should (be a (passedMarks) and (be a (validMarks))) 
        }
        e3.message should be (Some("101 was a passed marks, but 101 was not a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not be a and be a passes` {
        
        30 should (not be a (passedMarks) and be a validMarks)
        30 should ((not be a (passedMarks)) and (be a (validMarks)))
        30 should (not be a (passedMarks) and (be a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not be a passed but be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          -88 should (not be a (passedMarks) and be a validMarks)
        }
        e1.message should be (Some("-88 was not a passed marks, but -88 was not a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          -88 should ((not be a (passedMarks)) and (be a (validMarks)))
        }
        e2.message should be (Some("-88 was not a passed marks, but -88 was not a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          -88 should (not be a (passedMarks) and (be a (validMarks))) 
        }
        e3.message should be (Some("-88 was not a passed marks, but -88 was not a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when not be a failed but be a passed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          80 should (not be a (passedMarks) and be a validMarks)
        }
        e1.message should be (Some("80 was a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          80 should ((not be a (passedMarks)) and (be a (validMarks)))
        }
        e2.message should be (Some("80 was a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          80 should (not be a (passedMarks) and (be a (validMarks))) 
        }
        e3.message should be (Some("80 was a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when be a and not be a passes` {
        
        30 should (be a (validMarks) and not be a (passedMarks))
        30 should ((be a (validMarks)) and (not be a (passedMarks)))
        30 should (be a (validMarks) and (not be a (passedMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when be a passed but not be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          80 should (be a (passedMarks) and not be a (validMarks))
        }
        e1.message should be (Some("80 was a passed marks, but 80 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          80 should ((be a (passedMarks)) and (not be a (validMarks)))
        }
        e2.message should be (Some("80 was a passed marks, but 80 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          80 should (be a (passedMarks) and (not be a (validMarks))) 
        }
        e3.message should be (Some("80 was a passed marks, but 80 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when be a failed but not be a passed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          -80 should (be a (passedMarks) and not be a (validMarks))
        }
        e1.message should be (Some("-80 was not a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          -80 should ((be a (passedMarks)) and (not be a (validMarks)))
        }
        e2.message should be (Some("-80 was not a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          -80 should (be a (passedMarks) and (not be a (validMarks))) 
        }
        e3.message should be (Some("-80 was not a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not be a and not be a passes` {
        
        -10 should (not be a (passedMarks) and not be a (validMarks))
        -10 should ((not be a (passedMarks)) and (not be a (validMarks)))
        -10 should (not be a (passedMarks) and (not be a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not be a passed but not be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          38 should (not be a (passedMarks) and not be a (validMarks))
        }
        e1.message should be (Some("38 was not a passed marks, but 38 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          38 should ((not be a (passedMarks)) and (not be a (validMarks)))
        }
        e2.message should be (Some("38 was not a passed marks, but 38 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          38 should (not be a (passedMarks) and (not be a (validMarks))) 
        }
        e3.message should be (Some("38 was not a passed marks, but 38 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when not be a failed but not be a passed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          110 should (not be a (passedMarks) and not be a (validMarks))
        }
        e1.message should be (Some("110 was a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          110 should ((not be a (passedMarks)) and (not be a (validMarks)))
        }
        e2.message should be (Some("110 was a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          110 should (not be a (passedMarks) and (not be a (validMarks))) 
        }
        e3.message should be (Some("110 was a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when use with 'or'` {
      
      def `should pass when both be a passes` {
        
        88 should (be a passedMarks or be a validMarks)
        88 should ((be a (passedMarks)) or (be a (validMarks)))
        88 should (be a (passedMarks) or (be a (validMarks))) 
        
      }
      
      def `should pass when be a passed and be a failed` {
        
        128 should (be a passedMarks or be a validMarks)
        128 should ((be a (passedMarks)) or (be a (validMarks)))
        128 should (be a (passedMarks) or (be a (validMarks)))
        
      }
      
      def `should pass when be a failed and be a passed` {
        
        28 should (be a passedMarks or be a validMarks)
        28 should ((be a (passedMarks)) or (be a (validMarks)))
        28 should (be a (passedMarks) or (be a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when both be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          -68 should (be a validMarks or be a passedMarks)
        }
        e1.message should be (Some("-68 was not a valid marks, and -68 was not a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          -68 should ((be a (validMarks)) or (be a (passedMarks)))
        }
        e2.message should be (Some("-68 was not a valid marks, and -68 was not a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          -68 should (be a (validMarks) or (be a (passedMarks))) 
        }
        e3.message should be (Some("-68 was not a valid marks, and -68 was not a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not be a and be a passes` {
        
        30 should (not be a (passedMarks) or be a validMarks)
        30 should ((not be a (passedMarks)) or (be a (validMarks)))
        30 should (not be a (passedMarks) or (be a (validMarks)))
        
      }
      
      def `should pass when not be a passed and be a failed` {
        
        -30 should (not be a (passedMarks) or be a validMarks)
        -30 should ((not be a (passedMarks)) or (be a (validMarks)))
        -30 should (not be a (passedMarks) or (be a (validMarks)))
        
      }
      
      def `should pass when not be a failed and be a passed` {
        
        88 should (not be a (passedMarks) or be a validMarks)
        88 should ((not be a (passedMarks)) or (be a (validMarks)))
        88 should (not be a (passedMarks) or (be a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not be a failed and be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          168 should (not be a (passedMarks) or be a validMarks)
        }
        e1.message should be (Some("168 was a passed marks, and 168 was not a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          168 should ((not be a (passedMarks)) or (be a (validMarks)))
        }
        e2.message should be (Some("168 was a passed marks, and 168 was not a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          168 should (not be a (passedMarks) or (be a (validMarks))) 
        }
        e3.message should be (Some("168 was a passed marks, and 168 was not a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when be a and not be a passed` {
        
        30 should (be a (validMarks) or not be a (passedMarks))
        30 should ((be a (validMarks)) or (not be a (passedMarks)))
        30 should (be a (validMarks) or (not be a (passedMarks)))
        
      }
      
      def `should pass when be a passed and not be a failed` {
        
        88 should (be a (validMarks) or not be a (passedMarks))
        88 should ((be a (validMarks)) or (not be a (passedMarks)))
        88 should (be a (validMarks) or (not be a (passedMarks)))
        
      }
      
      def `should pass when be a failed and not be a passed` {
        
        -28 should (be a (validMarks) or not be a (passedMarks))
        -28 should ((be a (validMarks)) or (not be a (passedMarks)))
        -28 should (be a (validMarks) or (not be a (passedMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when be a failed and not be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          28 should (be a (passedMarks) or not be a (validMarks))
        }
        e1.message should be (Some("28 was not a passed marks, and 28 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          28 should ((be a (passedMarks)) or (not be a (validMarks)))
        }
        e2.message should be (Some("28 was not a passed marks, and 28 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          28 should (be a (passedMarks) or (not be a (validMarks))) 
        }
        e3.message should be (Some("28 was not a passed marks, and 28 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not be a and not be a passed` {
        
        -10 should (not be a (passedMarks) or not be a (validMarks))
        -10 should ((not be a (passedMarks)) or (not be a (validMarks)))
        -10 should (not be a (passedMarks) or (not be a (validMarks)))
        
      }
      
      def `should pass when not be a passed and not be a failed` {
        
        28 should (not be a (passedMarks) or not be a (validMarks))
        28 should ((not be a (passedMarks)) or (not be a (validMarks)))
        28 should (not be a (passedMarks) or (not be a (validMarks)))
        
      }
      
      def `should pass when not be a failed and not be a passed` {
        
        128 should (not be a (passedMarks) or not be a (validMarks))
        128 should ((not be a (passedMarks)) or (not be a (validMarks)))
        128 should (not be a (passedMarks) or (not be a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not be a failed and not be a failed` {
        
        val e1 = intercept[exceptions.TestFailedException] {
          68 should (not be a (passedMarks) or not be a (validMarks))
        }
        e1.message should be (Some("68 was a passed marks, and 68 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          68 should ((not be a (passedMarks)) or (not be a (validMarks)))
        }
        e2.message should be (Some("68 was a passed marks, and 68 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          68 should (not be a (passedMarks) or (not be a (validMarks))) 
        }
        e3.message should be (Some("68 was a passed marks, and 68 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
  }
  
}
