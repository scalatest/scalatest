package org.scalatest

import matchers.AMatcher
import matchers.AnMatcher

class ShouldContainAMatcherAndOrSpec extends Spec with Matchers with SharedHelpers {

  object `AMatcher ` {
    
    val passedMarks = AMatcher[Int]("passed marks") { _ >= 40  }
    val validMarks = AMatcher[Int]("valid marks") { mark => mark >= 0 && mark <= 100 }
   
    object `when use with 'and'` {
      
      def `should pass when both contain a passes` {
        
        List(128, 88, 28) should (contain a passedMarks and contain a validMarks)
        List(128, 88, 28) should ((contain a (passedMarks)) and (contain a (validMarks)))
        List(128, 88, 28) should (contain a (passedMarks) and (contain a (validMarks))) 
        
      }
      
      def `should failed with correctly stack depth and message when first contain a failed but second contain a passed` {
        val left = List(168, 178, 188)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain a validMarks and contain a passedMarks)
        }
        e1.message should be (Some(left + " did not contain a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain a (validMarks)) and (contain a (passedMarks)))
        }
        e2.message should be (Some(left + " did not contain a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain a (validMarks) and (contain a (passedMarks))) 
        }
        e3.message should be (Some(left + " did not contain a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first contain a passed but second contain a failed` {
        val left = List(168, 178, 188)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain a passedMarks and contain a validMarks)
        }
        e1.message should be (Some(left + " contained a passed marks: 168 was a passed marks, but " + left + " did not contain a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain a (passedMarks)) and (contain a (validMarks)))
        }
        e2.message should be (Some(left + " contained a passed marks: 168 was a passed marks, but " + left + " did not contain a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain a (passedMarks) and (contain a (validMarks))) 
        }
        e3.message should be (Some(left + " contained a passed marks: 168 was a passed marks, but " + left + " did not contain a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain a and contain a passes` {
        
        List(-1, 18, 28) should (not contain a (passedMarks) and contain a validMarks)
        List(-1, 18, 28) should ((not contain a (passedMarks)) and (contain a (validMarks)))
        List(-1, 18, 28) should (not contain a (passedMarks) and (contain a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain a passed but contain a failed` {
        val left = List(-1, -18, -28)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and contain a validMarks)
        }
        e1.message should be (Some(left + " did not contain a passed marks, but " + left + " did not contain a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain a (passedMarks)) and (contain a (validMarks)))
        }
        e2.message should be (Some(left + " did not contain a passed marks, but " + left + " did not contain a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and (contain a (validMarks))) 
        }
        e3.message should be (Some(left + " did not contain a passed marks, but " + left + " did not contain a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when not contain a failed but contain a passed` {
        val left = List(68, 18, 28)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and contain a validMarks)
        }
        e1.message should be (Some(left + " contained a passed marks: 68 was a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain a (passedMarks)) and (contain a (validMarks)))
        }
        e2.message should be (Some(left + " contained a passed marks: 68 was a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and (contain a (validMarks))) 
        }
        e3.message should be (Some(left + " contained a passed marks: 68 was a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when contain a and not contain a passes` {
        
        List(8, 18, 28) should (contain a (validMarks) and not contain a (passedMarks))
        List(8, 18, 28) should ((contain a (validMarks)) and (not contain a (passedMarks)))
        List(8, 18, 28) should (contain a (validMarks) and (not contain a (passedMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when contain a passed but not contain a failed` {
        val left = List(68, 78, 88)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain a (passedMarks) and not contain a (validMarks))
        }
        e1.message should be (Some(left + " contained a passed marks: 68 was a passed marks, but " + left + " contained a valid marks: 68 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain a (passedMarks)) and (not contain a (validMarks)))
        }
        e2.message should be (Some(left + " contained a passed marks: 68 was a passed marks, but " + left + " contained a valid marks: 68 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain a (passedMarks) and (not contain a (validMarks))) 
        }
        e3.message should be (Some(left + " contained a passed marks: 68 was a passed marks, but " + left + " contained a valid marks: 68 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when contain a failed but not contain a passed` {
        val left = List(8, 18, 28)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain a (passedMarks) and not contain a (validMarks))
        }
        e1.message should be (Some(left + " did not contain a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain a (passedMarks)) and (not contain a (validMarks)))
        }
        e2.message should be (Some(left + " did not contain a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain a (passedMarks) and (not contain a (validMarks))) 
        }
        e3.message should be (Some(left + " did not contain a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain a and not contain a passes` {
        
        List(-8, -18, -28) should (not contain a (passedMarks) and not contain a (validMarks))
        List(-8, -18, -28) should ((not contain a (passedMarks)) and (not contain a (validMarks)))
        List(-8, -18, -28) should (not contain a (passedMarks) and (not contain a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain a passed but not contain a failed` {
        val left = List(8, 18, 28)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and not contain a (validMarks))
        }
        e1.message should be (Some(left + " did not contain a passed marks, but " + left + " contained a valid marks: 8 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain a (passedMarks)) and (not contain a (validMarks)))
        }
        e2.message should be (Some(left + " did not contain a passed marks, but " + left + " contained a valid marks: 8 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and (not contain a (validMarks))) 
        }
        e3.message should be (Some(left + " did not contain a passed marks, but " + left + " contained a valid marks: 8 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when not contain a failed but not contain a passed` {
        val left = List(128, 168, 188)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and not contain a (validMarks))
        }
        e1.message should be (Some(left + " contained a passed marks: 128 was a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain a (passedMarks)) and (not contain a (validMarks)))
        }
        e2.message should be (Some(left + " contained a passed marks: 128 was a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) and (not contain a (validMarks))) 
        }
        e3.message should be (Some(left + " contained a passed marks: 128 was a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    object `when use with 'or'` {
      
      def `should pass when both contain a passes` {
        
        List(28, 68, 88) should (contain a passedMarks or contain a validMarks)
        List(28, 68, 88) should ((contain a (passedMarks)) or (contain a (validMarks)))
        List(28, 68, 88) should (contain a (passedMarks) or (contain a (validMarks))) 
        
      }
      
      def `should pass when contain a passed and contain a failed` {
        
        List(128, 168, 188) should (contain a passedMarks or contain a validMarks)
        List(128, 168, 188) should ((contain a (passedMarks)) or (contain a (validMarks)))
        List(128, 168, 188) should (contain a (passedMarks) or (contain a (validMarks)))
        
      }
      
      def `should pass when contain a failed and contain a passed` {
        
        List(18, 28, 38) should (contain a passedMarks or contain a validMarks)
        List(18, 28, 38) should ((contain a (passedMarks)) or (contain a (validMarks)))
        List(18, 28, 38) should (contain a (passedMarks) or (contain a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when both contain a failed` {
        val left = List(-18, -28, -88)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain a validMarks or contain a passedMarks)
        }
        e1.message should be (Some(left + " did not contain a valid marks, and " + left + " did not contain a passed marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain a (validMarks)) or (contain a (passedMarks)))
        }
        e2.message should be (Some(left + " did not contain a valid marks, and " + left + " did not contain a passed marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain a (validMarks) or (contain a (passedMarks))) 
        }
        e3.message should be (Some(left + " did not contain a valid marks, and " + left + " did not contain a passed marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain a and contain a passes` {
        
        List(8, 18, 28) should (not contain a (passedMarks) or contain a validMarks)
        List(8, 18, 28) should ((not contain a (passedMarks)) or (contain a (validMarks)))
        List(8, 18, 28) should (not contain a (passedMarks) or (contain a (validMarks)))
        
      }
      
      def `should pass when not contain a passed and contain a failed` {
        
        List(-8, -18, -28) should (not contain a (passedMarks) or contain a validMarks)
        List(-8, -18, -28) should ((not contain a (passedMarks)) or (contain a (validMarks)))
        List(-8, -18, -28) should (not contain a (passedMarks) or (contain a (validMarks)))
        
      }
      
      def `should pass when not contain a failed and contain a passed` {
        
        List(128, 28, 88) should (not contain a (passedMarks) or contain a validMarks)
        List(128, 28, 88) should ((not contain a (passedMarks)) or (contain a (validMarks)))
        List(128, 28, 88) should (not contain a (passedMarks) or (contain a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain a failed and contain a failed` {
        val left = List(118, 128, 168)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) or contain a validMarks)
        }
        e1.message should be (Some(left + " contained a passed marks: 118 was a passed marks, and " + left + " did not contain a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain a (passedMarks)) or (contain a (validMarks)))
        }
        e2.message should be (Some(left + " contained a passed marks: 118 was a passed marks, and " + left + " did not contain a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) or (contain a (validMarks))) 
        }
        e3.message should be (Some(left + " contained a passed marks: 118 was a passed marks, and " + left + " did not contain a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when contain a and not contain a passed` {
        
        List(18, 28, 88) should (contain a (validMarks) or not contain a (passedMarks))
        List(18, 28, 88) should ((contain a (validMarks)) or (not contain a (passedMarks)))
        List(18, 28, 88) should (contain a (validMarks) or (not contain a (passedMarks)))
        
      }
      
      def `should pass when contain a passed and not contain a failed` {
        
        List(18, 28, 88) should (contain a (validMarks) or not contain a (passedMarks))
        List(18, 28, 88) should ((contain a (validMarks)) or (not contain a (passedMarks)))
        List(18, 28, 88) should (contain a (validMarks) or (not contain a (passedMarks)))
        
      }
      
      def `should pass when contain a failed and not contain a passed` {
        
        List(-118, -168, -188) should (contain a (validMarks) or not contain a (passedMarks))
        List(-118, -168, -188) should ((contain a (validMarks)) or (not contain a (passedMarks)))
        List(-118, -168, -188) should (contain a (validMarks) or (not contain a (passedMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when contain a failed and not contain a failed` {
        val left = List(8, 18, 28)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain a (passedMarks) or not contain a (validMarks))
        }
        e1.message should be (Some(left + " did not contain a passed marks, and " + left + " contained a valid marks: 8 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain a (passedMarks)) or (not contain a (validMarks)))
        }
        e2.message should be (Some(left + " did not contain a passed marks, and " + left + " contained a valid marks: 8 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain a (passedMarks) or (not contain a (validMarks))) 
        }
        e3.message should be (Some(left + " did not contain a passed marks, and " + left + " contained a valid marks: 8 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain a and not contain a passed` {
        
        List(-18, -68, -88) should (not contain a (passedMarks) or not contain a (validMarks))
        List(-18, -68, -88) should ((not contain a (passedMarks)) or (not contain a (validMarks)))
        List(-18, -68, -88) should (not contain a (passedMarks) or (not contain a (validMarks)))
        
      }
      
      def `should pass when not contain a passed and not contain a failed` {
        
        List(8, 18, 28) should (not contain a (passedMarks) or not contain a (validMarks))
        List(8, 18, 28) should ((not contain a (passedMarks)) or (not contain a (validMarks)))
        List(8, 18, 28) should (not contain a (passedMarks) or (not contain a (validMarks)))
        
      }
      
      def `should pass when not contain a failed and not contain a passed` {
        
        List(128, 168, 188) should (not contain a (passedMarks) or not contain a (validMarks))
        List(128, 168, 188) should ((not contain a (passedMarks)) or (not contain a (validMarks)))
        List(128, 168, 188) should (not contain a (passedMarks) or (not contain a (validMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain a failed and not contain a failed` {
        val left = List(68, 78, 88)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) or not contain a (validMarks))
        }
        e1.message should be (Some(left + " contained a passed marks: 68 was a passed marks, and " + left + " contained a valid marks: 68 was a valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain a (passedMarks)) or (not contain a (validMarks)))
        }
        e2.message should be (Some(left + " contained a passed marks: 68 was a passed marks, and " + left + " contained a valid marks: 68 was a valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain a (passedMarks) or (not contain a (validMarks))) 
        }
        e3.message should be (Some(left + " contained a passed marks: 68 was a passed marks, and " + left + " contained a valid marks: 68 was a valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
  }
  
}
