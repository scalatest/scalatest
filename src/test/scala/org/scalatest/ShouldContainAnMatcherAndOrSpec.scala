package org.scalatest

import matchers.AnMatcher
import matchers.AMatcher

class ShouldContainAnMatcherAndOrSpec extends Spec with Matchers with SharedHelpers {

  object `AMatcher ` {
    
    val oddMarks = AnMatcher[Int]("odd marks") { _ % 2 == 1 }
    val integerValidMarks = AnMatcher[Int]("integer valid marks") { mark => mark >= 0 && mark <= 100 }
   
    object `when use with 'and'` {
      
      def `should pass when both contain a passes` {
        
        List(129, 88, 28) should (contain an oddMarks and contain an integerValidMarks)
        List(129, 88, 28) should ((contain an (oddMarks)) and (contain an (integerValidMarks)))
        List(129, 88, 28) should (contain an (oddMarks) and (contain an (integerValidMarks))) 
        
      }
      
      def `should failed with correctly stack depth and message when first contain an failed but second contain an passed` {
        val left = List(119, 129, 139)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain an integerValidMarks and contain an oddMarks)
        }
        e1.message should be (Some(left + " did not contain an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain an (integerValidMarks)) and (contain an (oddMarks)))
        }
        e2.message should be (Some(left + " did not contain an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain an (integerValidMarks) and (contain an (oddMarks))) 
        }
        e3.message should be (Some(left + " did not contain an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when first contain a passed but second contain a failed` {
        val left = List(119, 129, 139)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain an oddMarks and contain an integerValidMarks)
        }
        e1.message should be (Some(left + " contained an odd marks: 119 was an odd marks, but " + left + " did not contain an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain an (oddMarks)) and (contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " contained an odd marks: 119 was an odd marks, but " + left + " did not contain an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) and (contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " contained an odd marks: 119 was an odd marks, but " + left + " did not contain an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain an and contain an passes` {
        
        List(28, 38, 88) should (not contain an (oddMarks) and contain an integerValidMarks)
        List(28, 38, 88) should ((not contain an (oddMarks)) and (contain an (integerValidMarks)))
        List(28, 38, 88) should (not contain an (oddMarks) and (contain an (integerValidMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain an passed but contain an failed` {
        val left = List(118, 128, 168)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and contain an integerValidMarks)
        }
        e1.message should be (Some(left + " did not contain an odd marks, but " + left + " did not contain an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain an (oddMarks)) and (contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " did not contain an odd marks, but " + left + " did not contain an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and (contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " did not contain an odd marks, but " + left + " did not contain an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when not contain an failed but contain an passed` {
        val left = List(15, 28, 68)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and contain an integerValidMarks)
        }
        e1.message should be (Some(left + " contained an odd marks: 15 was an odd marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain an (oddMarks)) and (contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " contained an odd marks: 15 was an odd marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and (contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " contained an odd marks: 15 was an odd marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when contain an and not contain an passes` {
        
        List(28, 68, 88) should (contain an (integerValidMarks) and not contain an (oddMarks))
        List(28, 68, 88) should ((contain an (integerValidMarks)) and (not contain an (oddMarks)))
        List(28, 68, 88) should (contain an (integerValidMarks) and (not contain an (oddMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when contain an passed but not contain an failed` {
        val left = List(28, 68, 89)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) and not contain an (integerValidMarks))
        }
        e1.message should be (Some(left + " contained an odd marks: 89 was an odd marks, but " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain an (oddMarks)) and (not contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " contained an odd marks: 89 was an odd marks, but " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) and (not contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " contained an odd marks: 89 was an odd marks, but " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when contain a failed but not contain a passed` {
        val left = List(128, 168, 188)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) and not contain an (integerValidMarks))
        }
        e1.message should be (Some(left + " did not contain an odd marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain an (oddMarks)) and (not contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " did not contain an odd marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) and (not contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " did not contain an odd marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain an and not contain an passes` {
        
        List(128, 168, 188) should (not contain an (oddMarks) and not contain an (integerValidMarks))
        List(128, 168, 188) should ((not contain an (oddMarks)) and (not contain an (integerValidMarks)))
        List(128, 168, 188) should (not contain an (oddMarks) and (not contain an (integerValidMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain an passed but not contain an failed` {
        val left = List(28, 68, 88)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and not contain an (integerValidMarks))
        }
        e1.message should be (Some(left + " did not contain an odd marks, but " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain an (oddMarks)) and (not contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " did not contain an odd marks, but " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and (not contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " did not contain an odd marks, but " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should failed with correctly stack depth and message when not contain a failed but not contain a passed` {
        val left = List(128, 168, 189)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and not contain an (integerValidMarks))
        }
        e1.message should be (Some(left + " contained an odd marks: 189 was an odd marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain an (oddMarks)) and (not contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " contained an odd marks: 189 was an odd marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) and (not contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " contained an odd marks: 189 was an odd marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when use with 'or'` {
      
      def `should pass when both contain an passes` {
        
        List(19, 68, 88) should (contain an oddMarks or contain an integerValidMarks)
        List(19, 68, 88) should ((contain an (oddMarks)) or (contain an (integerValidMarks)))
        List(19, 68, 88) should (contain an (oddMarks) or (contain an (integerValidMarks))) 
        
      }
      
      def `should pass when contain an passed and contain an failed` {
        
        List(119, 168, 188) should (contain an oddMarks or contain an integerValidMarks)
        List(119, 168, 188) should ((contain an (oddMarks)) or (contain an (integerValidMarks)))
        List(119, 168, 188) should (contain an (oddMarks) or (contain an (integerValidMarks))) 
        
      }
      
      def `should pass when contain an failed and contain an passed` {
        
        List(18, 68, 88) should (contain an oddMarks or contain an integerValidMarks)
        List(18, 68, 88) should ((contain an (oddMarks)) or (contain an (integerValidMarks)))
        List(18, 68, 88) should (contain an (oddMarks) or (contain an (integerValidMarks))) 
        
      }
      
      def `should failed with correctly stack depth and message when both contain an failed` {
        val left = List(118, 168, 188)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain an oddMarks or contain an integerValidMarks)
        }
        e1.message should be (Some(left + " did not contain an odd marks, and " + left + " did not contain an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain an (oddMarks)) or (contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " did not contain an odd marks, and " + left + " did not contain an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) or (contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " did not contain an odd marks, and " + left + " did not contain an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain an and contain an passes` {
        
        List(28, 68, 88) should (not contain an (oddMarks) or contain an integerValidMarks)
        List(28, 68, 88) should ((not contain an (oddMarks)) or (contain an (integerValidMarks)))
        List(28, 68, 88) should (not contain an (oddMarks) or (contain an (integerValidMarks)))
        
      }
      
      def `should pass when not contain an passed and contain an failed` {
        
       List(128, 168, 188) should (not contain an (oddMarks) or contain an integerValidMarks)
       List(128, 168, 188) should ((not contain an (oddMarks)) or (contain an (integerValidMarks)))
       List(128, 168, 188) should (not contain an (oddMarks) or (contain an (integerValidMarks)))
        
      }
      
      def `should pass when not contain an failed and contain an passed` {
        
        List(28, 68, 89) should (not contain an (oddMarks) or contain an integerValidMarks)
        List(28, 68, 89) should ((not contain an (oddMarks)) or (contain an (integerValidMarks)))
        List(28, 68, 89) should (not contain an (oddMarks) or (contain an (integerValidMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain an failed and contain an failed` {
        val left = List(128, 168, 189)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) or contain an integerValidMarks)
        }
        e1.message should be (Some(left + " contained an odd marks: 189 was an odd marks, and " + left + " did not contain an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain an (oddMarks)) or (contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " contained an odd marks: 189 was an odd marks, and " + left + " did not contain an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) or (contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " contained an odd marks: 189 was an odd marks, and " + left + " did not contain an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when contain an and not contain an passed` {
        
        List(28, 68, 88) should (contain an (integerValidMarks) or not contain an (oddMarks))
        List(28, 68, 88) should ((contain an (integerValidMarks)) or (not contain an (oddMarks)))
        List(28, 68, 88) should (contain an (integerValidMarks) or (not contain an (oddMarks)))
        
      }
      
      def `should pass when contain an passed and not contain an failed` {
        
        List(28, 68, 89) should (contain an (integerValidMarks) or not contain an (oddMarks))
        List(28, 68, 89) should ((contain an (integerValidMarks)) or (not contain an (oddMarks)))
        List(28, 68, 89) should (contain an (integerValidMarks) or (not contain an (oddMarks)))
        
      }
      
      def `should pass when contain an failed and not contain an passed` {
        
        List(128, 168, 188) should (contain an (integerValidMarks) or not contain an (oddMarks))
        List(128, 168, 188) should ((contain an (integerValidMarks)) or (not contain an (oddMarks)))
        List(128, 168, 188) should (contain an (integerValidMarks) or (not contain an (oddMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when contain an failed and not contain an failed` {
        val left = List(28, 68, 88)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) or not contain an (integerValidMarks))
        }
        e1.message should be (Some(left + " did not contain an odd marks, and " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((contain an (oddMarks)) or (not contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " did not contain an odd marks, and " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (contain an (oddMarks) or (not contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " did not contain an odd marks, and " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should pass when not contain an and not contain an passed` {
        
        List(128, 168, 188) should (not contain an (oddMarks) or not contain an (integerValidMarks))
        List(128, 168, 188) should ((not contain an (oddMarks)) or (not contain an (integerValidMarks)))
        List(128, 168, 188) should (not contain an (oddMarks) or (not contain an (integerValidMarks)))
        
      }
      
      def `should pass when not contain an passed and not contain an failed` {
        
        List(28, 68, 88) should (not contain an (oddMarks) or not contain an (integerValidMarks))
        List(28, 68, 88) should ((not contain an (oddMarks)) or (not contain an (integerValidMarks)))
        List(28, 68, 88) should (not contain an (oddMarks) or (not contain an (integerValidMarks)))
        
      }
      
      def `should pass when not contain an failed and not contain an passed` {
        
        List(128, 168, 189) should (not contain an (oddMarks) or not contain an (integerValidMarks))
        List(128, 168, 189) should ((not contain an (oddMarks)) or (not contain an (integerValidMarks)))
        List(128, 168, 189) should (not contain an (oddMarks) or (not contain an (integerValidMarks)))
        
      }
      
      def `should failed with correctly stack depth and message when not contain an failed and not contain an failed` {
        val left = List(28, 68, 89)
        
        val e1 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) or not contain an (integerValidMarks))
        }
        e1.message should be (Some(left + " contained an odd marks: 89 was an odd marks, and " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          left should ((not contain an (oddMarks)) or (not contain an (integerValidMarks)))
        }
        e2.message should be (Some(left + " contained an odd marks: 89 was an odd marks, and " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          left should (not contain an (oddMarks) or (not contain an (integerValidMarks))) 
        }
        e3.message should be (Some(left + " contained an odd marks: 89 was an odd marks, and " + left + " contained an integer valid marks: 28 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldContainAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
  }
  
}
