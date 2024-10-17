/*
 * Copyright 2001-2024 Artima, Inc.
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

import matchers.AMatcher
import matchers.AnMatcher
import SharedHelpers._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeAnMatcherAndOrSpec extends AnyFunSpec {

  describe("AnMatcher ") {
    
    val oddMarks = AnMatcher[Int]("odd marks") { _ % 2 == 1 }
    val integerValidMarks = AnMatcher[Int]("integer valid marks") { mark => mark >= 0 && mark <= 100 }
    
    describe("when use with 'and'") {
      
      it("should pass when both be an passes") {
        
        99 should (be an oddMarks and be an integerValidMarks)
        99 should ((be an (oddMarks)) and (be an (integerValidMarks)))
        99 should (be an (oddMarks) and (be an (integerValidMarks))) 
        
      }
      
      it("should failed with correctly stack depth and message when first be an failed but second be an passed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          101 should (be an integerValidMarks and be an oddMarks)
        }
        e1.message should be (Some("101 was not an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          101 should ((be an (integerValidMarks)) and (be an (oddMarks)))
        }
        e2.message should be (Some("101 was not an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          101 should (be an (integerValidMarks) and (be an (oddMarks))) 
        }
        e3.message should be (Some("101 was not an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should failed with correctly stack depth and message when first be a passed but second be a failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          101 should (be an oddMarks and be an integerValidMarks)
        }
        e1.message should be (Some("101 was an odd marks, but 101 was not an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          101 should ((be an (oddMarks)) and (be an (integerValidMarks)))
        }
        e2.message should be (Some("101 was an odd marks, but 101 was not an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          101 should (be an (oddMarks) and (be an (integerValidMarks))) 
        }
        e3.message should be (Some("101 was an odd marks, but 101 was not an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should pass when not be an and be an passes") {
        
        30 should (not be an (oddMarks) and be an integerValidMarks)
        30 should ((not be an (oddMarks)) and (be an (integerValidMarks)))
        30 should (not be an (oddMarks) and (be an (integerValidMarks)))
        
      }
      
      it("should failed with correctly stack depth and message when not be an passed but be an failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          110 should (not be an (oddMarks) and be an integerValidMarks)
        }
        e1.message should be (Some("110 was not an odd marks, but 110 was not an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          110 should ((not be an (oddMarks)) and (be an (integerValidMarks)))
        }
        e2.message should be (Some("110 was not an odd marks, but 110 was not an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          110 should (not be an (oddMarks) and (be an (integerValidMarks))) 
        }
        e3.message should be (Some("110 was not an odd marks, but 110 was not an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should failed with correctly stack depth and message when not be an failed but be an passed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          31 should (not be an (oddMarks) and be an integerValidMarks)
        }
        e1.message should be (Some("31 was an odd marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          31 should ((not be an (oddMarks)) and (be an (integerValidMarks)))
        }
        e2.message should be (Some("31 was an odd marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          31 should (not be an (oddMarks) and (be an (integerValidMarks))) 
        }
        e3.message should be (Some("31 was an odd marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should pass when be an and not be an passes") {
        
        30 should (be an (integerValidMarks) and not be an (oddMarks))
        30 should ((be an (integerValidMarks)) and (not be an (oddMarks)))
        30 should (be an (integerValidMarks) and (not be an (oddMarks)))
        
      }
      
      it("should failed with correctly stack depth and message when be an passed but not be an failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          81 should (be an (oddMarks) and not be an (integerValidMarks))
        }
        e1.message should be (Some("81 was an odd marks, but 81 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          81 should ((be an (oddMarks)) and (not be an (integerValidMarks)))
        }
        e2.message should be (Some("81 was an odd marks, but 81 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          81 should (be an (oddMarks) and (not be an (integerValidMarks))) 
        }
        e3.message should be (Some("81 was an odd marks, but 81 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should failed with correctly stack depth and message when be a failed but not be a passed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          -80 should (be an (oddMarks) and not be an (integerValidMarks))
        }
        e1.message should be (Some("-80 was not an odd marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          -80 should ((be an (oddMarks)) and (not be an (integerValidMarks)))
        }
        e2.message should be (Some("-80 was not an odd marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          -80 should (be an (oddMarks) and (not be an (integerValidMarks))) 
        }
        e3.message should be (Some("-80 was not an odd marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should pass when not be an and not be an passes") {
        
        -30 should (not be an (oddMarks) and not be an (integerValidMarks))
        -30 should ((not be an (oddMarks)) and (not be an (integerValidMarks)))
        -30 should (not be an (oddMarks) and (not be an (integerValidMarks)))
        
      }
      
      it("should failed with correctly stack depth and message when not be an passed but not be an failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          80 should (not be an (oddMarks) and not be an (integerValidMarks))
        }
        e1.message should be (Some("80 was not an odd marks, but 80 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          80 should ((not be an (oddMarks)) and (not be an (integerValidMarks)))
        }
        e2.message should be (Some("80 was not an odd marks, but 80 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          80 should (not be an (oddMarks) and (not be an (integerValidMarks))) 
        }
        e3.message should be (Some("80 was not an odd marks, but 80 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should failed with correctly stack depth and message when not be a failed but not be a passed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          111 should (not be an (oddMarks) and not be an (integerValidMarks))
        }
        e1.message should be (Some("111 was an odd marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          111 should ((not be an (oddMarks)) and (not be an (integerValidMarks)))
        }
        e2.message should be (Some("111 was an odd marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          111 should (not be an (oddMarks) and (not be an (integerValidMarks))) 
        }
        e3.message should be (Some("111 was an odd marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when use with 'or'") {
      
      it("should pass when both be an passes") {
        
        89 should (be an oddMarks or be an integerValidMarks)
        89 should ((be an (oddMarks)) or (be an (integerValidMarks)))
        89 should (be an (oddMarks) or (be an (integerValidMarks))) 
        
      }
      
      it("should pass when be an passed and be an failed") {
        
        101 should (be an oddMarks or be an integerValidMarks)
        101 should ((be an (oddMarks)) or (be an (integerValidMarks)))
        101 should (be an (oddMarks) or (be an (integerValidMarks))) 
        
      }
      
      it("should pass when be an failed and be an passed") {
        
        88 should (be an oddMarks or be an integerValidMarks)
        88 should ((be an (oddMarks)) or (be an (integerValidMarks)))
        88 should (be an (oddMarks) or (be an (integerValidMarks))) 
        
      }
      
      it("should failed with correctly stack depth and message when both be an failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          168 should (be an oddMarks or be an integerValidMarks)
        }
        e1.message should be (Some("168 was not an odd marks, and 168 was not an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          168 should ((be an (oddMarks)) or (be an (integerValidMarks)))
        }
        e2.message should be (Some("168 was not an odd marks, and 168 was not an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          168 should (be an (oddMarks) or (be an (integerValidMarks))) 
        }
        e3.message should be (Some("168 was not an odd marks, and 168 was not an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should pass when not be an and be an passes") {
        
        30 should (not be an (oddMarks) or be an integerValidMarks)
        30 should ((not be an (oddMarks)) or (be an (integerValidMarks)))
        30 should (not be an (oddMarks) or (be an (integerValidMarks)))
        
      }
      
      it("should pass when not be an passed and be an failed") {
        
       168 should (not be an (oddMarks) or be an integerValidMarks)
       168 should ((not be an (oddMarks)) or (be an (integerValidMarks)))
       168 should (not be an (oddMarks) or (be an (integerValidMarks)))
        
      }
      
      it("should pass when not be an failed and be an passed") {
        
        99 should (not be an (oddMarks) or be an integerValidMarks)
        99 should ((not be an (oddMarks)) or (be an (integerValidMarks)))
        99 should (not be an (oddMarks) or (be an (integerValidMarks)))
        
      }
      
      it("should failed with correctly stack depth and message when not be an failed and be an failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          199 should (not be an (oddMarks) or be an integerValidMarks)
        }
        e1.message should be (Some("199 was an odd marks, and 199 was not an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          199 should ((not be an (oddMarks)) or (be an (integerValidMarks)))
        }
        e2.message should be (Some("199 was an odd marks, and 199 was not an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          199 should (not be an (oddMarks) or (be an (integerValidMarks))) 
        }
        e3.message should be (Some("199 was an odd marks, and 199 was not an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should pass when be an and not be an passed") {
        
        30 should (be an (integerValidMarks) or not be an (oddMarks))
        30 should ((be an (integerValidMarks)) or (not be an (oddMarks)))
        30 should (be an (integerValidMarks) or (not be an (oddMarks)))
        
      }
      
      it("should pass when be an passed and not be an failed") {
        
        31 should (be an (integerValidMarks) or not be an (oddMarks))
        31 should ((be an (integerValidMarks)) or (not be an (oddMarks)))
        31 should (be an (integerValidMarks) or (not be an (oddMarks)))
        
      }
      
      it("should pass when be an failed and not be an passed") {
        
        130 should (be an (integerValidMarks) or not be an (oddMarks))
        130 should ((be an (integerValidMarks)) or (not be an (oddMarks)))
        130 should (be an (integerValidMarks) or (not be an (oddMarks)))
        
      }
      
      it("should failed with correctly stack depth and message when be an failed and not be an failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          28 should (be an (oddMarks) or not be an (integerValidMarks))
        }
        e1.message should be (Some("28 was not an odd marks, and 28 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          28 should ((be an (oddMarks)) or (not be an (integerValidMarks)))
        }
        e2.message should be (Some("28 was not an odd marks, and 28 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          28 should (be an (oddMarks) or (not be an (integerValidMarks))) 
        }
        e3.message should be (Some("28 was not an odd marks, and 28 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should pass when not be an and not be an passed") {
        
        -10 should (not be an (oddMarks) or not be an (integerValidMarks))
        -10 should ((not be an (oddMarks)) or (not be an (integerValidMarks)))
        -10 should (not be an (oddMarks) or (not be an (integerValidMarks)))
        
      }
      
      it("should pass when not be an passed and not be an failed") {
        
        10 should (not be an (oddMarks) or not be an (integerValidMarks))
        10 should ((not be an (oddMarks)) or (not be an (integerValidMarks)))
        10 should (not be an (oddMarks) or (not be an (integerValidMarks)))
        
      }
      
      it("should pass when not be an failed and not be an passed") {
        
        111 should (not be an (oddMarks) or not be an (integerValidMarks))
        111 should ((not be an (oddMarks)) or (not be an (integerValidMarks)))
        111 should (not be an (oddMarks) or (not be an (integerValidMarks)))
        
      }
      
      it("should failed with correctly stack depth and message when not be an failed and not be an failed") {
        
        val e1 = intercept[exceptions.TestFailedException] {
          99 should (not be an (oddMarks) or not be an (integerValidMarks))
        }
        e1.message should be (Some("99 was an odd marks, and 99 was an integer valid marks"))
        e1.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e2 = intercept[exceptions.TestFailedException] {
          99 should ((not be an (oddMarks)) or (not be an (integerValidMarks)))
        }
        e2.message should be (Some("99 was an odd marks, and 99 was an integer valid marks"))
        e2.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))
        
        val e3 = intercept[exceptions.TestFailedException] {
          99 should (not be an (oddMarks) or (not be an (integerValidMarks))) 
        }
        e3.message should be (Some("99 was an odd marks, and 99 was an integer valid marks"))
        e3.failedCodeFileName should be (Some("ShouldBeAnMatcherAndOrSpec.scala"))
        e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
  }
  
}
