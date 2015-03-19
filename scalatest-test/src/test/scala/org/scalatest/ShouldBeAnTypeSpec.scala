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
import Matchers._

class ShouldBeAnTypeSpec extends Spec with Matchers {

  val fileName: String = "ShouldBeAnTypeSpec.scala"
  
  case class Book(title: String)
  
  def wasNotAnInstanceOf(left: Any, right: Class[_]) = 
    FailureMessages.wasNotAnInstanceOf(left, UnquotedString(right.getName), UnquotedString(left.getClass.getName))
    
  def wasAnInstanceOf(left: Any, right: Class[_]) = 
    FailureMessages.wasAnInstanceOf(left, UnquotedString(right.getName))
    
  def wasNotEqualTo(left: Any, right: Any) = 
    FailureMessages.wasNotEqualTo(left, right)
    
  def wasEqualTo(left: Any, right: Any) = 
    FailureMessages.wasEqualTo(left, right)
    
  def didNotEqual(left: Any, right: Any) = 
    FailureMessages.didNotEqual(left, right)
    
  def equaled(left: Any, right: Any) = 
    FailureMessages.equaled(left, right)
    
  val aTaleOfTwoCities = new Book("A Tale of Two Cities")
  val aTaleOfThreeCities = new Book("A Tale of Three Cities")

  // Checking for a specific size
  object `The be an [Type] syntax` {

    def `should do nothing if the LHS is an instance of specified RHS` { 
      aTaleOfTwoCities should be (an [Book])
      aTaleOfTwoCities shouldBe an [Book]
    }

    def `should throw TestFailedException if LHS is not an instance of specified RHS` { 
      val caught1 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should be (an [String])
      }
      assert(caught1.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught1.failedCodeFileName === Some(fileName))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught2 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities shouldBe an [String]
      }
      assert(caught2.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught2.failedCodeFileName === Some(fileName))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    def `should do nothing if LHS is not an instance of specified RHS, when used with not` { 
      aTaleOfTwoCities should not be an [String]
      aTaleOfTwoCities shouldNot be (an [String])
    }

    def `should throw TestFailedException LSH is an instance of specified RHS, when used with not` { 
      val caught1 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should not be an [Book]
      }
      assert(caught1.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught1.failedCodeFileName === Some(fileName))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught2 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities shouldNot be (an [Book])
      }
      assert(caught2.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught2.failedCodeFileName === Some(fileName))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    def `should do nothing if LHS true for both specified RHS, when used in a logical-and expression` { 
      aTaleOfTwoCities should (be (an [Book]) and be (an [Book]))
      aTaleOfTwoCities should (be (aTaleOfTwoCities) and be (an [Book]))
      aTaleOfTwoCities should (be (an [Book]) and be (aTaleOfTwoCities))
      aTaleOfTwoCities should (equal (aTaleOfTwoCities) and be (an [Book]))
      aTaleOfTwoCities should (be (an [Book]) and equal (aTaleOfTwoCities))
    }

    def `should throw TestFailedException if LHS is false for either specified RHS, when used in a logical-and expression` { 
      val caught1 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) and be (an [Book]))
      }
      assert(caught1.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught1.failedCodeFileName === Some(fileName))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught2 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [Book]) and be (an [String]))
      }
      assert(caught2.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book]) + ", but " + wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught2.failedCodeFileName === Some(fileName))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) and be (an [String]))
      }
      assert(caught3.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught3.failedCodeFileName === Some(fileName))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (aTaleOfThreeCities) and be (an [Book]))
      }
      assert(caught4.message === Some(wasNotEqualTo(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught4.failedCodeFileName === Some(fileName))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught5 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (aTaleOfTwoCities) and be (an [String]))
      }
      assert(caught5.message === Some(wasEqualTo(aTaleOfTwoCities, aTaleOfTwoCities) + ", but " + wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught5.failedCodeFileName === Some(fileName))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught6 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (aTaleOfThreeCities) and be (an [String]))
      }
      assert(caught6.message === Some(wasNotEqualTo(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught6.failedCodeFileName === Some(fileName))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught7 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) and be (aTaleOfTwoCities))
      }
      assert(caught7.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught7.failedCodeFileName === Some(fileName))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught8 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [Book]) and be (aTaleOfThreeCities))
      }
      assert(caught8.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book]) + ", but " + wasNotEqualTo(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught8.failedCodeFileName === Some(fileName))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught9 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) and be (aTaleOfThreeCities))
      }
      assert(caught9.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught9.failedCodeFileName === Some(fileName))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught10 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (equal (aTaleOfThreeCities) and be (an [Book]))
      }
      assert(caught10.message === Some(didNotEqual(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught10.failedCodeFileName === Some(fileName))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught11 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (equal (aTaleOfTwoCities) and be (an [String]))
      }
      assert(caught11.message === Some(equaled(aTaleOfTwoCities, aTaleOfTwoCities) + ", but " + wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught11.failedCodeFileName === Some(fileName))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught12 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (equal (aTaleOfThreeCities) and be (an [String]))
      }
      assert(caught12.message === Some(didNotEqual(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught12.failedCodeFileName === Some(fileName))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught13 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) and equal (aTaleOfTwoCities))
      }
      assert(caught13.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught13.failedCodeFileName === Some(fileName))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught14 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [Book]) and equal (aTaleOfThreeCities))
      }
      assert(caught14.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book]) + ", but " + didNotEqual(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught14.failedCodeFileName === Some(fileName))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught15 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) and equal (aTaleOfThreeCities))
      }
      assert(caught15.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught15.failedCodeFileName === Some(fileName))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    def `should do nothing if LHS is true for either specified RHS, when used in a logical-or expression` { 
      aTaleOfTwoCities should (be (an [Book]) or be (an [Book]))
      aTaleOfTwoCities should (be (an [String]) or be (an [Book]))
      aTaleOfTwoCities should (be (an [Book]) or be (an [String]))
      
      aTaleOfTwoCities should (be (aTaleOfTwoCities) or be (an [Book]))
      aTaleOfTwoCities should (be (aTaleOfThreeCities) or be (an [Book]))
      aTaleOfTwoCities should (be (aTaleOfTwoCities) or be (an [String]))
      
      aTaleOfTwoCities should (be (an [Book]) or be (aTaleOfTwoCities))
      aTaleOfTwoCities should (be (an [String]) or be (aTaleOfTwoCities))
      aTaleOfTwoCities should (be (an [Book]) or be (aTaleOfThreeCities))
      
      aTaleOfTwoCities should (equal (aTaleOfTwoCities) or be (an [Book]))
      aTaleOfTwoCities should (equal (aTaleOfThreeCities) or be (an [Book]))
      aTaleOfTwoCities should (equal (aTaleOfTwoCities) or be (an [String]))
      
      aTaleOfTwoCities should (be (an [Book]) or equal (aTaleOfTwoCities))
      aTaleOfTwoCities should (be (an [String]) or equal (aTaleOfTwoCities))
      aTaleOfTwoCities should (be (an [Book]) or equal (aTaleOfThreeCities))
    }

    def `should throw TestFailedException LHS is false for both specified RHS, when used in a logical-or expression` {
      val caught1 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) or be (an [String]))
      }
      assert(caught1.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String]) + ", and " + wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught1.failedCodeFileName === Some(fileName))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught2 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (aTaleOfThreeCities) or be (an [String]))
      }
      assert(caught2.message === Some(wasNotEqualTo(aTaleOfTwoCities, aTaleOfThreeCities) + ", and " + wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught2.failedCodeFileName === Some(fileName))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) or be (aTaleOfThreeCities))
      }
      assert(caught3.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String]) + ", and " + wasNotEqualTo(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught3.failedCodeFileName === Some(fileName))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (equal (aTaleOfThreeCities) or be (an [String]))
      }
      assert(caught4.message === Some(didNotEqual(aTaleOfTwoCities, aTaleOfThreeCities) + ", and " + wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String])))
      assert(caught4.failedCodeFileName === Some(fileName))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught5 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (be (an [String]) or equal (aTaleOfThreeCities))
      }
      assert(caught5.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String]) + ", and " + didNotEqual(aTaleOfTwoCities, aTaleOfThreeCities)))
      assert(caught5.failedCodeFileName === Some(fileName))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    def `should do nothing if should do nothing if LHS is false for both specified RHS, when used in a logical-and expression with not` { 
      
      aTaleOfTwoCities should (not be an [String] and not be an [String])
      aTaleOfTwoCities should (not be aTaleOfThreeCities and not be an [String])
      aTaleOfTwoCities should (not be an [String] and not be aTaleOfThreeCities)
      aTaleOfTwoCities should (not equal aTaleOfThreeCities and not be an [String])
      aTaleOfTwoCities should (not be an [String] and not equal aTaleOfThreeCities)
      
    }

    def `should throw TestFailedException if LHS true for either specified RHS, when used in a logical-and expression with not` { 
      val caught1 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [Book] and not be an [String])
      }
      assert(caught1.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught1.failedCodeFileName === Some(fileName))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught2 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [String] and not be an [Book])
      }
      assert(caught2.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String]) + ", but " + wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught2.failedCodeFileName === Some(fileName))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [Book] and not be an [Book])
      }
      assert(caught3.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught3.failedCodeFileName === Some(fileName))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be aTaleOfTwoCities and not be an [String])
      }
      assert(caught4.message === Some(wasEqualTo(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught4.failedCodeFileName === Some(fileName))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught5 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be aTaleOfThreeCities and not be an [Book])
      }
      assert(caught5.message === Some(wasNotEqualTo(aTaleOfTwoCities, aTaleOfThreeCities) + ", but " + wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught5.failedCodeFileName === Some(fileName))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught6 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be aTaleOfTwoCities and not be an [Book])
      }
      assert(caught6.message === Some(wasEqualTo(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught6.failedCodeFileName === Some(fileName))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught7 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [Book] and not be aTaleOfThreeCities)
      }
      assert(caught7.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught7.failedCodeFileName === Some(fileName))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught8 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [String] and not be aTaleOfTwoCities)
      }
      assert(caught8.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String]) + ", but " + wasEqualTo(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught8.failedCodeFileName === Some(fileName))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught9 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [Book] and not be aTaleOfTwoCities)
      }
      assert(caught9.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught9.failedCodeFileName === Some(fileName))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught10 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not equal aTaleOfTwoCities and not be an [String])
      }
      assert(caught10.message === Some(equaled(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught10.failedCodeFileName === Some(fileName))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught11 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not equal aTaleOfThreeCities and not be an [Book])
      }
      assert(caught11.message === Some(didNotEqual(aTaleOfTwoCities, aTaleOfThreeCities) + ", but " + wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught11.failedCodeFileName === Some(fileName))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught12 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not equal aTaleOfTwoCities and not be an [Book])
      }
      assert(caught12.message === Some(equaled(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught12.failedCodeFileName === Some(fileName))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught13 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [Book] and not equal aTaleOfThreeCities)
      }
      assert(caught13.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught13.failedCodeFileName === Some(fileName))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
    
      val caught14 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [String] and not equal aTaleOfTwoCities)
      }
      assert(caught14.message === Some(wasNotAnInstanceOf(aTaleOfTwoCities, classOf[String]) + ", but " + equaled(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught14.failedCodeFileName === Some(fileName))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught15 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be an [Book] and not equal aTaleOfTwoCities)
      }
      assert(caught15.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught15.failedCodeFileName === Some(fileName))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    def `should do nothing if LHS is false for either specified RHS, when used in a logical-or expression with not` { 
      aTaleOfTwoCities should (not be an [String] or not be an [String])
      aTaleOfTwoCities should (not be an [Book] or not be an [String])
      aTaleOfTwoCities should (not be an [String] or not be an [Book])
      
      aTaleOfTwoCities should (not be aTaleOfThreeCities or not be an [String])
      aTaleOfTwoCities should (not be aTaleOfTwoCities or not be an [String])
      aTaleOfTwoCities should (not be aTaleOfThreeCities or not be an [Book])
      
      aTaleOfTwoCities should (not be an [String] or not be aTaleOfThreeCities)
      aTaleOfTwoCities should (not be an [Book] or not be aTaleOfThreeCities)
      aTaleOfTwoCities should (not be an [String] or not be aTaleOfTwoCities)
      
      aTaleOfTwoCities should (not equal aTaleOfThreeCities or not be an [String])
      aTaleOfTwoCities should (not equal aTaleOfTwoCities or not be an [String])
      aTaleOfTwoCities should (not equal aTaleOfThreeCities or not be an [Book])
      
      aTaleOfTwoCities should (not be an [String] or not equal aTaleOfThreeCities)
      aTaleOfTwoCities should (not be an [Book] or not equal aTaleOfThreeCities)
      aTaleOfTwoCities should (not be an [String] or not equal (aTaleOfTwoCities))
    }

    def `should throw TestFailedException if LHS is true both specified RHS, when used in a logical-or expression with not` { 
      val caught1 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be a [Book] or not be a [Book])
      }
      assert(caught1.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book]) + ", and " + wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught1.failedCodeFileName === Some(fileName))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught2 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be aTaleOfTwoCities or not be a [Book])
      }
      assert(caught2.message === Some(wasEqualTo(aTaleOfTwoCities, aTaleOfTwoCities) + ", and " + wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught2.failedCodeFileName === Some(fileName))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be a [Book] or not be aTaleOfTwoCities)
      }
      assert(caught3.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book]) + ", and " + wasEqualTo(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught3.failedCodeFileName === Some(fileName))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not equal aTaleOfTwoCities or not be a [Book])
      }
      assert(caught4.message === Some(equaled(aTaleOfTwoCities, aTaleOfTwoCities) + ", and " + wasAnInstanceOf(aTaleOfTwoCities, classOf[Book])))
      assert(caught4.failedCodeFileName === Some(fileName))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught5 = intercept[exceptions.TestFailedException] {
        aTaleOfTwoCities should (not be a [Book] or not equal aTaleOfTwoCities)
      }
      assert(caught5.message === Some(wasAnInstanceOf(aTaleOfTwoCities, classOf[Book]) + ", and " + equaled(aTaleOfTwoCities, aTaleOfTwoCities)))
      assert(caught5.failedCodeFileName === Some(fileName))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    // TODO: to uncomment these tests after TypeMatcherMacro raises compiler error instead of warning

    def `should do nothing if the LHS is an instance of specified RHS with _ type parameter` {
      List(Book("Book 1"), Book("Book 2")) should be (an [List[_]])
      List(Book("Book 1"), Book("Book 2")) shouldBe an [List[_]]
    }

    /*def `should not compile when LHS is an instance of specified RHS with type parameter ` {
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should be (an [List[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) shouldBe an [List[Book]]" shouldNot compile
    }*/

    def `should do nothing if LHS is not an instance of specified RHS with _ type parameter` {
      Book("Book 1") should not be an [List[_]]
      Book("Book 1") shouldNot be (an [List[_]])
    }

    /*def `should not compile when LHS is not an instance of specified RHS with type parameter` {
      "Book(\"Book 1\") should not be an [List[Book]]" shouldNot compile
      "Book(\"Book 1\") shouldNot be (an [List[Book]])" shouldNot compile
    }*/

    def `should do nothing if LHS true for both specified RHS with _ type parameter, when used in a logical-and expression` {
      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) and be (an [List[_]]))
      List(Book("Book 1"), Book("Book 2")) should (be (List(Book("Book 1"), Book("Book 2"))) and be (an [List[_]]))
      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) and be (List(Book("Book 1"), Book("Book 2"))))
      List(Book("Book 1"), Book("Book 2")) should (equal (List(Book("Book 1"), Book("Book 2"))) and be (an [List[_]]))
      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) and equal (List(Book("Book 1"), Book("Book 2"))))
    }

    /*def `should not compile when LHS is true for both specified RHS with type parameter` {
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) and be (an [List[Book]]))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (List(Book(\"Book 1\"), Book(\"Book 2\"))) and be (an [List[Book]]))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) and be (List(Book(\"Book 1\"), Book(\"Book 2\"))))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (equal (List(Book(\"Book 1\"), Book(\"Book 2\"))) and be (an [List[Book]]))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) and equal (List(Book(\"Book 1\"), Book(\"Book 2\"))))" shouldNot compile
    }*/

    def `should do nothing if LHS is true for either specified RHS with _ type parameter, when used in a logical-or expression` {
      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) or be (an [List[_]]))
      List(Book("Book 1"), Book("Book 2")) should (be (an [String]) or be (an [List[_]]))
      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) or be (an [String]))

      List(Book("Book 1"), Book("Book 2")) should (be (List(Book("Book 1"), Book("Book 2"))) or be (an [List[_]]))
      List(Book("Book 1"), Book("Book 2")) should (be (aTaleOfThreeCities) or be (an [List[_]]))

      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) or be (List(Book("Book 1"), Book("Book 2"))))
      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) or be (aTaleOfThreeCities))

      List(Book("Book 1"), Book("Book 2")) should (equal (List(Book("Book 1"), Book("Book 2"))) or be (an [List[_]]))
      List(Book("Book 1"), Book("Book 2")) should (equal (aTaleOfThreeCities) or be (an [List[_]]))

      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) or equal (List(Book("Book 1"), Book("Book 2"))))
      List(Book("Book 1"), Book("Book 2")) should (be (an [List[_]]) or equal (aTaleOfThreeCities))
    }

    /*def `should not compile if LHS is true for either specified RHS with type parameter, when used in a logical-or expression` {
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) or be (an [List[Book]]))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [String]) or be (an [List[Book]]))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) or be (an [String]))" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (List(Book(\"Book 1\"), Book(\"Book 2\"))) or be (an [List[Book]]))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (aTaleOfThreeCities) or be (an [List[Book]]))" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) or be (List(Book(\"Book 1\"), Book(\"Book 2\"))))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) or be (aTaleOfThreeCities))" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (equal (List(Book(\"Book 1\"), Book(\"Book 2\"))) or be (an [List[Book]]))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (equal (aTaleOfThreeCities) or be (an [List[Book]]))" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) or equal (List(Book(\"Book 1\"), Book(\"Book 2\"))))" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (be (an [List[Book]]) or equal (aTaleOfThreeCities))" shouldNot compile
    }*/

    def `should do nothing if LHS is false for both specified RHS with _ type parameter, when used in a logical-and expression with not` {

      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] and not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not be aTaleOfThreeCities and not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] and not be aTaleOfThreeCities)
      List(Book("Book 1"), Book("Book 2")) should (not equal aTaleOfThreeCities and not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] and not equal aTaleOfThreeCities)

    }

    /*def `should not compile if LHS is false for both specified RHS with type parameter, when used in a logical-and expression with not ` {

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] and not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be aTaleOfThreeCities and not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] and not be aTaleOfThreeCities)" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not equal aTaleOfThreeCities and not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] and not equal aTaleOfThreeCities)" shouldNot compile

    }*/

    def `should do nothing if LHS is false for either specified RHS with _ type parameter, when used in a logical-or expression with not` {
      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] or not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not be an [List[_]] or not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] or not be an [List[_]])

      List(Book("Book 1"), Book("Book 2")) should (not be aTaleOfThreeCities or not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not be List(Book("Book 1"), Book("Book 2")) or not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not be aTaleOfThreeCities or not be an [List[_]])

      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] or not be aTaleOfThreeCities)
      List(Book("Book 1"), Book("Book 2")) should (not be an [List[_]] or not be aTaleOfThreeCities)
      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] or not be List(Book("Book 1"), Book("Book 2")))

      List(Book("Book 1"), Book("Book 2")) should (not equal aTaleOfThreeCities or not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not equal List(Book("Book 1"), Book("Book 2")) or not be an [Vector[_]])
      List(Book("Book 1"), Book("Book 2")) should (not equal aTaleOfThreeCities or not be an [List[_]])

      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] or not equal aTaleOfThreeCities)
      List(Book("Book 1"), Book("Book 2")) should (not be an [List[_]] or not equal aTaleOfThreeCities)
      List(Book("Book 1"), Book("Book 2")) should (not be an [Vector[_]] or not equal (List(Book("Book 1"), Book("Book 2"))))
    }

    /*def `should not compile if LHS is false for either specified RHS with type parameter, when used in a logical-or expression with not` {
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] or not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [List[Book]] or not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] or not be an [List[Book]])" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be aTaleOfThreeCities or not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be List(Book(\"Book 1\"), Book(\"Book 2\")) or not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be aTaleOfThreeCities or not be an [List[Book]])" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] or not be aTaleOfThreeCities)" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [List[Book]] or not be aTaleOfThreeCities)" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] or not be List(Book(\"Book 1\"), Book(\"Book 2\")))" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not equal aTaleOfThreeCities or not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not equal List(Book(\"Book 1\"), Book(\"Book 2\")) or not be an [Vector[Book]])" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not equal aTaleOfThreeCities or not be an [List[Book]])" shouldNot compile

      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] or not equal aTaleOfThreeCities)" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [List[Book]] or not equal aTaleOfThreeCities)" shouldNot compile
      "List(Book(\"Book 1\"), Book(\"Book 2\")) should (not be an [Vector[Book]] or not equal (List(Book(\"Book 1\"), Book(\"Book 2\"))))" shouldNot compile
    }*/
  }
}