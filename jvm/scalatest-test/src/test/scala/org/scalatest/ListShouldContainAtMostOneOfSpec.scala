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

import org.scalactic.Equality
import org.scalactic.Uniformity
import org.scalactic.Prettifier
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ListShouldContainAtMostOneOfSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }
  
  private def upperCase(value: Any): Any = 
    value match {
      case l: List[_] => l.map(upperCase(_))
      case s: String => s.toUpperCase
      case c: Char => c.toString.toUpperCase.charAt(0)
      case (s1: String, s2: String) => (s1.toUpperCase, s2.toUpperCase)
      case e: java.util.Map.Entry[_, _] => 
        (e.getKey, e.getValue) match {
          case (k: String, v: String) => Entry(k.toUpperCase, v.toUpperCase)
          case _ => value
        }
      case _ => value
    }
  
  //ADDITIONAL//

  describe("a List") {

    val fumList: List[String] = List("fum", "foe")
    val toList: List[String] = List("to", "you")
    val ecList: List[String] = List("\u0000fix", "fum", "foe")

    describe("when used with contain atMostOneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should contain atMostOneOf ("fee", "fie", "foe", "fam")
        val e1 = intercept[TestFailedException] {
          fumList should contain atMostOneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList should contain atMostOneOf ("FEE", "FIE", "FOE", "FAM")
        intercept[TestFailedException] {
          fumList should contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should contain atMostOneOf ("FEE", "FIE", "FOE", "FAM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          (fumList should contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
          
        }
        fumList should contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ")
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain atMostOneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should contain atMostOneOf ("fee", "fie", "foe", "fum")
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: \"\\u0000fix\""))
      }
    }

    describe("when used with (contain atMostOneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        fumList should (contain atMostOneOf ("fee", "fie", "foe", "fam"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))
        intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          (fumList should (contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        }
        fumList should (contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atMostOneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with not contain atMostOneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should not contain atMostOneOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should not contain atMostOneOf ("fee", "fie", "foe", "fam")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList should not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should not contain atMostOneOf ("FEE", "FIE", "FOE", "FAM")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should not contain atMostOneOf ("FEE", "FIE", "FOE", "FAM")) (decided by upperCaseStringEquality)
        }
        (fumList should not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList should not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should not contain atMostOneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with (not contain atMostOneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain atMostOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atMostOneOf ("fee", "fie", "foe", "fam"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        }
        (fumList should (not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList should (not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atMostOneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with shouldNot contain atMostOneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList shouldNot contain atMostOneOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList shouldNot contain atMostOneOf ("fee", "fie", "foe", "fam")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList shouldNot contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList shouldNot contain atMostOneOf ("FEE", "FIE", "FOE", "FAM")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList shouldNot contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList shouldNot contain atMostOneOf ("FEE", "FIE", "FOE", "FAM")) (decided by upperCaseStringEquality)
        }
        (fumList shouldNot contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList shouldNot contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList shouldNot contain atMostOneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with shouldNot (contain atMostOneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList shouldNot (contain atMostOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList shouldNot (contain atMostOneOf ("fee", "fie", "foe", "fam"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList shouldNot (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList shouldNot (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList shouldNot (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList shouldNot (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        }
        (fumList shouldNot (contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList shouldNot (contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList shouldNot (contain atMostOneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
  }

  describe("a collection of Lists") {

    val list1s: Vector[List[Int]] = Vector(List(1, 2), List(1, 2), List(1, 2))
    val lists: Vector[List[Int]] = Vector(List(1, 2), List(1, 2), List(2, 3))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1, 2), List(1, 2), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "he"), List("hi", "he"), List("hi", "he"))
    val toLists: Vector[List[String]] = Vector(List("to", "you"), List("to", "you"), List("to", "you"))

    describe("when used with contain atMostOneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should contain atMostOneOf (1, 3, 4)
        atLeast (2, lists) should contain atMostOneOf (2, 3, 4)
        atMost (2, lists) should contain atMostOneOf (2, 3, 4)
        no (lists) should contain atMostOneOf (1, 2, 3)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain atMostOneOf (2, 3, 4)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(prettifier, lists(2)) + " did not contain at most one of (2, 3, 4) (ListShouldContainAtMostOneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should contain atMostOneOf ("hi", "ho")
        intercept[TestFailedException] {
          all (hiLists) should contain atMostOneOf ("hi", "he")
        }

        {
          implicit val use = upperCaseStringEquality
          all (hiLists) should contain atMostOneOf ("HI", "HO")
          intercept[TestFailedException] {
            all (hiLists) should contain atMostOneOf ("HI", "HE")
          }
        }
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should contain atMostOneOf ("hi", "ho")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain atMostOneOf ("hi", "he")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain atMostOneOf ("hi", "HE")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain atMostOneOf ("hi", "he")) (decided by defaultEquality[String])
        }
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should contain atMostOneOf (1, 2, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with (contain atMostOneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain atMostOneOf (1, 3, 4))
        atLeast (2, lists) should (contain atMostOneOf (2, 3, 4))
        atMost (2, lists) should (contain atMostOneOf (2, 3, 4))
        no (lists) should (contain atMostOneOf (1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atMostOneOf (2, 3, 4))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(prettifier, lists(2)) + " did not contain at most one of (2, 3, 4) (ListShouldContainAtMostOneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should (contain atMostOneOf ("hi", "ho"))
        intercept[TestFailedException] {
          all (hiLists) should (contain atMostOneOf ("hi", "he"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiLists) should (contain atMostOneOf ("HI", "HO"))
          intercept[TestFailedException] {
            all (hiLists) should (contain atMostOneOf ("HI", "HE"))
          }
        }
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain atMostOneOf ("hi", "ho"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain atMostOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain atMostOneOf ("hi", "HE"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain atMostOneOf ("hi", "he"))) (decided by defaultEquality[String])
        }
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atMostOneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with not contain atMostOneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should not contain atMostOneOf ("happy", "birthday", "to", "you")
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain atMostOneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one of (\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        intercept[TestFailedException] {
          all (toLists) should not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")
        }
      }
      
      it("should use an explicitly provided Equality") {
        (all (toLists) should not contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        }
        (all (toLists) should not contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should not contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        }
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain atMostOneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with (not contain atMostOneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should (not contain atMostOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain atMostOneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one of (\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should (not contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        (all (toLists) should (not contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should (not contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain atMostOneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with shouldNot contain atMostOneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot contain atMostOneOf ("happy", "birthday", "to", "you")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain atMostOneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one of (\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")
        }
      }
      
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        }
        (all (toLists) shouldNot contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) shouldNot contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        }
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain atMostOneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with shouldNot (contain atMostOneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot (contain atMostOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain atMostOneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one of (\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot (contain atMostOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        (all (toLists) shouldNot (contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain atMostOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain atMostOneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
  }
}
