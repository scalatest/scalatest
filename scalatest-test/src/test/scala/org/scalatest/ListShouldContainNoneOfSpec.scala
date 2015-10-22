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
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.NotAllowedException
import exceptions.TestFailedException

class ListShouldContainNoneOfSpec extends FunSpec {

  val upperCaseEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }
  
  //ADDITIONAL//

  describe("a List") {

    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")

    describe("when used with contain noneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should contain noneOf ("fee", "fie", "foe", "fam")
        val e1 = intercept[TestFailedException] {
          fumList should contain noneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAtLeastOneOf(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""))
        // Contains duplicate elements in the right list
        val e2 = intercept[NotAllowedException] {
          fumList should contain noneOf ("fee", "fam", "foe", "fam")
        }
        e2.getMessage should be (Resources.noneOfDuplicate)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumList should contain noneOf ("FEE", "FAM", "FOE", "FU")
        assertThrows[TestFailedException] {
          fumList should contain noneOf ("FEE", "FUM", "FOE", "FU")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should contain noneOf ("FEE", "FAM", "FOE", "FU")) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (fumList should contain noneOf ("FEE", "FUM", "FOE", "FU")) (decided by upperCaseEquality)
        }
        assertThrows[TestFailedException] {
          (fumList should contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        }
        (fumList should contain noneOf (" FEE ", " FIE ", " FOE ", " FAM ")) (after being lowerCased and trimmed)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain noneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with (contain noneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        fumList should (contain noneOf ("fee", "fie", "foe", "fam"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAtLeastOneOf(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumList should (contain noneOf ("FEE", "FAM", "FOE", "FU"))
        assertThrows[TestFailedException] {
          fumList should (contain noneOf ("FEE", "FUM", "FOE", "FU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (contain noneOf ("FEE", "FAM", "FOE", "FU"))) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (fumList should (contain noneOf ("FEE", "FUM", "FOE", "FU"))) (decided by upperCaseEquality)
        }
        fumList should (contain noneOf (" FEE ", " FIE ", " FOE ", " FUM "))
        assertThrows[TestFailedException] {
          (fumList should (contain noneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain noneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with not contain noneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should not contain noneOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should not contain noneOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAtLeastOneOf(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumList should not contain noneOf ("FEE", "FIE", "FOE", "FUM")
        assertThrows[TestFailedException] {
          fumList should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should not contain noneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (fumList should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        }
        (fumList should not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          fumList should not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should not contain noneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with (not contain noneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList should (not contain noneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toList should (not contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAtLeastOneOf(decorateToStringValue(toList), "\"fee\", \"fie\", \"foe\", \"fum\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toList should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        assertThrows[TestFailedException] {
          toList should (not contain noneOf ("happy", "birthday", "to", "you"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (toList should (not contain noneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (toList should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          toList should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should (not contain noneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }
    
    describe("when used with shouldNot contain noneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList shouldNot contain noneOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList shouldNot contain noneOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAtLeastOneOf(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumList shouldNot contain noneOf ("FEE", "FIE", "FOE", "FUM")
        assertThrows[TestFailedException] {
          fumList shouldNot contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList shouldNot contain noneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (fumList shouldNot contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        }
        (fumList shouldNot contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          fumList shouldNot contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList shouldNot contain noneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with shouldNot (contain noneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList shouldNot (contain noneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAtLeastOneOf(decorateToStringValue(toList), "\"fee\", \"fie\", \"foe\", \"fum\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toList shouldNot (contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        assertThrows[TestFailedException] {
          toList shouldNot (contain noneOf ("happy", "birthday", "to", "you"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList shouldNot (contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (toList shouldNot (contain noneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (toList shouldNot (contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          toList shouldNot (contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot (contain noneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }
  }

  describe("a collection of Lists") {

    val list1s: Vector[List[Int]] = Vector(List(1), List(1), List(1))
    val lists: Vector[List[Int]] = Vector(List(1), List(1), List(2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1), List(1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi"), List("hi"), List("hi"))
    val toLists: Vector[List[String]] = Vector(List("to"), List("to"), List("to"))

    describe("when used with contain noneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should contain noneOf (2, 3, 4)
        atLeast (2, lists) should contain noneOf (8, 3, 4)
        atMost (2, lists) should contain noneOf (2, 3, 4)
        no (lists) should contain noneOf (1, 2, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain noneOf (2, 3, 4)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + FailureMessages.containedAtLeastOneOf(lists(2), UnquotedString("2, 3, 4")) + " (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should contain noneOf ("ho", "he")
        assertThrows[TestFailedException] {
          all (hiLists) should contain noneOf ("hi", "he")
        }
        implicit val ise = upperCaseEquality
        all (hiLists) should contain noneOf ("hi", "he")
        assertThrows[TestFailedException] {
          all (hiLists) should contain noneOf ("HI", "HE")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should contain noneOf ("hi", "he")) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (all (hiLists) should contain noneOf ("HI", "HE")) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should contain noneOf ("ho", "he")) (decided by defaultEquality[String])
        assertThrows[TestFailedException] {
          (all (hiLists) should contain noneOf ("hi", "he")) (decided by defaultEquality[String])
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should contain noneOf (1, 2, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with (contain noneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain noneOf (2, 3, 4))
        atLeast (2, lists) should (contain noneOf (2, 3, 4))
        atMost (2, lists) should (contain noneOf (2, 3, 4))
        no (lists) should (contain noneOf (1, 2, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain noneOf (2, 3, 4))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + FailureMessages.containedAtLeastOneOf(lists(2), UnquotedString("2, 3, 4")) + " (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should (contain noneOf ("ho", "he"))
        assertThrows[TestFailedException] {
          all (hiLists) should (contain noneOf ("hi", "he"))
        }
        implicit val ise = upperCaseEquality
        all (hiLists) should (contain noneOf ("hi", "he"))
        assertThrows[TestFailedException] {
          all (hiLists) should (contain noneOf ("HI", "HE"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain noneOf ("hi", "he"))) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (all (hiLists) should (contain noneOf ("HI", "HE"))) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should (contain noneOf ("ho", "he"))) (decided by defaultEquality[String])
        assertThrows[TestFailedException] {
          (all (hiLists) should (contain noneOf ("hi", "he"))) (decided by defaultEquality[String])
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain noneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with not contain noneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should not contain noneOf ("happy", "birthday", "to", "you")
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain noneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + FailureMessages.didNotContainAtLeastOneOf(toLists(0), UnquotedString("\"fee\", \"fie\", \"foe\", \"fum\"")) + " (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        assertThrows[TestFailedException] {
          all (toLists) should not contain noneOf ("happy", "birthday", "to", "you")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (all (toLists) should not contain noneOf ("happy", "birthday", "to", "you")) (decided by upperCaseEquality)
        }
        (all (toLists) should not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          all (toLists) should not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain noneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with (not contain noneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should (not contain noneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + FailureMessages.didNotContainAtLeastOneOf(toLists(0), UnquotedString("\"fee\", \"fie\", \"foe\", \"fum\"")) + " (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        assertThrows[TestFailedException] {
          all (toLists) should (not contain noneOf ("happy", "birthday", "to", "you"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (all (toLists) should (not contain noneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (all (toLists) should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          all (toLists) should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain noneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }
    
    describe("when used with shouldNot contain noneOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot contain noneOf ("happy", "birthday", "to", "you")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain noneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + FailureMessages.didNotContainAtLeastOneOf(toLists(0), UnquotedString("\"fee\", \"fie\", \"foe\", \"fum\"")) + " (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) shouldNot contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        assertThrows[TestFailedException] {
          all (toLists) shouldNot contain noneOf ("happy", "birthday", "to", "you")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (all (toLists) shouldNot contain noneOf ("happy", "birthday", "to", "you")) (decided by upperCaseEquality)
        }
        (all (toLists) shouldNot contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          all (toLists) shouldNot contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain noneOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }

    describe("when used with shouldNot (contain noneOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot (contain noneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + FailureMessages.didNotContainAtLeastOneOf(toLists(0), UnquotedString("\"fee\", \"fie\", \"foe\", \"fum\"")) + " (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) shouldNot (contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        assertThrows[TestFailedException] {
          all (toLists) shouldNot (contain noneOf ("happy", "birthday", "to", "you"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot  (contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        assertThrows[TestFailedException] {
          (all (toLists) shouldNot (contain noneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (all (toLists) shouldNot  (contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        assertThrows[TestFailedException] {
          all (toLists) shouldNot (contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain noneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.noneOfDuplicate))
      }
    }
  }
}
