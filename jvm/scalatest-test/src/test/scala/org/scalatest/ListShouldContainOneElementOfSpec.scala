/*
 * Copyright 2001-2025 Artima, Inc.
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
import exceptions.NotAllowedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ListShouldContainOneElementOfSpec extends AnyFunSpec with Matchers {

  private val prettifier = Prettifier.default

  val upperCaseEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  //ADDITIONAL//

  describe("a List") {

    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    val fumfuList: List[String] = List("fum", "fu")
    val ecList: List[String] = List("\u0000fum")

    describe("when used with contain oneElementOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should contain oneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should contain oneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")))
        // Here it contains two of, not one of
        val e2 = intercept[TestFailedException] {
          fumfuList should contain oneElementOf Seq("fee", "fum", "foe", "fu")
        }
        e2.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message.get should be (FailureMessages.didNotContainOneElementOf(prettifier, fumfuList, Seq("fee", "fum", "foe", "fu")))
        // Contains duplicate elements in the right list
        fumList should contain oneElementOf Seq("fee", "fum", "foe", "fum")
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumList should contain oneElementOf Seq("FEE", "FUM", "FOE", "FU")
        intercept[TestFailedException] {
          fumList should contain oneElementOf Seq("fee", "fum", "foe", "fu")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should contain oneElementOf Seq("FEE", "FUM", "FOE", "FU")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should contain oneElementOf Seq("fee", "fum", "foe", "fu")) (decided by upperCaseEquality)
        }
        intercept[TestFailedException] {
          fumList should contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
      it("should allow RHS to contain duplicated value") {
        fumList should contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum")
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should contain oneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: \"\\u0000fum\""))
      }
    }

    describe("when used with (contain oneElementOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumList should (contain oneElementOf Seq("FEE", "FUM", "FOE", "FU"))
        intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("fee", "fum", "foe", "fu"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (contain oneElementOf Seq("FEE", "FUM", "FOE", "FU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should (contain oneElementOf Seq("fee", "fum", "foe", "fu"))) (decided by upperCaseEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      it("should allow RHS to contain duplicated value") {
        fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should (contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: \"\\u0000fum\""))
      }
    }

    /*
     I purposely don't want to support this syntax:

            fumList should contain (oneElementOf ("fee", "fie", "foe", "fum"))
            fumList should (contain (oneElementOf ("fee", "fie", "foe", "fum")))

     Reason is that I don't want people putting parentheses between contain and oneElementOf, etc. This will not compile.
    */
    describe("when used with not contain oneElementOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList should not contain oneElementOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toList should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedOneElementOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toList should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        intercept[TestFailedException] {
          toList should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toList should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        }
        toList should not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList should not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    /*
    Interesting, of these three, the top one does happen to compile and run:

            toList should not contain (oneElementOf ("fee", "fie", "foe", "fum"))
            // toList should not (contain (oneElementOf ("fee", "fie", "foe", "fum")))
            // toList should (not (contain (oneElementOf ("fee", "fie", "foe", "fum"))))

    The bottom two don't, but still I don't want to support that in general.
    */
    describe("when used with (not contain oneElementOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          toList should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedOneElementOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toList should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        intercept[TestFailedException] {
          toList should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toList should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseEquality)
        }
        toList should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (toList should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with shouldNot contain oneElementOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList shouldNot contain oneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain oneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedOneElementOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toList shouldNot contain oneElementOf Seq("happy", "birthday", "to", "you")
        intercept[TestFailedException] {
          toList shouldNot contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      it("should use an explicitly provided Equality") {
        (toList shouldNot contain oneElementOf Seq("happy", "birthday", "to", "you")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        }
        toList shouldNot contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList shouldNot contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList shouldNot contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum")
      }
    }

    describe("when used with shouldNot (contain oneElementOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList shouldNot (contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedOneElementOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toList shouldNot (contain oneElementOf Seq("happy", "birthday", "to", "you"))
        intercept[TestFailedException] {
          toList shouldNot (contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList shouldNot (contain oneElementOf Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        }
        toList shouldNot (contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot (contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList shouldNot (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
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

    describe("when used with contain oneElementOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should contain oneElementOf Seq(1, 3, 4)
        atLeast (2, lists) should contain oneElementOf Seq(1, 3, 4)
        atMost (2, lists) should contain oneElementOf Seq(2, 3, 4)
        no (lists) should contain oneElementOf Seq(3, 4, 5)
        no (nils) should contain oneElementOf Seq(1, 3, 4)
        no (listsNil) should contain oneElementOf Seq(3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain oneElementOf Seq(1, 3, 4)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(prettifier, lists(2), Seq(1, 3, 4)) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, lists)))

        val e2 = intercept[TestFailedException] {
          all (nils) should contain oneElementOf Seq("ho", "hey", "howdy")
        }
        e2.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.didNotContainOneElementOf(prettifier, nils(0), Seq("ho", "hey", "howdy")) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, nils)))

        val e4 = intercept[TestFailedException] {
          all (listsNil) should contain oneElementOf Seq(1, 3, 4)
        }
        e4.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(prettifier, listsNil(2), Seq(1, 3, 4)) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, listsNil)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should contain oneElementOf Seq("hi", "he")
        intercept[TestFailedException] {
          all (hiLists) should contain oneElementOf Seq("ho", "he")
        }

        {
          implicit val ise = upperCaseEquality
          all (hiLists) should contain oneElementOf Seq("HI", "HE")
          intercept[TestFailedException] {
            all (hiLists) should contain oneElementOf Seq("hi", "he")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should contain oneElementOf Seq("HI", "HE")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain oneElementOf Seq("hi", "he")) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should contain oneElementOf Seq("hi", "he")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain oneElementOf Seq("ho", "he")) (decided by defaultEquality[String])
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (list1s) should contain oneElementOf Seq(1, 2, 2, 3)
      }
    }

    describe("when used with (contain oneElementOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain oneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (contain oneElementOf Seq(1, 3, 4))
        atMost (2, lists) should (contain oneElementOf Seq(2, 3, 4))
        no (lists) should (contain oneElementOf Seq(3, 4, 5))
        no (nils) should (contain oneElementOf Seq(1, 3, 4))
        no (listsNil) should (contain oneElementOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain oneElementOf Seq(1, 3, 4))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(prettifier, lists(2), Seq(1, 3, 4)) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, lists)))

        val e2 = intercept[TestFailedException] {
          all (nils) should (contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        e2.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.didNotContainOneElementOf(prettifier, nils(0), Seq("ho", "hey", "howdy")) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, nils)))

        val e4 = intercept[TestFailedException] {
          all (listsNil) should (contain oneElementOf Seq(1, 3, 4))
        }
        e4.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(prettifier, listsNil(2), Seq(1, 3, 4)) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, listsNil)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should (contain oneElementOf Seq("hi", "he"))
        intercept[TestFailedException] {
          all (hiLists) should (contain oneElementOf Seq("ho", "he"))
        }

        {
          implicit val ise = upperCaseEquality
          all (hiLists) should (contain oneElementOf Seq("HI", "HE"))
          intercept[TestFailedException] {
            all (hiLists) should (contain oneElementOf Seq("hi", "he"))
          }
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain oneElementOf Seq("hi", "he"))) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should (contain oneElementOf Seq("hi", "he"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain oneElementOf Seq("ho", "he"))) (decided by defaultEquality[String])
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (list1s) should (contain oneElementOf Seq(1, 2, 2, 3))
      }
    }

    /*
     I purposely don't want to support this syntax:

    scala> all (list1s) should contain (oneElementOf (1, 3, 4))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (list1s) should contain (oneElementOf (1, 3, 4))
                                                 ^

    scala> all (list1s) should (contain (oneElementOf (1, 3, 4)))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (list1s) should (contain (oneElementOf (1, 3, 4)))
                                                  ^

     Reason is that I don't want people putting parentheses between contain and oneElementOf, etc. This will not compile.
    */
    describe("when used with not contain oneElementOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should not contain oneElementOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.containedOneElementOf(prettifier, toLists(0), Seq("happy", "birthday", "to", "you")) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        intercept[TestFailedException] {
          all (toLists) should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        }
        all (toLists) should not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (all (toLists) should not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) should not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    /*
    Interesting, of these three, the last one does happen to compile and run:

    scala> all (toLists) should (not (contain (oneElementOf ("fee", "fie", "foe", "fum"))))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (toLists) should (not (contain (oneElementOf ("fee", "fie", "foe", "fum"))))
                                                        ^

    scala> all (toLists) should not (contain (oneElementOf ("fee", "fie", "foe", "fum")))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (toLists) should not (contain (oneElementOf ("fee", "fie", "foe", "fum")))
                                                       ^

    scala> all (toLists) should not contain (oneElementOf ("fee", "fie", "foe", "fum"))

    The top two don't, but still I don't want to support that in general.
    */
    describe("when used with (not contain oneElementOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.containedOneElementOf(prettifier, toLists(0), Seq("happy", "birthday", "to", "you")) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        intercept[TestFailedException] {
          all (toLists) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseEquality)
        }
        all (toLists) should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (all (toLists) should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with shouldNot contain oneElementOf (...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot contain oneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain oneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.containedOneElementOf(prettifier, toLists(0), Seq("happy", "birthday", "to", "you")) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) shouldNot contain oneElementOf Seq("happy", "birthday", "to", "you")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot contain oneElementOf Seq("happy", "birthday", "to", "you")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        }
        all (toLists) shouldNot contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) shouldNot contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum")
      }
    }

    describe("when used with shouldNot (contain oneElementOf (...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot (contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.containedOneElementOf(prettifier, toLists(0), Seq("happy", "birthday", "to", "you")) + " (ListShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toLists) shouldNot (contain oneElementOf Seq("happy", "birthday", "to", "you"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot (contain oneElementOf Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain oneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        }
        all (toLists) shouldNot (contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain oneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) shouldNot (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }
  }
}
