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
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.TestFailedException

class OptionShouldContainOneElementOfSpec extends FunSpec {

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val upperCaseEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  describe("an Option") {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    describe("when used with contain oneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumSome should contain oneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumSome should contain oneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainOneElementOf(fumSome, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumSome should contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumSome should contain oneElementOf Seq("fee", "fie", "foe", "fum")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumSome should contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumSome should contain oneElementOf Seq("fee", "fie", "foe", "fum")) (decided by upperCaseEquality)
        }
        intercept[TestFailedException] {
          fumSome should contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumSome should contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
      it("should allow RHS to contain duplicated value") {
        fumSome should contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum")
      }
    }

    describe("when used with (contain oneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainOneElementOf(fumSome, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseEquality)
        }
        intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumSome should (contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      it("should allow RHS to contain duplicated value") {
        fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    /*
     I purposely don't want to support this syntax:

            fumSome should contain (oneElementOf Seq("fee", "fie", "foe", "fum"))
            fumSome should (contain (oneElementOf Seq("fee", "fie", "foe", "fum")))

     Reason is that I don't want people putting parentheses between contain and oneOf, etc. This will not compile.
    */
    describe("when used with not contain oneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should not contain oneElementOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toSome should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedOneElementOf(toSome, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toSome should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        intercept[TestFailedException] {
          toSome should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toSome should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toSome should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        }
        toSome should not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toSome should not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toSome should not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    /*
    Interesting, of these three, the top one does happen to compile and run:

            toSome should not contain (oneElementOf Seq("fee", "fie", "foe", "fum"))
            // toSome should not (contain (oneElementOf Seq("fee", "fie", "foe", "fum")))
            // toSome should (not (contain (oneElementOf Seq("fee", "fie", "foe", "fum"))))

    The bottom two don't, but still I don't want to support that in general.
    */
    describe("when used with (not contain oneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedOneElementOf(toSome, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      it("should use an explicitly provided Equality") {
        (toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseEquality)
        }
        toSome should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (toSome should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }
  }

  describe("a collection of Options") {

    val some1s: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(1))
    val somes: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(2))
    val nones: Vector[Option[Int]] = Vector(None, None, None)
    val somesNone: Vector[Option[Int]] = Vector(Some(1), Some(1), None)
    val hiSomes: Vector[Option[String]] = Vector(Some("hi"), Some("hi"), Some("hi"))
    val toSomes: Vector[Option[String]] = Vector(Some("to"), Some("to"), Some("to"))

    describe("when used with contain oneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should contain oneElementOf Seq(1, 3, 4)
        atLeast (2, somes) should contain oneElementOf Seq(1, 3, 4)
        atMost (2, somes) should contain oneElementOf Seq(2, 3, 4)
        no (somes) should contain oneElementOf Seq(3, 4, 5)
        no (nones) should contain oneElementOf Seq(1, 3, 4)
        no (somesNone) should contain oneElementOf Seq(3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (somes) should contain oneElementOf Seq(1, 3, 4)
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(somes(2), Seq(1, 3, 4)) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(Some(1), Some(1), Some(2))"))

        val e2 = intercept[TestFailedException] {
          all (nones) should contain oneElementOf Seq("ho", "hey", "howdy")
        }
        e2.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.didNotContainOneElementOf(nones(0), Seq("ho", "hey", "howdy")) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(None, None, None)"))

        val e4 = intercept[TestFailedException] {
          all (somesNone) should contain oneElementOf Seq(1, 3, 4)
        }
        e4.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(somesNone(2), Seq(1, 3, 4)) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(Some(1), Some(1), None)"))
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should contain oneElementOf Seq("hi", "he")
        intercept[TestFailedException] {
          all (hiSomes) should contain oneElementOf Seq("ho", "he")
        }
        implicit val ise = upperCaseEquality
        all (hiSomes) should contain oneElementOf Seq("HI", "HE")
        intercept[TestFailedException] {
          all (hiSomes) should contain oneElementOf Seq("hi", "he")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should contain oneElementOf Seq("HI", "HE")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiSomes) should contain oneElementOf Seq("hi", "he")) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiSomes) should contain oneElementOf Seq("hi", "he")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiSomes) should contain oneElementOf Seq("HI", "HE")) (decided by defaultEquality[String])
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (some1s) should contain oneElementOf Seq(1, 2, 2, 3)
      }
    }

    describe("when used with (contain oneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should (contain oneElementOf Seq(1, 3, 4))
        atLeast (2, somes) should (contain oneElementOf Seq(1, 3, 4))
        atMost (2, somes) should (contain oneElementOf Seq(2, 3, 4))
        no (somes) should (contain oneElementOf Seq(3, 4, 5))
        no (nones) should (contain oneElementOf Seq(1, 3, 4))
        no (somesNone) should (contain oneElementOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (somes) should (contain oneElementOf Seq(1, 3, 4))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(somes(2), Seq(1, 3, 4)) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(Some(1), Some(1), Some(2))"))

        val e2 = intercept[TestFailedException] {
          all (nones) should (contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        e2.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.didNotContainOneElementOf(nones(0), Seq("ho", "hey", "howdy")) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(None, None, None)"))

        val e4 = intercept[TestFailedException] {
          all (somesNone) should (contain oneElementOf Seq(1, 3, 4))
        }
        e4.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages.didNotContainOneElementOf(somesNone(2), Seq(1, 3, 4)) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(Some(1), Some(1), None)"))
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should (contain oneElementOf Seq("hi", "he"))
        intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("HI", "HE"))
        }
        implicit val ise = upperCaseEquality
        all (hiSomes) should (contain oneElementOf Seq("HI", "HE"))
        intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("hi", "he"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should (contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiSomes) should (contain oneElementOf Seq("hi", "he"))) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiSomes) should (contain oneElementOf Seq("hi", "he"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiSomes) should (contain oneElementOf Seq("HI", "HE"))) (decided by defaultEquality[String])
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (some1s) should (contain oneElementOf Seq(1, 2, 2, 3))
      }
    }

    /*
     I purposely don't want to support this syntax:

    scala> all (some1s) should contain (oneElementOf Seq(1, 3, 4))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (some1s) should contain (oneElementOf Seq(1, 3, 4))
                                                 ^

    scala> all (some1s) should (contain (oneElementOf Seq(1, 3, 4)))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (some1s) should (contain (oneElementOf Seq(1, 3, 4)))
                                                  ^

     Reason is that I don't want people putting parentheses between contain and oneOf, etc. This will not compile.
    */
    describe("when used with not contain oneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should not contain oneElementOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.containedOneElementOf(toSomes(0), Seq("happy", "birthday", "to", "you")) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(Some(\"to\"), Some(\"to\"), Some(\"to\"))"))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toSomes) should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))
        intercept[TestFailedException] {
          all (toSomes) should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should not contain oneElementOf (Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toSomes) should not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        }
        all (toSomes) should not contain oneElementOf (Seq(" happy ", " birthday ", " to ", " you "))
        intercept[TestFailedException] {
          (all (toSomes) should not contain oneElementOf (Seq(" happy ", " birthday ", " to ", " you "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toSomes) should not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    /*
    Interesting, of these three, the last one does happen to compile and run:

    scala> all (toSomes) should (not (contain (oneElementOf Seq("fee", "fie", "foe", "fum"))))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (toSomes) should (not (contain (oneElementOf Seq("fee", "fie", "foe", "fum"))))
                                                        ^

    scala> all (toSomes) should not (contain (oneElementOf Seq("fee", "fie", "foe", "fum")))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (toSomes) should not (contain (oneElementOf Seq("fee", "fie", "foe", "fum")))
                                                       ^

    scala> all (toSomes) should not contain (oneElementOf Seq("fee", "fie", "foe", "fum"))

    The top two don't, but still I don't want to support that in general.
    */
    describe("when used with (not contain oneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages.containedOneElementOf(toSomes(0), Seq("happy", "birthday", "to", "you")) + " (OptionShouldContainOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Vector(Some(\"to\"), Some(\"to\"), Some(\"to\"))"))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseEquality
        all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseEquality)
        }
        all (toSomes) should (not contain oneElementOf (Seq(" happy ", " birthday ", " to ", " you ")))
        intercept[TestFailedException] {
          (all (toSomes) should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toSomes) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }
  }
}
