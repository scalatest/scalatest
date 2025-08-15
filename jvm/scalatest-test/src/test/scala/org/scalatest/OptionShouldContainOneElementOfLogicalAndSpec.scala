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
import org.scalactic.Prettifier
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages._
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class OptionShouldContainOneElementOfLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val fileName = "OptionShouldContainOneElementOfLogicalAndSpec.scala"

  describe("an Option") {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    describe("when used with (contain oneElementOf Seq(...) and contain oneElementOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("fee", "fie", "fum", "foe"))

        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("happy", "birthday", "to", "you") and contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.containedOneElementOf(prettifier, fumSome, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))

        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, FailureMessages.containedOneElementOf(prettifier, fumSome, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain oneElementOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedOneElementOf(prettifier, fumSome, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum") and contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        fumSome should (contain oneElementOf Seq("fee", "fie", "fum", "foe") and contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (be (...) and contain oneElementOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumSome should (be (fumSome) and contain oneElementOf Seq("fee", "fie", "fum", "foe"))

        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) and contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumSome, toSome), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          fumSome should (be (fumSome) and contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumSome, fumSome) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        fumSome should (be (fumSome) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))

        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumSome, toSome), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          fumSome should (be (fumSome) and contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumSome, fumSome) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (fumSome should (be (fumSome) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (fumSome should (be (toSome) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumSome, toSome), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (fumSome should (be (fumSome) and contain oneElementOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumSome, fumSome) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumSome, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        fumSome should (be (fumSome) and contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (not contain oneElementOf Seq(...) and not contain oneElementOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedOneElementOf(prettifier, toSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, toSome, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedOneElementOf(prettifier, toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, toSome, Seq("happy", "birthday", "to", "you")) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedOneElementOf(prettifier, toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, toSome, Seq("happy", "birthday", "to", "you")) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)

        toSome should not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))

        val e3 = intercept[TestFailedException] {
          (toSome should (not contain oneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) and not contain oneElementOf (Seq("to", "you")))) (after being lowerCased and trimmed, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e3, FailureMessages.containedOneElementOf(prettifier, toSome, Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")) and not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        toSome should (not contain oneElementOf (Seq("fee", "fie", "fum", "foe")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not be (...) and not contain oneElementOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should (not be (fumSome) and not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, toSome, toSome), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not be (fumSome) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, toSome, fumSome) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toSome should (not be (fumSome) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, toSome, toSome), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not be (fumSome) and not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, toSome, fumSome) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (toSome should (not be (fumSome) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not be (toSome) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, toSome, toSome), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (toSome should (not be (fumSome) and not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, toSome, fumSome) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        toSome should (not be (fumSome) and not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }
  }

  describe("a collection of Options") {

    val some1s: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(1))
    val somes: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(2))
    val nones: Vector[Option[Int]] = Vector(scala.None, scala.None, scala.None)
    val somesNone: Vector[Option[Int]] = Vector(Some(1), Some(1), scala.None)
    val hiSomes: Vector[Option[String]] = Vector(Some("hi"), Some("hi"), Some("hi"))
    val toSomes: Vector[Option[String]] = Vector(Some("to"), Some("to"), Some("to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    describe("when used with (contain oneElementOf Seq(...) and contain oneElementOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should (contain oneElementOf Seq(1, 3, 4) and contain oneElementOf Seq(1, 6, 8))
        atLeast (2, somes) should (contain oneElementOf Seq(1, 3, 4) and contain oneElementOf Seq(1, 6, 8))
        atMost (2, somes) should (contain oneElementOf Seq(2, 3, 4) and contain oneElementOf Seq(2, 6, 8))
        no (somes) should (contain oneElementOf Seq(3, 4, 5) and contain oneElementOf Seq(7, 8, 9))

        val e1 = intercept[TestFailedException] {
          all (somes) should (contain oneElementOf Seq(1, 3, 4) and contain oneElementOf Seq(1, 2, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.didNotContainOneElementOf(prettifier, somes(2), Seq(1, 3, 4)), thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (somes) should (contain oneElementOf Seq(1, 2, 8) and contain oneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, FailureMessages.containedOneElementOf(prettifier, somes(2), Seq(1, 2, 8)) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, somes(2), Seq(1, 3, 4)), thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("ho", "hey", "howdy") and contain oneElementOf Seq("ho", "hi", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("ho", "hi", "howdy") and contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiSomes(0), Seq("ho", "hi", "howdy")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should (contain oneElementOf Seq("hi", "he") and contain oneElementOf Seq("hi", "he"))
        intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("ho", "he") and contain oneElementOf Seq("hi", "he"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiSomes) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("HI", "HE"))
          val e1 = intercept[TestFailedException] {
            all (hiSomes) should (contain oneElementOf Seq("hi", "he") and contain oneElementOf Seq("ho", "he"))
          }
          checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
          val e2 = intercept[TestFailedException] {
            all (hiSomes) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("hi", "he"))
          }
          checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiSomes(0), Seq("HI", "HE")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (contain oneElementOf Seq("hi", "he") and contain oneElementOf Seq("ho", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (hiSomes) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiSomes(0), Seq("HI", "HE")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        all (some1s) should (contain oneElementOf Seq(1, 2, 2, 3) and contain oneElementOf Seq(1, 6, 8))
        all (some1s) should (contain oneElementOf Seq(1, 6, 8) and contain oneElementOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (be (...) and contain oneElementOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should (be (Some(1)) and contain oneElementOf Seq(1, 6, 8))
        atLeast (2, somes) should (be (Some(1)) and contain oneElementOf Seq(1, 6, 8))
        atMost (2, somes) should (be (Some(1)) and contain oneElementOf Seq(2, 6, 8))
        no (somes) should (be (Some(3)) and contain oneElementOf Seq(7, 8, 9))

        val e1 = intercept[TestFailedException] {
          all (somes) should (be (Some(1)) and contain oneElementOf Seq(1, 2, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(2, "Some(2) was not equal to Some(1)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (some1s) should (be (Some(1)) and contain oneElementOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(1) was equal to Some(1), but " + FailureMessages.didNotContainOneElementOf(prettifier, some1s(0), Seq(2, 3, 8)), thisLineNumber - 2, some1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hei")) and contain oneElementOf Seq("ho", "hi", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"hei\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hi")) and contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "Some(\"hi\") was equal to Some(\"hi\"), but " + FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should (be (Some("hi")) and contain oneElementOf Seq("hi", "he"))
        intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) and contain oneElementOf Seq("HI", "HE"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiSomes) should (be (Some("hi")) and contain oneElementOf Seq("HI", "HE"))
          val e1 = intercept[TestFailedException] {
            all (hiSomes) should (be (Some("ho")) and contain oneElementOf Seq("HI", "HE"))
          }
          checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"ho\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
          val e2 = intercept[TestFailedException] {
            all (hiSomes) should (be (Some("hi")) and contain oneElementOf Seq("hi", "he"))
          }
          checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") was equal to Some(\"hi\"), but " + FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should (be (Some("hi")) and contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("ho")) and contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"ho\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("hi")) and contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") was equal to Some(\"hi\"), but " + FailureMessages.didNotContainOneElementOf(prettifier, hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        all (some1s) should (be (Some(1)) and contain oneElementOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (not contain oneElementOf Seq(..) and not contain oneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain oneElementOf (Seq("have", "a", "nice", "day")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("have", "a", "nice", "day")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("happy", "birthday", "to", "you")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("have", "a", "nice", "day")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, toSomes(0), Seq("have", "a", "nice", "day")) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("happy", "birthday", "to", "you")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("nice", "to", "meet", "you")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) and not contain oneElementOf (Seq("nice", "to", "meet", "you")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, toSomes(0), Seq("happy", "birthday", "to", "you")) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("NICE", "TO", "MEET", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("nice", "to", "meet", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) and not contain oneElementOf (Seq("nice", "to", "meet", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) and not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, toSomes(0), Seq("happy", "birthday", "to", "you")) + ", but " + FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("NICE", "TO", "MEET", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        all (toSomes) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")) and not contain oneElementOf (Seq("have", "a", "nice", "day")))
        all (toSomes) should (not contain oneElementOf (Seq("have", "a", "nice", "day")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not be (...) and not contain oneElementOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should (not be (Some("fee")) and not contain oneElementOf (Seq("have", "a", "nice", "day")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) and not contain oneElementOf (Seq("have", "a", "nice", "day")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("nice")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") was not equal to Some(\"nice\"), but " + FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("happy", "birthday", "to", "you")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not be (Some("hi")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("hi")) and not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") was not equal to Some(\"hi\"), but " + FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should (not be (Some("hi")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("to")) and not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("he")) and not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") was not equal to Some(\"he\"), but " + FailureMessages.containedOneElementOf(prettifier, toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should allow RHS to contain duplicated value") {
        all (toSomes) should (not be (Some("fee")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }
  }
}
