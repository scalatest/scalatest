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

class OptionShouldContainOneElementOfLogicalOrSpec extends Spec {

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  val fileName = "OptionShouldContainOneElementOfLogicalOrSpec.scala"

  object `an Option` {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    object `when used with (contain oneElementOf Seq(...) or contain oneElementOf Seq(...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") or contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") or contain oneElementOf Seq("happy", "birthday", "to", "you"))
        fumSome should (contain oneElementOf Seq("happy", "birthday", "to", "you") or contain oneElementOf Seq("fee", "fie", "fum", "foe"))

        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("have", "a", "nice", "day") or contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainOneElementOf", fumSome, Seq("have", "a", "nice", "day")) + ", and " + FailureMessages("didNotContainOneElementOf", fumSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") or contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") or contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") or contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))

        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") or contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainOneElementOf", fumSome, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages("didNotContainOneElementOf", fumSome, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") or contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumSome should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") or contain oneElementOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") or contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fum") or contain oneElementOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainOneElementOf", fumSome, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages("didNotContainOneElementOf", fumSome, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum") or contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneElementOf Seq("fee", "fie", "fum", "foe") or contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }

    object `when used with (be (...) or contain oneElementOf Seq(...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (be_== (fumSome) or contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        fumSome should (be_== (toSome) or contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        fumSome should (be_== (fumSome) or contain oneElementOf Seq("happy", "birthday", "to", "you"))

        val e1 = intercept[TestFailedException] {
          fumSome should (be_== (toSome) or contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumSome, toSome) + ", and " + FailureMessages("didNotContainOneElementOf", fumSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        fumSome should (be_== (fumSome) or contain oneElementOf Seq("FEE", "FIE", "FUM", "FOE"))
        fumSome should (be_== (toSome) or contain oneElementOf Seq("FEE", "FIE", "FUM", "FOE"))
        fumSome should (be_== (fumSome) or contain oneElementOf Seq("fee", "fie", "fum", "foe"))

        val e1 = intercept[TestFailedException] {
          fumSome should (be_== (toSome) or contain oneElementOf Seq("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumSome, toSome) + ", and " + FailureMessages("didNotContainOneElementOf", fumSome, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (be_== (fumSome) or contain oneElementOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumSome should (be_== (toSome) or contain oneElementOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumSome should (be_== (fumSome) or contain oneElementOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (fumSome should (be_== (toSome) or contain oneElementOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumSome, toSome) + ", and " + FailureMessages("didNotContainOneElementOf", fumSome, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (be_== (fumSome) or contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }

    object `when used with (not contain oneElementOf Seq(...) or not contain oneElementOf Seq(...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("nice", "to", "meet", "you")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedOneElementOf", toSome, Seq("nice", "to", "meet", "you")) + ", and " + FailureMessages("containedOneElementOf", toSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))
        toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))
        toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedOneElementOf", toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) + ", and " + FailureMessages("containedOneElementOf", toSome, Seq("NICE", "TO", "MEET", "YOU")), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (toSome should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("containedOneElementOf", toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) + ", and " + FailureMessages("containedOneElementOf", toSome, Seq("NICE", "TO", "MEET", "YOU")), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneElementOf (Seq("fee", "fie", "fum", "foe")) or not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }

    object `when used with (not be (...) or not contain oneElementOf Seq(...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not be_== (fumSome) or not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        toSome should (not be_== (toSome) or not contain oneElementOf (Seq("fee", "fie", "fum", "foe")))
        toSome should (not be_== (fumSome) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        val e2 = intercept[TestFailedException] {
          toSome should (not be_== (toSome) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, FailureMessages("wasEqualTo", toSome, toSome) + ", and " + FailureMessages("containedOneElementOf", toSome, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toSome should (not be_== (fumSome) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        toSome should (not be_== (toSome) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        toSome should (not be_== (fumSome) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        val e1 = intercept[TestFailedException] {
          toSome should (not be_== (toSome) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", toSome, toSome) + ", and " + FailureMessages("containedOneElementOf", toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not be_== (fumSome) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        (toSome should (not be_== (toSome) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        (toSome should (not be_== (fumSome) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not be_== (toSome) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", toSome, toSome) + ", and " + FailureMessages("containedOneElementOf", toSome, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not be_== (fumSome) or not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }
  }

  object `a collection of Options` {

    val some1s: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(1))
    val somes: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(2))
    val nones: Vector[Option[Int]] = Vector(None, None, None)
    val somesNone: Vector[Option[Int]] = Vector(Some(1), Some(1), None)
    val hiSomes: Vector[Option[String]] = Vector(Some("hi"), Some("hi"), Some("hi"))
    val toSomes: Vector[Option[String]] = Vector(Some("to"), Some("to"), Some("to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    object `when used with (contain oneElementOf Seq(...) or contain oneElementOf Seq(...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (some1s) should (contain oneElementOf Seq(1, 3, 4) or contain oneElementOf Seq(1, 6, 8))
        all (some1s) should (contain oneElementOf Seq(2, 3, 4) or contain oneElementOf Seq(1, 6, 8))
        all (some1s) should (contain oneElementOf Seq(1, 3, 4) or contain oneElementOf Seq(2, 6, 8))

        val e1 = intercept[TestFailedException] {
          all (somes) should (contain oneElementOf Seq(1, 6, 8) or contain oneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages("didNotContainOneElementOf", somes(2), Seq(1, 6, 8)) + ", and " + FailureMessages("didNotContainOneElementOf", somes(2), Seq(1, 3, 4)), thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("ho", "he", "howdy") or contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("ho", "he", "howdy")) + ", and " + FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (contain oneElementOf Seq("hi", "he") or contain oneElementOf Seq("hi", "he"))
        all (hiSomes) should (contain oneElementOf Seq("HI", "HE") or contain oneElementOf Seq("hi", "he"))
        all (hiSomes) should (contain oneElementOf Seq("hi", "he") or contain oneElementOf Seq("HI", "HE"))
        intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("HI", "HE") or contain oneElementOf Seq("HI", "HE"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiSomes) should (contain oneElementOf Seq("HI", "HE") or contain oneElementOf Seq("HI", "HE"))
        all (hiSomes) should (contain oneElementOf Seq("hi", "he") or contain oneElementOf Seq("HI", "HE"))
        all (hiSomes) should (contain oneElementOf Seq("HI", "HE") or contain oneElementOf Seq("hi", "he"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneElementOf Seq("hi", "he") or contain oneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("hi", "he")) + ", and " + FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (contain oneElementOf Seq("HI", "HE") or contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiSomes) should (contain oneElementOf Seq("hi", "he") or contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiSomes) should (contain oneElementOf Seq("HI", "HE") or contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (contain oneElementOf Seq("hi", "he") or contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("hi", "he")) + ", and " + FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneElementOf Seq(1, 2, 2, 3) or contain oneElementOf Seq(1, 6, 8))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneElementOf Seq(1, 6, 8) or contain oneElementOf Seq(1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }

    object `when used with (be (...) or contain oneElementOf Seq(...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (some1s) should (be_== (Some(1)) or contain oneElementOf Seq(1, 6, 8))
        all (some1s) should (be_== (Some(2)) or contain oneElementOf Seq(1, 6, 8))
        all (some1s) should (be_== (Some(1)) or contain oneElementOf Seq(2, 6, 8))

        val e1 = intercept[TestFailedException] {
          all (some1s) should (be_== (Some(2)) or contain oneElementOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(1) was not equal to Some(2), and " + FailureMessages("didNotContainOneElementOf", some1s(0), Seq(2, 3, 8)), thisLineNumber - 2, some1s), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (be_== (Some("to")) or contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"to\"), and " + FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (be_== (Some("hi")) or contain oneElementOf Seq("hi", "he"))
        all (hiSomes) should (be_== (Some("hi")) or contain oneElementOf Seq("HI", "HE"))
        all (hiSomes) should (be_== (Some("he")) or contain oneElementOf Seq("hi", "he"))
        intercept[TestFailedException] {
          all (hiSomes) should (be_== (Some("ho")) or contain oneElementOf Seq("HI", "HE"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiSomes) should (be_== (Some("hi")) or contain oneElementOf Seq("HI", "HE"))
        all (hiSomes) should (be_== (Some("he")) or contain oneElementOf Seq("HI", "HE"))
        all (hiSomes) should (be_== (Some("hi")) or contain oneElementOf Seq("hi", "he"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (be_== (Some("he")) or contain oneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"he\"), and " + FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (be_== (Some("hi")) or contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiSomes) should (be_== (Some("he")) or contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiSomes) should (be_== (Some("hi")) or contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (be_== (Some("he")) or contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"he\"), and " + FailureMessages("didNotContainOneElementOf", hiSomes(0), Seq("hi", "he")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (be_== (Some(1)) or contain oneElementOf Seq(1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }

    object `when used with (not contain oneElementOf Seq(..) or not contain oneElementOf Seq(..)) ` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain oneElementOf (Seq("have", "a", "nice", "day")))
        all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("have", "a", "nice", "day")))
        all (toSomes) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("nice", "to", "meet", "you")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("containedOneElementOf", toSomes(0), Seq("nice", "to", "meet", "you")) + ", and " + FailureMessages("containedOneElementOf", toSomes(0), Seq("happy", "birthday", "to", "you")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))
        all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))
        all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("containedOneElementOf", toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) + ", and " + FailureMessages("containedOneElementOf", toSomes(0), Seq("NICE", "TO", "MEET", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("nice", "to", "meet", "you")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (toSomes) should (not contain oneElementOf (Seq("happy", "birthday", "to", "you")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) or not contain oneElementOf (Seq("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("containedOneElementOf", toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) + ", and " + FailureMessages("containedOneElementOf", toSomes(0), Seq("NICE", "TO", "MEET", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain oneElementOf (Seq("have", "a", "nice", "day")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneElementOf (Seq("have", "a", "nice", "day")) or not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }

    object `when used with (not be (..) or not contain oneElementOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not be_== (Some("fee")) or not contain oneElementOf (Seq("have", "a", "nice", "day")))
        all (toSomes) should (not be_== (Some("to")) or not contain oneElementOf (Seq("have", "a", "nice", "day")))
        all (toSomes) should (not be_== (Some("fee")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be_== (Some("to")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\"), and " + FailureMessages("containedOneElementOf", toSomes(0), Seq("happy", "birthday", "to", "you")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not be_== (Some("hi")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        all (toSomes) should (not be_== (Some("to")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))
        all (toSomes) should (not be_== (Some("hi")) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be_== (Some("to")) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\"), and " + FailureMessages("containedOneElementOf", toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not be_== (Some("hi")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        (all (toSomes) should (not be_== (Some("to")) or not contain oneElementOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseStringEquality)
        (all (toSomes) should (not be_== (Some("hi")) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not be_== (Some("to")) or not contain oneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\"), and " + FailureMessages("containedOneElementOf", toSomes(0), Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not be_== (Some("fee")) or not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("oneElementOfDuplicate")))
      }
    }
  }
}
