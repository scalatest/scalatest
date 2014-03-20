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

import org.scalautils.Equality
import org.scalautils.StringNormalizations._
import SharedHelpers._
import FailureMessages._
import Matchers._

class OptionShouldContainOneOfLogicalAndSpec extends FunSpec {
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val fileName = "OptionShouldContainOneOfLogicalAndSpec.scala"

  describe("an Option") {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    describe("when used with (contain oneOf (...) and contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumSome should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("happy", "birthday", "to", "you") and contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", decorateToStringValue(fumSome), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", decorateToStringValue(fumSome), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", decorateToStringValue(fumSome), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fie", "fum") and contain oneOf ("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf ("fee", "fie", "fum", "foe") and contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    describe("when used with (be (...) and contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumSome should (be (fumSome) and contain oneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) and contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumSome), decorateToStringValue(toSome)), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (be (fumSome) and contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", decorateToStringValue(fumSome), decorateToStringValue(fumSome)) + ", but " + Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (be (fumSome) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumSome), decorateToStringValue(toSome)), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (be (fumSome) and contain oneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", decorateToStringValue(fumSome), decorateToStringValue(fumSome)) + ", but " + Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (fumSome should (be (fumSome) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (be (toSome) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumSome), decorateToStringValue(toSome)), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (fumSome should (be (fumSome) and contain oneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", decorateToStringValue(fumSome), decorateToStringValue(fumSome)) + ", but " + Resources("didNotContainOneOfElements", decorateToStringValue(fumSome), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (be (fumSome) and contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    describe("when used with (not contain oneOf (...) and not contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should (not contain oneOf ("fee", "fie", "foe", "fum") and not contain oneOf ("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(toSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("fee", "fie", "foe", "fum") and not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", decorateToStringValue(toSome), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("containedOneOfElements", decorateToStringValue(toSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toSome should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", decorateToStringValue(toSome), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("containedOneOfElements", decorateToStringValue(toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (toSome should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (toSome should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", decorateToStringValue(toSome), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("containedOneOfElements", decorateToStringValue(toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
        
        toSome should not contain oneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        
        val e3 = intercept[TestFailedException] {
          (toSome should (not contain oneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ") and not contain oneOf ("to", "you"))) (after being lowerCased and trimmed, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e3, Resources("containedOneOfElements", decorateToStringValue(toSome), "\" HAPPY \", \" BIRTHDAY \", \" TO \", \" YOU \""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") and not contain oneOf ("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneOf ("fee", "fie", "fum", "foe") and not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    describe("when used with (not be (...) and not contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should (not be (fumSome) and not contain oneOf ("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) and not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(toSome), decorateToStringValue(toSome)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not be (fumSome) and not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(toSome), decorateToStringValue(fumSome)) + ", but " + Resources("containedOneOfElements", decorateToStringValue(toSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toSome should (not be (fumSome) and not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) and not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(toSome), decorateToStringValue(toSome)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not be (fumSome) and not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(toSome), decorateToStringValue(fumSome)) + ", but " + Resources("containedOneOfElements", decorateToStringValue(toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (toSome should (not be (fumSome) and not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not be (toSome) and not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(toSome), decorateToStringValue(toSome)), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (toSome should (not be (fumSome) and not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(toSome), decorateToStringValue(fumSome)) + ", but " + Resources("containedOneOfElements", decorateToStringValue(toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not be (fumSome) and not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
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
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + decorateToStringValue(left)

    describe("when used with (contain oneOf (...) and contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should (contain oneOf (1, 3, 4) and contain oneOf (1, 6, 8))
        atLeast (2, somes) should (contain oneOf (1, 3, 4) and contain oneOf (1, 6, 8)) 
        atMost (2, somes) should (contain oneOf (2, 3, 4) and contain oneOf (2, 6, 8))
        no (somes) should (contain oneOf (3, 4, 5) and contain oneOf (7, 8, 9))

        val e1 = intercept[TestFailedException] {
          all (somes) should (contain oneOf (1, 3, 4) and contain oneOf (1, 2, 8)) 
        }
        checkMessageStackDepth(e1, allErrMsg(2, "Some(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (somes) should (contain oneOf (1, 2, 8) and contain oneOf (1, 3, 4)) 
        }
        checkMessageStackDepth(e2, allErrMsg(2, "Some(2) contained one of (1, 2, 8), but Some(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("ho", "hey", "howdy") and contain oneOf ("ho", "hi", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "Some(\"hi\") did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("ho", "hi", "howdy") and contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "Some(\"hi\") contained one of (\"ho\", \"hi\", \"howdy\"), but Some(\"hi\") did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should (contain oneOf ("hi", "he") and contain oneOf ("hi", "he"))
        intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("ho", "he") and contain oneOf ("hi", "he"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiSomes) should (contain oneOf ("HI", "HE") and contain oneOf ("HI", "HE"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("hi", "he") and contain oneOf ("ho", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("HI", "HE") and contain oneOf ("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") contained one of (\"HI\", \"HE\"), but Some(\"hi\") did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should (contain oneOf ("HI", "HE") and contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (contain oneOf ("hi", "he") and contain oneOf ("ho", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (hiSomes) should (contain oneOf ("HI", "HE") and contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") contained one of (\"HI\", \"HE\"), but Some(\"hi\") did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf (1, 2, 2, 3) and contain oneOf (1, 6, 8))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf (1, 6, 8) and contain oneOf (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    describe("when used with (be (...) and contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should (be (Some(1)) and contain oneOf (1, 6, 8))
        atLeast (2, somes) should (be (Some(1)) and contain oneOf (1, 6, 8)) 
        atMost (2, somes) should (be (Some(1)) and contain oneOf (2, 6, 8))
        no (somes) should (be (Some(3)) and contain oneOf (7, 8, 9))

        val e1 = intercept[TestFailedException] {
          all (somes) should (be (Some(1)) and contain oneOf (1, 2, 8)) 
        }
        checkMessageStackDepth(e1, allErrMsg(2, "Some(2) was not equal to Some(1)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (some1s) should (be (Some(1)) and contain oneOf (2, 3, 8)) 
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(1) was equal to Some(1), but Some(1) did not contain one of (2, 3, 8)", thisLineNumber - 2, some1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hei")) and contain oneOf ("ho", "hi", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"hei\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hi")) and contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "Some(\"hi\") was equal to Some(\"hi\"), but Some(\"hi\") did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should (be (Some("hi")) and contain oneOf ("hi", "he"))
        intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) and contain oneOf ("HI", "HE"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiSomes) should (be (Some("hi")) and contain oneOf ("HI", "HE"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) and contain oneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"ho\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hi")) and contain oneOf ("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") was equal to Some(\"hi\"), but Some(\"hi\") did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should (be (Some("hi")) and contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("ho")) and contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"ho\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("hi")) and contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") was equal to Some(\"hi\"), but Some(\"hi\") did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (be (Some(1)) and contain oneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    describe("when used with (not contain oneOf (..) and not contain oneOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fum") and not contain oneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("have", "a", "nice", "day"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("have", "a", "nice", "day") and not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") did not contain one of (\"have\", \"a\", \"nice\", \"day\"), but Some(\"to\") contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("nice", "to", "meet", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not contain oneOf ("nice", "to", "meet", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") did not contain one of (\"happy\", \"birthday\", \"to\", \"you\"), but Some(\"to\") contained one of (\"NICE\", \"TO\", \"MEET\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") and not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") did not contain one of (\"happy\", \"birthday\", \"to\", \"you\"), but Some(\"to\") contained one of (\"NICE\", \"TO\", \"MEET\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") and not contain oneOf ("have", "a", "nice", "day"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneOf ("have", "a", "nice", "day") and not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    describe("when used with (not be (...) and not contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should (not be (Some("fee")) and not contain oneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) and not contain oneOf ("have", "a", "nice", "day"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("nice")) and not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") was not equal to Some(\"nice\"), but Some(\"to\") contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not be (Some("hi")) and not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) and not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("hi")) and not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") was not equal to Some(\"hi\"), but Some(\"to\") contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should (not be (Some("hi")) and not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("to")) and not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("he")) and not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"to\") was not equal to Some(\"he\"), but Some(\"to\") contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not be (Some("fee")) and not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
  }
}
