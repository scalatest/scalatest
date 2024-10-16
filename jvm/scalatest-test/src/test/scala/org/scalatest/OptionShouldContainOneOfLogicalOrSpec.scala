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

import org.scalactic.Equality
import org.scalactic.Prettifier
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class OptionShouldContainOneOfLogicalOrSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }
  
  val fileName = "OptionShouldContainOneOfLogicalOrSpec.scala"

  describe("an Option") {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    describe("when used with (contain oneOf (...) or contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))
        fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("happy", "birthday", "to", "you"))
        fumSome should (contain oneOf ("happy", "birthday", "to", "you") or contain oneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("have", "a", "nice", "day") or contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"have\", \"a\", \"nice\", \"day\"") + ", and " + Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("fee", "fie", "foe", "fum"))
        fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fie", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf ("fee", "fie", "fum", "foe") or contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }
    
    describe("when used with (be (...) or contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        fumSome should (be (toSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        fumSome should (be (fumSome) or contain oneOf ("happy", "birthday", "to", "you"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) or contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumSome), decorateToStringValue(prettifier, toSome)) + ", and " + Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (be (fumSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))
        fumSome should (be (toSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))
        fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumSome), decorateToStringValue(prettifier, toSome)) + ", and " + Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (fumSome should (be (fumSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumSome should (be (toSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (be (toSome) or contain oneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumSome), decorateToStringValue(prettifier, toSome)) + ", and " + Resources.didNotContainOneOfElements(decorateToStringValue(prettifier, fumSome), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    describe("when used with (not contain oneOf (...) or not contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("nice", "to", "meet", "you") or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"nice\", \"to\", \"meet\", \"you\"") + ", and " + Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))
        toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))
        toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        }
        checkMessageStackDepth(e1, Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"") + ", and " + Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"NICE\", \"TO\", \"MEET\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"") + ", and " + Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"NICE\", \"TO\", \"MEET\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") or not contain oneOf ("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneOf ("fee", "fie", "fum", "foe") or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }
    
    describe("when used with (not be (...) or not contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toSome should (not be (fumSome) or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not be (toSome) or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not be (fumSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        val e2 = intercept[TestFailedException] {
          toSome should (not be (toSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, toSome), decorateToStringValue(prettifier, toSome)) + ", and " + Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toSome should (not be (fumSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        toSome should (not be (toSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        toSome should (not be (fumSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, toSome), decorateToStringValue(prettifier, toSome)) + ", and " + Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (toSome should (not be (fumSome) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (toSome should (not be (toSome) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (toSome should (not be (fumSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not be (toSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, toSome), decorateToStringValue(prettifier, toSome)) + ", and " + Resources.containedOneOfElements(decorateToStringValue(prettifier, toSome), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not be (fumSome) or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
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
      "in " + decorateToStringValue(prettifier, left)

    describe("when used with (contain oneOf (...) or contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should (contain oneOf (1, 3, 4) or contain oneOf (1, 6, 8))
        all (some1s) should (contain oneOf (2, 3, 4) or contain oneOf (1, 6, 8))
        all (some1s) should (contain oneOf (1, 3, 4) or contain oneOf (2, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (somes) should (contain oneOf (1, 6, 8) or contain oneOf (1, 3, 4)) 
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.didNotContainOneOfElements(prettifier, somes(2), UnquotedString("1, 6, 8")) + ", and " + FailureMessages.didNotContainOneOfElements(prettifier, somes(2), UnquotedString("1, 3, 4")), thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("ho", "he", "howdy") or contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"ho\", \"he\", \"howdy\"")) + ", and " + FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"ho\", \"hey\", \"howdy\"")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should (contain oneOf ("hi", "he") or contain oneOf ("hi", "he"))
        all (hiSomes) should (contain oneOf ("HI", "HE") or contain oneOf ("hi", "he"))
        all (hiSomes) should (contain oneOf ("hi", "he") or contain oneOf ("HI", "HE"))
        intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("HI", "HE") or contain oneOf ("HI", "HE"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiSomes) should (contain oneOf ("HI", "HE") or contain oneOf ("HI", "HE"))
          all (hiSomes) should (contain oneOf ("hi", "he") or contain oneOf ("HI", "HE"))
          all (hiSomes) should (contain oneOf ("HI", "HE") or contain oneOf ("hi", "he"))
          val e1 = intercept[TestFailedException] {
            all (hiSomes) should (contain oneOf ("hi", "he") or contain oneOf ("hi", "he"))
          }
          checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"hi\", \"he\"")) + ", and " + FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should (contain oneOf ("HI", "HE") or contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiSomes) should (contain oneOf ("hi", "he") or contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiSomes) should (contain oneOf ("HI", "HE") or contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (contain oneOf ("hi", "he") or contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"hi\", \"he\"")) + ", and " + FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf (1, 2, 2, 3) or contain oneOf (1, 6, 8))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf (1, 6, 8) or contain oneOf (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }
    
    describe("when used with (be (...) or contain oneOf (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (some1s) should (be (Some(1)) or contain oneOf (1, 6, 8))
        all (some1s) should (be (Some(2)) or contain oneOf (1, 6, 8))
        all (some1s) should (be (Some(1)) or contain oneOf (2, 6, 8))

        val e1 = intercept[TestFailedException] {
          all (some1s) should (be (Some(2)) or contain oneOf (2, 3, 8)) 
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(1) was not equal to Some(2), and " + FailureMessages.didNotContainOneOfElements(prettifier, some1s(0), UnquotedString("2, 3, 8")), thisLineNumber - 2, some1s), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("to")) or contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"to\"), and " + FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"ho\", \"hey\", \"howdy\"")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        all (hiSomes) should (be (Some("hi")) or contain oneOf ("hi", "he"))
        all (hiSomes) should (be (Some("hi")) or contain oneOf ("HI", "HE"))
        all (hiSomes) should (be (Some("he")) or contain oneOf ("hi", "he"))
        intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) or contain oneOf ("HI", "HE"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiSomes) should (be (Some("hi")) or contain oneOf ("HI", "HE"))
          all (hiSomes) should (be (Some("he")) or contain oneOf ("HI", "HE"))
          all (hiSomes) should (be (Some("hi")) or contain oneOf ("hi", "he"))
          val e1 = intercept[TestFailedException] {
            all (hiSomes) should (be (Some("he")) or contain oneOf ("hi", "he"))
          }
          checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"he\"), and " + FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiSomes) should (be (Some("hi")) or contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiSomes) should (be (Some("he")) or contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiSomes) should (be (Some("hi")) or contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("he")) or contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"hi\") was not equal to Some(\"he\"), and " + FailureMessages.didNotContainOneOfElements(prettifier, hiSomes(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (be (Some(1)) or contain oneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    describe("when used with (not contain oneOf (..) or not contain oneOf (..)) ") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("nice", "to", "meet", "you") or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"nice\", \"to\", \"meet\", \"you\"")) + ", and " + FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"happy\", \"birthday\", \"to\", \"you\"")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))
        all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))
        all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"")) + ", and " + FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"NICE\", \"TO\", \"MEET\", \"YOU\"")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"")) + ", and " + FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"NICE\", \"TO\", \"MEET\", \"YOU\"")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") or not contain oneOf ("have", "a", "nice", "day"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneOf ("have", "a", "nice", "day") or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }
    
    describe("when used with (not be (..) or not contain oneOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toSomes) should (not be (Some("fee")) or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not be (Some("to")) or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not be (Some("fee")) or not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\"), and " + FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"happy\", \"birthday\", \"to\", \"you\"")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not be (Some("hi")) or not contain oneOf ("happy", "birthday", "to", "you"))
        all (toSomes) should (not be (Some("to")) or not contain oneOf ("happy", "birthday", "to", "you"))
        all (toSomes) should (not be (Some("hi")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\"), and " + FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should use an explicitly provided Equality") {
        (all (toSomes) should (not be (Some("hi")) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (all (toSomes) should (not be (Some("to")) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (all (toSomes) should (not be (Some("hi")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("to")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(\"to\") was equal to Some(\"to\"), and " + FailureMessages.containedOneOfElements(prettifier, toSomes(0), UnquotedString("\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"")), thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not be (Some("fee")) or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }
  }
}
