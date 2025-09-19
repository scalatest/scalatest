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
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ListShouldContainAllOfSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
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
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }
  
  //ADDITIONAL//

  describe("a List") {

    val fumList: List[String] = List("fex", "fum", "foe", "fie", "fee")
    val toList: List[String] = List("too", "you", "to", "birthday", "happy")
    val ecList: List[String] = List("\u0000fex", "fum", "foe", "fie", "fee")

    describe("when used with contain allOf (..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should contain allOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAllOfElements(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should contain allOf ("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should contain allOf ("fee", "fie", "foe", "fam")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should contain allOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain allOf ("fee", "fie", "foe", "fam")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should contain allOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should contain allOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should contain allOf ("fee", "fie", "foe", "fam")
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: " + prettifier(escapedString("\u0000fex"))))
      }
    }

    describe("when used with (contain allOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        fumList should (contain allOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAllOfElements(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allOf ("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (contain allOf ("fee", "fie", "foe", "fam"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (contain allOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain allOf ("fee", "fie", "foe", "fam"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain allOf (" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (contain allOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
      it("should throw NotAllowedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should (contain allOf ("fee", "fie", "foe", "fam"))
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: " + prettifier(escapedString("\u0000fex"))))
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should (contain allOf ("fee", "fie", "foe", "fam"))
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: " + prettifier(escapedString("\u0000fex"))))
      }
    }

    describe("when used with not contain allOf (..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList should not contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList should not contain allOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(prettifier, toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList should not contain allOf ("happy", "birthday", "to", "you", "dear")
        intercept[TestFailedException] {
          toList should not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      it("should use an explicitly provided Equality") {
        (toList should not contain allOf ("happy", "birthday", "to", "you", "dear")) (/* DOTTY-ONLY using */ decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (/* DOTTY-ONLY using */ decided by upperCaseStringEquality)
        }
        toList should not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList should not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (/* DOTTY-ONLY using */ after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should not contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    describe("when used with (not contain allOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList should (not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU", "DEAR"))
        val e1 = intercept[TestFailedException] {
          toList should (not contain allOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(prettifier, toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList should (not contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))
        intercept[TestFailedException] {
          toList should (not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList should (not contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should (not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should (not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should (not contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }
    
    describe("when used with shouldNot contain allOf (..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList shouldNot contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain allOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(prettifier, toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList shouldNot contain allOf ("happy", "birthday", "to", "you", "dear")
        intercept[TestFailedException] {
          toList shouldNot contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      it("should use an explicitly provided Equality") {
        (toList shouldNot contain allOf ("happy", "birthday", "to", "you", "dear")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList shouldNot contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList shouldNot contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    describe("when used with shouldNot (contain allOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList shouldNot (contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU", "DEAR"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain allOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(prettifier, toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList shouldNot (contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))
        intercept[TestFailedException] {
          toList shouldNot (contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList shouldNot (contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList shouldNot (contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot (contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot (contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }
  }

  describe("a col of Lists") {

    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(3, 2, 1, 0))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(8, 4, 3, 2))
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), Nil)
    val hiLists: Vector[List[String]] = Vector(List("howdy", "hi", "he"), List("howdy", "hi", "he"), List("howdy", "hi", "he"))
    val toLists: Vector[List[String]] = Vector(List("happy", "to", "you"), List("happy", "to", "you"), List("happy", "to", "you"))

    describe("when used with contain allOf (..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should contain allOf (1, 2, 3)
        atLeast (2, lists) should contain allOf (1, 2, 3)
        atMost (2, lists) should contain allOf (1, 2, 3)
        no (lists) should contain allOf (3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain allOf (1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(prettifier, List(8, 4, 3, 2)) + " did not contain all of " + "(1, 2, 3)" + " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should contain allOf ("he", "hi")
        intercept[TestFailedException] {
          all (hiLists) should contain allOf ("ho", "hi")
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiLists) should contain allOf ("HE", "HI")
          intercept[TestFailedException] {
            all (hiLists) should contain allOf ("HO", "HI")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should contain allOf ("HE", "HI")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain allOf ("HO", "HI")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain allOf ("he", "hi")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain allOf ("ho", "hi")) (decided by defaultEquality[String])
        }
      }
    }

    describe("when used with (contain allOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain allOf (1, 2, 3))
        atLeast (2, lists) should (contain allOf (1, 2, 3))
        atMost (2, lists) should (contain allOf (1, 2, 3))
        no (lists) should (contain allOf (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain allOf (1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(prettifier, List(8, 4, 3, 2)) + " did not contain all of " + "(1, 2, 3)" + " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should (contain allOf ("he", "hi"))
        intercept[TestFailedException] {
          all (hiLists) should (contain allOf ("ho", "hi"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiLists) should (contain allOf ("HE", "HI"))
          intercept[TestFailedException] {
            all (hiLists) should (contain allOf ("HO", "HI"))
          }
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain allOf ("HE", "HI"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain allOf ("HO", "HI"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain allOf ("he", "hi"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain allOf ("ho", "hi"))) (decided by defaultEquality[String])
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain allOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    describe("when used with not contain allOf (..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should not contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain allOf ("you", "to")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, List("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" +  " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain allOf ("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) should not contain allOf ("YOU", "TO")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should not contain allOf ("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain allOf ("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) should not contain allOf (" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) should not contain allOf (" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    describe("when used with (not contain allOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should (not contain allOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain allOf ("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, List("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" + " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain allOf ("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should (not contain allOf ("YOU", "TO"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should (not contain allOf ("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain allOf ("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not contain allOf (" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) should (not contain allOf (" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }
    
    describe("when used with shouldNot contain allOf (..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain allOf ("you", "to")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, List("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" +  " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain allOf ("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain allOf ("YOU", "TO")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot contain allOf ("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain allOf ("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot contain allOf (" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain allOf (" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    describe("when used with shouldNot (contain allOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot (contain allOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain allOf ("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(prettifier, List("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" + " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain allOf ("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain allOf ("YOU", "TO"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot (contain allOf ("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain allOf ("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot (contain allOf (" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain allOf (" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }
  }
}
