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

import org.scalactic.{Equality, Every, One, Many, Prettifier}
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class EveryShouldContainAllElementsOfSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  private def upperCase(value: Any): Any =
    value match {
      case l: Every[_] => l.map(upperCase(_))
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

  describe("an Every") {

    val fumList: Every[String] = Every("fex", "fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("too", "you", "to", "birthday", "happy")
    val ecList: Every[String] = Every("\u0000fex", "fum", "foe", "fie", "fee")

    describe("when used with contain allElementsOf Seq(..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should contain allElementsOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should contain allElementsOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should contain allElementsOf Seq("fee", "fie", "foe", "fam")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain allElementsOf Seq("fee", "fie", "foe", "fam")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
      it("should allow RHS to contain duplicated value") {
        fumList should contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum")
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should contain allElementsOf Seq("fee", "fie", "foe", "fam")
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: \"\\u0000fex\""))
      }
    }

    describe("when used with (contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      it("should allow RHS to contain duplicated value") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with not contain allElementsOf Seq(..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList should not contain allElementsOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toList should not contain allElementsOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAllElementsOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList should not contain allElementsOf (Seq("happy", "birthday", "to", "you", "dear"))
        intercept[TestFailedException] {
          toList should not contain allElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList should not contain allElementsOf (Seq("happy", "birthday", "to", "you", "dear"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not contain allElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should not contain allElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should not contain allElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList should not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList should (not contain allElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU", "DEAR")))
        val e1 = intercept[TestFailedException] {
          toList should (not contain allElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAllElementsOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList should (not contain allElementsOf (Seq("NICE", "TO", "MEET", "YOU", "TOO")))
        intercept[TestFailedException] {
          toList should (not contain allElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList should (not contain allElementsOf (Seq("NICE", "TO", "MEET", "YOU", "TOO")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not contain allElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        toList should (not contain allElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (toList should (not contain allElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with shouldNot contain allElementsOf Seq(..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList shouldNot contain allElementsOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain allElementsOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAllElementsOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList shouldNot contain allElementsOf Seq("happy", "birthday", "to", "you", "dear")
        intercept[TestFailedException] {
          toList shouldNot contain allElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      it("should use an explicitly provided Equality") {
        (toList shouldNot contain allElementsOf Seq("happy", "birthday", "to", "you", "dear")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain allElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList shouldNot contain allElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList shouldNot contain allElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList shouldNot contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum")
      }
    }

    describe("when used with shouldNot (contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        toList shouldNot (contain allElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU", "DEAR"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAllElementsOf(prettifier, toList, Seq("happy", "birthday", "to", "you")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        toList shouldNot (contain allElementsOf Seq("NICE", "TO", "MEET", "YOU", "TOO"))
        intercept[TestFailedException] {
          toList shouldNot (contain allElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      it("should use an explicitly provided Equality") {
        (toList shouldNot (contain allElementsOf Seq("NICE", "TO", "MEET", "YOU", "TOO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain allElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList shouldNot (contain allElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot (contain allElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        toList shouldNot (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }
  }

  describe("an every of Everys") {

    val list1s: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(3, 2, 1, 0))
    val lists: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(8, 4, 3, 2))
    val hiLists: Every[Every[String]] = Every(Every("howdy", "hi", "he"), Every("howdy", "hi", "he"), Every("howdy", "hi", "he"))
    val toLists: Every[Every[String]] = Every(Every("happy", "to", "you"), Every("happy", "to", "you"), Every("happy", "to", "you"))

    describe("when used with contain allElementsOf Seq(..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should contain allElementsOf Seq(1, 2, 3)
        atLeast (2, lists) should contain allElementsOf Seq(1, 2, 3)
        atMost (2, lists) should contain allElementsOf Seq(1, 2, 3)
        no (lists) should contain allElementsOf Seq(3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain allElementsOf Seq(1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        val offendingLine = thisLineNumber - 3
        e1.failedCodeLineNumber.get should be (offendingLine)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(1, 2, 3)) + " (EveryShouldContainAllElementsOfSpec.scala:" + offendingLine + ") \n" +
          "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should contain allElementsOf Seq("he", "hi")
        intercept[TestFailedException] {
          all (hiLists) should contain allElementsOf Seq("ho", "hi")
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiLists) should contain allElementsOf Seq("HE", "HI")
          intercept[TestFailedException] {
            all (hiLists) should contain allElementsOf Seq("HO", "HI")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should contain allElementsOf Seq("HE", "HI")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain allElementsOf Seq("HO", "HI")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain allElementsOf Seq("he", "hi")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain allElementsOf Seq("ho", "hi")) (decided by defaultEquality[String])
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (list1s) should contain allElementsOf Seq(1, 2, 2, 3)
      }
    }

    describe("when used with (contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain allElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (contain allElementsOf Seq(1, 2, 3))
        atMost (2, lists) should (contain allElementsOf Seq(1, 2, 3))
        no (lists) should (contain allElementsOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain allElementsOf Seq(1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        val offendingLine = thisLineNumber - 3
        e1.failedCodeLineNumber.get should be (offendingLine)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(1, 2, 3)) + " (EveryShouldContainAllElementsOfSpec.scala:" + offendingLine + ") \n" +
          "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should (contain allElementsOf Seq("he", "hi"))
        intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("ho", "hi"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiLists) should (contain allElementsOf Seq("HE", "HI"))
          intercept[TestFailedException] {
            all (hiLists) should (contain allElementsOf Seq("HO", "HI"))
          }
        }
      }
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain allElementsOf Seq("HE", "HI"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("HO", "HI"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain allElementsOf Seq("he", "hi"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("ho", "hi"))) (decided by defaultEquality[String])
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (list1s) should (contain allElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with not contain allElementsOf Seq(..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should not contain allElementsOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain allElementsOf (Seq("you", "to"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        val offendingLine = thisLineNumber - 3
        e1.failedCodeLineNumber.get should be (offendingLine)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, Many("happy", "to", "you")) + " contained all elements of " + decorateToStringValue(prettifier, List("you", "to")) +  " (EveryShouldContainAllElementsOfSpec.scala:" + offendingLine + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain allElementsOf (Seq("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should not contain allElementsOf (Seq("YOU", "TO"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should not contain allElementsOf (Seq("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain allElementsOf (Seq("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should not contain allElementsOf (Seq(" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) should not contain allElementsOf (Seq(" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) should not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should (not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain allElementsOf (Seq("you", "to")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        val offendingLine = thisLineNumber - 3
        e1.failedCodeLineNumber.get should be (offendingLine)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, Many("happy", "to", "you")) + " contained all elements of " + decorateToStringValue(prettifier, List("you", "to")) + " (EveryShouldContainAllElementsOfSpec.scala:" + offendingLine + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain allElementsOf (Seq("NICE", "MEET", "YOU")))
        intercept[TestFailedException] {
          all (toLists) should (not contain allElementsOf (Seq("YOU", "TO")))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should (not contain allElementsOf (Seq("NICE", "MEET", "YOU")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain allElementsOf (Seq("YOU", "TO")))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not contain allElementsOf (Seq(" YOU ", " TO ")))
        intercept[TestFailedException] {
          (all (toLists) should (not contain allElementsOf (Seq(" YOU ", " TO ")))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) should (not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with shouldNot contain allElementsOf Seq(..)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot contain allElementsOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain allElementsOf Seq("you", "to")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        val offendingLine = thisLineNumber - 3
        e1.failedCodeLineNumber.get should be (offendingLine)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, Many("happy", "to", "you")) + " contained all elements of " + decorateToStringValue(prettifier, List("you", "to")) +  " (EveryShouldContainAllElementsOfSpec.scala:" + (offendingLine) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain allElementsOf Seq("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain allElementsOf Seq("YOU", "TO")
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot contain allElementsOf Seq("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain allElementsOf Seq("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot contain allElementsOf Seq(" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain allElementsOf Seq(" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) shouldNot contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum")
      }
    }

    describe("when used with shouldNot (contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot (contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain allElementsOf Seq("you", "to"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllElementsOfSpec.scala")
        val offendingLine = thisLineNumber - 3
        e1.failedCodeLineNumber.get should be (offendingLine)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, Many("happy", "to", "you")) + " contained all elements of " + decorateToStringValue(prettifier, List("you", "to")) + " (EveryShouldContainAllElementsOfSpec.scala:" + offendingLine + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain allElementsOf Seq("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain allElementsOf Seq("YOU", "TO"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot (contain allElementsOf Seq("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain allElementsOf Seq("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot (contain allElementsOf Seq(" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain allElementsOf Seq(" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      it("should allow RHS to contain duplicated value") {
        all (toLists) shouldNot (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }
  }
}
