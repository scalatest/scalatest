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
import org.scalactic.Uniformity
import org.scalactic.Prettifier
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalactic.anyvals.{PosZInt, PosZDouble, PosInt}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ListShouldContainAtMostOneElementOfSpec extends AnyFunSpec {

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

    describe("when used with contain atMostOneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should contain atMostOneElementOf Seq("fee", "fie", "foe", "fam")
        val e1 = intercept[TestFailedException] {
          fumList should contain atMostOneElementOf Seq("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainAtMostOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList should contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM")
        intercept[TestFailedException] {
          fumList should contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          (fumList should contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)

        }
        fumList should contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")
      }
      it("should do nothing when RHS contain duplicated value") {
        fumList should contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fam")
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should contain atMostOneElementOf Seq("fee", "fie", "foe", "fum")
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: \"\\u0000fix\""))
      }
    }

    describe("when used with (contain atMostOneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fam"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.didNotContainAtMostOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM"))
        intercept[TestFailedException] {
          fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          (fumList should (contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        }
        fumList should (contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))
      }
      it("should do nothing when RHS contain duplicated value") {
        fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fam"))
      }
      it("should throw TestFailedException with analysis showing escaped string") {
        val e1 = intercept[exceptions.TestFailedException] {
          ecList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        e1.analysis should be (Vector("LHS contains at least one string with characters that might cause problem, the escaped string: \"\\u0000fix\""))
      }
    }

    describe("when used with not contain atMostOneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fam"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAtMostOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList should not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FAM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        }
        (fumList should not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList should not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        }
      }
      it("should do nothing when RHS contain duplicated value") {
        fumList should not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (not contain atMostOneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fam")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAtMostOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        intercept[TestFailedException] {
          fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FAM")))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FAM")))) (decided by upperCaseStringEquality)
        }
        (fumList should (not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList should (not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))
        }
      }
      it("should do nothing when RHS contain duplicated value") {
        fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with shouldNot contain atMostOneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList shouldNot contain atMostOneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList shouldNot contain atMostOneElementOf Seq("fee", "fie", "foe", "fam")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAtMostOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")))
      }
      it("should use the implicit Equality in scope") {
        implicit val use = upperCaseStringEquality
        fumList shouldNot contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList shouldNot contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM")
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList shouldNot contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList shouldNot contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM")) (decided by upperCaseStringEquality)
        }
        (fumList shouldNot contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList shouldNot contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")
        }
      }
      it("should do nothing when RHS contain duplicated value") {
        fumList shouldNot contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fum")
      }
    }

    describe("when used with shouldNot (contain atMostOneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList shouldNot (contain atMostOneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList shouldNot (contain atMostOneElementOf Seq("fee", "fie", "foe", "fam"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages.containedAtMostOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList shouldNot (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList shouldNot (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (fumList shouldNot (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList shouldNot (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        }
        (fumList shouldNot (contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList shouldNot (contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        }
      }
      it("should do nothing when RHS contain duplicated value") {
        fumList shouldNot (contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
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

    describe("when used with contain atMostOneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should contain atMostOneElementOf Seq(1, 3, 4)
        atLeast (2, lists) should contain atMostOneElementOf Seq(2, 3, 4)
        atMost (2, lists) should contain atMostOneElementOf Seq(2, 3, 4)
        no (lists) should contain atMostOneElementOf Seq(1, 2, 3)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain atMostOneElementOf Seq(2, 3, 4)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(prettifier, lists(2)) + " did not contain at most one element of " + decorateToStringValue(prettifier, Seq(2, 3, 4)) + " (ListShouldContainAtMostOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should contain atMostOneElementOf Seq("hi", "ho")
        intercept[TestFailedException] {
          all (hiLists) should contain atMostOneElementOf Seq("hi", "he")
        }

        {
          implicit val use = upperCaseStringEquality
          all (hiLists) should contain atMostOneElementOf Seq("HI", "HO")
          intercept[TestFailedException] {
            all (hiLists) should contain atMostOneElementOf Seq("HI", "HE")
          }
        }
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should contain atMostOneElementOf Seq("hi", "ho")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain atMostOneElementOf Seq("hi", "he")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain atMostOneElementOf Seq("hi", "HE")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain atMostOneElementOf Seq("hi", "he")) (decided by defaultEquality[String])
        }
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should contain atMostOneElementOf Seq(1, 3, 3, 8)
      }
    }

    describe("when used with (contain atMostOneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain atMostOneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (contain atMostOneElementOf Seq(2, 3, 4))
        atMost (2, lists) should (contain atMostOneElementOf Seq(2, 3, 4))
        no (lists) should (contain atMostOneElementOf Seq(1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atMostOneElementOf Seq(2, 3, 4))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(prettifier, lists(2)) + " did not contain at most one element of List(2, 3, 4) (ListShouldContainAtMostOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, lists)))
      }

      it("should use the implicit Equality in scope") {
        all (hiLists) should (contain atMostOneElementOf Seq("hi", "ho"))
        intercept[TestFailedException] {
          all (hiLists) should (contain atMostOneElementOf Seq("hi", "he"))
        }

        {
          implicit val ise = upperCaseStringEquality
          all (hiLists) should (contain atMostOneElementOf Seq("HI", "HO"))
          intercept[TestFailedException] {
            all (hiLists) should (contain atMostOneElementOf Seq("HI", "HE"))
          }
        }
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain atMostOneElementOf Seq("hi", "ho"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain atMostOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain atMostOneElementOf Seq("hi", "HE"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain atMostOneElementOf Seq("hi", "he"))) (decided by defaultEquality[String])
        }
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (contain atMostOneElementOf Seq(1, 3, 3, 8))
      }
    }

    describe("when used with not contain atMostOneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should not contain atMostOneElementOf (Seq("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one element of List(\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain atMostOneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))
        }
      }

      it("should use an explicitly provided Equality") {
        (all (toLists) should not contain atMostOneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        (all (toLists) should not contain atMostOneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should not contain atMostOneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }

      it("should do nothing when RHS contain duplicated value") {
        all (toLists) should not contain atMostOneElementOf (Seq("happy", "birthday", "to", "to", "you"))
      }
    }

    describe("when used with (not contain atMostOneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) should (not contain atMostOneElementOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one element of List(\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain atMostOneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        intercept[TestFailedException] {
          all (toLists) should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) should (not contain atMostOneElementOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        (all (toLists) should (not contain atMostOneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should (not contain atMostOneElementOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        }
      }
      it("should do nothing when RHS contain duplicated value") {
        all (toLists) should (not contain atMostOneElementOf (Seq("happy", "birthday", "to", "to", "you")))
      }
    }

    describe("when used with shouldNot contain atMostOneElementOf Seq(...) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot contain atMostOneElementOf Seq("happy", "birthday", "to", "you")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain atMostOneElementOf Seq("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one element of List(\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain atMostOneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM")
        }
      }

      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot contain atMostOneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        }
        (all (toLists) shouldNot contain atMostOneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) shouldNot contain atMostOneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        }
      }

      it("should do nothing when RHS contain duplicated value") {
        all (toLists) shouldNot contain atMostOneElementOf Seq("happy", "birthday", "to", "to", "you")
      }
    }

    describe("when used with shouldNot (contain atMostOneElementOf Seq(...)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (toLists) shouldNot (contain atMostOneElementOf Seq("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain atMostOneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAtMostOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, toLists(0)) + " contained at most one element of List(\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainAtMostOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, toLists)))
      }
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain atMostOneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
      }
      it("should use an explicitly provided Equality") {
        (all (toLists) shouldNot (contain atMostOneElementOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        (all (toLists) shouldNot (contain atMostOneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain atMostOneElementOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      it("should do nothing when RHS contain duplicated value") {
        all (toLists) shouldNot (contain atMostOneElementOf Seq("happy", "birthday", "to", "to", "you"))
      }
    }
  }
}
