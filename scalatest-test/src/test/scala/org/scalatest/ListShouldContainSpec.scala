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
import org.scalactic.NormalizingEquality
import org.scalactic.Uniformity
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.TestFailedException

class ListShouldContainSpec extends FunSpec {

  describe("a List") {

    val xs: List[String] = List("hi", "hi", "hi")
    val nil: List[String] = List.empty[String]
    val caseLists: List[String] = List("tell", "them", "Hi")

    describe("when used with contain (value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        xs should contain ("hi")

        val e1 = intercept[TestFailedException] {
          xs should contain ("ho")
        }
        e1.message.get should be (Resources.didNotContainExpectedElement(decorateToStringValue(xs), "\"ho\""))
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e2 = intercept[TestFailedException] {
          nil should contain ("ho")
        }
        e2.message.get should be (Resources.didNotContainExpectedElement(nil, "\"ho\""))
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        xs should contain ("hi")
        assertThrows[TestFailedException] {
          xs should contain ("ho")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should contain ("ho")
        assertThrows[TestFailedException] {
          xs should contain ("hi")
        }
      }
      it("should use an explicitly provided Equality") {
        assertThrows[TestFailedException] {
          caseLists should contain ("HI")
        }
        (caseLists should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (caseLists should contain ("HI")) (after being lowerCased)
        (caseLists should contain ("HI ")) (after being lowerCased and trimmed)
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        (xs should contain ("hi")) (decided by defaultEquality[String])
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        assertThrows[TestFailedException] {
          caseLists should contain ("HI")
        }
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
          def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
          def normalizedOrSame(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
        }
        caseLists should contain ("HI")
        normalizedInvokedCount should be (4)
      }
    }

    describe("when used with not contain value syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        xs should not contain "ho"
        nil should not contain "hi"

        val e3 = intercept[TestFailedException] {
          xs should not contain "hi"
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        xs should not contain "ho"
        assertThrows[TestFailedException] {
          xs should not contain "hi"
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should not contain "hi"
        assertThrows[TestFailedException] {
          xs should not contain "ho"
        }
      }
      it("should use an explicitly provided Equality") {
        caseLists should not contain "HI"
        caseLists should not contain "HI "
        (caseLists should not contain "HI ") (decided by defaultEquality afterBeing lowerCased)
        (caseLists should not contain "HI ") (after being lowerCased)
        assertThrows[TestFailedException] {
          (caseLists should not contain "HI") (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists should not contain "HI ") (after being lowerCased and trimmed)
        }
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        caseLists should not contain "HI"
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
          def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
          def normalizedOrSame(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
        }
        assertThrows[TestFailedException] {
          caseLists should not contain "HI"
        }
        normalizedInvokedCount should be (4)
      }
    }

    describe("when used with not (contain (value)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        xs should not (contain ("ho"))
        nil should not (contain ("hi"))

        val e3 = intercept[TestFailedException] {
          xs should not (contain ("hi"))
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        xs should not (contain ("ho"))
        assertThrows[TestFailedException] {
          xs should not (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should not (contain ("hi"))
        assertThrows[TestFailedException] {
          xs should not (contain ("ho"))
        }
      }
      it("should use an explicitly provided Equality") {
        caseLists should not (contain ("HI"))
        caseLists should not (contain ("HI "))
        (caseLists should not (contain ("HI "))) (decided by defaultEquality afterBeing lowerCased)
        (caseLists should not (contain ("HI "))) (after being lowerCased)
        assertThrows[TestFailedException] {
          (caseLists should not (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists should not (contain ("HI"))) (after being lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists should not (contain ("HI "))) (after being lowerCased and trimmed)
        }
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        caseLists should not (contain ("HI"))
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
          def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
          def normalizedOrSame(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
        }
        assertThrows[TestFailedException] {
          caseLists should not (contain ("HI"))
        }
        normalizedInvokedCount should be (4)
      }
    }

    describe("when used with (not contain value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        xs should (not contain "ho")
        nil should (not contain "hi")

        val e3 = intercept[TestFailedException] {
          xs should (not contain "hi")
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        xs should (not contain "ho")
        assertThrows[TestFailedException] {
          xs should (not contain "hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should (not contain "hi")
        assertThrows[TestFailedException] {
          xs should (not contain "ho")
        }
      }
      it("should use an explicitly provided Equality") {
        caseLists should (not contain "HI")
        caseLists should (not contain "HI ")
        (caseLists should (not contain "HI ")) (decided by defaultEquality afterBeing lowerCased)
        (caseLists should (not contain "HI ")) (after being lowerCased)
        assertThrows[TestFailedException] {
          (caseLists should (not contain "HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists should (not contain "HI")) (after being lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists should (not contain "HI ")) (after being lowerCased and trimmed)
        }
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        caseLists should (not contain "HI")
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
          def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
          def normalizedOrSame(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
        }
        assertThrows[TestFailedException] {
          caseLists should (not contain "HI")
        }
        normalizedInvokedCount should be (4)
      }
    }
    
    describe("when used with shouldNot contain value syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        xs shouldNot contain ("ho")
        nil shouldNot contain ("hi")

        val e3 = intercept[TestFailedException] {
          xs shouldNot contain ("hi")
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        xs shouldNot contain ("ho")
        assertThrows[TestFailedException] {
          xs shouldNot contain ("hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs shouldNot contain ("hi")
        assertThrows[TestFailedException] {
          xs shouldNot contain ("ho")
        }
      }
      it("should use an explicitly provided Equality") {
        caseLists shouldNot contain ("HI")
        caseLists shouldNot contain ("HI ")
        (caseLists shouldNot contain ("HI ")) (decided by defaultEquality afterBeing lowerCased)
        (caseLists shouldNot contain ("HI ")) (after being lowerCased)
        assertThrows[TestFailedException] {
          (caseLists shouldNot contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists shouldNot contain ("HI ")) (after being lowerCased and trimmed)
        }
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        caseLists shouldNot contain ("HI")
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
          def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
          def normalizedOrSame(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
        }
        assertThrows[TestFailedException] {
          caseLists shouldNot contain ("HI")
        }
        normalizedInvokedCount should be (4)
      }
    }
    
    describe("when used with shouldNot (contain (value)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        xs shouldNot (contain ("ho"))
        nil shouldNot (contain ("hi"))

        val e3 = intercept[TestFailedException] {
          xs shouldNot (contain ("hi"))
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        xs shouldNot (contain ("ho"))
        assertThrows[TestFailedException] {
          xs shouldNot (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs shouldNot (contain ("hi"))
        assertThrows[TestFailedException] {
          xs shouldNot (contain ("ho"))
        }
      }
      it("should use an explicitly provided Equality") {
        caseLists shouldNot (contain ("HI"))
        caseLists shouldNot (contain ("HI "))
        (caseLists shouldNot (contain ("HI "))) (decided by defaultEquality afterBeing lowerCased)
        (caseLists shouldNot (contain ("HI "))) (after being lowerCased)
        assertThrows[TestFailedException] {
          (caseLists shouldNot (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists shouldNot (contain ("HI"))) (after being lowerCased)
        }
        assertThrows[TestFailedException] {
          (caseLists shouldNot (contain ("HI "))) (after being lowerCased and trimmed)
        }
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        caseLists shouldNot (contain ("HI"))
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
          def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
          def normalizedOrSame(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
        }
        assertThrows[TestFailedException] {
          caseLists shouldNot (contain ("HI"))
        }
        normalizedInvokedCount should be (4)
      }
    }
  }

  describe("a collection of Lists") {

    val list123s: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
    val lists: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))
    val nils: List[List[Int]] = List(Nil, Nil, Nil)
    val mixed: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), Nil)
    val hiLists: List[List[String]] = List(List("hi"), List("hi"), List("hi"))

    describe("when used with contain (value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (list123s) should contain (1)
        atLeast (2, lists) should contain (1)
        atMost (2, lists) should contain (4)
        no (lists) should contain (7)
        no (nils) should contain (1)
        no (mixed) should contain (4)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain (1)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) did not contain element 1 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))

        val e2 = intercept[TestFailedException] {
          all (nils) should contain ("ho")
        }
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, List() did not contain element \"ho\" (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(), List(), List())"))

        val e3 = intercept[TestFailedException] {
          all (lists) should not contain (4)
        }
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e3.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) contained element 4 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))

        val e4 = intercept[TestFailedException] {
          all (lists) should contain (1)
        }
        e4.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) did not contain element 1 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))
      }
      it("should use the implicit Equality in scope") {
        assertThrows[TestFailedException] {
          all (hiLists) should contain ("ho")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should contain ("ho")
        assertThrows[TestFailedException] {
          all (hiLists) should contain ("hi")
        }
      }
      it("should use an explicitly provided Equality") {
        assertThrows[TestFailedException] {
          all (hiLists) should contain ("HI")
        }
        assertThrows[TestFailedException] {
          all (hiLists) should contain ("HI ")
        }
        (all (hiLists) should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (all (hiLists) should contain ("HI ")) (after being trimmed and lowerCased)
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        val hiHeHoLists: List[List[String]] = List(List("hi", "he", "ho"), List("hi", "he", "ho"), List("hi", "he", "ho"))
        assertThrows[TestFailedException] {
          all (hiHeHoLists) should contain ("HO")
        }
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
          def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
          def normalizedOrSame(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
        }
        all (hiHeHoLists) should contain ("HO")
        normalizedInvokedCount should be (12)
      }
    }
    describe("when used with not contain value syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (list123s) should not contain 4
        atLeast (2, lists) should not contain 4
        atMost (2, lists) should not contain 4
        no (list123s) should not contain 1 // I will recommend against double negatives, but we should test it
        all (nils) should not contain 1
        all (mixed) should not contain 4

        val e1 = intercept[TestFailedException] {
          all (lists) should not contain 6
        }
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) contained element 6 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nils) should not contain "ho"
        }
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in List(List(), List(), List())"))
      }
      it("should use the implicit Equality in scope") {
        all (hiLists) should not contain "ho"
        assertThrows[TestFailedException] {
          all (hiLists) should not contain "hi"
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should not contain "hi"
        assertThrows[TestFailedException] {
          all (hiLists) should not contain "ho"
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiLists) should not contain "HI"
        all (hiLists) should not contain "HI "
        assertThrows[TestFailedException] {
          (all (hiLists) should not contain "HI") (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (all (hiLists) should not contain "HI ") (after being trimmed and lowerCased)
        }
      }
    }
    describe("when used with not (contain (value)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (list123s) should not (contain (4))
        atLeast (2, lists) should not (contain (4))
        atMost (2, lists) should not (contain (4))
        no (list123s) should not (contain (1)) // I will recommend against double negatives, but we should test it
        all (nils) should not (contain (1))
        all (mixed) should not (contain (4))

        val e1 = intercept[TestFailedException] {
          all (lists) should not (contain (6))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) contained element 6 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nils) should not (contain ("ho"))
        }
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in List(List(), List(), List())"))
      }
      it("should use the implicit Equality in scope") {
        all (hiLists) should not (contain ("ho"))
        assertThrows[TestFailedException] {
          all (hiLists) should not (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should not (contain ("hi"))
        assertThrows[TestFailedException] {
          all (hiLists) should not (contain ("ho"))
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiLists) should not (contain ("HI"))
        all (hiLists) should not (contain ("HI "))
        assertThrows[TestFailedException] {
          (all (hiLists) should not (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (all (hiLists) should not (contain ("HI "))) (after being trimmed and lowerCased)
        }
      }
    }
    describe("when used with (not contain value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (list123s) should (not contain 4)
        atLeast (2, lists) should (not contain 4)
        atMost (2, lists) should (not contain 4)
        no (list123s) should (not contain 1) // I will recommend against double negatives, but we should test it
        all (nils) should (not contain 1)
        all (mixed) should (not contain 4)

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain 6)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) contained element 6 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nils) should (not contain "ho")
        }
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in List(List(), List(), List())"))
      }
      it("should use the implicit Equality in scope") {
        all (hiLists) should (not contain "ho")
        assertThrows[TestFailedException] {
          all (hiLists) should (not contain "hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should (not contain "hi")
        assertThrows[TestFailedException] {
          all (hiLists) should (not contain "ho")
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiLists) should (not contain "HI")
        all (hiLists) should (not contain "HI ")
        assertThrows[TestFailedException] {
          (all (hiLists) should (not contain "HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (all (hiLists) should (not contain "HI ")) (after being trimmed and lowerCased)
        }
      }
    }
    
    describe("when used with shouldNot contain value syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (list123s) shouldNot contain (4)
        atLeast (2, lists) shouldNot contain (4)
        atMost (2, lists) shouldNot contain (4)
        no (list123s) shouldNot contain (1) // I will recommend against double negatives, but we should test it
        all (nils) shouldNot contain (1)
        all (mixed) shouldNot contain (4)

        val e1 = intercept[TestFailedException] {
          all (lists) shouldNot contain (6)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) contained element 6 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nils) shouldNot contain ("ho")
        }
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in List(List(), List(), List())"))
      }
      it("should use the implicit Equality in scope") {
        all (hiLists) shouldNot contain ("ho")
        assertThrows[TestFailedException] {
          all (hiLists) shouldNot contain ("hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) shouldNot contain ("hi")
        assertThrows[TestFailedException] {
          all (hiLists) shouldNot contain ("ho")
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiLists) shouldNot contain ("HI")
        all (hiLists) shouldNot contain ("HI ")
        assertThrows[TestFailedException] {
          (all (hiLists) shouldNot contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (all (hiLists) shouldNot contain ("HI ")) (after being trimmed and lowerCased)
        }
      }
    }
    describe("when used with shouldNot (contain (value)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (list123s) shouldNot (contain (4))
        atLeast (2, lists) shouldNot (contain (4))
        atMost (2, lists) shouldNot (contain (4))
        no (list123s) shouldNot (contain (1)) // I will recommend against double negatives, but we should test it
        all (nils) shouldNot (contain (1))
        all (mixed) shouldNot (contain (4))

        val e1 = intercept[TestFailedException] {
          all (lists) shouldNot (contain (6))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, List(4, 5, 6) contained element 6 (ListShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nils) shouldNot (contain ("ho"))
        }
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in List(List(), List(), List())"))
      }
      it("should use the implicit Equality in scope") {
        all (hiLists) shouldNot (contain ("ho"))
        assertThrows[TestFailedException] {
          all (hiLists) shouldNot (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) shouldNot (contain ("hi"))
        assertThrows[TestFailedException] {
          all (hiLists) shouldNot (contain ("ho"))
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiLists) shouldNot (contain ("HI"))
        all (hiLists) shouldNot (contain ("HI "))
        assertThrows[TestFailedException] {
          (all (hiLists) shouldNot (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        assertThrows[TestFailedException] {
          (all (hiLists) shouldNot (contain ("HI "))) (after being trimmed and lowerCased)
        }
      }
    }
  }
}
