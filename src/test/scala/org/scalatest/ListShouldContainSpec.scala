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
import org.scalautils.NormalizingEquality
import org.scalautils.Normalization
import org.scalautils.StringNormalizations._
import SharedHelpers._

class ListShouldContainSpec extends Spec with Matchers {

  object `a List` {

    val xs: List[String] = List("hi", "hi", "hi")
    val nil: List[String] = List.empty[String]
    val caseLists: List[String] = List("tell", "them", "Hi")

    object `when used with contain (value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        xs should contain ("hi")

        val e1 = intercept[TestFailedException] {
          xs should contain ("ho")
        }
        e1.message.get should be (Resources("didNotContainExpectedElement", xs, "\"ho\""))
        e1.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e2 = intercept[TestFailedException] {
          nil should contain ("ho")
        }
        e2.message.get should be (Resources("didNotContainExpectedElement", nil, "\"ho\""))
        e2.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        xs should contain ("hi")
        intercept[TestFailedException] {
          xs should contain ("ho")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should contain ("ho")
        intercept[TestFailedException] {
          xs should contain ("hi")
        }
      }
      def `should use an explicitly provided Equality` {
        intercept[TestFailedException] {
          caseLists should contain ("HI")
        }
        (caseLists should contain ("HI")) (withGenTraversableElementEquality(decided by defaultEquality afterBeing lowerCased))
        (caseLists should contain ("HI")) (withGenTraversableElementEquality(decided afterBeing lowerCased))
        (caseLists should contain ("HI ")) (withGenTraversableElementEquality(decided afterBeing lowerCased and trimmed))
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        (xs should contain ("hi")) (withGenTraversableElementEquality(decided by defaultEquality))
      }
      def `should minimize normalization if an implicit NormalizingEquality is in scope` {
        intercept[TestFailedException] {
          caseLists should contain ("HI")
        }
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalizedIfInstanceOfA(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
        }
        caseLists should contain ("HI")
        normalizedInvokedCount should be (4)
      }
    }

    object `when used with not contain value syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        xs should not contain "ho"
        nil should not contain "hi"

        val e3 = intercept[TestFailedException] {
          xs should not contain "hi"
        }
        e3.message.get should be (Resources("containedExpectedElement", xs, "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        xs should not contain "ho"
        intercept[TestFailedException] {
          xs should not contain "hi"
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should not contain "hi"
        intercept[TestFailedException] {
          xs should not contain "ho"
        }
      }
      def `should use an explicitly provided Equality` {
        caseLists should not contain "HI"
        caseLists should not contain "HI "
        (caseLists should not contain "HI ") (withGenTraversableElementEquality(decided by defaultEquality afterBeing lowerCased))
        (caseLists should not contain "HI ") (withGenTraversableElementEquality(decided afterBeing lowerCased))
        intercept[TestFailedException] {
          (caseLists should not contain "HI") (withGenTraversableElementEquality(decided by defaultEquality afterBeing lowerCased))
        }
        intercept[TestFailedException] {
          (caseLists should not contain "HI") (withGenTraversableElementEquality(decided afterBeing lowerCased))
        }
        intercept[TestFailedException] {
          (caseLists should not contain "HI ") (withGenTraversableElementEquality(decided afterBeing lowerCased and trimmed))
        }
      }
      def `should minimize normalization if an implicit NormalizingEquality is in scope` {
        caseLists should not contain "HI"
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalizedIfInstanceOfA(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
        }
        intercept[TestFailedException] {
          caseLists should not contain "HI"
        }
        normalizedInvokedCount should be (4)
      }
    }

    object `when used with not (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        xs should not (contain ("ho"))
        nil should not (contain ("hi"))

        val e3 = intercept[TestFailedException] {
          xs should not (contain ("hi"))
        }
        e3.message.get should be (Resources("containedExpectedElement", xs, "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        xs should not (contain ("ho"))
        intercept[TestFailedException] {
          xs should not (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should not (contain ("hi"))
        intercept[TestFailedException] {
          xs should not (contain ("ho"))
        }
      }
      def `should use an explicitly provided Equality` {
        caseLists should not (contain ("HI"))
        caseLists should not (contain ("HI "))
        (caseLists should not (contain ("HI "))) (withGenTraversableElementEquality(decided by defaultEquality afterBeing lowerCased))
        (caseLists should not (contain ("HI "))) (withGenTraversableElementEquality(decided afterBeing lowerCased))
        intercept[TestFailedException] {
          (caseLists should not (contain ("HI"))) (withGenTraversableElementEquality(decided by defaultEquality afterBeing lowerCased))
        }
        intercept[TestFailedException] {
          (caseLists should not (contain ("HI"))) (withGenTraversableElementEquality(decided afterBeing lowerCased))
        }
        intercept[TestFailedException] {
          (caseLists should not (contain ("HI "))) (withGenTraversableElementEquality(decided afterBeing lowerCased and trimmed))
        }
      }
      def `should minimize normalization if an implicit NormalizingEquality is in scope` {
        caseLists should not (contain ("HI"))
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalizedIfInstanceOfA(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
        }
        intercept[TestFailedException] {
          caseLists should not (contain ("HI"))
        }
        normalizedInvokedCount should be (4)
      }
    }

    object `when used with (not contain value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        xs should (not contain "ho")
        nil should (not contain "hi")

        val e3 = intercept[TestFailedException] {
          xs should (not contain "hi")
        }
        e3.message.get should be (Resources("containedExpectedElement", xs, "\"hi\""))
        e3.failedCodeFileName.get should be ("ListShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        xs should (not contain "ho")
        intercept[TestFailedException] {
          xs should (not contain "hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs should (not contain "hi")
        intercept[TestFailedException] {
          xs should (not contain "ho")
        }
      }
      def `should use an explicitly provided Equality` {
        caseLists should (not contain "HI")
        caseLists should (not contain "HI ")
        (caseLists should (not contain "HI ")) (withGenTraversableElementEquality(decided by defaultEquality afterBeing lowerCased))
        (caseLists should (not contain "HI ")) (withGenTraversableElementEquality(decided afterBeing lowerCased))
        intercept[TestFailedException] {
          (caseLists should (not contain "HI")) (withGenTraversableElementEquality(decided by defaultEquality afterBeing lowerCased))
        }
        intercept[TestFailedException] {
          (caseLists should (not contain "HI")) (withGenTraversableElementEquality(decided afterBeing lowerCased))
        }
        intercept[TestFailedException] {
          (caseLists should (not contain "HI ")) (withGenTraversableElementEquality(decided afterBeing lowerCased and trimmed))
        }
      }
      def `should minimize normalization if an implicit NormalizingEquality is in scope` {
        caseLists should (not contain "HI")
        var normalizedInvokedCount = 0
        implicit val e = new NormalizingEquality[String] {
          def normalizedIfInstanceOfA(b: Any) =
            b match {
              case s: String => normalized(s)
              case _ => b
            }
          def normalized(s: String): String = {
            normalizedInvokedCount += 1
            s.toLowerCase
          }
        }
        intercept[TestFailedException] {
          caseLists should (not contain "HI")
        }
        normalizedInvokedCount should be (4)
      }
    }
  }

  object `a collection of Lists` {

    val list123s: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
    val lists: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))
    val nils: List[List[Int]] = List(Nil, Nil, Nil)
    val mixed: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), Nil)
    val hiLists: List[List[String]] = List(List("hi"), List("hi"), List("hi"))

    object `when used with contain (value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

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
      def `should use the implicit Equality in scope` {
        intercept[TestFailedException] {
          all (hiLists) should contain ("ho")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should contain ("ho")
        intercept[TestFailedException] {
          all (hiLists) should contain ("hi")
        }
      }
      def `should use an explicitly provided Equality` {
        val mappedToLowerCase: Normalization[List[String]] =
          new Normalization[List[String]] {
            def normalized(xs: List[String]): List[String] = xs.map(_.toLowerCase)
            def normalizedIfInstanceOfA(xs: Any): Any =
              xs match {
                case list: List[_] =>
                  list map {
                    case s: String => s.toLowerCase
                    case other => other
                  }
                case other => other
              }
          }
        val mappedToTrimmed: Normalization[List[String]] =
          new Normalization[List[String]] {
            def normalized(xs: List[String]): List[String] = xs.map(_.toLowerCase)
            def normalizedIfInstanceOfA(xs: Any): Any =
              xs match {
                case list: List[_] =>
                  list map {
                    case s: String => s.trim
                    case other => other
                  }
                case other => other
              }
          }
        intercept[TestFailedException] {
          all (hiLists) should contain (List("HI"))
        }
        intercept[TestFailedException] {
          all (hiLists) should contain (List("HI "))
        }
        (hiLists should contain (List("HI"))) (withGenTraversableElementEquality(decided by defaultEquality afterBeing mappedToLowerCase))
        (hiLists should contain (List("HI "))) (withGenTraversableElementEquality(decided afterBeing mappedToTrimmed and mappedToLowerCase))
      }
    }
    object `when used with not contain value syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

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
      def `should use the implicit Equality in scope` {
        all (hiLists) should not contain "ho"
        intercept[TestFailedException] {
          all (hiLists) should not contain "hi"
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should not contain "hi"
        intercept[TestFailedException] {
          all (hiLists) should not contain "ho"
        }
      }
    }
    object `when used with not (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

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
      def `should use the implicit Equality in scope` {
        all (hiLists) should not (contain ("ho"))
        intercept[TestFailedException] {
          all (hiLists) should not (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should not (contain ("hi"))
        intercept[TestFailedException] {
          all (hiLists) should not (contain ("ho"))
        }
      }
    }
    object `when used with (not contain value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

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
      def `should use the implicit Equality in scope` {
        all (hiLists) should (not contain "ho")
        intercept[TestFailedException] {
          all (hiLists) should (not contain "hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) should (not contain "hi")
        intercept[TestFailedException] {
          all (hiLists) should (not contain "ho")
        }
      }
    }
  }
}
