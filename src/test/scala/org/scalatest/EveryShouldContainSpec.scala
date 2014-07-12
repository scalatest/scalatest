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

import org.scalactic.{Equality, NormalizingEquality, Every}
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._

class EveryShouldContainSpec extends Spec {

  object `a List` {

    val xs: Every[String] = Every("hi", "hi", "hi")
    val caseLists: Every[String] = Every("tell", "them", "Hi")

    object `when used with contain (value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        xs should contain ("hi")

        val e1 = intercept[TestFailedException] {
          xs should contain ("ho")
        }
        e1.message.get should be (Resources("didNotContainExpectedElement", decorateToStringValue(xs), "\"ho\""))
        e1.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 4)
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
        (caseLists should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (caseLists should contain ("HI")) (after being lowerCased)
        (caseLists should contain ("HI ")) (after being lowerCased and trimmed)
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        (xs should contain ("hi")) (decided by defaultEquality[String])
      }
      @Ignore def `should minimize normalization if an implicit NormalizingEquality is in scope` {
        intercept[TestFailedException] {
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

    object `when used with not contain value syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        xs should not contain "ho"

        val e3 = intercept[TestFailedException] {
          xs should not contain "hi"
        }
        e3.message.get should be (Resources("containedExpectedElement", decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
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
        (caseLists should not contain "HI ") (decided by defaultEquality afterBeing lowerCased)
        (caseLists should not contain "HI ") (after being lowerCased)
        intercept[TestFailedException] {
          (caseLists should not contain "HI") (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists should not contain "HI ") (after being lowerCased and trimmed)
        }
      }
      @Ignore def `should minimize normalization if an implicit NormalizingEquality is in scope` {
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
        intercept[TestFailedException] {
          caseLists should not contain "HI"
        }
        normalizedInvokedCount should be (4)
      }
    }

    object `when used with not (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        xs should not (contain ("ho"))

        val e3 = intercept[TestFailedException] {
          xs should not (contain ("hi"))
        }
        e3.message.get should be (Resources("containedExpectedElement", decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
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
        (caseLists should not (contain ("HI "))) (decided by defaultEquality afterBeing lowerCased)
        (caseLists should not (contain ("HI "))) (after being lowerCased)
        intercept[TestFailedException] {
          (caseLists should not (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists should not (contain ("HI"))) (after being lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists should not (contain ("HI "))) (after being lowerCased and trimmed)
        }
      }
      @Ignore def `should minimize normalization if an implicit NormalizingEquality is in scope` {
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
        intercept[TestFailedException] {
          caseLists should not (contain ("HI"))
        }
        normalizedInvokedCount should be (4)
      }
    }

    object `when used with (not contain value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        xs should (not contain "ho")

        val e3 = intercept[TestFailedException] {
          xs should (not contain "hi")
        }
        e3.message.get should be (Resources("containedExpectedElement", decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
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
        (caseLists should (not contain "HI ")) (decided by defaultEquality afterBeing lowerCased)
        (caseLists should (not contain "HI ")) (after being lowerCased)
        intercept[TestFailedException] {
          (caseLists should (not contain "HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists should (not contain "HI")) (after being lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists should (not contain "HI ")) (after being lowerCased and trimmed)
        }
      }
      @Ignore def `should minimize normalization if an implicit NormalizingEquality is in scope` {
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
        intercept[TestFailedException] {
          caseLists should (not contain "HI")
        }
        normalizedInvokedCount should be (4)
      }
    }

    object `when used with shouldNot contain value syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        xs shouldNot contain ("ho")

        val e3 = intercept[TestFailedException] {
          xs shouldNot contain ("hi")
        }
        e3.message.get should be (Resources("containedExpectedElement", decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        xs shouldNot contain ("ho")
        intercept[TestFailedException] {
          xs shouldNot contain ("hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs shouldNot contain ("hi")
        intercept[TestFailedException] {
          xs shouldNot contain ("ho")
        }
      }
      def `should use an explicitly provided Equality` {
        caseLists shouldNot contain ("HI")
        caseLists shouldNot contain ("HI ")
        (caseLists shouldNot contain ("HI ")) (decided by defaultEquality afterBeing lowerCased)
        (caseLists shouldNot contain ("HI ")) (after being lowerCased)
        intercept[TestFailedException] {
          (caseLists shouldNot contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists shouldNot contain ("HI ")) (after being lowerCased and trimmed)
        }
      }
      @Ignore def `should minimize normalization if an implicit NormalizingEquality is in scope` {
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
        intercept[TestFailedException] {
          caseLists shouldNot contain ("HI")
        }
        normalizedInvokedCount should be (4)
      }
    }

    object `when used with shouldNot (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        xs shouldNot (contain ("ho"))

        val e3 = intercept[TestFailedException] {
          xs shouldNot (contain ("hi"))
        }
        e3.message.get should be (Resources("containedExpectedElement", decorateToStringValue(xs), "\"hi\""))
        e3.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        xs shouldNot (contain ("ho"))
        intercept[TestFailedException] {
          xs shouldNot (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        xs shouldNot (contain ("hi"))
        intercept[TestFailedException] {
          xs shouldNot (contain ("ho"))
        }
      }
      def `should use an explicitly provided Equality` {
        caseLists shouldNot (contain ("HI"))
        caseLists shouldNot (contain ("HI "))
        (caseLists shouldNot (contain ("HI "))) (decided by defaultEquality afterBeing lowerCased)
        (caseLists shouldNot (contain ("HI "))) (after being lowerCased)
        intercept[TestFailedException] {
          (caseLists shouldNot (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists shouldNot (contain ("HI"))) (after being lowerCased)
        }
        intercept[TestFailedException] {
          (caseLists shouldNot (contain ("HI "))) (after being lowerCased and trimmed)
        }
      }
      @Ignore def `should minimize normalization if an implicit NormalizingEquality is in scope` {
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
        intercept[TestFailedException] {
          caseLists shouldNot (contain ("HI"))
        }
        normalizedInvokedCount should be (4)
      }
    }
  }

  object `a collection of Lists` {

    val list123s: Every[Every[Int]] = Every(Every(1, 2, 3), Every(1, 2, 3), Every(1, 2, 3))
    val lists: Every[Every[Int]] = Every(Every(1, 2, 3), Every(1, 2, 3), Every(4, 5, 6))
    val hiLists: Every[Every[String]] = Every(Every("hi"), Every("hi"), Every("hi"))

    object `when used with contain (value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (list123s) should contain (1)
        atLeast (2, lists) should contain (1)
        atMost (2, lists) should contain (4)
        no (lists) should contain (7)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain (1)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) did not contain element 1 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))

        val e2 = intercept[TestFailedException] {
          all (lists) should not contain (4)
        }
        e2.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) contained element 4 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))

        val e3 = intercept[TestFailedException] {
          all (lists) should contain (1)
        }
        e3.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e3.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) did not contain element 1 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))
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
        intercept[TestFailedException] {
          all (hiLists) should contain ("HI")
        }
        intercept[TestFailedException] {
          all (hiLists) should contain ("HI ")
        }
        (all (hiLists) should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (all (hiLists) should contain ("HI ")) (after being trimmed and lowerCased)
      }
      @Ignore def `should minimize normalization if an implicit NormalizingEquality is in scope` {
        val hiHeHoLists: List[List[String]] = List(List("hi", "he", "ho"), List("hi", "he", "ho"), List("hi", "he", "ho"))
        intercept[TestFailedException] {
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
    object `when used with not contain value syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (list123s) should not contain 4
        atLeast (2, lists) should not contain 4
        atMost (2, lists) should not contain 4
        no (list123s) should not contain 1 // I will recommend against double negatives, but we should test it

        val e1 = intercept[TestFailedException] {
          all (lists) should not contain 6
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) contained element 6 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))
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
      def `should use an explicitly provided Equality` {
        all (hiLists) should not contain "HI"
        all (hiLists) should not contain "HI "
        intercept[TestFailedException] {
          (all (hiLists) should not contain "HI") (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiLists) should not contain "HI ") (after being trimmed and lowerCased)
        }
      }
    }
    object `when used with not (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (list123s) should not (contain (4))
        atLeast (2, lists) should not (contain (4))
        atMost (2, lists) should not (contain (4))
        no (list123s) should not (contain (1)) // I will recommend against double negatives, but we should test it

        val e1 = intercept[TestFailedException] {
          all (lists) should not (contain (6))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) contained element 6 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))
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
      def `should use an explicitly provided Equality` {
        all (hiLists) should not (contain ("HI"))
        all (hiLists) should not (contain ("HI "))
        intercept[TestFailedException] {
          (all (hiLists) should not (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiLists) should not (contain ("HI "))) (after being trimmed and lowerCased)
        }
      }
    }
    object `when used with (not contain value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (list123s) should (not contain 4)
        atLeast (2, lists) should (not contain 4)
        atMost (2, lists) should (not contain 4)
        no (list123s) should (not contain 1) // I will recommend against double negatives, but we should test it

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain 6)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) contained element 6 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))
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
      def `should use an explicitly provided Equality` {
        all (hiLists) should (not contain "HI")
        all (hiLists) should (not contain "HI ")
        intercept[TestFailedException] {
          (all (hiLists) should (not contain "HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiLists) should (not contain "HI ")) (after being trimmed and lowerCased)
        }
      }
    }

    object `when used with shouldNot contain value syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (list123s) shouldNot contain (4)
        atLeast (2, lists) shouldNot contain (4)
        atMost (2, lists) shouldNot contain (4)
        no (list123s) shouldNot contain (1) // I will recommend against double negatives, but we should test it

        val e1 = intercept[TestFailedException] {
          all (lists) shouldNot contain (6)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) contained element 6 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))
      }
      def `should use the implicit Equality in scope` {
        all (hiLists) shouldNot contain ("ho")
        intercept[TestFailedException] {
          all (hiLists) shouldNot contain ("hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) shouldNot contain ("hi")
        intercept[TestFailedException] {
          all (hiLists) shouldNot contain ("ho")
        }
      }
      def `should use an explicitly provided Equality` {
        all (hiLists) shouldNot contain ("HI")
        all (hiLists) shouldNot contain ("HI ")
        intercept[TestFailedException] {
          (all (hiLists) shouldNot contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiLists) shouldNot contain ("HI ")) (after being trimmed and lowerCased)
        }
      }
    }
    object `when used with shouldNot (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (list123s) shouldNot (contain (4))
        atLeast (2, lists) shouldNot (contain (4))
        atMost (2, lists) shouldNot (contain (4))
        no (list123s) shouldNot (contain (1)) // I will recommend against double negatives, but we should test it

        val e1 = intercept[TestFailedException] {
          all (lists) shouldNot (contain (6))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, Many(4, 5, 6) contained element 6 (EveryShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in Many(Many(1, 2, 3), Many(1, 2, 3), Many(4, 5, 6))"))
      }
      def `should use the implicit Equality in scope` {
        all (hiLists) shouldNot (contain ("ho"))
        intercept[TestFailedException] {
          all (hiLists) shouldNot (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiLists) shouldNot (contain ("hi"))
        intercept[TestFailedException] {
          all (hiLists) shouldNot (contain ("ho"))
        }
      }
      def `should use an explicitly provided Equality` {
        all (hiLists) shouldNot (contain ("HI"))
        all (hiLists) shouldNot (contain ("HI "))
        intercept[TestFailedException] {
          (all (hiLists) shouldNot (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiLists) shouldNot (contain ("HI "))) (after being trimmed and lowerCased)
        }
      }
    }
  }
}
