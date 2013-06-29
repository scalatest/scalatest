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
import org.scalautils.Normalizer
import org.scalautils.StringNormalizers._
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ListShouldContainNoneOfSpec extends Spec with Matchers {

  val upperCaseEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }
  
  //ADDITIONAL//

  object `a List` {

    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")

    object `when used with contain noneOf (...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should contain noneOf ("fee", "fie", "foe", "fam")
        val e1 = intercept[TestFailedException] {
          fumList should contain noneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""))
        // Contains duplicate elements in the right list
        val e2 = intercept[IllegalArgumentException] {
          fumList should contain noneOf ("fee", "fam", "foe", "fam")
        }
        e2.getMessage should be (Resources("noneOfDuplicate", "\"fam\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        fumList should contain noneOf ("FEE", "FAM", "FOE", "FU")
        intercept[TestFailedException] {
          fumList should contain noneOf ("FEE", "FUM", "FOE", "FU")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should contain noneOf ("FEE", "FAM", "FOE", "FU")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should contain noneOf ("FEE", "FUM", "FOE", "FU")) (decided by upperCaseEquality)
        }
        intercept[TestFailedException] {
          (fumList should contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        }
        (fumList should contain noneOf (" FEE ", " FIE ", " FOE ", " FAM ")) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain noneOf (...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (contain noneOf ("fee", "fie", "foe", "fam"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        fumList should (contain noneOf ("FEE", "FAM", "FOE", "FU"))
        intercept[TestFailedException] {
          fumList should (contain noneOf ("FEE", "FUM", "FOE", "FU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (contain noneOf ("FEE", "FAM", "FOE", "FU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should (contain noneOf ("FEE", "FUM", "FOE", "FU"))) (decided by upperCaseEquality)
        }
        fumList should (contain noneOf (" FEE ", " FIE ", " FOE ", " FUM "))
        intercept[TestFailedException] {
          (fumList should (contain noneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with not contain noneOf (...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should not contain noneOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should not contain noneOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        fumList should not contain noneOf ("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should not contain noneOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        }
        (fumList should not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList should not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
      }
    }

    object `when used with (not contain noneOf (...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not contain noneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toList should (not contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainOneOfElements", decorateToStringValue(toList), "\"fee\", \"fie\", \"foe\", \"fum\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        toList should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          toList should (not contain noneOf ("happy", "birthday", "to", "you"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toList should (not contain noneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (toList should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          toList should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
    }
  }

  object `a collection of Lists` {

    val list1s: Vector[List[Int]] = Vector(List(1), List(1), List(1))
    val lists: Vector[List[Int]] = Vector(List(1), List(1), List(2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1), List(1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi"), List("hi"), List("hi"))
    val toLists: Vector[List[String]] = Vector(List("to"), List("to"), List("to"))

    object `when used with contain noneOf (...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should contain noneOf (2, 3, 4)
        atLeast (2, lists) should contain noneOf (8, 3, 4)
        atMost (2, lists) should contain noneOf (2, 3, 4)
        no (lists) should contain noneOf (1, 2, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain noneOf (2, 3, 4)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(2)) + " contained one of (2, 3, 4) (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should contain noneOf ("ho")
        intercept[TestFailedException] {
          all (hiLists) should contain noneOf ("hi")
        }
        implicit val ise = upperCaseEquality
        all (hiLists) should contain noneOf ("hi")
        intercept[TestFailedException] {
          all (hiLists) should contain noneOf ("HI")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should contain noneOf ("hi")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain noneOf ("HI")) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should contain noneOf ("ho")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain noneOf ("hi")) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with (contain noneOf (...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain noneOf (2, 3, 4))
        atLeast (2, lists) should (contain noneOf (2, 3, 4))
        atMost (2, lists) should (contain noneOf (2, 3, 4))
        no (lists) should (contain noneOf (1, 2, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain noneOf (2, 3, 4))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(2)) + " contained one of (2, 3, 4) (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (contain noneOf ("ho"))
        intercept[TestFailedException] {
          all (hiLists) should (contain noneOf ("hi"))
        }
        implicit val ise = upperCaseEquality
        all (hiLists) should (contain noneOf ("hi"))
        intercept[TestFailedException] {
          all (hiLists) should (contain noneOf ("HI"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain noneOf ("hi"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain noneOf ("HI"))) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should (contain noneOf ("ho"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain noneOf ("hi"))) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with not contain noneOf (...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not contain noneOf ("happy", "birthday", "to", "you")
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain noneOf ("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to")) + " did not contain one of (\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        all (toLists) should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        intercept[TestFailedException] {
          all (toLists) should not contain noneOf ("happy", "birthday", "to", "you")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain noneOf ("happy", "birthday", "to", "you")) (decided by upperCaseEquality)
        }
        (all (toLists) should not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        }
      }
    }

    object `when used with (not contain noneOf (...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not contain noneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain noneOf ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainNoneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to")) + " did not contain one of (\"fee\", \"fie\", \"foe\", \"fum\") (ListShouldContainNoneOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        all (toLists) should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should (not contain noneOf ("happy", "birthday", "to", "you"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not contain noneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain noneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (all (toLists) should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should (not contain noneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
    }
  }
}
