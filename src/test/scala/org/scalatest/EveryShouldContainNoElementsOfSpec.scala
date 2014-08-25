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

import org.scalactic.{Equality, Every, One, Many}
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._

class EveryShouldContainNoElementsOfSpec extends Spec {

  val upperCaseEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  //ADDITIONAL//

  object `an Every` {

    val fumList: Every[String] = Every("fum")
    val toList: Every[String] = Every("to")

    object `when used with contain noElementsOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should contain noElementsOf Seq("fee", "fie", "foe", "fam")
        val e1 = intercept[TestFailedException] {
          fumList should contain noElementsOf Seq("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("containedAtLeastOneOf", fumList, Seq("fee", "fie", "foe", "fum")))
        // Contains duplicate elements in the right list
        val e2 = intercept[NotAllowedException] {
          fumList should contain noElementsOf Seq("fee", "fam", "foe", "fam")
        }
        e2.getMessage should be (FailureMessages("noElementsOfDuplicate"))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        fumList should contain noElementsOf Seq("FEE", "FAM", "FOE", "FU")
        intercept[TestFailedException] {
          fumList should contain noElementsOf Seq("FEE", "FUM", "FOE", "FU")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should contain noElementsOf Seq("FEE", "FAM", "FOE", "FU")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should contain noElementsOf Seq("FEE", "FUM", "FOE", "FU")) (decided by upperCaseEquality)
        }
        intercept[TestFailedException] {
          (fumList should contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        }
        (fumList should contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM ")) (after being lowerCased and trimmed)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with (contain noElementsOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fam"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("containedAtLeastOneOf", fumList, Seq("fee", "fie", "foe", "fum")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        fumList should (contain noElementsOf Seq("FEE", "FAM", "FOE", "FU"))
        intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("FEE", "FUM", "FOE", "FU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (contain noElementsOf Seq("FEE", "FAM", "FOE", "FU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq("FEE", "FUM", "FOE", "FU"))) (decided by upperCaseEquality)
        }
        fumList should (contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with not contain noElementsOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should not contain noElementsOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should not contain noElementsOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("didNotContainAtLeastOneOf", fumList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        fumList should not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList should not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        }
        (fumList should not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList should not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with (not contain noElementsOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not contain noElementsOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          toList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("didNotContainAtLeastOneOf", toList, Seq("fee", "fie", "foe", "fum")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        toList should (not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        intercept[TestFailedException] {
          toList should (not contain noElementsOf (Seq("happy", "birthday", "to", "you")))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toList should (not contain noElementsOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseEquality)
        }
        (toList should (not contain noElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          toList should (not contain noElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with shouldNot contain noElementsOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList shouldNot contain noElementsOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList shouldNot contain noElementsOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("didNotContainAtLeastOneOf", fumList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        fumList shouldNot contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList shouldNot contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList shouldNot contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (fumList shouldNot contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        }
        (fumList shouldNot contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          fumList shouldNot contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList shouldNot contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with shouldNot (contain noElementsOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot (contain noElementsOf Seq("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("didNotContainAtLeastOneOf", toList, Seq("fee", "fie", "foe", "fum")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        toList shouldNot (contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          toList shouldNot (contain noElementsOf Seq("happy", "birthday", "to", "you"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot (contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain noElementsOf Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (toList shouldNot (contain noElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          toList shouldNot (contain noElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot (contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }
  }

  object `an every of Everys` {

    val list1s: Every[Every[Int]] = Every(Every(1), Every(1), Every(1))
    val lists: Every[Every[Int]] = Every(Every(1), Every(1), Every(2))
    val hiLists: Every[Every[String]] = Every(Every("hi"), Every("hi"), Every("hi"))
    val toLists: Every[Every[String]] = Every(Every("to"), Every("to"), Every("to"))

    object `when used with contain noElementsOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should contain noElementsOf Seq(2, 3, 4)
        atLeast (2, lists) should contain noElementsOf Seq(8, 3, 4)
        atMost (2, lists) should contain noElementsOf Seq(2, 3, 4)
        no (lists) should contain noElementsOf Seq(1, 2, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain noElementsOf Seq(2, 3, 4)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages("containedAtLeastOneOf", lists(2), Seq(2, 3, 4)) + " (EveryShouldContainNoElementsOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should contain noElementsOf Seq("ho", "he")
        intercept[TestFailedException] {
          all (hiLists) should contain noElementsOf Seq("hi", "he")
        }
        implicit val ise = upperCaseEquality
        all (hiLists) should contain noElementsOf Seq("hi", "he")
        intercept[TestFailedException] {
          all (hiLists) should contain noElementsOf Seq("HI", "HE")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should contain noElementsOf Seq("hi", "he")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain noElementsOf Seq("HI", "HE")) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should contain noElementsOf Seq("ho", "he")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain noElementsOf Seq("hi", "he")) (decided by defaultEquality[String])
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should contain noElementsOf Seq(1, 2, 2, 3)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with (contain noElementsOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain noElementsOf Seq(2, 3, 4))
        atLeast (2, lists) should (contain noElementsOf Seq(2, 3, 4))
        atMost (2, lists) should (contain noElementsOf Seq(2, 3, 4))
        no (lists) should (contain noElementsOf Seq(1, 2, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain noElementsOf Seq(2, 3, 4))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + FailureMessages("containedAtLeastOneOf", lists(2), Seq(2, 3, 4)) + " (EveryShouldContainNoElementsOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (contain noElementsOf Seq("ho", "he"))
        intercept[TestFailedException] {
          all (hiLists) should (contain noElementsOf Seq("hi", "he"))
        }
        implicit val ise = upperCaseEquality
        all (hiLists) should (contain noElementsOf Seq("hi", "he"))
        intercept[TestFailedException] {
          all (hiLists) should (contain noElementsOf Seq("HI", "HE"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain noElementsOf Seq("hi", "he"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain noElementsOf Seq("HI", "HE"))) (decided by upperCaseEquality)
        }
        implicit val ise = upperCaseEquality
        (all (hiLists) should (contain noElementsOf Seq("ho", "he"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain noElementsOf Seq("hi", "he"))) (decided by defaultEquality[String])
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain noElementsOf Seq(1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with not contain noElementsOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not contain noElementsOf (Seq("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain noElementsOf (Seq("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages("didNotContainAtLeastOneOf", toLists(0), Seq("fee", "fie", "foe", "fum")) + " (EveryShouldContainNoElementsOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        all (toLists) should not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should not contain noElementsOf (Seq("happy", "birthday", "to", "you"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain noElementsOf (Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (all (toLists) should not contain noElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should not contain noElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with (not contain noElementsOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not contain noElementsOf (Seq("happy", "birthday", "to", "you")))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages("didNotContainAtLeastOneOf", toLists(0), Seq("fee", "fie", "foe", "fum")) + " (EveryShouldContainNoElementsOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        all (toLists) should (not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))
        intercept[TestFailedException] {
          all (toLists) should (not contain noElementsOf (Seq("happy", "birthday", "to", "you")))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not contain noElementsOf (Seq("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain noElementsOf (Seq("happy", "birthday", "to", "you")))) (decided by upperCaseEquality)
        }
        (all (toLists) should (not contain noElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) should (not contain noElementsOf (Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with shouldNot contain noElementsOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot contain noElementsOf Seq("happy", "birthday", "to", "you")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain noElementsOf Seq("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages("didNotContainAtLeastOneOf", toLists(0), Seq("fee", "fie", "foe", "fum")) + " (EveryShouldContainNoElementsOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        all (toLists) shouldNot contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain noElementsOf Seq("happy", "birthday", "to", "you")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain noElementsOf Seq("happy", "birthday", "to", "you")) (decided by upperCaseEquality)
        }
        (all (toLists) shouldNot contain noElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) shouldNot contain noElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }

    object `when used with shouldNot (contain noElementsOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot (contain noElementsOf Seq("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + FailureMessages("didNotContainAtLeastOneOf", toLists(0), Seq("fee", "fie", "foe", "fum")) + " (EveryShouldContainNoElementsOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseEquality
        all (toLists) shouldNot (contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain noElementsOf Seq("happy", "birthday", "to", "you"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot  (contain noElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain noElementsOf Seq("happy", "birthday", "to", "you"))) (decided by upperCaseEquality)
        }
        (all (toLists) shouldNot  (contain noElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain noElementsOf Seq(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainNoElementsOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("noElementsOfDuplicate")))
      }
    }
  }
}
