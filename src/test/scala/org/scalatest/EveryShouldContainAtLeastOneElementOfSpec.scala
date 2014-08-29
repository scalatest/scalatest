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
import org.scalactic.Uniformity
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._

class EveryShouldContainAtLeastOneElementOfSpec extends Spec {

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  //ADDITIONAL//

  object `an Every` {

    val fumList: Every[String] = Every("fum")
    val toList: Every[String] = Every("to")

    object `when used with contain atLeastOneElementOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should contain atLeastOneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should contain atLeastOneElementOf Seq("fum", "foe")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain atLeastOneElementOf Seq("fum", "foe")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    object `when used with (contain atLeastOneElementOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fum", "foe"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain atLeastOneElementOf Seq("fum", "foe"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    /*
     I purposely don't want to support this syntax:

            fumList should contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
            fumList should (contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum")))

     Reason is that I don't want people putting parentheses between contain and atLeastOneElementOf, etc. This will not compile.
    */
    object `when used with not contain atLeastOneElementOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toList should not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("containedAtLeastOneElementOf", toList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should not contain atLeastOneElementOf (Seq("to", "you"))
        intercept[TestFailedException] {
          toList should not contain atLeastOneElementOf (Seq("TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should not contain atLeastOneElementOf (Seq("to", "you"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not contain atLeastOneElementOf (Seq("TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should not contain atLeastOneElementOf (Seq(" TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should not contain atLeastOneElementOf (Seq(" TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    /*
    Interesting, of these three, the top one does happen to compile and run:

            toList should not contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
            // toList should not (contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum")))
            // toList should (not (contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))))

    The bottom two don't, but still I don't want to support that in general.
    */
    object `when used with (not contain atLeastOneElementOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          toList should (not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("containedAtLeastOneElementOf", toList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should (not contain atLeastOneElementOf (Seq("to", "you")))
        intercept[TestFailedException] {
          toList should (not contain atLeastOneElementOf (Seq("TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not contain atLeastOneElementOf (Seq("to", "you")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not contain atLeastOneElementOf (Seq("TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        toList should (not contain atLeastOneElementOf (Seq(" TO ", " YOU ")))
        intercept[TestFailedException] {
          (toList should (not contain atLeastOneElementOf (Seq(" TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    object `when used with shouldNot contain atLeastOneElementOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain atLeastOneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("containedAtLeastOneElementOf", toList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot contain atLeastOneElementOf Seq("to", "you")
        intercept[TestFailedException] {
          toList shouldNot contain atLeastOneElementOf Seq("TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot contain atLeastOneElementOf Seq("to", "you")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain atLeastOneElementOf Seq("TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList shouldNot contain atLeastOneElementOf Seq(" TO ", " YOU ")
        intercept[TestFailedException] {
          (toList shouldNot contain atLeastOneElementOf Seq(" TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    object `when used with shouldNot (contain atLeastOneElementOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (FailureMessages("containedAtLeastOneElementOf", toList, Seq("happy", "birthday", "to", "you")))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot (contain atLeastOneElementOf Seq("to", "you"))
        intercept[TestFailedException] {
          toList shouldNot (contain atLeastOneElementOf Seq("TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot (contain atLeastOneElementOf Seq("to", "you"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain atLeastOneElementOf Seq("TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList shouldNot (contain atLeastOneElementOf Seq(" TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot (contain atLeastOneElementOf Seq(" TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }
  }

  object `an every of Everys` {

    val list1s: Every[Every[Int]] = Every(Every(1), Every(1), Every(1))
    val lists: Every[Every[Int]] = Every(Every(1), Every(1), Every(2))
    val hiLists: Every[Every[String]] = Every(Every("hi"), Every("hi"), Every("hi"))
    val toLists: Every[Every[String]] = Every(Every("to"), Every("to"), Every("to"))

    object `when used with contain atLeastOneElementOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should contain atLeastOneElementOf Seq(1, 3, 4)
        atLeast (2, lists) should contain atLeastOneElementOf Seq(1, 3, 4)
        atMost (2, lists) should contain atLeastOneElementOf Seq(2, 3, 4)
        no (lists) should contain atLeastOneElementOf Seq(3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain atLeastOneElementOf Seq(1, 3, 4)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(1, 3, 4)) + " (EveryShouldContainAtLeastOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should contain atLeastOneElementOf Seq("hi", "he")
        intercept[TestFailedException] {
          all (hiLists) should contain atLeastOneElementOf Seq("ho", "he")
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should contain atLeastOneElementOf Seq("HI", "HE")
        intercept[TestFailedException] {
          all (hiLists) should contain atLeastOneElementOf Seq("hi", "he")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should contain atLeastOneElementOf Seq("HI", "HE")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain atLeastOneElementOf Seq("hi", "he")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain atLeastOneElementOf Seq("hi", "he")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain atLeastOneElementOf Seq("HI", "HE")) (decided by defaultEquality[String])
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should contain atLeastOneElementOf Seq(1, 3, 3, 4)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    object `when used with (contain atLeastOneElementOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain atLeastOneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneElementOf Seq(1, 3, 4))
        atMost (2, lists) should (contain atLeastOneElementOf Seq(2, 3, 4))
        no (lists) should (contain atLeastOneElementOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneElementOf Seq(1, 3, 4))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(1, 3, 4)) + " (EveryShouldContainAtLeastOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he"))
        intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE"))
        intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE"))) (decided by defaultEquality[String])
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneElementOf Seq(1, 3, 3, 4))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    /*
     I purposely don't want to support this syntax:

    scala> all (list1s) should contain (atLeastOneElementOf Seq(1, 3, 4))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (list1s) should contain (atLeastOneElementOf Seq(1, 3, 4))
                                                 ^

    scala> all (list1s) should (contain (atLeastOneElementOf Seq(1, 3, 4)))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (list1s) should (contain (atLeastOneElementOf Seq(1, 3, 4)))
                                                  ^

     Reason is that I don't want people putting parentheses between contain and atLeastOneElementOf, etc. This will not compile.
    */
    object `when used with not contain atLeastOneElementOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(toLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("happy", "birthday", "to", "you")) + " (EveryShouldContainAtLeastOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain atLeastOneElementOf (Seq("to", "you"))
        intercept[TestFailedException] {
          all (toLists) should not contain atLeastOneElementOf (Seq("TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not contain atLeastOneElementOf (Seq("to", "you"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain atLeastOneElementOf (Seq("TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should not contain atLeastOneElementOf (Seq(" TO ", " YOU "))
        intercept[TestFailedException] {
          (all (toLists) should not contain atLeastOneElementOf (Seq(" TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    /*
    Interesting, of these three, the last one does happen to compile and run:

    scala> all (toLists) should (not (contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (toLists) should (not (contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))))
                                                        ^

    scala> all (toLists) should not (contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum")))
    <console>:15: error: org.scalatest.words.NewContainWord does not take parameters
                  all (toLists) should not (contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum")))
                                                       ^

    scala> all (toLists) should not contain (atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))

    The top two don't, but still I don't want to support that in general.
    */
    object `when used with (not contain atLeastOneElementOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(One("to")) + " contained at least one element of " + decorateToStringValue(Seq("happy", "birthday", "to", "you")) + " (EveryShouldContainAtLeastOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain atLeastOneElementOf (Seq("to", "you")))
        intercept[TestFailedException] {
          all (toLists) should (not contain atLeastOneElementOf (Seq("TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not contain atLeastOneElementOf (Seq("to", "you")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain atLeastOneElementOf (Seq("TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not contain atLeastOneElementOf (Seq(" TO ", " YOU ")))
        intercept[TestFailedException] {
          (all (toLists) should (not contain atLeastOneElementOf (Seq(" TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    object `when used with shouldNot contain atLeastOneElementOf Seq(...) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain atLeastOneElementOf Seq("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(toLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("happy", "birthday", "to", "you")) + " (EveryShouldContainAtLeastOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain atLeastOneElementOf Seq("to", "you")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain atLeastOneElementOf Seq("TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot contain atLeastOneElementOf Seq("to", "you")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain atLeastOneElementOf Seq("TO", "YOU")) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot contain atLeastOneElementOf Seq(" TO ", " YOU ")
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain atLeastOneElementOf Seq(" TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    object `when used with shouldNot (contain atLeastOneElementOf Seq(...)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(toLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("happy", "birthday", "to", "you")) + " (EveryShouldContainAtLeastOneElementOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain atLeastOneElementOf Seq("to", "you"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain atLeastOneElementOf Seq("TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot (contain atLeastOneElementOf Seq("to", "you"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain atLeastOneElementOf Seq("TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot (contain atLeastOneElementOf Seq(" TO ", " YOU "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain atLeastOneElementOf Seq(" TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAtLeastOneElementOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }
  }
}
