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

import org.scalactic.{Equality, Every, One, Many, Entry}
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import scala.collection.mutable.LinkedList
import Matchers._
import exceptions.TestFailedException

class EveryShouldContainTheSameElementsInOrderAsSpec extends Spec {

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

  object `an Every` {

    val fumList: Every[String] = Every("fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("happy", "birthday", "to", "you")

    object `when used with contain theSameElementsInOrderAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should contain theSameElementsInOrderAs LinkedList("fum", "foe", "fie", "fee")
        val e1 = intercept[TestFailedException] {
          fumList should contain theSameElementsInOrderAs LinkedList("fee", "fie", "foe", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainSameElementsInOrder(decorateToStringValue(fumList), decorateToStringValue(LinkedList("fee", "fie", "foe", "fum"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should contain theSameElementsInOrderAs LinkedList("FUM", "FOE", "FIE", "FEE")
        intercept[TestFailedException] {
          fumList should contain theSameElementsInOrderAs LinkedList("fee", "fie", "foe", "fum")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should contain theSameElementsInOrderAs LinkedList("FUM", "FOE", "FIE", "FEE")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain theSameElementsInOrderAs LinkedList("fee", "fie", "foe", "fum")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should contain theSameElementsInOrderAs LinkedList(" FUM ", " FOE ", " FIE ", " FEE ")
        }
        (fumList should contain theSameElementsInOrderAs LinkedList(" FUM ", " FOE ", " FIE ", " FEE ")) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain theSameElementsInOrderAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (contain theSameElementsInOrderAs LinkedList("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs LinkedList("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainSameElementsInOrder(decorateToStringValue(fumList), decorateToStringValue(LinkedList("fee", "fie", "foe", "fum"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsInOrderAs LinkedList("FUM", "FOE", "FIE", "FEE"))
        intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs LinkedList("fee", "fie", "foe", "fum"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (contain theSameElementsInOrderAs LinkedList("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain theSameElementsInOrderAs LinkedList("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs LinkedList(" FUM ", " FOE ", " FIE ", " FEE "))
        }
        (fumList should (contain theSameElementsInOrderAs LinkedList(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with not contain theSameElementsInOrderAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should not contain theSameElementsInOrderAs (LinkedList("you", "to", "birthday", "happy"))
        val e1 = intercept[TestFailedException] {
          toList should not contain theSameElementsInOrderAs (LinkedList("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedSameElementsInOrder(decorateToStringValue(toList), decorateToStringValue(LinkedList("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should not contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY"))
        intercept[TestFailedException] {
          toList should not contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should not contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should not contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should not contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with (not contain theSameElementsInOrderAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not contain theSameElementsInOrderAs (LinkedList("you", "to", "birthday", "happy")))
        val e1 = intercept[TestFailedException] {
          toList should (not contain theSameElementsInOrderAs (LinkedList("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedSameElementsInOrder(decorateToStringValue(toList), decorateToStringValue(LinkedList("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should (not contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY")))
        intercept[TestFailedException] {
          toList should (not contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        toList should (not contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (toList should (not contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with shouldNot contain theSameElementsInOrderAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot contain theSameElementsInOrderAs (LinkedList("you", "to", "birthday", "happy"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain theSameElementsInOrderAs (LinkedList("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedSameElementsInOrder(decorateToStringValue(toList), decorateToStringValue(LinkedList("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY"))
        intercept[TestFailedException] {
          toList shouldNot contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList shouldNot contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with shouldNot (contain theSameElementsInOrderAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot (contain theSameElementsInOrderAs (LinkedList("you", "to", "birthday", "happy")))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain theSameElementsInOrderAs (LinkedList("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedSameElementsInOrder(decorateToStringValue(toList), decorateToStringValue(LinkedList("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot (contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY")))
        intercept[TestFailedException] {
          toList shouldNot (contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot (contain theSameElementsInOrderAs (LinkedList("YOU", "TO", "BIRTHDAY", "HAPPY")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain theSameElementsInOrderAs (LinkedList("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        toList shouldNot (contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (toList shouldNot (contain theSameElementsInOrderAs (LinkedList(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
    }
  }

  object `every of Everys` {

    val list1s: Every[Every[Int]] = Every( Every(1, 2, 3), Every(1, 2, 3), Every(1, 2, 3))
    val lists: Every[Every[Int]] = Every( Every(1, 2, 3), Every(1, 2, 3), Every(2, 3, 4))
    val hiLists: Every[Every[String]] = Every( Every("hi", "he"), Every("hi", "he"), Every("hi", "he"))
    val toLists: Every[Every[String]] = Every( Every("to", "you"), Every("to", "you"), Every("to", "you"))

    object `when used with contain theSameElementsInOrderAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should contain theSameElementsInOrderAs LinkedList(1, 2, 3)
        atLeast (2, lists) should contain theSameElementsInOrderAs LinkedList(1, 2, 3)
        atMost (2, lists) should contain theSameElementsInOrderAs LinkedList(1, 2, 3)
        no (lists) should contain theSameElementsInOrderAs LinkedList(3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain theSameElementsInOrderAs LinkedList(1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(Many(2, 3, 4)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(LinkedList(1, 2, 3)) + " (EveryShouldContainTheSameElementsInOrderAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should contain theSameElementsInOrderAs LinkedList("hi", "he")
        intercept[TestFailedException] {
          all (hiLists) should contain theSameElementsInOrderAs LinkedList("hi", "ho")
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should contain theSameElementsInOrderAs LinkedList("HI", "HE")
        intercept[TestFailedException] {
          all (hiLists) should contain theSameElementsInOrderAs LinkedList("HI", "HO")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should contain theSameElementsInOrderAs LinkedList("HI", "HE")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain theSameElementsInOrderAs LinkedList("HI", "HO")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain theSameElementsInOrderAs LinkedList("hi", "he")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain theSameElementsInOrderAs LinkedList("hi", "ho")) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with (contain theSameElementsInOrderAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain theSameElementsInOrderAs LinkedList(1, 2, 3))
        atLeast (2, lists) should (contain theSameElementsInOrderAs LinkedList(1, 2, 3))
        atMost (2, lists) should (contain theSameElementsInOrderAs LinkedList(1, 2, 3))
        no (lists) should (contain theSameElementsInOrderAs LinkedList(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain theSameElementsInOrderAs LinkedList(1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(Many(2, 3, 4)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(LinkedList(1, 2, 3)) + " (EveryShouldContainTheSameElementsInOrderAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (contain theSameElementsInOrderAs LinkedList("hi", "he"))
        intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsInOrderAs LinkedList("he", "hi"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should (contain theSameElementsInOrderAs LinkedList("HI", "HE"))
        intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsInOrderAs LinkedList("HI", "HO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain theSameElementsInOrderAs LinkedList("HI", "HE"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsInOrderAs LinkedList("HI", "HO"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain theSameElementsInOrderAs LinkedList("hi", "he"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsInOrderAs LinkedList("he", "hi"))) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with not contain theSameElementsInOrderAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not contain theSameElementsInOrderAs (LinkedList("you", "to"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain theSameElementsInOrderAs (LinkedList("to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("to", "you")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(LinkedList("to", "you")) +  " (EveryShouldContainTheSameElementsInOrderAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain theSameElementsInOrderAs (LinkedList("YOU", "TO"))
        intercept[TestFailedException] {
          all (toLists) should not contain theSameElementsInOrderAs (LinkedList("TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not contain theSameElementsInOrderAs (LinkedList("YOU", "TO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain theSameElementsInOrderAs (LinkedList("TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should not contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU "))
        intercept[TestFailedException] {
          (all (toLists) should not contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with (not contain theSameElementsInOrderAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not contain theSameElementsInOrderAs (LinkedList("you", "to")))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain theSameElementsInOrderAs (LinkedList("to", "you")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("to", "you")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(LinkedList("to", "you")) + " (EveryShouldContainTheSameElementsInOrderAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain theSameElementsInOrderAs (LinkedList("YOU", "TO")))
        intercept[TestFailedException] {
          all (toLists) should (not contain theSameElementsInOrderAs (LinkedList("TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not contain theSameElementsInOrderAs (LinkedList("YOU", "TO")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain theSameElementsInOrderAs (LinkedList("TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU ")))
        intercept[TestFailedException] {
          (all (toLists) should (not contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with shouldNot contain theSameElementsInOrderAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList("you", "to"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList("to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("to", "you")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(LinkedList("to", "you")) +  " (EveryShouldContainTheSameElementsInOrderAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList("YOU", "TO"))
        intercept[TestFailedException] {
          all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList("TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList("YOU", "TO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList("TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with shouldNot (contain theSameElementsInOrderAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList("you", "to")))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList("to", "you")))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainTheSameElementsInOrderAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("to", "you")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(LinkedList("to", "you")) + " (EveryShouldContainTheSameElementsInOrderAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList("YOU", "TO")))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList("TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList("YOU", "TO")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList("TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU ")))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain theSameElementsInOrderAs (LinkedList(" TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
    }
  }
}
