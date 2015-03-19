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
import org.scalatest.exceptions.TestFailedException

class EveryShouldContainAllOfSpec extends Spec {

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

    val fumList: Every[String] = Every("fex", "fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("too", "you", "to", "birthday", "happy")

    object `when used with contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should contain allOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAllOfElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should contain allOf ("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should contain allOf ("fee", "fie", "foe", "fam")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should contain allOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain allOf ("fee", "fie", "foe", "fam")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should contain allOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should contain allOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with (contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (contain allOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainAllOfElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allOf ("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (contain allOf ("fee", "fie", "foe", "fam"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (contain allOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain allOf ("fee", "fie", "foe", "fam"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain allOf (" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (contain allOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with not contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should not contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList should not contain allOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should not contain allOf ("happy", "birthday", "to", "you", "dear")
        intercept[TestFailedException] {
          toList should not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should not contain allOf ("happy", "birthday", "to", "you", "dear")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList should not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList should not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should not contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with (not contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU", "DEAR"))
        val e1 = intercept[TestFailedException] {
          toList should (not contain allOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should (not contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))
        intercept[TestFailedException] {
          toList should (not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should (not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should (not contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should (not contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with shouldNot contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain allOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot contain allOf ("happy", "birthday", "to", "you", "dear")
        intercept[TestFailedException] {
          toList shouldNot contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot contain allOf ("happy", "birthday", "to", "you", "dear")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList shouldNot contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList shouldNot contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with shouldNot (contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot (contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU", "DEAR"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain allOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedAllOfElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot (contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))
        intercept[TestFailedException] {
          toList shouldNot (contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot (contain allOf ("NICE", "TO", "MEET", "YOU", "TOO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain allOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList shouldNot (contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot (contain allOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot (contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }
  }

  object `an every of Everys` {

    val list1s: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(3, 2, 1, 0))
    val lists: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(8, 4, 3, 2))
    val hiLists: Every[Every[String]] = Every(Every("howdy", "hi", "he"), Every("howdy", "hi", "he"), Every("howdy", "hi", "he"))
    val toLists: Every[Every[String]] = Every(Every("happy", "to", "you"), Every("happy", "to", "you"), Every("happy", "to", "you"))

    object `when used with contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should contain allOf (1, 2, 3)
        atLeast (2, lists) should contain allOf (1, 2, 3)
        atMost (2, lists) should contain allOf (1, 2, 3)
        no (lists) should contain allOf (3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain allOf (1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(Many(8, 4, 3, 2)) + " did not contain all of " + "(1, 2, 3)" + " (EveryShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should contain allOf ("he", "hi")
        intercept[TestFailedException] {
          all (hiLists) should contain allOf ("ho", "hi")
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should contain allOf ("HE", "HI")
        intercept[TestFailedException] {
          all (hiLists) should contain allOf ("HO", "HI")
        }
      }
      def `should use an explicitly provided Equality` {
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
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should contain allOf (1, 2, 2, 3)
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with (contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain allOf (1, 2, 3))
        atLeast (2, lists) should (contain allOf (1, 2, 3))
        atMost (2, lists) should (contain allOf (1, 2, 3))
        no (lists) should (contain allOf (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain allOf (1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(Many(8, 4, 3, 2)) + " did not contain all of " + "(1, 2, 3)" + " (EveryShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (contain allOf ("he", "hi"))
        intercept[TestFailedException] {
          all (hiLists) should (contain allOf ("ho", "hi"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should (contain allOf ("HE", "HI"))
        intercept[TestFailedException] {
          all (hiLists) should (contain allOf ("HO", "HI"))
        }
      }
      def `should use an explicitly provided Equality` {
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
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain allOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with not contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain allOf ("you", "to")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" +  " (EveryShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain allOf ("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) should not contain allOf ("YOU", "TO")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not contain allOf ("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain allOf ("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) should not contain allOf (" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) should not contain allOf (" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with (not contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not contain allOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain allOf ("you", "to"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" + " (EveryShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain allOf ("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should (not contain allOf ("YOU", "TO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not contain allOf ("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain allOf ("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not contain allOf (" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) should (not contain allOf (" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with shouldNot contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot contain allOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain allOf ("you", "to")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" +  " (EveryShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain allOf ("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain allOf ("YOU", "TO")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot contain allOf ("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain allOf ("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot contain allOf (" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain allOf (" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain allOf ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }

    object `when used with shouldNot (contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot (contain allOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain allOf ("you", "to"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Many("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" + " (EveryShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain allOf ("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain allOf ("YOU", "TO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot (contain allOf ("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain allOf ("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot (contain allOf (" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain allOf (" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain allOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("EveryShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.allOfDuplicate))
      }
    }
  }
}
