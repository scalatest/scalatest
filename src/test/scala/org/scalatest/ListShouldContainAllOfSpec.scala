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
import org.scalautils.Normalization
import org.scalautils.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ListShouldContainAllOfSpec extends Spec with Matchers {
  
  private def upperCase(value: Any): Any = 
    value match {
      case l: List[_] => l.map(upperCase(_))
      case s: String => s.toUpperCase
      case c: Char => c.toString.toUpperCase.charAt(0)
      case (s1: String, s2: String) => (s1.toUpperCase, s2.toUpperCase)
      case _ => value
    }
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }
  
  //ADDITIONAL//

  object `a List` {

    val fumList: List[String] = List("fex", "fum", "foe", "fie", "fee")
    val toList: List[String] = List("too", "you", "to", "birthday", "happy")

    object `when used with contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should newContain newAllOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should newContain newAllOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should newContain newAllOf ("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should newContain newAllOf ("fee", "fie", "foe", "fam")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should newContain newAllOf ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should newContain newAllOf ("fee", "fie", "foe", "fam")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (newContain newAllOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (newContain newAllOf ("fee", "fie", "foe", "fam"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (newContain newAllOf ("fee", "fie", "foe", "fam"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with not contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should not newContain newAllOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList should not newContain newAllOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedAllOfElements", decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should not newContain newAllOf ("happy", "birthday", "to", "you", "dear")
        intercept[TestFailedException] {
          toList should not newContain newAllOf ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should not newContain newAllOf ("happy", "birthday", "to", "you", "dear")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not newContain newAllOf ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList should not newContain newAllOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList should not newContain newAllOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with (not contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not newContain newAllOf ("HAPPY", "BIRTHDAY", "TO", "YOU", "DEAR"))
        val e1 = intercept[TestFailedException] {
          toList should (not newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedAllOfElements", decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should (not newContain newAllOf ("NICE", "TO", "MEET", "YOU", "TOO"))
        intercept[TestFailedException] {
          toList should (not newContain newAllOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not newContain newAllOf ("NICE", "TO", "MEET", "YOU", "TOO"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not newContain newAllOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should (not newContain newAllOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should (not newContain newAllOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
    }
  }

  object `a col of Lists` {

    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(3, 2, 1, 0))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(8, 4, 3, 2))
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), Nil)
    val hiLists: Vector[List[String]] = Vector(List("howdy", "hi", "he"), List("howdy", "hi", "he"), List("howdy", "hi", "he"))
    val toLists: Vector[List[String]] = Vector(List("happy", "to", "you"), List("happy", "to", "you"), List("happy", "to", "you"))

    object `when used with contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should newContain newAllOf (1, 2, 3)
        atLeast (2, lists) should newContain newAllOf (1, 2, 3)
        atMost (2, lists) should newContain newAllOf (1, 2, 3)
        no (lists) should newContain newAllOf (3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should newContain newAllOf (1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(8, 4, 3, 2)) + " did not contain all of " + "(1, 2, 3)" + " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should newContain newAllOf ("he", "hi")
        intercept[TestFailedException] {
          all (hiLists) should newContain newAllOf ("ho", "hi")
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should newContain newAllOf ("HE", "HI")
        intercept[TestFailedException] {
          all (hiLists) should newContain newAllOf ("HO", "HI")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should newContain newAllOf ("HE", "HI")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should newContain newAllOf ("HO", "HI")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should newContain newAllOf ("he", "hi")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should newContain newAllOf ("ho", "hi")) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with (contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newAllOf (1, 2, 3))
        atLeast (2, lists) should (newContain newAllOf (1, 2, 3))
        atMost (2, lists) should (newContain newAllOf (1, 2, 3))
        no (lists) should (newContain newAllOf (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newAllOf (1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(8, 4, 3, 2)) + " did not contain all of " + "(1, 2, 3)" + " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (newContain newAllOf ("he", "hi"))
        intercept[TestFailedException] {
          all (hiLists) should (newContain newAllOf ("ho", "hi"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should (newContain newAllOf ("HE", "HI"))
        intercept[TestFailedException] {
          all (hiLists) should (newContain newAllOf ("HO", "HI"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newAllOf ("HE", "HI"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (newContain newAllOf ("HO", "HI"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (newContain newAllOf ("he", "hi"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (newContain newAllOf ("ho", "hi"))) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with not contain allOf (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not newContain newAllOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) should not newContain newAllOf ("you", "to")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" +  " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not newContain newAllOf ("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) should not newContain newAllOf ("YOU", "TO")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not newContain newAllOf ("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not newContain newAllOf ("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) should not newContain newAllOf (" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) should not newContain newAllOf (" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with (not contain allOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not newContain newAllOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not newContain newAllOf ("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainAllOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("happy", "to", "you")) + " contained all of " + "(\"you\", \"to\")" + " (ListShouldContainAllOfSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not newContain newAllOf ("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should (not newContain newAllOf ("YOU", "TO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not newContain newAllOf ("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not newContain newAllOf ("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not newContain newAllOf (" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) should (not newContain newAllOf (" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
    }
  }
}
