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
import org.scalactic.Uniformity
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.TestFailedException

class ListShouldContainTheSameElementsAsSpec extends Spec {
  
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
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }
  
  //ADDITIONAL//

  object `a List` {

    val fumList: List[String] = List("fum", "foe", "fie", "fee")
    val toList: List[String] = List("you", "to", "birthday", "happy")

    object `when used with contain theSameElementsAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should contain theSameElementsAs Set("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should contain theSameElementsAs Set("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should contain theSameElementsAs Set("fee", "fie", "foe")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain theSameElementsAs Set("fee", "fie", "foe")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain theSameElementsAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("fee", "fie", "foe"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain theSameElementsAs Set("fee", "fie", "foe"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with not contain theSameElementsAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should not contain theSameElementsAs (Set("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toList should not contain theSameElementsAs (Set("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedSameElements", decorateToStringValue(toList), decorateToStringValue(Set("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should not contain theSameElementsAs (Set("happy", "birthday", "to"))
        intercept[TestFailedException] {
          toList should not contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should not contain theSameElementsAs (Set("happy", "birthday", "to"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should not contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should not contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with (not contain theSameElementsAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU")))
        val e1 = intercept[TestFailedException] {
          toList should (not contain theSameElementsAs (Set("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedSameElements", decorateToStringValue(toList), decorateToStringValue(Set("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should (not contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))
        intercept[TestFailedException] {
          toList should (not contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        toList should (not contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (toList should (not contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
    }
    
    object `when used with shouldNot contain theSameElementsAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot contain theSameElementsAs (Set("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain theSameElementsAs (Set("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedSameElements", decorateToStringValue(toList), decorateToStringValue(Set("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot contain theSameElementsAs (Set("happy", "birthday", "to"))
        intercept[TestFailedException] {
          toList shouldNot contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot contain theSameElementsAs (Set("happy", "birthday", "to"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList shouldNot contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with shouldNot (contain theSameElementsAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot (contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU")))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain theSameElementsAs (Set("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedSameElements", decorateToStringValue(toList), decorateToStringValue(Set("happy", "birthday", "to", "you"))))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot (contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))
        intercept[TestFailedException] {
          toList shouldNot (contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU")))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot (contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain theSameElementsAs (Set("HAPPY", "BIRTHDAY", "TO", "YOU")))) (decided by upperCaseStringEquality)
        }
        toList shouldNot (contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))
        intercept[TestFailedException] {
          (toList shouldNot (contain theSameElementsAs (Set(" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")))) (after being lowerCased and trimmed)
        }
      }
    }
  }

  object `a col of Lists` {

    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(3, 2, 1))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(4, 3, 2))
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "he"), List("hi", "he"), List("hi", "he"))
    val toLists: Vector[List[String]] = Vector(List("to", "you"), List("to", "you"), List("to", "you"))

    object `when used with contain theSameElementsAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should contain theSameElementsAs Set(1, 2, 3)
        atLeast (2, lists) should contain theSameElementsAs Set(1, 2, 3)
        atMost (2, lists) should contain theSameElementsAs Set(1, 2, 3)
        no (lists) should contain theSameElementsAs Set(3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain theSameElementsAs Set(1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(4, 3, 2)) + " did not contain the same elements as " + decorateToStringValue(Set(1, 2, 3)) + " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))

        val e3 = intercept[TestFailedException] {
          all (listsNil) should contain theSameElementsAs Set(1, 2, 3)
        }
        e3.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e3.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(Nil) + " did not contain the same elements as " + decorateToStringValue(Set(1, 2, 3)) + " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(listsNil)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should contain theSameElementsAs Set("he", "hi")
        intercept[TestFailedException] {
          all (hiLists) should contain theSameElementsAs Set("ho", "hi")
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should contain theSameElementsAs Set("HE", "HI")
        intercept[TestFailedException] {
          all (hiLists) should contain theSameElementsAs Set("HO", "HI")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should contain theSameElementsAs Set("HE", "HI")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain theSameElementsAs Set("HO", "HI")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain theSameElementsAs Set("he", "hi")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain theSameElementsAs Set("ho", "hi")) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with (contain theSameElementsAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain theSameElementsAs Set(1, 2, 3))
        atLeast (2, lists) should (contain theSameElementsAs Set(1, 2, 3))
        atMost (2, lists) should (contain theSameElementsAs Set(1, 2, 3))
        no (lists) should (contain theSameElementsAs Set(3, 4, 5))
        no (listsNil) should (contain theSameElementsAs Set(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain theSameElementsAs Set(1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(4, 3, 2)) + " did not contain the same elements as " + decorateToStringValue(Set(1, 2, 3)) + " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))

        val e4 = intercept[TestFailedException] {
          all (listsNil) should (contain theSameElementsAs Set(1, 2, 3))
        }
        e4.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(Nil) + " did not contain the same elements as " + decorateToStringValue(Set(1, 2, 3)) + " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(listsNil)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (contain theSameElementsAs Set("he", "hi"))
        intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsAs Set("ho", "hi"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should (contain theSameElementsAs Set("HE", "HI"))
        intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsAs Set("HO", "HI"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain theSameElementsAs Set("HE", "HI"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsAs Set("HO", "HI"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain theSameElementsAs Set("he", "hi"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsAs Set("ho", "hi"))) (decided by defaultEquality[String])
        }
      }
    }

    object `when used with not contain theSameElementsAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not contain theSameElementsAs (Set("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain theSameElementsAs (Set("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained the same elements as " + decorateToStringValue(Set("you", "to")) +  " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should not contain theSameElementsAs (Set("YOU", "TO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain theSameElementsAs (Set("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should not contain theSameElementsAs (Set(" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) should not contain theSameElementsAs (Set(" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with (not contain theSameElementsAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain theSameElementsAs (Set("you", "to")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained the same elements as " + decorateToStringValue(Set("you", "to")) + " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))
        intercept[TestFailedException] {
          all (toLists) should (not contain theSameElementsAs (Set("YOU", "TO")))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain theSameElementsAs (Set("YOU", "TO")))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not contain theSameElementsAs (Set(" YOU ", " TO ")))
        intercept[TestFailedException] {
          (all (toLists) should (not contain theSameElementsAs (Set(" YOU ", " TO ")))) (after being lowerCased and trimmed)
        }
      }
    }
    
    object `when used with shouldNot contain theSameElementsAs (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot contain theSameElementsAs (Set("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain theSameElementsAs (Set("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained the same elements as " + decorateToStringValue(Set("you", "to")) +  " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot contain theSameElementsAs (Set("YOU", "TO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain theSameElementsAs (Set("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot contain theSameElementsAs (Set(" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain theSameElementsAs (Set(" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with shouldNot (contain theSameElementsAs (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot (contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain theSameElementsAs (Set("you", "to")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainTheSameElementsAsSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained the same elements as " + decorateToStringValue(Set("you", "to")) + " (ListShouldContainTheSameElementsAsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain theSameElementsAs (Set("YOU", "TO")))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot (contain theSameElementsAs (Set("NICE", "TO", "MEET", "YOU")))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain theSameElementsAs (Set("YOU", "TO")))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot (contain theSameElementsAs (Set(" YOU ", " TO ")))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain theSameElementsAs (Set(" YOU ", " TO ")))) (after being lowerCased and trimmed)
        }
      }
    }
  }
}
