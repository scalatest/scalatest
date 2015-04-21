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
import org.scalatest.exceptions.TestFailedException

class ListShouldContainOnlySpec extends Spec {

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

    object `when used with contain only (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should contain only ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          fumList should contain only ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should contain only ("FEE", "FIE", "FOE", "FUM")
        intercept[TestFailedException] {
          fumList should contain only ("fee", "fie", "foe")
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should contain only ("FEE", "FIE", "FOE", "FUM")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should contain only ("fee", "fie", "foe")) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should contain only (" FEE ", " FIE ", " FOE ", " FUM ")
        }
        (fumList should contain only (" FEE ", " FIE ", " FOE ", " FUM ")) (after being lowerCased and trimmed)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain only ()
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should contain only ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TestFailedException with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[exceptions.TestFailedException] {
          fumList should contain only (Vector("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(fumList), decorateToStringValue(Vector("happy", "birthday", "to", "you"))))
      }

      def `should throw TestFailedException when used to check Vector(2, 3) should contain only (1, 2, 3)` {
        val e1 = intercept[exceptions.TestFailedException] {
          Vector(2, 3) should contain only (1, 2, 3)
        }
      }
    }

    object `when used with (contain only (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        fumList should (contain only ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain only ("FEE", "FIE", "FOE", "FUM"))
        intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe"))
        }
      }
      def `should use an explicitly provided Equality` {
        (fumList should (contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (fumList should (contain only ("fee", "fie", "foe"))) (decided by upperCaseStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ())
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TestFailedException with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[exceptions.TestFailedException] {
          fumList should (contain only (Vector("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(fumList), decorateToStringValue(Vector("happy", "birthday", "to", "you"))))
      }
    }

    object `when used with not contain only (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should not contain only ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList should not contain only ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should not contain only ("happy", "birthday", "to")
        intercept[TestFailedException] {
          toList should not contain only ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should not contain only ("happy", "birthday", "to")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should not contain only ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList should not contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList should not contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should not contain only ()
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should not contain only ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TestFailedException with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[exceptions.TestFailedException] {
          Vector(Vector("happy", "birthday", "to", "you")) should not contain only (Vector("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("happy", "birthday", "to", "you"))), decorateToStringValue(Vector("happy", "birthday", "to", "you"))))
      }
    }

    object `when used with (not contain only (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList should (not contain only ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        val e1 = intercept[TestFailedException] {
          toList should (not contain only ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList should (not contain only ("NICE", "TO", "MEET", "YOU"))
        intercept[TestFailedException] {
          toList should (not contain only ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList should (not contain only ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList should (not contain only ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList should (not contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList should (not contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should (not contain only ())
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList should (not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TestFailedException with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[exceptions.TestFailedException] {
          Vector(Vector("happy", "birthday", "to", "you")) should (not contain only (Vector("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("happy", "birthday", "to", "you"))), decorateToStringValue(Vector("happy", "birthday", "to", "you"))))
      }
    }
    
    object `when used with shouldNot contain only (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot contain only ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          toList shouldNot contain only ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot contain only ("happy", "birthday", "to")
        intercept[TestFailedException] {
          toList shouldNot contain only ("HAPPY", "BIRTHDAY", "TO", "YOU")
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot contain only ("happy", "birthday", "to")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot contain only ("HAPPY", "BIRTHDAY", "TO", "YOU")) (decided by upperCaseStringEquality)
        }
        toList shouldNot contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        intercept[TestFailedException] {
          (toList shouldNot contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot contain only ()
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot contain only ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      def `should throw TestFailedException with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("happy", "birthday", "to", "you")) shouldNot contain only (Vector("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("happy", "birthday", "to", "you"))), decorateToStringValue(Vector("happy", "birthday", "to", "you"))))
      }
    }

    object `when used with shouldNot (contain only (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toList shouldNot (contain only ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        val e1 = intercept[TestFailedException] {
          toList shouldNot (contain only ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElements(decorateToStringValue(toList), "\"happy\", \"birthday\", \"to\", \"you\""))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toList shouldNot (contain only ("NICE", "TO", "MEET", "YOU"))
        intercept[TestFailedException] {
          toList shouldNot (contain only ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
      }
      def `should use an explicitly provided Equality` {
        (toList shouldNot (contain only ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (toList shouldNot (contain only ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        toList shouldNot (contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))
        intercept[TestFailedException] {
          (toList shouldNot (contain only (" HAPPY ", " BIRTHDAY ", " TO ", " YOU "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot (contain only ())
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toList shouldNot (contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TestFailedException with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("happy", "birthday", "to", "you")) shouldNot (contain only (Vector("happy", "birthday", "to", "you")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("happy", "birthday", "to", "you"))), decorateToStringValue(Vector("happy", "birthday", "to", "you"))))
      }
    }
  }

  object `a col of Lists` {

    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(3, 2, 1))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(4, 3, 2))
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "he"), List("hi", "he"), List("hi", "he"))
    val toLists: Vector[List[String]] = Vector(List("to", "you"), List("to", "you"), List("to", "you"))

    object `when used with contain only (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should contain only (1, 2, 3)
        atLeast (2, lists) should contain only (1, 2, 3)
        atMost (2, lists) should contain only (1, 2, 3)
        no (lists) should contain only (3, 4, 5)

        val e1 = intercept[TestFailedException] {
          all (lists) should contain only (1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(1, 2, 3)" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))

        val e3 = intercept[TestFailedException] {
          all (lists) should contain only (1, 2, 3)
        }
        e3.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e3.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(1, 2, 3)" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should contain only ("he", "hi")
        intercept[TestFailedException] {
          all (hiLists) should contain only ("ho", "hi")
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should contain only ("HE", "HI")
        intercept[TestFailedException] {
          all (hiLists) should contain only ("HO", "HI")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should contain only ("HE", "HI")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should contain only ("HO", "HI")) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should contain only ("he", "hi")) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should contain only ("ho", "hi")) (decided by defaultEquality[String])
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should contain only ()
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should contain only (1, 2, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(3, 2, 1), Vector(3, 2, 1), Vector(4, 3, 2))) should contain only Vector(1, 2, 3)
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(Vector(3, 2, 1)) + " did not contain only (" + decorateToStringValue(Vector(1, 2, 3)) + "), did you forget to say : _*" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(Vector(Vector(3, 2, 1), Vector(3, 2, 1), Vector(4, 3, 2)))))
      }
    }

    object `when used with (contain only (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain only (1, 2, 3))
        atLeast (2, lists) should (contain only (1, 2, 3))
        atMost (2, lists) should (contain only (1, 2, 3))
        no (lists) should (contain only (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain only (1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(1, 2, 3)" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))

        val e4 = intercept[TestFailedException] {
          all (lists) should (contain only (1, 2, 3))
        }
        e4.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, " + decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(1, 2, 3)" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(lists)))
      }

      def `should use the implicit Equality in scope` {
        all (hiLists) should (contain only ("he", "hi"))
        intercept[TestFailedException] {
          all (hiLists) should (contain only ("ho", "hi"))
        }
        implicit val ise = upperCaseStringEquality
        all (hiLists) should (contain only ("HE", "HI"))
        intercept[TestFailedException] {
          all (hiLists) should (contain only ("HO", "HI"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain only ("HE", "HI"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (hiLists) should (contain only ("HO", "HI"))) (decided by upperCaseStringEquality)
        }
        implicit val ise = upperCaseStringEquality
        (all (hiLists) should (contain only ("he", "hi"))) (decided by defaultEquality[String])
        intercept[TestFailedException] {
          (all (hiLists) should (contain only ("ho", "hi"))) (decided by defaultEquality[String])
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only ())
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(3, 2, 1), Vector(3, 2, 1), Vector(4, 3, 2))) should (contain only Vector(1, 2, 3))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(Vector(3, 2, 1)) + " did not contain only (" + decorateToStringValue(Vector(1, 2, 3)) + "), did you forget to say : _*" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(Vector(Vector(3, 2, 1), Vector(3, 2, 1), Vector(4, 3, 2)))))
      }
    }

    object `when used with not contain only (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should not contain only ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) should not contain only ("you", "to")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained only " + "(\"you\", \"to\")" +  " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should not contain only ("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) should not contain only ("YOU", "TO")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should not contain only ("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should not contain only ("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) should not contain only (" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) should not contain only (" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain only ()
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should not contain only ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("you", "to")))) should not contain only (Vector("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Vector(Vector("you", "to"))) + " contained only (" + decorateToStringValue(Vector("you", "to")) + "), did you forget to say : _*" +  " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(Vector(Vector(Vector("you", "to"))))))
      }
    }

    object `when used with (not contain only (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) should (not contain only ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) should (not contain only ("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained only " + "(\"you\", \"to\")" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) should (not contain only ("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) should (not contain only ("YOU", "TO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) should (not contain only ("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) should (not contain only ("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) should (not contain only (" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) should (not contain only (" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain only ())
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) should (not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("you", "to")))) should (not contain only (Vector("you", "to")))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(Vector(Vector("you", "to"))) + " contained only (" + decorateToStringValue(Vector("you", "to")) + "), did you forget to say : _*" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(Vector(Vector(Vector("you", "to"))))))
      }
    }
    
    object `when used with shouldNot contain only (..)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot contain only ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot contain only ("you", "to")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained only " + "(\"you\", \"to\")" +  " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot contain only ("NICE", "MEET", "YOU")
        intercept[TestFailedException] {
          all (toLists) shouldNot contain only ("YOU", "TO")
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot contain only ("NICE", "MEET", "YOU")) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain only ("YOU", "TO")) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot contain only (" YOU ", " TO ")
        intercept[TestFailedException] {
          (all (toLists) shouldNot contain only (" YOU ", " TO ")) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain only ()
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot contain only ("fee", "fie", "foe", "fie", "fum")
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("to", "you")))) shouldNot contain only (Vector("to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Vector(Vector("to", "you"))) + " contained only (" + decorateToStringValue(Vector("to", "you")) + "), did you forget to say : _*" +  " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(Vector(Vector(Vector("to", "you"))))))
      }
    }

    object `when used with shouldNot (contain only (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toLists) shouldNot (contain only ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toLists) shouldNot (contain only ("you", "to"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 0, " + decorateToStringValue(List("to", "you")) + " contained only " + "(\"you\", \"to\")" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(toLists)))
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toLists) shouldNot (contain only ("NICE", "MEET", "YOU"))
        intercept[TestFailedException] {
          all (toLists) shouldNot (contain only ("YOU", "TO"))
        }
      }
      def `should use an explicitly provided Equality` {
        (all (toLists) shouldNot (contain only ("NICE", "MEET", "YOU"))) (decided by upperCaseStringEquality)
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain only ("YOU", "TO"))) (decided by upperCaseStringEquality)
        }
        all (toLists) shouldNot (contain only (" YOU ", " TO "))
        intercept[TestFailedException] {
          (all (toLists) shouldNot (contain only (" YOU ", " TO "))) (after being lowerCased and trimmed)
        }
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain only ())
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toLists) shouldNot (contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }
      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("to", "you")))) shouldNot (contain only Vector("to", "you"))
        }
        e1.failedCodeFileName.get should be ("ListShouldContainOnlySpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(Vector(Vector("to", "you"))) + " contained only (" + decorateToStringValue(Vector("to", "you")) + "), did you forget to say : _*" + " (ListShouldContainOnlySpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(Vector(Vector(Vector("to", "you"))))))
      }
    }
  }
}
