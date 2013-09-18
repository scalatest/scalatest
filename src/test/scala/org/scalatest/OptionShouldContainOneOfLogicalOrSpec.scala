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
import org.scalautils.StringNormalizations._
import SharedHelpers._

class OptionShouldContainOneOfLogicalOrSpec extends Spec with Matchers {

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }
  
  val fileName = "OptionShouldContainOneOfLogicalOrSpec.scala"

  object `an Option` {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    object `when used with (contain oneOf (...) or contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))
        fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("happy", "birthday", "to", "you"))
        fumSome should (contain oneOf ("happy", "birthday", "to", "you") or contain oneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("have", "a", "nice", "day") or contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"have\", \"a\", \"nice\", \"day\"") + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("fee", "fie", "foe", "fum"))
        fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumSome should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf () or contain oneOf ("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfEmpty")))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf ("fee", "fie", "foe", "fie", "fum") or contain oneOf ("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumSome should (contain oneOf ("fee", "fie", "fum", "foe") or contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (be (...) or contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        fumSome should (be (toSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        fumSome should (be (fumSome) or contain oneOf ("happy", "birthday", "to", "you"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) or contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome) + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (be (fumSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))
        fumSome should (be (toSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))
        fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) or contain oneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome) + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (be (fumSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumSome should (be (toSome) or contain oneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (be (toSome) or contain oneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome) + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (be (fumSome) or contain oneOf ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfEmpty")))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumSome should (be (fumSome) or contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    object `when used with (not contain oneOf (...) or not contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("nice", "to", "meet", "you") or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"nice\", \"to\", \"meet\", \"you\"") + ", and " + Resources("containedOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))
        toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))
        toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        val e1 = intercept[TestFailedException] {
          toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"") + ", and " + Resources("containedOneOfElements", toSome, "\"NICE\", \"TO\", \"MEET\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (toSome should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"") + ", and " + Resources("containedOneOfElements", toSome, "\"NICE\", \"TO\", \"MEET\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") or not contain oneOf ("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          toSome should (not contain oneOf ("fee", "fie", "fum", "foe") or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (not be (...) or not contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not be (fumSome) or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not be (toSome) or not contain oneOf ("fee", "fie", "fum", "foe"))
        toSome should (not be (fumSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        val e2 = intercept[TestFailedException] {
          toSome should (not be (toSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", toSome, toSome) + ", and " + Resources("containedOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toSome should (not be (fumSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        toSome should (not be (toSome) or not contain oneOf ("happy", "birthday", "to", "you"))
        toSome should (not be (fumSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", toSome, toSome) + ", and " + Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not be (fumSome) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (toSome should (not be (toSome) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (toSome should (not be (fumSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not be (toSome) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", toSome, toSome) + ", and " + Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          toSome should (not be (fumSome) or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
  }

  object `a collection of Options` {

    val some1s: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(1))
    val somes: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(2))
    val nones: Vector[Option[Int]] = Vector(None, None, None)
    val somesNone: Vector[Option[Int]] = Vector(Some(1), Some(1), None)
    val hiSomes: Vector[Option[String]] = Vector(Some("hi"), Some("hi"), Some("hi"))
    val toSomes: Vector[Option[String]] = Vector(Some("to"), Some("to"), Some("to"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + left

    object `when used with (contain oneOf (...) or contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (some1s) should (contain oneOf (1, 3, 4) or contain oneOf (1, 6, 8))
        all (some1s) should (contain oneOf (2, 3, 4) or contain oneOf (1, 6, 8))
        all (some1s) should (contain oneOf (1, 3, 4) or contain oneOf (2, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (somes) should (contain oneOf (1, 6, 8) or contain oneOf (1, 3, 4)) 
        }
        checkMessageStackDepth(e1, allErrMsg(2, "Some(2) did not contain one of (1, 6, 8), and Some(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("ho", "he", "howdy") or contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) did not contain one of (\"ho\", \"he\", \"howdy\"), and Some(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (contain oneOf ("hi") or contain oneOf ("hi"))
        all (hiSomes) should (contain oneOf ("he") or contain oneOf ("hi"))
        all (hiSomes) should (contain oneOf ("hi") or contain oneOf ("he"))
        intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("ho") or contain oneOf ("ho"))
        }
        implicit val ise = invertedStringEquality
        all (hiSomes) should (contain oneOf ("ho") or contain oneOf ("he"))
        all (hiSomes) should (contain oneOf ("hi") or contain oneOf ("he"))
        all (hiSomes) should (contain oneOf ("ho") or contain oneOf ("hi"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (contain oneOf ("hi") or contain oneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) did not contain one of (\"hi\"), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (contain oneOf ("ho") or contain oneOf ("he"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiSomes) should (contain oneOf ("hi") or contain oneOf ("he"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiSomes) should (contain oneOf ("ho") or contain oneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (contain oneOf ("hi") or contain oneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) did not contain one of (\"hi\"), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf () or contain oneOf (1, 6, 8))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf (1, 6, 8) or contain oneOf ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfEmpty")))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf (1, 2, 2, 3) or contain oneOf (1, 6, 8))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (contain oneOf (1, 6, 8) or contain oneOf (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (be (...) or contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (some1s) should (be (Some(1)) or contain oneOf (1, 6, 8))
        all (some1s) should (be (Some(2)) or contain oneOf (1, 6, 8))
        all (some1s) should (be (Some(1)) or contain oneOf (2, 6, 8))

        val e1 = intercept[TestFailedException] {
          all (some1s) should (be (Some(2)) or contain oneOf (2, 3, 8)) 
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(1) was not equal to Some(2), and Some(1) did not contain one of (2, 3, 8)", thisLineNumber - 2, some1s), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("to")) or contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) was not equal to Some(to), and Some(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (be (Some("hi")) or contain oneOf ("hi"))
        all (hiSomes) should (be (Some("hi")) or contain oneOf ("he"))
        all (hiSomes) should (be (Some("he")) or contain oneOf ("hi"))
        intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) or contain oneOf ("he"))
        }
        implicit val ise = invertedStringEquality
        all (hiSomes) should (be (Some("hi")) or contain oneOf ("he"))
        all (hiSomes) should (be (Some("he")) or contain oneOf ("he"))
        all (hiSomes) should (be (Some("hi")) or contain oneOf ("hi"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("he")) or contain oneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) was not equal to Some(he), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (be (Some("hi")) or contain oneOf ("he"))) (decided by invertedStringEquality)
        (all (hiSomes) should (be (Some("he")) or contain oneOf ("he"))) (decided by invertedStringEquality)
        (all (hiSomes) should (be (Some("hi")) or contain oneOf ("hi"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("he")) or contain oneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) was not equal to Some(he), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (be (Some(1)) or contain oneOf ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfEmpty")))
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (some1s) should (be (Some(1)) or contain oneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    object `when used with (not contain oneOf (..) or not contain oneOf (..)) ` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("nice", "to", "meet", "you") or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"nice\", \"to\", \"meet\", \"you\"), and Some(to) contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))
        all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))
        all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"), and Some(to) contained one of (\"NICE\", \"TO\", \"MEET\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (toSomes) should (not contain oneOf ("happy", "birthday", "to", "you") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") or not contain oneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\"), and Some(to) contained one of (\"NICE\", \"TO\", \"MEET\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") or not contain oneOf ("have", "a", "nice", "day"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not contain oneOf ("have", "a", "nice", "day") or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (not be (..) or not contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not be (Some("fee")) or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not be (Some("to")) or not contain oneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not be (Some("fee")) or not contain oneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) or not contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to), and Some(to) contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not be (Some("hi")) or not contain oneOf ("happy", "birthday", "to", "you"))
        all (toSomes) should (not be (Some("to")) or not contain oneOf ("happy", "birthday", "to", "you"))
        all (toSomes) should (not be (Some("hi")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to), and Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not be (Some("hi")) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (all (toSomes) should (not be (Some("to")) or not contain oneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        (all (toSomes) should (not be (Some("hi")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("to")) or not contain oneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to), and Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (toSomes) should (not be (Some("fee")) or not contain oneOf ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
  }
}
