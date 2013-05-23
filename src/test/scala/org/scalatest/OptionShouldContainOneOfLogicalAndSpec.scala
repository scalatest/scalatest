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

class OptionShouldContainOneOfLogicalAndSpec extends Spec with Matchers {
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val fileName = "OptionShouldContainOneOfLogicalAndSpec.scala"

  object `an Option` {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    object `when used with (contain oneOf (...) and contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (newContain newOneOf ("happy", "birthday", "to", "you") and newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumSome, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotContainOneOfElements", fumSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumSome, "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (fumSome should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumSome, "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (...) and contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (be (fumSome) and newContain newOneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) and newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (be (fumSome) and newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", fumSome, fumSome) + ", but " + Resources("didNotContainOneOfElements", fumSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        fumSome should (be (fumSome) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          fumSome should (be (fumSome) and newContain newOneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", fumSome, fumSome) + ", but " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (be (fumSome) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (be (toSome) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (fumSome should (be (fumSome) and newContain newOneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", fumSome, fumSome) + ", but " + Resources("didNotContainOneOfElements", fumSome, "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
    }

    object `when used with (not contain oneOf (...) and not contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") and not newContain newOneOf ("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          toSome should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") and not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", toSome, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("containedOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toSome should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toSome should (not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not newContain newOneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (toSome should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
        
        toSome should not newContain newOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ")
        
        val e3 = intercept[TestFailedException] {
          (toSome should (not newContain newOneOf (" HAPPY ", " BIRTHDAY ", " TO ", " YOU ") and not newContain newOneOf ("to", "to", "to", "to"))) (after being lowerCased and trimmed, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e3, Resources("containedOneOfElements", toSome, "\" HAPPY \", \" BIRTHDAY \", \" TO \", \" YOU \""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) and not contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not be (fumSome) and not newContain newOneOf ("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", toSome, toSome), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not be (fumSome) and not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", toSome, fumSome) + ", but " + Resources("containedOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        toSome should (not be (fumSome) and not newContain newOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) and not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", toSome, toSome), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          toSome should (not be (fumSome) and not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", toSome, fumSome) + ", but " + Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not be (fumSome) and not newContain newOneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not be (toSome) and not newContain newOneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", toSome, toSome), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (toSome should (not be (fumSome) and not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", toSome, fumSome) + ", but " + Resources("containedOneOfElements", toSome, "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
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

    object `when used with (contain oneOf (...) and contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (some1s) should (newContain newOneOf (1, 3, 4) and newContain newOneOf (1, 6, 8))
        atLeast (2, somes) should (newContain newOneOf (1, 3, 4) and newContain newOneOf (1, 6, 8)) 
        atMost (2, somes) should (newContain newOneOf (2, 3, 4) and newContain newOneOf (2, 6, 8))
        no (somes) should (newContain newOneOf (3, 4, 5) and newContain newOneOf (7, 8, 9))

        val e1 = intercept[TestFailedException] {
          all (somes) should (newContain newOneOf (1, 3, 4) and newContain newOneOf (1, 2, 8)) 
        }
        checkMessageStackDepth(e1, allErrMsg(2, "Some(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (somes) should (newContain newOneOf (1, 2, 8) and newContain newOneOf (1, 3, 4)) 
        }
        checkMessageStackDepth(e2, allErrMsg(2, "Some(2) contained one of (1, 2, 8), but Some(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("ho", "hey", "howdy") and newContain newOneOf ("ho", "hi", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "Some(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("ho", "hi", "howdy") and newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "Some(hi) contained one of (\"ho\", \"hi\", \"howdy\"), but Some(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (newContain newOneOf ("hi") and newContain newOneOf ("hi"))
        intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("ho") and newContain newOneOf ("hi"))
        }
        implicit val ise = invertedStringEquality
        all (hiSomes) should (newContain newOneOf ("ho") and newContain newOneOf ("he"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("hi") and newContain newOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("ho") and newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) contained one of (\"ho\"), but Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (newContain newOneOf ("ho") and newContain newOneOf ("he"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (newContain newOneOf ("hi") and newContain newOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (hiSomes) should (newContain newOneOf ("ho") and newContain newOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) contained one of (\"ho\"), but Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (...) and contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (some1s) should (be (Some(1)) and newContain newOneOf (1, 6, 8))
        atLeast (2, somes) should (be (Some(1)) and newContain newOneOf (1, 6, 8)) 
        atMost (2, somes) should (be (Some(1)) and newContain newOneOf (2, 6, 8))
        no (somes) should (be (Some(3)) and newContain newOneOf (7, 8, 9))

        val e1 = intercept[TestFailedException] {
          all (somes) should (be (Some(1)) and newContain newOneOf (1, 2, 8)) 
        }
        checkMessageStackDepth(e1, allErrMsg(2, "Some(2) was not equal to Some(1)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (some1s) should (be (Some(1)) and newContain newOneOf (2, 3, 8)) 
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(1) was equal to Some(1), but Some(1) did not contain one of (2, 3, 8)", thisLineNumber - 2, some1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hei")) and newContain newOneOf ("ho", "hi", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "Some(hi) was not equal to Some(hei)", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hi")) and newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "Some(hi) was equal to Some(hi), but Some(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (be (Some("hi")) and newContain newOneOf ("hi"))
        intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) and newContain newOneOf ("hi"))
        }
        implicit val ise = invertedStringEquality
        all (hiSomes) should (be (Some("hi")) and newContain newOneOf ("he"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) and newContain newOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) was not equal to Some(ho)", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("hi")) and newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) was equal to Some(hi), but Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (be (Some("hi")) and newContain newOneOf ("he"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("ho")) and newContain newOneOf ("ho"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) was not equal to Some(ho)", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("hi")) and newContain newOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) was equal to Some(hi), but Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      
    }

    object `when used with (not contain oneOf (..) and not contain oneOf (..)) ` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not newContain newOneOf ("fee", "fie", "foe", "fum") and not newContain newOneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("have", "a", "nice", "day"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not newContain newOneOf ("have", "a", "nice", "day") and not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(to) did not contain one of (\"have\", \"a\", \"nice\", \"day\"), but Some(to) contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("nice", "to", "meet", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not newContain newOneOf ("nice", "to", "meet", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("NICE", "TO", "MEET", "YOU"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(to) did not contain one of (\"happy\", \"birthday\", \"to\", \"you\"), but Some(to) contained one of (\"NICE\", \"TO\", \"MEET\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and not newContain newOneOf ("nice", "to", "meet", "you"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (toSomes) should (not newContain newOneOf ("happy", "birthday", "to", "you") and not newContain newOneOf ("NICE", "TO", "MEET", "YOU"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(to) did not contain one of (\"happy\", \"birthday\", \"to\", \"you\"), but Some(to) contained one of (\"NICE\", \"TO\", \"MEET\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) and not contain oneOf (...)) ` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not be (Some("fee")) and not newContain newOneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) and not newContain newOneOf ("have", "a", "nice", "day"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to)", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("nice")) and not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(to) was not equal to Some(nice), but Some(to) contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        all (toSomes) should (not be (Some("hi")) and not newContain newOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) and not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to)", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("hi")) and not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(to) was not equal to Some(hi), but Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not be (Some("hi")) and not newContain newOneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("to")) and not newContain newOneOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to)", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("he")) and not newContain newOneOf ("HAPPY", "BIRTHDAY", "TO", "YOU"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(to) was not equal to Some(he), but Some(to) contained one of (\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
    }
  }
}
