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
  
  val fileName = "OptionShouldContainOneOfLogicalOrSpec.scala"

  object `an Option` {

    val fumSome: Option[String] = Some("fum")
    val toSome: Option[String] = Some("to")

    object `when used with (contain oneOf (...) or contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (newContain newOneOf ("fee", "fie", "foe", "fum") or newContain newOneOf ("fee", "fie", "fum", "foe"))
        fumSome should (newContain newOneOf ("fee", "fie", "foe", "fum") or newContain newOneOf ("happy", "birthday", "to", "you"))
        fumSome should (newContain newOneOf ("happy", "birthday", "to", "you") or newContain newOneOf ("fee", "fie", "fum", "foe"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (newContain newOneOf ("have", "a", "nice", "day") or newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"have\", \"a\", \"nice\", \"day\"") + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        fumSome should (newContain newOneOf ("happy", "birthday", "to", "you") or newContain newOneOf ("have", "a", "nice", "day"))
        fumSome should (newContain newOneOf ("happy", "birthday", "to", "you") or newContain newOneOf ("fum", "fum", "fum"))
        fumSome should (newContain newOneOf ("fum", "fum", "fum") or newContain newOneOf ("have", "a", "nice", "day"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (newContain newOneOf ("fum", "fum", "fum") or newContain newOneOf ("fum", "fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fum\", \"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (newContain newOneOf ("happy", "birthday", "to", "you") or newContain newOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumSome should (newContain newOneOf ("fum", "fum", "fum") or newContain newOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumSome should (newContain newOneOf ("happy", "birthday", "to", "you") or newContain newOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (newContain newOneOf ("fum", "fum", "fum") or newContain newOneOf ("fum", "fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumSome, "\"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fum\", \"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (...) or contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumSome should (be (fumSome) or newContain newOneOf ("fee", "fie", "fum", "foe"))
        fumSome should (be (toSome) or newContain newOneOf ("fee", "fie", "fum", "foe"))
        fumSome should (be (fumSome) or newContain newOneOf ("happy", "birthday", "to", "you"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) or newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome) + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        fumSome should (be (fumSome) or newContain newOneOf ("have", "a", "nice", "day"))
        fumSome should (be (toSome) or newContain newOneOf ("have", "a", "nice", "day"))
        fumSome should (be (fumSome) or newContain newOneOf ("fum", "fum", "fum"))
        
        val e1 = intercept[TestFailedException] {
          fumSome should (be (toSome) or newContain newOneOf ("fum", "fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome) + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fum\", \"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (fumSome should (be (fumSome) or newContain newOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality)
        (fumSome should (be (toSome) or newContain newOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality)
        (fumSome should (be (fumSome) or newContain newOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (fumSome should (be (toSome) or newContain newOneOf ("fum", "fum", "fum", "fum"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumSome, toSome) + ", and " + Resources("didNotContainOneOfElements", fumSome, "\"fum\", \"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
    }

    object `when used with (not contain oneOf (...) or not contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("fee", "fie", "fum", "foe"))
        toSome should (not newContain newOneOf ("happy", "birthday", "to", "you") or not newContain newOneOf ("fee", "fie", "fum", "foe"))
        toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          toSome should (not newContain newOneOf ("nice", "to", "meet", "you") or not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"nice\", \"to\", \"meet\", \"you\"") + ", and " + Resources("containedOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        toSome should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("to", "to", "to"))
        toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("to", "to", "to", "to"))
        toSome should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("fee", "fie", "foe"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedOneOfElements", toSome, "\"fee\", \"fie\", \"foe\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("to", "to", "to", "to"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("to", "to", "to", "to"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (toSome should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("fee", "fie", "foe"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", toSome, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedOneOfElements", toSome, "\"fee\", \"fie\", \"foe\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) or not contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        toSome should (not be (fumSome) or not newContain newOneOf ("fee", "fie", "fum", "foe"))
        toSome should (not be (toSome) or not newContain newOneOf ("fee", "fie", "fum", "foe"))
        toSome should (not be (fumSome) or not newContain newOneOf ("happy", "birthday", "to", "you"))
        val e2 = intercept[TestFailedException] {
          toSome should (not be (toSome) or not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", toSome, toSome) + ", and " + Resources("containedOneOfElements", toSome, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        toSome should (not be (fumSome) or not newContain newOneOf ("to", "to", "to", "to"))
        toSome should (not be (toSome) or not newContain newOneOf ("to", "to", "to", "to"))
        toSome should (not be (fumSome) or not newContain newOneOf ("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          toSome should (not be (toSome) or not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", toSome, toSome) + ", and " + Resources("containedOneOfElements", toSome, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (toSome should (not be (fumSome) or not newContain newOneOf ("to", "to", "to", "to"))) (decided by invertedStringEquality)
        (toSome should (not be (toSome) or not newContain newOneOf ("to", "to", "to", "to"))) (decided by invertedStringEquality)
        (toSome should (not be (fumSome) or not newContain newOneOf ("fee", "fie", "fum", "foe"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (toSome should (not be (toSome) or not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", toSome, toSome) + ", and " + Resources("containedOneOfElements", toSome, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
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
        all (some1s) should (newContain newOneOf (1, 3, 4) or newContain newOneOf (1, 6, 8))
        all (some1s) should (newContain newOneOf (2, 3, 4) or newContain newOneOf (1, 6, 8))
        all (some1s) should (newContain newOneOf (1, 3, 4) or newContain newOneOf (2, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (somes) should (newContain newOneOf (1, 6, 8) or newContain newOneOf (1, 3, 4)) 
        }
        checkMessageStackDepth(e1, allErrMsg(2, "Some(2) did not contain one of (1, 6, 8), and Some(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, somes), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("ho", "he", "howdy") or newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) did not contain one of (\"ho\", \"he\", \"howdy\"), and Some(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (newContain newOneOf ("hi") or newContain newOneOf ("hi"))
        all (hiSomes) should (newContain newOneOf ("he") or newContain newOneOf ("hi"))
        all (hiSomes) should (newContain newOneOf ("hi") or newContain newOneOf ("he"))
        intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("ho") or newContain newOneOf ("ho"))
        }
        implicit val ise = invertedStringEquality
        all (hiSomes) should (newContain newOneOf ("ho") or newContain newOneOf ("he"))
        all (hiSomes) should (newContain newOneOf ("hi") or newContain newOneOf ("he"))
        all (hiSomes) should (newContain newOneOf ("ho") or newContain newOneOf ("hi"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (newContain newOneOf ("hi") or newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) did not contain one of (\"hi\"), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (newContain newOneOf ("ho") or newContain newOneOf ("he"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiSomes) should (newContain newOneOf ("hi") or newContain newOneOf ("he"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiSomes) should (newContain newOneOf ("ho") or newContain newOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (newContain newOneOf ("hi") or newContain newOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) did not contain one of (\"hi\"), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (...) or contain oneOf (...))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (some1s) should (be (Some(1)) or newContain newOneOf (1, 6, 8))
        all (some1s) should (be (Some(2)) or newContain newOneOf (1, 6, 8))
        all (some1s) should (be (Some(1)) or newContain newOneOf (2, 6, 8))

        val e1 = intercept[TestFailedException] {
          all (some1s) should (be (Some(2)) or newContain newOneOf (2, 3, 8)) 
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(1) was not equal to Some(2), and Some(1) did not contain one of (2, 3, 8)", thisLineNumber - 2, some1s), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("to")) or newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "Some(hi) was not equal to Some(to), and Some(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        all (hiSomes) should (be (Some("hi")) or newContain newOneOf ("hi"))
        all (hiSomes) should (be (Some("hi")) or newContain newOneOf ("he"))
        all (hiSomes) should (be (Some("he")) or newContain newOneOf ("hi"))
        intercept[TestFailedException] {
          all (hiSomes) should (be (Some("ho")) or newContain newOneOf ("he"))
        }
        implicit val ise = invertedStringEquality
        all (hiSomes) should (be (Some("hi")) or newContain newOneOf ("he"))
        all (hiSomes) should (be (Some("he")) or newContain newOneOf ("he"))
        all (hiSomes) should (be (Some("hi")) or newContain newOneOf ("hi"))
        val e1 = intercept[TestFailedException] {
          all (hiSomes) should (be (Some("he")) or newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) was not equal to Some(he), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (hiSomes) should (be (Some("hi")) or newContain newOneOf ("he"))) (decided by invertedStringEquality)
        (all (hiSomes) should (be (Some("he")) or newContain newOneOf ("he"))) (decided by invertedStringEquality)
        (all (hiSomes) should (be (Some("hi")) or newContain newOneOf ("hi"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiSomes) should (be (Some("he")) or newContain newOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(hi) was not equal to Some(he), and Some(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiSomes), fileName, thisLineNumber - 2)
      }
      
    }

    object `when used with (not contain oneOf (..) or not contain oneOf (..)) ` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not newContain newOneOf ("happy", "birthday", "to", "you") or not newContain newOneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not newContain newOneOf ("nice", "to", "meet", "you") or not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"nice\", \"to\", \"meet\", \"you\"), and Some(to) contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        all (toSomes) should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("to", "to", "to"))
        all (toSomes) should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("fee", "fie", "foe", "fum"))
        all (toSomes) should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("to", "to", "to"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not newContain newOneOf ("have", "a", "nice", "day") or not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"have\", \"a\", \"nice\", \"day\"), and Some(to) contained one of (\"fee\", \"fie\", \"foe\", \"fum\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("to", "to", "to"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (toSomes) should (not newContain newOneOf ("to", "to", "to", "to") or not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (toSomes) should (not newContain newOneOf ("fee", "fie", "foe", "fum") or not newContain newOneOf ("to", "to", "to"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not newContain newOneOf ("have", "a", "nice", "day") or not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) contained one of (\"have\", \"a\", \"nice\", \"day\"), and Some(to) contained one of (\"fee\", \"fie\", \"foe\", \"fum\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (..) or not contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (toSomes) should (not be (Some("fee")) or not newContain newOneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not be (Some("to")) or not newContain newOneOf ("have", "a", "nice", "day"))
        all (toSomes) should (not be (Some("fee")) or not newContain newOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) or not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to), and Some(to) contained one of (\"happy\", \"birthday\", \"to\", \"you\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        all (toSomes) should (not be (Some("hi")) or not newContain newOneOf ("to", "to", "to"))
        all (toSomes) should (not be (Some("to")) or not newContain newOneOf ("to", "to", "to"))
        all (toSomes) should (not be (Some("hi")) or not newContain newOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          all (toSomes) should (not be (Some("to")) or not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to), and Some(to) contained one of (\"fee\", \"fie\", \"foe\", \"fum\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
      def `should use an explicitly provided Equality` {
        (all (toSomes) should (not be (Some("hi")) or not newContain newOneOf ("to", "to", "to"))) (decided by invertedStringEquality)
        (all (toSomes) should (not be (Some("to")) or not newContain newOneOf ("to", "to", "to"))) (decided by invertedStringEquality)
        (all (toSomes) should (not be (Some("hi")) or not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (toSomes) should (not be (Some("to")) or not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "Some(to) was equal to Some(to), and Some(to) contained one of (\"fee\", \"fie\", \"foe\", \"fum\")", thisLineNumber - 2, toSomes), fileName, thisLineNumber - 2)
      }
    }
  }
}
