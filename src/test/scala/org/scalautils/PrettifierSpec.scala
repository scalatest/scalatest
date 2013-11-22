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
package org.scalautils

import org.scalatest._
import scala.collection.mutable.WrappedArray

class PrettifierSpec extends Spec with Matchers {
  object `A Prettifier` {
    def `should convert an Any to a String` {
      val f =
        new Prettifier {
          def apply(o: Any) = o.toString
        }

      f("hi") should be ("hi")
      f(List("hi")) should be ("List(hi)")
    }
    def `can be composed with another Prettifier` {
      case class Yell(secret: String)
      val myLittlePretty =
        new Prettifier {
          def apply(o: Any) =
            o match {
              case Yell(secret) => secret.toUpperCase + "!!!"
              case _ => Prettifier.default(o)
            }
        }
      myLittlePretty(Yell("I like fruit loops")) should be ("I LIKE FRUIT LOOPS!!!")
      myLittlePretty("hi") should be ("\"hi\"")
      myLittlePretty('h') should be ("'h'")
      myLittlePretty(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
      myLittlePretty(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
      myLittlePretty(null) should be ("null")
      myLittlePretty(()) should be ("<(), the Unit value>")
      myLittlePretty(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
  }

  object `the basic Prettifier` {
    def `should put double quotes around strings` {
      Prettifier.basic("hi") should be ("\"hi\"")
    }
    def `should put single quotes around chars` {
      Prettifier.basic('h') should be ("'h'")
    }
    def `should pretty print arrays` {
      Prettifier.basic(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    def `should pretty print wrapped arrays` {
      Prettifier.basic(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    def `should pretty print string arrays` {
      Prettifier.basic(Array("1", "2", "3")) should be ("Array(1, 2, 3)")
    }
    def `should pretty print nested string arrays` {
      Prettifier.basic(Array(Array("1", "2", "3"))) should be ("Array(Array(1, 2, 3))")
    }
    def `should pretty print wrapped string arrays` {
      Prettifier.basic(WrappedArray.make(Array("1", "2", "3"))) should be ("Array(1, 2, 3)")
    }
    def `should show null as "null"` {
      Prettifier.basic(null) should be ("null")
    }
    def `should clarify the Unit value` {
      Prettifier.basic(()) should be ("<(), the Unit value>")
    }
    def `should just call toString on anything not specially treated` {
      Prettifier.basic(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
    def `should pretty print GenTraversable` {
      Prettifier.basic(List(1, 2, 3)) should be ("List(1, 2, 3)")
    }
    def `should pretty print string GenTraversable` {
      Prettifier.basic(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
    def `should pretty print nested string GenTraversable` {
      Prettifier.basic(List(List("1", "2", "3"))) should be ("List(List(1, 2, 3))")
    }
  }

  object `the default Prettifier` {
    def `should put double quotes around strings` {
      Prettifier.default("hi") should be ("\"hi\"")
    }
    def `should put single quotes around chars` {
      Prettifier.default('h') should be ("'h'")
    }
    def `should pretty print arrays` {
      Prettifier.default(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    def `should pretty print wrapped arrays` {
      Prettifier.default(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    def `should pretty print string arrays` {
      Prettifier.default(Array("1", "2", "3")) should be ("Array(\"1\", \"2\", \"3\")")
    }
    def `should pretty print nested string arrays` {
      Prettifier.default(Array(Array("1", "2", "3"))) should be ("Array(Array(\"1\", \"2\", \"3\"))")
    }
    def `should pretty print wrapped string arrays` {
      Prettifier.default(WrappedArray.make(Array("1", "2", "3"))) should be ("Array(\"1\", \"2\", \"3\")")
    }
    def `should show null as "null"` {
      Prettifier.default(null) should be ("null")
    }
    def `should clarify the Unit value` {
      Prettifier.default(()) should be ("<(), the Unit value>")
    }
    def `should just call toString on anything not specially treated` {
      Prettifier.default(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
    def `should pretty print GenTraversable` {
      Prettifier.default(List(1, 2, 3)) should be ("List(1, 2, 3)")
    }
    def `should pretty print string GenTraversable` {
      Prettifier.default(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
    def `should pretty print nested string GenTraversable` {
      Prettifier.default(List(List("1", "2", "3"))) should be ("List(List(\"1\", \"2\", \"3\"))")
    }
  }
}

