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

class FormatterSpec extends Spec with Matchers {
  object `A Formatter` {
    def `should convert an Any to a String` {
      val f =
        new Formatter {
          def apply(o: Any) = o.toString
        }

      f("hi") should be ("hi")
      f(List("hi")) should be ("List(hi)")
    }
  }
  object `the default Formatter` {
    def `should put double quotes around strings` {
      Formatter.default("hi") should be ("\"hi\"")
    }
    def `should put single quotes around chars` {
      Formatter.default('h') should be ("'h'")
    }
    def `should pretty print arrays` {
      Formatter.default(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    def `should pretty print wrapped arrays` {
      Formatter.default(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    def `should show null as "null"` {
      Formatter.default(null) should be ("null")
    }
    def `should clarify the Unit value` {
      Formatter.default(()) should be ("<(), the Unit value>")
    }
    def `should just call toString on anything not specially treated` {
      Formatter.default(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
  }
}

