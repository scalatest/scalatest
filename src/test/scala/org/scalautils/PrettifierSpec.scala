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
    def `should convert an IndexedSeq[Any] to a IndexedSeq[String]` {
      val f =
        new Prettifier {
          def apply(raws: IndexedSeq[Any]) = raws map (_.toString)
        }

      f(Vector("hi")) should be (Vector("hi"))
      f(Vector(List("hi"))) should be (Vector("List(hi)"))
    }
  }
  object `the default Prettifier` {
    def `should put double quotes around strings` {
      Prettifier.defaultPrettify("hi") should be ("\"hi\"")
    }
    def `should put single quotes around chars` {
      Prettifier.defaultPrettify('h') should be ("'h'")
    }
    def `should pretty print arrays` {
      Prettifier.defaultPrettify(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    def `should pretty print wrapped arrays` {
      Prettifier.defaultPrettify(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    def `should show null as "null"` {
      Prettifier.defaultPrettify(null) should be ("null")
    }
    def `should clarify the Unit value` {
      Prettifier.defaultPrettify(()) should be ("<(), the Unit value>")
    }
    def `should just call toString on anything not specially treated` {
      Prettifier.defaultPrettify(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
  }
}

