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
package org.scalactic

import org.scalatest._
import scala.collection.mutable.WrappedArray

class PrettyMethodsSpec extends Spec with Matchers {
  object `Trait PrettyMethods` {
    object `should by default allow you to call pretty on anything and get default Prettifier output,` {
      import PrettyMethods._
      def `putting double quotes around strings` {
        "hi".pretty should be ("\"hi\"")
      }
      def `putting single quotes around chars` {
        'h'.pretty should be ("'h'")
      }
      def `putting print arrays` {
        Array(1, 2, 3).pretty should be ("Array(1, 2, 3)")
      }
      def `putting print wrapped arrays` {
        WrappedArray.make(Array(1, 2, 3)).pretty should be ("Array(1, 2, 3)")
      }
      def `putting the Unit value` {
        ().pretty should be ("<(), the Unit value>")
      }
      def `putting call toString on anything not specially treated` {
        List("1", "2", "3").pretty should be ("List(\"1\", \"2\", \"3\")")
      }
    }
/* This proved that I got rid of the Any => String conversion, but by not compiling. 
    def `should not simply convert Any to String` {
      new ConversionCheckedTripleEquals {
        import PrettyMethods._
        "2" should === (2)
      }
    }
*/
  }
}


