/*
* Copyright 2001-2013 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
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

class ListShouldContainOneOfAndOrSpec extends Spec with Matchers {
  
  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val fileName: String = "ListShouldContainOneOfAndOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    object `when used with (contain oneOf (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("happy", "birthday", "to", "you") and newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
/*
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotContainOneOfElements", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
*/
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (newContain newOneOf ("happy", "birthday", "to", "you") and newContain newOneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("fum", "fum", "fum", "fum") and newContain newOneOf ("fum", "fum", "fum"))
        }
        // checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumList, "\"fum\", \"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should ((newContain newOneOf ("happy", "birthday", "to", "you")) and (newContain newOneOf ("fum", "fum", "fum")))
        }
        // checkMessageStackDepth(e2, Resources("containedOneOfElements", fumList, "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("didNotContainOneOfElements", fumList, "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newOneOf ("happy", "birthday", "to", "you") and newContain newOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        intercept[TestFailedException] {
          
          (fumList should (newContain newOneOf ("fum", "fum", "fum", "fum") and newContain newOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        intercept[TestFailedException] {
          fumList should (newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM "))
        }
        (fumList should (newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      /*def `should do nothing if valid, else throw a TFE with an appropriate error message` {
}*/
      
      /*def `should use the implicit Equality in scope` {
}*/
      
    }
  }
}
