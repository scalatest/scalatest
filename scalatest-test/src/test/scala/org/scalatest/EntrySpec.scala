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

import java.util.{Map => JMap}
import java.util.{HashMap => JHashMap}
import Matchers._

class EntrySpec extends FunSpec {

  describe("An org.scalatest.Entry") {
    it("can be compared against an Entry coming from a java.util.Map") {
      val jmap: JMap[String, Int] = new JHashMap[String, Int]
      jmap.put("one", 1)
      jmap.put("two", 2)
      jmap.put("three", 3)

      jmap.entrySet should contain (Entry("one", 1))
      jmap.entrySet should not contain (Entry("one", 100))
      jmap.entrySet should contain allOf (Entry("one", 1), Entry("two", 2))
    }
    it("should have a toString consistent with the ones coming from Java") {
      Entry("one", 1).toString should be ("one=1")
      Entry(1, "one").toString should be ("1=one")
    }
  }
  describe("the loneElement method") {
    describe("when used with java.util.Map") {
      it("should return an Entry that has key and value methods") {
        import LoneElement._
        val jmap: JMap[String, Int] = new JHashMap[String, Int]
        jmap.put("one", 1)
        jmap.loneElement.key should be ("one")
        jmap.loneElement.value should be (1)
      }
    }
  }
}

