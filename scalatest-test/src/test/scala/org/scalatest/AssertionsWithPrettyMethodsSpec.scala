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

import scala.collection.mutable.WrappedArray
import org.scalactic.PrettyMethods
import org.scalactic.Prettifier
import org.scalactic.Pretty

class AssertionsWithPrettyMethodsSpec extends FunSpec with Matchers with PrettyMethods {
  describe("Trait Assertions when PrettyMethods is mixed in") {
    describe("should by default allow you to call pretty on anything and get default Prettifier output,") {
      it("putting double quotes around strings") {
        "hi".pretty should be ("\"hi\"")
      }
      it("putting single quotes around chars") {
        'h'.pretty should be ("'h'")
      }
      it("putting print arrays") {
        Array(1, 2, 3).pretty should be ("Array(1, 2, 3)")
      }
      it("putting print wrapped arrays") {
        WrappedArray.make(Array(1, 2, 3)).pretty should be ("Array(1, 2, 3)")
      }
      it("putting the Unit value") {
        ().pretty should be ("<(), the Unit value>")
      }
      it("putting call toString on anything not specially treated") {
        List("1", "2", "3").pretty should be ("List(\"1\", \"2\", \"3\")")
      }
    }
    it("should allow .pretty output to be customized by defining a typeclass") {
      'c'.pretty shouldBe "'c'"
      implicit val prettyString: Pretty[String] = Pretty((s: String) => "!!! " + s + " !!!")
      "hello".pretty shouldBe "!!! hello !!!"
    }
  }
}


