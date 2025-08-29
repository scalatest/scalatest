/*
 * Copyright 2001-2025 Artima, Inc.
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

class PrettyMethodsSpec extends funspec.AnyFunSpec with matchers.should.Matchers {
  describe("Trait PrettyMethods") {
    describe("should by default allow you to call pretty on anything and get default Prettifier output,") {
      import PrettyMethods._
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
    it("should pick up an overridden Prettifier") {
      implicit val prettifier =
        new Prettifier {
          def apply(o: Any) =
            o match {
              case s: String => "!!!" + s + "!!!"
              case _ => Prettifier.default(o)
            }
        }
      import PrettyMethods._
      "hi".pretty should be ("!!!hi!!!")
    }
  }
}


