/*
 * Copyright 2001-2024 Artima, Inc.
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

import scala.xml.{Node, Text, NodeSeq}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class StreamlinedXmlEqualitySpec extends AnyFunSpec with Matchers {

  import StreamlinedXmlEquality._

  describe("Streamlined Xml Equality of Elems") {

    it("should leave already-normalized XML alone") {
      <summer></summer> should equal (<summer></summer>)
    }

    it("should zap text that is only whitespace") {

      <summer> </summer> should equal (<summer></summer>)

      <summer>
       </summer> should equal (<summer></summer>)

      <summer>
        <day></day>
      </summer> should equal (<summer><day></day></summer>)

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>
      )

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>
      )

      <div>{Text("My name is ")}{Text("Harry")}</div> should equal (<div>My name is Harry</div>)
    }
  }

  describe("Streamlined Xml Equality of Nodes") {

    it("should leave already-normalized XML alone") {

      (<summer></summer>: Node) shouldEqual (<summer></summer>)

      (Text("Bla"): Node) shouldEqual (Text("Bla"))
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

      (<summer> </summer>: Node) should equal (<summer></summer>)

      (<summer>
      </summer>: Node) should equal (<summer></summer>)

      (<summer>
        <day></day>
      </summer>: Node) should equal (<summer><day></day></summer>)

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>: Node
      )

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>: Node
      )

      (Text("   "): Node) should equal (Text("   "))

      (<div>{Text("My name is ")}{Text("Harry")}</div>: Node) should equal (<div>My name is Harry</div>)
    }
  }

  describe("Streamlined Xml Normalization of NodeSeq") {

    it("should leave already-normalized XML alone") {

      (<summer></summer>: NodeSeq) should equal (<summer></summer>)

      (Text("Bla"): NodeSeq) should equal (Text("Bla"))
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

      (<summer> </summer>: NodeSeq) should equal (<summer></summer>)

      (<summer>
      </summer>: NodeSeq) should equal (<summer></summer>)

      (<summer>
        <day></day>
      </summer>: NodeSeq) should equal (<summer><day></day></summer>)

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>: NodeSeq
      )

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>: NodeSeq
      )

      (Text("   "): NodeSeq) should equal (Text("   "))

      (<div>{Text("My name is ")}{Text("Harry")}</div>: NodeSeq) should equal (<div>My name is Harry</div>)
    }
  }
}

