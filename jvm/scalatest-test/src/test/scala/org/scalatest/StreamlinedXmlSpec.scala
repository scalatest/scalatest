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

import scala.xml.{Elem, Node, Text, NodeSeq}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StreamlinedXmlSpec extends AnyFunSpec with Matchers with StreamlinedXml {

  describe("Xml Equality of Elems (after being streamlined)") {

    it("should leave already-normalized XML alone") {
      <summer></summer> should equal (<summer></summer>) (after being streamlined[Elem])
    }

    it("should zap text that is only whitespace") {

      <summer> </summer> should equal (<summer></summer>) (after being streamlined[Elem])

      <summer>
       </summer> should equal (<summer></summer>) (after being streamlined[Elem])

      <summer>
        <day></day>
      </summer> should equal (<summer><day></day></summer>) (after being streamlined[Elem])

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>
      ) (after being streamlined[Elem])

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>
      ) (after being streamlined[Elem])

      <div>{Text("My name is ")}{Text("Harry")}</div> should equal (<div>My name is Harry</div>) (after being streamlined[Elem])
    }
  }

  describe("Xml Equality of Nodes (after being streamlined)") {

    it("should leave already-normalized XML alone") {

      ((<summer></summer>: Node) shouldEqual (<summer></summer>)) (after being streamlined[Node])

      ((Text("Bla"): Node) shouldEqual (Text("Bla"))) (after being streamlined[Node])
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

      (<summer> </summer>: Node) should equal (<summer></summer>) (after being streamlined[Node])

      (<summer>
      </summer>: Node) should equal (<summer></summer>) (after being streamlined[Node])

      (<summer>
        <day></day>
      </summer>: Node) should equal (<summer><day></day></summer>) (after being streamlined[Node])

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>: Node
      ) (after being streamlined[Node])

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>: Node
      ) (after being streamlined[Node])

      (Text("   "): Node) should equal (Text("   ")) (after being streamlined[Node])

      (<div>{Text("My name is ")}{Text("Harry")}</div>: Node) should equal (<div>My name is Harry</div>) (after being streamlined[Node])
    }
  }

  describe("Xml Equality of NodeSeq (after being streamlined)") {

    it("should leave already-normalized XML alone") {

      (<summer></summer>: NodeSeq) should equal (<summer></summer>) (after being streamlined[NodeSeq])

      (Text("Bla"): NodeSeq) should equal (Text("Bla")) (after being streamlined[NodeSeq])
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

      (<summer> </summer>: NodeSeq) should equal (<summer></summer>) (after being streamlined[NodeSeq])

      (<summer>
      </summer>: NodeSeq) should equal (<summer></summer>) (after being streamlined[NodeSeq])

      (<summer>
        <day></day>
      </summer>: NodeSeq) should equal (<summer><day></day></summer>) (after being streamlined[NodeSeq])

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>: NodeSeq
      ) (after being streamlined[NodeSeq])

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>: NodeSeq
      ) (after being streamlined[NodeSeq])

      (Text("   "): NodeSeq) should equal (Text("   ")) (after being streamlined[NodeSeq])

      (<div>{Text("My name is ")}{Text("Harry")}</div>: NodeSeq) should equal (<div>My name is Harry</div>) (after being streamlined[NodeSeq])
    }
  }
}

