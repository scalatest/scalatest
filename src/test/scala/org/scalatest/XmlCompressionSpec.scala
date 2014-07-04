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

import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import scala.xml.{Elem, Node, Text, NodeSeq}

class XmlCompressionSpec extends Spec with Matchers with XmlCompression {

  object `Xml Equality of Elems (after being compressed)` {

    def `should leave already-normalized XML alone` {
      <summer></summer> should equal (<summer></summer>) (after being compressed[Elem])
    }

    def `should zap text that is only whitespace` {

      <summer> </summer> should equal (<summer></summer>) (after being compressed[Elem])

      <summer>
       </summer> should equal (<summer></summer>) (after being compressed[Elem])

      <summer>
        <day></day>
      </summer> should equal (<summer><day></day></summer>) (after being compressed[Elem])

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>
      ) (after being compressed[Elem])

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>
      ) (after being compressed[Elem])

      <div>{Text("My name is ")}{Text("Harry")}</div> should equal (<div>My name is Harry</div>) (after being compressed[Elem])
    }
  }

  object `Xml Equality of Nodes (after being compressed)` {

    def `should leave already-normalized XML alone` {

      ((<summer></summer>: Node) shouldEqual (<summer></summer>)) (after being compressed[Node])

      ((Text("Bla"): Node) shouldEqual (Text("Bla"))) (after being compressed[Node])
    }

    def `should zap text that is only whitespace, unless it is already a Text` {

      (<summer> </summer>: Node) should equal (<summer></summer>) (after being compressed[Node])

      (<summer>
      </summer>: Node) should equal (<summer></summer>) (after being compressed[Node])

      (<summer>
        <day></day>
      </summer>: Node) should equal (<summer><day></day></summer>) (after being compressed[Node])

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>: Node
      ) (after being compressed[Node])

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>: Node
      ) (after being compressed[Node])

      (Text("   "): Node) should equal (Text("   ")) (after being compressed[Node])

      (<div>{Text("My name is ")}{Text("Harry")}</div>: Node) should equal (<div>My name is Harry</div>) (after being compressed[Node])
    }
  }

  object `Xml Equality of NodeSeq (after being compressed)` {

    def `should leave already-normalized XML alone` {

      (<summer></summer>: NodeSeq) should equal (<summer></summer>) (after being compressed[NodeSeq])

      (Text("Bla"): NodeSeq) should equal (Text("Bla")) (after being compressed[NodeSeq])
    }

    def `should zap text that is only whitespace, unless it is already a Text` {

      (<summer> </summer>: NodeSeq) should equal (<summer></summer>) (after being compressed[NodeSeq])

      (<summer>
      </summer>: NodeSeq) should equal (<summer></summer>) (after being compressed[NodeSeq])

      (<summer>
        <day></day>
      </summer>: NodeSeq) should equal (<summer><day></day></summer>) (after being compressed[NodeSeq])

      <summer><day></day></summer> should equal (
        <summer>
          <day></day>
        </summer>: NodeSeq
      ) (after being compressed[NodeSeq])

      <summer><day>Dude!</day></summer> should equal (
        <summer>
          <day>
            Dude!
          </day>
        </summer>: NodeSeq
      ) (after being compressed[NodeSeq])

      (Text("   "): NodeSeq) should equal (Text("   ")) (after being compressed[NodeSeq])

      (<div>{Text("My name is ")}{Text("Harry")}</div>: NodeSeq) should equal (<div>My name is Harry</div>) (after being compressed[NodeSeq])
    }
  }
}

