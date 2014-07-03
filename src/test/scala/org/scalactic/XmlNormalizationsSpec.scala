/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalactic.NormMethods._
import scala.xml.{Node, Text}

class XmlNormalizationsSpec extends Spec with Matchers {

  import XmlNormalizations._

  object `Xml Normalization of Elems` {

    def `should leave already-normalized XML alone` {
      <summer></summer>.norm == <summer></summer> shouldBe true
    }

    def `should zap text that is only whitespace` {
      <summer> </summer>.norm == <summer></summer> shouldBe true
      <summer>
       </summer>.norm == <summer></summer> shouldBe true
      <summer>
        <day></day>
      </summer>.norm == <summer><day></day></summer> shouldBe true
      <summer><day></day></summer> ==
        <summer>
          <day></day>
        </summer>.norm shouldBe true
      <summer><day>Dude!</day></summer> ==
        <summer>
          <day>
            Dude!
          </day>
        </summer>.norm shouldBe true
      <div>{Text("My name is ")}{Text("Harry")}</div>.norm shouldBe <div>My name is Harry</div>
    }
  }

  object `Xml Normalization of Nodes` {

    def `should leave already-normalized XML alone` {
      (<summer></summer>: Node).norm == <summer></summer> shouldBe true
      (Text("Bla"): Node).norm shouldBe Text("Bla")
    }

    def `should zap text that is only whitespace, unless it is already a Text` {
      <summer> </summer>.norm == <summer></summer> shouldBe true
      <summer>
      </summer>.norm == <summer></summer> shouldBe true
      <summer>
        <day></day>
      </summer>.norm == <summer><day></day></summer> shouldBe true
      <summer><day></day></summer> ==
        <summer>
          <day></day>
        </summer>.norm shouldBe true
      <summer><day>Dude!</day></summer> ==
        <summer>
          <day>
            Dude!
          </day>
        </summer>.norm shouldBe true
      (Text("   "): Node).norm shouldBe Text("   ")
      (<div>{Text("My name is ")}{Text("Harry")}</div>: Node).norm shouldBe <div>My name is Harry</div>
    }
  }
}
