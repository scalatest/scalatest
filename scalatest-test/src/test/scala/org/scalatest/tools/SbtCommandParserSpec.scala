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

package org.scalatest.tools

import org.scalatest.FunSpec
import org.scalatest.Matchers

class SbtCommandParserSpec extends FunSpec with Matchers {

  val parser = new SbtCommandParser

  def canParsePhrase(s: String) {
      val result = parser.parseResult(s)
      result match {
        case ns: parser.NoSuccess => fail(ns.toString)
        case _ => 
      }
  }

  def cannotParsePhrase(s: String) {
      val result = parser.parseResult(s)
      result match {
        case parser.Success(result, _) => fail("wasn't supposed to, but parsed: " + result)
        case _ =>
      }
  }

  describe("the cmd terminal?") {
    it("should parse 'st'") {
      canParsePhrase("""st""")
      canParsePhrase("""st --""")
    }
  }
}

