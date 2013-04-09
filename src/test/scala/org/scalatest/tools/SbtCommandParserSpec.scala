
package org.scalatest.tools

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class SbtCommandParserSpec extends FunSpec with ShouldMatchers {

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

