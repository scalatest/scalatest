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

import org.scalatest.DocSpec.trimMarkup
import prop.TableDrivenPropertyChecks._
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class DocSpecSpec extends AnyFunSpec {
  describe("A DocSpec") {
    describe("with no suites inside") {

      // This one I'm putting flat against the margin on purpose.
      val flatAgainstMargin =
        new DocSpec {
          val doc = markup"""
This is a Title
===============

This is a paragraph later...
"""
        }
      // TODO: Blank line first, line with no chars, line with some white chars, and no lines, and only white lines
      // TODO: test with different end of line characters
      // This one is indented eight characters
      val indented8 =
        new DocSpec {
          val doc = markup"""
            This is a Title
            ===============

            This is a paragraph later...
          """
        }

      val emptyLineInMiddle =
        new DocSpec {
          val doc = markup"""
            This is a Title
            ===============

            This is a paragraph later...
          """
        }

      val shortLineInMiddle =
        new DocSpec {
          val doc = markup"""
            This is a Title
            ===============

            This is a paragraph later...
          """
        }

      val blankLineFirst =
        new DocSpec {
          val doc = markup"""

            This is a Title
            ===============

            This is a paragraph later...
          """
        }

      // Initial line has no chars
      val emptyLineFirst =
        new DocSpec {
          val doc = markup"""

            This is a Title
            ===============

            This is a paragraph later...
          """
        }

      // Initial line has 3 space chars
      val shortLineFirst =
        new DocSpec {
          val doc = markup"""

            This is a Title
            ===============

            This is a paragraph later...
          """
        }

      val examples = Table(
        "doc spec",
        flatAgainstMargin,
        indented8,
        emptyLineInMiddle,
        shortLineInMiddle,
        blankLineFirst,
        emptyLineFirst,
        shortLineFirst
      )

      it("should send the markup unindented out the door") {
        forAll (examples) { docSpec =>
          val rep = new EventRecordingReporter
          docSpec.run(None, Args(rep))
          val mp = rep.markupProvidedEventsReceived
          assert(mp.size === 1)
          val event = mp(0)
          val expected =
            trimMarkup("""
              |This is a Title
              |===============
              |
              |This is a paragraph later...
              |""".stripMargin
            )
          assert(event.text === expected)
        }
      }
      it("should return an empty list from nestedSuites") {
        forAll (examples) { doc =>
          doc.nestedSuites should equal (Nil)
        }
      }
    }
    describe("with just suites") {
      class NestedSpec extends AnyFunSpec {
        var wasRun = false
        it("a test") {
          wasRun = true
        }
      }

      it("should run the nested suite as well as outputing the markup text") {

        val nestedSpec = new NestedSpec

        val docSpec =
          new DocSpec {
            val doc = markup"""

              This is a Title
              ===============

              This is a nested suite:${
                nestedSpec
              }

              And here is some text after.

          """
        }

        val rep = new EventRecordingReporter
        docSpec.run(None, Args(rep))
        val mp = rep.markupProvidedEventsReceived
        assert(mp.size === 2)
        val expectedTop =
          trimMarkup("""
            |This is a Title
            |===============
            |
            |This is a nested suite:
          """.stripMargin)
        assert(mp(0).text === expectedTop)

        val expectedBottom = "And here is some text after."
        assert(mp(1).text === expectedBottom)

        assert(nestedSpec.wasRun, "nested spec was not run")
      }
    }
  }

  describe("The trimMarkup method") {
    it("should strip any blank lines off of the front") {
     trimMarkup("\n\n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("\n  \n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("  \n  \t \n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("\n\n\n\n  \n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff") should equal ("  First line with stuff")
    }
    it("should strip any blank lines off the front and have no blank line or return at the end") {
      trimMarkup("\n\n  First line with stuff\n") should equal ("  First line with stuff")
      trimMarkup("\n  \n  First line with stuff\n\n") should equal ("  First line with stuff")
      trimMarkup("  \n  \t \n  First line with stuff\n  \n") should equal ("  First line with stuff")
      trimMarkup("\n\n\n\n  \n  First line with stuff\n \t\t   \n   \n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n\n\n") should equal ("  First line with stuff")
    }
    it("should have no blank line or return at the end") {
      trimMarkup("  First line with stuff\n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n\n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n  \n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n \t\t   \n   \n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n\n\n") should equal ("  First line with stuff")
    }
  }

  describe("The stripMargin method") {

    it("should throw NPE if null passed") {
      a [NullPointerException] should be thrownBy { DocSpec.stripMargin(null) }
    }
    it("should return an empty string as is") {
      DocSpec.stripMargin("") should equal ("")
    }
    it("when passed a string with leading space, should return the string with the leading space omitted") {
      DocSpec.stripMargin(" Howdy") should equal ("Howdy")
      DocSpec.stripMargin("  Howdy") should equal ("Howdy")
      DocSpec.stripMargin("   Howdy") should equal ("Howdy")
      DocSpec.stripMargin("\tHowdy") should equal ("Howdy")
      DocSpec.stripMargin("\t\tHowdy") should equal ("Howdy")
      DocSpec.stripMargin(" \t \tHowdy") should equal ("Howdy")
    }
    it("when passed a string with leading space and two lines, should return the string with the leading space omitted from the first line, and the same amound omitted from the second line, with tabs converted to one space") {
      DocSpec.stripMargin(" Howdy\n123456789") should equal ("Howdy\n23456789")
      DocSpec.stripMargin("  Howdy\n123456789") should equal ("Howdy\n3456789")
      DocSpec.stripMargin("   Howdy\n123456789") should equal ("Howdy\n456789")
      DocSpec.stripMargin("\tHowdy\n123456789") should equal ("Howdy\n23456789")
      DocSpec.stripMargin("\t\tHowdy\n123456789") should equal ("Howdy\n3456789")
      DocSpec.stripMargin(" \t \tHowdy\n123456789") should equal ("Howdy\n56789")
    }
    it("when passed a string with one or more blank lines, a line with leading space and two lines, should return the string with the leading space omitted from the first line, and the same amound omitted from the second line, with tabs converted to one space") {
      DocSpec.stripMargin("\n Howdy\n123456789") should equal ("\nHowdy\n23456789")
      DocSpec.stripMargin("\n  \n\n  Howdy\n123456789") should equal ("\n\n\nHowdy\n3456789")
      DocSpec.stripMargin("\n  \t\t\n   Howdy\n123456789") should equal ("\n\t\nHowdy\n456789")
      DocSpec.stripMargin("\n\n\n\n\tHowdy\n123456789") should equal ("\n\n\n\nHowdy\n23456789")
      DocSpec.stripMargin("\n\t\tHowdy\n123456789") should equal ("\nHowdy\n3456789")
      DocSpec.stripMargin("\n      \n \t \tHowdy\n123456789") should equal ("\n  \nHowdy\n56789")
    }
  }
}
