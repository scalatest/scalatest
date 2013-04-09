/*
 * Copyright 2001-2011 Artima, Inc.
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
package tools

import org.scalatest.matchers.ShouldMatchers
import StringReporter.{colorizeLinesIndividually => colorize}
import StringReporter.{countTrailingEOLs, countLeadingEOLs}
import PrintReporter.ansiReset
import PrintReporter.ansiCyan

class StringReporterSuite extends FunSuite with ShouldMatchers {

  test("Empty string should just come back as an empty string.") {
    assert(colorize("", ansiCyan) === "")
  }

  test("Blank string should just come back as the same string.") {
    assert(colorize(" ", ansiCyan) === " ")
    assert(colorize("  ", ansiCyan) === "  ")
    assert(colorize("   ", ansiCyan) === "   ")
  }

  val cyanRegex = "\033\\[36m"
  val resetRegex = "\033\\[0m"

  // Have to pad the strings to get the count of appearances to be one less than 
  // the array resulting from splitting on it, because split doesn't give me one
  // if it is on the end
  def pad(s: String) = " " + s + " "

  def countCyan(s: String) = pad(s).split(cyanRegex).length - 1
  def countReset(s: String) = pad(s).split(resetRegex).length - 1

  test("A non-blank string with no EOLs should come back surrounded by ansiColor and ansiReset, " +
      "with no other occurrences of them in the returned string") {

    val aResult = colorize("a", ansiCyan)
    aResult should startWith (ansiCyan)
    aResult should endWith (ansiReset)
    countCyan(aResult) should equal (1)
    countReset(aResult) should equal (1)
  }

  test("A non-blank string with one EOL should come back surrounded by ansiColor and ansiReset, " +
      "with two occurrences of them in the returned string") {

    val aResult = colorize("a\nb", ansiCyan)
    aResult should startWith (ansiCyan)
    aResult should endWith (ansiReset)
    countCyan(aResult) should equal (2)
    countReset(aResult) should equal (2)
  }

  test("countTrailingEOLs should return the number of EOLs at the end of the passed string") {
    countTrailingEOLs("") should be (0)
    countTrailingEOLs("\n") should be (1)
    countTrailingEOLs("\n\n") should be (2)
    countTrailingEOLs("howdy\n\n") should be (2)
    countTrailingEOLs("\nhowdy\n\n") should be (2)
    countTrailingEOLs("hohoho\nhowdy\n\n") should be (2)
    countTrailingEOLs("\n\nhohoho\nhowdy\n\n") should be (2)
    countTrailingEOLs("\n\nhohoho\nhowdy\n") should be (1)
    countTrailingEOLs("\n\nhohoho\nhowdy") should be (0)
    countTrailingEOLs("not a single one ") should be (0)
    countTrailingEOLs("not a single one") should be (0)
  }

  test("countLeadingEOLs should return the number of EOLs at the beginning of the passed string") {
    countLeadingEOLs("") should be (0)
    countLeadingEOLs("\n") should be (0) // Allow these to be counted as at the end
    countLeadingEOLs("\n\n") should be (0)
    countLeadingEOLs("\n\nhowdy") should be (2)
    countLeadingEOLs("\n\nhowdy\n") should be (2)
    countLeadingEOLs("\n\nhohoho\nhowdy") should be (2)
    countLeadingEOLs("\n\nhohoho\nhowdy\n\n") should be (2)
    countLeadingEOLs("\nhohoho\nhowdy\n\n") should be (1)
    countLeadingEOLs("hohoho\nhowdy\n\n") should be (0)
    countLeadingEOLs("not a single one ") should be (0)
    countLeadingEOLs("not a single one") should be (0)
  }

  test("A non-blank string with one EOL in the middle and one EOL at the end should come back with the first two " +
      "strings surrounded by ansiColor and ansiReset, but nothing after the trailing EOL") {

    val aResult = colorize("a\nb\n", ansiCyan)
    aResult should startWith (ansiCyan)
    aResult should not endWith (ansiReset)
    aResult should endWith (ansiReset + "\n")
    countCyan(aResult) should equal (2)
    countReset(aResult) should equal (2)
  }

  test("A non-blank string with one EOL in the middle and two EOLs at the end should come back with the first two " +
      "strings surrounded by ansiColor and ansiReset, but nothing after the trailing EOLs") {

    val aResult = colorize("a\nb\n\n", ansiCyan)
    aResult should startWith (ansiCyan)
    aResult should not endWith (ansiReset)
    aResult should endWith (ansiReset + "\n\n")
    countCyan(aResult) should equal (2)
    countReset(aResult) should equal (2)
  }

  test("A non-blank string with one EOL in the middle and one EOL at the beginning should come back with the last two " +
      "strings surrounded by ansiColor and ansiReset, but nothing before the initial EOL") {

    val aResult = colorize("\na\nb", ansiCyan)
    aResult should not startWith (ansiCyan)
    aResult should endWith (ansiReset)
    aResult should startWith ("\n" + ansiCyan)
    withClue("\"" + aResult.toList.mkString(" ").replaceAll("\n", "EOL") + "\"") {
      countCyan(aResult) should equal (2)
      countReset(aResult) should equal (2)
    }
  }
}

