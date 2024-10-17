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
package tools

import StringReporter._
import Fragment.{countTrailingEOLs, countLeadingEOLs}
import SharedHelpers.thisLineNumber
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StringReporterSuite extends AnyFunSuite with Matchers {

  test("Empty string should just come back as an empty string.") {
    assert(Fragment("", AnsiCyan).toPossiblyColoredText(true) === "")
  }

  test("Blank string should just come back as the same string.") {
    assert(Fragment(" ", AnsiCyan).toPossiblyColoredText(true) === " ")
    assert(Fragment("  ", AnsiCyan).toPossiblyColoredText(true) === "  ")
    assert(Fragment("   ", AnsiCyan).toPossiblyColoredText(true) === "   ")
  }

  val cyanRegex = "\u001b\\[36m"
  val resetRegex = "\u001b\\[0m"

  // Have to pad the strings to get the count of appearances to be one less than 
  // the array resulting from splitting on it, because split doesn't give me one
  // if it is on the end
  def pad(s: String) = " " + s + " "

  def countCyan(s: String) = pad(s).split(cyanRegex).length - 1
  def countReset(s: String) = pad(s).split(resetRegex).length - 1

  test("A non-blank string with no EOLs should come back surrounded by ansiColor and ansiReset, " +
      "with no other occurrences of them in the returned string") {

    val aResult = Fragment("a", AnsiCyan).toPossiblyColoredText(true)
    aResult should startWith (ansiCyan)
    aResult should endWith (ansiReset)
    countCyan(aResult) should equal (1)
    countReset(aResult) should equal (1)
  }

  test("A non-blank string with one EOL should come back surrounded by ansiColor and ansiReset, " +
      "with two occurrences of them in the returned string") {

    val aResult = Fragment("a\nb", AnsiCyan).toPossiblyColoredText(true)
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

    val aResult = Fragment("a\nb\n", AnsiCyan).toPossiblyColoredText(true)
    aResult should startWith (ansiCyan)
    aResult should not endWith (ansiReset)
    aResult should endWith (ansiReset + "\n")
    countCyan(aResult) should equal (2)
    countReset(aResult) should equal (2)
  }

  test("A non-blank string with one EOL in the middle and two EOLs at the end should come back with the first two " +
      "strings surrounded by ansiColor and ansiReset, but nothing after the trailing EOLs") {

    val aResult = Fragment("a\nb\n\n", AnsiCyan).toPossiblyColoredText(true)
    aResult should startWith (ansiCyan)
    aResult should not endWith (ansiReset)
    aResult should endWith (ansiReset + "\n\n")
    countCyan(aResult) should equal (2)
    countReset(aResult) should equal (2)
  }

  test("A non-blank string with one EOL in the middle and one EOL at the beginning should come back with the last two " +
      "strings surrounded by ansiColor and ansiReset, but nothing before the initial EOL") {

    val aResult = Fragment("\na\nb", AnsiCyan).toPossiblyColoredText(true)
    aResult should not startWith (ansiCyan)
    aResult should endWith (ansiReset)
    aResult should startWith ("\n" + ansiCyan)
    withClue("\"" + aResult.toList.mkString(" ").replaceAll("\n", "EOL") + "\"") {
      countCyan(aResult) should equal (2)
      countReset(aResult) should equal (2)
    }
  }

  test("stringsToPrintOnError should include the message in unformatted mode") {
    
    val msg = "A stitch in time saves nine."
    val strings: Vector[String] =
      StringReporter.stringsToPrintOnError(
        noteMessageFun = Resources.infoProvidedNote,
        errorMessageFun = Resources.infoProvided,
        message = msg,
        throwable = None,
        analysis = Vector.empty,
        formatter = None,
        suiteName = None,
        testName = None,
        duration = None,
        presentUnformatted = true,
        presentAllDurations = false,
        presentShortStackTraces = false,
        presentFullStackTraces = false,
        presentFilePathname = false
      )
    strings should have size 1
    strings(0) should include (msg)
  }

  test("makeDurationString when duration < 1000") {
    assert(makeDurationString(23) === "23 milliseconds")
  }

  test("makeDurationString when duration == 1") {
    assert(makeDurationString(1) === "1 millisecond")
  }

  test("makeDurationString when duration == 1000") {
    assert(makeDurationString(1000) === "1 second")
  }

  test("makeDurationString when duration == 1001") {
    assert(makeDurationString(1001) === "1 second, 1 millisecond")
  }

  test("makeDurationString when duration == 1049") {
    assert(makeDurationString(1049) === "1 second, 49 milliseconds")
  }

  test("makeDurationString when duration == 2000") {
    assert(makeDurationString(2000) === "2 seconds")
  }

  test("makeDurationString when duration == 10000") {
    assert(makeDurationString(10000) === "10 seconds")
  }

  test("makeDurationString when duration == 3049") {
    assert(makeDurationString(3049) === "3 seconds, 49 milliseconds")
  }

  test("makeDurationString when duration == 60000") {
    assert(makeDurationString(60000) === "1 minute")
  }

  test("makeDurationString when duration == 60001") {
    assert(makeDurationString(60000) === "1 minute")
  }

  test("makeDurationString when duration == 60999") {
    assert(makeDurationString(60000) === "1 minute")
  }

  test("makeDurationString when duration == 61000") {
    assert(makeDurationString(61000) === "1 minute, 1 second")
  }

  test("makeDurationString when duration == 61999") {
    assert(makeDurationString(61000) === "1 minute, 1 second")
  }

  test("makeDurationString when duration == 62000") {
    assert(makeDurationString(62000) === "1 minute, 2 seconds")
  }

  test("makeDurationString when duration == 65388") {
    assert(makeDurationString(65388) === "1 minute, 5 seconds")
  }

  test("makeDurationString when duration == 120000") {
    assert(makeDurationString(120000) === "2 minutes")
  }

  test("makeDurationString when duration == 120999") {
    assert(makeDurationString(120999) === "2 minutes")
  }

  test("makeDurationString when duration == 121000") {
    assert(makeDurationString(121000) === "2 minutes, 1 second")
  }

  test("makeDurationString when duration == 241999") {
    assert(makeDurationString(241999) === "4 minutes, 1 second")
  }

  test("makeDurationString when duration == 122000") {
    assert(makeDurationString(122000) === "2 minutes, 2 seconds")
  }

  test("makeDurationString when duration == 299999") {
    assert(makeDurationString(299999) === "4 minutes, 59 seconds")
  }

  test("makeDurationString when duration == 3600000") {
    assert(makeDurationString(3600000) === "1 hour")
  }

  test("makeDurationString when duration == 3600999") {
    assert(makeDurationString(3600999) === "1 hour")
  }

  test("makeDurationString when duration == 3601000") {
    assert(makeDurationString(3601000) === "1 hour, 1 second")
  }

  test("makeDurationString when duration == 3601999") {
    assert(makeDurationString(3601999) === "1 hour, 1 second")
  }

  test("makeDurationString when duration == 3602000") {
    assert(makeDurationString(3602000) === "1 hour, 2 seconds")
  }

  test("makeDurationString when duration == 3659999") {
    assert(makeDurationString(3659999) === "1 hour, 59 seconds")
  }

  test("makeDurationString when duration == 3660000") {
    assert(makeDurationString(3660000) === "1 hour, 1 minute")
  }

  test("makeDurationString when duration == 3660999") {
    assert(makeDurationString(3660999) === "1 hour, 1 minute")
  }

  test("makeDurationString when duration == 3661000") {
    assert(makeDurationString(3661000) === "1 hour, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 3661999") {
    assert(makeDurationString(3661999) === "1 hour, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 3662000") {
    assert(makeDurationString(3662000) === "1 hour, 1 minute, 2 seconds")
  }

  test("makeDurationString when duration == 3719999") {
    assert(makeDurationString(3719999) === "1 hour, 1 minute, 59 seconds")
  }

  test("makeDurationString when duration == 3720000") {
    assert(makeDurationString(3720000) === "1 hour, 2 minutes")
  }

  test("makeDurationString when duration == 7140999") {
    assert(makeDurationString(7140999) === "1 hour, 59 minutes")
  }

  test("makeDurationString when duration == 3721000") {
    assert(makeDurationString(3721000) === "1 hour, 2 minutes, 1 second")
  }

  test("makeDurationString when duration == 7141999") {
    assert(makeDurationString(7141999) === "1 hour, 59 minutes, 1 second")
  }

  test("makeDurationString when duration == 3722500") {
    assert(makeDurationString(3722500) === "1 hour, 2 minutes, 2 seconds")
  }

  test("makeDurationString when duration == 7199999") {
    assert(makeDurationString(7199999) === "1 hour, 59 minutes, 59 seconds")
  }

  test("makeDurationString when duration == 7200000") {
    assert(makeDurationString(7200000) === "2 hours")
  }

  test("makeDurationString when duration == 360000000") {
    assert(makeDurationString(360000000) === "100 hours")
  }

  test("makeDurationString when duration == 7201000") {
    assert(makeDurationString(7201000) === "2 hours, 1 second")
  }

  test("makeDurationString when duration == 7201999") {
    assert(makeDurationString(7201999) === "2 hours, 1 second")
  }

  test("makeDurationString when duration == 7202000") {
    assert(makeDurationString(7202000) === "2 hours, 2 seconds")
  }

  test("makeDurationString when duration == 7259999") {
    assert(makeDurationString(7259999) === "2 hours, 59 seconds")
  }

  test("makeDurationString when duration == 7260000") {
    assert(makeDurationString(7260000) === "2 hours, 1 minute")
  }

  test("makeDurationString when duration == 7260999") {
    assert(makeDurationString(7260999) === "2 hours, 1 minute")
  }

  test("makeDurationString when duration == 7261000") {
    assert(makeDurationString(7261000) === "2 hours, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 7261999") {
    assert(makeDurationString(7261999) === "2 hours, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 7262000") {
    assert(makeDurationString(7262000) === "2 hours, 1 minute, 2 seconds")
  }

  test("makeDurationString when duration == 7319999") {
    assert(makeDurationString(7319999) === "2 hours, 1 minute, 59 seconds")
  }

  test("makeDurationString when duration == 7320000") {
    assert(makeDurationString(7320000) === "2 hours, 2 minutes")
  }

  test("makeDurationString when duration == 10740999") {
    assert(makeDurationString(10740999) === "2 hours, 59 minutes")
  }

  test("makeDurationString when duration == 7321000") {
    assert(makeDurationString(7321000) === "2 hours, 2 minutes, 1 second")
  }

  test("makeDurationString when duration == 10741999") {
    assert(makeDurationString(10741999) === "2 hours, 59 minutes, 1 second")
  }

  test("makeDurationString when duration == 7322500") {
    assert(makeDurationString(7322500) === "2 hours, 2 minutes, 2 seconds")
  }

  test("makeDurationString when duration == 10799999") {
    assert(makeDurationString(10799999) === "2 hours, 59 minutes, 59 seconds")
  }

  test("withPossibleLineNumber returns simple file name on same line if presentFilePathname is false") {
    import org.scalactic.source
import StringReporter.withPossibleLineNumber
    val result = withPossibleLineNumber("oops", Some(new TestFailedException((_: StackDepthException) => Some("also oops"), None, Left(source.Position.here), None, Vector.empty)), false)
    assert(result === "oops (StringReporterSuite.scala:" + (thisLineNumber - 1) + ")")
  }

  test("withPossibleLineNumber returns full file pathname on next line if presentFilePathname is true and it is available") {
    import StringReporter.withPossibleLineNumber
    import org.scalactic.source
    val result = withPossibleLineNumber("oops", Some(new TestFailedException((_: StackDepthException) => Some("also oops"), None, Left(source.Position.here), None, Vector.empty)), true)
    assert(result startsWith "oops\n** ")
    if (System.getenv("SCALACTIC_FILL_FILE_PATHNAMES") != null && System.getenv("SCALACTIC_FILL_FILE_PATHNAMES") == "yes")
      assert(result endsWith "org/scalatest/tools/StringReporterSuite.scala:" + (thisLineNumber - 3) + " **")
    else
      assert(result endsWith "Please set the environment variable SCALACTIC_FILL_FILE_PATHNAMES to yes at compile time to enable this feature.:" + (thisLineNumber - 5) + " **")
  }

  class BuilderStringReporter(presentInColor: Boolean) extends StringReporter(
    false,
    presentInColor,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false
  ) {
    val builder = new StringBuilder

    protected def printPossiblyInColor(fragment: Fragment): Unit = {
      builder.append(fragment.toPossiblyColoredText(presentInColor) + System.lineSeparator)
    }

    protected def printNoColor(text: String): Unit = {
      builder.append(text)
    }

    def dispose(): Unit = {

    }

    def content: String = builder.toString

  }

  case class Person(name: String, age: Int)

  test("StringReporter should include difference analysis in the content it display") {
    class ExampleSpec extends AnyFunSuite with Matchers {
      test("test") {
        Person("Student 1", 22) shouldEqual Person("Student 2", 23)
      }
    }
    val rep = new BuilderStringReporter(false)
    val suite = new ExampleSpec
    suite.run(None, Args(rep))
    val failingLineNumber = thisLineNumber - 6
    assert(rep.content ==
      s"""- test *** FAILED ***
        |  Person("Student 1", 22) did not equal Person("Student 2", 23) (StringReporterSuite.scala:$failingLineNumber)
        |  Analysis:
        |  StringReporterSuite$$Person(age: 22 -> 23, name: "Student [1]" -> "Student [2]")
        |""".stripMargin
    )
  }
}

