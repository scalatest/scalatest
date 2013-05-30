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

import org.scalatest.UnitSpec
import org.scalatest.events.Summary
import PrintReporter.makeDurationString
import StringReporter.summaryFragments
import org.scalatest.Resources

class StringReporterSummarySpec extends UnitSpec {
  object `the summaryFragments method` {
    def `should produce a good summary when Summary is all zeroes` {
      val summary =
        new Summary(
          testsSucceededCount = 0,
          testsFailedCount = 0,
          testsIgnoredCount = 0,
          testsPendingCount = 0,
          testsCanceledCount = 0,
          suitesCompletedCount = 0,
          suitesAbortedCount = 0,
          scopesPendingCount = 0
        )

      val fragments = summaryFragments(true, Some(0L), Some(summary))
      fragments should be (
        Vector(
          Fragment(Resources("runCompletedIn", makeDurationString(0)), AnsiCyan),
          Fragment(Resources("totalNumberOfTestsRun", summary.testsCompletedCount.toString), AnsiCyan),
          Fragment(Resources("suiteSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString), AnsiCyan),
          Fragment(Resources("testSummary", summary.testsSucceededCount.toString, summary.testsFailedCount.toString, summary.testsCanceledCount.toString, summary.testsIgnoredCount.toString, summary.testsPendingCount.toString), AnsiCyan),
          Fragment(Resources("allTestsPassed"), AnsiGreen)
        )
      )
    }
    def `should produce a good summary when Summary one test failed` {
/*
[scalatest] Run completed in 213 milliseconds.
[scalatest] Total number of tests run: 1
[scalatest] Suites: completed 1, aborted 0
[scalatest] Tests: succeeded 0, failed 1, canceled 0, ignored 0, pending 0
[scalatest] *** 1 TEST FAILED ***
*/

      val summary =
        new Summary(
          testsSucceededCount = 0,
          testsFailedCount = 1,
          testsIgnoredCount = 0,
          testsPendingCount = 0,
          testsCanceledCount = 0,
          suitesCompletedCount = 1,
          suitesAbortedCount = 0,
          scopesPendingCount = 0
        )

      val fragments = summaryFragments(true, Some(213L), Some(summary))
      fragments should be (
        Vector(
          Fragment(Resources("runCompletedIn", makeDurationString(213L)), AnsiCyan),
          Fragment(Resources("totalNumberOfTestsRun", summary.testsCompletedCount.toString), AnsiCyan),
          Fragment(Resources("suiteSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString), AnsiCyan),
          Fragment(Resources("testSummary", summary.testsSucceededCount.toString, summary.testsFailedCount.toString, summary.testsCanceledCount.toString, summary.testsIgnoredCount.toString, summary.testsPendingCount.toString), AnsiCyan),
          Fragment(Resources("oneTestFailed"), AnsiRed)
        )
      )
    }
    def `should fail on purpose` {
      fail("I meant to do that!")
    }
  }
}

