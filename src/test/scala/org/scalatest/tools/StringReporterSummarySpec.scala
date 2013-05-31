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
import org.scalatest.events.TestFailed
import org.scalatest.exceptions.TestFailedException
import org.scalatest.events.ExceptionalEvent
import org.scalatest.events.Ordinal
import org.scalatest.events.IndentedText
import org.scalatest.events.SeeStackDepthException

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

      val fragments = summaryFragments(true, Some(0L), Some(summary), Vector.empty)
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

    object `when one test failed` {
      def `should produce a good summary when reminders are disabled` {
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

        val fragments = summaryFragments(true, Some(213L), Some(summary), Vector.empty)
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

      def `should produce a good summary when reminders are enabled without stack traces` {
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

        val exceptionalEvents: Vector[ExceptionalEvent] = 
          Vector(
/*
TestFailed(Ordinal(1, 6),I meant to do that!,StringReporterSummarySpec,org.scalatest.tools.StringReporterSummarySpec,Some(org.scalatest.tools.StringReporterSummarySpec),the summaryFragments method should fail on purpose,should fail on purpose,Vector(),Some(org.scalatest.exceptions.TestFailedException: I meant to do that!),Some(16),Some(IndentedText(- should fail on purpose,should fail on purpose,1)),Some(SeeStackDepthException),Some(org.scalatest.tools.StringReporterSummarySpec),None,Thread-10,1369965123278
*/
            TestFailed(
              ordinal = new Ordinal(123),
              message = "I meant to do that!",
              suiteName = "StringReporterSummarySpec",
              suiteId = "org.scalatest.tools.StringReporterSummarySpec",
              suiteClassName = Some("org.scalatest.tools.StringReporterSummarySpec"),
              testName = "the summaryFragments method should fail on purpose",
              testText = "should fail on purpose",
              recordedEvents = Vector.empty,
              throwable =
                Some(
                  try { assert(false, "I meant to do that!"); new Exception("Not reached") }
                  catch { case e: TestFailedException => e }
                ),
              duration = Some(16),
              formatter = Some(IndentedText("THIS SHOULD NOT BE USED", "SHOULD NOT BE USED", 1)),
              location = Some(SeeStackDepthException),
              rerunner = Some("org.scalatest.tools.StringReporterSummarySpec"),
              payload = None,
              threadName = "Thread-10",
              timeStamp = 1369965123278L
            )
          )
        val fragments = summaryFragments(true, Some(213L), Some(summary), exceptionalEvents)
        fragments should be (
          Vector(
            Fragment(Resources("runCompletedIn", makeDurationString(213L)), AnsiCyan),
            Fragment(Resources("totalNumberOfTestsRun", summary.testsCompletedCount.toString), AnsiCyan),
            Fragment(Resources("suiteSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString), AnsiCyan),
            Fragment(Resources("testSummary", summary.testsSucceededCount.toString, summary.testsFailedCount.toString, summary.testsCanceledCount.toString, summary.testsIgnoredCount.toString, summary.testsPendingCount.toString), AnsiCyan),
            Fragment(Resources("oneTestFailed"), AnsiRed),
            Fragment("StringReporterSummarySpec:", AnsiRed),
            Fragment("the summaryFragments method ", AnsiRed),
            Fragment("- should fail on purpose", AnsiRed)
          )
        )
      }
    }

    /* @org.scalatest.Ignore */
    @org.scalatest.Ignore def `should fail on purpose` {
      fail("I meant to do that!")
    }
  }
}

