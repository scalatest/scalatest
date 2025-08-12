/*
 * Copyright 2001-2025 Artima, Inc.
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
import StringReporter.makeDurationString
import StringReporter.summaryFragments
import org.scalatest.Resources
import org.scalatest.events.TestFailed
import org.scalatest.events.TestCanceled
import org.scalatest.events.SuiteAborted
import org.scalatest.events.InfoProvided
import org.scalatest.events.NameInfo
import org.scalatest.events.LineInFile
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.events.ExceptionalEvent
import org.scalatest.events.Ordinal
import org.scalatest.events.IndentedText
import org.scalatest.events.SeeStackDepthException
import org.scalatest.SharedHelpers._

class StringReporterSummarySpec extends UnitSpec {

  val summaryWithOneFailedTest =
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

  val summaryWithOneFailedAndOneCanceledTest =
    new Summary(
      testsSucceededCount = 0,
      testsFailedCount = 1,
      testsIgnoredCount = 0,
      testsPendingCount = 0,
      testsCanceledCount = 1,
      suitesCompletedCount = 1,
      suitesAbortedCount = 0,
      scopesPendingCount = 0
    )

  val testFailedException: Throwable =
    try { assert(false, "I meant to do that!"); new Exception("Not reached") }
    catch { case e: TestFailedException => e }
  val tfeLineNumber = thisLineNumber - 2

  val testCanceledException: Throwable =
    try { assume(false, "I meant to do that!"); new Exception("Not reached") }
    catch { case e: TestCanceledException => e }
  val tceLineNumber = thisLineNumber - 2

  val firstOrdinal = new Ordinal(123)
  val secondOrdinal = firstOrdinal.next
  val thirdOrdinal = secondOrdinal.next

  val justOneTestFailed: Vector[ExceptionalEvent] = 
    Vector(
      TestFailed(
        ordinal = firstOrdinal,
        message = "I meant to do that!",
        suiteName = "StringReporterSummarySpec",
        suiteId = "org.scalatest.tools.StringReporterSummarySpec",
        suiteClassName = Some("org.scalatest.tools.StringReporterSummarySpec"),
        testName = "the summaryFragments method should fail on purpose",
        testText = "should fail on purpose",
        recordedEvents = Vector.empty,
        analysis = Vector.empty,
        throwable = Some(testFailedException),
        duration = Some(16),
        formatter = Some(IndentedText("THIS SHOULD NOT BE USED", "SHOULD NOT BE USED", 1)),
        location = Some(SeeStackDepthException),
        rerunner = Some("org.scalatest.tools.StringReporterSummarySpec"),
        payload = None,
        threadName = "Thread-10",
        timeStamp = 1369965123278L
      )
    )

  val oneTestFailedAndOneTestCanceled: Vector[ExceptionalEvent] = 

    Vector(
      justOneTestFailed(0),
      TestCanceled(
        ordinal = thirdOrdinal,
        message = "I meant to do that!",
        suiteName = "StringReporterSummarySpec",
        suiteId = "org.scalatest.tools.StringReporterSummarySpec",
        suiteClassName = Some("org.scalatest.tools.StringReporterSummarySpec"),
        testName = "the summaryFragments method should cancel on purpose",
        testText = "should cancel on purpose",
        recordedEvents =
          Vector(
            InfoProvided(
              secondOrdinal,
              "I should not show up in the reminder",
              Some(
                NameInfo(
                  "StringReporterSummarySpec",
                  "org.scalatest.tools.StringReporterSummarySpec",
                  Some("org.scalatest.tools.StringReporterSummarySpec"),
                  Some("the summaryFragments method should fail on purpose")
                )
              ),
              None,
              Some(IndentedText("  + I should not show up in the reminder", "I should not show up in the reminder", 2)),
              Some(LineInFile(442, "StringReporterSummarySpec.scala", None)),
              None,
              "Thread-10",
              1369965123278L
            )
          ),
        throwable = Some(testCanceledException),
        duration = Some(15),
        formatter = Some(IndentedText("THIS SHOULD NOT BE USED", "SHOULD NOT BE USED", 1)),
        location = Some(SeeStackDepthException),
        // rerunner = Some("org.scalatest.tools.StringReporterSummarySpec"),
        payload = None,
        threadName = "Thread-10",
        timeStamp = 1370103395076L
      )
    )

  val initialFragmentsForJustOneFailedTest: Vector[Fragment] =
    Vector(
      Fragment(Resources.runCompletedIn(makeDurationString(213L)), AnsiCyan),
      Fragment(Resources.totalNumberOfTestsRun(summaryWithOneFailedTest.testsCompletedCount.toString), AnsiCyan),
      Fragment(Resources.suiteSummary(summaryWithOneFailedTest.suitesCompletedCount.toString, summaryWithOneFailedTest.suitesAbortedCount.toString), AnsiCyan),
      Fragment(Resources.testSummary(summaryWithOneFailedTest.testsSucceededCount.toString, summaryWithOneFailedTest.testsFailedCount.toString, summaryWithOneFailedTest.testsCanceledCount.toString, summaryWithOneFailedTest.testsIgnoredCount.toString, summaryWithOneFailedTest.testsPendingCount.toString), AnsiCyan),
      Fragment(Resources.oneTestFailed, AnsiRed),
      Fragment("StringReporterSummarySpec:", AnsiRed),
      Fragment("the summaryFragments method ", AnsiRed),
      Fragment(Resources.specTextAndNote("- should fail on purpose", Resources.failedNote), AnsiRed),
      Fragment("  I meant to do that! (StringReporterSummarySpec.scala:" + tfeLineNumber + ")", AnsiRed)
    )

  val fragmentsWhenNoReminderForOneFailedTest: Vector[Fragment] =
    Vector(
      Fragment(Resources.runCompletedIn(makeDurationString(213L)), AnsiCyan),
      Fragment(Resources.totalNumberOfTestsRun(summaryWithOneFailedTest.testsCompletedCount.toString), AnsiCyan),
      Fragment(Resources.suiteSummary(summaryWithOneFailedTest.suitesCompletedCount.toString, summaryWithOneFailedTest.suitesAbortedCount.toString), AnsiCyan),
      Fragment(Resources.testSummary(summaryWithOneFailedTest.testsSucceededCount.toString, summaryWithOneFailedTest.testsFailedCount.toString, summaryWithOneFailedTest.testsCanceledCount.toString, summaryWithOneFailedTest.testsIgnoredCount.toString, summaryWithOneFailedTest.testsPendingCount.toString), AnsiCyan),
      Fragment(Resources.oneTestFailed, AnsiRed)
    )

  val fragmentsWhenNoReminderForOneFailedAndOneCanceledTest: Vector[Fragment] =
    Vector(
      Fragment(Resources.runCompletedIn(makeDurationString(213L)), AnsiCyan),
      Fragment(Resources.totalNumberOfTestsRun(summaryWithOneFailedAndOneCanceledTest.testsCompletedCount.toString), AnsiCyan),
      Fragment(Resources.suiteSummary(summaryWithOneFailedAndOneCanceledTest.suitesCompletedCount.toString, summaryWithOneFailedAndOneCanceledTest.suitesAbortedCount.toString), AnsiCyan),
      Fragment(Resources.testSummary(summaryWithOneFailedAndOneCanceledTest.testsSucceededCount.toString, summaryWithOneFailedAndOneCanceledTest.testsFailedCount.toString, summaryWithOneFailedAndOneCanceledTest.testsCanceledCount.toString, summaryWithOneFailedAndOneCanceledTest.testsIgnoredCount.toString, summaryWithOneFailedAndOneCanceledTest.testsPendingCount.toString), AnsiCyan),
      Fragment(Resources.oneTestFailed, AnsiRed)
    )

  val initialFragmentsForOneFailedAndOneCanceledTest: Vector[Fragment] =
    Vector(
      Fragment(Resources.runCompletedIn(makeDurationString(213L)), AnsiCyan),
      Fragment(Resources.totalNumberOfTestsRun(summaryWithOneFailedAndOneCanceledTest.testsCompletedCount.toString), AnsiCyan),
      Fragment(Resources.suiteSummary(summaryWithOneFailedAndOneCanceledTest.suitesCompletedCount.toString, summaryWithOneFailedAndOneCanceledTest.suitesAbortedCount.toString), AnsiCyan),
      Fragment(Resources.testSummary(summaryWithOneFailedAndOneCanceledTest.testsSucceededCount.toString, summaryWithOneFailedAndOneCanceledTest.testsFailedCount.toString, summaryWithOneFailedAndOneCanceledTest.testsCanceledCount.toString, summaryWithOneFailedAndOneCanceledTest.testsIgnoredCount.toString, summaryWithOneFailedAndOneCanceledTest.testsPendingCount.toString), AnsiCyan),
      Fragment(Resources.oneTestFailed, AnsiRed),
      Fragment("StringReporterSummarySpec:", AnsiRed),
      Fragment("the summaryFragments method ", AnsiRed),
      Fragment(Resources.specTextAndNote("- should fail on purpose", Resources.failedNote), AnsiRed),
      Fragment("  I meant to do that! (StringReporterSummarySpec.scala:" + tfeLineNumber + ")", AnsiRed)
    )

  val initialFragmentsTheOneCanceledTestReminder: Vector[Fragment] =
    Vector(
      Fragment("StringReporterSummarySpec:", AnsiYellow),
      Fragment("the summaryFragments method ", AnsiYellow),
      Fragment(Resources.specTextAndNote("- should cancel on purpose", Resources.canceledNote), AnsiYellow),
      Fragment("  I meant to do that! (StringReporterSummarySpec.scala:" + tceLineNumber + ")", AnsiYellow)
    )

  describe("the summaryFragments method") {
    it("should produce just a run completed message when the run completes but the summary is not defined") {
      val fragments =
        summaryFragments(
          true,
          Some(0L),
          None,
          Vector.empty,
          presentAllDurations = false,
          presentReminder = true,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          presentFilePathname = false
        )
      fragments should be (Vector(Fragment(Resources.runCompleted, AnsiCyan)))
    }
    it("should produce just a run stopped message when the run stops but the summary is not defined") {
      val fragments =
        summaryFragments(
          false,
          Some(0L),
          None,
          Vector.empty,
          presentAllDurations = false,
          presentReminder = true,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          presentFilePathname = false
        )
      fragments should be (Vector(Fragment(Resources.runStopped, AnsiCyan)))
    }

    it("should produce a good summary when Summary is all zeroes") {
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

      val fragments =
        summaryFragments(
          true,
          Some(0L),
          Some(summary),
          Vector.empty,
          presentAllDurations = false,
          presentReminder = true,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          presentFilePathname = false
        )
      fragments should be (
        Vector(
          Fragment(Resources.runCompletedIn(makeDurationString(0)), AnsiCyan),
          Fragment(Resources.totalNumberOfTestsRun(summary.testsCompletedCount.toString), AnsiCyan),
          Fragment(Resources.suiteSummary(summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString), AnsiCyan),
          Fragment(Resources.testSummary(summary.testsSucceededCount.toString, summary.testsFailedCount.toString, summary.testsCanceledCount.toString, summary.testsIgnoredCount.toString, summary.testsPendingCount.toString), AnsiCyan),
          Fragment(Resources.noTestsWereExecuted, AnsiYellow)
        )
      )
    }

    it("should produce a good summary when Summary is all tests pass") {
      val summaryAllPassed =
        new Summary(
          testsSucceededCount = 5,
          testsFailedCount = 0,
          testsIgnoredCount = 0,
          testsPendingCount = 0,
          testsCanceledCount = 0,
          suitesCompletedCount = 0,
          suitesAbortedCount = 0,
          scopesPendingCount = 0
        )

      val fragments =
        summaryFragments(
          true,
          Some(0L),
          Some(summaryAllPassed),
          Vector.empty,
          presentAllDurations = false,
          presentReminder = true,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          presentFilePathname = false
        )
      fragments should be (
        Vector(
          Fragment(Resources.runCompletedIn(makeDurationString(0)), AnsiCyan),
          Fragment(Resources.totalNumberOfTestsRun(summaryAllPassed.testsCompletedCount.toString), AnsiCyan),
          Fragment(Resources.suiteSummary(summaryAllPassed.suitesCompletedCount.toString, summaryAllPassed.suitesAbortedCount.toString), AnsiCyan),
          Fragment(Resources.testSummary(summaryAllPassed.testsSucceededCount.toString, summaryAllPassed.testsFailedCount.toString, summaryAllPassed.testsCanceledCount.toString, summaryAllPassed.testsIgnoredCount.toString, summaryAllPassed.testsPendingCount.toString), AnsiCyan),
          Fragment(Resources.allTestsPassed, AnsiGreen)
        )
      )
    }
 
    it("should sort the passed exceptionalEvents in ordinal order") {
      val fragments =
        summaryFragments(
          true,
          Some(213L),
          Some(summaryWithOneFailedAndOneCanceledTest),
          Vector(oneTestFailedAndOneTestCanceled(1), oneTestFailedAndOneTestCanceled(0)),
          presentAllDurations = false,
          presentReminder = true,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          presentFilePathname = false
        )

        fragments.take(initialFragmentsForOneFailedAndOneCanceledTest.size) should be (initialFragmentsForOneFailedAndOneCanceledTest)
        fragments.drop(initialFragmentsForOneFailedAndOneCanceledTest.size) should be (initialFragmentsTheOneCanceledTestReminder)
    }

    describe("when one test failed") {
      it("should produce a good summary when reminders are disabled") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedTest),
            justOneTestFailed,
            presentAllDurations = false,
            presentReminder = false,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )
        fragments should be (fragmentsWhenNoReminderForOneFailedTest)
      }

      it("should produce a good summary when reminders are enabled but the exceptional events vector is empty") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedTest),
            Vector.empty,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )
        fragments should be (fragmentsWhenNoReminderForOneFailedTest)
      }

      it("should produce a good summary when reminders are enabled without stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedTest),
            justOneTestFailed,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments should be (initialFragmentsForJustOneFailedTest)
      }

      it("should produce a good summary when reminders are enabled with short stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedTest),
            justOneTestFailed,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = true,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments.take(initialFragmentsForJustOneFailedTest.size) should be (initialFragmentsForJustOneFailedTest)
        fragments.length should equal (initialFragmentsForJustOneFailedTest.size + StringReporter.shortStackTraceSize) 
      }

      it("should produce a good summary when reminders are enabled with full stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedTest),
            justOneTestFailed,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = true,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments.take(initialFragmentsForJustOneFailedTest.size) should be (initialFragmentsForJustOneFailedTest)
        fragments.length should be > (initialFragmentsForJustOneFailedTest.size + StringReporter.shortStackTraceSize) 
      }
    }

    describe("when one test failed and one canceled") {
      it("should produce a good summary when reminders are disabled") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedAndOneCanceledTest),
            oneTestFailedAndOneTestCanceled,
            presentAllDurations = false,
            presentReminder = false,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )
        fragments should be (fragmentsWhenNoReminderForOneFailedAndOneCanceledTest)
      }

      it("should produce a good summary when reminders are enabled but the exceptional events vector is empty") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedAndOneCanceledTest),
            Vector.empty,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )
        fragments should be (fragmentsWhenNoReminderForOneFailedAndOneCanceledTest)
      }

      it("should produce a good summary when reminders are enabled without stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedAndOneCanceledTest),
            oneTestFailedAndOneTestCanceled,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments.take(initialFragmentsForOneFailedAndOneCanceledTest.size) should be (initialFragmentsForOneFailedAndOneCanceledTest)
        fragments.drop(initialFragmentsForOneFailedAndOneCanceledTest.size) should be (initialFragmentsTheOneCanceledTestReminder)
      }

      it("should produce a good summary when reminders are enabled, but canceled tests are deselected") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedAndOneCanceledTest),
            oneTestFailedAndOneTestCanceled,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = true,
            presentFilePathname = false
          )

        fragments should be (initialFragmentsForOneFailedAndOneCanceledTest)
      }

      it("should produce a good summary when reminders are enabled with short stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedAndOneCanceledTest),
            oneTestFailedAndOneTestCanceled,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = true,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments.take(initialFragmentsForOneFailedAndOneCanceledTest.size) should be (initialFragmentsForOneFailedAndOneCanceledTest)
        fragments.drop(initialFragmentsForOneFailedAndOneCanceledTest.size + StringReporter.shortStackTraceSize).take(initialFragmentsTheOneCanceledTestReminder.size) should be (initialFragmentsTheOneCanceledTestReminder)
        fragments.length should equal (initialFragmentsForJustOneFailedTest.size + initialFragmentsTheOneCanceledTestReminder.size + (2 * StringReporter.shortStackTraceSize))
      }

      it("should produce a good summary when reminders are enabled with full stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneFailedAndOneCanceledTest),
            oneTestFailedAndOneTestCanceled,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = true,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments.take(initialFragmentsForOneFailedAndOneCanceledTest.size) should be (initialFragmentsForOneFailedAndOneCanceledTest)
        fragments.length should be > (initialFragmentsForJustOneFailedTest.size + initialFragmentsTheOneCanceledTestReminder.size + (2 * StringReporter.shortStackTraceSize))
      }
    }

    describe("when one Suite aborted") {
      val summaryWithOneAbortedSuite =
        new Summary(
          testsSucceededCount = 0,
          testsFailedCount = 0,
          testsIgnoredCount = 0,
          testsPendingCount = 0,
          testsCanceledCount = 0,
          suitesCompletedCount = 0,
          suitesAbortedCount = 1,
          scopesPendingCount = 0
        )

      val oneSuiteAborted: Vector[ExceptionalEvent] = 
        Vector(
          SuiteAborted(
            ordinal = firstOrdinal,
            message = "I meant to do that!",
            suiteName = "StringReporterSummarySpec",
            suiteId = "org.scalatest.tools.StringReporterSummarySpec",
            suiteClassName =
              Some("org.scalatest.tools.StringReporterSummarySpec"),
            throwable = Some(testFailedException),
            duration = Some(16),
            formatter =
              Some(IndentedText(
                "THIS SHOULD NOT BE USED", "SHOULD NOT BE USED", 1)),
            location = Some(SeeStackDepthException),
            rerunner = Some("org.scalatest.tools.StringReporterSummarySpec"),
            payload = None,
            threadName = "Thread-10",
            timeStamp = 1369965123278L
          )
        )

      val fragmentsWhenNoReminderForOneSuiteAborted: Vector[Fragment] =
        Vector(
          Fragment(Resources.runCompletedIn(makeDurationString(213L)), AnsiCyan),
          Fragment(Resources.totalNumberOfTestsRun(summaryWithOneAbortedSuite.testsCompletedCount.toString), AnsiCyan),
          Fragment(Resources.suiteSummary(summaryWithOneAbortedSuite.suitesCompletedCount.toString, summaryWithOneAbortedSuite.suitesAbortedCount.toString), AnsiCyan),
          Fragment(
            Resources.testSummary(
              summaryWithOneAbortedSuite.testsSucceededCount.toString,
              summaryWithOneAbortedSuite.testsFailedCount.toString,
              summaryWithOneAbortedSuite.testsCanceledCount.toString,
              summaryWithOneAbortedSuite.testsIgnoredCount.toString,
              summaryWithOneAbortedSuite.testsPendingCount.toString),
            AnsiCyan),
          Fragment(Resources.oneSuiteAborted, AnsiRed)
        )

      val initialFragmentsForJustOneAbortedSuite: Vector[Fragment] =
        Vector(
          Fragment(Resources.runCompletedIn(makeDurationString(213L)), AnsiCyan),
          Fragment(Resources.totalNumberOfTestsRun(summaryWithOneAbortedSuite.testsCompletedCount.toString), AnsiCyan),
          Fragment(Resources.suiteSummary(summaryWithOneAbortedSuite.suitesCompletedCount.toString, summaryWithOneAbortedSuite.suitesAbortedCount.toString), AnsiCyan),
          Fragment(Resources.testSummary(
              summaryWithOneAbortedSuite.testsSucceededCount.toString,
              summaryWithOneAbortedSuite.testsFailedCount.toString,
              summaryWithOneAbortedSuite.testsCanceledCount.toString,
              summaryWithOneAbortedSuite.testsIgnoredCount.toString,
              summaryWithOneAbortedSuite.testsPendingCount.toString), 
            AnsiCyan),
          Fragment(Resources.oneSuiteAborted, AnsiRed),
          Fragment("StringReporterSummarySpec:", AnsiRed),
          Fragment(Resources.specTextAndNote("", Resources.abortedNote), AnsiRed),
          Fragment(
            "  I meant to do that! (StringReporterSummarySpec.scala:" +
              tfeLineNumber + ")", 
            AnsiRed)
        )


      it("should produce a good summary when reminders are disabled") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneAbortedSuite),
            oneSuiteAborted,
            presentAllDurations = false,
            presentReminder = false,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )
        fragments should be (fragmentsWhenNoReminderForOneSuiteAborted)
      }

      it("should produce a good summary when reminders are enabled but the exceptional events vector is empty") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneAbortedSuite),
            Vector.empty,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )
        fragments should be (fragmentsWhenNoReminderForOneSuiteAborted)
      }

      it("should produce a good summary when reminders are enabled without stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneAbortedSuite),
            oneSuiteAborted,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments should be (initialFragmentsForJustOneAbortedSuite)
      }

      it("should produce a good summary when reminders are enabled with short stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneAbortedSuite),
            oneSuiteAborted,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = true,
            presentReminderWithFullStackTraces = false,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments.take(initialFragmentsForJustOneAbortedSuite.size) should be (
          initialFragmentsForJustOneAbortedSuite)

        fragments.length should equal (
          initialFragmentsForJustOneAbortedSuite.size +
          StringReporter.shortStackTraceSize) 
      }

      it("should produce a good summary when reminders are enabled with full stack traces") {
        val fragments =
          summaryFragments(
            true,
            Some(213L),
            Some(summaryWithOneAbortedSuite),
            oneSuiteAborted,
            presentAllDurations = false,
            presentReminder = true,
            presentReminderWithShortStackTraces = false,
            presentReminderWithFullStackTraces = true,
            presentReminderWithoutCanceledTests = false,
            presentFilePathname = false
          )

        fragments.take(initialFragmentsForJustOneAbortedSuite.size) should be (
          initialFragmentsForJustOneAbortedSuite)

        fragments.length should be > (
          initialFragmentsForJustOneAbortedSuite.size +
          StringReporter.shortStackTraceSize) 
      }
    }

    ignore("should fail on purpose") {
      info("I should not show up in the reminder")
      fail("I meant to do that!")
    }
    ignore("should cancel on purpose") {
      info("I should not show up in the reminder")
      cancel("I meant to do that!")
    }
  }
}

