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

import org.scalatest._
import org.scalatest.events._
import org.scalatest.exceptions.PropertyCheckFailedException
import Suite.indentation
import org.scalatest.exceptions.StackDepth
import StringReporter._
import scala.collection.mutable.ListBuffer

/**
 * A <code>Reporter</code> that prints test status information to
 * a <code>Writer</code>, <code>OutputStream</code>, or file.
 *
 * @author Bill Venners
 */
private[scalatest] abstract class StringReporter(
  presentAllDurations: Boolean,
  presentInColor: Boolean,
  presentShortStackTraces: Boolean,
  presentFullStackTraces: Boolean,
  presentUnformatted: Boolean,
  presentReminder: Boolean,
  presentReminderWithShortStackTraces: Boolean,
  presentReminderWithFullStackTraces: Boolean,
  presentReminderWithoutCanceledTests: Boolean
) extends ResourcefulReporter {

  val reminderEventsBuf = new ListBuffer[ExceptionalEvent]

  protected def printPossiblyInColor(fragment: Fragment)

/*
I either want to print the full stack trace, like this:

[scalatest] TEST FAILED - JUnitTestCaseSuite: testSomething(org.scalatestexamples.junit.JUnitTestCaseSuite) (JUnitTestCaseSuite.scala:22)
[scalatest]   hi there
[scalatest]   org.scalatest.junit.JUnitTestFailedError: hi there
[scalatest]   at org.scalatest.junit.AssertionsForJUnit$class.newAssertionFailedException(AssertionsForJUnit.scala:101)
[scalatest]   at org.scalatest.junit.JUnit3Suite.newAssertionFailedException(JUnit3Suite.scala:140)
[scalatest]   at org.scalatest.Assertions$class.fail(Assertions.scala:601)
[scalatest]   at org.scalatest.junit.JUnit3Suite.fail(JUnit3Suite.scala:140)
[scalatest]   at org.scalatestexamples.junit.JUnitTestCaseSuite.testSomething(JUnitTestCaseSuite.scala:22)
[scalatest]   at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
[scalatest]   at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
[scalatest]   at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
[scalatest]   at java.lang.reflect.Method.invoke(Method.java:585)
[scalatest]   at junit.framework.TestCase.runTest(TestCase.java:168)
[scalatest]   at junit.framework.TestCase.runBare(TestCase.java:134)
[scalatest]   at junit.framework.TestResult$1.protect(TestResult.java:110)
[scalatest]   at junit.framework.TestResult.runProtected(TestResult.java:128)
[scalatest]   at junit.framework.TestResult.run(TestResult.java:113)
[scalatest]   at junit.framework.TestCase.run(TestCase.java:124)
[scalatest]   at junit.framework.TestSuite.runTest(TestSuite.java:232)
[scalatest]   at junit.framework.TestSuite.run(TestSuite.java:227)
[scalatest]   at junit.framework.TestSuite.runTest(TestSuite.java:232)
[scalatest]   at junit.framework.TestSuite.run(TestSuite.java:227)
[scalatest]   at org.scalatest.junit.JUnit3Suite.run(JUnit3Suite.scala:151)
[scalatest]   at org.scalatest.tools.SuiteRunner.run(SuiteRunner.scala:59)
[scalatest]   at org.scalatest.tools.Runner$$anonfun$doRunRunRunADoRunRun$2.apply(Runner.scala:1430)
[scalatest]   at org.scalatest.tools.Runner$$anonfun$doRunRunRunADoRunRun$2.apply(Runner.scala:1427)
[scalatest]   at scala.List.foreach(List.scala:834)
[scalatest]   at org.scalatest.tools.Runner$.doRunRunRunADoRunRun(Runner.scala:1427)
[scalatest]   at org.scalatest.tools.RunnerJFrame$RunnerThread$$anonfun$run$1.apply(RunnerJFrame.scala:1352)
[scalatest]   at org.scalatest.tools.RunnerJFrame$RunnerThread$$anonfun$run$1.apply(RunnerJFrame.scala:1350)
[scalatest]   at org.scalatest.tools.Runner$.withClassLoaderAndDispatchReporter(Runner.scala:1471)
[scalatest]   at org.scalatest.tools.RunnerJFrame$RunnerThread.run(RunnerJFrame.scala:1349)

Or show a truncated one like this:

[scalatest] TEST FAILED - JUnitTestCaseSuite: testSomething(org.scalatestexamples.junit.JUnitTestCaseSuite) (JUnitTestCaseSuite.scala:22)
[scalatest]   hi there
[scalatest] org.scalatest.junit.JUnitTestFailedError: hi there
[scalatest]   ...
[scalatest]   at org.scalatestexamples.junit.JUnitTestCaseSuite.testSomething(JUnitTestCaseSuite.scala:22)
[scalatest]   at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
[scalatest]   at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
[scalatest]   at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
[scalatest]   at java.lang.reflect.Method.invoke(Method.java:585)
[scalatest]   at junit.framework.TestCase.runTest(TestCase.java:168)
[scalatest]   at junit.framework.TestCase.runBare(TestCase.java:134)
[scalatest]   ...

If F is specified for the reporter, then show the full stack trace (or if it is not a StackDepth). But
if a StackDepth and no F specified, then show the truncated form.

Now want to change from:
- should do something interesting *** FAILED *** (<console>:18) (0 milliseconds)
  org.scalatest.TestFailedException: 2 did not equal 3

To:

- should do something interesting *** FAILED *** (0 milliseconds)
  2 did not equal 3 (<console>:18)
  org.scalatest.TestFailedException: 

The second line would only be printed out if there was an exception. That way
when I add noStacks option, I get:

- should do something interesting *** FAILED *** (0 milliseconds)
  2 did not equal 3 (<console>:18)

Or for a prop check get:

- should do something interesting *** FAILED *** (0 milliseconds)
  Property check failed. (InfoInsideTestFiredAfterTestProp.scala:24)
  Message: 2 was not less than 1
  Location: InfoInsideTestFiredAfterTestProp.scala:27
  Occurred at table row 0 (zero based, not counting headings), which had values ( / This shouldb e had value without the s
    suite = org.scalatest.InfoInsideTestFiredAfterTestProp$$anon$3@18a4edc4
  )

Easiest thing is if the exception message just printed this out. Then StringReporter would just print the message always,
and not print it after the outermost exception
org.scalatest.prop.TableDrivenPropertyCheckFailedException:
...

And does print it out after the subsequent ones:
org.scalatest.TestFailedException: 2 did not equal 3

And it would not need to put the line number there. It would already be in the message. It would use the message sent with
the event. Message should just be the throwable's message, or "<exception class> was thrown" Then it is easy. Always
use the message from the event.

org.scalatest.prop.TableDrivenPropertyCheckFailedException: TestFailedException (included as this exception's cause) was thrown during property evaluation.
[scalatest]   Message: 
[scalatest]   Location: InfoInsideTestFiredAfterTestProp.scala:27
[scalatest]   Occurred at table row 0 (zero based, not counting headings), which had values (
[scalatest]     suite = org.scalatest.InfoInsideTestFiredAfterTestProp$$anon$3@18a4edc4
[scalatest]   )
*/

  def apply(event: Event) {
    event match {
      case ee: ExceptionalEvent if presentReminder =>
        if (!presentReminderWithoutCanceledTests || event.isInstanceOf[TestFailed]) {
          reminderEventsBuf += ee
        }
      case _ =>
    }
    fragmentsForEvent(
      event,
      presentUnformatted,
      presentAllDurations,
      presentShortStackTraces,
      presentFullStackTraces,
      presentReminder,
      presentReminderWithShortStackTraces,
      presentReminderWithFullStackTraces,
      presentReminderWithoutCanceledTests,
      reminderEventsBuf
   ) foreach printPossiblyInColor
  }

  // We subtract one from test reports because we add "- " in front, so if one is actually zero, it will come here as -1
  // private def indent(s: String, times: Int) = if (times <= 0) s else ("  " * times) + s

  // Stupid properties file won't let me put spaces at the beginning of a property
  // "  {0}" comes out as "{0}", so I can't do indenting in a localizable way. For now
  // just indent two space to the left.  //  if (times <= 0) s 
  //  else Resources.indentOnce(indent(s, times - 1))
}
 
private[scalatest] object StringReporter {

  val shortStackTraceSize = 10

  def fragmentsWhenNoError(
    messageFun: Any => String,
    formatter: Option[Formatter],
    suiteName: String,
    testName: Option[String],
    message: Option[String],
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    ansiColor: AnsiColor = AnsiGreen,
    duration: Option[Long] = None
  ): Vector[Fragment] = {
    val lines: Vector[String] = stringToPrintWhenNoError(messageFun, formatter, suiteName, testName, message, presentAllDurations, presentUnformatted,
      duration)

    lines map (new Fragment(_, ansiColor))
  }

  def fragmentsOnError(
    noteMessageFun: String,
    errorMessageFun: Any => String,
    message: String,
    throwable: Option[Throwable],
    formatter: Option[Formatter],
    suiteName: Option[String],
    testName: Option[String],
    duration: Option[Long],
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    ansiColor: AnsiColor
  ): Vector[Fragment] = {

    val lines: Vector[String] = stringsToPrintOnError(noteMessageFun, errorMessageFun, message, throwable, formatter, suiteName, testName, duration,
        presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

    lines map (new Fragment(_, ansiColor))
  }

  def summaryFragments(
     runCompleted: Boolean,
     duration: Option[Long],
     summaryOption: Option[Summary],
     exceptionalEvents: Vector[ExceptionalEvent],
     presentAllDurations: Boolean,
     presentReminder: Boolean,
     presentReminderWithShortStackTraces: Boolean,
     presentReminderWithFullStackTraces: Boolean,
     presentReminderWithoutCanceledTests: Boolean
  ): Vector[Fragment] = {

    val resourceName =
      if (runCompleted) "runCompleted"
      else "runStopped"

    val summaryFrags: Vector[Fragment] = 
      summaryOption match {
        case Some(summary) =>
  
          import summary._
  
          Vector(
  
            duration match {
              case Some(msSinceEpoch) =>
                Some(Fragment(if (runCompleted) Resources.runCompletedIn(makeDurationString(msSinceEpoch)) else Resources.runStoppedIn(makeDurationString(msSinceEpoch)), AnsiCyan))
              case None =>
                Some(Fragment(if (runCompleted) Resources.runCompleted else Resources.runStopped, AnsiCyan))
            },
  
            // totalNumberOfTestsRun=Total number of tests run was: {0}
            Some(Fragment(Resources.totalNumberOfTestsRun(testsCompletedCount.toString), AnsiCyan)),
  
            if (scopesPendingCount > 0) {
              // Suites: completed {0}, aborted {1}  Scopes: pending {2}
              Some(Fragment(Resources.suiteScopeSummary(suitesCompletedCount.toString, suitesAbortedCount.toString, scopesPendingCount.toString), AnsiCyan))
            }
            else {
              // Suites: completed {0}, aborted {1}
              Some(Fragment(Resources.suiteSummary(suitesCompletedCount.toString, suitesAbortedCount.toString), AnsiCyan))
            },
  
            // Tests: succeeded {0}, failed {1}, ignored, {2}, pending {3}, canceled {4}
            Some(Fragment(Resources.testSummary(testsSucceededCount.toString, testsFailedCount.toString, testsCanceledCount.toString, testsIgnoredCount.toString, testsPendingCount.toString), AnsiCyan)),
  
            // *** 1 SUITE ABORTED ***
            if (suitesAbortedCount == 1) {
              Some(Fragment(Resources.oneSuiteAborted, AnsiRed))
            }
            // *** {0} SUITES ABORTED ***
            else if (suitesAbortedCount > 1) {
              Some(Fragment(Resources.multipleSuitesAborted(suitesAbortedCount.toString), AnsiRed))
            }
            else None,
  
            // *** 1 TEST FAILED ***
            if (testsFailedCount == 1) {
              Some(Fragment(Resources.oneTestFailed, AnsiRed))
            }
            // *** {0} TESTS FAILED ***
            else if (testsFailedCount > 1) {
              Some(Fragment(Resources.multipleTestsFailed(testsFailedCount.toString), AnsiRed))
            }
            else if (suitesAbortedCount == 0) { // Maybe don't want to say this if the run aborted or stopped because "all"
              if (testsCompletedCount > 0)
                Some(Fragment(Resources.allTestsPassed, AnsiGreen))
              else
                Some(Fragment(Resources.noTestsWereExecuted, AnsiYellow))
            }
            else None
          ).flatten
  
        case None => Vector(Fragment(if (runCompleted) Resources.runCompleted else Resources.runStopped, AnsiCyan))
      }

      val filteredSortedEvents =
        if (presentReminderWithoutCanceledTests)
          exceptionalEvents.filter(_.isInstanceOf[TestFailed]).sortBy(_.ordinal)
        else
          exceptionalEvents.sortBy(_.ordinal)

      val reminderFrags: Vector[Fragment] =
        if (presentReminder)
          for {
            event <- filteredSortedEvents
            frag <- exceptionalFragments(
              event,
              presentAllDurations,
              presentReminderWithShortStackTraces,
              presentReminderWithFullStackTraces,
              presentReminderWithoutCanceledTests
            )
          } yield frag
        else Vector.empty

      summaryFrags ++ reminderFrags
  }

  def exceptionalFragments(
     exceptionalEvent: ExceptionalEvent,
     presentAllDurations: Boolean,
     presentReminderWithShortStackTraces: Boolean,
     presentReminderWithFullStackTraces: Boolean,
     presentReminderWithoutCanceledTests: Boolean
  ): Vector[Fragment] = {

    def theFragments(
      testNameOpt: Option[String],
      testTextOpt: Option[String],
      suiteName: String,
      noteMessageFun: String,
      errorMessageFun: Any => String,
      message: String,
      throwable: Option[Throwable],
      duration: Option[Long],
      ansiColor: AnsiColor
    ): Vector[Fragment] = {
      val prefix: Option[String] = {
        (testNameOpt, testTextOpt) match {
          case (Some(testName), Some(testText)) =>
            val prefixLength = testName.length - testText.length
            if (testName.drop(prefixLength) == testText)
              Some(testName.take(prefixLength))
            else None
          case _ => None
        }
      }
      val suiteNameFrag = Fragment(suiteName + ":", ansiColor)
      val formatter = testTextOpt match {
        case Some(testText) =>
          Some(IndentedText("- " + testText, testText, 0))
        case None =>
          Some(IndentedText("", "", 0))
      }
      val otherFrags: Vector[Fragment] =
        fragmentsOnError(
          noteMessageFun,
          errorMessageFun,
          message,
          throwable,
          formatter,
          Some(suiteName),
          testNameOpt,
          duration,
          false,
          presentAllDurations,
          presentReminderWithShortStackTraces,
          presentReminderWithFullStackTraces,
          ansiColor
        )
        val testNameFrags: Vector[Fragment] =
          prefix match {
            case Some(pre) =>
              val preFrag = Fragment(pre, ansiColor)
              preFrag +: otherFrags
            case None =>
              otherFrags
          }
        suiteNameFrag +: testNameFrags
    }
    exceptionalEvent match {
      case tf: TestFailed =>
        // Usually, the testName should end with the testText. In that normal
        // case we'll separate them, because that makes it easier for people to
        // search (by searching just for the test text). But we'll put the scopes
        // all on one line as a "prefix", like:
        //
        // FredSpec:
        // A Stack (when empty)
        // - should be empty
        //
        // Even though this might have come out when it actually failed as:
        //
        // FredSpec:
        // A Stack
        //   (when empty)
        //   - should be empty
        //
        theFragments(
          Some(tf.testName),
          Some(tf.testText),
          tf.suiteName,
          Resources.failedNote,
          Resources.testFailed _,
          tf.message,
          tf.throwable,
          tf.duration,
          AnsiRed
        )
      case tc: TestCanceled =>
        theFragments(
          Some(tc.testName),
          Some(tc.testText),
          tc.suiteName,
          Resources.canceledNote,
          Resources.testCanceled _,
          tc.message,
          tc.throwable,
          tc.duration,
          AnsiYellow
        )
      case sa: SuiteAborted =>
        theFragments(
          None,
          None,
          sa.suiteName,
          Resources.abortedNote,
          Resources.suiteAborted _,
          sa.message,
          sa.throwable,
          sa.duration,
          AnsiRed
        )
    }
  }

  def withPossibleLineNumber(stringToPrint: String, throwable: Option[Throwable]): String = {
    throwable match {
      case Some(stackDepth: StackDepth) =>
        stackDepth.failedCodeFileNameAndLineNumberString match {
          case Some(lineNumberString) =>
            Resources.printedReportPlusLineNumber(stringToPrint, lineNumberString)
          case None => stringToPrint
        }
      case _ => stringToPrint
    }
  }

  def recordedEventFragments(
    recordedEvents: collection.immutable.IndexedSeq[RecordableEvent],
    ansiColor: AnsiColor,
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean
  ): Vector[Fragment] = {
    // (for (e <- recordedEvents.toVector) yield
    (for (e <- Vector.empty ++ recordedEvents) yield { // While supporting 2.9, can't use toVector
      e match {
        case ipEvent: InfoProvided =>
          infoProvidedFragments(ipEvent, ansiColor, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)
        case mpEvent: MarkupProvided =>
          markupProvidedOptionalFragment(mpEvent, ansiColor, presentUnformatted)
      }
    }).flatten
  }

  def infoProvidedFragments(
    event: InfoProvided,
    ansiColor: AnsiColor,
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean
  ): Vector[Fragment] = {
    val (suiteName, testName) =
      event.nameInfo match {
        case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
        case None => (None, None)
      }
    val lines: Vector[String] =
      stringsToPrintOnError(
        Resources.infoProvidedNote,
        Resources.infoProvided _,
        event.message,
        event.throwable,
        event.formatter,
        suiteName,
        testName,
        None,
        presentUnformatted,
        presentAllDurations,
        presentShortStackTraces,
        presentFullStackTraces
      )

    lines map (new Fragment(_, ansiColor))
    // for (line <- lines) printPossiblyInColor(line, ansiColor)
  }

  def alertProvidedFragments(
    event: AlertProvided,
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean
  ): Vector[Fragment] = {
    val (suiteName, testName) =
      event.nameInfo match {
        case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
        case None => (None, None)
      }
    val lines: Vector[String] =
      stringsToPrintOnError(
        Resources.alertProvidedNote,
        Resources.alertProvided _,
        event.message,
        event.throwable,
        event.formatter,
        suiteName,
        testName,
        None,
        presentUnformatted,
        presentAllDurations,
        presentShortStackTraces,
        presentFullStackTraces
      )

    lines map (new Fragment(_, AnsiYellow))
  }

  def noteProvidedFragments(
    event: NoteProvided,
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean
  ): Vector[Fragment] = {
    val (suiteName, testName) =
      event.nameInfo match {
        case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
        case None => (None, None)
      }
    val lines: Vector[String] =
      stringsToPrintOnError(
        Resources.noteProvidedNote,
        Resources.noteProvided _,
        event.message,
        event.throwable,
        event.formatter,
        suiteName,
        testName,
        None,
        presentUnformatted,
        presentAllDurations,
        presentShortStackTraces,
        presentFullStackTraces
      )

    lines map (new Fragment(_, AnsiGreen))
  }

  // This will return either an empty Vector or a Vector with one element. Vector instead of Option because
  // that makes it easier to combine them with other Vector[Fragment]s coming back from other sibling methods.
  def markupProvidedOptionalFragment(event: MarkupProvided, ansiColor: AnsiColor, presentUnformatted: Boolean): Vector[Fragment] = {

    val (suiteName, testName) =
      event.nameInfo match {
        case Some(NameInfo(suiteName, _, _, testName)) =>
          (Some(suiteName), testName)
        case None => (None, None)
      }

    val stringToPrint: Vector[String] = stringToPrintWhenMarkup(event.formatter, suiteName, testName, event.text, presentUnformatted)

    stringToPrint map (new Fragment(_, ansiColor))
  }

  // Will return a Vector that is either empty or contains one string
  def stringToPrintWhenMarkup(
    formatter: Option[Formatter],
    suiteName: Option[String],
    testName: Option[String],
    text: String,
    presentUnformatted: Boolean
  ): Vector[String] = {

    def genUnformattedText = {
      val prefix =
        (suiteName, testName) match {
          case (None,        None)        => ""
          case (None,        Some(tName)) => tName + ": "
          case (Some(sName), None)        => sName + ": "
          case (Some(sName), Some(tName)) => sName + ": " + tName + ": "
        }

      Some(prefix + text)
    }

    def genFormattedText = {
      formatter match {
        case Some(IndentedText(formattedText, _, _)) => Some(formattedText)
        case Some(MotionToSuppress)                  => None
        case _                                       => genUnformattedText
      }
    }

    val resultAsOption: Option[String] =
      if (presentUnformatted) genUnformattedText
      else                    genFormattedText

    // resultAsOption.toVector
    Vector.empty ++ resultAsOption // While supporting 2.9 can't use toVector
  }

  // Will return a Vector that is either empty or contains one string
  def stringToPrintWhenNoError(messageFun: Any => String, formatter: Option[Formatter], suiteName: String, testName: Option[String], message: Option[String], presentAllDurations: Boolean, presentUnformatted: Boolean, duration: Option[Long] = None): Vector[String] = {
    def genUnformattedText = {
        val arg =
          testName match {
            case Some(tn) => suiteName + ": " + tn
            case None => suiteName
          }
        val messageText =
          message match {
            case Some(text) => ": " + text
            case None       => ""
          }
        val unformattedText = messageFun(arg + messageText)
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources.withDuration(unformattedText, makeDurationString(milliseconds)))
            else
              Some(unformattedText)
          case None => Some(unformattedText)
        }
    }

    def genFormattedText = {
      formatter match {
        case Some(IndentedText(formattedText, _, _)) =>
          duration match {
            case Some(milliseconds) =>
              if (presentAllDurations)
                Some(Resources.withDuration(formattedText, makeDurationString(milliseconds)))
              else
                Some(formattedText)
            case None => Some(formattedText)
          }
        case Some(MotionToSuppress) => None
        case _ => genUnformattedText
      }
    }

    val resultAsOption =
      if (presentUnformatted) genUnformattedText
      else                    genFormattedText

    // resultAsOption.toVector
    Vector.empty ++ resultAsOption // While supporting 2.9 can't use toVector
  }

  def fragmentsForEvent(
    event: Event,
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean,
    reminderEvents: Seq[ExceptionalEvent]
  ): Vector[Fragment] = {

    event match {
      // TODO: I think we should let people elide DiscoveryStarting and DiscoveryCompleted events in reporters
      case _: DiscoveryStarting =>

        fragmentsWhenNoError(String => Resources.discoveryStarting, None, "", None, None, presentUnformatted, presentAllDurations, AnsiCyan)

      case DiscoveryCompleted(_, duration, _, _) => 
        val stringToPrint =
          duration match {
            case Some(milliseconds) => 
              Resources.discoveryCompletedIn(makeDurationString(milliseconds))
            case None =>
              Resources.discoveryCompleted
          }

        Vector(Fragment(stringToPrint, AnsiCyan))

      case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) => 

        val string = Resources.runStarting(testCount.toString)

        Vector(Fragment(string, AnsiCyan))

      case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 

        summaryFragments(
          true,
          duration,
          summary,
          // reminderEvents.toVector,
          Vector.empty ++ reminderEvents, // While supporting 2.9, can't use toVector
          presentAllDurations,
          presentReminder,
          presentReminderWithShortStackTraces,
          presentReminderWithFullStackTraces,
          presentReminderWithoutCanceledTests
        )

      case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>

        summaryFragments(
          false,
          duration,
          summary,
          // reminderEvents.toVector,
          Vector.empty ++ reminderEvents, // While supporting 2.9, can't use toVector
          presentAllDurations,
          presentReminder,
          presentReminderWithShortStackTraces,
          presentReminderWithFullStackTraces,
          presentReminderWithoutCanceledTests
       ) 

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 

        fragmentsOnError(Resources.abortedNote, Any => Resources.runAborted, message, throwable, formatter, None, None, duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces, AnsiRed)

      case SuiteStarting(ordinal, suiteName, suiteId, suiteClassName, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        fragmentsWhenNoError(Resources.suiteStarting _, formatter, suiteName, None, None, presentUnformatted, presentAllDurations)

      case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

        fragmentsWhenNoError(Resources.suiteCompleted _, formatter, suiteName, None, None, presentUnformatted, presentAllDurations, AnsiGreen, duration)

      case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

        val lines = stringsToPrintOnError(Resources.abortedNote, Resources.suiteAborted _, message, throwable, formatter, Some(suiteName), None, duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        for (line <- lines) yield new Fragment(line, AnsiRed)

      case TestStarting(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        fragmentsWhenNoError(Resources.testStarting, formatter, suiteName, Some(testName), None, presentUnformatted, presentAllDurations)

      case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        val tsf: Vector[Fragment] = 
          fragmentsWhenNoError(Resources.testSucceeded, formatter, suiteName, Some(testName), None, presentUnformatted, presentAllDurations, AnsiGreen, duration)

        val ref = recordedEventFragments(recordedEvents, AnsiGreen, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        tsf ++ ref

      case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 

        val stringToPrint =
          if (presentUnformatted)
            Vector(Resources.testIgnored(suiteName + ": " + testName))
          else
            formatter match {
              case Some(IndentedText(formattedText, _, _)) => Vector(Resources.specTextAndNote(formattedText, Resources.ignoredNote))
              case Some(MotionToSuppress) => Vector.empty
              case _ => Vector(Resources.testIgnored(suiteName + ": " + testName))
            }
 
        stringToPrint map (new Fragment(_, AnsiYellow))

      case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        val tff: Vector[Fragment] = fragmentsOnError(Resources.failedNote, Resources.testFailed _, message, throwable, formatter, Some(suiteName), Some(testName), duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces, AnsiRed)

        val ref = recordedEventFragments(recordedEvents, AnsiRed, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        tff ++ ref

      case TestCanceled(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        val tcf: Vector[Fragment] = fragmentsOnError(Resources.canceledNote, Resources.testCanceled _, message, throwable, formatter, Some(suiteName), Some(testName), duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces, AnsiYellow)

        val ref = recordedEventFragments(recordedEvents, AnsiYellow, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        tcf ++ ref

      case ipEvent: InfoProvided =>

        infoProvidedFragments(ipEvent, AnsiGreen, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

      case apEvent: AlertProvided =>

        alertProvidedFragments(apEvent, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

      case npEvent: NoteProvided =>

        noteProvidedFragments(npEvent, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

      case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>

        val testNameInfo = nameInfo.testName
        fragmentsWhenNoError(Resources.scopeOpened, formatter, nameInfo.suiteName, nameInfo.testName, Some(message), presentUnformatted, presentAllDurations)

      // TODO: Reduce duplication among InfoProvided, ScopeOpened, and ScopeClosed
      case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
        
        val testNameInfo = nameInfo.testName
        fragmentsWhenNoError(Resources.scopeClosed, formatter, nameInfo.suiteName, nameInfo.testName, Some(message), presentUnformatted, presentAllDurations) // TODO: I think I want to say Scope Closed - + message

      case ScopePending(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) => 
        val stringToPrint =
          if (presentUnformatted)
            Vector(Resources.scopePending(nameInfo.suiteName + ": " + message))
          else
            formatter match {
              case Some(IndentedText(formattedText, _, _)) => Vector(Resources.specTextAndNote(formattedText, Resources.pendingNote))
              case Some(MotionToSuppress) => Vector.empty
              case _ => Vector(Resources.scopePending(nameInfo.suiteName + ": " + message))
            }
        stringToPrint map (new Fragment(_, AnsiYellow))
        
      case mpEvent: MarkupProvided =>

        markupProvidedOptionalFragment(mpEvent, AnsiGreen, presentUnformatted)

      case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>

        val stringToPrint =
          if (presentUnformatted)
            Vector(Resources.testPending(suiteName + ": " + testName))
          else
            formatter match {
              case Some(IndentedText(formattedText, _, _)) => Vector(Resources.specTextAndNote(formattedText, Resources.pendingNote))
              case Some(MotionToSuppress) => Vector.empty
              case _ => Vector(Resources.testPending(suiteName + ": " + testName))
            }

        val tpf = stringToPrint map (new Fragment(_, AnsiYellow))

        val ref = recordedEventFragments(recordedEvents, AnsiYellow, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        tpf ++ ref

     // case _ => throw new RuntimeException("Unhandled event")
    }
  }

  // Called for TestFailed, InfoProvided (because it can have a throwable in it), SuiteAborted, and RunAborted
  def stringsToPrintOnError(
    noteMessageFun: String,
    errorMessageFun: Any => String,
    message: String,
    throwable: Option[Throwable],
    formatter: Option[Formatter],
    suiteName: Option[String],
    testName: Option[String],
    duration: Option[Long],
    presentUnformatted: Boolean,
    presentAllDurations: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean
  ): Vector[String] = {

    def genFormattedText = {
      formatter match {
        case Some(IndentedText(formattedText, _, _)) =>
          Resources.specTextAndNote(formattedText, noteMessageFun)
        case _ =>
          genUnformattedText
      }
    }

    def genUnformattedText = {
      // Deny MotionToSuppress directives in error events, because error info needs to be seen by users
      suiteName match {
        case Some(sn) =>
          testName match {
            case Some(tn) => errorMessageFun(sn + ": " + tn + ": " + message)
            case None => errorMessageFun(sn + ": " + message)
          }
        // Can get here for slowpoke notices sent by DispatchReporter, and custom stuff could get here also.
        case None => errorMessageFun(Resources.noNameSpecified + ": " + message)
      }
    }

    val stringToPrint =
      if (presentUnformatted) genUnformattedText
      else                    genFormattedText

    val stringToPrintWithPossibleDuration =
      duration match {
        case Some(milliseconds) =>
          if (presentAllDurations)
            Resources.withDuration(stringToPrint, makeDurationString(milliseconds))
          else
            stringToPrint
        case None => stringToPrint
      }

    // If there's a message, put it on the next line, indented two spaces
    val possiblyEmptyMessage = Reporter.messageOrThrowablesDetailMessage(message, throwable)

    val possiblyEmptyMessageWithPossibleLineNumber =
      throwable match {
        case Some(e: PropertyCheckFailedException) => possiblyEmptyMessage // PCFEs already include the line number
        case Some(e: StackDepth) => withPossibleLineNumber(possiblyEmptyMessage, throwable) // Show it in the stack depth case
        case _ => "" // Don't show it in the non-stack depth case. It will be shown after the exception class name and colon.
      }

    // The whiteSpace is just used for printing out stack traces, etc., things that go after a test name. The formatted
    // text for test names actually goes over to the left once in a sense, to make room for the icon. So if the indentation
    // level is 3 for example, the "- " for that test's formatted text is really indented 2 times (or four spaces: "    ")
    // So that's why normally the indentation level times two spaces should be the white space. But at the top level (indentation
    // level of 0), the icon also has to go at 0 (because subtracting one would put it at -1), so in that case the white space
    // should be two spaces (or 1 level of indentation).
    val whiteSpace =
      formatter match {
        case Some(IndentedText(_, _, indentationLevel)) if (indentationLevel != 0) => indentation(indentationLevel)
        case _ => indentation(1)
      }

    def getStackTrace(throwable: Option[Throwable]): List[String] =
      throwable match {
        case Some(throwable) =>

          def stackTrace(throwable: Throwable, isCause: Boolean): List[String] = {

            val className = throwable.getClass.getName 
            val labeledClassName = if (isCause) Resources.DetailsCause + ": " + className else className
            // Only show the : message if a cause, because first one will have its message printed out 
            // Or if it is a non-StackDepth exception, because if they throw Exception with no message, the
            // message was coming out as "java.lang.Exception" then on the next line it repeated it. In the
            // case of no exception message, I think it looks best to just say the class name followed by a colon
            // and nothing else.
            val colonMessageOrJustColon =
              if ((throwable.getMessage != null && !throwable.getMessage.trim.isEmpty) && (isCause || !(throwable.isInstanceOf[StackDepth])))
                ": " + throwable.getMessage.trim
              else
                ":"

            val labeledClassNameWithMessage =
              whiteSpace + labeledClassName + colonMessageOrJustColon

            if (presentShortStackTraces || presentFullStackTraces || !(throwable.isInstanceOf[StackDepth])) {

              // Indent each stack trace item two spaces, and prepend that with an "at "
              val stackTraceElements = throwable.getStackTrace.toList map { whiteSpace + "at " + _.toString }
              val cause = throwable.getCause

              val stackTraceThisThrowable = labeledClassNameWithMessage :: stackTraceElements

              if (presentFullStackTraces) {
                if (cause == null)
                  stackTraceThisThrowable
                else
                  stackTraceThisThrowable ::: stackTrace(cause, true) // Not tail recursive, but shouldn't be too deep
              }
              else {

                // The drop(1) or drop(stackDepth + 1) that extra one is the labeledClassNameWithMessage
                val stackTraceThisThrowableTruncated = 
                  throwable match {
                    case e: Throwable with StackDepth =>
                      val stackDepth = e.failedCodeStackDepth
                      stackTraceThisThrowable.head :: (whiteSpace + "...") :: stackTraceThisThrowable.drop(stackDepth + 1).take(shortStackTraceSize - 3) ::: List(whiteSpace + "...")
                    case _ => // In case of IAE or what not, show top 10 stack frames
                      stackTraceThisThrowable.head :: stackTraceThisThrowable.drop(1).take(shortStackTraceSize) ::: List(whiteSpace + "...")
                  }
    
                if (cause == null)
                  stackTraceThisThrowableTruncated
                else
                  stackTraceThisThrowableTruncated ::: stackTrace(cause, true) // Not tail recursive, but shouldn't be too deep
              }
            }
            else
              Nil
          }
          stackTrace(throwable, false)
        case None => List()
      }

    val resultAsList =
      if (possiblyEmptyMessageWithPossibleLineNumber.isEmpty)
        stringToPrintWithPossibleDuration :: getStackTrace(throwable)
      else
        stringToPrintWithPossibleDuration :: possiblyEmptyMessageWithPossibleLineNumber.split("\n").toList.map(whiteSpace + _) ::: getStackTrace(throwable)
    Vector.empty ++ resultAsList
  }

  final val ansiReset = "\033[0m"
  final val ansiGreen = "\033[32m"
  final val ansiCyan = "\033[36m"
  final val ansiYellow = "\033[33m"
  final val ansiRed = "\033[31m"

  def makeDurationString(duration: Long) = {

    val milliseconds = duration % 1000
    val seconds = ((duration - milliseconds) / 1000) % 60
    val minutes = ((duration - milliseconds) / 60000) % 60
    val hours = (duration - milliseconds) / 3600000
    val hoursInSeconds = hours * 3600
    val hoursInMinutes = hours * 60

    val durationInSeconds = duration / 1000
    val durationInMinutes = durationInSeconds / 60

    if (duration == 1)
      Resources.oneMillisecond
    else if (duration < 1000)
      Resources.milliseconds(duration.toString)
    else if (duration == 1000)
      Resources.oneSecond
    else if (duration == 1001)
      Resources.oneSecondOneMillisecond
    else if (duration % 1000 == 0 && duration < 60000) // 2 seconds, 10 seconds, etc.
      Resources.seconds(seconds.toString)
    else if (duration > 1001 && duration < 2000)// 1 second, 45 milliseconds, etc.
      Resources.oneSecondMilliseconds(milliseconds.toString)
    else if (durationInSeconds < 60)// 3 seconds, 45 milliseconds, etc.
      Resources.secondsMilliseconds(seconds.toString, milliseconds.toString)
    else if (durationInSeconds < 61)
      Resources.oneMinute
    else if (durationInSeconds < 62)
      Resources.oneMinuteOneSecond
    else if (durationInSeconds < 120)
      Resources.oneMinuteSeconds(seconds.toString)
    else if (durationInSeconds < 121)
      Resources.minutes(minutes.toString) //
    else if (durationInSeconds < 3600 && (durationInSeconds % 60) == 1)
      Resources.minutesOneSecond(minutes.toString)
    else if (durationInSeconds < 3600)
      Resources.minutesSeconds(minutes.toString, seconds.toString)
    else if (durationInSeconds < hoursInSeconds + 1) {
      if (hours == 1)
        Resources.oneHour
      else
        Resources.hours(hours.toString)
    }
    else if (durationInSeconds < hoursInSeconds + 2) {
      if (hours == 1)
        Resources.oneHourOneSecond
      else
        Resources.hoursOneSecond(hours.toString)
    }
    else if (durationInSeconds < hoursInSeconds + 60) {
      if (hours == 1)
        Resources.oneHourSeconds(seconds.toString)
      else
        Resources.hoursSeconds(hours.toString, seconds.toString)
    }
    else if (durationInSeconds == hoursInSeconds + 60) {
      if (hours == 1)
        Resources.oneHourOneMinute
      else
        Resources.hoursOneMinute(hours.toString)
    }
    else if (durationInSeconds == hoursInSeconds + 61) {
      if (hours == 1)
        Resources.oneHourOneMinuteOneSecond
      else
        Resources.hoursOneMinuteOneSecond(hours.toString)
    }
    else if (durationInSeconds < hoursInSeconds + 120) {
      if (hours == 1)
        Resources.oneHourOneMinuteSeconds(seconds.toString)
      else
        Resources.hoursOneMinuteSeconds(hours.toString, seconds.toString)
    }
    else if (durationInSeconds % 60 == 0) {
      if (hours == 1)
        Resources.oneHourMinutes(minutes.toString)
      else
        Resources.hoursMinutes(hours.toString, minutes.toString)
    }
    else if (durationInMinutes % 60 != 1 && durationInSeconds % 60 == 1) {
      if (hours == 1)
        Resources.oneHourMinutesOneSecond(minutes.toString)
      else
        Resources.hoursMinutesOneSecond(hours.toString, minutes.toString)
    }
    else {
      if (hours == 1)
        Resources.oneHourMinutesSeconds(minutes.toString, seconds.toString)
      else
        Resources.hoursMinutesSeconds(hours.toString, minutes.toString, seconds.toString)
    }
  }
}


