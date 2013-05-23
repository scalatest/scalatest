/*
 * Copyright 2001-2008 Artima, Inc.
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
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.util.Iterator
import java.util.Set
import java.io.StringWriter
import org.scalatest.events._
import PrintReporter._
import org.scalatest.junit.JUnitTestFailedError
import org.scalatest.exceptions.PropertyCheckFailedException
import org.scalatest.exceptions.TableDrivenPropertyCheckFailedException
import Suite.indentation
import org.scalatest.exceptions.StackDepth
import StringReporter._

/**
 * A <code>Reporter</code> that prints test status information to
 * a <code>Writer</code>, <code>OutputStream</code>, or file.
 *
 * @author Bill Venners
 */
private[scalatest] abstract class StringReporter(presentAllDurations: Boolean,
        presentInColor: Boolean, presentShortStackTraces: Boolean, presentFullStackTraces: Boolean,
        presentUnformatted: Boolean) extends ResourcefulReporter {

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

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String], message: Option[String]): Option[String] =
    stringToPrintWhenNoError(resourceName, formatter, suiteName, testName, None, message)

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String], duration: Option[Long], message: Option[String]): Option[String] = {
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
        val unformattedText = Resources(resourceName, arg + messageText)
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources("withDuration", unformattedText, makeDurationString(milliseconds)))
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
                Some(Resources("withDuration", formattedText, makeDurationString(milliseconds)))
              else
                Some(formattedText)
            case None => Some(formattedText)
          }
        case Some(MotionToSuppress) => None
        case _ => genUnformattedText
      }
    }

    if (presentUnformatted) genUnformattedText
    else                    genFormattedText
  }

  def apply(event: Event) {

    event match {
      case _: DiscoveryStarting =>
        val stringToPrint: Option[String] =
          stringToPrintWhenNoError("discoveryStarting", None, "", None, None)

        val optFragment: Vector[Fragment] = stringToPrint.toVector map (new Fragment(_, AnsiCyan))

        optFragment foreach printPossiblyInColor
    
// TODO: I think we should let people elide these events in reporters
      case DiscoveryCompleted(_, duration, _, _) => 
        val stringToPrint =
          duration match {
            case Some(milliseconds) => 
              Resources("discoveryCompletedIn", makeDurationString(milliseconds))
            case None =>
              Resources("discoveryCompleted")
          }

        val fragment = Vector(Fragment(stringToPrint, AnsiCyan))

        fragment foreach printPossiblyInColor

      case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) => 

        val string = Resources("runStarting", testCount.toString)

        val fragment = Vector(Fragment(string, AnsiCyan))

        fragment foreach printPossiblyInColor

      case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 

        val fragments: Vector[Fragment] = summaryFragments(true, duration, summary) 

        fragments foreach printPossiblyInColor

      case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>

        val fragments: Vector[Fragment] = summaryFragments(false, duration, summary) 

        fragments foreach printPossiblyInColor

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 

        val lines = stringsToPrintOnError("abortedNote", "runAborted", message, throwable, formatter, None, None, duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        for (line <- lines) printPossiblyInColor(Fragment(line, AnsiRed))

      case SuiteStarting(ordinal, suiteName, suiteId, suiteClassName, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        val stringToPrint = stringToPrintWhenNoError("suiteStarting", formatter, suiteName, None, None)

        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiGreen))
          case None =>
        }

      case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

        val stringToPrint = stringToPrintWhenNoError("suiteCompleted", formatter, suiteName, None, duration, None)

        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiGreen))
          case None =>
        }

      case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

        val lines = stringsToPrintOnError("abortedNote", "suiteAborted", message, throwable, formatter, Some(suiteName), None, duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        for (line <- lines) printPossiblyInColor(Fragment(line, AnsiRed))

      case TestStarting(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        val stringToPrint = stringToPrintWhenNoError("testStarting", formatter, suiteName, Some(testName), None)

        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiGreen))
          case None =>
        }

      case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        val stringToPrint = stringToPrintWhenNoError("testSucceeded", formatter, suiteName, Some(testName), duration, None)

        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiGreen))
          case None =>
        }
        
       val fragments = recordedEventFragments(recordedEvents, AnsiGreen, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        fragments foreach printPossiblyInColor
    
      case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 

        val stringToPrint =
          if (presentUnformatted)
            Some(Resources("testIgnored", suiteName + ": " + testName))
          else
            formatter match {
              case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("ignoredNote")))
              case Some(MotionToSuppress) => None
              case _ => Some(Resources("testIgnored", suiteName + ": " + testName))
            }
 
        val optFragment = stringToPrint.toVector map (new Fragment(_, AnsiYellow))

        optFragment foreach printPossiblyInColor
    
      case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>

        val tff: Vector[Fragment] = testFailedFragments("failedNote", "testFailed", message, throwable, formatter, Some(suiteName), Some(testName), duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        val ref = recordedEventFragments(recordedEvents, AnsiRed, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        val fragments = tff ++ ref

        fragments foreach printPossiblyInColor

      case TestCanceled(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, payload, threadName, timeStamp) =>

        val lines: Vector[String] = stringsToPrintOnError("canceledNote", "testCanceled", message, throwable, formatter, Some(suiteName), Some(testName), duration,
            presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces).toVector

        val tcf =
          for (line <- lines)
          yield new Fragment(line, AnsiYellow)

        val ref = recordedEventFragments(recordedEvents, AnsiYellow, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        val fragments = tcf ++ ref

        fragments foreach printPossiblyInColor

      case ipEvent: InfoProvided =>

        val fragments = infoProvidedFragments(ipEvent, AnsiGreen, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        fragments foreach printPossiblyInColor

      case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>

        val testNameInfo = nameInfo.testName
        val stringToPrint = stringToPrintWhenNoError("scopeOpened", formatter, nameInfo.suiteName, nameInfo.testName, Some(message))
        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiGreen))
          case None =>
        }

      // TODO: Reduce duplication among InfoProvided, ScopeOpened, and ScopeClosed
      case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
        
        val testNameInfo = nameInfo.testName
        val stringToPrint = stringToPrintWhenNoError("scopeClosed", formatter, nameInfo.suiteName, nameInfo.testName, Some(message)) // TODO: I htink I want ot say Scope Closed - + message
        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiGreen))
          case None =>
        }
        
      case ScopePending(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) => 
        val stringToPrint =
          if (presentUnformatted)
            Some(Resources("scopePending", nameInfo.suiteName + ": " + message))
          else
            formatter match {
              case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("pendingNote")))
              case Some(MotionToSuppress) => None
              case _ => Some(Resources("scopePending", nameInfo.suiteName + ": " + message))
            }
        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiYellow))
          case None =>
        }
        
      case mpEvent: MarkupProvided =>

        val optFragment = markupProvidedOptionalFragment(mpEvent, AnsiGreen, presentUnformatted)

        optFragment foreach printPossiblyInColor

      case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>

        val stringToPrint =
          if (presentUnformatted)
            Some(Resources("testPending", suiteName + ": " + testName))
          else
            formatter match {
              case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("pendingNote")))
              case Some(MotionToSuppress) => None
              case _ => Some(Resources("testPending", suiteName + ": " + testName))
            }

        stringToPrint match {
          case Some(string) => printPossiblyInColor(Fragment(string, AnsiYellow))
          case None =>
        }
        
        val fragments = recordedEventFragments(recordedEvents, AnsiYellow, presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces)

        fragments foreach printPossiblyInColor

     // case _ => throw new RuntimeException("Unhandled event")
    }
  }
  
  protected def makeFinalReport(runCompleted: Boolean, duration: Option[Long], summaryOption: Option[Summary]) {
     
    val fragments: Vector[Fragment] = summaryFragments(runCompleted, duration, summaryOption) 

    fragments foreach printPossiblyInColor
  }

  // We subtract one from test reports because we add "- " in front, so if one is actually zero, it will come here as -1
  // private def indent(s: String, times: Int) = if (times <= 0) s else ("  " * times) + s

  // Stupid properties file won't let me put spaces at the beginning of a property
  // "  {0}" comes out as "{0}", so I can't do indenting in a localizable way. For now
  // just indent two space to the left.  //  if (times <= 0) s 
  //  else Resources("indentOnce", indent(s, times - 1))
}
 
private[scalatest] object StringReporter {

  def testFailedFragments(
    noteResourceName: String,
    errorResourceName: String,
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
  ): Vector[Fragment] = {

    val lines: Vector[String] = stringsToPrintOnError("failedNote", "testFailed", message, throwable, formatter, suiteName, testName, duration,
        presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces).toVector

    lines map (new Fragment(_, AnsiRed))
  }

  def summaryFragments(runCompleted: Boolean, duration: Option[Long], summaryOption: Option[Summary]): Vector[Fragment] = {

    val resourceName =
      if (runCompleted) "runCompleted"
      else "runStopped"

    summaryOption match {
      case Some(summary) =>

        import summary._

        Vector(

          duration match {
            case Some(msSinceEpoch) =>
              Some(Fragment(Resources(resourceName + "In", makeDurationString(msSinceEpoch)), AnsiCyan))
            case None =>
              Some(Fragment(Resources(resourceName), AnsiCyan))
          },

          // totalNumberOfTestsRun=Total number of tests run was: {0}
          Some(Fragment(Resources("totalNumberOfTestsRun", testsCompletedCount.toString), AnsiCyan)),

          if (scopesPendingCount > 0) {
            // Suites: completed {0}, aborted {1}  Scopes: pending {2}
            Some(Fragment(Resources("suiteScopeSummary", suitesCompletedCount.toString, suitesAbortedCount.toString, scopesPendingCount.toString), AnsiCyan))
          }
          else {
            // Suites: completed {0}, aborted {1}
            Some(Fragment(Resources("suiteSummary", suitesCompletedCount.toString, suitesAbortedCount.toString), AnsiCyan))
          },

          // Tests: succeeded {0}, failed {1}, ignored, {2}, pending {3}, canceled {4}
          Some(Fragment(Resources("testSummary", testsSucceededCount.toString, testsFailedCount.toString, testsCanceledCount.toString, testsIgnoredCount.toString, testsPendingCount.toString), AnsiCyan)),

          // *** 1 SUITE ABORTED ***
          if (suitesAbortedCount == 1) {
            Some(Fragment(Resources("oneSuiteAborted"), AnsiRed))
          }
          // *** {0} SUITES ABORTED ***
          else if (suitesAbortedCount > 1) {
            Some(Fragment(Resources("multipleSuitesAborted", suitesAbortedCount.toString), AnsiRed))
          }
          else None,
   
          // *** 1 TEST FAILED ***
          if (testsFailedCount == 1) {
            Some(Fragment(Resources("oneTestFailed"), AnsiRed))
          }
          // *** {0} TESTS FAILED ***
          else if (testsFailedCount > 1) {
            Some(Fragment(Resources("multipleTestsFailed", testsFailedCount.toString), AnsiRed))
          }
          else if (suitesAbortedCount == 0) { // Maybe don't want to say this if the run aborted or stopped because "all"
            Some(Fragment(Resources("allTestsPassed"), AnsiGreen))
          }
          else None
        ).flatten

      case None => Vector.empty
    }
  }

  // Called for TestFailed, InfoProvided (because it can have a throwable in it), SuiteAborted, and RunAborted
  def stringsToPrintOnError(
    noteResourceName: String,
    errorResourceName: String,
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
  ): List[String] = {

    def genFormattedText = {
      formatter match {
        case Some(IndentedText(formattedText, _, _)) =>
          Resources("specTextAndNote", formattedText, Resources(noteResourceName))
        case _ =>
          genUnformattedText
      }
    }

    def genUnformattedText = {
      // Deny MotionToSuppress directives in error events, because error info needs to be seen by users
      suiteName match {
        case Some(sn) =>
          testName match {
            case Some(tn) => Resources(errorResourceName, sn + ": " + tn + ": " + message)
            case None => Resources(errorResourceName, sn + ": " + message)
          }
        // Should not get here with built-in ScalaTest stuff, but custom stuff could get here.
        case None => Resources(errorResourceName, Resources("noNameSpecified"))
      }
    }

    val stringToPrint =
      if (presentUnformatted) genUnformattedText
      else                    genFormattedText

    val stringToPrintWithPossibleDuration =
      duration match {
        case Some(milliseconds) =>
          if (presentAllDurations)
            Resources("withDuration", stringToPrint, makeDurationString(milliseconds))
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
            val labeledClassName = if (isCause) Resources("DetailsCause") + ": " + className else className
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
                      stackTraceThisThrowable.head :: (whiteSpace + "...") :: stackTraceThisThrowable.drop(stackDepth + 1).take(7) ::: List(whiteSpace + "...")
                    case _ => // In case of IAE or what not, show top 10 stack frames
                      stackTraceThisThrowable.head :: stackTraceThisThrowable.drop(1).take(10) ::: List(whiteSpace + "...")
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

    if (possiblyEmptyMessageWithPossibleLineNumber.isEmpty)
      stringToPrintWithPossibleDuration :: getStackTrace(throwable)
    else
      stringToPrintWithPossibleDuration :: possiblyEmptyMessageWithPossibleLineNumber.split("\n").toList.map(whiteSpace + _) ::: getStackTrace(throwable)
  }

  def withPossibleLineNumber(stringToPrint: String, throwable: Option[Throwable]): String = {
    throwable match {
      case Some(stackDepth: StackDepth) =>
        stackDepth.failedCodeFileNameAndLineNumberString match {
          case Some(lineNumberString) =>
            Resources("printedReportPlusLineNumber", stringToPrint, lineNumberString)
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
    (for (e <- recordedEvents.toVector) yield {
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
    val lines: Vector[String] = stringsToPrintOnError("infoProvidedNote", "infoProvided", event.message, event.throwable, event.formatter, suiteName, testName, None,
        presentUnformatted, presentAllDurations, presentShortStackTraces, presentFullStackTraces).toVector

    lines map (new Fragment(_, ansiColor))
    // for (line <- lines) printPossiblyInColor(line, ansiColor)
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

    val stringToPrint: Option[String] = stringToPrintWhenMarkup(event.formatter, suiteName, testName, event.text, presentUnformatted)

    stringToPrint.toVector map (new Fragment(_, ansiColor))
  }

  def stringToPrintWhenMarkup(
    formatter: Option[Formatter],
    suiteName: Option[String],
    testName: Option[String],
    text: String,
    presentUnformatted: Boolean
  ): Option[String] = {

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

    if (presentUnformatted) genUnformattedText
    else                    genFormattedText
  }
}

