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
package org.scalatest.junit

import org.scalatest._
import org.junit.runner.notification.RunNotifier
import org.junit.runner.Description
import org.junit.runner.notification.Failure
import org.scalatest.events._

// TODO: Mention on each Reporter method that it does nothing

// There's no way to really pass along a suiteStarting or suiteCompleted
// report. They have a dumb comment to "Do not invoke" fireTestRunStarted
// and fireTestRunFinished, so I think they must be doing that themselves.
// This means we don't have a way to really forward runStarting and
// runCompleted reports either. But runAborted reports should be sent
// out the door somehow, so we report them with yet another fireTestFailure.
private[junit] class RunNotifierReporter(runNotifier: RunNotifier) extends Reporter {

  // This form isn't clearly specified in JUnit docs, but some tools may assume it, so why rock the boat.
  // Here's what JUnit code does:
  //   public static Description createTestDescription(Class<?> clazz, String name, Annotation... annotations) {
  //       return new Description(String.format("%s(%s)", name, clazz.getName()), annotations);
  //   }
  // So you can see the test name shows up, which is normally a test method name, followed by the fully qualified class name in parens
  // We put test name and suite class name (or suite name if no class) in parens, but don't try and do anything to get rid of spaces or
  // parens the test or suite names themselves, since it is unclear if this format is used by anyone anyway. If actual bug reports come
  // in, then we can fix each actual problem once it is understood.
  //
  private def testDescriptionName(suiteName: String, suiteClassName: Option[String], testName: String) =
    suiteClassName match {
      case Some(suiteClassName) => testName + "(" + suiteClassName + ")"
      case None => testName + "(" + suiteName + ")"
    }

  private def suiteDescriptionName(suiteName: String, suiteClassName: Option[String]) =
    suiteClassName match {
      case Some(suiteClassName) => suiteClassName
      case None => suiteName
    }

  override def apply(event: Event) {

    event match {

      case TestStarting(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, rerunnable, payload, threadName, timeStamp) =>
        runNotifier.fireTestStarted(Description.createSuiteDescription(testDescriptionName(suiteName, suiteClassName, testName)))

      case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
        val throwableOrNull =
          throwable match {
            case Some(t) => t
            case None => null // Yuck. Not sure if the exception passed to new Failure can be null, but it could be given this code. Usually throwable would be defined.
          }
        val description = Description.createSuiteDescription(testDescriptionName(suiteName, suiteClassName, testName))
        runNotifier.fireTestFailure(new Failure(description, throwableOrNull))
        runNotifier.fireTestFinished(description)

      case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
        runNotifier.fireTestFinished(Description.createSuiteDescription(testDescriptionName(suiteName, suiteClassName, testName)))

      case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 
        runNotifier.fireTestIgnored(Description.createSuiteDescription(testDescriptionName(suiteName, suiteClassName, testName)))

// TODO: I dont see TestCanceled here. Probably need to add it
      // Closest thing we can do with pending is report an ignored test
      case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>
        runNotifier.fireTestIgnored(Description.createSuiteDescription(testDescriptionName(suiteName, suiteClassName, testName)))

      case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 
        val throwableOrNull =
          throwable match {
            case Some(t) => t
            case None => null // Yuck. Not sure if the exception passed to new Failure can be null, but it could be given this code. Usually throwable would be defined.
          }
        val description = Description.createSuiteDescription(suiteDescriptionName(suiteName, suiteClassName))
        runNotifier.fireTestFailure(new Failure(description, throwableOrNull)) // Best we can do in JUnit, as far as I know
        runNotifier.fireTestFinished(description)

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
        val throwableOrNull =
          throwable match {
            case Some(t) => t
            case None => null // Yuck. Not sure if the exception passed to new Failure can be null, but it could be given this code. Usually throwable would be defined.
          }
        val possiblyEmptyMessage = Reporter.messageOrThrowablesDetailMessage(message, throwable)
        val description = Description.createSuiteDescription(Resources("runAborted") + " " + possiblyEmptyMessage)
        runNotifier.fireTestFailure(new Failure(description, throwableOrNull)) // Best we can do in JUnit, as far as I know
        runNotifier.fireTestFinished(description)

      case _ =>
    }
  }
}
