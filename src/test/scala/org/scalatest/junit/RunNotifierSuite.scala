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

// There's no way to really pass along a SuiteStarting or SuiteCompleted
// event. They have a dumb comment to "Do not invoke" fireTestRunStarted
// and fireTestRunFinished, so I think they must be doing that themselves.
// This means we don't have a way to really forward RunStarting and
// RunCompleted reports either. But RunAborted and SuiteAborted events should be sent
// out the door somehow, so we report them with yet another fireTestFailure.
class RunNotifierSuite extends FunSuite {

  val ordinal = new Ordinal(99)

  test("report(TestStarting) generates a fireTestStarted invocation") {

    val runNotifier =
      new RunNotifier {
        var fireTestStartedInvocationCount = 0
        var passedDesc: Option[Description] = None
        override def fireTestStarted(description: Description) {
          fireTestStartedInvocationCount += 1
          passedDesc = Some(description)
        }
      }

    val reporter = new RunNotifierReporter(runNotifier)
    reporter(TestStarting(new Ordinal(99), "suite name", "suite ID", Some("suite.class.name"), "some test name", "some test name"))
    assert(runNotifier.fireTestStartedInvocationCount === 1)
    assert(runNotifier.passedDesc.get.getDisplayName === "some test name(suite.class.name)")

    reporter(TestStarting(new Ordinal(99), "suiteName", "suite ID", Some("suite.class.name"), "someTestName", "someTestName"))
    assert(runNotifier.fireTestStartedInvocationCount === 2)
    assert(runNotifier.passedDesc.get.getDisplayName === "someTestName(suite.class.name)")

    reporter(TestStarting(ordinal, "suiteName", "suite ID", None, "theTestName", "theTestName"))
    assert(runNotifier.passedDesc.get.getDisplayName === "theTestName(suiteName)")
  }

  test("report(TestFailed) generates a fireTestFailure invocation") {

    val runNotifier =
      new RunNotifier {
        var methodInvocationCount = 0
        var passed: Option[Failure] = None
        override def fireTestFailure(failure: Failure) {
          methodInvocationCount += 1
          passed = Some(failure)
        }
      }

    val reporter = new RunNotifierReporter(runNotifier)
    val exception = new IllegalArgumentException

    reporter(TestFailed(ordinal, "No msg", "SuiteClassName", "suite ID", Some("fully.qualified.SuiteClassName"), "theTestName", "theTestName", Vector.empty, Some(exception)))
    assert(runNotifier.passed.get.getDescription.getDisplayName === "theTestName(fully.qualified.SuiteClassName)")
    reporter(TestFailed(ordinal, "No msg", "SuiteClassName", "suite ID", None, "theTestName", "theTestName", Vector.empty, Some(exception)))
    assert(runNotifier.passed.get.getDescription.getDisplayName === "theTestName(SuiteClassName)")
  }

  test("report(TestSucceeded) generates a fireTestFinished invocation") {

    val runNotifier =
      new RunNotifier {
        var methodInvocationCount = 0
        var passed: Option[Description] = None
        override def fireTestFinished(description: Description) {
          methodInvocationCount += 1
          passed = Some(description)
        }
      }

    val reporter = new RunNotifierReporter(runNotifier)
    reporter(TestSucceeded(ordinal, "SuiteClassName", "suite ID", Some("fully.qualified.SuiteClassName"), "theTestName", "theTestName", Vector.empty))
    assert(runNotifier.passed.get.getDisplayName === "theTestName(fully.qualified.SuiteClassName)")
    reporter(TestSucceeded(ordinal, "SuiteClassName", "suite ID", None, "theTestName", "theTestName", Vector.empty))
    assert(runNotifier.passed.get.getDisplayName === "theTestName(SuiteClassName)")
  }

  test("report(TestIgnored) generates a fireTestIgnored invocation") {

    val runNotifier =
      new RunNotifier {
        var methodInvocationCount = 0
        var passed: Option[Description] = None
        override def fireTestIgnored(description: Description) {
          methodInvocationCount += 1
          passed = Some(description)
        }
      }

    val reporter = new RunNotifierReporter(runNotifier)
    reporter(TestIgnored(new Ordinal(99), "suite name", "suite ID", Some("suite class name"), "some test name", "some test name"))
    assert(runNotifier.methodInvocationCount === 1)
    assert(runNotifier.passed.get.getDisplayName === "some test name(suite class name)")

    reporter(TestIgnored(new Ordinal(99), "suiteName2", "suite ID", Some("suite.class.name"), "testName", "testName"))
    assert(runNotifier.methodInvocationCount === 2)
    assert(runNotifier.passed.get.getDisplayName === "testName(suite.class.name)")

    reporter(TestIgnored(ordinal, "SuiteClassName", "suite ID", Some("fully.qualified.SuiteClassName"), "theTestName", "theTestName"))
    assert(runNotifier.passed.get.getDisplayName === "theTestName(fully.qualified.SuiteClassName)")
    reporter(TestIgnored(ordinal, "SuiteClassName", "suite ID", None, "theTestName", "theTestName"))
    assert(runNotifier.passed.get.getDisplayName === "theTestName(SuiteClassName)")
  }

  // fireTestFailure is the best we could do given the RunNotifier interface
  test("report(SuiteAborted) generates a fireTestFailure invocation") {

    val runNotifier =
      new RunNotifier {
        var methodInvocationCount = 0
        var passed: Option[Failure] = None
        override def fireTestFailure(failure: Failure) {
          methodInvocationCount += 1
          passed = Some(failure)
        }
      }

    val reporter = new RunNotifierReporter(runNotifier)
    val exception = new IllegalArgumentException
    val otherException = new NullPointerException

    reporter(SuiteAborted(ordinal, "some message", "SuiteClassName", "suite ID", Some("fully.qualified.SuiteClassName"), Some(exception)))
    assert(runNotifier.methodInvocationCount === 1)
    assert(runNotifier.passed.get.getException === exception)
    assert(runNotifier.passed.get.getDescription.getDisplayName === "fully.qualified.SuiteClassName")

    reporter(SuiteAborted(ordinal, "a different message", "SuiteClassName", "suite ID", Some("fully.qualified.SuiteClassName"), Some(otherException)))
    assert(runNotifier.methodInvocationCount === 2)
    assert(runNotifier.passed.get.getException === otherException)
    assert(runNotifier.passed.get.getDescription.getDisplayName === "fully.qualified.SuiteClassName")

    reporter(SuiteAborted(ordinal, "No msg", "SuiteClassName", "suite ID", Some("fully.qualified.SuiteClassName"), Some(exception)))
    assert(runNotifier.passed.get.getDescription.getDisplayName === "fully.qualified.SuiteClassName")
    reporter(SuiteAborted(ordinal, "No msg", "SuiteClassName", "suite ID", None, Some(exception)))
    assert(runNotifier.passed.get.getDescription.getDisplayName === "SuiteClassName")
  }

  // fireTestFailure is the best we could do given the RunNotifier interface
  test("report(RunAborted) generates a fireTestFailure invocation") {

    val runNotifier =
      new RunNotifier {
        var methodInvocationCount = 0
        var passed: Option[Failure] = None
        override def fireTestFailure(failure: Failure) {
          methodInvocationCount += 1
          passed = Some(failure)
        }
      }

    val reporter = new RunNotifierReporter(runNotifier)
    val exception = new IllegalArgumentException
    val otherException = new NullPointerException

    reporter(RunAborted(ordinal, "some message", Some(exception)))
    assert(runNotifier.methodInvocationCount === 1)
    assert(runNotifier.passed.get.getException === exception)
    assert(runNotifier.passed.get.getDescription.getDisplayName === "*** RUN ABORTED *** some message")

    reporter(RunAborted(ordinal, "a different message", Some(otherException)))
    assert(runNotifier.methodInvocationCount === 2)
    assert(runNotifier.passed.get.getException === otherException)
    assert(runNotifier.passed.get.getDescription.getDisplayName === "*** RUN ABORTED *** a different message")
  }
}

