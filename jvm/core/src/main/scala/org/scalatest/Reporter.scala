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

import org.scalatest.events.Event

/**
 * Trait whose instances collect the results of a running
 * suite of tests and presents those results in some way to the user. Instances of this trait can
 * be called "report functions" or "reporters."
 *
 * <p>
 * Reporters receive test results via fifteen events.
 * Each event is fired to pass a particular kind of information to
 * the reporter. The events are:
 * </p>
 *
 * <ul>
 * <li><a href="events/DiscoveryStarting.html"><code>DiscoveryStarting</code></a></li>
 * <li><a href="events/DiscoveryCompleted.html"><code>DiscoveryCompleted</code></a></li>
 * <li><a href="events/RunStarting.html"><code>RunStarting</code></a></li>
 * <li><a href="events/RunStopped.html"><code>RunStopped</code></a></li>
 * <li><a href="events/RunAborted.html"><code>RunAborted</code></a></li>
 * <li><a href="events/RunCompleted.html"><code>RunCompleted</code></a></li>
 * <li><a href="events/ScopeOpened.html"><code>ScopeOpened</code></a></li>
 * <li><a href="events/ScopeClosed.html"><code>ScopeClosed</code></a></li>
 * <li><a href="events/ScopePending.html"><code>ScopePending</code></a></li>
 * <li><a href="events/TestStarting.html"><code>TestStarting</code></a></li>
 * <li><a href="events/TestSucceeded.html"><code>TestSucceeded</code></a></li>
 * <li><a href="events/TestFailed.html"><code>TestFailed</code></a></li>
 * <li><a href="events/TestCanceled.html"><code>TestCanceled</code></a></li>
 * <li><a href="events/TestIgnored.html"><code>TestIgnored</code></a></li>
 * <li><a href="events/TestPending.html"><code>TestPending</code></a></li>
 * <li><a href="events/SuiteStarting.html"><code>SuiteStarting</code></a></li>
 * <li><a href="events/SuiteCompleted.html"><code>SuiteCompleted</code></a></li>
 * <li><a href="events/SuiteAborted.html"><code>SuiteAborted</code></a></li>
 * <li><a href="events/InfoProvided.html"><code>InfoProvided</code></a></li>
 * <li><a href="events/MarkupProvided.html"><code>MarkupProvided</code></a></li>
 * <li><a href="events/AlertProvided.html"><code>AlertProvided</code></a></li>
 * <li><a href="events/NoteProvided.html"><code>NoteProvided</code></a></li>
 * </ul>
 *
 * <p>
 * Reporters may be implemented such that they only present some of the reported events to the user. For example, you could
 * define a reporter class that does nothing in response to <code>SuiteStarting</code> events.
 * Such a class would always ignore <code>SuiteStarting</code> events.
 * </p>
 *
 * <p>
 * The term <em>test</em> as used in the <code>TestStarting</code>, <code>TestSucceeded</code>,
 * and <code>TestFailed</code> event names
 * is defined abstractly to enable a wide range of test implementations.
 * ScalaTest's style traits (subclasse of trait <a href="Suite.html"><code>Suite</code></a>) fire
 * <code>TestStarting</code> to indicate they are about to invoke one
 * of their tests, <code>TestSucceeded</code> to indicate a test returned normally,
 * and <code>TestFailed</code> to indicate a test completed abruptly with an exception.
 * Although the execution of a <code>Suite</code> subclass's tests will likely be a common event
 * reported via the
 * <code>TestStarting</code>, <code>TestSucceeded</code>, and <code>TestFailed</code> events, because
 * of the abstract definition of &ldquo;test&rdquo; used by the
 * the event classes, these events are not limited to this use. Information about any conceptual test
 * may be reported via the <code>TestStarting</code>, <code>TestSucceeded</code>, and
 * <code>TestFailed</code> events.
 *
 * <p>
 * Likewise, the term <em>suite</em> as used in the <code>SuiteStarting</code>, <code>SuiteAborted</code>,
 * and <code>SuiteCompleted</code> event names
 * is defined abstractly to enable a wide range of suite implementations.
 * Object <a href="tools/Runner$.html"><code>Runner</code></a> fires <code>SuiteStarting</code> to indicate it is about to invoke
 * <code>run</code> on a
 * <code>Suite</code>, <code>SuiteCompleted</code> to indicate a <code>Suite</code>'s
 * <code>run</code> method returned normally,
 * and <code>SuiteAborted</code> to indicate a <code>Suite</code>'s <code>run</code>
 * method completed abruptly with an exception.
 * Similarly, class <code>Suite</code> fires <code>SuiteStarting</code> to indicate it is about to invoke
 * <code>run</code> on a
 * nested <code>Suite</code>, <code>SuiteCompleted</code> to indicate a nested <code>Suite</code>'s
 * <code>run</code> method returned normally,
 * and <code>SuiteAborted</code> to indicate a nested <code>Suite</code>'s <code>run</code>
 * method completed abruptly with an exception.
 * Although the execution of a <code>Suite</code>'s <code>run</code> method will likely be a
 * common event reported via the
 * <code>SuiteStarting</code>, <code>SuiteAborted</code>, and <code>SuiteCompleted</code> events, because
 * of the abstract definition of "suite" used by the
 * event classes, these events are not limited to this use. Information about any conceptual suite
 * may be reported via the <code>SuiteStarting</code>, <code>SuiteAborted</code>, and
 * <code>SuiteCompleted</code> events.
 *
 * <h2>Extensibility</h2>
 *
 * <p>
 * You can create classes that extend <code>Reporter</code> to report test results in custom ways, and to
 * report custom information passed as an event "payload."
 * <code>Reporter</code> classes can handle events in any manner, including doing nothing.
 * </p>
 *
 * @author Bill Venners
 */
trait Reporter {

  /**
   * Invoked to report an event that subclasses may wish to report in some way to the user.
   *
   * @param event the event being reported
   */
  def apply(event: Event): Unit
}

private[scalatest] object Reporter {

  private[scalatest] def indentStackTrace(stackTrace: String, level: Int): String = {
    val indentation = if (level > 0) "  " * level else ""
    val withTabsZapped = stackTrace.replaceAll("\t", "  ")
    val withInitialIndent = indentation + withTabsZapped
    withInitialIndent.replaceAll("\n", "\n" + indentation) // I wonder if I need to worry about alternate line endings. Probably.
  }

  // In the unlikely event that a message is blank, use the throwable's detail message
  def messageOrThrowablesDetailMessage(message: String, throwable: Option[Throwable]): String = {
    val trimmedMessage = message.trim
    if (!trimmedMessage.isEmpty)
      trimmedMessage
    else
      throwable match {
        case Some(t) => t.getMessage.trim
        case None => ""
      }
  }

  // TODO: Not a real problem, but if a DispatchReporter ever got itself in
  // its list of reporters, this would end up being an infinite loop. But
  // That first part, a DispatchReporter getting itself in there would be the real
  // bug.
  def propagateDispose(reporter: Reporter): Unit = {
    reporter match {
      // SKIP-SCALATESTJS,NATIVE-START
      case dispatchReporter: DispatchReporter => dispatchReporter.dispatchDisposeAndWaitUntilDone()
      // SKIP-SCALATESTJS,NATIVE-END
      case resourcefulReporter: ResourcefulReporter => resourcefulReporter.dispose()
      case _ =>
    }
  }
}

  /*
      case RunStarting(ordinal, testCount, formatter, payload, threadName, timeStamp) => runStarting(testCount)

      case TestStarting(ordinal, suiteName, suiteClassName, testName, formatter, rerunnable, payload, threadName, timeStamp) =>

      case TestSucceeded(ordinal, suiteName, suiteClassName, testName, duration, formatter, rerunnable, payload, threadName, timeStamp) => 

      case TestFailed(ordinal, message, suiteName, suiteClassName, testName, throwable, duration, formatter, rerunnable, payload, threadName, timeStamp) => 

      case TestIgnored(ordinal, suiteName, suiteClassName, testName, formatter, payload, threadName, timeStamp) => 

      case TestPending(ordinal, suiteName, suiteClassName, testName, formatter, payload, threadName, timeStamp) => 

      case SuiteStarting(ordinal, suiteName, suiteClassName, formatter, rerunnable, payload, threadName, timeStamp) =>

      case SuiteCompleted(ordinal, suiteName, suiteClassName, duration, formatter, rerunnable, payload, threadName, timeStamp) => 

      case SuiteAborted(ordinal, message, suiteName, suiteClassName, throwable, duration, formatter, rerunnable, payload, threadName, timeStamp) => 

      case InfoProvided(ordinal, message, nameInfo, throwable, formatter, payload, threadName, timeStamp) => {

      case RunStopped(ordinal, duration, summary, formatter, payload, threadName, timeStamp) => runStopped()

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, payload, threadName, timeStamp) => 

      case RunCompleted(ordinal, duration, summary, formatter, payload, threadName, timeStamp) => runCompleted()
*/

