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
 * <li><code>DiscoveryStarting</code>
 * <li><code>DiscoveryCompleted</code>
 * <li><code>RunStarting</code>
 * <li><code>TestStarting</code>
 * <li><code>TestSucceeded</code>
 * <li><code>TestFailed</code>
 * <li><code>TestIgnored</code>
 * <li><code>TestPending</code>
 * <li><code>SuiteStarting</code>
 * <li><code>SuiteCompleted</code>
 * <li><code>SuiteAborted</code>
 * <li><code>InfoProvided</code>
 * <li><code>RunStopped</code>
 * <li><code>RunAborted</code>
 * <li><code>RunCompleted</code>
 * </ul>
 *
 * <p>
 * Reporters may be implemented such that they only present some of the reported events to the user. For example, you could
 * define a reporter class that doesn nothing in response to <code>SuiteStarting</code> events.
 * Such a class would always ignore <code>SuiteStarting</code> events.
 * </p>
 *
 * <p>
 * The term <em>test</em> as used in the <code>TestStarting</code>, <code>TestSucceeded</code>,
 * and <code>TestFailed</code> event names
 * is defined abstractly to enable a wide range of test implementations.
 * Trait <code>Suite</code> fires <code>TestStarting</code> to indicate it is about to invoke one
 * of its test methods, <code>TestSucceeded</code> to indicate a test method returned normally,
 * and <code>TestFailed</code> to indicate a test method completed abruptly with an exception.
 * Although the execution of a <code>Suite</code>'s test methods will likely be a common event
 * reported via the
 * <code>TestStarting</code>, <code>TestSucceeded</code>, and <code>TestFailed</code> methods, because
 * of the abstract definition of &#8220;test&#8221; used by the
 * the event classes, these events are not limited to this use. Information about any conceptual test
 * may be reported via the <code>TestStarting</code>, <code>TestSucceeded</code>, and
 * <code>TestFailed</code> events.
 *
 * <p>
 * Likewise, the term <em>suite</em> as used in the <code>SuiteStarting</code>, <code>SuiteAborted</code>,
 * and <code>SuiteCompleted</code> event names
 * is defined abstractly to enable a wide range of suite implementations.
 * Object <code>Runner</code> fires <code>SuiteStarting</code> to indicate it is about to invoke
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
 * report custom information passed as an event "payload." For more information on the latter
 * use case, see the <em>Extensibility</em> section of the <a href="events/Event.html"><code>Event</code> documentation</a>.
 * </p>
 *
 * <p>
 * <code>Reporter</code> classes can handle events in any manner, including doing nothing.
 * </p>
 *
 * @author Bill Venners
 */
trait Reporter /* extends (Event => Unit) */ {

  /**
   * Invoked to report an event that subclasses may wish to report in some way to the user.
   *
   * @param event the event being reported
   */
  def apply(event: Event)

  /*
   * Release any non-memory finite resources, such as file handles, held by this <code>Reporter</code>. Clients should
   * call this method when they no longer need the <code>Reporter</code>, before releasing the last reference
   * to the <code>Reporter</code>. After this method is invoked, the <code>Reporter</code> may be defunct,
   * and therefore not usable anymore. If the <code>Reporter</code> holds no resources, it may do nothing when
   * this method is invoked. This trait's implementation of this method does nothing, so that <code>Reporter</code>
   * subclasses that hold no non-memory, finite resources can simply inherit this trait's implementation.
  def dispose() = ()
   */
}

/**
 * TODO: Document on the 2.0 release notes that I dropped this deprecated implicit.
 * Companion object to Reporter that holds a deprecated implicit conversion.
 */
private[scalatest] object Reporter {

  /*
   * Converts a <code>Reporter</code> to a function type that prior to the ScalaTest 1.5 release the
   * <code>Reporter</code> extended.
   *
   * <p>
   * Prior to ScalaTest 1.5, <code>Reporter</code> extended function type <code>(Event) => Unit</code>.
   * This inheritance relationship was severed in 1.5 to make it possible to implement <code>Reporter</code>s in Java, a request by an IDE
   * vendor to isolate their ScalaTest integration from binary incompatibility between different Scala/ScalaTest releases.
   * To make a trait easily implementable in Java, it needs to have no concrete methods. <code>Reporter</code> itself does not declare
   * any concrete methods, but <code>Function1</code> does.
   * </p>
   *
   * <p>
   * This implicit conversion was added in ScalaTest 1.5 to avoid breaking any source code that was actually using
   * <code>Reporter</code> as an <code>(Event) => Unit</code> function. It is unlikely anyone was actually doing that, but if you were
   * and now get the deprecation warning, please email scalatest-users@googlegroups.com if you believe this implicit conversion should
   * be retained. If no one steps forward with a compelling justification, it will be removed in a future version of ScalaTest.
   * </p>
   */
/*
  @deprecated("See the documentation for Reporter.convertReporterToFunction for information")
  implicit def convertReporterToFunction(repo: Reporter): (Event) => Unit =
    (e: Event) => repo(e)
*/

  private[scalatest] def indentStackTrace(stackTrace: String, level: Int): String = {
    val indentation = if (level > 0) "  " * level else ""
    val withTabsZapped = stackTrace.replaceAll("\t", "  ")
    val withInitialIndent = indentation + withTabsZapped
    withInitialIndent.replaceAll("\n", "\n" + indentation) // I wonder if I need to worry about alternate line endings. Probably.
  }

  // In the unlikely event that a message is blank, use the throwable's detail message
  private[scalatest] def messageOrThrowablesDetailMessage(message: String, throwable: Option[Throwable]): String = {
    val trimmedMessage = message.trim
    if (!trimmedMessage.isEmpty)
      trimmedMessage
    else
      throwable match {
        case Some(t) => t.getMessage.trim
        case None => ""
      }
  }

  private[scalatest] def messageToPrint(resourceName: String, message: String, throwable: Option[Throwable], suiteName: Option[String],
    testName: Option[String]): String = {

    val arg =
      suiteName match {
        case Some(sn) =>
          testName match {
            case Some(tn) => sn + ": " + tn
            case None => sn
          }
        case None => ""
      }

    val msgToPrint = messageOrThrowablesDetailMessage(message, throwable)
    if (msgToPrint.isEmpty)
      Resources(resourceName + "NoMessage", arg)
    else
      if (resourceName == "runAborted")
        Resources(resourceName, msgToPrint)
      else
        Resources(resourceName, arg, msgToPrint)
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

