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

import sbt.testing.{Status => SbtStatus, _}
import org.scalatest.Reporter

private class SbtReporter(suiteId: String, fullyQualifiedName: String, fingerprint: Fingerprint, eventHandler: EventHandler, report: Reporter) extends Reporter {

  import org.scalatest.events._

  private def getTestSelector(eventSuiteId: String, testName: String) = {
    if (suiteId == eventSuiteId)
      new TestSelector(testName)
    else
      new NestedTestSelector(eventSuiteId, testName)
  }

  private def getSuiteSelector(eventSuiteId: String) = {
    if (suiteId == eventSuiteId)
      new SuiteSelector
    else
      new NestedSuiteSelector(eventSuiteId)
  }

  private def getOptionalThrowable(throwable: Option[Throwable]): OptionalThrowable =
    if (throwable.isDefined) {
      // throwing the original throwable causes the program to crash
      new OptionalThrowable(throwable.get)
    } else {
      new OptionalThrowable()
    }

  override def apply(event: Event): Unit = {
    report(event)
    event match {
      // the results of running an actual test
      case t: TestPending =>
        eventHandler.handle(new ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Pending, new OptionalThrowable, t.duration.getOrElse(0)))
      case t: TestFailed =>
        eventHandler.handle(new ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Failure, getOptionalThrowable(t.throwable), t.duration.getOrElse(0)))
      case t: TestSucceeded =>
        eventHandler.handle(new ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Success, new OptionalThrowable, t.duration.getOrElse(0)))
      case t: TestIgnored =>
        eventHandler.handle(new ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Ignored, new OptionalThrowable, -1))
      case t: TestCanceled =>
        eventHandler.handle(new ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Canceled, new OptionalThrowable, t.duration.getOrElse(0)))
      case t: SuiteAborted =>
        eventHandler.handle(new ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getSuiteSelector(t.suiteId), SbtStatus.Error, getOptionalThrowable(t.throwable), t.duration.getOrElse(0)))
      case _ =>
    }
  }
}