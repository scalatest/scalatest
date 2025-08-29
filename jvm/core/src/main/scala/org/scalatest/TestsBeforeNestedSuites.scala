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
package org.scalatest

import org.scalatest.tools.Utils.wrapReporterIfNecessary
import org.scalactic.Requirements.requireNonNull
import org.scalatest.events.{InfoProvided, NameInfo}

/**
 * `SuiteMixin` trait that overrides `run` to execute tests ''before'' nested suites.
 *
 * The default behavior of `run` executes nested suites before running
 * tests declared in this suite. This trait overrides `run` and executes
 * this suites's tests first, then executes its nested suites.
 */
trait TestsBeforeNestedSuites extends SuiteMixin { thisSuite: Suite =>

  /**
   * Runs this suite's tests (if any), then runs this suite's nested suites (if any).
   */
  override def run(testName: Option[String], args: Args): Status = {

    requireNonNull(testName, args)

    import args._

    val originalThreadName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(SuiteHelpers.augmentedThreadName(originalThreadName, suiteName))

      val report = wrapReporterIfNecessary(thisSuite, reporter)
      val newArgs = args.copy(reporter = report)

      val testsStatus = runTests(testName, newArgs)

      val nestedSuitesStatus = 
        testName match {
          case None => runNestedSuites(newArgs)
          case Some(_) => SucceededStatus
        }

      if (stopper.stopRequested) {
        val rawString = Resources.executeStopping
        report(InfoProvided(tracker.nextOrdinal(), rawString, Some(NameInfo(thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), testName))))
      }
      new CompositeStatus(Set(nestedSuitesStatus, testsStatus))
    }
    finally Thread.currentThread.setName(originalThreadName)
  }
}
