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

import org.scalatest.Suite.checkForPublicNoArgConstructor

import org.scalatest.events._
import Suite.formatterForSuiteStarting
import Suite.formatterForSuiteCompleted
import Suite.formatterForSuiteAborted

/**
 * A Rerunner for Suites.
 *
 * @author Bill Venners
 */
private[scalatest] class SuiteRerunner(suiteClassName: String) extends Rerunner with java.io.Serializable {

  if (suiteClassName == null)
    throw new NullPointerException

  def apply(report: Reporter, stopRequested: Stopper, filter: Filter,
            configMap: ConfigMap, distributor: Option[Distributor], tracker: Tracker, loader: ClassLoader) {

    val tagsToInclude =
      filter.tagsToInclude match {
        case None => Set[String]()
        case Some(tti) => tti
      }
    val tagsToExclude = filter.tagsToExclude

    val runStartTime = System.currentTimeMillis

    try {
      val suiteClass = loader.loadClass(suiteClassName)
      val suite = suiteClass.newInstance().asInstanceOf[Suite]
      val expectedTestCount = suite.expectedTestCount(filter)

      // Create a Rerunner if the Suite has a public no-arg constructor
      val rerunnable =
        if (Suite.checkForPublicNoArgConstructor(suite.getClass))
          Some(new SuiteRerunner(suite.getClass.getName))
        else
          None

      report(RunStarting(tracker.nextOrdinal(), expectedTestCount, configMap))

      val suiteStartTime = System.currentTimeMillis
      try {

        val rawString = Resources("suiteExecutionStarting")
        val formatter = formatterForSuiteStarting(suite)

        report(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), formatter, Some(TopOfClass(suite.getClass.getName)), suite.rerunner))
        // TODO: I had to pass Set.empty for chosenStyles now. Fix this later.
        suite.run(None, Args(report, stopRequested, filter, configMap, distributor, tracker, Set.empty))

        val rawString2 = Resources("suiteCompletedNormally")
        val formatter2 = formatterForSuiteCompleted(suite)
        val duration = System.currentTimeMillis - suiteStartTime

        report(SuiteCompleted(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), Some(duration), formatter2, Some(TopOfClass(suite.getClass.getName)), suite.rerunner))
      }
      catch {
        case e: RuntimeException => {
          val eMessage = e.getMessage
          val rawString3 = 
            if (eMessage != null && eMessage.length > 0)
              Resources("executeExceptionWithMessage", eMessage)
            else
              Resources("executeException")
          val formatter3 = formatterForSuiteAborted(suite, rawString3)

          val duration = System.currentTimeMillis - suiteStartTime
          report(SuiteAborted(tracker.nextOrdinal(), rawString3, suite.suiteName, suite.suiteId, Some(suite.getClass.getName), Some(e), Some(duration), formatter3, Some(SeeStackDepthException), suite.rerunner))
        }
      }

      val duration = System.currentTimeMillis - runStartTime
      if (stopRequested()) {
        report(RunStopped(tracker.nextOrdinal(), Some(duration)))
      }
      else {
        report(RunCompleted(tracker.nextOrdinal(), Some(duration)))
      }
    }
    catch {
      case e: ClassNotFoundException => {
        val duration = System.currentTimeMillis - runStartTime
        report(RunAborted(tracker.nextOrdinal(), Resources("cannotLoadSuite", e.getMessage), Some(e), Some(duration)))
      }
      case e: InstantiationException => {
        val duration = System.currentTimeMillis - runStartTime
        report(RunAborted(tracker.nextOrdinal(), Resources("cannotInstantiateSuite", e.getMessage), Some(e), Some(duration)))
      }
      case e: IllegalAccessException => {
        val duration = System.currentTimeMillis - runStartTime
        report(RunAborted(tracker.nextOrdinal(), Resources("cannotInstantiateSuite", e.getMessage), Some(e), Some(duration)))
      }
      case e: SecurityException => {
        val duration = System.currentTimeMillis - runStartTime
        report(RunAborted(tracker.nextOrdinal(), Resources("securityWhenRerruning", e.getMessage), Some(e), Some(duration)))
      }
      case e: NoClassDefFoundError => {
        // Suggest the problem might be a bad runpath
        // Maybe even print out the current runpath
        val duration = System.currentTimeMillis - runStartTime
        report(RunAborted(tracker.nextOrdinal(), Resources("cannotLoadClass", e.getMessage), Some(e), Some(duration)))
      }
      case e: Throwable => {
        val duration = System.currentTimeMillis - runStartTime
        report(RunAborted(tracker.nextOrdinal(), Resources.bigProblems(e), Some(e), Some(duration)))
      }
    }
  }
}
