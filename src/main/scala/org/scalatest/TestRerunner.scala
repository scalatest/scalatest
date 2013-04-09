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

import java.lang.reflect.Method
import org.scalatest.events._

/**
 * A rerunner for test methods.
 *
 * @author Bill Venners
 */
private[scalatest] class TestRerunner(suiteClassName: String, testName: String) extends Rerunner with java.io.Serializable {

  if (suiteClassName == null || testName == null)
    throw new NullPointerException

  // [bv: I wasn't sure if I need to say override here.]
  def apply(report: Reporter, stopper: Stopper, filter: Filter, configMap: ConfigMap,
            distributor: Option[Distributor], tracker: Tracker, loader: ClassLoader) {

    val runStartTime = System.currentTimeMillis

    try {
      val suiteClass = loader.loadClass(suiteClassName)
      val suite = suiteClass.newInstance.asInstanceOf[Suite]

      report(RunStarting(tracker.nextOrdinal(), 1, configMap))

      // TODO: I Had to pass Set.empty for chosenStyles for now. Check this later.
      suite.run(Some(testName), Args(report, stopper, filter, configMap, distributor, tracker, Set.empty))

      val duration = System.currentTimeMillis - runStartTime
      report(RunCompleted(tracker.nextOrdinal(), Some(duration)))
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
      case e: NoSuchMethodException => {
        val duration = System.currentTimeMillis - runStartTime
        report(RunAborted(tracker.nextOrdinal(), Resources("cannotFindMethod", e.getMessage), Some(e), Some(duration)))
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
