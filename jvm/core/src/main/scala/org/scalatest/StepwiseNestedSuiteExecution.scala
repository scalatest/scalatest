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
package org.scalatest

import org.scalactic.Requirements._
import org.scalatest.events._
import Suite.formatterForSuiteAborted
import Suite.formatterForSuiteCompleted
import Suite.formatterForSuiteStarting
import Suite.getSuiteClassName
import org.scalatest.tools.Utils.wrapReporterIfNecessary
import collection.mutable.ListBuffer

/**
 * Trait that causes the nested suites of any suite it is mixed into to be run sequentially even if
 * a <a href="Distributor.html"><code>Distributor</code></a> is passed to <code>runNestedSuites</code>. This trait overrides the 
 * <code>runNestedSuites</code> method and fowards every parameter passed to it to a superclass invocation
 * of <code>runNestedSuites</code>, and make sure the nested suites are run and completed one after one in order.
 * Mix in this trait into any suite whose nested suites need to be run sequentially even with the rest of the
 * run is being executed concurrently.
 */
trait StepwiseNestedSuiteExecution extends SuiteMixin { thisSuite: Suite =>

  /**
   * This trait's implementation of <code>runNestedSuites</code>s invokes <code>runNestedSuites</code> on <code>super</code>,
   * passing in <code>None</code> for the <code>Distributor</code>.
   *
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all nested suites started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullArgumentException if any passed parameter is <code>null</code>.
   */
  abstract override protected def runNestedSuites(args: Args): Status = {

    requireNonNull(args)

    import args._

    val report = wrapReporterIfNecessary(thisSuite, reporter)

    def callExecuteOnSuite(nestedSuite: Suite): Status = {

      if (!stopper.stopRequested) {
        val rawString = Resources.suiteExecutionStarting
        val formatter = formatterForSuiteStarting(nestedSuite)
        val suiteClassName = getSuiteClassName(nestedSuite)

        val suiteStartTime = System.currentTimeMillis

        report(SuiteStarting(tracker.nextOrdinal(), nestedSuite.suiteName, nestedSuite.suiteId, Some(suiteClassName), formatter, Some(TopOfClass(nestedSuite.getClass.getName)), nestedSuite.rerunner))

        try {
          // Same thread, so OK to send same tracker
          val status = nestedSuite.run(None, Args(report, stopper, filter, configMap, distributor, tracker))

          val rawString = Resources.suiteCompletedNormally
          val formatter = formatterForSuiteCompleted(nestedSuite)

          distributor match {
            case Some(_) => 
              status.withAfterEffect {
                val duration = System.currentTimeMillis - suiteStartTime
                report(SuiteCompleted(tracker.nextOrdinal(), nestedSuite.suiteName, nestedSuite.suiteId, Some(suiteClassName), Some(duration), formatter, Some(TopOfClass(nestedSuite.getClass.getName)), nestedSuite.rerunner))
              }

            case None => 
              val duration = System.currentTimeMillis - suiteStartTime
              report(SuiteCompleted(tracker.nextOrdinal(), nestedSuite.suiteName, nestedSuite.suiteId, Some(suiteClassName), Some(duration), formatter, Some(TopOfClass(nestedSuite.getClass.getName)), nestedSuite.rerunner))
          }
          status
        }
        catch {       
          case e: RuntimeException => {
            val eMessage = e.getMessage
            val rawString = 
              if (eMessage != null && eMessage.length > 0)
                Resources.executeExceptionWithMessage(eMessage)
              else
                Resources.executeException
            val formatter = formatterForSuiteAborted(nestedSuite, rawString)

            val duration = System.currentTimeMillis - suiteStartTime
            report(SuiteAborted(tracker.nextOrdinal(), rawString, nestedSuite.suiteName, nestedSuite.suiteId, Some(suiteClassName), Some(e), Some(duration), formatter, Some(SeeStackDepthException), nestedSuite.rerunner))
            FailedStatus
          }
        }
      }
      else
        FailedStatus
    }
    
    val statusBuffer = new ListBuffer[Status]()
    if (!filter.excludeNestedSuites) {
      for (nestedSuite <- nestedSuites) {
        if (!stopper.stopRequested) {
          val st = callExecuteOnSuite(nestedSuite)
          // The distributor is being passed down with stepwise execution,
          // so it may run in parallel. Make sure all is done before moving on.
          // SKIP-SCALATESTJS,NATIVE-START
          st.waitUntilCompleted() // TODO, make this async
          // SKIP-SCALATESTJS,NATIVE-END
          statusBuffer += st
        }
      }
    }
    new CompositeStatus(Set.empty ++ statusBuffer)
  }
}
