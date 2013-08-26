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
package org.scalatest.tools

import org.scalatest._
import java.lang.reflect.Constructor
import java.lang.reflect.Modifier
import org.scalatest.events._
import Suite.formatterForSuiteStarting
import Suite.formatterForSuiteCompleted
import Suite.formatterForSuiteAborted
import org.scalatest.exceptions.NotAllowedException

private[scalatest] class SuiteRunner(suite: Suite, args: Args, status: ScalaTestStatefulStatus) extends Runnable {

  private val stopRequested = args.stopper

  def run() {

    if (!stopRequested()) {
      // Create a Rerunner if the Suite has a no-arg constructor
      val hasPublicNoArgConstructor: Boolean =
        try {
          val constructor: Constructor[_] = suite.getClass.getConstructor(Array[java.lang.Class[_]](): _*)
          Modifier.isPublic(constructor.getModifiers())
        }
        catch {
          case nsme: NoSuchMethodException => false
        }
  
      val rawString = Resources("suiteExecutionStarting")
      val formatter = formatterForSuiteStarting(suite)
      val dispatch = args.reporter
      val tracker = args.tracker

      val suiteStartTime = System.currentTimeMillis

      if (!suite.isInstanceOf[DistributedTestRunnerSuite])
        dispatch(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), formatter, Some(TopOfClass(suite.getClass.getName)), suite.rerunner))
        
      try {
        val runStatus = suite.run(None, args)

        // Must wait until runStatus completed before dispatching SuiteCompleted, because if parallel test execution is mixed in,
        // the main thread will return before the tests are done. And the HTMLReporter uses SuiteCompleted to
        // determine when to write the page for that suite.
        runStatus.whenCompleted { succeeded =>
          if (!succeeded)
            status.setFailed()
          val formatter = formatterForSuiteCompleted(suite)
          val duration = System.currentTimeMillis - suiteStartTime
          try {
            if (!suite.isInstanceOf[DistributedTestRunnerSuite])
              dispatch(SuiteCompleted(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), Some(duration), formatter, Some(TopOfClass(suite.getClass.getName)), suite.rerunner))
          }
          finally status.setCompleted()
        }
      }
      catch {
        case e: NotAllowedException =>
          val formatter = Suite.formatterForSuiteAborted(suite, e.getMessage)
          val duration = System.currentTimeMillis - suiteStartTime
          dispatch(SuiteAborted(tracker.nextOrdinal(), e.getMessage, suite.suiteName, suite.suiteId, Some(suite.getClass.getName), Some(e), Some(duration), formatter, Some(SeeStackDepthException), suite.rerunner))
          status.setFailed()
          status.setCompleted()
        case e: RuntimeException => { // Do fire SuiteAborted even if a DistributedTestRunnerSuite 
          val eMessage = e.getMessage
          val rawString3 = 
            if (eMessage != null && eMessage.length > 0)
                Resources("executeExceptionWithMessage", eMessage)
              else
                Resources("executeException")
          val formatter3 = formatterForSuiteAborted(suite, rawString3)

          val duration = System.currentTimeMillis - suiteStartTime
          dispatch(SuiteAborted(tracker.nextOrdinal(), rawString3, suite.suiteName, suite.suiteId, Some(suite.getClass.getName), Some(e), Some(duration), formatter3, Some(SeeStackDepthException), suite.rerunner))
          status.setFailed()
          status.setCompleted()
          
        }
        case e: Throwable => 
          status.setFailed()
          status.setCompleted()
          throw e
      }
    }
  }
}
