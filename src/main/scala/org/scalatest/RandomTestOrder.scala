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

import scala.util.Random
import java.util.concurrent.LinkedBlockingQueue
import collection.JavaConverters._

trait RandomTestOrder extends SuiteMixin {

  this: Suite =>

  private[scalatest] case class DeferredSuiteRun(suite: Suite with RandomTestOrder, testName: String, status: ScalaTestStatefulStatus)

  //private val suiteList = new ListBuffer[DeferredSuiteRun]
  private val suiteRunQueue = new LinkedBlockingQueue[DeferredSuiteRun]

  protected abstract override def runTest(testName: String, args: Args): Status = {

    if (args.runTestInNewInstance) {
      // In initial instance, so create a new test-specific instance for this test and invoke run on it.
      val oneInstance = newInstance
      // defer the suite execution
      val status = new ScalaTestStatefulStatus
      suiteRunQueue.put(DeferredSuiteRun(oneInstance, testName, status))
      status
    }
    else {// Therefore, in test-specific instance, so run the test.
      super.runTest(testName, args)
    }
  }

  protected abstract override def runTests(testName: Option[String], args: Args): Status = {

    if (args.runTestInNewInstance) {
      if (testName.isEmpty)
        throw new IllegalArgumentException("args.runTestInNewInstance was true, but testName was not defined")
      // In test-specific instance, so run the test. (We are removing RTINI
      // so that runTest will realize it is in the test-specific instance.)
      runTest(testName.get, args.copy(runTestInNewInstance = false))
    }
    else {
      super.runTests(testName, args.copy(runTestInNewInstance = true))
      // Random shuffle the deferred suite list, before executing them.
      val statusList: List[Status] =
        Random.shuffle(suiteRunQueue.asScala.toList).map { case DeferredSuiteRun(suite, testName, statefulStatus) =>
          val status = suite.run(Some(testName), args.copy(runTestInNewInstance = true))
          status.whenCompleted { result =>
            if (!result)
              statefulStatus.setFailed()
            statefulStatus.setCompleted()
          }
          statefulStatus
        }
      new CompositeStatus(statusList.toSet)
    }
  }

  def newInstance: Suite with RandomTestOrder = this.getClass.newInstance.asInstanceOf[Suite with RandomTestOrder]
}