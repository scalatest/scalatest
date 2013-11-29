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
import org.scalatest.events.Event
import org.scalatest.time.Span
import org.scalatest.tools.{TestSortingReporter, Runner}

trait RandomTestOrder extends SuiteMixin {

  this: Suite =>

  private[scalatest] case class DeferredSuiteRun(suite: Suite with RandomTestOrder, testName: String, status: ScalaTestStatefulStatus)

  private val suiteRunQueue = new LinkedBlockingQueue[DeferredSuiteRun]

  protected abstract override def runTest(testName: String, args: Args): Status = {

    if (args.runTestInNewInstance) {
      // Tell the TSR that the test is being distributed
      for (sorter <- args.distributedTestSorter)
        sorter.distributingTest(testName)

      // It will be oneInstance, testName, args.copy(reporter = ...)
      //distribute(new DistributedTestRunnerSuite(newInstance, testName, args), args.copy(tracker = args.tracker.nextTracker))
      // defer the suite execution
      val status = new ScalaTestStatefulStatus
      suiteRunQueue.put(DeferredSuiteRun(newInstance, testName, status))
      status
    }
    else {
      // In test-specific (distributed) instance, so just run the test. (RTINI was
      // removed by OIPT's implementation of runTests.)
      try {
        super.runTest(testName, args)
      }
      finally {
        // Tell the TSR that the distributed test has completed
        for (sorter <- args.distributedTestSorter)
          sorter.completedTest(testName)
      }
    }
  }

  protected abstract override def runTests(testName: Option[String], args: Args): Status = {
    val newArgs =
      if (args.runTestInNewInstance)
        args // This is the test-specific instance
      else {
        val testSortingReporter = new TestSortingReporter(suiteId, args.reporter, sortingTimeout, testNames.size, args.distributedSuiteSorter, System.err)
        args.copy(reporter = testSortingReporter, distributedTestSorter = Some(testSortingReporter))
      }

    if (newArgs.runTestInNewInstance) {
      if (testName.isEmpty)
        throw new IllegalArgumentException("args.runTestInNewInstance was true, but testName was not defined")
      // In test-specific instance, so run the test. (We are removing RTINI
      // so that runTest will realize it is in the test-specific instance.)
      runTest(testName.get, newArgs.copy(runTestInNewInstance = false))
    }
    else {
      super.runTests(testName, newArgs.copy(runTestInNewInstance = true))
      // Random shuffle the deferred suite list, before executing them.
      val statusList: List[Status] =
        Random.shuffle(suiteRunQueue.asScala.toList).map { case DeferredSuiteRun(suite, testName, statefulStatus) =>
          val status = suite.run(Some(testName), newArgs.copy(runTestInNewInstance = true))
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

  protected def sortingTimeout: Span = Runner.testSortingReporterTimeout

  abstract override def run(testName: Option[String], args: Args): Status = {
    (testName, args.distributedTestSorter) match {
      case (Some(name), Some(sorter)) =>
        super.run(testName, args.copy(reporter = createTestSpecificReporter(sorter, name)))
      case _ =>
        super.run(testName, args)
    }
  }

  private[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    class TestSpecificReporter(testSorter: DistributedTestSorter, testName: String) extends Reporter {
      def apply(event: Event) {
        testSorter.apply(testName, event)
      }
    }
    new TestSpecificReporter(testSorter, testName)
  }
}