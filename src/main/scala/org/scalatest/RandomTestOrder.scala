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

/**
 * Trait that causes tests to be run in pseudo-random order.
 *
 * <p>
 * Although the tests are run in pseudo-random order, events will be fired in the &ldquo;normal&rdquo; order for the <code>Suite</code>
 * that mixes in this trait, as determined by <code>runTests</code>. 
 * </p>
 *
 * <p>
 * The purpose of this trait is to reduce the likelihood of unintentional order dependencies between tests
 * in the same test class.
 * </p>
 *
 * @author Chee Seng
 */
trait RandomTestOrder extends OneInstancePerTest { this: Suite =>

  private[scalatest] case class DeferredSuiteRun(suite: Suite with RandomTestOrder, testName: String, status: ScalaTestStatefulStatus)

  private val suiteRunQueue = new LinkedBlockingQueue[DeferredSuiteRun]

  /**
   * Modifies the behavior of <code>super.runTest</code> to facilitate pseudo-random order test execution.
   *
   * <p>
   * If <code>runTestInNewInstance</code> is <code>false</code>, this is the test-specific (distributed)
   * instance, so this trait's implementation of this method simply invokes <code>super.runTest</code>,
   * passing along the same <code>testName</code> and <code>args</code> object, delegating responsibility
   * for actually running the test to the super implementation. After <code>super.runTest</code> returns
   * (or completes abruptly by throwing an exception), it notifies <code>args.distributedTestSorter</code>
   * that it has completed running the test by invoking <code>completedTest</code> on it,
   * passing in the <code>testName</code>.
   * </p>
   *
   * <p>
   * If <code>runTestInNewInstance</code> is <code>true</code>, it notifies <code>args.distributedTestSorter</code>
   * that it is distributing the test by invoking <code>distributingTest</code> on it,
   * passing in the <code>testName</code>.  The test execution will be deferred to be run in pseudo-random order later.
   * </p>
   *
   * <p>
   * Note: this trait's implementation of this method is <code>final</code> to ensure that
   * any other desired <code>runTest</code> behavior is executed by the same thread that executes
   * the test. For example, if you were to mix in <code>BeforeAndAfter</code> after
   * <code>RandomTestOrder</code>, the <code>before</code> and <code>after</code> code would
   * be executed by the general instance on the main test thread, rather than by the test-specific
   * instance on the distributed thread. Marking this method <code>final</code> ensures that
   * traits like <code>BeforeAndAfter</code> can only be &lquot;super&rquot; to <code>RandomTestOrder</code>
   * and, therefore, that its <code>before</code> and <code>after</code> code will be run
   * by the same distributed thread that runs the test itself.
   * </p>
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   */
  final protected abstract override def runTest(testName: String, args: Args): Status = {

    if (args.runTestInNewInstance) {
      // Tell the TSR that the test is being distributed
      for (sorter <- args.distributedTestSorter)
        sorter.distributingTest(testName)

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

  /**
   * Construct a new instance of this <code>Suite</code>.
   *
   * <p>
   * This trait's implementation of <code>runTests</code> invokes this method to create
   * a new instance of this <code>Suite</code> for each test. This trait's implementation
   * of this method uses reflection to call <code>this.getClass.newInstance</code>. This
   * approach will succeed only if this <code>Suite</code>'s class has a public, no-arg
   * constructor. In most cases this is likely to be true, because to be instantiated
   * by ScalaTest's <code>Runner</code> a <code>Suite</code> needs a public, no-arg
   * constructor. However, this will not be true of any <code>Suite</code> defined as
   * an inner class of another class or trait, because every constructor of an inner
   * class type takes a reference to the enclosing instance. In such cases, and in
   * cases where a <code>Suite</code> class is explicitly defined without a public,
   * no-arg constructor, you will need to override this method to construct a new
   * instance of the <code>Suite</code> in some other way.
   * </p>
   *
   * <p>
   * Here's an example of how you could override <code>newInstance</code> to construct
   * a new instance of an inner class:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest._
   *
   * class Outer {
   *   class InnerSuite extends Suite with RandomTestOrder {
   *     def testOne() {}
   *     def testTwo() {}
   *     override def newInstance = new InnerSuite
   *   }
   * }
   * </pre>
   */
  override def newInstance: Suite with RandomTestOrder = this.getClass.newInstance.asInstanceOf[Suite with RandomTestOrder]

  /**
   * A maximum amount of time to wait for out-of-order events generated by running the tests
   * of this <code>Suite</code> in parallel while sorting the events back into a more
   * user-friendly, sequential order.
   *
   * <p>
   * The default implementation of this method returns the value specified via <code>-T</code> to
   * <a href="tools/Runner$.html"></code>Runner</code></a>, or 2 seconds, if no <code>-T</code> was supplied.
   * </p>
   *
   * @return a maximum amount of time to wait for events while resorting them into sequential order
   */
  protected def sortingTimeout: Span = Runner.testSortingReporterTimeout

  /**
   * Modifies the behavior of <code>super.run</code> to facilitate pseudo-random order test execution.
   *
   * <p>
   * If both <code>testName</code> and <code>args.distributedTestSorter</code> are defined,
   * this trait's implementation of this method will create a "test-specific reporter" whose <code>apply</code>
   * method will invoke the <code>apply</code> method of the <code>DistributedTestSorter</code>, which takes
   * a test name as well as the event. It will then invoke <code>super.run</code> passing along
   * the same <code>testName</code> and an <code>Args</code> object that is the same except with the
   * original reporter replaced by the test-specific reporter.
   * </p>
   *
   * <p>
   * If either <code>testName</code> or <code>args.distributedTestSorter</code> is empty, it will create <code>TestSortingReporter</code>
   * and override <code>args</code>'s <code>reporter</code> and <code>distributedTestSorter</code> with it.  It then call <code>super.run</code>
   * to delegate the run to super's implementation, and to collect all children suites in <code>suiteRunQueue</code>.  After <code>super.run</code>
   * completed, it then shuffle the order of the suites collected in <code>suiteRunQueue</code> and run them.
   * </p>
   *
   * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests and nested suites started by this method have completed, and whether or not a failure occurred.
   */
  abstract override def run(testName: Option[String], args: Args): Status = {
    (testName, args.distributedTestSorter) match {
      case (Some(name), Some(sorter)) =>
        super.run(testName, args.copy(reporter = createTestSpecificReporter(sorter, name)))
      case _ =>
        val testSortingReporter = new TestSortingReporter(suiteId, args.reporter, sortingTimeout, testNames.size, args.distributedSuiteSorter, System.err)
        val newArgs = args.copy(reporter = testSortingReporter, distributedTestSorter = Some(testSortingReporter))
        val status = super.run(testName, newArgs)
        // Random shuffle the deferred suite list, before executing them.
        Random.shuffle(suiteRunQueue.asScala.toList).map { case DeferredSuiteRun(suite, testName, statefulStatus) =>
          val status = suite.run(Some(testName), newArgs.copy(runTestInNewInstance = true))
          status.whenCompleted { result =>
            if (!result)
              statefulStatus.setFailed()
            statefulStatus.setCompleted()
          }
        }
        status
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
