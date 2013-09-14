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

import org.scalatest.tools.SuiteRunner
import java.util.concurrent.CountDownLatch
import scala.collection.GenSet
import java.util.concurrent.ConcurrentLinkedQueue
import collection.JavaConverters._
import java.io.Serializable

/**
 * The result status of running a test or a suite.
 *
 * <p>
 * This trait is the return type of the "run" lifecycle methods of trait <a href="Suite.html#lifecycle-methods"><code>Suite</code></a>: <code>run</code>, <code>runNestedSuites</code>, 
 * <code>runTests</code>, and <code>runTest</code>. It can be used to determine whether a test or suite has completed, and if completed,
 * whether it succeeded or failed. The main use case for this trait in ScalaTest is to enable <a href="BeforeAndAfterAll.html"><code>BeforeAndAfterAll</code></a>'s <code>afterAll</code>
 * method to wait until all relevant tests and nested suites have completed before performing the "after all" code, even if those tests are
 * nested suites are run in parallel.
 * </p>
 * 
 * @author cheeseng
 */
trait Status {

  /**
   * Blocking call that waits until completion, then returns returns <code>true</code> if no tests failed and no suites aborted, else returns <code>false</code>.
   * 
   * <p>
   * This only reports <code>false</code> if there was a failed test or aborted suite in the context of the "run" lifecycle method it was returned from. For example,
   * if you call <code>succeeds</code> on the return <code>Status</code> of <code>runTest</code>, it returns (after that test has completed) <code>true</code> if the
   * test whose name was passed to <code>runTest</code> succeeded, <code>false</code> if that test failed (or the suite aborts). If you call
   * <code>succeeds</code> on the return value of <code>runTests</code>, by contrast, it returns (after the suite's tests have completed) <code>true</code> only
   * if all tests in the suite succeeded. If any test in the suite fails (or the whole suite aborts), the <code>succeeds</code> call will return <code>false</code>. 
   * The <code>Status</code> returned from <code>runNestedSuites</code> will return true only if all tests in all nested suites (and their nested suites, etc.) fired
   * off by that <code>runNestedSuites</code> call succeed and no suites abort. 
   * Simlarly, the <code>Status</code> returned from <code>run</code> will return true only if all tests in all nested suites (and their nested suites, etc.) fired
   * off by that <code>run</code> call succeed and no suites abort. 
   * </p>
   *
   * @return <code>true</code> if no tests failed and no suites aborted, <code>false</code> otherwise
   */
  def succeeds(): Boolean

  /**
   * Non-blocking call that indicates whether the all the tests or nested suites fired off by the run method that returned the <code>Status</code> have completed.
   * Because this is non-blocking, you can use this to poll the completion status.
   * 
   * @return <code>true</code> if the test or suite run is already completed, <code>false</code> otherwise.
   */
  def isCompleted: Boolean

  /**
   * Blocking call that returns only after the underlying test or suite is completed.
   */
  def waitUntilCompleted()

  /**
   * Registers the passed function to be executed when this status completes.
   *
   * <p>
   * You may register multiple functions, which on completion will be executed in an undefined
   * order.
   * </p>
   */
  def whenCompleted(f: Boolean => Unit)
}

/**
 * Singleton status that represents an already completed run with no tests failed and no suites aborted.
 *
 * <p>
 * Note: the difference between this <code>SucceededStatus</code> object and the similarly named <a href="Succeeded$.html"><code>Succeeded</code></a>
 * object is that the <code>Succeeded</code> object indicates one test succeeded, whereas this <code>SucceededStatus</code> object indicates the absence
 * of any failed tests or aborted suites during a run. Both are used as the result type of <a href="Suite.html#lifecycle-methods"><code>Suite</code></a> lifecycle methods, but <code>Succeeded</code>
 * is a possible result of <code>withFixture</code>, whereas <code>SucceededStatus</code> is a possible result of <code>run</code>, <code>runNestedSuites</code>,
 * <code>runTests</code>, or <code>runTest</code>. In short, <code>Succeeded</code> is always just about one test, whereas <code>SucceededStatus</code> could be
 * about something larger: multiple tests or an entire suite.
 * </p>
 */
object SucceededStatus extends Status with Serializable {

  /**
   * Always returns <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def succeeds() = true

  /**
   * Always returns <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def isCompleted = true

  /**
   * Always returns immediately.
   */
  def waitUntilCompleted() {}

  /**
   * Executes the passed function immediately on the calling thread.
   */
  def whenCompleted(f: Boolean => Unit) { f(true) }
}

/**
 * Singleton status that represents an already completed run with at least one failed test or aborted suite.
 *
 * <p>
 * Note: the difference between this <code>FailedStatus</code> object and the similarly named <a href="Failed.html"><code>Failed</code></a>
 * class is that a <code>Failed</code> instance indicates one test failed, whereas this <code>FailedStatus</code> object indicates either one or more tests failed
 * and/or one or more suites aborted during a run. Both are used as the result type of <code>Suite</code> lifecycle methods, but <code>Failed</code>
 * is a possible result of <code>withFixture</code>, whereas <code>FailedStatus</code> is a possible result of <code>run</code>, <code>runNestedSuites</code>,
 * <code>runTests</code>, or <code>runTest</code>. In short, <code>Failed</code> is always just about one test, whereas <code>FailedStatus</code> could be
 * about something larger: multiple tests or an entire suite.
 * </p>
 */
object FailedStatus extends Status with Serializable {

  /**
   * Always returns <code>false</code>.
   * 
   * @return <code>true</code>
   */
  def succeeds() = false

  /**
   * Always returns <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def isCompleted = true

  /**
   * Always returns immediately.
   */
  def waitUntilCompleted() {}

  /**
   * Executes the passed function immediately on the calling thread.
   */
  def whenCompleted(f: Boolean => Unit) { f(false) }
}

// Used internally in ScalaTest. We don't use the StatefulStatus, because
// then user code could pattern match on it and then access the setCompleted
// and setFailed methods. We wouldn't want that.
private[scalatest] final class ScalaTestStatefulStatus extends Status with Serializable {

  @transient private final val latch = new CountDownLatch(1)

  @volatile private var succeeded = true

  private final val queue = new ConcurrentLinkedQueue[Boolean => Unit]

  def succeeds() = {
    waitUntilCompleted()
    succeeded
  }

  def isCompleted = latch.getCount == 0L

  def waitUntilCompleted() {
    latch.await()
  }

  def setFailed() {
    if (isCompleted)
      throw new IllegalStateException("status is already completed")
    succeeded = false
  }

  def setCompleted() {
    synchronized {
      latch.countDown()
    }
    for (f <- queue.iterator.asScala)
      f(succeeded)
  }

  def whenCompleted(f: Boolean => Unit) {
    var executeLocally = false
    synchronized {
      if (!isCompleted)
        queue.add(f)
      else
        executeLocally = true
    }
    if (executeLocally)
      f(succeeded)
  }
}

/**
 * Status implementation that can change its state over time.
 *
 * <p>
 * A <code>StatefulStatus</code> begins its life in a successful state, and will remain successful unless <code>setFailed</code> is called.
 * Once <code>setFailed</code> is called, the status will remain at failed. The <code>setFailed</code> method can be called multiple times (even
 * though invoking it once is sufficient to permanently set the status to failed), but only up until <code>setCompleted</code> has been called.
 * After <code>setCompleted</code> has been called, any invocation of <code>setFailed</code> will be greeted with an <code>IllegalStateException</code>.
 * </p>
 *
 * <p>
 * Instances of this class are thread safe.
 * </p>
 */
final class StatefulStatus extends Status with Serializable {
  @transient private final val latch = new CountDownLatch(1)
  @volatile private var succeeded = true
  private final val queue = new ConcurrentLinkedQueue[Boolean => Unit]

  /**
   * Blocking call that waits until completion, as indicated by an invocation of <code>setCompleted</code> on this instance, then returns returns <code>false</code> 
   * if <code>setFailed</code> was called on this instance, else returns <code>true</code>.
   * 
   * @return <code>true</code> if no tests failed and no suites aborted, <code>false</code> otherwise
   */
  def succeeds() = {
    waitUntilCompleted()
    succeeded
  }

  /**
   * Non-blocking call that returns <code>true</code> if <code>setCompleted</code> has been invoked on this instance, <code>false</code> otherwise.
   * 
   * @return <code>true</code> if the test or suite run is already completed, <code>false</code> otherwise.
   */
  def isCompleted = latch.getCount == 0L

  /**
   * Blocking call that returns only after <code>setCompleted</code> has been invoked on this <code>StatefulStatus</code> instance.
   */
  def waitUntilCompleted() {
    latch.await()
  }

  /**
   * Sets the status to failed without changing the completion status.
   *
   * <p>
   * This method may be invoked repeatedly, even though invoking it once is sufficient to set the state of the <code>Status</code> to failed, but only
   * up until <code>setCompleted</code> has been called. Once <code>setCompleted</code> has been called, invoking this method will result in a
   * thrown <code>IllegalStateException</code>.
   * <p>
   *
   * @throws IllegalStateException if this method is invoked on this instance after <code>setCompleted</code> has been invoked on this instance.
   */
  def setFailed() {
    if (isCompleted)
      throw new IllegalStateException("status is already completed")
    succeeded = false
  }

  /**
   * Sets the status to completed.
   *
   * <p>
   * This method may be invoked repeatedly, even though invoking it once is sufficient to set the state of the <code>Status</code> to completed.
   * <p>
   */
  def setCompleted() {
    synchronized {
      latch.countDown()
    }
    for (f <- queue.iterator.asScala)
      f(succeeded)
  }

  /**
   * Registers the passed function to be executed when this status completes.
   *
   * <p>
   * You may register multiple functions, which on completion will be executed in an undefined
   * order.
   * </p>
   */
  def whenCompleted(f: Boolean => Unit) {
    var executeLocally = false
    synchronized {
      if (!isCompleted)
        queue.add(f)
      else
        executeLocally = true
    }
    if (executeLocally)
      f(succeeded)
  }
}

/**
 * Composite <code>Status</code> that aggregates its completion and failed states of set of other <code>Status</code>es passed to its constructor.
 *
 * @param status the <code>Status</code>es out of which this status is composed.
 */
final class CompositeStatus(statuses: Set[Status]) extends Status with Serializable {
  
  // TODO: Ensure this is visible to another thread, because I'm letting the reference
  // escape with my for loop below prior to finishing this object's construction.
  @transient private final val latch = new CountDownLatch(statuses.size)

  @volatile private var succeeded = true

  private final val queue = new ConcurrentLinkedQueue[Boolean => Unit]

  for (status <- statuses) {
    status.whenCompleted { st =>
      synchronized {
        latch.countDown()
      }
      if (!st)
        succeeded = false
      if (latch.getCount == 0) {
        for (f <- queue.iterator.asScala)
          f(succeeded)
      }
    }
  }

  /**
   * Blocking call that waits until all composite <code>Status</code>es have completed, then returns
   * <code>true</code> only if all of the composite <code>Status</code>es succeeded. If any <code>Status</code> passed in the <code>statuses</code> set fails, this method
   * will return <code>false</code>.
   * 
   * @return <code>true</code> if all composite <code>Status</code>es succeed, <code>false</code> otherwise.
   */
  def succeeds() = statuses.forall(_.succeeds())
  
  /**
   * Non-blocking call to check if the test or suite run is completed, returns <code>true</code> if all composite <code>Status</code>es have completed, 
   * <code>false</code> otherwise.  You can use this to poll the run status.
   * 
   * @return <code>true</code> if all composite <code>Status</code>es have completed, <code>false</code> otherwise.
   */
  def isCompleted = statuses.forall(_.isCompleted)
  
  /**
   * Blocking call that returns only after all composite <code>Status</code>s have completed.
   */
  def waitUntilCompleted() {
    statuses.foreach(_.waitUntilCompleted())
  }

  /**
   * Registers the passed function to be executed when this status completes.
   *
   * <p>
   * You may register multiple functions, which on completion will be executed in an undefined
   * order.
   * </p>
   */
  def whenCompleted(f: Boolean => Unit) {
    var executeLocally = false
    synchronized {
      if (!isCompleted)
        queue.add(f)
      else
        executeLocally = true
    }
    if (executeLocally)
      f(succeeded)
  }
}

