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

import java.util.concurrent.atomic.AtomicReference

/**
 * Trait that can be mixed into suites that need code executed before and after running each test.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use trait <code>BeforeAndAfter</code> when you need to perform the same side-effects before and/or after tests, rather than at the beginning
 * or end of tests. <em>Note: For more insight into where <code>BeforeAndAfter</code> fits into the big picture, see the </em>
 * <a href="FlatSpec.html#sharedFixtures">Shared fixtures</a> section in the documentation for your chosen style trait.</em>
 * </td></tr></table>
 * 
 * <p>
 * A test <em>fixture</em> is composed of the objects and other artifacts (files, sockets, database
 * connections, <em>etc.</em>) tests use to do their work.
 * When multiple tests need to work with the same fixtures, it is important to try and avoid
 * duplicating the fixture code across those tests. The more code duplication you have in your
 * tests, the greater drag the tests will have on refactoring the actual production code.
 * Trait <code>BeforeAndAfter</code> offers one way to eliminate such code duplication:
 * a <code>before</code> clause that will register code to be run before each test,
 * and an <code>after</code> clause that will register code to be run after.
 * </p>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.flatspec.beforeandafter
 * 
 * import org.scalatest._
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends FlatSpec with BeforeAndAfter {
 * 
 *   val builder = new StringBuilder
 *   val buffer = new ListBuffer[String]
 * 
 *   before {
 *     builder.append("ScalaTest is ")
 *   }
 * 
 *   after {
 *     builder.clear()
 *     buffer.clear()
 *   }
 * 
 *   "Testing" should "be easy" in {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   it should "be fun" in {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * The <code>before</code> and <code>after</code> methods can each only be called once per <code>Suite</code>,
 * and cannot be invoked after <code>run</code> has been invoked.  If either of the registered before or after functions
 * complete abruptly with an exception, it will be reported as an aborted suite and no more tests will be attempted in that suite.
 * </p>
 *
 * <p>
 * Note that the only way <code>before</code> and <code>after</code> code can communicate with test code is via some side-effecting mechanism, commonly by
 * reassigning instance <code>var</code>s or by changing the state of mutable objects held from instance <code>val</code>s (as in this example). If using
 * instance <code>var</code>s or mutable objects held from instance <code>val</code>s you wouldn't be able to run tests in parallel in the same instance
 * of the test class unless you synchronized access to the shared, mutable state. This is why ScalaTest's <code>ParallelTestExecution</code> trait extends
 * <code>OneInstancePerTest</code>. By running each test in its own instance of the class, each test has its own copy of the instance variables, so you
 * don't need to synchronize. Were you to mix <code>ParallelTestExecution</code> into the <code>ExampleSuite</code> above, the tests would run in parallel just fine
 * without any synchronization needed on the mutable <code>StringBuilder</code> and <code>ListBuffer[String]</code> objects.
 * </p>
 *
 * <p>
 * Although <code>BeforeAndAfter</code> provides a minimal-boilerplate way to execute code before and after tests, it isn't designed to enable stackable
 * traits, because the order of execution would be non-obvious.  If you want to factor out before and after code that is common to multiple test suites, you 
 * should use trait <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a> instead.
 * </p>
 *
 * <p>
 * The advantage this trait has over <code>BeforeAndAfterEach</code> is that its syntax is more concise. 
 * The main disadvantage is that it is not stackable, whereas <code>BeforeAndAfterEach</code> is. <em>I.e.</em>, 
 * you can write several traits that extend <code>BeforeAndAfterEach</code> and provide <code>beforeEach</code> methods
 * that include a call to <code>super.beforeEach</code>, and mix them together in various combinations. By contrast,
 * only one call to the <code>before</code> registration function is allowed in a suite or spec that mixes
 * in <code>BeforeAndAfter</code>. In addition, <code>BeforeAndAfterEach</code> allows you to access
 * the config map and test name via the <a href="TestData.html"><code>TestData</code></a> passed to its <code>beforeEach</code> and
 * <code>afterEach</code> methods, whereas <code>BeforeAndAfter</code>
 * gives you no access to the config map.
 * </p>
 *
 * @author Bill Venners
 */
trait BeforeAndAfter extends SuiteMixin { this: Suite =>

  private val beforeFunctionAtomic = new AtomicReference[Option[() => Any]](None)
  private val afterFunctionAtomic = new AtomicReference[Option[() => Any]](None)
  @volatile private var runHasBeenInvoked = false

  /**
   * Registers code to be executed before each of this suite's tests.
   *
   * <p>
   * This trait's implementation
   * of <code>runTest</code> executes the code passed to this method before running
   * each test. Thus the code passed to this method can be used to set up a test fixture
   * needed by each test.
   * </p>
   *
   * @throws NotAllowedException if invoked more than once on the same <code>Suite</code> or if
   *                             invoked after <code>run</code> has been invoked on the <code>Suite</code>
   */
  protected def before(fun: => Any) {
    if (runHasBeenInvoked)
      throw new NotAllowedException("You cannot call before after run has been invoked (such as, from within a test). It is probably best to move it to the top level of the Suite class so it is executed during object construction.", 0)
    val success = beforeFunctionAtomic.compareAndSet(None, Some(() => fun))
    if (!success)
      throw new NotAllowedException("You are only allowed to call before once in each Suite that mixes in BeforeAndAfter.", 0)
  }

  /**
   * Registers code to be executed after each of this suite's tests.
   *
   * <p>
   * This trait's implementation of <code>runTest</code> executes the code passed to this method after running
   * each test. Thus the code passed to this method can be used to tear down a test fixture
   * needed by each test.
   * </p>
   *
   * @throws NotAllowedException if invoked more than once on the same <code>Suite</code> or if
   *                             invoked after <code>run</code> has been invoked on the <code>Suite</code>
   */
  protected def after(fun: => Any) {
    if (runHasBeenInvoked)
      throw new NotAllowedException("You cannot call after after run has been invoked (such as, from within a test. It is probably best to move it to the top level of the Suite class so it is executed during object construction.", 0)
    val success = afterFunctionAtomic.compareAndSet(None, Some(() => fun))
    if (!success)
      throw new NotAllowedException("You are only allowed to call after once in each Suite that mixes in BeforeAndAfter.", 0)
  }

  /**
   * Run a test surrounded by calls to the code passed to <code>before</code> and <code>after</code>, if any.
   *
   * <p>
   * This trait's implementation of this method ("this method") invokes
   * the function registered with <code>before</code>, if any,
   * before running each test and the function registered with <code>after</code>, if any,
   * after running each test. It runs each test by invoking <code>super.runTest</code>, passing along
   * the five parameters passed to it.
   * </p>
   * 
   * <p>
   * If any invocation of the function registered with <code>before</code> completes abruptly with an exception, this
   * method will complete abruptly with the same exception. If any call to
   * <code>super.runTest</code> completes abruptly with an exception, this method
   * will complete abruptly with the same exception, however, before doing so, it will
   * invoke the function registered with <code>after</code>, if any. If the function registered with <code>after</code>
   * <em>also</em> completes abruptly with an exception, this
   * method will nevertheless complete abruptly with the exception previously thrown by <code>super.runTest</code>.
   * If <code>super.runTest</code> returns normally, but the function registered with <code>after</code> completes abruptly with an
   * exception, this method will complete abruptly with the exception thrown by the function registered with <code>after</code>.
   * </p>
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
  */
  abstract protected override def runTest(testName: String, args: Args): Status = {

    var thrownException: Option[Throwable] = None

    beforeFunctionAtomic.get match {
      case Some(fun) => fun()
      case None =>
    }

    try {
      super.runTest(testName, args)
    }
    catch {
      case e: Exception => thrownException = Some(e)
      FailedStatus
    }
    finally {
      try {
        // Make sure that afterEach is called even if runTest completes abruptly.
        afterFunctionAtomic.get match {
          case Some(fun) => fun()
          case None =>
        }

        thrownException match {
          case Some(e) => throw e
          case None =>
        }
      }
      catch {
        case laterException: Exception =>
          thrownException match { // If both run and afterAll throw an exception, report the test exception
            case Some(earlierException) => throw earlierException
            case None => throw laterException
          }
      }
    }
  }

  /**
   * This trait's implementation of run sets a flag indicating run has been invoked, after which
   * any invocation to <code>before</code> or <code>after</code> will complete abruptly
   * with a <code>NotAllowedException</code>.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests and nested suites started by this method have completed, and whether or not a failure occurred.
   */
  abstract override def run(testName: Option[String], args: Args): Status = {
    runHasBeenInvoked = true
    super.run(testName, args)
  }
}
