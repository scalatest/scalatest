/*
 * Copyright 2001-2012 Artima, Inc.
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

/**
 * Stackable trait that can be mixed into suites that need code that makes use of test data (test name, tags, config map, <em>etc.</em>) executed
 * before and/or after running each test.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use trait <code>BeforeAndAfterEachTestData</code> when you want to stack traits that perform side-effects before and/or after tests, rather
 * than at the beginning or end of tests, when you need access to any test data (such as the config map) in the before and/or after code.
 * <em>Note: For more insight into where <code>BeforeAndAfterEachTestData</code> fits into the big picture, see the </em>
 * <a href="FlatSpec.html#sharedFixtures">Shared fixtures</a> section in the documentation for your chosen style trait.</em>
 * </td></tr></table>
 * 
 * <p>
 * A test <em>fixture</em> is composed of the objects and other artifacts (files, sockets, database
 * connections, <em>etc.</em>) tests use to do their work.
 * When multiple tests need to work with the same fixtures, it is important to try and avoid
 * duplicating the fixture code across those tests. The more code duplication you have in your
 * tests, the greater drag the tests will have on refactoring the actual production code.
 * Trait <code>BeforeAndAfterEachTestData</code> offers one way to eliminate such code duplication:
 * a <code>beforeEach(TestData)</code> method that will be run before each test (like JUnit's <code>setUp</code>),
 * and an <code>afterEach(TestData)</code> method that will be run after (like JUnit's <code>tearDown</code>).
 * </p>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.flatspec.composingbeforeandaftereachtestdata
 * 
 * import org.scalatest._
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends BeforeAndAfterEachTestData { this: Suite =&gt;
 * 
 *   val builder = new StringBuilder
 * 
 *   override def beforeEach(td: TestData) {
 *     builder.append(td.name)
 *     super.beforeEach(td) // To be stackable, must call super.beforeEach(TestData)
 *   }
 * 
 *   override def afterEach(td: TestData) {
 *     try {
 *       super.afterEach(td) // To be stackable, must call super.afterEach(TestData)
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends BeforeAndAfterEachTestData { this: Suite =&gt;
 * 
 *   val buffer = new ListBuffer[String]
 * 
 *   override def afterEach(td: TestData) {
 *     try {
 *       super.afterEach(td) // To be stackable, must call super.afterEach(TestData)
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSpec extends FlatSpec with Builder with Buffer {
 * 
 *   "Testing" should "be easy" in {
 *     builder.append("!")
 *     assert(builder.toString === "Testing should be easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   it should "be fun" in {
 *     builder.append("!")
 *     assert(builder.toString === "Testing should be fun!")
 *     assert(buffer.isEmpty)
 *     buffer += "clear"
 *   }
 * }
 * </pre>
 *
 * <p>
 * To get the same ordering as <code>withFixture</code>, place your <code>super.beforeEach(TestData)</code> call at the end of each
 * <code>beforeEach(TestData)</code> method, and the <code>super.afterEach(TestData)</code> call at the beginning of each <code>afterEach(TestData)</code>
 * method, as shown in the previous example. It is a good idea to invoke <code>super.afterEach(TestData)</code> in a <code>try</code>
 * block and perform cleanup in a <code>finally</code> clause, as shown in the previous example, because this ensures the
 * cleanup code is performed even if <code>super.afterEach(TestData)</code> throws an exception.
 * </p>
 *
 * <p>
 * Besides enabling trait stacking, the other main advantage of <code>BeforeAndAfterEachTestData</code> over <code>BeforeAndAfter</code>
 * that <code>BeforeAndAfterEachTestData</code> allows you to make use of test data (such as the test name and config map) in your before
 * and/or after code, whereas <code>BeforeAndAfter</code> do not.
 * </p>
 *
 * <p>
 * The main disadvantage of <code>BeforeAndAfterEachTestData</code> compared to <code>BeforeAndAfter</code> and <code>BeforeAndAfterEach</code> is
 * that <code>BeforeAndAfterEachTestData</code> requires more boilerplate. If you don't need trait stacking or access to the test data, use
 * <a href="BeforeAndAfter.html"><code>BeforeAndAfter</code></a> instead
 * of <code>BeforeAndAfterEachTestData</code>.
 * If you need trait stacking, but not access to the <code>TestData</code>, use
 * <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a> instead.
 * </p>
 *
 * @author Bill Venners
 */
trait BeforeAndAfterEachTestData extends SuiteMixin {

  this: Suite =>

  /**
   * Defines a method (that takes a <code>TestData</code>) to be run before
   * each of this suite's tests.
   *
   * <p>
   * This trait's implementation
   * of <code>runTest</code> invokes this method before running
   * each test (passing in a TestData that includes the <code>configMap</code> passed to it), thus this
   * method can be used to set up a test fixture
   * needed by each test. This trait's implementation of this method does nothing.
   * </p>
   */
  protected def beforeEach(testData: TestData) {
  }

  /**
   * Defines a method (that takes a <code>TestData</code>) to be run after
   * each of this suite's tests.
   *
   * <p>
   * This trait's implementation
   * of <code>runTest</code> invokes this method after running
   * each test (passing in a <code>TestData</code> containing the <code>configMap</code> passed
   * to it), thus this method can be used to tear down a test fixture
   * needed by each test. This trait's implementation of this method does nothing.
   * </p>
   */
  protected def afterEach(testData: TestData) {
  }

  /**
   * Run a test surrounded by calls to <code>beforeEach</code> and <code>afterEach</code>.
   *
   * <p>
   * This trait's implementation of this method ("this method") invokes
   * <code>beforeEach(TestData)</code>
   * before running each test and <code>afterEach(TestData)</code>
   * after running each test. It runs each test by invoking <code>super.runTest</code>, passing along
   * the two parameters passed to it.
   * </p>
   * 
   * <p>
   * If any invocation of <code>beforeEach(TestData)</code> completes abruptly with an exception, this
   * method will complete abruptly with the same exception. If any call to
   * <code>super.runTest</code> completes abruptly with an exception, this method
   * will complete abruptly with the same exception, however, before doing so, it will
   * invoke <code>afterEach(TestData)</code>. If <code>afterEach(TestData)</code> <em>also</em> completes abruptly with an exception, this
   * method will nevertheless complete abruptly with the exception previously thrown by <code>super.runTest</code>.
   * If <code>super.runTest</code> returns normally, but <code>afterEach(TestData)</code> completes abruptly with an
   * exception, this method will complete abruptly with the exception thrown by <code>afterEach(TestData)</code>.
   * </p>
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
  */
  abstract protected override def runTest(testName: String, args: Args): Status = {

    var thrownException: Option[Throwable] = None

    beforeEach(testDataFor(testName, args.configMap))
    try {
      super.runTest(testName, args)
    }
    catch {
      case e: Exception => 
        thrownException = Some(e)
        FailedStatus
    }
    finally {
      try {
        afterEach(testDataFor(testName, args.configMap)) // Make sure that afterEach is called even if runTest completes abruptly.
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
}
