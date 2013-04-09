/*
 * Copyright 2001-2009 Artima, Inc.
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
 * Trait that facilitates a style of testing in which each test is run in its own instance
 * of the suite class to isolate each test from the side effects of the other tests in the
 * suite.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>: Trait <code>OneInstancePerTest</code> is intended primarily to serve as a supertrait for
 * <a href="ParallelTestExecution.html"><code>ParallelTestExecution</code></a> and the <a href="path/package.html">path traits</a>, to
 * facilitate porting JUnit tests to ScalaTest, and to make it easy for users who prefer JUnit's approach to isolation to obtain similar
 * behavior in ScalaTest.
 * </td></tr></table>
 * 
 * <p>
 * If you mix this trait into a <code>Suite</code>, you can initialize shared reassignable
 * fixture variables as well as shared mutable fixture objects in the constructor of the
 * class. Because each test will run in its own instance of the class, each test will
 * get a fresh copy of the instance variables. This is the approach to test isolation taken,
 * for example, by the JUnit framework. <code>OneInstancePerTest</code> can, therefore,
 * be handy when porting JUnit tests to ScalaTest.
 * </p>
 *
 * <p>
 * Here's an example of <code>OneInstancePerTest</code> being used in a <code>FunSuite</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 * import org.scalatest.OneInstancePerTest
 * import collection.mutable.ListBuffer
 * 
 * class MySuite extends FunSuite with OneInstancePerTest {
 * 
 *   val builder = new StringBuilder("ScalaTest is ")
 *   val buffer = new ListBuffer[String]
 * 
 *   test("easy") {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   test("fun") {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * <code>OneInstancePerTest</code> is supertrait to <a href="ParallelTestExecution.html"><code>ParallelTestExecution</code></a>, in which
 * running each test in its own instance is intended to make it easier to write suites of tests that run in parallel (by reducing the likelihood
 * of concurrency bugs in those suites.) <code>OneInstancePerTest</code> is also supertrait to the <em>path</em> traits,
 * <a href="path/FunSpec.html"><code>path.FunSpec</code></a> and <a href="path/FunSpec.html"><code>path.FreeSpec</code></a>, to make it obvious
 * these traits run each test in a new, isolated instance.
 * </p>
 * 
 * <p>
 * For the details on how <code>OneInstancePerTest</code> works, see the documentation for methods <code>runTests</code> and <code>runTest</code>,
 * which this trait overrides.
 * </p>
 * 
 * @author Bill Venners
 */
trait OneInstancePerTest extends SuiteMixin {
  
  this: Suite =>

  /**
   * Modifies the behavior of <code>super.runTest</code> to facilitate running each test in its
   * own instance of this <code>Suite</code>'s class.
   *
   * <p>
   * This trait's implementation of <code>runTest</code> 
   * uses the <code>runTestInNewInstance</code> flag of the passed <code>Args</code> object to determine whether this instance is the general instance responsible
   * for running all tests in the suite (<code>runTestInNewInstance</code> is <code>true</code>), or a test-specific instance
   * responsible for running just one test (<code>runTestInNewInstance</code> is <code>false</code>).
   * Note that these <code>Boolean</code> values are reverse those used by <code>runTests</code>, because <code>runTests</code> always inverts the <code>Boolean</code> value
   * of <code>runTestInNewInstance</code> when invoking <code>runTest</code>.
   * </p>
   * 
   * <p>
   * If <code>runTestInNewInstance</code> is <code>true</code>, this trait's implementation of this method creates a new instance of this class (by
   * invoking <code>newInstance</code> on itself), then invokes <code>run</code> on the new instance,
   * passing in <code>testName</code>, wrapped in a <code>Some</code>, and <code>args</code> unchanged.
   * (<em>I.e.</em>, the <code>Args</code> object passed to <code>runTest</code> is forwarded as is to <code>run</code>
   * on the new instance, including with <code>runTestInNewInstance</code> set.)
   * If the invocation of either <code>newInstance</code> on this
   * <code>Suite</code> or <code>run</code> on a newly created instance of this <code>Suite</code>
   * completes abruptly with an exception, then this <code>runTests</code> method will complete
   * abruptly with the same exception.
   * </p>
   * 
   * <p>
   * If <code>runTestInNewInstance</code> is <code>false</code>, this trait's implementation of this method simply invokes <code>super.runTest</code>,
   * passing along the same <code>testName</code> and <code>args</code> objects.
   * </p>
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   */
  protected abstract override def runTest(testName: String, args: Args): Status = {

    if (args.runTestInNewInstance) {
      // In initial instance, so create a new test-specific instance for this test and invoke run on it.
      val oneInstance = newInstance
      oneInstance.run(Some(testName), args)
    }
    else // Therefore, in test-specific instance, so run the test.
      super.runTest(testName, args)
  }

  /**
   * Modifies the behavior of <code>super.runTests</code> to facilitate running each test in its
   * own instance of this <code>Suite</code>'s class.
   *
   * <p>
   * This trait's implementation of <code>runTest</code> 
   * uses the <code>runTestInNewInstance</code> flag of the passed <code>Args</code> object to determine whether this instance is the general instance responsible
   * for running all tests in the suite (<code>runTestInNewInstance</code> is <code>false</code>), or a test-specific instance
   * responsible for running just one test (<code>runTestInNewInstance</code> is <code>true</code>). Note that these <code>Boolean</code> values are
   * reverse those used by <code>runTest</code>, because <code>runTests</code> always inverts the <code>Boolean</code> value of
   * <code>runTestInNewInstance</code> when invoking <code>runTest</code>.
   * </p>
   * 
   * <p>
   * If <code>runTestInNewInstance</code> is <code>false</code>, this trait's implementation of this method will invoke
   * <code>super.runTests</code>, passing along <code>testName</code> and <code>args</code>, but with the 
   * <code>runTestInNewInstance</code> flag set to <code>true</code>. By setting <code>runTestInNewInstance</code> to
   * <code>true</code>, <code>runTests</code> is telling <code>runTest</code> to create a new instance to run each test.
   * </p>
   *
   * <p>
   * If <code>runTestInNewInstance</code> is <code>true</code>, this trait's implementation of this method will invoke
   * <code>runTest</code> directly, passing in <code>testName.get</code> and the <code>args</code> object, with
   * the <code>runTestInNewInstance</code> flag set to <code>false</code>. By setting <code>runTestInNewInstance</code> to
   * <code>false</code>, <code>runTests</code> is telling <code>runTest</code> that this is the test-specific instance,
   * so it should just run the specified test.
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>, or if <code>runTestInNewInstance</code> is <code>true</code>, but <code>testName</code>
   *     is empty.
   */
  protected abstract override def runTests(testName: Option[String], args: Args): Status = {

    if (args.runTestInNewInstance) {
      if (testName.isEmpty)
        throw new IllegalArgumentException("args.runTestInNewInstance was true, but testName was not defined")
      // In test-specific instance, so run the test. (We are removing RTINI
      // so that runTest will realize it is in the test-specific instance.)
      runTest(testName.get, args.copy(runTestInNewInstance = false))
    }
    else {
      // In initial instance, so set the RTINI flag and call super.runTests, which
      // will go through any scopes and call runTest as usual. If this method was called
      // via super.runTests from PTE, the TestSortingReporter and WrappedDistributor
      // will already be in place.
      super.runTests(testName, args.copy(runTestInNewInstance = true))
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
   * import org.scalatest.Suite
   *
   * class Outer {
   *   class InnerSuite extends Suite with OneInstancePerTest {
   *     def testOne() {}
   *     def testTwo() {}
   *     override def newInstance = new InnerSuite
   *   }
   * }
   * </pre>
   */
  def newInstance: Suite with OneInstancePerTest = this.getClass.newInstance.asInstanceOf[Suite with OneInstancePerTest]
}

