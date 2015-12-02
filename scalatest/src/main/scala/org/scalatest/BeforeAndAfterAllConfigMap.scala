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

/**
 * Trait that can be mixed into suites that need methods that make use of the config map invoked before and/or after
 * executing the suite.
 *
 * <p>
 * This trait allows code to be executed before and/or after all the tests and nested suites of a
 * suite are run. This trait overrides <code>run</code> and calls the
 * <code>beforeAll(ConfigMap)</code> method, then calls <code>super.run</code>. After the <code>super.run</code>
 * invocation completes, whether it returns normally or completes abruptly with an exception,
 * this trait's <code>run</code> method will invoke <code>afterAll(ConfigMap)</code>.
 * </p>
 *
 * <p>
 * Note that this trait differs from <code>BeforeAndAfterAll</code> in that it gives
 * the <code>beforeAll</code> and <code>afterAll</code> code access to the config map. If you don't need
 * the config map, use <a href="BeforeAndAfterAll.html"><code>BeforeAndAfterAll</code></a> instead.
 * </p>
 *
 * <p>
 * Trait <code>BeforeAndAfterAllConfigMap</code> defines <code>beforeAll</code>
 * and <code>afterAll</code> methods that take a <code>configMap</code>.
 * This trait's implemention of each method does nothing.
 * </p>
 *
 * <p>
 * For example, the following <code>ExampleSpec</code> mixes in <code>BeforeAndAfterAllConfigMap</code> and
 * in <code>beforeAll</code>, creates and writes to a temp file, taking the name of the temp file
 * from the <code>configMap</code>. This same <code>configMap</code> is then passed to the <code>run</code>
 * methods of the nested suites, <code>OneSpec</code>, <code>TwoSpec</code>, <code>RedSpec</code>,
 * and <code>BlueSpec</code>, so those suites can access the filename and, therefore, the file's
 * contents. After all of the nested suites have executed, <code>afterAll</code> is invoked, which
 * again grabs the file name from the <code>configMap</code> and deletes the file. Each of these five
 * test classes extend trait <code>TempFileExistsSpec</code>, which defines a test that ensures the temp file exists.
 * (Note: if you're unfamiliar with the <code>withFixture(OneArgTest)</code> approach to shared fixtures, check out
 * the documentation for trait <a href="fixture/FlatSpec.html"><code>fixture.FlatSpec</code></a>.)
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.beforeandafterallconfigmap
 *
 * import org.scalatest._
 * import java.io._
 * 
 * trait TempFileExistsSpec extends fixture.FlatSpec {
 * 
 *   type FixtureParam = File
 *   override def withFixture(test: OneArgTest) = {
 *     val fileName = test.configMap.getRequired[String]("tempFileName")
 *     val file = new File(fileName)
 *     withFixture(test.toNoArgTest(file)) // loan the fixture to the test
 *   }
 * 
 *   "The temp file" should ("exist in " + suiteName) in { file =&gt;
 *     assert(file.exists)
 *   }
 * }
 * 
 * class OneSpec extends TempFileExistsSpec
 * class TwoSpec extends TempFileExistsSpec
 * class RedSpec extends TempFileExistsSpec
 * class BlueSpec extends TempFileExistsSpec
 * 
 * class ExampleSpec extends Suites(
 *   new OneSpec,
 *   new TwoSpec,
 *   new RedSpec,
 *   new BlueSpec
 * ) with TempFileExistsSpec with BeforeAndAfterAllConfigMap {
 * 
 *   private val tempFileName = "tempFileName"
 * 
 *   // Set up the temp file needed by the test, taking
 *   // a file name from the config map
 *   override def beforeAll(cm: ConfigMap) {
 *     assume(
 *       cm.isDefinedAt(tempFileName),
 *       "must place a temp file name in the config map under the key: " + tempFileName
 *     )
 *     val fileName = cm.getRequired[String](tempFileName)
 *     val writer = new FileWriter(fileName)
 *     try writer.write("Hello, suite of tests!")
 *     finally writer.close()
 *   }
 * 
 *   // Delete the temp file
 *   override def afterAll(cm: ConfigMap) {
 *     val fileName = cm.getRequired[String]("tempFileName")
 *     val file = new File(fileName)
 *     file.delete()
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running the above class in the interpreter will give an error if you don't supply a mapping for <code>"tempFileName"</code> in the config map:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSpec execute
 * <span class="stGreen">ExampleSpec:</span>
 * <span class="stRed">Exception encountered when invoking run on a suite. *** ABORTED ***
 *   Exception encountered when invoking run on a suite. (<console>:30)
 * *** RUN ABORTED ***
 *   An exception or error caused a run to abort: must place a temp file name in the config map under the key: tempFileName (<console>:30)</span>
 * </pre>
 *
 * <p>
 * If you do supply a mapping for <code>"tempFileName"</code> in the config map, you'll see that the temp file is available to all the tests:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSpec execute (configMap = ConfigMap("tempFileName" -&gt; "tmp.txt"))
 * <span class="stGreen">ExampleSpec:
 * OneSpec:
 * The temp file
 * - should exist in OneSpec
 * TwoSpec:
 * The temp file
 * - should exist in TwoSpec
 * RedSpec:
 * The temp file
 * - should exist in RedSpec
 * BlueSpec:
 * The temp file
 * - should exist in BlueSpec
 * The temp file
 * - should exist in ExampleSpec</span>
 * </pre>
 *
 * <p>
 * <strong>Note: As of 2.0.M5, this trait uses the newly added <code>Status</code> result of <code>Suite</code>'s "run" methods
 * to ensure that the code in <code>afterAll</code> is executed after
 * all the tests and nested suites are executed even if a <code>Distributor</code> is passed.</strong>
 * </p>
 *
 * <p>
 * Note that it is <em>not</em> guaranteed that <code>afterAll</code> is invoked from the same thread as <code>beforeAll</code>,
 * so if there's any shared state between <code>beforeAll</code> and <code>afterAll</code> you'll need to make sure they are
 * synchronized correctly.
 * </p>
 *
 * @author Bill Venners
 * @author Chee Seng
 */
trait BeforeAndAfterAllConfigMap  extends SuiteMixin { this: Suite =>
  
  /**
   * Flag to indicate whether to invoke beforeAll and afterAll even when there are no tests expected.
   *
   * <p>
   * The default value is <code>false</code>, which means beforeAll and afterAll will not be invoked 
   * if there are no tests expected. Whether tests are expected is determined by invoking <code>expectedTestCount</code> passing in
   * the passed filter. Because this count does not include tests excluded based on tags, such as ignored tests, this prevents
   * any side effects in <code>beforeAll</code> or <code>afterAll</code> if no tests will ultimately be executed anyway.
   * If you always want to see the side effects even if no tests are expected, override this <code>val</code> and set it to true.
   * </p>
   */
  val invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected = false

  /**
   * Defines a method (that takes a <code>configMap</code>) to be run before any
   * of this suite's tests or nested suites are run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes this method before executing
   * any tests or nested suites (passing in the <code>configMap</code> passed to it), thus this
   * method can be used to set up a test fixture
   * needed by the entire suite. This trait's implementation of this method does nothing.
   * </p>
   */
  protected def beforeAll(configMap: ConfigMap) {
  }

  /**
   * Defines a method (that takes a <code>configMap</code>) to be run after
   * all of this suite's tests and nested suites have been run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes this method after executing all tests
   * and nested suites (passing in the <code>configMap</code> passed to it), thus this
   * method can be used to tear down a test fixture
   * needed by the entire suite. This trait's implementation of this method does nothing.
   * </p>
   */
  protected def afterAll(configMap: ConfigMap) {
  }

  /**
   * Execute a suite surrounded by calls to <code>beforeAll</code> and <code>afterAll</code>.
   *
   * <p>
   * This trait's implementation of this method ("this method") invokes <code>beforeAll(ConfigMap)</code>
   * before executing any tests or nested suites and <code>afterAll(ConfigMap)</code>
   * after executing all tests and nested suites. It runs the suite by invoking <code>super.run</code>, passing along
   * the parameters passed to it.
   * </p>
   *
   * <p>
   * If any invocation of <code>beforeAll</code> completes abruptly with an exception, this
   * method will complete abruptly with the same exception. If any call to
   * <code>super.run</code> completes abruptly with an exception, this method
   * will complete abruptly with the same exception, however, before doing so, it will
   * invoke <code>afterAll</code>. If <code>afterAll</code> <em>also</em> completes abruptly with an exception, this
   * method will nevertheless complete abruptly with the exception previously thrown by <code>super.run</code>.
   * If <code>super.run</code> returns normally, but <code>afterAll</code> completes abruptly with an
   * exception, this method will complete abruptly with the same exception.
   * </p>
   *
   * <p>
   * This method does not invoke either <code>beforeAll</code> or <code>afterAll</code> if <code>runTestsInNewInstance</code> is true so
   * that any side effects only happen once per test if <code>OneInstancePerTest</code> is being used. In addition, if no tests
   * are expected, then <code>beforeAll</code> and <code>afterAll</code> will be invoked only if the
   * <code>invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected</code> flag is true. By default, this flag is false, so that if 
   * all tests are excluded (such as if the entire suite class has been marked with <code>@Ignore</code>), then side effects
   * would happen only if at least one test will ultimately be executed in this suite or its nested suites.
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
  */
  abstract override def run(testName: Option[String], args: Args): Status = {
    if (!args.runTestInNewInstance && (expectedTestCount(args.filter) > 0 || invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected))
      beforeAll(args.configMap)

    val (runStatus, thrownException) =
      try {
        (super.run(testName, args), None)
      }
      catch {
        case e: Exception => (FailedStatus, Some(e))
      }

    thrownException match {
      case Some(earlierException) =>
        try {
          if (!args.runTestInNewInstance && (expectedTestCount(args.filter) > 0 || invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected))
            afterAll(args.configMap) // Make sure that afterAll is called even if run completes abruptly.
          runStatus
        }
        catch {
          case laterException: Exception => // Do nothing, will need to throw the earlier exception
          runStatus // TODO: do a println here of the swallowed exception
        }
        finally {
          throw earlierException
        }
      case None =>
        if (!args.runTestInNewInstance && (expectedTestCount(args.filter) > 0 || invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected)) {
          // runStatus may not be completed, call afterAll only after it is completed
          runStatus withAfterEffect {
            afterAll(args.configMap)
          }
        }
        else runStatus
    }
  }
}
