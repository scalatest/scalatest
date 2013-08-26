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
 * Trait that can be mixed into suites that need methods invoked before and after executing the
 * suite.
 *
 * <p>
 * This trait allows code to be executed before and/or after all the tests and nested suites of a
 * suite are run. This trait overrides <code>run</code> and calls the
 * <code>beforeAll</code> method, then calls <code>super.run</code>. After the <code>super.run</code>
 * invocation completes, whether it returns normally or completes abruptly with an exception,
 * this trait's <code>run</code> method will invoke <code>afterAll</code>.
 * </p>
 *
 * <p>
 * Trait <code>BeforeAndAfterAll</code> defines <code>beforeAll</code>
 * and <code>afterAll</code> methods that take no parameters. This trait's implementation of these
 * methods do nothing.
 * </p>
 *
 * <p>
 * For example, the following <code>ExampleSpec</code> mixes in <code>BeforeAndAfterAll</code> and
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
 * package org.scalatest.examples.beforeandafterall
 *
 * import org.scalatest._
 * import java.io._
 * 
 * trait TempFileExistsSpec extends fixture.FlatSpecLike {
 * 
 *   private val tempFileName = "tmp.txt"
 * 
 *   type FixtureParam = File
 *   override def withFixture(test: OneArgTest) = {
 *     val file = new File(tempFileName)
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
 * ) with TempFileExistsSpec with BeforeAndAfterAll {
 * 
 *   // Set up the temp file needed by the test, taking
 *   // a file name from the config map
 *   override def beforeAll() {
 *     val writer = new FileWriter(tempFileName)
 *     try writer.write("Hello, suite of tests!")
 *     finally writer.close()
 *   }
 * 
 *   // Delete the temp file
 *   override def afterAll() {
 *     val file = new File(tempFileName)
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
 * @author Bill Venners
 */
trait BeforeAndAfterAll  extends SuiteMixin { this: Suite =>

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
   * Defines a method to be run before any of this suite's tests or nested suites are run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes the overloaded form of this method that
   * takes a <code>configMap</code> (which has been deprecated) before executing
   * any tests or nested suites. This trait's implementation of that <code>beforeAll(ConfigMap)</code>
   * method simply invokes this <code>beforeAll()</code>
   * method. Thus this method can be used to set up a test fixture
   * needed by the entire suite, when you don't need anything from the <code>configMap</code>.
   * This trait's implementation of this method does nothing.
   * </p>
   */
  protected def beforeAll() = ()

  /**
   * <strong>This overloaded form of <code>beforeAll</code> has been deprecated and will
   * be removed in a future version of ScalaTest. Please use the <code>beforeAll(ConfigMap)</code> method
   * of trait <code>BeforeAndAfterAllConfigMap</code> instead.</strong>
   */
  @deprecated("Please use the beforeAll(ConfigMap) method of trait BeforeAndAfterAllConfigMap instead.")
  protected def beforeAll(configMap: ConfigMap) {
    beforeAll()
  }

  /**
   * Defines a method to be run after all of this suite's tests and nested suites have
   * been run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes the overloaded form of this method that
   * takes a <code>configMap</code> (which has been deprecated) after executing
   * all tests and nested suites. This trait's implementation of that <code>afterAll(ConfigMap)</code> method simply invokes this
   * <code>afterAll()</code> method. Thus this method can be used to tear down a test fixture
   * needed by the entire suite, when you don't need anything from the <code>configMap</code>.
   * This trait's implementation of this method does nothing.
   * </p>
   */
  protected def afterAll() = ()

  /**
   * <strong>This overloaded form of <code>afterAll</code> has been deprecated and will
   * be removed in a future version of ScalaTest. Please use the <code>afterAll(ConfigMap)</code> method
   * of trait <code>BeforeAndAfterAllConfigMap</code> instead.</strong>
   */
  @deprecated("Please use the afterAll(ConfigMap) method of trait BeforeAndAfterAllConfigMap instead.")
  protected def afterAll(configMap: ConfigMap) {
    afterAll()
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
    var thrownException: Option[Throwable] = None

    if (!args.runTestInNewInstance && (expectedTestCount(args.filter) > 0 || invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected))
      beforeAll(args.configMap)
    try {
      val runStatus = super.run(testName, args)
      runStatus.succeeds()
      runStatus
    }
    catch {
      case e: Exception => 
        thrownException = Some(e)
        FailedStatus
    }
    finally {
      try {
        if (!args.runTestInNewInstance && (expectedTestCount(args.filter) > 0 || invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected))
          afterAll(args.configMap) // Make sure that afterAll is called even if run completes abruptly.
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
