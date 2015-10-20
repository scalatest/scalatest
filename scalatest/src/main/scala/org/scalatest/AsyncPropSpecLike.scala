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

import scala.collection.immutable.ListSet
import Suite.autoTagClassAnnotations
import scala.concurrent.Future

/**
 * Implementation trait for class <code>PropSpec</code>, which represents
 * a suite of property-based tests.
 *
 * <p>
 * <a href="PropSpec.html"><code>PropSpec</code></a> is a class, not a trait,
 * to minimize compile time given there is a slight compiler overhead to
 * mixing in traits compared to extending classes. If you need to mix the
 * behavior of <code>PropSpec</code> into some other class, you can use this
 * trait instead, because class <code>PropSpec</code> does nothing more than
 * extend this trait and add a nice <code>toString</code> implementation.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="PropSpec.html">detailed
 * overview of <code>PropSpec</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
//SCALATESTJS-ONLY @scala.scalajs.js.annotation.JSExportDescendentClasses(ignoreInvalidDescendants = true)
@Finders(Array("org.scalatest.finders.PropSpecFinder"))
trait AsyncPropSpecLike extends AsyncSuite with AsyncTestRegistration with AsyncCompatibility { thisSuite =>

  override private[scalatest] def transformToOutcome(testFun: => Future[Assertion]): () => AsyncOutcome =
    () => {
      val futureSucceeded = testFun.map(_ => Succeeded)
      FutureOutcome(
        futureSucceeded.recover {
          case ex: exceptions.TestCanceledException => Canceled(ex)
          case _: exceptions.TestPendingException => Pending
          case tfe: exceptions.TestFailedException => Failed(tfe)
          case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
        }
      )
    }

  private final val engine = new AsyncEngine(Resources.concurrentPropSpecMod, "PropSpec")

  import engine._

  final def registerTest(testText: String, testTags: Tag*)(testFun: => Future[Assertion]) {
    // SKIP-SCALATESTJS-START
    val stackDepthAdjustment = -1
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepthAdjustment = -4
    engine.registerTest(testText, transformToOutcome(testFun), Resources.testCannotBeNestedInsideAnotherTest, "PropSpecRegistering.scala", "registerTest", 4, stackDepthAdjustment, None, None, testTags: _*)
  }

  final def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: => Future[Assertion]) {
    // SKIP-SCALATESTJS-START
    val stackDepthAdjustment = -2
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepthAdjustment = -4
    engine.registerIgnoredTest(testText, transformToOutcome(testFun), Resources.testCannotBeNestedInsideAnotherTest, "PropSpecRegistering.scala", "registerIgnoredTest", 4, stackDepthAdjustment, None, testTags: _*)
  }

  /**
   * Register a property-based test with the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later execution via an invocation of one of the <code>run</code>
   * methods. The passed test name must not have been registered previously on
   * this <code>PropSpec</code> instance.
   *
   * @param testName the name of the property
   * @param testTags the optional list of tags for this property
   * @param testFun the property function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   * @throws NullArgumentException if <code>testName</code> or any passed test tag is <code>null</code>
   */
  protected def property(testName: String, testTags: Tag*)(testFun: => Future[Assertion]) {
    // SKIP-SCALATESTJS-START
    val stackDepth = 4
    val stackDepthAdjustment = -2
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepth = 6
    //SCALATESTJS-ONLY val stackDepthAdjustment = -4
    engine.registerTest(testName, transformToOutcome(testFun), Resources.propertyCannotAppearInsideAnotherProperty, "PropSpecRegistering.scala", "property", stackDepth, stackDepthAdjustment, None, None, testTags: _*)
  }

  /**
   * Register a property-based test to ignore, which has the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>run</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>test</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be run, but a
   * report will be sent that indicates the test was ignored. The passed test name must not have been registered previously on
   * this <code>PropSpec</code> instance.
   *
   * @param testName the name of the test
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   */
  protected def ignore(testName: String, testTags: Tag*)(testFun: => Future[Assertion]) {
    // SKIP-SCALATESTJS-START
    val stackDepth = 4
    val stackDepthAdjustment = -2
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepth = 6
    //SCALATESTJS-ONLY val stackDepthAdjustment = -6
    engine.registerIgnoredTest(testName, transformToOutcome(testFun), Resources.ignoreCannotAppearInsideAProperty, "PropSpecRegistering.scala", "ignore", stackDepth, stackDepthAdjustment, None, testTags: _*)
  }

  /**
   * An immutable <code>Set</code> of test names. If this <code>PropSpec</code> contains no tests, this method returns an empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's iterator will
   * return those names in the order in which the tests were registered.
   * </p>
   */
  override def testNames: Set[String] = {
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by <code>testName</code>.
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   *
   * @throws IllegalArgumentException if <code>testName</code> is defined but a test with that name does not exist on this <code>PropSpec</code>
   * @throws NullArgumentException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {
    def invokeWithAsyncFixture(theTest: TestLeaf): AsyncOutcome = {
      val theConfigMap = args.configMap
      val testData = testDataFor(testName, theConfigMap)
      FutureOutcome(
        withAsyncFixture(
          new NoArgAsyncTest {
            val name = testData.name
            def apply(): Future[Outcome] = { theTest.testFun().toFutureOutcome }
            val configMap = testData.configMap
            val scopes = testData.scopes
            val text = testData.text
            val tags = testData.tags
          }
        )
      )
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithAsyncFixture)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> names of tagged tests and whose associated values are
   * the <code>Set</code> of tags for the test. If this <code>PropSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to
   * methods <code>property</code> and <code>ignore</code>.
   * </p>
   *
   * <p>
   * In addition, this trait's implementation will also auto-tag tests with class level annotations.
   * For example, if you annotate <code>@Ignore</code> at the class level, all test methods in the class will be auto-annotated with
   * <code>org.scalatest.Ignore</code>.
   * </p>
   */
  override def tags: Map[String, Set[String]] = autoTagClassAnnotations(atomic.get.tagsMap, this)

  /**
   * Run zero to many of this <code>PropSpec</code>'s tests.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullArgumentException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    runTestsImpl(thisSuite, testName, args, true, parallelAsyncTestExecution, runTest)
  }

  override def run(testName: Option[String], args: Args): Status = {
    runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Registers shared tests.
   *
   * <p>
   * This method enables the following syntax for shared tests in a <code>PropSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * propertiesFor(nonEmptyStack(lastValuePushed))
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared tests. For examples of shared tests, see the
   * <a href="#sharedTests">Shared tests section</a> in the main documentation for this trait.
   * </p>
   */
  protected def propertiesFor(unit: Unit) {}

  /**
   * Suite style name.
   */
  final override val styleName: String = "org.scalatest.PropSpec"

  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
