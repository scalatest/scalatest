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
package org.scalatest.fixture

import org.scalatest._
import scala.collection.immutable.ListSet
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import org.scalatest.Suite.autoTagClassAnnotations
import org.scalatest.exceptions.NotAllowedException
import scala.concurrent.Future

/**
 * Implementation trait for class <code>fixture.FeatureSpec</code>, which is
 * a sister class to <a href="../FeatureSpec.html"><code>org.scalatest.FeatureSpec</code></a> that can pass a
 * fixture object into its tests.
 *
 * <p>
 * <a href="FeatureSpec.html"><code>fixture.FeatureSpec</code></a> is a class,
 * not a trait, to minimize compile time given there is a slight compiler
 * overhead to mixing in traits compared to extending classes. If you need
 * to mix the behavior of <code>fixture.FeatureSpec</code> into some other
 * class, you can use this trait instead, because class
 * <code>fixture.FeatureSpec</code> does nothing more than extend this trait and add a nice <code>toString</code> implementation.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="FeatureSpec.html">detailed
 * overview of <code>fixture.FeatureSpec</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
//SCALATESTJS-ONLY @scala.scalajs.js.annotation.JSExportDescendentClasses(ignoreInvalidDescendants = true)
@Finders(Array("org.scalatest.finders.FeatureSpecFinder"))
trait AsyncFeatureSpecLike extends AsyncSuite with AsyncTestRegistration { thisSuite =>

  override private[scalatest] def transformToOutcome(testFun: FixtureParam => Future[Assertion]): FixtureParam => AsyncOutcome =
    (fixture: FixtureParam) => {
      val futureUnit = testFun(fixture)
      FutureOutcome(
        futureUnit.map(u => Succeeded).recover {
          case ex: exceptions.TestCanceledException => Canceled(ex)
          case _: exceptions.TestPendingException => Pending
          case tfe: exceptions.TestFailedException => Failed(tfe)
          case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
        }
      )
    }

  private final val engine = new AsyncFixtureEngine[FixtureParam](Resources.concurrentFeatureSpecMod, "FixtureFeatureSpec")

  import engine._

  private[scalatest] val sourceFileName = "FeatureSpecRegistering.scala"

  final def registerTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Future[Assertion]) {
    // SKIP-SCALATESTJS-START
    val stackDepthAdjustment = -1
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepthAdjustment = -4
    engine.registerTest(Resources.scenario(testText.trim), transformToOutcome(testFun), Resources.testCannotBeNestedInsideAnotherTest, "FeatureSpecRegistering.scala", "registerTest", 4, stackDepthAdjustment, None, None, testTags: _*)
  }

  final def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Future[Assertion]) {
    // SKIP-SCALATESTJS-START
    val stackDepthAdjustment = -3
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepthAdjustment = -5
    engine.registerIgnoredTest(Resources.scenario(testText.trim), transformToOutcome(testFun), Resources.testCannotBeNestedInsideAnotherTest, "FeatureSpecRegistering.scala", "registerIgnoredTest", 4, stackDepthAdjustment, None, testTags: _*)
  }

  class ResultOfScenarioInvocation(specText: String, testTags: Tag*) {
    def apply(testFun: FixtureParam => Future[Assertion]): Unit = {
      // SKIP-SCALATESTJS-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY val stackDepth = 6
      //SCALATESTJS-ONLY val stackDepthAdjustment = -6
      engine.registerTest(Resources.scenario(specText.trim), transformToOutcome(testFun), Resources.scenarioCannotAppearInsideAnotherScenario, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, None, testTags: _*)
    }
    def apply(testFun: () => Future[Assertion]): Unit = {
      // SKIP-SCALATESTJS-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY val stackDepth = 6
      //SCALATESTJS-ONLY val stackDepthAdjustment = -6
      engine.registerTest(Resources.scenario(specText.trim), transformToOutcome(new NoArgTestWrapper(testFun)), Resources.scenarioCannotAppearInsideAnotherScenario, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, None, testTags: _*)
    }
  }

  /**
   * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
   * An invocation of this method is called an &ldquo;example.&rdquo;
   *
   * This method will register the test for later execution via an invocation of one of the <code>execute</code>
   * methods. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>FeatureSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  protected def scenario(specText: String, testTags: Tag*): ResultOfScenarioInvocation =
    new ResultOfScenarioInvocation(specText, testTags: _*)

  class ResultOfIgnoreInvocation(specText: String, testTags: Tag*) {
    def apply(testFun: FixtureParam => Future[Assertion]): Unit = {
      // SKIP-SCALATESTJS-START
      val stackDepth = 3
      val stackDepthAdjustment = -3
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY val stackDepth = 6
      //SCALATESTJS-ONLY val stackDepthAdjustment = -7
      engine.registerIgnoredTest(Resources.scenario(specText), transformToOutcome(testFun), Resources.ignoreCannotAppearInsideAScenario, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, testTags: _*)
    }
    def apply(testFun: () => Future[Assertion]): Unit = {
      // SKIP-SCALATESTJS-START
      val stackDepth = 3
      val stackDepthAdjustment = -3
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY val stackDepth = 6
      //SCALATESTJS-ONLY val stackDepthAdjustment = -7
      engine.registerIgnoredTest(Resources.scenario(specText), transformToOutcome(new NoArgTestWrapper(testFun)), Resources.ignoreCannotAppearInsideAScenario, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, testTags: _*)
    }
  }

  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>FeatureSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  protected def ignore(specText: String, testTags: Tag*): ResultOfIgnoreInvocation =
    new ResultOfIgnoreInvocation(specText, testTags: _*)

  /**
   * Describe a &ldquo;subject&rdquo; being specified and tested by the passed function value. The
   * passed function value may contain more describers (defined with <code>describe</code>) and/or tests
   * (defined with <code>it</code>). This trait's implementation of this method will register the
   * description string and immediately invoke the passed function.
   *
   * @param description the description text
   */
  protected def feature(description: String)(fun: => Unit) {

    // SKIP-SCALATESTJS-START
    val stackDepth = 4
    val stackDepthAdjustment = -2
    val scopeErrorStackDepth = 4
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepth = 6
    //SCALATESTJS-ONLY val stackDepthAdjustment = -4
    //SCALATESTJS-ONLY val scopeErrorStackDepth = 11

    if (!currentBranchIsTrunk)
      throw new NotAllowedException(Resources.cantNestFeatureClauses, getStackDepthFun(sourceFileName, "feature"))

    try {
      registerNestedBranch(Resources.feature(description.trim), None, fun, Resources.featureCannotAppearInsideAScenario, sourceFileName, "feature", stackDepth, stackDepthAdjustment, None)
    }
    catch {
      case e: exceptions.TestFailedException => throw new exceptions.NotAllowedException(FailureMessages.assertionShouldBePutInsideScenarioClauseNotFeatureClause, Some(e), e => scopeErrorStackDepth)
      case e: exceptions.TestCanceledException => throw new exceptions.NotAllowedException(FailureMessages.assertionShouldBePutInsideScenarioClauseNotFeatureClause, Some(e), e => scopeErrorStackDepth)
      case nae: exceptions.NotAllowedException => throw nae
      case other: Throwable if (!Suite.anExceptionThatShouldCauseAnAbort(other)) => throw new exceptions.NotAllowedException(FailureMessages.exceptionWasThrownInFeatureClause(UnquotedString(other.getClass.getName), description), Some(other), e => scopeErrorStackDepth)
      case other: Throwable => throw other
    }
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>FeatureSpec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>FeatureSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to
   * methods <code>test</code> and <code>ignore</code>.
   * </p>
   *
   * <p>
   * In addition, this trait's implementation will also auto-tag tests with class level annotations.
   * For example, if you annotate @Ignore at the class level, all test methods in the class will be auto-annotated with @Ignore.
   * </p>
   */
  override def tags: Map[String, Set[String]] = autoTagClassAnnotations(atomic.get.tagsMap, this)

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   * @throws NullArgumentException if <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {
    def invokeWithAsyncFixture(theTest: TestLeaf): AsyncOutcome = {
      val theConfigMap = args.configMap
      val testData = testDataFor(testName, theConfigMap)
      FutureOutcome(
        withAsyncFixture(
          new OneArgAsyncTest {
            val name = testData.name

            def apply(fixture: FixtureParam): Future[Outcome] =
              theTest.testFun(fixture).toFutureOutcome

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
   * <p>
   * Run zero to many of this <code>FeatureSpec</code>'s tests.
   * </p>
   *
   * <p>
   * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
   * If <code>testName</code> is <code>Some</code>, this trait's implementation of this method
   * invokes <code>runTest</code> on this object with passed <code>args</code>.
   * </p>
   *
   * <p>
   * This method takes an <code>args</code> that contains a <code>Set</code> of tag names that should be included (<code>tagsToInclude</code>), and a <code>Set</code>
   * that should be excluded (<code>tagsToExclude</code>), when deciding which of this <code>Suite</code>'s tests to execute.
   * If <code>tagsToInclude</code> is empty, all tests will be executed
   * except those those belonging to tags listed in the <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is non-empty, only tests
   * belonging to tags mentioned in <code>tagsToInclude</code>, and not mentioned in <code>tagsToExclude</code>
   * will be executed. However, if <code>testName</code> is <code>Some</code>, <code>tagsToInclude</code> and <code>tagsToExclude</code> are essentially ignored.
   * Only if <code>testName</code> is <code>None</code> will <code>tagsToInclude</code> and <code>tagsToExclude</code> be consulted to
   * determine which of the tests named in the <code>testNames</code> <code>Set</code> should be run. For more information on trait tags, see the main documentation for this trait.
   * </p>
   *
   * <p>
   * If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * invokes <code>testNames</code> on this <code>Suite</code> to get a <code>Set</code> of names of tests to potentially execute.
   * (A <code>testNames</code> value of <code>None</code> essentially acts as a wildcard that means all tests in
   * this <code>Suite</code> that are selected by <code>tagsToInclude</code> and <code>tagsToExclude</code> should be executed.)
   * For each test in the <code>testName</code> <code>Set</code>, in the order
   * they appear in the iterator obtained by invoking the <code>elements</code> method on the <code>Set</code>, this trait's implementation
   * of this method checks whether the test should be run based on the <code>tagsToInclude</code> and <code>tagsToExclude</code> <code>Set</code>s.
   * If so, this implementation invokes <code>runTest</code> with passed in <code>args</code>.
   * </p>
   *
   * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>fixture.FeatureSpec</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   * @throws NullArgumentException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
   */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    runTestsImpl(thisSuite, testName, args, false, parallelAsyncTestExecution, runTest)
  }

  /**
   * An immutable <code>Set</code> of test names. If this <code>FeatureSpec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space.
   * </p>
   *
   * @return the <code>Set</code> of test names
   */
  //override def testNames: Set[String] = ListSet(atomic.get.testsList.map(_.testName): _*)
  override def testNames: Set[String] = {
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  override def run(testName: Option[String], args: Args): Status = {
    runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Registers shared scenarios.
   *
   * <p>
   * This method enables the following syntax for shared scenarios in a <code>FeatureSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * scenariosFor(nonEmptyStack(lastValuePushed))
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared scenarios. For examples of shared scenarios, see the
   * <a href="../FeatureSpec.html#SharedScenarios">Shared scenarios section</a> in the main documentation for
   * trait <code>FeatureSpec</code>.
   * </p>
   */
  protected def scenariosFor(unit: Unit) {}

  import scala.language.implicitConversions

  /**
   * Implicitly converts a function that takes no parameters and results in <code>PendingStatement</code> to
   * a function from <code>FixtureParam</code> to <code>Any</code>, to enable pending tests to registered as by-name parameters
   * by methods that require a test function that takes a <code>FixtureParam</code>.
   *
   * <p>
   * This method makes it possible to write pending tests as simply <code>(pending)</code>, without needing
   * to write <code>(fixture => pending)</code>.
   * </p>
   *
   * @param f a function
   * @return a function of <code>FixtureParam => Any</code>
   */
  protected implicit def convertPendingToFixtureFunction(f: => PendingStatement): FixtureParam => Assertion = {
    fixture => { f; Succeeded }
  }

  // I need this implicit because the function is passed to scenario as the 2nd parameter list, and
  // I can't overload on that. I could if I took the ScenarioWord approach, but that has possibly a worse
  // downside of people could just say scenario("...") and nothing else.
  /**
   * Implicitly converts a function that takes no parameters and results in <code>Any</code> to
   * a function from <code>FixtureParam</code> to <code>Any</code>, to enable no-arg tests to registered
   * by methods that require a test function that takes a <code>FixtureParam</code>.
   *
   * @param fun a function
   * @return a function of <code>FixtureParam => Any</code>
   */
/*
  protected implicit def convertNoArgToFixtureFunction(fun: () => Assertion): (FixtureParam => Assertion) =
    new NoArgTestWrapper(fun)
*/

  /**
   * Suite style name.
   *
   * @return <code>org.scalatest.fixture.FeatureSpec</code>
   */
  final override val styleName: String = "org.scalatest.fixture.FeatureSpec"

  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
