/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.funsuite

import org.scalatest._
import org.scalactic.source
import org.scalatest.Suite.autoTagClassAnnotations

/**
  * Implementation trait for class <code>FixtureAnyFunSuite</code>, which is
  * a sister class to <code>org.scalatest.funsuite.AnyFunSuite</code> that can pass a
  * fixture object into its tests.
  *
  * <p>
  * <a href="FixtureAnyFunSuite.html"><code>FixtureAnyFunSuite</code></a> is a class,
  * not a trait, to minimize compile time given there is a slight compiler
  * overhead to mixing in traits compared to extending classes. If you need
  * to mix the behavior of <code>FixtureAnyFunSuite</code> into some other
  * class, you can use this trait instead, because class
  * <code>FixtureAnyFunSuite</code> does nothing more than extend this trait and add a nice <code>toString</code> implementation.
  * </p>
  *
  * <p>
  * See the documentation of the class for a <a href="FixtureAnyFunSuite.html">detailed
  * overview of <code>FixtureAnyFunSuite</code></a>.
  * </p>
  *
  * @author Bill Venners
  */
//SCALATESTJS-ONLY @scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
//SCALATESTNATIVE-ONLY @scala.scalanative.reflect.annotation.EnableReflectiveInstantiation
@Finders(Array("org.scalatest.finders.FunSuiteFinder"))
trait FixtureAnyFunSuiteLike extends org.scalatest.FixtureTestSuite with Informing with Notifying with Alerting with Documenting { thisSuite =>

  private final val engine = new FixtureEngine[FixtureParam](Resources.concurrentFixtureFunSuiteMod, "FixtureFunSuite")

  import engine._

  private[scalatest] val sourceFileName = "FixtureAnyFunSuite.scala"

  /**
    * Returns an <code>Informer</code> that during test execution will forward strings passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked from inside a scope,
    * it will forward the information to the current reporter immediately.  If invoked from inside a test function,
    * it will record the information and forward it to the current reporter only after the test completed, as <code>recordedEvents</code>
    * of the test completed event, such as <code>TestSucceeded</code>. If invoked at any other time, it will print to the standard output.
    * This method can be called safely by any thread.
    */
  protected def info: Informer = atomicInformer.get

  /**
    * Returns a <code>Notifier</code> that during test execution will forward strings (and other objects) passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked while this
    * <code>FixtureAnyFunSuite</code> is being executed, such as from inside a test function, it will forward the information to
    * the current reporter immediately. If invoked at any other time, it will
    * print to the standard output. This method can be called safely by any thread.
    */
  protected def note: Notifier = atomicNotifier.get

  /**
    * Returns an <code>Alerter</code> that during test execution will forward strings (and other objects) passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked while this
    * <code>FixtureAnyFunSuite</code> is being executed, such as from inside a test function, it will forward the information to
    * the current reporter immediately. If invoked at any other time, it will
    * print to the standard output. This method can be called safely by any thread.
    */
  protected def alert: Alerter = atomicAlerter.get

  /**
    * Returns a <code>Documenter</code> that during test execution will forward strings passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked from inside a scope,
    * it will forward the information to the current reporter immediately.  If invoked from inside a test function,
    * it will record the information and forward it to the current reporter only after the test completed, as <code>recordedEvents</code>
    * of the test completed event, such as <code>TestSucceeded</code>. If invoked at any other time, it will print to the standard output.
    * This method can be called safely by any thread.
    */
  protected def markup: Documenter = atomicDocumenter.get

  private final def registerTestImpl(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -1
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -4
    engine.registerTest(testText, org.scalatest.fixture.Transformer(testFun), Resources.testCannotBeNestedInsideAnotherTest, "FunSuite.scala", "registerTest", 4, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestImpl(testText, testTags: _*)(testFun, pos) }) } 
  //DOTTY-ONLY }

  private final def registerIgnoredTestImpl(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -4
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -5
    engine.registerIgnoredTest(testText, org.scalatest.fixture.Transformer(testFun), Resources.testCannotBeNestedInsideAnotherTest, "FunSuite.scala", "registerIgnoredTest", 4, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerIgnoredTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerIgnoredTestImpl(testText, testTags: _*)(testFun, pos) }) } 
  //DOTTY-ONLY }

  class ResultOfTestInvocation(testName: String, testTags: Tag*) {
    
    private final def applyImpl(testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -5
      engine.registerTest(testName, org.scalatest.fixture.Transformer(testFun), Resources.testCannotAppearInsideAnotherTest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
    }
    
    // SKIP-DOTTY-START
    final def apply(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      applyImpl(testFun: FixtureParam => Any /* Assertion */, pos: source.Position)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline def apply(testFun: FixtureParam => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => applyImpl(testFun: FixtureParam => Any /* Assertion */, pos: source.Position) }) } 
    //DOTTY-ONLY }

    private final def applyImpl(testFun: () => Any /* Assertion */, pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
      engine.registerTest(testName, org.scalatest.fixture.Transformer(new org.scalatest.fixture.NoArgTestWrapper(testFun)), Resources.testCannotAppearInsideAnotherTest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
    }

    // SKIP-DOTTY-START
    final def apply(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      applyImpl(testFun: () => Any /* Assertion */, pos: source.Position)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline def apply(testFun: () => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => applyImpl(testFun: () => Any /* Assertion */, pos: source.Position) }) } 
    //DOTTY-ONLY }
  }

  /**
    * Register a test with the specified name, optional tags, and function value that takes no arguments.
    * This method will register the test for later execution via an invocation of one of the <code>run</code>
    * methods. The passed test name must not have been registered previously on
    * this <code>FixtureAnyFunSuite</code> instance.
    *
    * @param testName the name of the test
    * @param testTags the optional list of tags for this test
    * @param testFun the test function
    * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
    * @throws DuplicateTestNameException if a test with the same name has been registered previously
    * @throws NotAllowedException if <code>testName</code> had been registered previously
    * @throws NullArgumentException if <code>testName</code> or any passed test tag is <code>null</code>
    */
  protected def test(testName: String, testTags: Tag*): ResultOfTestInvocation = new ResultOfTestInvocation(testName, testTags: _*)

  class ResultOfIgnoreInvocation(testName: String, testTags: Tag*) {
    
    private final def applyImpl(testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -3
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
      engine.registerIgnoredTest(testName, org.scalatest.fixture.Transformer(testFun), Resources.ignoreCannotAppearInsideATest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, Some(pos), testTags: _*)
    }
    
    // SKIP-DOTTY-START
    final def apply(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      applyImpl(testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline def apply(testFun: FixtureParam => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => applyImpl(testFun, pos) }) } 
    //DOTTY-ONLY }

    private final def applyImpl(testFun: () => Any /* Assertion */, pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -3
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
      engine.registerIgnoredTest(testName, org.scalatest.fixture.Transformer(new org.scalatest.fixture.NoArgTestWrapper(testFun)), Resources.ignoreCannotAppearInsideATest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, Some(pos), testTags: _*)
    }

    // SKIP-DOTTY-START
    final def apply(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      applyImpl(testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline def apply(testFun: () => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => applyImpl(testFun, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
    * Register a test to ignore, which has the specified name, optional tags, and function value that takes no arguments.
    * This method will register the test for later ignoring via an invocation of one of the <code>run</code>
    * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>test</code>
    * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be run, but a
    * report will be sent that indicates the test was ignored. The passed test name must not have been registered previously on
    * this <code>FixtureAnyFunSuite</code> instance.
    *
    * @param testName the name of the test
    * @param testTags the optional list of tags for this test
    * @param testFun the test function
    * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
    * @throws DuplicateTestNameException if a test with the same name has been registered previously
    * @throws NotAllowedException if <code>testName</code> had been registered previously
    */
  protected def ignore(testName: String, testTags: Tag*): ResultOfIgnoreInvocation = new ResultOfIgnoreInvocation(testName, testTags: _*)

  /**
    * An immutable <code>Set</code> of test names. If this <code>FixtureAnyFunSuite</code> contains no tests, this method returns an empty <code>Set</code>.
    *
    * <p>
    * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's iterator will
    * return those names in the order in which the tests were registered.
    * </p>
    *
    * @return the <code>Set</code> of test names
    */
  override def testNames: Set[String] = {
    InsertionOrderSet(atomic.get.testNamesList)
  }

  /**
    * Run a test. This trait's implementation runs the test registered with the name specified by <code>testName</code>.
    *
    * @param testName the name of one test to run.
    * @param args the <code>Args</code> for this run
    * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
    * @throws IllegalArgumentException if <code>testName</code> is defined but a test with that name does not exist on this <code>FixtureAnyFunSuite</code>
    * @throws NullArgumentException if <code>testName</code> or <code>args</code> is <code>null</code>.
    */
  protected override def runTest(testName: String, args: Args): Status = {

    def invokeWithFixture(theTest: TestLeaf): Outcome = {
      theTest.testFun match {
        case transformer: org.scalatest.fixture.Transformer[_] =>
          transformer.exceptionalTestFun match {
            case wrapper: fixture.NoArgTestWrapper[_, _] =>
              withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
            case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
          }
        case other =>
          other match {
            case wrapper: fixture.NoArgTestWrapper[_, _] =>
              withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
            case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
          }
      }
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
    * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>FixtureAnyFunSuite</code> belong, and values
    * the <code>Set</code> of test names that belong to each tag. If this <code>FixtureAnyFunSuite</code> contains no tags, this method returns an empty
    * <code>Map</code>.
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
    * <p>
    * Run zero to many of this <code>FixtureAnyFunSuite</code>'s tests.
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
    * If so, this implementation invokes <code>runTest</code> with passed <code>args</code>.
    * </p>
    *
    * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
    *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>AnyFunSpec</code>.
    * @param args the <code>Args</code> to which results will be reported
    * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
    * @throws NullArgumentException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
    */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  override def run(testName: Option[String], args: Args): Status = {
    runImpl(thisSuite, testName, args: Args, super.run)
  }

  /**
    * Registers shared tests.
    *
    * <p>
    * This method enables the following syntax for shared tests in a <code>FixtureAnyFunSuite</code>:
    * </p>
    *
    * <pre class="stHighlight">
    * testsFor(nonEmptyStack(lastValuePushed))
    * </pre>
    *
    * <p>
    * This method just provides syntax sugar intended to make the intent of the code clearer.
    * Because the parameter passed to it is
    * type <code>Unit</code>, the expression will be evaluated before being passed, which
    * is sufficient to register the shared tests. For examples of shared tests, see the
    * <a href="AnyFunSuite.html#SharedTests">Shared tests section</a> in the main documentation for
    * trait <code>AnyFunSuite</code>.
    * </p>
    */
  protected def testsFor(unit: Unit): Unit = {}

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
  protected implicit def convertPendingToFixtureFunction(f: => PendingStatement): (FixtureParam => Any /* Assertion */) = {
    fixture => { f; Succeeded }
  }

  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
