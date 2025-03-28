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
package org.scalatest.funspec

import org.scalactic.{source, Prettifier}
import org.scalatest._
import Suite.autoTagClassAnnotations
import verbs.BehaveWord
import org.scalatest.exceptions._

/**
 * Implementation trait for class <code>AnyFunSpec</code>, which
 * facilitates a &ldquo;behavior-driven&rdquo; style of development (BDD),
 * in which tests are combined with text that specifies the behavior the tests
 * verify.
 * 
 * <p>
 * <a href="AnyFunSpec.html"><code>AnyFunSpec</code></a> is a class, not a trait,
 * to minimize compile time given there is a slight compiler overhead to
 * mixing in traits compared to extending classes. If you need to mix the
 * behavior of <code>AnyFunSpec</code> into some other class, you can use this
 * trait instead, because class <code>AnyFunSpec</code> does nothing more than
 * extend this trait and add a nice <code>toString</code> implementation.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="AnyFunSpec.html">detailed
 * overview of <code>AnyFunSpec</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.FunSpecFinder"))
//SCALATESTJS-ONLY @scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
//SCALATESTNATIVE-ONLY @scala.scalanative.reflect.annotation.EnableReflectiveInstantiation
trait AnyFunSpecLike extends TestSuite with Informing with Notifying with Alerting with Documenting { thisSuite =>

  private final val engine = new Engine(Resources.concurrentSpecMod, "FunSpec")
  import engine._

  // TODO: Probably make this private final val sourceFileName in a singleton object so it gets compiled in rather than carried around in each instance
  private[scalatest] val sourceFileName = "AnyFunSpecLike.scala"

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
   * <code>AnyFunSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * print to the standard output. This method can be called safely by any thread.
   */
  protected def note: Notifier = atomicNotifier.get

  /**
   * Returns an <code>Alerter</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>AnyFunSpec</code> is being executed, such as from inside a test function, it will forward the information to
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

  private final def registerTestImpl(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -2
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -5
    engine.registerTest(testText, Transformer(() => testFun), Resources.testCannotBeNestedInsideAnotherTest, sourceFileName, "registerTest", 5, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {  // Note: we can't remove the implicit pos here because it is the signature of registerTest in TestRegistration.
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestImpl(testText, testTags: _*)(testFun, pos) }) } 
  //DOTTY-ONLY }

  private final def registerIgnoredTestImpl(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -3
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -5
    engine.registerIgnoredTest(testText, Transformer(() => testFun), Resources.testCannotBeNestedInsideAnotherTest, sourceFileName, "registerIgnoredTest", 4, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerIgnoredTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {  // Note: we can't remove the implicit pos here because it is the signature of registerTest in TestRegistration.
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerIgnoredTestImpl(testText, testTags: _*)(testFun, pos) }) } 
  //DOTTY-ONLY }

  /**
   * Class that, via an instance referenced from the <code>it</code> field,
   * supports test (and shared test) registration in <code>AnyFunSpec</code>s.
   *
   * <p>
   * This class supports syntax such as the following test registration:
   * </p>
   *
   * <pre class="stExamples">
   * it("should be empty")
   * ^
   * </pre>
   *
   * <p>
   * and the following shared test registration:
   * </p>
   *
   * <pre class="stExamples">
   * it should behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples, see the <a href="AnyFunSpec.html">main documentation for <code>AnyFunSpec</code></a>.
   * </p>
   */
  protected class ItWord {

    private final def applyImpl(specText: String, testTags: Tag*)(testFun: => Any /* Assertion */, pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -4
      engine.registerTest(specText, Transformer(() => testFun), Resources.itCannotAppearInsideAnotherItOrThey, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
    }

    /**
     * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
     * An invocation of this method is called an &ldquo;example.&rdquo;
     *
     * This method will register the test for later execution via an invocation of one of the <code>execute</code>
     * methods. The name of the test will be a concatenation of the text of all surrounding describers,
     * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
     * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
     * this <code>AnyFunSpec</code> instance.
     *
     * @param specText the specification text, which will be combined with the descText of any surrounding describers
     * to form the test name
     * @param testTags the optional list of tags for this test
     * @param testFun the test function
     * @throws DuplicateTestNameException if a test with the same name has been registered previously
     * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
     * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
     */
    // SKIP-DOTTY-START
    def apply(specText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -4
      applyImpl(specText, testTags: _*)(testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline def apply(specText: String, testTags: Tag*)(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => applyImpl(specText, testTags: _*)(testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of shared tests.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stExamples">
     * it should behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFunSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFunSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix 
    def should(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stExamples">
     * it must behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFunSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFunSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix 
    def must(behaveWord: BehaveWord) = behaveWord
  }

  /**
   * Supports test (and shared test) registration in <code>AnyFunSpec</code>s.
   *
   * <p>
   * This field supports syntax such as the following:
   * </p>
   *
   * <pre class="stExamples">
   * it("should be empty")
   * ^
   * </pre>
   *
   * <pre> class="stExamples"
   * it should behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation for this trait.
   * </p>
   */
  protected val it = new ItWord
  
  /**
   * Class that, via an instance referenced from the <code>they</code> field,
   * supports test (and shared test) registration in <code>AnyFunSpec</code>s.
   *
   * <p>
   * This class supports syntax such as the following test registration:
   * </p>
   *
   * <pre class="stExamples">
   * they("should be empty")
   * ^
   * </pre>
   *
   * <p>
   * and the following shared test registration:
   * </p>
   *
   * <pre class="stExamples">
   * they should behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples, see the <a href="AnyFunSpec.html">main documentation for <code>AnyFunSpec</code></a>.
   * </p>
   */
  protected class TheyWord {

    private final def applyImpl(specText: String, testTags: Tag*)(testFun: => Any /* Assertion */, pos: source.Position): Unit = {
      engine.registerTest(specText, Transformer(() => testFun), Resources.theyCannotAppearInsideAnotherItOrThey, sourceFileName, "apply", 3, -2, None, None, Some(pos), None, testTags: _*)
    }

    /**
     * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
     * An invocation of this method is called an &ldquo;example.&rdquo;
     *
     * This method will register the test for later execution via an invocation of one of the <code>execute</code>
     * methods. The name of the test will be a concatenation of the text of all surrounding describers,
     * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
     * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
     * this <code>AnyFunSpec</code> instance.
     *
     * @param specText the specification text, which will be combined with the descText of any surrounding describers
     * to form the test name
     * @param testTags the optional list of tags for this test
     * @param testFun the test function
     * @throws DuplicateTestNameException if a test with the same name has been registered previously
     * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
     * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
     */
    // SKIP-DOTTY-START
    def apply(specText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      applyImpl(specText, testTags: _*)(testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline def apply(specText: String, testTags: Tag*)(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => applyImpl(specText, testTags: _*)(testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of shared tests.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stExamples">
     * they should behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFunSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFunSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix 
    def should(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stExamples">
     * they must behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFunSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFunSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix 
    def must(behaveWord: BehaveWord) = behaveWord
  }

  /**
   * Supports test (and shared test) registration in <code>AnyFunSpec</code>s.
   *
   * <p>
   * This field supports syntax such as the following:
   * </p>
   *
   * <pre class="stExamples">
   * they("should be empty")
   * ^
   * </pre>
   *
   * <pre> class="stExamples"
   * it should behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation for this trait.
   * </p>
   */
  protected val they = new TheyWord

  private final def ignoreImpl(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepth = 4
    val stackDepthAdjustment = -3
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
    engine.registerIgnoredTest(testText, Transformer(() => testFun), Resources.ignoreCannotAppearInsideAnItOrAThey, sourceFileName, "ignore", stackDepth, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>AnyFunSpec</code> instance.
   *
   * @param testText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  // SKIP-DOTTY-START
  protected def ignore(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    ignoreImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def ignore(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */): Unit = {
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => ignoreImpl(testText, testTags: _*)(testFun, pos) }) } 
  //DOTTY-ONLY }

  private final def describeImpl(description: String)(fun: => Unit, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepth = 4
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
    try {
      registerNestedBranch(description, None, fun, Resources.describeCannotAppearInsideAnIt, sourceFileName, "describe", stackDepth, -2, None, Some(pos))
    }
    catch {
      case e: TestFailedException => throw new NotAllowedException(FailureMessages.assertionShouldBePutInsideItOrTheyClauseNotDescribeClause, Some(e), e.position.getOrElse(pos))
      case e: TestCanceledException => throw new NotAllowedException(FailureMessages.assertionShouldBePutInsideItOrTheyClauseNotDescribeClause, Some(e), e.position.getOrElse(pos))
      case e: DuplicateTestNameException => throw new NotAllowedException(FailureMessages.exceptionWasThrownInDescribeClause(Prettifier.default, UnquotedString(e.getClass.getName), description, e.getMessage), Some(e), e.position.getOrElse(pos))
      case other: Throwable if (!Suite.anExceptionThatShouldCauseAnAbort(other)) => throw new NotAllowedException(FailureMessages.exceptionWasThrownInDescribeClause(Prettifier.default, UnquotedString(other.getClass.getName), description, other.getMessage), Some(other), pos)
      case other: Throwable => throw other
    }
  }

  /**
   * Describe a &ldquo;subject&rdquo; being specified and tested by the passed function value. The
   * passed function value may contain more describers (defined with <code>describe</code>) and/or tests
   * (defined with <code>it</code>). This trait's implementation of this method will register the
   * description string and immediately invoke the passed function.
   */
  // SKIP-DOTTY-START
  protected def describe(description: String)(fun: => Unit)(implicit pos: source.Position): Unit = {
    describeImpl(description)(fun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def describe(description: String)(fun: => Unit): Unit = {
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => describeImpl(description)(fun, pos) }) }
  //DOTTY-ONLY }

  /**
   * An immutable <code>Set</code> of test names. If this <code>AnyFunSpec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space. For example, consider this <code>AnyFunSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.funspec.AnyFunSpec
   *
   * class StackSpec extends AnyFunSpec {
   *   describe("A Stack") {
   *     describe("(when not empty)") {
   *       it("must allow me to pop") {}
   *     }
   *     describe("(when not full)") {
   *       it("must allow me to push") {}
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Invoking <code>testNames</code> on this <code>AnyFunSpec</code> will yield a set that contains the following
   * two test name strings:
   * </p>
   *
   * <pre class="stExamples">
   * "A Stack (when not empty) must allow me to pop"
   * "A Stack (when not full) must allow me to push"
   * </pre>
   */
  override def testNames: Set[String] = {
    InsertionOrderSet(atomic.get.testNamesList)
  }

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   *
   * @throws NullArgumentException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {

    def invokeWithFixture(theTest: TestLeaf): Outcome = {
      val theConfigMap = args.configMap
      val testData = testDataFor(testName, theConfigMap)
      withFixture(
        new NoArgTest {
          val name = testData.name
          def apply(): Outcome = { theTest.testFun() }
          val configMap = testData.configMap
          val scopes = testData.scopes
          val text = testData.text
          val tags = testData.tags
          val pos = testData.pos
        }
      )
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> names of tagged tests and whose associated values are
   * the <code>Set</code> of tags for the test. If this <code>AnyFunSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to 
   * methods <code>it</code> and <code>ignore</code>. 
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
   * Run zero to many of this <code>AnyFunSpec</code>'s tests.
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
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  override def run(testName: Option[String], args: Args): Status = {
    runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Supports shared test registration in <code>AnyFunSpec</code>s.
   *
   * <p>
   * This field supports syntax such as the following:
   * </p>
   *
   * <pre class="stExamples">
   * it should behave like nonFullStack(stackWithOneItem)
   *           ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of <cod>behave</code>, see the <a href="#sharedTests">Shared tests section</a>
   * in the main documentation for this trait.
   * </p>
   */
  protected val behave = new BehaveWord
    
  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
