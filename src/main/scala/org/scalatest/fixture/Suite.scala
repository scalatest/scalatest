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
import collection.immutable.TreeSet
// import Suite._
import java.lang.reflect.{InvocationTargetException, Method, Modifier}
import org.scalatest.events._
import org.scalatest.Suite._
import exceptions.{TestCanceledException, TestPendingException}
import OutcomeOf.outcomeOf

/**
 * Base trait for a family of style traits that can pass a fixture object into tests.
 *
 * <p>
 * <strong>Prior to ScalaTest 2.0.M4, trait <code>fixture.Suite</code> served two purposes: 1) It served as the base
 * class of ScalaTest's family of "fixture" style traits, and 2) It was itself a style trait in which tests are methods
 * that take a fixture parameter. Although it will continue to serve its first purpose, <code>fixture.Suite</code> has
 * been deprecated as a style trait. Pre-existing code that used <code>fixture.Suite</code> as a style trait to define
 * tests as methods will continue to work during the deprecation period, but will generate a deprecation warning. Please
 * change all such uses of <code>fixture.Suite</code> to use trait <a href="Spec.html"><code>fixture.Spec</code></a> instead.</strong>
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.MethodFinder"))
trait Suite extends org.scalatest.Suite { thisSuite =>

  /**
   * The type of the fixture parameter that can be passed into tests in this suite.
   */
  protected type FixtureParam

  /**
   * A test function taking a fixture parameter and returning an <code>Outcome</code>.
   *
   * <p>
   * For more detail and examples, see the
   * <a href="FlatSpec.html">documentation for trait <code>fixture.FlatSpec</code></a>.
   * </p>
   */
  protected trait OneArgTest extends (FixtureParam => Outcome) with TestData { thisOneArgTest =>

    /**
     * Runs the test, using the passed <code>FixtureParam</code>.
     *
     * @param fixture the <code>FixtureParam</code>
     * @return an instance of <code>Outcome</code>
     */
    def apply(fixture: FixtureParam): Outcome

    /**
     * Convert this <code>OneArgTest</code> to a <code>NoArgTest</code> whose
     * <code>name</code> and <code>configMap</code> methods return the same values
     * as this <code>OneArgTest</code>, and whose <code>apply</code> method invokes
     * this <code>OneArgTest</code>'s apply method,
     * passing in the given <code>fixture</code>.
     *
     * <p>
     * This method makes it easier to invoke the <code>withFixture</code> method
     * that takes a <code>NoArgTest</code>. For example, if a <code>fixture.Suite</code> 
     * mixes in <code>SeveredStackTraces</code>, it will inherit an implementation
     * of <code>withFixture(NoArgTest)</code> provided by
     * <code>SeveredStackTraces</code> that implements the stack trace severing
     * behavior. If the <code>fixture.Suite</code> does not delegate to that
     * <code>withFixture(NoArgTest)</code> method, the stack trace severing behavior
     * will not happen. Here's how that might look in a <code>fixture.Suite</code>
     * whose <code>FixtureParam</code> is <code>StringBuilder</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * def withFixture(test: OneArgTest) = {
     *   withFixture(test.toNoArgTest(new StringBuilder))
     * }
     * </pre>
     *
     * <p>
     * Invoking this method has no side effect. It just returns a <code>NoArgTest</code> whose
     * <code>apply</code> method invokes <code>apply</code> on this <code>OneArgTest</code>, passing
     * in the <code>FixtureParam</code> passed to <code>toNoArgTest</code>.
     * </p>
     *
     * @param fixture the <code>FixtureParam</code>
     * @return an new instance of <code>NoArgTest</code>
     */
    def toNoArgTest(fixture: FixtureParam) = 
      new NoArgTest {
        val name = thisOneArgTest.name
        val configMap = thisOneArgTest.configMap
        def apply(): Outcome = { thisOneArgTest(fixture) }
        val scopes = thisOneArgTest.scopes
        val text = thisOneArgTest.text
        val tags = thisOneArgTest.tags
      }
  }

  /**
   * Companion object for <code>OneArgTest</code> that provides factory method to create new <code>OneArgTest</code>
   * instance by passing in a <code>OneArgTest</code> and a <code>FixtureParam</code> => <code>Outcome</code> function.
   */
  object OneArgTest {
    /**
     * Create new <code>OneArgTest</code> instance.
     *
     * @param test a <code>OneArgTest</code>
     * @param f a <code>FixtureParam</code> => <code>Outcome</code> function
     * @return a new instance of <code>OneArgTest</code>, which will call the passed <code>f</code> function in its <code>apply</code> method
     */
    def apply(test: OneArgTest)(f: FixtureParam => Outcome): OneArgTest = {
      new OneArgTest {
        def apply(fixture: FixtureParam): Outcome = { f(fixture) }
        val text: String = test.text
        val configMap: ConfigMap = test.configMap
        val scopes: collection.immutable.IndexedSeq[String] = test.scopes
        val name: String = test.name
        val tags: Set[String] = test.tags
      }
    }
  }

  /**
   *  Run the passed test function with a fixture created by this method.
   *
   * <p>
   * This method should create the fixture object needed by the tests of the
   * current suite, invoke the test function (passing in the fixture object),
   * and if needed, perform any clean up needed after the test completes.
   * For more detail and examples, see the <a href="Suite.html">main documentation for this trait</a>.
   * </p>
   *
   * @param test the <code>OneArgTest</code> to invoke, passing in a fixture
   * @return an instance of <code>Outcome</code>
   */
  protected def withFixture(test: OneArgTest): Outcome

  private[fixture] class TestFunAndConfigMap(val name: String, test: FixtureParam => Any, val configMap: ConfigMap)
    extends OneArgTest {
    
    def apply(fixture: FixtureParam): Outcome = {
      outcomeOf { test(fixture) }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
  }

  private[fixture] class FixturelessTestFunAndConfigMap(override val name: String, test: () => Any, override val configMap: ConfigMap)
    extends NoArgTest {

    def apply(): Outcome = {
      outcomeOf { test() }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
  }

  /**
   * A <code>Set</code> of test names. If this <code>fixture.Suite</code> contains no tests, this method returns an empty <code>Set</code>.
   *
   * <p>
   * <strong><code>Suite</code> has been deprecated as a style trait. During the deprecation period, the following behavior will continue
   * to work as before, but will go away at the conclusion of the deprecation period:</strong>
   * This trait's implementation of this method uses Java reflection to discover all public methods whose name starts with <code>"test"</code>,
   * which take either nothing, a single <code>Informer</code>, a single <code>FixtureParam</code> or two parameters of type <code>FixtureParam</code>
   * and <code>Informer</code>. For each discovered test method, it assigns a test name
   * comprised of just the method name if the method takes no parameters, or the method name plus <code>(Informer)</code> or <code>FixtureParam</code>
   * if the method takes a <code>Informer</code> or <code>FixtureParam</code>.
   * Here are a few method signatures and the names that this trait's implementation assigns them:
   * </p>
   *
   * <pre class="stHighlight">
   * def testCat() {}         // test name: "testCat"
   * def testCat(Informer) {} // test name: "testCat(Informer)"
   * def testCat(FixtureParam) {} // test name: "testCat(FixtureParam)"
   * def testCat(FixtureParam, Informer) {} // test name: "testCat(FixtureParam, Informer)"
   * def testDog() {}         // test name: "testDog"
   * def testDog(Informer) {} // test name: "testDog(Informer)"
   * def testDog(FixtureParam) {} // test name: "testDog(FixtureParam)"
   * def testDog(FixtureParam, Informer) {} // test name: "testDog(FixtureParam, Informer)"
   * def test() {}            // test name: "test"
   * def test(Informer) {}    // test name: "test(Informer)"
   * def test(FixtureParam) {}    // test name: "test(FixtureParam)"
   * def test(FixtureParam, Informer) {}    // test name: "test(FixtureParam, Informer)"
   * </pre>
   *
   * <p>
   * This trait's implementation of this method returns an immutable <code>Set</code> of all such names, excluding the name
   * <code>testNames</code>. The iterator obtained by invoking <code>elements</code> on this
   * returned <code>Set</code> will produce the test names in their <em>natural order</em>, as determined by <code>String</code>'s
   * <code>compareTo</code> method.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTests</code> invokes this method
   * and calls <code>runTest</code> for each test name in the order they appear in the returned <code>Set</code>'s iterator.
   * Although this trait's implementation of this method returns a <code>Set</code> whose iterator produces <code>String</code>
   * test names in a well-defined order, the contract of this method does not required a defined order. Subclasses are free to
   * override this method and return test names in an undefined order, or in a defined order that's different from <code>String</code>'s
   * natural order.
   * </p>
   *
   * <p>
   * Subclasses may override this method to produce test names in a custom manner. One potential reason to override <code>testNames</code> is
   * to run tests in a different order, for example, to ensure that tests that depend on other tests are run after those other tests.
   * Another potential reason to override is allow tests to be defined in a different manner, such as methods annotated <code>@Test</code> annotations
   * (as is done in <code>JUnitSuite</code> and <code>TestNGSuite</code>) or test functions registered during construction (as is
   * done in <code>FunSuite</code> and <code>FunSpec</code>).
   * </p>
   *
   * <p>
   * In ScalaTest's event model, a test may be surrounded by &ldquo;scopes.&rdquo; Each test and scope is associated with string of text.
   * A test's name is concatenation of the text of any surrounding scopes followed by the text provided with the test
   * itself, after each text element has been trimmed and one space inserted between each component. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * package org.scalatest.examples.fixture.freespec
   *
   * import org.scalatest._
   *
   * class SetSpec extends fixture.FreeSpec with fixture.UnitFixture {
   *
   *   "A Set" - {
   *     "when empty" - {
   *       "should have size 0" in {
   *         assert(Set.empty.size === 0)
   *       }
   *
   *       "should produce NoSuchElementException when head is invoked" in {
   *         intercept[NoSuchElementException] {
   *           Set.empty.head
   *         }
   *       }
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * The above <code>FreeSpec</code> contains two tests, both nested inside the same two scopes. The outermost scope names
   * the subject, <code>A Set</code>. The nested scope qualifies the subject with <code>when empty</code>. Inside that
   * scope are the two tests. The text of the tests are:
   * <p>
   *
   * <ul>
   * <li><code>should have size 0</code></li>
   * <li><code>should produce NoSuchElementException when head is invoked</code></li>
   * </ul>
   *
   * <p>
   * Therefore, the names of these two tests are:
   * </p>
   *
   * <ul>
   * <li><code>A Stack when empty should have size 0</code></li>
   * <li><code>A Stack when empty should produce NoSuchElementException when head is invoked</code></li>
   * </ul>
   *
   * <p>
   * Note that because the component scope and test text strings are trimmed, any leading or trailing space will be dropped
   * before they are strung together to form the test name, with each trimmed component separated by a space. If the scopes
   * in the above example had text <code>" A Set "</code> and <code>" when empty "</code>, and the first test had text
   * <code>" should have size 0 "</code>, its test name would still be the same, "A Set when empty should have size 0"</code>.
   * </p>
   */
  override def testNames: Set[String] = {

    def takesTwoParamsOfTypesAnyAndInformer(m: Method) = {
      val paramTypes = m.getParameterTypes
      val hasTwoParams = paramTypes.length == 2
      hasTwoParams && classOf[Informer].isAssignableFrom(paramTypes(1))
    }

    def takesOneParamOfAnyType(m: Method) = m.getParameterTypes.length == 1

    def isTestMethod(m: Method) = {

      // Factored out to share code with Suite.testNames
      val (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags, isTestDataFor) = isTestMethodGoodies(m)

      // Also, will discover both
      // testNames(Object) and testNames(Object, Informer). Reason is if I didn't discover these
      // it would likely just be silently ignored, and that might waste users' time
      isInstanceMethod && (firstFour == "test") && !isTestDataFor && ((hasNoParams && !isTestNames && !isTestTags) ||
          takesInformer(m) || takesOneParamOfAnyType(m) || takesTwoParamsOfTypesAnyAndInformer(m))
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m)) yield
        if (takesInformer(m))
          m.getName + InformerInParens
        else if (takesOneParamOfAnyType(m))
          m.getName + FixtureInParens
        else if (takesTwoParamsOfTypesAnyAndInformer(m))
          m.getName + FixtureAndInformerInParens
        else m.getName

    TreeSet[String]() ++ testNameArray
  }

  /**
   * Run a test.
   *
   * <p>
   * This trait's implementation uses Java reflection to invoke on this object the test method identified by the passed <code>testName</code>.
   * </p>
   *
   * <p>
   * Implementations of this method are responsible for ensuring a <code>TestStarting</code> event
   * is fired to the <code>Reporter</code> before executing any test, and either <code>TestSucceeded</code>,
   * <code>TestFailed</code>, <code>TestPending</code> or <code>TestCanceled</code> after executing any nested
   * <code>Suite</code>. (If a test is marked with the <code>org.scalatest.Ignore</code> tag, the
   * <code>runTests</code> method is responsible for ensuring a <code>TestIgnored</code> event is fired and that
   * this <code>runTest</code> method is not invoked for that ignored test.)
   * </p>
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   *
   * @throws NullPointerException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected override def runTest(testName: String, args: Args): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val (stopRequested, report, method, testStartTime) =
      getSuiteRunTestGoodies(thisSuite, stopper, reporter, testName)

    reportTestStarting(thisSuite, report, tracker, testName, testName, thisSuite.rerunner, Some(getTopOfMethod(thisSuite, testName)))

    val formatter = getEscapedIndentedTextForTest(testName, 1, true)

    val messageRecorderForThisTest = new MessageRecorder(report)
    val informerForThisTest =
      MessageRecordingInformer(
        messageRecorderForThisTest, 
        (message, payload, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, payload, 2, location, isConstructingThread, true)
      )

    val documenterForThisTest =
      MessageRecordingDocumenter(
        messageRecorderForThisTest, 
        (message, _, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, None, 2, location, isConstructingThread, true) // TODO: Need a test that fails because testWasCanceleed isn't being passed
      )

// TODO: Use a message recorder in FixtureSuite. Maybe just allow the state and
// use Engine in Suite, though then I'd have two Engines in everything. Or even three down here.
// Nah, go ahead and use message recording informer here, and maybe find some other way to
// reduce the duplication between Suite, FixtureSuite, and Engine.
    try {
      if (testMethodTakesAFixtureAndInformer(testName) || testMethodTakesAFixture(testName)) {
        val testFun: FixtureParam => Unit = {
          (fixture: FixtureParam) => {
            val anyRefFixture: AnyRef = fixture.asInstanceOf[AnyRef] // TODO zap this cast
            val args: Array[Object] =
              if (testMethodTakesAFixtureAndInformer(testName)) {
                Array(anyRefFixture, informerForThisTest)
              }
              else
                Array(anyRefFixture)

            method.invoke(thisSuite, args: _*)
          }
        }
        withFixture(new TestFunAndConfigMap(testName, testFun, configMap)).toUnit
      }
      else { // Test method does not take a fixture
        val testFun: () => Unit = {
          () => {
            val args: Array[Object] =
              if (testMethodTakesAnInformer(testName)) 
                Array(informerForThisTest)
              else
                Array()

            method.invoke(this, args: _*)
          }
        }
        withFixture(new FixturelessTestFunAndConfigMap(testName, testFun, configMap)).toUnit
      }

      val duration = System.currentTimeMillis - testStartTime
      reportTestSucceeded(thisSuite, report, tracker, testName, testName, messageRecorderForThisTest.recordedEvents(false, false), duration, formatter, thisSuite.rerunner, Some(getTopOfMethod(thisSuite, method)))
      SucceededStatus
    }
    catch { 
      case ite: InvocationTargetException =>
        val t = ite.getTargetException
        t match {
          case _: TestPendingException =>
            val duration = System.currentTimeMillis - testStartTime
            // testWasPending = true so info's printed out in the finally clause show up yellow
            reportTestPending(thisSuite, report, tracker, testName, testName, messageRecorderForThisTest.recordedEvents(true, false), duration, formatter, Some(getTopOfMethod(thisSuite, method)))
            SucceededStatus
          case e: TestCanceledException =>
            val duration = System.currentTimeMillis - testStartTime
            val message = getMessageForException(e)
            val formatter = getEscapedIndentedTextForTest(testName, 1, true)
            // testWasCanceled = true so info's printed out in the finally clause show up yellow
            report(TestCanceled(tracker.nextOrdinal(), message, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), testName, testName, messageRecorderForThisTest.recordedEvents(false, true), Some(e), Some(duration), Some(formatter), Some(getTopOfMethod(thisSuite, method)), thisSuite.rerunner))
            SucceededStatus
          case e if !anExceptionThatShouldCauseAnAbort(e) =>
            val duration = System.currentTimeMillis - testStartTime
            handleFailedTest(thisSuite, t, testName, messageRecorderForThisTest.recordedEvents(false, false), report, tracker, getEscapedIndentedTextForTest(testName, 1, true), duration)
            FailedStatus
          case e => throw e
        }
      case e if !anExceptionThatShouldCauseAnAbort(e) =>
        val duration = System.currentTimeMillis - testStartTime
        handleFailedTest(thisSuite, e, testName, messageRecorderForThisTest.recordedEvents(false, false), report, tracker, getEscapedIndentedTextForTest(testName, 1, true), duration)
        FailedStatus
      case e: Throwable => throw e
    }
  }

/*
  // Overriding this in fixture.Suite to reduce duplication of tags method
  private[scalatest] override def getMethodForTestName(theSuite: org.scalatest.Suite, testName: String): Method = {
    val candidateMethods = theSuite.getClass.getMethods.filter(_.getName == Suite.simpleNameForTest(testName))
    val found =
      if (testMethodTakesAFixtureAndInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 2 && paramTypes(1) == classOf[Informer]
          }
        )
      else if (testMethodTakesAnInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1 && paramTypes(0) == classOf[Informer]
          }
        )
      else if (testMethodTakesAFixture(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1
          }
        )
      else
        candidateMethods.find(_.getParameterTypes.length == 0)

     found match {
       case Some(method) => method
       case None =>
         throw new IllegalArgumentException(Resources("testNotFound", testName))
     }
  }
*/
  
  /**
   * Suite style name.
   *
   * @return <code>org.scalatest.fixture.Suite</code>
   */
  override val styleName: String = "org.scalatest.fixture.Suite"
}

/*
private[scalatest] object Suite {

  val FixtureAndInformerInParens = "(FixtureParam, Informer)"
  val FixtureInParens = "(FixtureParam)"

  private def testMethodTakesAFixtureAndInformer(testName: String) = testName.endsWith(FixtureAndInformerInParens)
  private[scalatest] def testMethodTakesAFixture(testName: String) = testName.endsWith(FixtureInParens)

  private[scalatest] def simpleNameForTest(testName: String) =
    if (testName.endsWith(FixtureAndInformerInParens))
      testName.substring(0, testName.length - FixtureAndInformerInParens.length)
    else if (testName.endsWith(FixtureInParens))
      testName.substring(0, testName.length - FixtureInParens.length)
    else if (testName.endsWith(InformerInParens))
      testName.substring(0, testName.length - InformerInParens.length)
    else
      testName

  private def argsArrayForTestName(testName: String): Array[Class[_]] =
    if (testMethodTakesAFixtureAndInformer(testName))
      Array(classOf[Object], classOf[Informer])
    else
      Array(classOf[Informer])
}
*/