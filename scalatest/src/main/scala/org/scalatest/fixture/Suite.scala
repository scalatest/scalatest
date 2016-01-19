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
import scala.reflect.NameTransformer.decode

/**
 * Base trait for a family of style traits that can pass a fixture object into tests.
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.MethodFinder"))
trait Suite extends org.scalatest.Suite { thisSuite =>

  /**
   * The type of the fixture parameter that can be passed into tests in this suite.
   */
  protected type FixtureParam
  /*
   * A <code>Set</code> of test names. If this <code>fixture.Suite</code> contains no tests, this method returns an empty <code>Set</code>.
   *
   * <p>
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
   *         assertThrows[NoSuchElementException] {
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
   // Can just inherit the supertrait implementation of testnames that returns an empty set

/*
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
*/

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
   * @throws NullArgumentException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  // Can just inheirt the supertrait implementation of runTest that 
/*
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
*/

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
         throw new IllegalArgumentException(Resources.testNotFound(testName))
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
