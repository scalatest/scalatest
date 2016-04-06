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
package org.scalatest.junit

import collection.immutable.TreeSet
import java.lang.reflect.{Method, Modifier}
import org.scalatest._
import _root_.junit.framework.TestCase
import _root_.junit.framework.TestResult
import _root_.junit.framework.TestSuite
import _root_.junit.framework.TestListener
import _root_.junit.framework.Test
import _root_.junit.framework.AssertionFailedError
import scala.collection.mutable.HashSet
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.TestFailed
import org.scalatest.events.MotionToSuppress
import org.scalactic.Requirements._
import Suite.getIndentedTextForTest
import org.scalatest.events._
import exceptions._
import Suite.wrapReporterIfNecessary
import org.scalactic.source.SourceInfo

/**
 * A <code>Suite</code> that is also a <code>junit.framework.TestCase</code>. 
 *
 * <p>
 * A <code>JUnit3Suite</code> may be run by either JUnit 3 (such as JUnit 3.8) or ScalaTest's runner. You write it the way
 * you write a JUnit 3 <code>TestCase</code>. Tests are methods that start with <code>test</code>, take no parameters, and
 * have a <code>Unit</code> return type. You manage fixtures with methods <code>setUp</code> and <code>tearDown</code>.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.junit.JUnit3Suite
 * import scala.collection.mutable.ListBuffer
 *
 * class BlastFromThePastSuite extends JUnit3Suite {
 *
 *   var sb: StringBuilder = _
 *   var lb: ListBuffer[String] = _
 *
 *   override def setUp(): Unit = {
 *     sb = new StringBuilder("ScalaTest is ")
 *     lb = new ListBuffer[String]
 *   }
 *
 *   def testEasy(): Unit = { // Uses JUnit-style assertions
 *     sb.append("easy!")
 *     assertEquals("ScalaTest is easy!", sb.toString)
 *     assertTrue(lb.isEmpty)
 *     lb += "sweet"
 *   }
 *
 *   def testFun(): Unit = { // Uses ScalaTest assertions
 *     sb.append("fun!")
 *     assert(sb.toString === "ScalaTest is fun!")
 *     assert(lb.isEmpty)
 *   }
 * }
 * </pre>
 * 
 * <p>
 * You can use either JUnit's assertions, inherited from <code>TestCase</code>, or ScalaTest's, inherited from <code>AssertionsForJUnit</code>.
 * </p>
 *
 * <p>
 * When writing JUnit 3 tests in Scala, you should keep in mind that JUnit 3 will not run tests that have a return type other than
 * <code>Unit</code>. Thus it is best to explicitly state the <code>Unit</code> result type, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * def testGoodIdea(): Unit = { // result type will be Unit
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * Instead of this:
 * </p>
 *
 * <pre class="stHighlight">
 * def testBadIdea() = { // result type will be inferred
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * If the <code>testBadIdea</code> method ends in an expression that has a result type other than <code>Unit</code>, the Scala
 * compiler will infer a result type to the <code>testBadIdea</code> method to be the same non-<code>Unit</code> type. As a "result,"
 * JUnit 3 will not discover or run the <code>testBadIdea</code> method at all.
 * </p>
 *
 * @author Bill Venners
 */
class JUnit3Suite extends TestCase with Suite with AssertionsForJUnit { thisSuite =>

  // This is volatile, because who knows what Thread JUnit will fire through this.
  @volatile private var theTracker = new Tracker

  /**
   * Returns the set of test names that will be executed by JUnit when <code>run</code> is invoked
   * on an instance of this class, or the instance is passed directly to JUnit for running.
   *
   * <p>
   * The iterator obtained by invoking <code>elements</code> on this
   * returned <code>Set</code> will produce the test names in their <em>natural order</em>, as determined by <code>String</code>'s
   * <code>compareTo</code> method. Nevertheless, this method is not consulted by JUnit when it
   * runs the tests, and JUnit may run the tests in any order.
   * </p>
   */
  override def testNames: Set[String] = {

    def isTestMethod(m: Method) = {

      val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

      // name must have at least 4 chars (minimum is "test")
      val simpleName = m.getName
      val firstFour = if (simpleName.length >= 4) simpleName.substring(0, 4) else ""

      val paramTypes = m.getParameterTypes
      val hasNoParams = paramTypes.length == 0
      val hasVoidReturnType = m.getReturnType == Void.TYPE

      // won't discover testNames because it has a non-Unit return type
      isInstanceMethod && (firstFour == "test") && hasNoParams && hasVoidReturnType
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m))
      yield m.getName

    TreeSet[String]() ++ testNameArray
  }

  /**
   * Returns an empty <code>Map</code>, because tags are not supported by JUnit 3.
   */
  override def tags = Map()

  /**
   * Returns the number of tests expected to be run by JUnit when <code>run</code> is invoked
   * on this <code>Suite</code>.
   *
   * <p>
   * If <code>tagsToInclude</code> in the passed <code>Filter</code> is defined, this class's
   * implementation of this method returns 0. Else this class's implementation of this method
   * returns the size of the set returned by <code>testNames</code> on the current instance.
   * </p>
   */
  override def expectedTestCount(filter: Filter) =
    if (filter.tagsToInclude.isDefined) 0 else testNames.size

  /**
   * Throws <code>UnsupportedOperationException</code>, because this method is unused by this
   * class, given this class's <code>run</code> method delegates to JUnit to run
   * its tests.
   *
   * <p>
   * The main purpose of this method implementation is to render a compiler error an attempt
   * to mix in a trait that overrides <code>runNestedSuites</code>. Because this
   * trait does not actually use <code>runNestedSuites</code>, the attempt to mix
   * in behavior would very likely not work.
   * </p>
   *
   * @param args the <code>Args</code> for this run
   *
   * @throws UnsupportedOperationException always.
   */
  override final protected def runNestedSuites(args: Args): Status = {

    throw new UnsupportedOperationException
  }

  /**
   * Throws <code>UnsupportedOperationException</code>, because this method is unused by this
   * class, given this class's <code>run</code> method delegates to JUnit to run
   * its tests.
   *
   * <p>
   * The main purpose of this method implementation is to render a compiler error an attempt
   * to mix in a trait that overrides <code>runTests</code>. Because this
   * trait does not actually use <code>runTests</code>, the attempt to mix
   * in behavior would very likely not work.
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   *
   * @throws UnsupportedOperationException always.
   */
  override protected final def runTests(testName: Option[String], args: Args): Status = {
    throw new UnsupportedOperationException
  }

  /**
   * Throws <code>UnsupportedOperationException</code>, because this method is unused by this
   * class, given this class's <code>run</code> method delegates to JUnit to run
   * its tests.
   *
   * <p>
   * The main purpose of this method implementation is to render a compiler error an attempt
   * to mix in a trait that overrides <code>runTest</code>. Because this
   * trait does not actually use <code>runTest</code>, the attempt to mix
   * in behavior would very likely not work.
   * </p>
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   *
   * @throws UnsupportedOperationException always.
   */
  override protected final def runTest(testName: String, args: Args): Status = {
        throw new UnsupportedOperationException
  }

  /**
   * Overrides to use JUnit 3 to run the test(s).
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests and nested suites started by this method have completed, and whether or not a failure occurred.
   *
   */
  override def run(testName: Option[String], args: Args): Status = {

    import args._

    theTracker = tracker
    val status = new ScalaTestStatefulStatus

    if (!filter.tagsToInclude.isDefined) {
      val testResult = new TestResult
      testResult.addListener(new MyTestListener(wrapReporterIfNecessary(thisSuite, reporter), tracker, status))
      testName match {
        case None => new TestSuite(this.getClass).run(testResult)
        case Some(tn) =>
          if (!testNames.contains(tn))
            throw new IllegalArgumentException(Resources.testNotFound(testName))
          setName(tn)
          run(testResult)
      }
    }
    
    status.setCompleted()
    status
  }
  
  /**
   * Suite style name.
   *
   * @return <code>JUnit3Suite</code>
   */
  final override val styleName: String = "JUnit3Suite"
    
  final override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = 
    new TestData {
      val configMap = theConfigMap 
      val name = testName
      val scopes = Vector.empty
      val text = testName
      val tags = Set.empty[String]
      val sourceInfo = SourceInfo("NA", "NA", 0)
    }
}

private[scalatest] class MyTestListener(report: Reporter, tracker: Tracker, status: ScalaTestStatefulStatus) extends TestListener {

  // TODO: worry about threading
  private val failedTestsSet = scala.collection.mutable.Set[Test]()

  private def getSuiteNameForTestCase(testCase: Test) =
    testCase match {
      case junit3Suite: JUnit3Suite => junit3Suite.suiteName
      case _ => Suite.getSimpleNameOfAnObjectsClass(testCase) // Should never happen, but just in case
    }

  def getMessageGivenThrowable(throwable: Throwable, isAssertionFailedError: Boolean) =
    if (throwable.getMessage == null)
      "A JUnit3Suite test failed with an " + (if (isAssertionFailedError) "AssertionFailedError" else "exception") // Hopefully will never happen
    else
      throwable.getMessage

  def getTopOfMethod(className: String, methodName: String) = Some(TopOfMethod(className, "public void " + className + "." + methodName + "()"))

  // The Test passed to these methods is an instance of the JUnit3Suite class, Calling
  // test.getClass.getName on it gets the fully qualified name of the class
  // test.asInstanceOf[TestCase].getName gives you the name of the test method, without any parens
  // Calling test.toSring gives you testError(org.scalatestexamples.junit.JUnit3ExampleSuite)
  // So that's that old JUnit-style test name thing.
  def startTest(testCase: Test) {
    requireNonNull(testCase)
    val suiteName = getSuiteNameForTestCase(testCase)
    report(TestStarting(tracker.nextOrdinal(), suiteName, testCase.getClass.getName, Some(testCase.getClass.getName), testCase.toString, testCase.toString, Some(MotionToSuppress), getTopOfMethod(testCase.getClass.getName, testCase.asInstanceOf[TestCase].getName)))
  }
  
  def addError(testCase: Test, throwable: Throwable) {

    requireNonNull(testCase, throwable)

    val formatter = getIndentedTextForTest(testCase.toString, 1, true)
    val suiteName = getSuiteNameForTestCase(testCase)
    val payload = 
      throwable match {
        case optPayload: PayloadField => 
          optPayload.payload
        case _ => 
          None
      }
    report(TestFailed(tracker.nextOrdinal(), getMessageGivenThrowable(throwable, false), suiteName, testCase.getClass.getName, Some(testCase.getClass.getName), testCase.toString, testCase.toString, Vector.empty, Some(throwable), None, Some(formatter), Some(SeeStackDepthException), None, payload))

    failedTestsSet += testCase
  }

  def addFailure(testCase: Test, assertionFailedError: AssertionFailedError) {

    requireNonNull(testCase, assertionFailedError)

    val formatter = getIndentedTextForTest(testCase.toString, 1, true)
    val suiteName = getSuiteNameForTestCase(testCase)
    report(TestFailed(tracker.nextOrdinal(), getMessageGivenThrowable(assertionFailedError, true), suiteName, testCase.getClass.getName, Some(testCase.getClass.getName), testCase.toString, testCase.toString, Vector.empty, Some(assertionFailedError), None, Some(formatter), Some(SeeStackDepthException), None))

    failedTestsSet += testCase
  }

  def endTest(testCase: Test) {

    val testHadFailed = failedTestsSet.contains(testCase)

    if (!testHadFailed) {
      requireNonNull(testCase)
      val formatter = getIndentedTextForTest(testCase.toString, 1, true)
      val suiteName = getSuiteNameForTestCase(testCase)
      report(TestSucceeded(tracker.nextOrdinal(), suiteName, testCase.getClass.getName, Some(testCase.getClass.getName), testCase.toString, testCase.toString, Vector.empty, None, Some(formatter), getTopOfMethod(testCase.getClass.getName, testCase.asInstanceOf[TestCase].getName)))
    }
    else {
      failedTestsSet -= testCase  
      status.setFailed()
    }
  }

}

