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
package org.scalatest.testng

import org.scalatest._
import org.scalatest.events._
import Suite.getIndentedTextForTest
import Suite.formatterForSuiteAborted
import Suite.formatterForSuiteStarting
import Suite.formatterForSuiteCompleted
import events.MotionToSuppress

import org.testng.TestNG
import org.testng.TestListenerAdapter
import exceptions._
import Suite.wrapReporterIfNecessary

/**
 * Implementation trait for class <code>TestNGSuite</code>, which represents
 * a suite of tests that can be run with either TestNG or ScalaTest.
 * 
 * <p>
 * <a href="TestNGSuite.html"><code>TestNGSuite</code></a> is a class, not a
 * trait, to minimize compile time given there is a slight compiler overhead to
 * mixing in traits compared to extending classes. If you need to mix the
 * behavior of <code>TestNGSuite</code> into some other class, you can use this
 * trait instead, because class <code>TestNGSuite</code> does nothing more than
 * extend this trait.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="TestNGSuite.html">detailed
 * overview of <code>TestNGSuite</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
trait TestNGSuiteLike extends Suite { thisSuite =>

  /**
   * Execute this <code>TestNGSuite</code>.
   * 
   * @param testName an optional name of one test to execute. If <code>None</code>, this class will execute all relevant tests.
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>TestNGSuite</code>.
   * @param args the <code>Args</code> for this run
   */
  override def run(testName: Option[String], args: Args): Status = {
    import args._
    val status = new ScalaTestStatefulStatus
    runTestNG(testName, wrapReporterIfNecessary(thisSuite, reporter), filter, tracker, status)
    
    status.setCompleted()
    status
  }

  /**
   * Runs TestNG with no test name, no groups. All tests in the class will be executed.
   * @param   reporter   the reporter to be notified of test events (success, failure, etc)
   * @param   status   Status of run.
   */
  private[testng] def runTestNG(reporter: Reporter, tracker: Tracker, status: ScalaTestStatefulStatus) {
    runTestNG(None, reporter, Filter(), tracker, status)
  }

  /**
   * Runs TestNG, running only the test method with the given name. 
   * @param   testName   the name of the method to run
   * @param   reporter   the reporter to be notified of test events (success, failure, etc)
   * @param   status   Status of run.
   */
  private[testng] def runTestNG(testName: String, reporter: Reporter, tracker: Tracker, status: ScalaTestStatefulStatus) {
    runTestNG(Some(testName), reporter, Filter(), tracker, status)
  }
  
  /**
   * Runs TestNG. The meat and potatoes. 
   *
   * @param   testName   if present (Some), then only the method with the supplied name is executed and groups will be ignored
   * @param   reporter   the reporter to be notified of test events (success, failure, etc)
   * @param   groupsToInclude    contains the names of groups to run. only tests in these groups will be executed
   * @param   groupsToExclude    tests in groups in this Set will not be executed
   * @param   status   Status of run.
   */  
  private[testng] def runTestNG(testName: Option[String], reporter: Reporter,
      filter: Filter, tracker: Tracker, status: ScalaTestStatefulStatus) {
    
    val tagsToInclude =
      filter.tagsToInclude match {
        case None => Set[String]()
        case Some(tti) => tti
      }
    val tagsToExclude = filter.tagsToExclude

    val testng = new TestNG()
    
    // only run the test methods in this class
    testng.setTestClasses(Array(this.getClass))
    
    // if testName is supplied, ignore groups.
    testName match {
      case Some(tn) => setupTestNGToRunSingleMethod(tn, testng)
      case None => handleGroups(tagsToInclude, tagsToExclude, testng)
    }

    this.run(testng, reporter, tracker, status)
  }
  
  /**
   * Runs the TestNG object which calls back to the given Reporter.
   */
  private[testng] def run(testng: TestNG, reporter: Reporter, tracker: Tracker, status: ScalaTestStatefulStatus) {
    
    // setup the callback mechanism
    val tla = new MyTestListenerAdapter(reporter, tracker, status)
    testng.addListener(tla)
    
    // finally, run TestNG
    testng.run()
  }
  
  /**
   * Tells TestNG which groups to include and exclude, which is directly a one-to-one mapping.
   */
  private[testng] def handleGroups(groupsToInclude: Set[String], groupsToExclude: Set[String], testng: TestNG) {
    testng.setGroups(groupsToInclude.mkString(","))
    testng.setExcludedGroups(groupsToExclude.mkString(","))
  }
  
  /**
   * This method ensures that TestNG will only run the test method whose name matches testName.
   * 
   * The approach is a bit odd however because TestNG doesn't have an easy API for
   * running a single method. To get around that we chose to use an AnnotationTransformer 
   * to add a secret group to the test method's annotation. We then set up TestNG to run only that group. 
   * 
   * @param    testName    the name of the test method to be executed
   */
  private def setupTestNGToRunSingleMethod(testName: String, testng: TestNG) = {
    // NOTE: There was another option - we could TestNG's XmlSuites to specify which method to run.
    // This approach was about as much work, offered no clear benefits, and no additional problems either.
    
    // Using reflection because TestNG has a incompatible change, we want to allow people to use the old and the new version of TestNG.
    try {
      val transformerSuperClass = Class.forName("org.testng.IAnnotationTransformer")
      val transformerSubClass = Class.forName("org.scalatest.testng.SingleTestAnnotationTransformer")
      // Go with TestNG 6
      val transformerInstance = transformerSubClass.getConstructor(classOf[String]).newInstance(testName).asInstanceOf[SingleTestAnnotationTransformer]
      testng.setGroups("org.scalatest.testng.singlemethodrun.methodname")
      val method = testng.getClass.getMethod("setAnnotationTransformer", transformerSuperClass)
      method.invoke(testng, transformerInstance)
    }
    catch {
      case e: ClassNotFoundException => 
        new UnsupportedOperationException("Sorry, due to incompatible changes in TestNG, running a single test is only supported in TestNG version 6 or later.", e)
    }
  }
  
  /*
   * This class hooks TestNG's callback mechanism (TestListenerAdapter) to ScalaTest's
   * reporting mechanism. TestNG has many different callback points which are a near one-to-one
   * mapping with ScalaTest. At each callback point, this class simply creates ScalaTest 
   * reports and calls the appropriate method on the Reporter.
   * 
   * TODO: 
   * (12:02:27 AM) bvenners: onTestFailedButWithinSuccessPercentage(ITestResult tr) 
   * (12:02:34 AM) bvenners: maybe a TestSucceeded with some extra info in the report
   */
  private[testng] class MyTestListenerAdapter(reporter: Reporter, tracker: Tracker, status: ScalaTestStatefulStatus) extends TestListenerAdapter {
    
    // TODO: Put the tracker in an atomic, because TestNG can go multithreaded?

    val report = reporter

    import org.testng.ITestContext
    import org.testng.ITestResult
    
    private val className = TestNGSuiteLike.this.getClass.getName

    def getTopOfMethod(className: String, methodName: String) = Some(TopOfMethod(className, "public void " + className + "." + methodName + "()"))

    /**
     * TestNG's onTestStart maps cleanly to TestStarting. Simply build a report 
     * and pass it to the Reporter.
     */
    override def onTestStart(result: ITestResult) = {
      report(TestStarting(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.getClass.getName, Some(thisSuite.getClass.getName), result.getName + params(result), result.getName + params(result),
             Some(MotionToSuppress), getTopOfMethod(thisSuite.getClass.getName, result.getName), Some(className)))
    }

    /**
     * TestNG's onTestSuccess maps cleanly to TestSucceeded. Again, simply build
     * a report and pass it to the Reporter.
     */
    override def onTestSuccess(result: ITestResult) = {
      val testName = result.getName + params(result)
      val formatter = getIndentedTextForTest(testName, 1, true)
      report(TestSucceeded(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.getClass.getName, Some(thisSuite.getClass.getName), testName, testName, 
             Vector.empty, None, Some(formatter), getTopOfMethod(thisSuite.getClass.getName, result.getName), Some(className))) // Can I add a duration?
    }

    /**
     * TestNG's onTestSkipped maps cleanly to TestIgnored. Again, simply build
     * a report and pass it to the Reporter.
     */
    override def onTestSkipped(result: ITestResult) = {
      val testName = result.getName + params(result)
      val formatter = getIndentedTextForTest(testName, 1, true)
      report(TestIgnored(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.getClass.getName, Some(thisSuite.getClass.getName), testName, testName, Some(formatter), getTopOfMethod(thisSuite.getClass.getName, result.getName)))
    }

    /**
     * TestNG's onTestFailure maps cleanly to TestFailed.
     */
    override def onTestFailure(result: ITestResult) = {
      val throwableOrNull = result.getThrowable
      val throwable = if (throwableOrNull != null) Some(throwableOrNull) else None
      val message = if (throwableOrNull != null && throwableOrNull.getMessage != null) throwableOrNull.getMessage else Resources("testNGConfigFailed")
      val testName = result.getName + params(result)
      val formatter = getIndentedTextForTest(testName, 1, true)
      val payload = 
      throwable match {
        case optPayload: PayloadField => 
          optPayload.payload
        case _ => 
          None
      }
      report(TestFailed(tracker.nextOrdinal(), message, thisSuite.suiteName, thisSuite.getClass.getName, Some(thisSuite.getClass.getName), testName, testName, Vector.empty, throwable, None, Some(formatter), Some(SeeStackDepthException), Some(className), payload)) // Can I add a duration?
      status.setFailed()
    }

    /**
     * A TestNG setup method resulted in an exception, and a test method will later fail to run. 
     * This TestNG callback method has the exception that caused the problem, as well
     * as the name of the method that failed. Create a Report with the method name and the
     * exception and call reporter(SuiteAborted).
     */
    override def onConfigurationFailure(result: ITestResult) = {
      val throwableOrNull = result.getThrowable
      val throwable = if (throwableOrNull != null) Some(throwableOrNull) else None
      val message = if (throwableOrNull != null && throwableOrNull.getMessage != null) throwableOrNull.getMessage else Resources("testNGConfigFailed")
      val formatter = formatterForSuiteAborted(thisSuite, message)
      report(SuiteAborted(tracker.nextOrdinal(), message, thisSuite.suiteName, thisSuite.getClass.getName, Some(thisSuite.getClass.getName), throwable, None, formatter, Some(SeeStackDepthException)))
      status.setFailed()
    }

    /**
     * TestNG's onConfigurationSuccess doesn't have a clean mapping in ScalaTest.
     * Simply create a Report and fire InfoProvided. This works well
     * because there may be a large number of setup methods and InfoProvided doesn't 
     * show up in your face on the UI, and so doesn't clutter the UI. 
     */
    override def onConfigurationSuccess(result: ITestResult) = { // TODO: Work on this report
      // For now don't print anything. Succeed with silence. Is adding clutter.
      // report(InfoProvided(tracker.nextOrdinal(), result.getName, Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), None))))
    }

    private def params(itr: ITestResult): String = {
      itr.getParameters match {   
        case Array() => ""
        case _ => "(" + itr.getParameters.mkString(",") + ")"
      }
    }
  }
  
  /*
     TODO
    (12:02:27 AM) bvenners: onTestFailedButWithinSuccessPercentage(ITestResult tr)
    (12:02:34 AM) bvenners: maybe a TestSucceeded with some extra info in the report
  */

  /**
   * Throws <code>UnsupportedOperationException</code>, because this method is unused by this
   * class, given this class's <code>run</code> method delegates to JUnit to run
   * its tests.
   *
   * <p>
   * The main purpose of this method implementation is to render a compiler error an attempt
   * to mix in a trait that overrides <code>withFixture</code>. Because this
   * trait does not actually use <code>withFixture</code>, the attempt to mix
   * in behavior would very likely not work.
   * </p>
   *
   *
   * @param test the no-arg test function to run with a fixture
   */
  override final protected def withFixture(test: NoArgTest): Outcome = {
     throw new UnsupportedOperationException
  }

  /**
   * Throws <code>UnsupportedOperationException</code>, because this method is unused by this
   * trait, given this trait's <code>run</code> method delegates to TestNG to run
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
   * trait, given this trait's <code>run</code> method delegates to TestNG to run
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
   * trait, given this trait's <code>run</code> method delegates to TestNG to run
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
   * Suite style name.
   */
  final override val styleName: String = "TestNGSuite"
}
