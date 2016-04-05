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
package org.scalatest.junit;

import collection.immutable.TreeSet
import java.lang.reflect.{Method, Modifier}
import org.junit.runner.{Request, JUnitCore, Description, Result}
import org.scalatest._
import org.scalatest.Suite
import org.junit.runner.notification.RunListener
import org.junit.runner.notification.Failure
import org.scalatest.events._
import org.scalatest.Suite.autoTagClassAnnotations
import Suite.wrapReporterIfNecessary
import org.scalactic.SourceInfo

/**
 * Implementation trait for class <code>JUnitSuite</code>, which represents
 * a suite of tests that can be run with either JUnit or ScalaTest.
 * 
 * <p>
 * <a href="JUnitSuite.html"><code>JUnitSuite</code></a> is a class, not a
 * trait, to minimize compile time given there is a slight compiler overhead to
 * mixing in traits compared to extending classes. If you need to mix the
 * behavior of <code>JUnitSuite</code> into some other class, you can use this
 * trait instead, because class <code>JUnitSuite</code> does nothing more than
 * extend this trait.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="JUnitSuite.html">detailed
 * overview of <code>JUnitSuite</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
trait JUnitSuiteLike extends Suite with AssertionsForJUnit { thisSuite =>
                 
  // This is volatile, because who knows what Thread JUnit will fire through this.
  @volatile private var theTracker = new Tracker

  /**
   * Throws <code>UnsupportedOperationException</code>, because this method is unused by this
   * trait, given this trait's <code>run</code> method delegates to JUnit to run
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
   * trait, given this trait's <code>run</code> method delegates to JUnit to run
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
   * trait, given this traits's <code>run</code> method delegates to JUnit to run
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

    // TODO: Check to see if JUnit discovers static methods, private methods, etc.
    // Also, JUnit has something about test methods that can be parameterized. Will
    // eventually need to find those here too. What a pain.
    def isTestMethod(m: Method) = {

      val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

      val paramTypes = m.getParameterTypes
      val hasNoParams = paramTypes.length == 0
      // val hasVoidReturnType = m.getReturnType == Void.TYPE
      val hasTestAnnotation = m.getAnnotation(classOf[org.junit.Test]) != null

      isInstanceMethod && hasNoParams && hasTestAnnotation
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m))
      yield m.getName

    TreeSet[String]() ++ testNameArray
  }

  /**
   * Returns the number of tests expected to be run by JUnit when <code>run</code> is invoked
   * on this <code>JUnitSuite</code>.
   *
   * <p>
   * If <code>tagsToInclude</code> in the passed <code>Filter</code> is defined, this class's
   * implementation of this method returns 0. Else this class's implementation of this method
   * returns the size of the set returned by <code>testNames</code> on the current instance,
   * less the number of tests that were annotated with <code>org.junit.Ignore</code>.
   * </p>
   *
   * @param filter a <code>Filter</code> for test filtering
   * @return number of expected test count
   */
  override def expectedTestCount(filter: Filter) =
    if (filter.tagsToInclude.isDefined) 0 else (testNames.size - tags.size)

  /**
   * Overrides to return just tests that have org.junit.Ignore on them, but calls it org.scalatest.Ignore.
   * It also auto-tags suite level annotation.
   */
  override def tags: Map[String, Set[String]] = {

    val elements =
      for (testName <- testNames; if hasIgnoreTag(testName))
        yield testName -> Set("org.scalatest.Ignore")

    autoTagClassAnnotations(Map() ++ elements, this)
  }
  
  private def getMethodForJUnitTestName(testName: String) =
      getClass.getMethod(testName, new Array[Class[_]](0): _*)

  private def hasIgnoreTag(testName: String) = getMethodForJUnitTestName(testName).getAnnotation(classOf[org.junit.Ignore]) != null

  /**
   * Overrides to retrieve suite and test tags from annotations.
   *
   * @param testName the name of the test for which to return a <code>TestData</code> instance
   * @param theConfigMap the config map to include in the returned <code>TestData</code>
   * @return a <code>TestData</code> instance for the specified test, which includes the specified config map
   */
  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = {
    val suiteTags = for { 
      a <- this.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName
    val testTags: Set[String] = 
      try {
        if (hasIgnoreTag(testName))
          Set("org.scalatest.Ignore")
        else
          Set.empty[String]
      }
      catch {
        case e: IllegalArgumentException => Set.empty[String]
      }
    new TestData {
      val configMap = theConfigMap 
      val name = testName
      val scopes = Vector.empty
      val text = testName
      val tags = Set.empty ++ suiteTags ++ testTags
      val sourceInfo = SourceInfo("NA", "NA", 0)
    }
  }

  /**
   * Overrides to use JUnit 4 to run the test(s).
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
      val jUnitCore = new JUnitCore
      jUnitCore.addListener(new MyRunListener(wrapReporterIfNecessary(thisSuite, reporter), configMap, tracker, status))
      val myClass = this.getClass
      testName match {
        case None => jUnitCore.run(myClass)
        case Some(tn) =>
          if (!testNames.contains(tn))
            throw new IllegalArgumentException(Resources.testNotFound(testName))
          jUnitCore.run(Request.method(myClass, tn))
      }
    }
    
    status.setCompleted()
    status
  }
  
  /**
   * Suite style name.
   *
   * @return <code>JUnitSuite</code>
   */
  final override val styleName: String = "JUnitSuite"

// verifySomething(org.scalatest.junit.helpers.HappySuite)
// Description.displayName of a test report has the form <testMethodName>(<suiteClassName>)
}
