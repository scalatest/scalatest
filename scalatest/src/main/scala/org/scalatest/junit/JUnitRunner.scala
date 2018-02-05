
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

import collection.JavaConverters._

import org.scalatest._
import org.junit.runner.notification.RunNotifier
import org.junit.runner.notification.Failure
import org.junit.runner.Description
import org.junit.runner.manipulation.{Filterable, NoTestsRemainException}

/*
 I think that Stopper really should be a no-op, like it is, because the user has
 no way to stop it. This is wierd, because it will call nested suites. So the tests
 just pile up. Oh, I see, the darn information about which test it is is in the
 stupid Description displayName. We probably need to add optional test name and
 suite class name to Report, just to satisfy JUnit integration.
*/
/**
 * A JUnit <code>Runner</code> that knows how to run any ScalaTest <code>Suite</code>.
 * This enables you to provide a JUnit <code>RunWith</code> annotation on any
 * ScalaTest <code>Suite</code>. Here's an example:
 *
 * <pre class="stHighlight">
 * import org.junit.runner.RunWith
 * import org.scalatest.junit.JUnitRunner
 * import org.scalatest.FunSuite
 *
 * @RunWith(classOf[JUnitRunner])
 * class MySuite extends FunSuite {
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * This <code>RunWith</code> annotation will enable the <code>MySuite</code> class
 * to be run by JUnit 4.
 * </p>
 *
 * @param suiteClass suite class to be run
 *
 * @author Bill Venners
 * @author Daniel Watson
 * @author Jon-Anders Teigen
 * @author Colin Howe
 */
final class JUnitRunner(suiteClass: java.lang.Class[_ <: Suite]) extends org.junit.runner.Runner with Filterable {

  private val excludedTestTag = "org.scalatest.junit.JUnitExcludedWithDynaTags"

  private val canInstantiate = Suite.checkForPublicNoArgConstructor(suiteClass)
  require(canInstantiate, "Must pass an org.scalatest.Suite with a public no-arg constructor")

  private val suiteToRun = suiteClass.newInstance

  private var description: Description = createDescription(suiteToRun, None)

  private def extractTestNamesFromDescription(description: Description): Set[String] = {
    for {
      child <- description.getChildren.asScala
      if child.isSuite
    } yield extractTestNamesFromDescription(child)

    val testNameRegEx = """^(.+)\([\w|\.]+\)""".r
    val tests = for {
      child <- description.getChildren.asScala
      if child.isTest
      testName = child.getDisplayName match {
        case testNameRegEx(name) => name
      }
    } yield testName

    tests.toSet
  }

  private def allTests(suite: Suite): Set[String] = {
     for {
       nested <- suite.nestedSuites
     } yield allTests(nested)

    suite.testNames
  }

  private def createDescription(suite: Suite, junitFilter: Option[org.junit.runner.manipulation.Filter]): Description = {
    val description = Description.createSuiteDescription(suite.getClass)
    // If we don't add the testNames and nested suites in, we get
    // Unrooted Tests show up in Eclipse
    for (name <- suite.testNames) {
      junitFilter match {
        case Some(filter) =>
          val tempDescription = Description.createTestDescription(suite.getClass, name)
          if (filter.shouldRun(tempDescription))
            description.addChild(tempDescription)
        case _ =>
          description.addChild(Description.createTestDescription(suite.getClass, name))
      }
    }
    for (nestedSuite <- suite.nestedSuites) {
      description.addChild(createDescription(nestedSuite, junitFilter))
    }
    description
  }

  /**
    * Get a JUnit <code>Description</code> for this ScalaTest <code>Suite</code> of tests.
    *
    * @return a <code>Description</code> of this suite of tests
    */
  def getDescription: Description = description

  /**
   * Run this <code>Suite</code> of tests, reporting results to the passed <code>RunNotifier</code>.
   * This class's implementation of this method invokes <code>run</code> on an instance of the
   * <code>suiteClass</code> <code>Class</code> passed to the primary constructor, passing
   * in a <code>Reporter</code> that forwards to the  <code>RunNotifier</code> passed to this
   * method as <code>notifier</code>.
   *
   * @param notifier the JUnit <code>RunNotifier</code> to which to report the results of executing
   * this suite of tests
   */
  def run(notifier: RunNotifier): Unit = {
    try {
      val testsToRun = extractTestNamesFromDescription(description)
      val excludedTestNames = allTests(suiteToRun) -- testsToRun
      val excludedTestsByTag = Map(suiteToRun.suiteId -> excludedTestNames.map(testName => testName -> Set(excludedTestTag)).toMap)

      val filter = Filter(tagsToExclude = Set(excludedTestTag), dynaTags = DynaTags(Map.empty, excludedTestsByTag))

      // TODO: What should this Tracker be?
      suiteToRun.run(None, Args(new RunNotifierReporter(notifier),
                                Stopper.default, filter, ConfigMap.empty, None,
                                new Tracker, Set.empty))
    }
    catch {
      case e: Exception =>
        notifier.fireTestFailure(new Failure(getDescription, e))
    }
  }

  /**
   * Returns the number of tests that are expected to run when this ScalaTest <code>Suite</code>
   * is run.
   *
   *  @return the expected number of tests that will run when this suite is run
   */
  override def testCount(): Int = description.testCount()

  /**
   * Called by the JUnit Runner to filter the tests in the <code>Suite</code>
   *
   * @param the JUnit filter to call <code>shouldRun</code> for the test
   */
  @throws(classOf[NoTestsRemainException])
  override def filter(filter: org.junit.runner.manipulation.Filter): Unit = {
    description = createDescription(suiteToRun, Some(filter))
    if (description.testCount() == 0)
      throw new NoTestsRemainException
  }
}
