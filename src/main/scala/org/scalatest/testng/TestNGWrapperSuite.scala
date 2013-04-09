/*
 * Copyright 2001-2008 Artima, Inc.
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
import org.testng.TestNG
import org.testng.TestListenerAdapter

/**
 * <p>
 * Suite that wraps existing TestNG test suites, described by TestNG XML config files. This class allows
 * existing TestNG tests written in Java to be run by ScalaTest.
 * </p>
 *
 * <p>
 * One way to use this class is to extend it and provide a list of one or more
 * names of TestNG XML config file names to run. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * class MyWrapperSuite extends TestNGWrapperSuite(
 *   List("oneTest.xml", "twoTest.xml", "redTest.xml", "blueTest.xml")
 * )
 * </pre>
 * 
 * <p>
 * You can also specify TestNG XML config files on <code>Runner</code>'s command line with <code>-t</code> parameters. See
 * the documentation for <code>Runner</code> for more information.
 * </p>
 *
 * <p>
 * To execute <code>TestNGWrapperSuite</code>s with ScalaTest's <code>Runner</code>, you must include TestNG's jar file on the class path or runpath.
 * This version of <code>TestNGSuite</code> was tested with TestNG version 6.3.1.
 * </p>
 *
 * @author Josh Cough
 */
class TestNGWrapperSuite(xmlSuiteFilenames: List[String]) extends TestNGSuite {

// Probably mention FileNotFoundException here
// If any files contained in the property cannot be found, a FileNotFoundException will be thrown.
  /**
   * Runs TestNG with the XML config file or files provided to the primary constructor, passing reports to the specified <code>Reporter</code>.
   * 
   * @param testName If present (Some), then only the method with the supplied name is executed and groups will be ignored.
   * @param args the <code>Args</code> for this run
   */
  override def run(testName: Option[String], args: Args): Status = {

    import args._

    val tagsToInclude =
      filter.tagsToInclude match {
        case None => Set[String]()
        case Some(tti) => tti
      }
    val tagsToExclude = filter.tagsToExclude

    val status = new ScalaTestStatefulStatus
    runTestNG(reporter, tagsToInclude, tagsToExclude, tracker, status)

    status.setCompleted()
    status
  }

  /**
   * Runs all tests in the xml suites.
   * @param   reporter   the reporter to be notified of test events (success, failure, etc)
   */
  override private[testng] def runTestNG(reporter: Reporter, tracker: Tracker, status: ScalaTestStatefulStatus) {
    runTestNG(reporter, Set[String](), Set[String](), tracker, status)
  }

  /**
   * Executes the following:
   * 
   * 1) Calls the super class to set up groups with the given groups Sets.
   * 2) Adds the xml suites to TestNG
   * 3) Runs TestNG
   *
   * @param   reporter   the reporter to be notified of test events (success, failure, etc)
   * @param   groupsToInclude    contains the names of groups to run. only tests in these groups will be executed
   * @param   groupsToExclude    tests in groups in this Set will not be executed
   * @param   status   Run status.
   */ 
  private[testng] def runTestNG(reporter: Reporter, groupsToInclude: Set[String], 
      groupsToExclude: Set[String], tracker: Tracker, status: ScalaTestStatefulStatus) {
    
    val testng = new TestNG
    handleGroups(groupsToInclude, groupsToExclude, testng)
    addXmlSuitesToTestNG(testng)
    
    run(testng, reporter, tracker, status)
  }
  
  /**
   * TestNG allows users to programmatically tell it which xml suites to run via the setTestSuites method.
   * This method takes a java.util.List containing java.io.File objects, where each file is a TestNG xml suite. 
   * TestNGWrapperSuite takes xmlSuitesPropertyName in its constructor. This property should contain
   * the full paths of one or more xml suites, comma seperated. This method simply creates a java.util.List 
   * containing each xml suite contained in xmlSuitesPropertyName and calls the setTestSuites method on the
   * given TestNG object. 
   *
   * @param testng	the TestNG object to set the suites on 
   *
   * @throws FileNotFoundexception if a file in xmlSuitesPropertyName does not exist.
   *
   * TODO: We should probably do this checking in the constructor.    
   */
  private def addXmlSuitesToTestNG(testng: TestNG) {
    import java.io.File
    import java.io.FileNotFoundException
    
    val files = new java.util.ArrayList[String]
    
    xmlSuiteFilenames.foreach( { name => 
        val f = new File( name )
        if( ! f.exists ) throw new FileNotFoundException( f.getAbsolutePath )
        files add name
      } 
    )
    testng.setTestSuites(files)
  }
}
