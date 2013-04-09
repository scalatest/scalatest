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
import org.scalatest._
import org.scalatest.testng._
import org.scalatest.jmock._
import java.io.File
import org.apache.commons.io.FileUtils
import org.jmock.Mockery
import org.jmock.Expectations
import org.scalatest.events.Ordinal

package org.scalatest.testng {

  class TestNGWrapperSuiteSuite extends FunSuite with SuiteExpectations {
  
    val XML_SUITES_PROPERTY = "xml_suites"
      
    val legacySuiteXml = 
      <suite name="Simple Suite">
        <test verbose="10" name="org.scalatest.testng.test" annotations="JDK">
          <classes>
            <class name="org.scalatest.testng.testpackage.LegacySuite"/>
          </classes>
        </test>
      </suite>
      
    test("wrapper suite properly notifies reporter when tests start, and pass") {
    
      val xmlSuiteFile = this.createSuite( legacySuiteXml )
          
      val context = new Mockery
      val reporter = context.mock(classOf[Reporter])

      context.checking(
        new Expectations() {
          expectSingleTestToPass(this, reporter)
        }
      )
      
      val status = new ScalaTestStatefulStatus
      (new TestNGWrapperSuite(List(xmlSuiteFile))).runTestNG(reporter, new Tracker, status)
      status.setCompleted()

      context.assertIsSatisfied()
    }

    val legacySuiteWithThreeTestsXml = 
      <suite name="Simple Suite">
        <test verbose="10" name="org.scalatest.testng.test" annotations="JDK">
          <classes>
            <class name="org.scalatest.testng.testpackage.LegacySuite"/>
            <class name="org.scalatest.testng.testpackage.LegacySuiteWithTwoTests"/>
          </classes>
        </test>
      </suite>
    
    test("wrapper suite should be notified for all tests") {
      
      val xmlSuiteFile = this.createSuite(legacySuiteWithThreeTestsXml)
          
      val context = new Mockery
      val reporter = context.mock(classOf[Reporter])

      context.checking(
        new Expectations() {
          expectNTestsToPass(this, 3, reporter) 
        }
      )
      
      val status = new ScalaTestStatefulStatus()
      (new TestNGWrapperSuite(List(xmlSuiteFile))).runTestNG(reporter, new Tracker, status)
      status.setCompleted()

      context.assertIsSatisfied()
    }
    
    def createSuite(suiteNode: scala.xml.Elem) : String = {
      val tmp = File.createTempFile("testng", "wrapper")
      FileUtils.writeStringToFile(tmp, suiteNode.toString)
      tmp.getAbsolutePath
    }
  }

  package testpackage {
    import org.testng.annotations._
  
    class LegacySuite extends TestNGSuite {
      @Test def legacyTestThatPasses() {}
    }
    class LegacySuiteWithTwoTests extends TestNGSuite {
      @Test def anotherLegacyTestThatPasses() {}
      @Test def anotherLegacyTestThatPasses2() {}
    }
  }
}
