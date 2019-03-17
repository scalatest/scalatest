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
package org.scalatestplus

// SKIP-SCALATESTJS-START
/**
 * Package objects of type alias for deprecated easymock classes, there will be moved in these new packages in the future version of ScalaTest.
 */
package object easymock {

  type EasyMockSugar = org.scalatest.easymock.EasyMockSugar

  val EasyMockSugar = org.scalatest.easymock.EasyMockSugar

}

/**
 * Package objects of type alias for deprecated mockito classes, there will be moved in these new packages in the future version of ScalaTest.
 */
package object mockito {

  type MockitoSugar = org.scalatest.mockito.MockitoSugar

  val MockitoSugar = org.scalatest.mockito.MockitoSugar

}

/**
 * Package objects of type alias for deprecated jmock classes, there will be moved in these new packages in the future version of ScalaTest.
 */
package object jmock {

  type JMockCycle = org.scalatest.jmock.JMockCycle

  type AsyncJMockCycleFixture = org.scalatest.jmock.AsyncJMockCycleFixture

  type JMockCycleFixture = org.scalatest.jmock.JMockCycleFixture

  type JMockExpectations = org.scalatest.jmock.JMockExpectations

}

/**
 * Package objects of type alias for deprecated selenium classes, there will be moved in these new packages in the future version of ScalaTest.
 */
package object selenium {

  type Page = org.scalatest.selenium.Page

  type WebBrowser = org.scalatest.selenium.WebBrowser

  val WebBrowser = org.scalatest.selenium.WebBrowser

  type Driver = org.scalatest.selenium.Driver

  type HtmlUnit = org.scalatest.selenium.HtmlUnit

  val HtmlUnit = org.scalatest.selenium.HtmlUnit

  type Firefox = org.scalatest.selenium.Firefox

  val Firefox = org.scalatest.selenium.Firefox

  type Safari = org.scalatest.selenium.Safari

  val Safari = org.scalatest.selenium.Safari

  type Chrome = org.scalatest.selenium.Chrome

  val Chrome = org.scalatest.selenium.Chrome

  type InternetExplorer = org.scalatest.selenium.InternetExplorer

  val InternetExplorer = org.scalatest.selenium.InternetExplorer

}

/**
  * Package objects of type alias for deprecated junit classes, there will be moved in these new packages in the future version of ScalaTest.
  */
package object junit {

  type AssertionsForJUnit = org.scalatest.junit.AssertionsForJUnit

  val AssertionsForJUnit = org.scalatest.junit.AssertionsForJUnit

  type JUnit3Suite = org.scalatest.junit.JUnit3Suite

  type JUnitRunner = org.scalatest.junit.JUnitRunner

  type JUnitSuite = org.scalatest.junit.JUnitSuite

  type JUnitSuiteLike = org.scalatest.junit.JUnitSuiteLike

  type JUnitTestFailedError = org.scalatest.junit.JUnitTestFailedError

  type JUnitWrapperSuite = org.scalatest.junit.JUnitWrapperSuite

}

/**
  * Package objects of type alias for deprecated testng classes, there will be moved in these new packages in the future version of ScalaTest.
  */
package object testng {

  type TestNGSuite = org.scalatest.testng.TestNGSuite

  type TestNGSuiteLike = org.scalatest.testng.TestNGSuiteLike

  type TestNGWrapperSuite = org.scalatest.testng.TestNGWrapperSuite

}

// SKIP-SCALATESTJS-END

/**
 * Package objects of type alias for deprecated scalacheck classes, there will be moved in these new packages in the future version of ScalaTest.
 */
package object scalacheck {

  type Checkers = org.scalatest.prop.Checkers

  val Checkers = org.scalatest.prop.Checkers

  type ScalaCheckDrivenPropertyChecks = org.scalatest.prop.GeneratorDrivenPropertyChecks

  val ScalaCheckDrivenPropertyChecks = org.scalatest.prop.PropertyChecks

  type ScalaCheckPropertyChecks = org.scalatest.prop.PropertyChecks

  val ScalaCheckPropertyChecks = org.scalatest.prop.PropertyChecks

}
