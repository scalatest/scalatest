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
 * A suite of tests that can be run with either TestNG or ScalaTest. This class allows you to mark any
 * method as a test using TestNG's <code>@Test</code> annotation, and supports all other TestNG annotations.
 * Here's an example:
 *
 * <pre class="stHighlight">
 * import org.scalatest.testng.TestNGSuite
 * import org.testng.annotations.Test
 * import org.testng.annotations.Configuration
 * import scala.collection.mutable.ListBuffer
 * 
 * class MySuite extends TestNGSuite {
 * 
 *   var sb: StringBuilder = _
 *   var lb: ListBuffer[String] = _
 * 
 *   @Configuration(beforeTestMethod = true)
 *   def setUpFixture() {
 *     sb = new StringBuilder("ScalaTest is ")
 *     lb = new ListBuffer[String]
 *   }
 * 
 *   @Test(invocationCount = 3)
 *   def easyTest() {
 *     sb.append("easy!")
 *     assert(sb.toString === "ScalaTest is easy!")
 *     assert(lb.isEmpty)
 *     lb += "sweet"
 *   }
 * 
 *   @Test(groups = Array("com.mycompany.groups.SlowTest"))
 *   def funTest() {
 *     sb.append("fun!")
 *     assert(sb.toString === "ScalaTest is fun!")
 *     assert(lb.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * To execute <code>TestNGSuite</code>s with ScalaTest's <code>Runner</code>, you must include TestNG's jar file on the class path or runpath.
 * This version of <code>TestNGSuite</code> was tested with TestNG version 6.3.1.
 * </p>
 *
 * <p>
 * See also: <a href="http://www.scalatest.org/getting_started_with_testng" target="_blank">Getting started with TestNG and ScalaTest.</a>
 * </p>
 * 
 * @author Josh Cough
 * @author Bill Venners
 */
class TestNGSuite extends TestNGSuiteLike
