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
package org.scalatest

import org.scalatest.prop.Tables
import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.scalatest.testng.TestNGSuite

trait MethodSuiteExamples extends Tables {
  type FixtureServices
  
  def suite: Suite with FixtureServices
  def fixtureSuite: fixture.Suite with FixtureServices
  def spec: Spec with FixtureServices
  def fixtureSpec: fixture.Spec with FixtureServices
  def junit3Suite: JUnit3Suite with FixtureServices
  def junitSuite: JUnitSuite with FixtureServices
  def testngSuite: TestNGSuite with FixtureServices
  
  def examples =
    Table[Suite with FixtureServices](
      "suite",
      suite,
      fixtureSuite,
      spec,
      fixtureSpec, 
      junit3Suite, 
      junitSuite,
      testngSuite
    )
}
