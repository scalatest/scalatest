/*
 * Copyright 2001-2011 Artima, Inc.
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

trait SuiteExamples extends Tables {

  type FixtureServices
  
  def suite: Suite with FixtureServices
  def fixtureSuite: fixture.Suite with FixtureServices
  def junit3Suite: JUnit3Suite with FixtureServices
  def junitSuite: JUnitSuite with FixtureServices
  def testngSuite: TestNGSuite with FixtureServices
  def funSuite: FunSuite with FixtureServices
  def fixtureFunSuite: fixture.FunSuite with FixtureServices
  def funSpec: FunSpec with FixtureServices
  def fixtureSpec: fixture.FunSpec with FixtureServices
  def featureSpec: FeatureSpec with FixtureServices
  def fixtureFeatureSpec: fixture.FeatureSpec with FixtureServices
  def flatSpec: FlatSpec with FixtureServices
  def fixtureFlatSpec: fixture.FlatSpec with FixtureServices
  def freeSpec: FreeSpec with FixtureServices
  def fixtureFreeSpec: fixture.FreeSpec with FixtureServices
  def propSpec: PropSpec with FixtureServices
  def fixturePropSpec: fixture.PropSpec with FixtureServices
  def wordSpec: WordSpec with FixtureServices
  def fixtureWordSpec: fixture.WordSpec with FixtureServices

  def examples =
    Table(
      "suite",
      suite,
      fixtureSuite,
      junit3Suite,
      junitSuite,
      testngSuite,
      funSuite,
      fixtureFunSuite,
      funSpec,
      fixtureSpec,
      featureSpec,
      fixtureFeatureSpec,
      flatSpec,
      fixtureFlatSpec,
      freeSpec,
      fixtureFreeSpec,
      propSpec,
      fixturePropSpec,
      wordSpec,
      fixtureWordSpec
    )
}
