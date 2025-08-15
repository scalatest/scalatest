/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalatestplus.junit._
import org.junit.Test
import org.scalatest.refspec.RefSpec
import org.scalatestplus.testng.TestNGSuite
import org.scalatest
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class DeprecatedClassTaggingProp extends AllSuiteProp {

  type FixtureServices = DeprecatedClassTaggingFixtureServices
  
  def suite = new DeprecatedExampleClassTaggingSuite
  def fixtureSuite = new DeprecatedExampleClassTaggingFixtureSuite
  def spec = new DeprecatedExampleClassTaggingSpec
  def junit3Suite = new DeprecatedExampleClassTaggingJUnit3Suite
  def junitSuite = new DeprecatedExampleClassTaggingJUnitSuite
  def testngSuite = new DeprecatedExampleClassTaggingTestNGSuite
  def funSuite = new DeprecatedExampleClassTaggingFunSuite
  def fixtureFunSuite = new DeprecatedExampleClassTaggingFixtureFunSuite
  def funSpec = new DeprecatedExampleClassTaggingFunSpec
  def fixtureFunSpec = new DeprecatedExampleClassTaggingFixtureFunSpec
  def featureSpec = new DeprecatedExampleClassTaggingFeatureSpec
  def fixtureFeatureSpec = new DeprecatedExampleClassTaggingFixtureFeatureSpec
  def flatSpec = new DeprecatedExampleClassTaggingFlatSpec
  def fixtureFlatSpec = new DeprecatedExampleClassTaggingFixtureFlatSpec
  def freeSpec = new DeprecatedExampleClassTaggingFreeSpec
  def fixtureFreeSpec = new DeprecatedExampleClassTaggingFixtureFreeSpec
  def propSpec = new DeprecatedExampleClassTaggingPropSpec
  def fixturePropSpec = new DeprecatedExampleClassTaggingFixturePropSpec
  def wordSpec = new DeprecatedExampleClassTaggingWordSpec
  def fixtureWordSpec = new DeprecatedExampleClassTaggingFixtureWordSpec
  def pathFreeSpec = new DeprecatedExampleClassTaggingPathFreeSpec
  def pathFunSpec = new DeprecatedExampleClassTaggingPathFunSpec
  
  test("@Ignore marked at the class level must auto-mark all tests in the suite") {
    forAll(examples.filter(_.included)) { s =>
      val testNames = s.testNames
      val tags = s.tags
      testNames.foreach { tn => 
        val tagSet = tags.get(tn)
        tagSet.isDefined should be (true)
        tagSet.get should contain ("org.scalatest.Ignore")
      }
    }
  }
}

trait DeprecatedClassTaggingFixtureServices {
  def included = this match {
    case _: JUnit3Suite => false
    case _ => true
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingSuite extends Suite with DeprecatedClassTaggingFixtureServices {
  def testMethod1(): Unit = {}
  def testMethod2(): Unit = {}
  def testMethod3(): Unit = {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureSuite extends scalatest.FixtureTestSuite with DeprecatedClassTaggingFixtureServices with StringFixture {
  def testMethod1(): Unit = {}
  def testMethod2(): Unit = {}
  def testMethod3(): Unit = {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingSpec extends RefSpec with DeprecatedClassTaggingFixtureServices {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingJUnit3Suite extends JUnit3Suite with DeprecatedClassTaggingFixtureServices  // JUnit3Suite does not support tag, it always return empty map in tags method.

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingJUnitSuite extends JUnitSuite with DeprecatedClassTaggingFixtureServices {
  @Test
  def testMethod1(): Unit = {}
  @Test 
  def testMethod2(): Unit = {}
  @Test 
  def testMethod3(): Unit = {}
}

import org.testng.annotations.{Test => TestNG }

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingTestNGSuite extends TestNGSuite with DeprecatedClassTaggingFixtureServices {
  @TestNG
  def testMethod1(): Unit = {}
  @TestNG
  def testMethod2(): Unit = {}
  @TestNG
  def testMethod3(): Unit = {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFunSuite extends AnyFunSuite with DeprecatedClassTaggingFixtureServices {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFunSuite extends funsuite.FixtureAnyFunSuite with DeprecatedClassTaggingFixtureServices with StringFixture {
  test("Test 1") {s =>}
  test("Test 2") {s =>}
  test("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFunSpec extends AnyFunSpec with DeprecatedClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFunSpec extends funspec.FixtureAnyFunSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  describe("Scope 1") {
    it("Test 1") {s =>}
    it("Test 2") {s =>}
    it("Test 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFeatureSpec extends AnyFeatureSpec with DeprecatedClassTaggingFixtureServices {
  Feature("Feature 1") {
    Scenario("Scenario 1") {}
    Scenario("Scenario 2") {}
    Scenario("Scenario 3") {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  Feature("Feature 1") {
    Scenario("Scenario 1") {s =>}
    Scenario("Scenario 2") {s =>}
    Scenario("Scenario 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFlatSpec extends AnyFlatSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in {}
  it should "do thing 3" in {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  "Scope 1" should "do thing 1" in {s =>}
  it should "do thing 2" in {s =>}
  it should "do thing 3" in {s =>}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFreeSpec extends AnyFreeSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  "Scope 1" - {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingPropSpec extends AnyPropSpec with DeprecatedClassTaggingFixtureServices {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixturePropSpec extends propspec.FixtureAnyPropSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  property("Test 1") {s =>}
  property("Test 2") {s =>}
  property("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingWordSpec extends AnyWordSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureWordSpec extends wordspec.FixtureAnyWordSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  "Scope 1" should {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingPathFreeSpec extends freespec.PathAnyFreeSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingPathFunSpec extends funspec.PathAnyFunSpec with DeprecatedClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}
