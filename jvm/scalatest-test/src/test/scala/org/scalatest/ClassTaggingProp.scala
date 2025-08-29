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

class ClassTaggingProp extends AllSuiteProp {

  type FixtureServices = ClassTaggingFixtureServices
  
  def suite = new ExampleClassTaggingSuite
  def fixtureSuite = new ExampleClassTaggingFixtureSuite
  def spec = new ExampleClassTaggingSpec
  def junit3Suite = new ExampleClassTaggingJUnit3Suite
  def junitSuite = new ExampleClassTaggingJUnitSuite
  def testngSuite = new ExampleClassTaggingTestNGSuite
  def funSuite = new ExampleClassTaggingFunSuite
  def fixtureFunSuite = new ExampleClassTaggingFixtureFunSuite
  def funSpec = new ExampleClassTaggingFunSpec
  def fixtureFunSpec = new ExampleClassTaggingFixtureFunSpec
  def featureSpec = new ExampleClassTaggingFeatureSpec
  def fixtureFeatureSpec = new ExampleClassTaggingFixtureFeatureSpec
  def flatSpec = new ExampleClassTaggingFlatSpec
  def fixtureFlatSpec = new ExampleClassTaggingFixtureFlatSpec
  def freeSpec = new ExampleClassTaggingFreeSpec
  def fixtureFreeSpec = new ExampleClassTaggingFixtureFreeSpec
  def propSpec = new ExampleClassTaggingPropSpec
  def fixturePropSpec = new ExampleClassTaggingFixturePropSpec
  def wordSpec = new ExampleClassTaggingWordSpec
  def fixtureWordSpec = new ExampleClassTaggingFixtureWordSpec
  def pathFreeSpec = new ExampleClassTaggingPathFreeSpec
  def pathFunSpec = new ExampleClassTaggingPathFunSpec
  
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

trait ClassTaggingFixtureServices {
  def included = this match {
    case _: JUnit3Suite => false
    case _ => true
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingSuite extends Suite with ClassTaggingFixtureServices {
  def testMethod1(): Unit = {}
  def testMethod2(): Unit = {}
  def testMethod3(): Unit = {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureSuite extends scalatest.FixtureTestSuite with ClassTaggingFixtureServices with StringFixture {
  def testMethod1(): Unit = {}
  def testMethod2(): Unit = {}
  def testMethod3(): Unit = {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingSpec extends RefSpec with ClassTaggingFixtureServices {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingJUnit3Suite extends JUnit3Suite with ClassTaggingFixtureServices  // JUnit3Suite does not support tag, it always return empty map in tags method.

@DoNotDiscover
@Ignore
class ExampleClassTaggingJUnitSuite extends JUnitSuite with ClassTaggingFixtureServices {
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
class ExampleClassTaggingTestNGSuite extends TestNGSuite with ClassTaggingFixtureServices {
  @TestNG
  def testMethod1(): Unit = {}
  @TestNG
  def testMethod2(): Unit = {}
  @TestNG
  def testMethod3(): Unit = {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFunSuite extends AnyFunSuite with ClassTaggingFixtureServices {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFunSuite extends funsuite.FixtureAnyFunSuite with ClassTaggingFixtureServices with StringFixture {
  test("Test 1") {s =>}
  test("Test 2") {s =>}
  test("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFunSpec extends AnyFunSpec with ClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFunSpec extends funspec.FixtureAnyFunSpec with ClassTaggingFixtureServices with StringFixture {
  describe("Scope 1") {
    it("Test 1") {s =>}
    it("Test 2") {s =>}
    it("Test 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFeatureSpec extends AnyFeatureSpec with ClassTaggingFixtureServices {
  Feature("Feature 1") {
    Scenario("Scenario 1") {}
    Scenario("Scenario 2") {}
    Scenario("Scenario 3") {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with ClassTaggingFixtureServices with StringFixture {
  Feature("Feature 1") {
    Scenario("Scenario 1") {s =>}
    Scenario("Scenario 2") {s =>}
    Scenario("Scenario 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFlatSpec extends AnyFlatSpec with ClassTaggingFixtureServices {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in {}
  it should "do thing 3" in {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with ClassTaggingFixtureServices with StringFixture {
  "Scope 1" should "do thing 1" in {s =>}
  it should "do thing 2" in {s =>}
  it should "do thing 3" in {s =>}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFreeSpec extends AnyFreeSpec with ClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with ClassTaggingFixtureServices with StringFixture {
  "Scope 1" - {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingPropSpec extends AnyPropSpec with ClassTaggingFixtureServices {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixturePropSpec extends propspec.FixtureAnyPropSpec with ClassTaggingFixtureServices with StringFixture {
  property("Test 1") {s =>}
  property("Test 2") {s =>}
  property("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingWordSpec extends AnyWordSpec with ClassTaggingFixtureServices {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureWordSpec extends wordspec.FixtureAnyWordSpec with ClassTaggingFixtureServices with StringFixture {
  "Scope 1" should {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingPathFreeSpec extends freespec.PathAnyFreeSpec with ClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingPathFunSpec extends funspec.PathAnyFunSpec with ClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}
