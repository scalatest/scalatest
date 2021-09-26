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

import org.scalatestplus.junit._
import org.junit.Test
import org.scalatest.refspec.RefSpec
import org.scalatestplus.testng.TestNGSuite

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
class DeprecatedExampleClassTaggingFixtureSuite extends fixture.TestSuite with DeprecatedClassTaggingFixtureServices with StringFixture {
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
class DeprecatedExampleClassTaggingFunSuite extends FunSuite with DeprecatedClassTaggingFixtureServices {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFunSuite extends fixture.FunSuite with DeprecatedClassTaggingFixtureServices with StringFixture {
  test("Test 1") {s =>}
  test("Test 2") {s =>}
  test("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFunSpec extends FunSpec with DeprecatedClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFunSpec extends fixture.FunSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  describe("Scope 1") {
    it("Test 1") {s =>}
    it("Test 2") {s =>}
    it("Test 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFeatureSpec extends FeatureSpec with DeprecatedClassTaggingFixtureServices {
  feature("Feature 1") {
    scenario("Scenario 1") {}
    scenario("Scenario 2") {}
    scenario("Scenario 3") {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFeatureSpec extends fixture.FeatureSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  feature("Feature 1") {
    scenario("Scenario 1") {s =>}
    scenario("Scenario 2") {s =>}
    scenario("Scenario 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFlatSpec extends FlatSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in {}
  it should "do thing 3" in {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFlatSpec extends fixture.FlatSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  "Scope 1" should "do thing 1" in {s =>}
  it should "do thing 2" in {s =>}
  it should "do thing 3" in {s =>}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFreeSpec extends FreeSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureFreeSpec extends fixture.FreeSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  "Scope 1" - {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingPropSpec extends PropSpec with DeprecatedClassTaggingFixtureServices {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixturePropSpec extends fixture.PropSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  property("Test 1") {s =>}
  property("Test 2") {s =>}
  property("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingWordSpec extends WordSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingFixtureWordSpec extends fixture.WordSpec with DeprecatedClassTaggingFixtureServices with StringFixture {
  "Scope 1" should {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingPathFreeSpec extends path.FreeSpec with DeprecatedClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class DeprecatedExampleClassTaggingPathFunSpec extends path.FunSpec with DeprecatedClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}
