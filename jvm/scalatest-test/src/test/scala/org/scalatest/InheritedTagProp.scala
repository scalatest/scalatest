/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalatestplus.junit.{JUnitSuite, JUnit3Suite}
import org.scalatestplus.testng.TestNGSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG}
import org.scalatest.refspec.RefSpec
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

trait InheritedTagFixtureServices

class InheritedTagProp extends SuiteProp {

  // JUnit3Suite does not support tagging
  val filteredExamples =
    examples.filter { e =>
      e match {
        case _: JUnit3Suite => false
        case _ => true
      }
    }

  test("All suite types should return inherited org.scalatest.tags.ChromeBrowser tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.ChromeBrowser"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.FirefoxBrowser tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.FirefoxBrowser"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.HtmlUnitBrowser tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.HtmlUnitBrowser"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.InternetExplorerBrowser tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.InternetExplorerBrowser"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.SafariBrowser tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.SafariBrowser"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.CPU tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.CPU"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Disk tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.Disk"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Network tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.Network"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Retryable tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.Retryable"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Slow tag inherited from super class in tags method") {
    forAll(filteredExamples) { suite =>
      val resultTags = suite.tags
      assert(resultTags.size == 1)
      resultTags.foreach { case (testName, tagSet) =>
        assert(tagSet.contains("org.scalatest.tags.Slow"))
      }
    }
  }

  test("All suite types should return inherited org.scalatest.tags.ChromeBrowser tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.ChromeBrowser"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.FirefoxBrowser tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.FirefoxBrowser"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.HtmlUnitBrowser tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.HtmlUnitBrowser"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.InternetExplorerBrowser tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.InternetExplorerBrowser"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.SafariBrowser tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.SafariBrowser"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.CPU tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.CPU"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Disk tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.Disk"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Network tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.Network"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Retryable tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.Retryable"))
    }
  }

  test("All suite types should return inherited org.scalatest.tags.Slow tag inherited from super class in testDataFor method") {
    forAll(filteredExamples) { suite =>
      assert(suite.testNames.size == 1)
      val resultTestData = suite.testDataFor(suite.testNames.head)
      assert(resultTestData.tags.contains("org.scalatest.tags.Slow"))
    }
  }

  type FixtureServices = InheritedTagFixtureServices

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagJUnit3Suite extends JUnit3Suite with InheritedTagFixtureServices
  class InheritedTagJUnit3Suite extends BaseTagJUnit3Suite {
    def testSomething(): Unit = {}
  }
  def junit3Suite = new InheritedTagJUnit3Suite

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagJUnitSuite extends JUnitSuite with InheritedTagFixtureServices
  class InheritedTagJUnitSuite extends BaseTagJUnitSuite {
    @Test def testSomething(): Unit = {}
  }
  def junitSuite = new InheritedTagJUnitSuite

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagTestNGSuite extends TestNGSuite with FixtureServices
  class InheritedTagTestNGSuite extends BaseTagTestNGSuite {
    @TestNG def testSomething(): Unit = {}
  }
  def testngSuite = new InheritedTagTestNGSuite

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFunSuite extends AnyFunSuite with FixtureServices
  class InheritedTagFunSuite extends BaseTagFunSuite {
    test("something") {}
  }
  def funSuite = new InheritedTagFunSuite

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFixtureFunSuite extends funsuite.FixtureAnyFunSuite with StringFixture with FixtureServices
  class InheritedTagFixtureFunSuite extends BaseTagFixtureFunSuite {
    test("something") { f => }
  }
  def fixtureFunSuite = new InheritedTagFixtureFunSuite

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFunSpec extends AnyFunSpec with FixtureServices
  class InheritedTagFunSpec extends BaseTagFunSpec {
    it("test something") {}
  }
  def funSpec = new InheritedTagFunSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFixtureFunSpec extends funspec.FixtureAnyFunSpec with StringFixture with FixtureServices
  class InheritedTagFixtureFunSpec extends BaseTagFixtureFunSpec {
    it("test something") { f => }
  }
  def fixtureFunSpec = new InheritedTagFixtureFunSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFeatureSpec extends AnyFeatureSpec with FixtureServices
  class InheritedTagFeatureSpec extends BaseTagFeatureSpec {
    Scenario("test something") {}
  }
  def featureSpec = new InheritedTagFeatureSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with StringFixture with FixtureServices
  class InheritedTagFixtureFeatureSpec extends BaseTagFixtureFeatureSpec {
    Scenario("test something") { f => }
  }
  def fixtureFeatureSpec = new InheritedTagFixtureFeatureSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFlatSpec extends AnyFlatSpec with FixtureServices
  class InheritedTagFlatSpec extends BaseTagFlatSpec {
    "This" should "test something" in {}
  }
  def flatSpec = new InheritedTagFlatSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with StringFixture with FixtureServices
  class InheritedTagFixtureFlatSpec extends BaseTagFixtureFlatSpec {
    "This" should "test something" in { f => }
  }
  def fixtureFlatSpec = new InheritedTagFixtureFlatSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFreeSpec extends AnyFreeSpec with FixtureServices
  class InheritedTagFreeSpec extends BaseTagFreeSpec {
    "test something" in {}
  }
  def freeSpec = new InheritedTagFreeSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with StringFixture with FixtureServices
  class InheritedTagFixtureFreeSpec extends BaseTagFixtureFreeSpec
  def fixtureFreeSpec = new InheritedTagFixtureFreeSpec {
    "test something" in { f => }
  }

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagPropSpec extends AnyPropSpec with FixtureServices
  class InheritedTagPropSpec extends BaseTagPropSpec {
    property("test something") {}
  }
  def propSpec = new InheritedTagPropSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFixturePropSpec extends propspec.FixtureAnyPropSpec with StringFixture with FixtureServices
  class InheritedTagFixturePropSpec extends BaseTagFixturePropSpec {
    property("test something") { f => }
  }
  def fixturePropSpec = new InheritedTagFixturePropSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagWordSpec extends AnyWordSpec with FixtureServices
  class InheritedTagWordSpec extends BaseTagWordSpec {
    "test something" in {}
  }
  def wordSpec = new InheritedTagWordSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagFixtureWordSpec extends wordspec.FixtureAnyWordSpec with StringFixture with FixtureServices
  class InheritedTagFixtureWordSpec extends BaseTagFixtureWordSpec {
    "test something" in { f => }
  }
  def fixtureWordSpec = new InheritedTagFixtureWordSpec

  @org.scalatest.tags.ChromeBrowser
  @org.scalatest.tags.FirefoxBrowser
  @org.scalatest.tags.HtmlUnitBrowser
  @org.scalatest.tags.InternetExplorerBrowser
  @org.scalatest.tags.SafariBrowser
  @org.scalatest.tags.CPU
  @org.scalatest.tags.Disk
  @org.scalatest.tags.Network
  @org.scalatest.tags.Retryable
  @org.scalatest.tags.Slow
  class BaseTagSpec extends RefSpec with FixtureServices
  class InheritedTagSpec extends BaseTagSpec {
    def `test something`: Unit = {}
  }
  def spec = new InheritedTagSpec
}
