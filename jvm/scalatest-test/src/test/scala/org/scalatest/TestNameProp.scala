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
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatestplus.junit.JUnit3Suite
import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG }
import org.scalatestplus.testng.TestNGSuite
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class TestNameProp  extends AllSuiteProp {

  type FixtureServices = TestNameFixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  def spec = new ExampleTestNameSpec
  def junit3Suite = new ExampleTestNameJUnit3Suite
  def junitSuite = new ExampleTestNameJUnitSuite
  def testngSuite = new ExampleTestNameTestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
  def funSuite = new ExampleTestNameFunSuite
  def fixtureFunSuite = new ExampleTestNameFixtureFunSuite
  def funSpec = new ExampleTestNameFunSpec
  def fixtureFunSpec = new ExampleTestNameFixtureFunSpec
  def featureSpec = new ExampleTestNameFeatureSpec
  def fixtureFeatureSpec = new ExampleTestNameFixtureFeatureSpec
  def flatSpec = new ExampleTestNameFlatSpec
  def fixtureFlatSpec = new ExampleTestNameFixtureFlatSpec
  def freeSpec = new ExampleTestNameFreeSpec
  def fixtureFreeSpec = new ExampleTestNameFixtureFreeSpec
  def propSpec = new ExampleTestNamePropSpec
  def fixturePropSpec = new ExampleTestNameFixturePropSpec
  def wordSpec = new ExampleTestNameWordSpec
  def fixtureWordSpec = new ExampleTestNameFixtureWordSpec
  def pathFreeSpec = new ExampleTestNamePathFreeSpec
  def pathFunSpec = new ExampleTestNamePathFunSpec
  
  test("test name will be constructed by concatennating scopes, outer to inner, followed by the test text, separated by a space after each component is trimmed.") {
    forAll(examples) { s => s.assertTestNames() }
  }
  
}

trait TestNameFixtureServices { suite: Suite =>
  val expectedTestNames: Set[String]
  
  def assertTestNames(): Unit = {
    val expectedSet = expectedTestNames
    val testNameSet = testNames
    assert(expectedSet.size === testNameSet.size)
    expectedSet.foreach { tn =>
      assert(testNameSet contains tn, "Unable to find test name: '" + tn + "', testNames is: \n" + testNameSet.map("'" + _ + "'").mkString("\n"))
    }
  }
}

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
class ExampleTestNameSpec extends RefSpec with TestNameFixtureServices {
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun", 
     "Testing 2 Scala code should be fun", 
     "Testing 3 Scala code should be fun", 
     "Testing 4 Scala code should be fun", 
     "Testing 5 Scala code should be fun", 
     "Testing 6 Scala code should be fun", 
     "Testing 7 Scala code should be fun", 
     "Testing 8 Scala code should be fun", 
     "Testing 9 Scala code should be fun"
    )
  
  object `Testing 1` { 
    object `Scala code` {
      def `should be fun`: Unit = {}
    }
  }

  object `Testing 2 ` { 
    object `Scala code` {
      def `should be fun`: Unit = {}
    }
  }

  object `Testing 3` { 
    object ` Scala code` {
      def `should be fun`: Unit = {}
    }
  }

  object `Testing 4` { 
    object `Scala code ` {
      def `should be fun`: Unit = {}
    }
  }

  object `Testing 5` { 
    object `Scala code` {
      def ` should be fun`: Unit = {}
    }
  }

  object ` Testing 6` { 
    object `Scala code` {
      def `should be fun`: Unit = {}
    }
  }

  object `Testing 7` { 
    object `Scala code` {
      def `should be fun `: Unit = {}
    }
  }

  object `Testing 8 ` { 
    object ` Scala code` {
      def `should be fun`: Unit = {}
    }
  }

  object `Testing 9  ` { 
    object `Scala code` {
      def `should be fun`: Unit = {}
    }
  }
}

@DoNotDiscover
class ExampleTestNameJUnit3Suite extends JUnit3Suite with TestNameFixtureServices {
  
  val expectedTestNames = 
    Set(
     "testingShouldBeFun"     
    )
    
  def testingShouldBeFun(): Unit = { }
}

@DoNotDiscover
class ExampleTestNameJUnitSuite extends JUnitSuite with TestNameFixtureServices {
  
  val expectedTestNames = 
    Set(
     "testingShouldBeFun"     
    )
  
  @Test
  def testingShouldBeFun(): Unit = {}
}

@DoNotDiscover
class ExampleTestNameTestNGSuite extends TestNGSuite with TestNameFixtureServices {
  
  val expectedTestNames = 
    Set(
     "testingShouldBeFun"     
    )
  
  @TestNG
  def testingShouldBeFun(): Unit = {}
}
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[scalatest] class ExampleTestNameFunSuite extends AnyFunSuite with TestNameFixtureServices {
  
  val expectedTestNames = 
    Set(
     "Testing 1 should be fun",
     "Testing 2 should be fun",
     "Testing 3 should be fun",
     "Testing 4 should be fun",
     "Testing 5 should be fun"     
    )
  
  test("Testing 1 should be fun") {}
  test(" Testing 2 should be fun") {}
  test("Testing 3 should be fun ") {}
  test("  Testing 4 should be fun") {}
  test("Testing 5 should be fun  ") {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFixtureFunSuite extends funsuite.FixtureAnyFunSuite with TestNameFixtureServices with StringFixture {
  
  val expectedTestNames = 
    Set(
     "Testing 1 should be fun",
     "Testing 2 should be fun",
     "Testing 3 should be fun",
     "Testing 4 should be fun",
     "Testing 5 should be fun"     
    )
  
  test("Testing 1 should be fun") { s => }
  test(" Testing 2 should be fun") { s => }
  test("Testing 3 should be fun ") { s => }
  test("  Testing 4 should be fun") { s => }
  test("Testing 5 should be fun  ") { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFunSpec extends AnyFunSpec with TestNameFixtureServices {
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun", 
     "Testing 2 Scala code should be fun", 
     "Testing 3 Scala code should be fun", 
     "Testing 4 Scala code should be fun", 
     "Testing 5 Scala code should be fun", 
     "Testing 6 Scala code should be fun", 
     "Testing 7 Scala code should be fun", 
     "Testing 8 Scala code should be fun", 
     "Testing 9 Scala code should be fun"
    )
  
  describe("Testing 1") { 
    describe("Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 2 ") { 
    describe("Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 3") { 
    describe(" Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 4") { 
    describe("Scala code ") {
      it("should be fun") {}
    }
  }

  describe("Testing 5") { 
    describe("Scala code") {
      it(" should be fun") {}
    }
  }

  describe(" Testing 6") { 
    describe("Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 7") { 
    describe("Scala code") {
      it("should be fun ") {}
    }
  }

  describe("Testing 8 ") {  
    describe(" Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 9  ") {  
    describe("Scala code") {
      it("should be fun") {}
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFixtureFunSpec extends funspec.FixtureAnyFunSpec with TestNameFixtureServices with StringFixture {
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun", 
     "Testing 2 Scala code should be fun", 
     "Testing 3 Scala code should be fun", 
     "Testing 4 Scala code should be fun", 
     "Testing 5 Scala code should be fun", 
     "Testing 6 Scala code should be fun", 
     "Testing 7 Scala code should be fun", 
     "Testing 8 Scala code should be fun", 
     "Testing 9 Scala code should be fun"
    )
  
  describe("Testing 1") { 
    describe("Scala code") {
      it("should be fun") { s => }
    }
  }

  describe("Testing 2 ") { 
    describe("Scala code") {
      it("should be fun") { s => }
    }
  }

  describe("Testing 3") { 
    describe(" Scala code") {
      it("should be fun") { s => }
    }
  }

  describe("Testing 4") { 
    describe("Scala code ") {
      it("should be fun") { s => }
    }
  }

  describe("Testing 5") { 
    describe("Scala code") {
      it(" should be fun") { s => }
    }
  }

  describe(" Testing 6") { 
    describe("Scala code") {
      it("should be fun") { s => }
    }
  }

  describe("Testing 7") { 
    describe("Scala code") {
      it("should be fun ") { s => }
    }
  }

  describe("Testing 8 ") { 
    describe(" Scala code") {
      it("should be fun") { s => }
    }
  }

  describe("Testing 9  ") { 
    describe("Scala code") {
      it("should be fun") { s => }
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFeatureSpec extends AnyFeatureSpec with TestNameFixtureServices {
  val expectedTestNames = 
    Set(
     "Feature: Testing 1 Scenario: Scala code should be fun", 
     "Feature: Testing 2 Scenario: Scala code should be fun", 
     "Feature: Testing 3 Scenario: Scala code should be fun", 
     "Feature: Testing 4 Scenario: Scala code should be fun", 
     "Feature: Testing 5 Scenario: Scala code should be fun", 
     "Feature: Testing 6 Scenario: Scala code should be fun", 
     "Feature: Testing 7 Scenario: Scala code should be fun"
    )
  
  Feature("Testing 1") { 
    Scenario("Scala code should be fun") {}
  }

  Feature("Testing 2 ") { 
    Scenario("Scala code should be fun") {}
  }

  Feature("Testing 3") { 
    Scenario(" Scala code should be fun") {}
  }

  Feature("Testing 4") { 
    Scenario("Scala code should be fun ") {}
  }

  Feature(" Testing 5") { 
    Scenario("Scala code should be fun") {}
  }

  Feature("Testing 6 ") { 
    Scenario(" Scala code should be fun") {}
  }

  Feature("Testing 7  ") {  
    Scenario("Scala code should be fun") {}
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with TestNameFixtureServices with StringFixture {
  val expectedTestNames = 
    Set(
     "Feature: Testing 1 Scenario: Scala code should be fun", 
     "Feature: Testing 2 Scenario: Scala code should be fun", 
     "Feature: Testing 3 Scenario: Scala code should be fun", 
     "Feature: Testing 4 Scenario: Scala code should be fun", 
     "Feature: Testing 5 Scenario: Scala code should be fun", 
     "Feature: Testing 6 Scenario: Scala code should be fun", 
     "Feature: Testing 7 Scenario: Scala code should be fun"
    )
  
  Feature("Testing 1") { 
    Scenario("Scala code should be fun") { s => }
  }

  Feature("Testing 2 ") { 
    Scenario("Scala code should be fun") { s => }
  }

  Feature("Testing 3") { 
    Scenario(" Scala code should be fun") { s => }
  }

  Feature("Testing 4") { 
    Scenario("Scala code should be fun ") { s => }
  }

  Feature(" Testing 5") { 
    Scenario("Scala code should be fun") { s => }
  }

  Feature("Testing 6 ") { 
    Scenario(" Scala code should be fun") { s => }
  }

  Feature("Testing 7  ") { 
    Scenario("Scala code should be fun") { s => }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFlatSpec extends AnyFlatSpec with TestNameFixtureServices {
  
  val expectedTestNames = 
    Set(
     "Testing 1 should be fun to code in Scala", 
     "Testing 2 should be fun to code in Scala", 
     "Testing 3 should be fun to code in Scala", 
     "Testing 4 should be fun to code in Scala", 
     "Testing 5 should be fun to code in Scala", 
     "Testing 6 should be fun to code in Scala", 
     "Testing 7 should be fun to code in Scala"
    )
  
  "Testing 1" should "be fun to code in Scala" in { 
  }

  "Testing 2 " should "be fun to code in Scala" in { 
  }

  "Testing 3" should " be fun to code in Scala" in { 
  }

  "Testing 4" should "be fun to code in Scala " in { 
  }

  " Testing 5" should "be fun to code in Scala" in { 
  }

  "Testing 6 " should " be fun to code in Scala" in {  
  }

  "Testing 7  " should "be fun to code in Scala" in {  
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with TestNameFixtureServices with StringFixture {
  val expectedTestNames = 
    Set(
     "Testing 1 should be fun to code in Scala", 
     "Testing 2 should be fun to code in Scala", 
     "Testing 3 should be fun to code in Scala", 
     "Testing 4 should be fun to code in Scala", 
     "Testing 5 should be fun to code in Scala", 
     "Testing 6 should be fun to code in Scala", 
     "Testing 7 should be fun to code in Scala"
    )
  
  "Testing 1" should "be fun to code in Scala" in { s => 
  }

  "Testing 2 " should "be fun to code in Scala" in { s => 
  }

  "Testing 3" should " be fun to code in Scala" in { s => 
  }

  "Testing 4" should "be fun to code in Scala " in { s => 
  }

  " Testing 5" should "be fun to code in Scala" in { s => 
  }

  "Testing 6 " should " be fun to code in Scala" in { s =>  
  }

  "Testing 7  " should "be fun to code in Scala" in { s =>  
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFreeSpec extends AnyFreeSpec with TestNameFixtureServices {
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun", 
     "Testing 2 Scala code should be fun", 
     "Testing 3 Scala code should be fun", 
     "Testing 4 Scala code should be fun", 
     "Testing 5 Scala code should be fun", 
     "Testing 6 Scala code should be fun", 
     "Testing 7 Scala code should be fun", 
     "Testing 8 Scala code should be fun", 
     "Testing 9 Scala code should be fun"
    )
  
  "Testing 1" - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 2 " - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 3" - { 
    " Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 4" - { 
    "Scala code " - {
      "should be fun" in {}
    }
  }

  "Testing 5" - { 
    "Scala code" - {
      " should be fun" in {}
    }
  }

  " Testing 6" - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 7" - { 
    "Scala code" - {
      "should be fun " in {}
    }
  }

  "Testing 8 " - { 
    " Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 9  " - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with TestNameFixtureServices with StringFixture {
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun", 
     "Testing 2 Scala code should be fun", 
     "Testing 3 Scala code should be fun", 
     "Testing 4 Scala code should be fun", 
     "Testing 5 Scala code should be fun", 
     "Testing 6 Scala code should be fun", 
     "Testing 7 Scala code should be fun", 
     "Testing 8 Scala code should be fun", 
     "Testing 9 Scala code should be fun"
    )
  
  "Testing 1" - { 
    "Scala code" - {
      "should be fun" in { s => }
    }
  }

  "Testing 2 " - { 
    "Scala code" - {
      "should be fun" in { s => }
    }
  }

  "Testing 3" - { 
    " Scala code" - {
      "should be fun" in { s => }
    }
  }

  "Testing 4" - { 
    "Scala code " - {
      "should be fun" in { s => }
    }
  }

  "Testing 5" - { 
    "Scala code" - {
      " should be fun" in { s => }
    }
  }

  " Testing 6" - { 
    "Scala code" - {
      "should be fun" in { s => }
    }
  }

  "Testing 7" - { 
    "Scala code" - {
      "should be fun " in { s => }
    }
  }

  "Testing 8 " - { 
    " Scala code" - {
      "should be fun" in { s => }
    }
  }

  "Testing 9  " - { 
    "Scala code" - {
      "should be fun" in { s => }
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNamePropSpec extends AnyPropSpec with TestNameFixtureServices {
  
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun",
     "Testing 2 Scala code should be fun",
     "Testing 3 Scala code should be fun",
     "Testing 4 Scala code should be fun",
     "Testing 5 Scala code should be fun",
     "Testing 6 Scala code should be fun"
    )
  
  property("Testing 1 Scala code should be fun") {}
  property(" Testing 2 Scala code should be fun") {}
  property("Testing 3 Scala code should be fun ") {}
  property("  Testing 4 Scala code should be fun") {}
  property("Testing 5 Scala code should be fun  ") {}
  property("  Testing 6 Scala code should be fun  ") {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFixturePropSpec extends propspec.FixtureAnyPropSpec with TestNameFixtureServices with StringFixture {
  
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun",
     "Testing 2 Scala code should be fun",
     "Testing 3 Scala code should be fun",
     "Testing 4 Scala code should be fun",
     "Testing 5 Scala code should be fun",
     "Testing 6 Scala code should be fun"
    )
  
  property("Testing 1 Scala code should be fun") { s => }
  property(" Testing 2 Scala code should be fun") { s => }
  property("Testing 3 Scala code should be fun ") { s => }
  property("  Testing 4 Scala code should be fun") { s => }
  property("Testing 5 Scala code should be fun  ") { s => }
  property("  Testing 6 Scala code should be fun  ") { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameWordSpec extends AnyWordSpec with TestNameFixtureServices {
  val expectedTestNames = 
    Set(
     "Testing 1 should test Scala code should be fun", 
     "Testing 2 should test Scala code should be fun", 
     "Testing 3 should test Scala code should be fun", 
     "Testing 4 should test Scala code should be fun", 
     "Testing 5 should test Scala code should be fun", 
     "Testing 6 should test Scala code should be fun", 
     "Testing 7 should test Scala code should be fun", 
     "Testing 8 should test Scala code should be fun", 
     "Testing 9 should test Scala code should be fun"
    )
  
  "Testing 1" should { 
    "test Scala code" should {
      "be fun" in {}
    }
  }

  "Testing 2 " should { 
    "test Scala code" should {
      "be fun" in {}
    }
  }

  "Testing 3" should { 
    " test Scala code" should {
      "be fun" in {}
    }
  }

  "Testing 4" should { 
    "test Scala code " should {
      "be fun" in {}
    }
  }

  "Testing 5" should { 
    "test Scala code" should {
      " be fun" in {}
    }
  }

  " Testing 6" should { 
    "test Scala code" should {
      "be fun" in {}
    }
  }

  "Testing 7" should { 
    "test Scala code" should {
      "be fun " in {}
    }
  }

  "Testing 8 " should { 
    " test Scala code" should {
      "be fun" in {}
    }
  }

  "Testing 9  " should { 
    "test Scala code" should {
      "be fun" in {}
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNameFixtureWordSpec extends wordspec.FixtureAnyWordSpec with TestNameFixtureServices with StringFixture {
  val expectedTestNames = 
    Set(
     "Testing 1 should test Scala code should be fun", 
     "Testing 2 should test Scala code should be fun", 
     "Testing 3 should test Scala code should be fun", 
     "Testing 4 should test Scala code should be fun", 
     "Testing 5 should test Scala code should be fun", 
     "Testing 6 should test Scala code should be fun", 
     "Testing 7 should test Scala code should be fun", 
     "Testing 8 should test Scala code should be fun", 
     "Testing 9 should test Scala code should be fun"
    )
  
  "Testing 1" should { 
    "test Scala code" should {
      "be fun" in { s => }
    }
  }

  "Testing 2 " should { 
    "test Scala code" should {
      "be fun" in { s => }
    }
  }

  "Testing 3" should { 
    " test Scala code" should {
      "be fun" in { s => }
    }
  }

  "Testing 4" should { 
    "test Scala code " should {
      "be fun" in { s => }
    }
  }

  "Testing 5" should { 
    "test Scala code" should {
      " be fun" in { s => }
    }
  }

  " Testing 6" should { 
    "test Scala code" should {
      "be fun" in { s => }
    }
  }

  "Testing 7" should { 
    "test Scala code" should {
      "be fun " in { s => }
    }
  }

  "Testing 8 " should { 
    " test Scala code" should {
      "be fun" in { s => }
    }
  }

  "Testing 9  " should { 
    "test Scala code" should {
      "be fun" in { s => }
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestNamePathFreeSpec extends freespec.PathAnyFreeSpec with TestNameFixtureServices {
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun", 
     "Testing 2 Scala code should be fun", 
     "Testing 3 Scala code should be fun", 
     "Testing 4 Scala code should be fun", 
     "Testing 5 Scala code should be fun", 
     "Testing 6 Scala code should be fun", 
     "Testing 7 Scala code should be fun", 
     "Testing 8 Scala code should be fun", 
     "Testing 9 Scala code should be fun"
    )
  
  "Testing 1" - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 2 " - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 3" - { 
    " Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 4" - { 
    "Scala code " - {
      "should be fun" in {}
    }
  }

  "Testing 5" - { 
    "Scala code" - {
      " should be fun" in {}
    }
  }

  " Testing 6" - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 7" - { 
    "Scala code" - {
      "should be fun " in {}
    }
  }

  "Testing 8 " - { 
    " Scala code" - {
      "should be fun" in {}
    }
  }

  "Testing 9  " - { 
    "Scala code" - {
      "should be fun" in {}
    }
  }

  override def newInstance: freespec.PathAnyFreeSpecLike = new ExampleTestNamePathFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleTestNamePathFunSpec extends funspec.PathAnyFunSpec with TestNameFixtureServices {
  val expectedTestNames = 
    Set(
     "Testing 1 Scala code should be fun", 
     "Testing 2 Scala code should be fun", 
     "Testing 3 Scala code should be fun", 
     "Testing 4 Scala code should be fun", 
     "Testing 5 Scala code should be fun", 
     "Testing 6 Scala code should be fun", 
     "Testing 7 Scala code should be fun", 
     "Testing 8 Scala code should be fun", 
     "Testing 9 Scala code should be fun"
    )
  
  describe("Testing 1") { 
    describe("Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 2 ") { 
    describe("Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 3") { 
    describe(" Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 4") { 
    describe("Scala code ") {
      it("should be fun") {}
    }
  }

  describe("Testing 5") { 
    describe("Scala code") {
      it(" should be fun") {}
    }
  }

  describe(" Testing 6") { 
    describe("Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 7") { 
    describe("Scala code") {
      it("should be fun ") {}
    }
  }

  describe("Testing 8 ") { 
    describe(" Scala code") {
      it("should be fun") {}
    }
  }

  describe("Testing 9  ") { 
    describe("Scala code") {
      it("should be fun") {}
    }
  }

  override def newInstance: funspec.PathAnyFunSpecLike = new ExampleTestNamePathFunSpec
}
