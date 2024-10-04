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
package org.scalatest.deprecated
import org.scalatest._
// SKIP-SCALATESTJS,NATIVE-START
import org.junit.Test
import org.scalatestplus.junit.JUnit3Suite
import org.scalatestplus.junit.JUnitSuite
import org.scalatest.refspec.RefSpec
import org.scalatestplus.testng.TestNGSuite
import org.testng.annotations.{Test => TestNG }
// SKIP-SCALATESTJS,NATIVE-END
import SharedHelpers._
import org.scalactic._
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class DeprecatedTestDataProp extends AllSuiteProp {

  type FixtureServices = TestDataFixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  def spec = new ExampleTestDataSpec
  def junit3Suite = new ExampleTestDataJUnit3Suite
  def junitSuite = new ExampleTestDataJUnitSuite
  def testngSuite = new ExampleTestDataTestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
  def funSuite = new ExampleTestDataFunSuite
  def fixtureFunSuite = new ExampleTestDataFixtureFunSuite
  def funSpec = new ExampleTestDataFunSpec
  def fixtureFunSpec = new ExampleTestDataFixtureFunSpec
  def featureSpec = new ExampleTestDataFeatureSpec
  def fixtureFeatureSpec = new ExampleTestDataFixtureFeatureSpec
  def flatSpec = new ExampleTestDataFlatSpec
  def fixtureFlatSpec = new ExampleTestDataFixtureFlatSpec
  def freeSpec = new ExampleTestDataFreeSpec
  def fixtureFreeSpec = new ExampleTestDataFixtureFreeSpec
  def propSpec = new ExampleTestDataPropSpec
  def fixturePropSpec = new ExampleTestDataFixturePropSpec
  def wordSpec = new ExampleTestDataWordSpec
  def fixtureWordSpec = new ExampleTestDataFixtureWordSpec
  def pathFreeSpec = new ExampleTestDataPathFreeSpec
  def pathFunSpec = new ExampleTestDataPathFunSpec
  
  test("TestData should include correct configMap, name, scopes and text") {
    forAll(examples.filter(_.included)) { s =>
      s.run(None, Args(reporter = SilentReporter, configMap = ConfigMap("key1" -> "value1")))
      assert(s.testData != null, s.getClass.getName + "'s testData is null.")
      assert(s.expectedTestData.configMap === s.testData.configMap)
      assert(s.expectedTestData.name === s.testData.name)
      assert(s.expectedTestData.scopes === s.testData.scopes)
      assert(s.expectedTestData.text === s.testData.text)
      assert(s.expectedTestData.tags === s.testData.tags)
    }
  }
}

trait TestDataFixtureServices {
  def included = this match {
    case _ => true
  }
  def testData: TestData
  val expectedTestData: TestData
}

object TestDataTag extends Tag("org.scalatest.tags.TestDataTag")

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
class ExampleTestDataSpec extends RefSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.SlowAsMolasses")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  
  object `Scope 1` {
    object `Scope 2` {
      @SlowAsMolasses
      def `test 1`: Unit = {}
    }
  }
}

@DoNotDiscover
class ExampleTestDataJUnit3Suite extends JUnit3Suite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "testMethod1"
    val scopes = Vector.empty
    val text = "testMethod1"
    val tags = Set.empty[String]
    val pos = Some(source.Position.here)
  }
  var testData: TestData = this.testDataFor("testMethod1", ConfigMap("key1" -> "value1"))
  
  def testMethod1(): Unit = {}
}

@DoNotDiscover
class ExampleTestDataJUnitSuite extends JUnitSuite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "testMethod1"
    val scopes = Vector.empty
    val text = "testMethod1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.Ignore")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = this.testDataFor("testMethod1", ConfigMap("key1" -> "value1"))

  @org.junit.Ignore
  @Test def testMethod1(): Unit = ()
}



@DoNotDiscover
class ExampleTestDataTestNGSuite extends TestNGSuite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "testMethod1"
    val scopes = Vector.empty
    val text = "testMethod1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.Ignore")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = this.testDataFor("testMethod1", ConfigMap("key1" -> "value1"))
  @TestNG
  @Ignore
  def testMethod1(): Unit = {}
}
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[scalatest] class ExampleTestDataFunSuite extends AnyFunSuite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  test("Test 1", TestDataTag) {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFunSuite extends funsuite.FixtureAnyFunSuite with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  test("Test 1", TestDataTag) { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFunSpec extends AnyFunSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  describe("Scope 1") {
    describe("Scope 2") {
      it("test 1", TestDataTag) {}
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFunSpec extends funspec.FixtureAnyFunSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  describe("Scope 1") {
    describe("Scope 2") {
      it("test 1", TestDataTag) { s => }
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFeatureSpec extends AnyFeatureSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Feature: Feature 1 Scenario: Scenario 1"
    val scopes = Vector("Feature: Feature 1")
    val text = "Scenario: Scenario 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  Feature("Feature 1") {
    Scenario("Scenario 1", TestDataTag) {}
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Feature: Feature 1 Scenario: Scenario 1"
    val scopes = Vector("Feature: Feature 1")
    val text = "Scenario: Scenario 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  Feature("Feature 1") {
    Scenario("Scenario 1", TestDataTag) { s => }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFlatSpec extends AnyFlatSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should test 1"
    val scopes = Vector("Scope 1")
    val text = "should test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should "test 1" taggedAs(TestDataTag) in {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should test 1"
    val scopes = Vector("Scope 1")
    val text = "should test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should "test 1" taggedAs(TestDataTag) in { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFreeSpec extends AnyFreeSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" - {
    "Scope 2" - {
      "test 1" taggedAs(TestDataTag) in {}
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" - {
    "Scope 2" - {
      "test 1" taggedAs(TestDataTag) in { s => }
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataPropSpec extends AnyPropSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  property("Test 1", TestDataTag) {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixturePropSpec extends propspec.FixtureAnyPropSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  property("Test 1", TestDataTag) { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataWordSpec extends AnyWordSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should Scope 2 should test 1"
    val scopes = Vector("Scope 1 should", "Scope 2 should")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should {
    "Scope 2" should {
      "test 1" taggedAs(TestDataTag) in {}
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureWordSpec extends wordspec.FixtureAnyWordSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should Scope 2 should test 1"
    val scopes = Vector("Scope 1 should", "Scope 2 should")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should {
    "Scope 2" should {
      "test 1" taggedAs(TestDataTag) in { s => }
    }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataPathFreeSpec extends freespec.PathAnyFreeSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  def testData: TestData = testDataFor("Scope 1 Scope 2 test 1", ConfigMap("key1" -> "value1"))
  "Scope 1" - {
    "Scope 2" - {
      "test 1" taggedAs(TestDataTag) in {}
    }
  }
  override def newInstance: freespec.PathAnyFreeSpecLike = new ExampleTestDataPathFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataPathFunSpec extends funspec.PathAnyFunSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS,NATIVE-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val pos = Some(source.Position.here)
  }
  def testData: TestData = testDataFor("Scope 1 Scope 2 test 1", ConfigMap("key1" -> "value1"))
  describe("Scope 1") {
    describe("Scope 2") {
      it("test 1", TestDataTag) {}
    }
  }
  override def newInstance: funspec.PathAnyFunSpecLike = new ExampleTestDataPathFunSpec
}
