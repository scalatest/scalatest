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
// SKIP-SCALATESTJS-START
import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG }
import org.scalatest.testng.TestNGSuite
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS-END
import SharedHelpers._
import org.scalactic.source.SourceInfo

class TestDataProp extends AllSuiteProp {

  type FixtureServices = TestDataFixtureServices

  // SKIP-SCALATESTJS-START
  def spec = new ExampleTestDataSpec
  def fixtureSpec = new ExampleTestDataFixtureSpec
  def junit3Suite = new ExampleTestDataJUnit3Suite
  def junitSuite = new ExampleTestDataJUnitSuite
  def testngSuite = new ExampleTestDataTestNGSuite
  // SKIP-SCALATESTJS-END
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

// SKIP-SCALATESTJS-START
@DoNotDiscover
class ExampleTestDataSpec extends RefSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.SlowAsMolasses")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  
  object `Scope 1` {
    object `Scope 2` {
      @SlowAsMolasses
      def `test 1` {}
    }
  }
}

@DoNotDiscover
class ExampleTestDataFixtureSpec extends fixture.Spec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.SlowAsMolasses")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  
  object `Scope 1` {
    object `Scope 2` {
      @SlowAsMolasses
      def `test 1`(fixture: String) { }
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
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = this.testDataFor("testMethod1", ConfigMap("key1" -> "value1"))
  
  def testMethod1() {}
}

@DoNotDiscover
class ExampleTestDataJUnitSuite extends JUnitSuite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "testMethod1"
    val scopes = Vector.empty
    val text = "testMethod1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.Ignore")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = this.testDataFor("testMethod1", ConfigMap("key1" -> "value1"))

  @org.junit.Ignore
  @Test def testMethod1() = ()
}



@DoNotDiscover
class ExampleTestDataTestNGSuite extends TestNGSuite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "testMethod1"
    val scopes = Vector.empty
    val text = "testMethod1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.Ignore")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = this.testDataFor("testMethod1", ConfigMap("key1" -> "value1"))
  @TestNG
  @Ignore
  def testMethod1() {}
}
// SKIP-SCALATESTJS-END

@DoNotDiscover
protected[scalatest] class ExampleTestDataFunSuite extends FunSuite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  test("Test 1", TestDataTag) {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFunSuite extends fixture.FunSuite with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  test("Test 1", TestDataTag) { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFunSpec extends FunSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
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
protected[scalatest] class ExampleTestDataFixtureFunSpec extends fixture.FunSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
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
protected[scalatest] class ExampleTestDataFeatureSpec extends FeatureSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Feature: Feature 1 Scenario: Scenario 1"
    val scopes = Vector("Feature: Feature 1")
    val text = "Scenario: Scenario 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  feature("Feature 1") {
    scenario("Scenario 1", TestDataTag) {}
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFeatureSpec extends fixture.FeatureSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Feature: Feature 1 Scenario: Scenario 1"
    val scopes = Vector("Feature: Feature 1")
    val text = "Scenario: Scenario 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  feature("Feature 1") {
    scenario("Scenario 1", TestDataTag) { s => }
  }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFlatSpec extends FlatSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should test 1"
    val scopes = Vector("Scope 1")
    val text = "should test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should "test 1" taggedAs(TestDataTag) in {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixtureFlatSpec extends fixture.FlatSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should test 1"
    val scopes = Vector("Scope 1")
    val text = "should test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should "test 1" taggedAs(TestDataTag) in { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFreeSpec extends FreeSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
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
protected[scalatest] class ExampleTestDataFixtureFreeSpec extends fixture.FreeSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
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
protected[scalatest] class ExampleTestDataPropSpec extends PropSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  property("Test 1", TestDataTag) {}
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataFixturePropSpec extends fixture.PropSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  property("Test 1", TestDataTag) { s => }
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataWordSpec extends WordSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should Scope 2 should test 1"
    val scopes = Vector("Scope 1 should", "Scope 2 should")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
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
protected[scalatest] class ExampleTestDataFixtureWordSpec extends fixture.WordSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should Scope 2 should test 1"
    val scopes = Vector("Scope 1 should", "Scope 2 should")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
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
protected[scalatest] class ExampleTestDataPathFreeSpec extends path.FreeSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  def testData: TestData = testDataFor("Scope 1 Scope 2 test 1", ConfigMap("key1" -> "value1"))
  "Scope 1" - {
    "Scope 2" - {
      "test 1" taggedAs(TestDataTag) in {}
    }
  }
  override def newInstance: path.FreeSpecLike = new ExampleTestDataPathFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleTestDataPathFunSpec extends path.FunSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    // SKIP-SCALATESTJS-START
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val tags = Set("org.scalatest.tags.TestDataTag")
    val sourceInfo = Some(SourceInfo.sourceInfo)
  }
  def testData: TestData = testDataFor("Scope 1 Scope 2 test 1", ConfigMap("key1" -> "value1"))
  describe("Scope 1") {
    describe("Scope 2") {
      it("test 1", TestDataTag) {}
    }
  }
  override def newInstance: path.FunSpecLike = new ExampleTestDataPathFunSpec
}
