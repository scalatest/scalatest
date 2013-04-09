package org.scalatest
import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG }
import org.scalatest.testng.TestNGSuite

class TestDataProp extends AllSuiteProp {

  type FixtureServices = TestDataFixtureServices
  
  def suite = new ExampleTestDataSuite
  def fixtureSuite = new ExampleTestDataFixtureSuite
  def spec = new ExampleTestDataSpec
  def fixtureSpec = new ExampleTestDataFixtureSpec
  def junit3Suite = new ExampleTestDataJUnit3Suite
  def junitSuite = new ExampleTestDataJUnitSuite
  def testngSuite = new ExampleTestDataTestNGSuite
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

@DoNotDiscover
class ExampleTestDataSuite extends Suite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "testMethod1"
    val scopes = Vector.empty
    val text = "testMethod1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.SlowAsMolasses")
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  @SlowAsMolasses
  def testMethod1() {}
}

@DoNotDiscover
class ExampleTestDataFixtureSuite extends fixture.Suite with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "testMethod1(FixtureParam)"
    val scopes = Vector.empty
    val text = "testMethod1(FixtureParam)"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.SlowAsMolasses")
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  @SlowAsMolasses
  def testMethod1(fixture: String) {}
}

@DoNotDiscover
class ExampleTestDataSpec extends Spec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.SlowAsMolasses")
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
  }
  var testData: TestData = this.testDataFor("testMethod1", ConfigMap("key1" -> "value1"))
  @TestNG
  @Ignore
  def testMethod1() {}
}

@DoNotDiscover
class ExampleTestDataFunSuite extends FunSuite with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  test("Test 1", TestDataTag) {}
}

@DoNotDiscover
class ExampleTestDataFixtureFunSuite extends fixture.FunSuite with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  test("Test 1", TestDataTag) { s => }
}

@DoNotDiscover
class ExampleTestDataFunSpec extends FunSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataFixtureFunSpec extends fixture.FunSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataFeatureSpec extends FeatureSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Feature: Feature 1 Scenario: Scenario 1"
    val scopes = Vector("Feature: Feature 1")
    val text = "Scenario: Scenario 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataFixtureFeatureSpec extends fixture.FeatureSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Feature: Feature 1 Scenario: Scenario 1"
    val scopes = Vector("Feature: Feature 1")
    val text = "Scenario: Scenario 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataFlatSpec extends FlatSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should test 1"
    val scopes = Vector("Scope 1")
    val text = "should test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should "test 1" taggedAs(TestDataTag) in {}
}

@DoNotDiscover
class ExampleTestDataFixtureFlatSpec extends fixture.FlatSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should test 1"
    val scopes = Vector("Scope 1")
    val text = "should test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  "Scope 1" should "test 1" taggedAs(TestDataTag) in { s => }
}

@DoNotDiscover
class ExampleTestDataFreeSpec extends FreeSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataFixtureFreeSpec extends fixture.FreeSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataPropSpec extends PropSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  var testData: TestData = null
  override def withFixture(test: NoArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  property("Test 1", TestDataTag) {}
}

@DoNotDiscover
class ExampleTestDataFixturePropSpec extends fixture.PropSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Test 1"
    val scopes = Vector.empty
    val text = "Test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  var testData: TestData = null
  override def withFixture(test: OneArgTest): Outcome = {
    testData = test
    super.withFixture(test)
  }
  property("Test 1", TestDataTag) { s => }
}

@DoNotDiscover
class ExampleTestDataWordSpec extends WordSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should Scope 2 should test 1"
    val scopes = Vector("Scope 1 should", "Scope 2 should")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataFixtureWordSpec extends fixture.WordSpec with TestDataFixtureServices with StringFixture {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 should Scope 2 should test 1"
    val scopes = Vector("Scope 1 should", "Scope 2 should")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
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
class ExampleTestDataPathFreeSpec extends path.FreeSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  def testData: TestData = testDataFor("Scope 1 Scope 2 test 1", ConfigMap("key1" -> "value1"))
  "Scope 1" - {
    "Scope 2" - {
      "test 1" taggedAs(TestDataTag) in {}
    }
  }
}

@DoNotDiscover
class ExampleTestDataPathFunSpec extends path.FunSpec with TestDataFixtureServices {
  val expectedTestData = new TestData {
    val configMap = ConfigMap("key1" -> "value1") 
    val name = "Scope 1 Scope 2 test 1"
    val scopes = Vector("Scope 1", "Scope 2")
    val text = "test 1"
    val tags = Set("org.scalatest.DoNotDiscover", "org.scalatest.tags.TestDataTag")
  }
  def testData: TestData = testDataFor("Scope 1 Scope 2 test 1", ConfigMap("key1" -> "value1"))
  describe("Scope 1") {
    describe("Scope 2") {
      it("test 1", TestDataTag) {}
    }
  }
}
