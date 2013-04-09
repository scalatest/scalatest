package org.scalatest

import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG }
import org.scalatest.testng.TestNGSuite

class BeforeAndAfterAllProp extends AllSuiteProp {

  type FixtureServices = BeforeAndAfterAllPropFixtureServices
  
  def suite = new ExampleBeforeAndAfterAllPropSuite
  def fixtureSuite = new ExampleBeforeAndAfterAllPropFixtureSuite
  def spec = new ExampleBeforeAndAfterAllPropSpec
  def fixtureSpec = new ExampleBeforeAndAfterAllPropFixtureSpec
  def junit3Suite = new ExampleBeforeAndAfterAllPropJUnit3Suite
  def junitSuite = new ExampleBeforeAndAfterAllPropJUnitSuite
  def testngSuite = new ExampleBeforeAndAfterAllPropTestNGSuite
  def funSuite = new ExampleBeforeAndAfterAllPropFunSuite
  def fixtureFunSuite = new ExampleBeforeAndAfterAllPropFixtureFunSuite
  def funSpec = new ExampleBeforeAndAfterAllPropFunSpec
  def fixtureFunSpec = new ExampleBeforeAndAfterAllPropFixtureFunSpec
  def featureSpec = new ExampleBeforeAndAfterAllPropFeatureSpec
  def fixtureFeatureSpec = new ExampleBeforeAndAfterAllPropFixtureFeatureSpec
  def flatSpec = new ExampleBeforeAndAfterAllPropFlatSpec
  def fixtureFlatSpec = new ExampleBeforeAndAfterAllPropFixtureFlatSpec
  def freeSpec = new ExampleBeforeAndAfterAllPropFreeSpec
  def fixtureFreeSpec = new ExampleBeforeAndAfterAllPropFixtureFreeSpec
  def propSpec = new ExampleBeforeAndAfterAllPropPropSpec
  def fixturePropSpec = new ExampleBeforeAndAfterAllPropFixturePropSpec
  def wordSpec = new ExampleBeforeAndAfterAllPropWordSpec
  def fixtureWordSpec = new ExampleBeforeAndAfterAllPropFixtureWordSpec
  def pathFreeSpec = new ExampleBeforeAndAfterAllPropPathFreeSpec
  def pathFunSpec = new ExampleBeforeAndAfterAllPropPathFunSpec
  
  test("BeforeAndAfterAll should call beforeAll before any test starts, and call afterAll after all tests completed") {
    forAll(examples.filter(_.included)) { suite =>
      if (suite.included) {
        val rep = new EventRecordingReporter()
        val dist = new TestConcurrentDistributor(2)
        suite.run(None, Args(reporter = rep, distributor = Some(dist)))
        dist.waitUntilDone()
      
        // should call beforeAll before any test starts
        val beforeAllTime = suite.beforeAllTime
        val testStartingEvents = rep.testStartingEventsReceived
        testStartingEvents should have size 3
        testStartingEvents.foreach { testStarting =>
          beforeAllTime should be <= testStarting.timeStamp
        }
      
        // should call afterAll after all tests completed
        val afterAllTime = suite.afterAllTime
        val testSucceededEvents = rep.testSucceededEventsReceived
        testSucceededEvents should have size 3
        testSucceededEvents.foreach { testSucceeded =>
          afterAllTime should be >= testSucceeded.timeStamp
        }
      }
    }
  }
}

trait BeforeAndAfterAllPropFixtureServices {
  def included = this match {
    case _: JUnit3Suite => false
    case _: JUnitSuite => false
    case _: TestNGSuite => false
    case _: path.FreeSpec => false
    case _: path.FunSpec => false
    case _ => true
  }
  @volatile var beforeAllTime: Long = 0
  @volatile var afterAllTime: Long = 0
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropSuite extends Suite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  def testMethod1() { Thread.sleep(10) }
  def testMethod2() { Thread.sleep(10) }
  def testMethod3() { Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureSuite extends fixture.Suite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  def testMethod1() { Thread.sleep(10) }
  def testMethod2() { Thread.sleep(10) }
  def testMethod3() { Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropSpec extends Spec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  object `Scope 1` {
    def `Test 1` { Thread.sleep(10) }
    def `Test 2` { Thread.sleep(10) }
    def `Test 3` { Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureSpec extends fixture.Spec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  object `Scope 1` {
    def `Test 1`(fixture: String) { Thread.sleep(10) }
    def `Test 2`(fixture: String) { Thread.sleep(10) }
    def `Test 3`(fixture: String) { Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

// Not supported as JUnit3Suite cannot use ParallelTestExecution
@DoNotDiscover
class ExampleBeforeAndAfterAllPropJUnit3Suite extends JUnit3Suite with BeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
class ExampleBeforeAndAfterAllPropJUnitSuite extends JUnitSuite with BeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
class ExampleBeforeAndAfterAllPropTestNGSuite extends TestNGSuite with BeforeAndAfterAllPropFixtureServices { }

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFunSuite extends FunSuite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  test("Test 1") { Thread.sleep(10) }
  test("Test 2") { Thread.sleep(10) }
  test("Test 3") { Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureFunSuite extends fixture.FunSuite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  test("Test 1") { s => Thread.sleep(10) }
  test("Test 2") { s => Thread.sleep(10) }
  test("Test 3") { s => Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFunSpec extends FunSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") { Thread.sleep(10) }
    it("Test 2") { Thread.sleep(10) }
    it("Test 3") { Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureFunSpec extends fixture.FunSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") { s => Thread.sleep(10) }
    it("Test 2") { s => Thread.sleep(10) }
    it("Test 3") { s => Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFeatureSpec extends FeatureSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") { Thread.sleep(10) }
    scenario("Scenario 2") { Thread.sleep(10) }
    scenario("Scenario 3") { Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureFeatureSpec extends fixture.FeatureSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") { s => Thread.sleep(10) }
    scenario("Scenario 2") { s => Thread.sleep(10) }
    scenario("Scenario 3") { s => Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFlatSpec extends FlatSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { Thread.sleep(10) }
  it should "do thing 2" in { Thread.sleep(10) }
  it should "do thing 3" in { Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureFlatSpec extends fixture.FlatSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { s => Thread.sleep(10) }
  it should "do thing 2" in { s => Thread.sleep(10) }
  it should "do thing 3" in { s => Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFreeSpec extends FreeSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in { Thread.sleep(10) }
    "Test 2" in { Thread.sleep(10) }
    "Test 3" in { Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureFreeSpec extends fixture.FreeSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in { s => Thread.sleep(10) }
    "Test 2" in { s => Thread.sleep(10) }
    "Test 3" in { s => Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropPropSpec extends PropSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  property("Test 1") { Thread.sleep(10) }
  property("Test 2") { Thread.sleep(10) }
  property("Test 3") { Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixturePropSpec extends fixture.PropSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  property("Test 1") { s => Thread.sleep(10) }
  property("Test 2") { s => Thread.sleep(10) }
  property("Test 3") { s => Thread.sleep(10) }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropWordSpec extends WordSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in { Thread.sleep(10) }
    "Test 2" in { Thread.sleep(10) }
    "Test 3" in { Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
class ExampleBeforeAndAfterAllPropFixtureWordSpec extends fixture.WordSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in { s => Thread.sleep(10) }
    "Test 2" in { s => Thread.sleep(10) }
    "Test 3" in { s => Thread.sleep(10) }
  }
  
  override protected def beforeAll(configMap: ConfigMap) {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(configMap: ConfigMap) {
    afterAllTime = System.currentTimeMillis
  }
}

// Not supported as path.FreeSpec cannot use ParallelTestExecution
@DoNotDiscover
class ExampleBeforeAndAfterAllPropPathFreeSpec extends path.FreeSpec with BeforeAndAfterAllPropFixtureServices { }

// Not supported as path.FunSpec cannot use ParallelTestExecution
@DoNotDiscover
class ExampleBeforeAndAfterAllPropPathFunSpec extends path.FunSpec with BeforeAndAfterAllPropFixtureServices { }
