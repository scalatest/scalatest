package org.scalatest
import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG }
import org.scalatest.testng.TestNGSuite

class StopOnFailureProp extends AllSuiteProp {

  type FixtureServices = StopOnFailureFixtureServices
  
  def suite = new ExampleStopOnFailureSuite
  def fixtureSuite = new ExampleStopOnFailureFixtureSuite
  def spec = new ExampleStopOnFailureSpec
  def fixtureSpec = new ExampleStopOnFailureFixtureSpec
  def junit3Suite = new ExampleStopOnFailureJUnit3Suite
  def junitSuite = new ExampleStopOnFailureJUnitSuite
  def testngSuite = new ExampleStopOnFailureTestNGSuite
  def funSuite = new ExampleStopOnFailureFunSuite
  def fixtureFunSuite = new ExampleStopOnFailureFixtureFunSuite
  def funSpec = new ExampleStopOnFailureFunSpec
  def fixtureFunSpec = new ExampleStopOnFailureFixtureFunSpec
  def featureSpec = new ExampleStopOnFailureFeatureSpec
  def fixtureFeatureSpec = new ExampleStopOnFailureFixtureFeatureSpec
  def flatSpec = new ExampleStopOnFailureFlatSpec
  def fixtureFlatSpec = new ExampleStopOnFailureFixtureFlatSpec
  def freeSpec = new ExampleStopOnFailureFreeSpec
  def fixtureFreeSpec = new ExampleStopOnFailureFixtureFreeSpec
  def propSpec = new ExampleStopOnFailurePropSpec
  def fixturePropSpec = new ExampleStopOnFailureFixturePropSpec
  def wordSpec = new ExampleStopOnFailureWordSpec
  def fixtureWordSpec = new ExampleStopOnFailureFixtureWordSpec
  def pathFreeSpec = new ExampleStopOnFailurePathFreeSpec
  def pathFunSpec = new ExampleStopOnFailurePathFunSpec
  
  test("StopOnFailure when mixed in should stop to execute tests in suite when encounter a failed test.") {
    forAll(examples.filter(_.supported)) { s =>
      val rep = new EventRecordingReporter
      val stopper = Stopper.default
      s.run(None, Args(reporter = rep, stopper = stopper))
      
      assert(stopper.stopRequested, "Expected stopRequested to be true, but it is not.")
      
      if (s.supportStopTest) {
        val testStartingList = rep.testStartingEventsReceived
        assert(testStartingList.length === 4)
      
        val succeededTestName = testStartingList(0).testName
        val pendingTestName = testStartingList(1).testName
        val canceledTestName = testStartingList(2).testName
        val failedTestName = testStartingList(3).testName
        val testSucceededList = rep.testSucceededEventsReceived
      
        assert(testSucceededList.length === 1)
        assert(testSucceededList(0).testName === succeededTestName)
      
        val testPendingList = rep.testPendingEventsReceived
        assert(testPendingList.length === 1)
        assert(testPendingList(0).testName === pendingTestName)
      
        val testCanceledList = rep.testCanceledEventsReceived
        assert(testCanceledList.length === 1)
        assert(testCanceledList(0).testName === canceledTestName)
      
        val testFailedList = rep.testFailedEventsReceived
        assert(testFailedList.length === 1)
        assert(testFailedList(0).testName === failedTestName)
      
        val testIgnoredList = rep.testIgnoredEventsReceived
        assert(testIgnoredList.length === 1)
      }
    }
  }
}

trait StopOnFailureFixtureServices {
  val supported: Boolean = true
  val supportStopTest: Boolean = true
}

@DoNotDiscover
class ExampleStopOnFailureSuite extends Suite with StopOnFailure with StopOnFailureFixtureServices {
  def testMethod1() {}
  def testMethod2() { pending }
  def testMethod3() { cancel }
  @Ignore def testMethod4() {}
  def testMethod5() { fail }
  def testMethod6() {}
}

@DoNotDiscover
class ExampleStopOnFailureFixtureSuite extends fixture.Suite with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  def testMethod1(fixture: String) {}
  def testMethod2(fixture: String) { pending }
  def testMethod3(fixture: String) { cancel }
  @Ignore def testMethod4(fixture: String) {}
  def testMethod5(fixture: String) { fail }
  def testMethod6(fixture: String) {}
}

@DoNotDiscover
class ExampleStopOnFailureSpec extends Spec with StopOnFailure with StopOnFailureFixtureServices {
  def `test 1` {}
  def `test 2` { pending }
  def `test 3` { cancel }
  @Ignore def `test 4` {}
  def `test 5` { fail }
  def `test 6` {}
}

@DoNotDiscover
class ExampleStopOnFailureFixtureSpec extends fixture.Spec with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) { pending }
  def `test 3`(fixture: String) { cancel }
  @Ignore def `test 4`(fixture: String) {}
  def `test 5`(fixture: String) { fail }
  def `test 6`(fixture: String) {}
}

@DoNotDiscover
class ExampleStopOnFailureJUnit3Suite extends JUnit3Suite with StopOnFailure with StopOnFailureFixtureServices {
  def testMethod1() {}
  def testMethod2() { pending }
  def testMethod3() { cancel }
  def testMethod4() { fail }
  def testMethod5() {}
  
  override val supportStopTest: Boolean = false
}

@DoNotDiscover
class ExampleStopOnFailureJUnitSuite extends JUnitSuite with StopOnFailure with StopOnFailureFixtureServices {
  @Test
  def testMethod1() {}
  @Test 
  def testMethod2() { pending }
  @Test 
  def testMethod3() { cancel }
  @Test
  @org.junit.Ignore
  def testMethod4() {}
  @Test
  def testMethod5() { fail }
  @Test
  def testMethod6() {}
  
  override val supportStopTest: Boolean = false
}

@DoNotDiscover
class ExampleStopOnFailureTestNGSuite extends TestNGSuite with StopOnFailure with StopOnFailureFixtureServices {
  @TestNG
  def testMethod1() {}
  @TestNG
  def testMethod2() { pending }
  @TestNG
  def testMethod3() { cancel }
  @TestNG
  @Ignore
  def testMethod4() {}
  @TestNG
  def testMethod5() { fail }
  @TestNG
  def testMethod6() {}
  
  override val supportStopTest: Boolean = false
}

@DoNotDiscover
class ExampleStopOnFailureFunSuite extends FunSuite with StopOnFailure with StopOnFailureFixtureServices {
  test("Test 1") {}
  test("Test 2") { pending }
  test("Test 3") { cancel }
  ignore("Test 4") {}
  test("Test 5") { fail }
  test("Test 6") {}
}

@DoNotDiscover
class ExampleStopOnFailureFixtureFunSuite extends fixture.FunSuite with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  test("Test 1") { s => }
  test("Test 2") { s => pending }
  test("Test 3") { s => cancel }
  ignore("Test 4") { s => }
  test("Test 5") { s => fail }
  test("Test 6") { s => }
}

@DoNotDiscover
class ExampleStopOnFailureFunSpec extends FunSpec with StopOnFailure with StopOnFailureFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") { pending }
    it("Test 3") { cancel }
    ignore("Test 4") {}
    it("Test 5") { fail }
    it("Test 6") {}
  }
}

@DoNotDiscover
class ExampleStopOnFailureFixtureFunSpec extends fixture.FunSpec with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  describe("Scope 1") {
    it("Test 1") { s => }
    it("Test 2") { s => pending }
    it("Test 3") { s => cancel }
    ignore("Test 4") { s => }
    it("Test 5") { s => fail }
    it("Test 6") { s => }
  }
}

@DoNotDiscover
class ExampleStopOnFailureFeatureSpec extends FeatureSpec with StopOnFailure with StopOnFailureFixtureServices {
  feature("Feature 1") {
    scenario("Scenario 1") {}
    scenario("Scenario 2") { pending }
    scenario("Scenario 3") { cancel }
    ignore("Scenario 4") {}
    scenario("Scenario 5") { fail }
    scenario("Scenario 6") {}
  }
}

@DoNotDiscover
class ExampleStopOnFailureFixtureFeatureSpec extends fixture.FeatureSpec with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  feature("Feature 1") {
    scenario("Scenario 1") { s => }
    scenario("Scenario 2") { s => pending }
    scenario("Scenario 3") { s => cancel }
    ignore("Scenario 4") { s => }
    scenario("Scenario 5") { s => fail }
    scenario("Scenario 6") { s => }
  }
}

@DoNotDiscover
class ExampleStopOnFailureFlatSpec extends FlatSpec with StopOnFailure with StopOnFailureFixtureServices {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in { pending }
  it should "do thing 3" in { cancel }
  it should "do thing 4" ignore {}
  it should "do thing 5" in { fail }
  it should "do thing 6" in {}
}

@DoNotDiscover
class ExampleStopOnFailureFixtureFlatSpec extends fixture.FlatSpec with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  "Scope 1" should "do thing 1" in { s => }
  it should "do thing 2" in { s => pending }
  it should "do thing 3" in { s => cancel }
  it should "do thing 4" ignore { s => }
  it should "do thing 5" in { s => fail }
  it should "do thing 6" in { s => }
}

@DoNotDiscover
class ExampleStopOnFailureFreeSpec extends FreeSpec with StopOnFailure with StopOnFailureFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in { pending }
    "Test 3" in { cancel }
    "Test 4" ignore {}
    "Test 5" in { fail }
    "Test 6" in {}
  }
}

@DoNotDiscover
class ExampleStopOnFailureFixtureFreeSpec extends fixture.FreeSpec with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  "Scope 1" - {
    "Test 1" in { s => }
    "Test 2" in { s => pending }
    "Test 3" in { s => cancel }
    "Test 4" ignore { s => }
    "Test 5" in { s => fail }
    "Test 6" in { s => }
  }
}

@DoNotDiscover
class ExampleStopOnFailurePropSpec extends PropSpec with StopOnFailure with StopOnFailureFixtureServices {
  property("Test 1") {}
  property("Test 2") { pending }
  property("Test 3") { cancel }
  ignore("Test 4") {}
  property("Test 5") { fail }
  property("Test 6") {}
}

@DoNotDiscover
class ExampleStopOnFailureFixturePropSpec extends fixture.PropSpec with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  property("Test 1") { s => }
  property("Test 2") { s => pending }
  property("Test 3") { s => cancel }
  ignore("Test 4") { s => }
  property("Test 5") { s => fail }
  property("Test 6") { s => }
}

@DoNotDiscover
class ExampleStopOnFailureWordSpec extends WordSpec with StopOnFailure with StopOnFailureFixtureServices {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in { pending }
    "Test 3" in { cancel }
    "Test 4" ignore {}
    "Test 5" in { fail }
    "Test 6" in {}
  }
}

@DoNotDiscover
class ExampleStopOnFailureFixtureWordSpec extends fixture.WordSpec with StopOnFailure with StopOnFailureFixtureServices with StringFixture {
  "Scope 1" should {
    "Test 1" in { s => }
    "Test 2" in { s => pending }
    "Test 3" in { s => cancel }
    "Test 4" ignore { s => }
    "Test 5" in { s => fail }
    "Test 6" in { s => }
  }
}

// Not supported as run is final.
@DoNotDiscover
class ExampleStopOnFailurePathFreeSpec extends path.FreeSpec with StopOnFailureFixtureServices {
  override val supported = false
}

// Not supported as run is final.
@DoNotDiscover
class ExampleStopOnFailurePathFunSpec extends path.FunSpec with StopOnFailureFixtureServices {
  override val supported = false
}