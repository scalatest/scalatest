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
// SKIP-SCALATESTJS-END
import SharedHelpers._
import scala.compat.Platform
import org.scalatest.concurrent.SleepHelper

class BeforeAndAfterAllProp extends AllSuiteProp {

  type FixtureServices = BeforeAndAfterAllPropFixtureServices

  // SKIP-SCALATESTJS-START
  def spec = new ExampleBeforeAndAfterAllPropSpec
  def fixtureSpec = new ExampleBeforeAndAfterAllPropFixtureSpec
  def junit3Suite = new ExampleBeforeAndAfterAllPropJUnit3Suite
  def junitSuite = new ExampleBeforeAndAfterAllPropJUnitSuite
  def testngSuite = new ExampleBeforeAndAfterAllPropTestNGSuite
  // SKIP-SCALATESTJS-END
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
    // SKIP-SCALATESTJS-START
    case _: JUnit3Suite => false
    case _: JUnitSuite => false
    case _: TestNGSuite => false
    // SKIP-SCALATESTJS-END
    case _: path.FreeSpec => false
    case _: path.FunSpec => false
    case _ => true
  }
  @volatile var beforeAllTime: Long = 0
  @volatile var afterAllTime: Long = 0
}

// SKIP-SCALATESTJS-START
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropSpec extends Spec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  object `Scope 1` {
    def `Test 1` { Thread.sleep(10) }
    def `Test 2` { Thread.sleep(10) }
    def `Test 3` { Thread.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll() {
    afterAllTime = System.currentTimeMillis
  }
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureSpec extends fixture.Spec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  object `Scope 1` {
    def `Test 1`(fixture: String) { Thread.sleep(10) }
    def `Test 2`(fixture: String) { Thread.sleep(10) }
    def `Test 3`(fixture: String) { Thread.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll() {
    afterAllTime = System.currentTimeMillis
  }
}

// Not supported as JUnit3Suite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropJUnit3Suite extends JUnit3Suite with BeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropJUnitSuite extends JUnitSuite with BeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropTestNGSuite extends TestNGSuite with BeforeAndAfterAllPropFixtureServices { }
// SKIP-SCALATESTJS-END

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFunSuite extends FunSuite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  test("Test 1") { SleepHelper.sleep(10) }
  test("Test 2") { SleepHelper.sleep(10) }
  test("Test 3") { SleepHelper.sleep(10) }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFunSuite extends fixture.FunSuite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  test("Test 1") { s => SleepHelper.sleep(10) }
  test("Test 2") { s => SleepHelper.sleep(10) }
  test("Test 3") { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFunSpec extends FunSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") { SleepHelper.sleep(10) }
    it("Test 2") { SleepHelper.sleep(10) }
    it("Test 3") { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFunSpec extends fixture.FunSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") { s => SleepHelper.sleep(10) }
    it("Test 2") { s => SleepHelper.sleep(10) }
    it("Test 3") { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFeatureSpec extends FeatureSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") { SleepHelper.sleep(10) }
    scenario("Scenario 2") { SleepHelper.sleep(10) }
    scenario("Scenario 3") { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFeatureSpec extends fixture.FeatureSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") { s => SleepHelper.sleep(10) }
    scenario("Scenario 2") { s => SleepHelper.sleep(10) }
    scenario("Scenario 3") { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFlatSpec extends FlatSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { SleepHelper.sleep(10) }
  it should "do thing 2" in { SleepHelper.sleep(10) }
  it should "do thing 3" in { SleepHelper.sleep(10) }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFlatSpec extends fixture.FlatSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { s => SleepHelper.sleep(10) }
  it should "do thing 2" in { s => SleepHelper.sleep(10) }
  it should "do thing 3" in { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFreeSpec extends FreeSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in { SleepHelper.sleep(10) }
    "Test 2" in { SleepHelper.sleep(10) }
    "Test 3" in { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFreeSpec extends fixture.FreeSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in { s => SleepHelper.sleep(10) }
    "Test 2" in { s => SleepHelper.sleep(10) }
    "Test 3" in { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropPropSpec extends PropSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  property("Test 1") { SleepHelper.sleep(10) }
  property("Test 2") { SleepHelper.sleep(10) }
  property("Test 3") { SleepHelper.sleep(10) }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropPropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixturePropSpec extends fixture.PropSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  property("Test 1") { s => SleepHelper.sleep(10) }
  property("Test 2") { s => SleepHelper.sleep(10) }
  property("Test 3") { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixturePropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropWordSpec extends WordSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in { SleepHelper.sleep(10) }
    "Test 2" in { SleepHelper.sleep(10) }
    "Test 3" in { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropWordSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureWordSpec extends fixture.WordSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in { s => SleepHelper.sleep(10) }
    "Test 2" in { s => SleepHelper.sleep(10) }
    "Test 3" in { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll() {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll() {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureWordSpec
}

// Not supported as path.FreeSpec cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropPathFreeSpec extends path.FreeSpec with BeforeAndAfterAllPropFixtureServices {
  override def newInstance: path.FreeSpecLike = new ExampleBeforeAndAfterAllPropPathFreeSpec
}

// Not supported as path.FunSpec cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropPathFunSpec extends path.FunSpec with BeforeAndAfterAllPropFixtureServices {
  override def newInstance: path.FunSpecLike = new ExampleBeforeAndAfterAllPropPathFunSpec
}
