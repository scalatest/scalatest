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

// SKIP-SCALATESTJS,NATIVE-START
import org.scalatestplus.junit.JUnit3Suite
import org.scalatestplus.junit.JUnitSuite
import org.scalatest.refspec.RefSpec
import org.scalatestplus.testng.TestNGSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG }
// SKIP-SCALATESTJS,NATIVE-END
import SharedHelpers._
import scala.compat.Platform
import org.scalatest.concurrent.SleepHelper

class DeprecatedBeforeAndAfterAllProp extends AllSuiteProp {

  type FixtureServices = DeprecatedBeforeAndAfterAllPropFixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  def spec = new DeprecatedExampleBeforeAndAfterAllPropSpec
  def junit3Suite = new DeprecatedExampleBeforeAndAfterAllPropJUnit3Suite
  def junitSuite = new DeprecatedExampleBeforeAndAfterAllPropJUnitSuite
  def testngSuite = new DeprecatedExampleBeforeAndAfterAllPropTestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
  def funSuite = new DeprecatedExampleBeforeAndAfterAllPropFunSuite
  def fixtureFunSuite = new DeprecatedExampleBeforeAndAfterAllPropFixtureFunSuite
  def funSpec = new DeprecatedExampleBeforeAndAfterAllPropFunSpec
  def fixtureFunSpec = new DeprecatedExampleBeforeAndAfterAllPropFixtureFunSpec
  def featureSpec = new DeprecatedExampleBeforeAndAfterAllPropFeatureSpec
  def fixtureFeatureSpec = new DeprecatedExampleBeforeAndAfterAllPropFixtureFeatureSpec
  def flatSpec = new DeprecatedExampleBeforeAndAfterAllPropFlatSpec
  def fixtureFlatSpec = new DeprecatedExampleBeforeAndAfterAllPropFixtureFlatSpec
  def freeSpec = new DeprecatedExampleBeforeAndAfterAllPropFreeSpec
  def fixtureFreeSpec = new DeprecatedExampleBeforeAndAfterAllPropFixtureFreeSpec
  def propSpec = new DeprecatedExampleBeforeAndAfterAllPropPropSpec
  def fixturePropSpec = new DeprecatedExampleBeforeAndAfterAllPropFixturePropSpec
  def wordSpec = new DeprecatedExampleBeforeAndAfterAllPropWordSpec
  def fixtureWordSpec = new DeprecatedExampleBeforeAndAfterAllPropFixtureWordSpec
  def pathFreeSpec = new DeprecatedExampleBeforeAndAfterAllPropPathFreeSpec
  def pathFunSpec = new DeprecatedExampleBeforeAndAfterAllPropPathFunSpec
  
  test("BeforeAndAfterAll should call beforeAll before any test starts, and call afterAll after all tests completed") {
    forAll(examples.filter(_.included)) { suite =>
      if (suite.included) {
        // SKIP-SCALATESTJS,NATIVE-START
        val execService = java.util.concurrent.Executors.newFixedThreadPool(2)
        val dist = new TestConcurrentDistributor(execService)
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS,NATIVE-ONLY val dist = new TestConcurrentDistributor()
        try {
          val rep = new EventRecordingReporter()
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
        finally {
          // SKIP-SCALATESTJS,NATIVE-START
          execService.shutdown()
          // SKIP-SCALATESTJS,NATIVE-END
        }
      }
    }
  }
}

trait DeprecatedBeforeAndAfterAllPropFixtureServices {
  def included = this match {
    // SKIP-SCALATESTJS,NATIVE-START
    case _: JUnit3Suite => false
    case _: JUnitSuite => false
    case _: TestNGSuite => false
    // SKIP-SCALATESTJS,NATIVE-END
    case _: path.FreeSpec => false
    case _: path.FunSpec => false
    case _ => true
  }
  @volatile var beforeAllTime: Long = 0
  @volatile var afterAllTime: Long = 0
}

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropSpec extends RefSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  object `Scope 1` {
    def `Test 1`: Unit = { Thread.sleep(10) }
    def `Test 2`: Unit = { Thread.sleep(10) }
    def `Test 3`: Unit = { Thread.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = System.currentTimeMillis
  }
  override protected def afterAll(): Unit = {
    afterAllTime = System.currentTimeMillis
  }
}

// Not supported as JUnit3Suite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropJUnit3Suite extends JUnit3Suite with DeprecatedBeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropJUnitSuite extends JUnitSuite with DeprecatedBeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropTestNGSuite extends TestNGSuite with DeprecatedBeforeAndAfterAllPropFixtureServices { }
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFunSuite extends FunSuite with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  test("Test 1") { SleepHelper.sleep(10) }
  test("Test 2") { SleepHelper.sleep(10) }
  test("Test 3") { SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFunSuite
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFixtureFunSuite extends fixture.FunSuite with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  test("Test 1") { s => SleepHelper.sleep(10) }
  test("Test 2") { s => SleepHelper.sleep(10) }
  test("Test 3") { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFixtureFunSuite
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFunSpec extends FunSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") { SleepHelper.sleep(10) }
    it("Test 2") { SleepHelper.sleep(10) }
    it("Test 3") { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFunSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFixtureFunSpec extends fixture.FunSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") { s => SleepHelper.sleep(10) }
    it("Test 2") { s => SleepHelper.sleep(10) }
    it("Test 3") { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFixtureFunSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFeatureSpec extends FeatureSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") { SleepHelper.sleep(10) }
    scenario("Scenario 2") { SleepHelper.sleep(10) }
    scenario("Scenario 3") { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFeatureSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFixtureFeatureSpec extends fixture.FeatureSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") { s => SleepHelper.sleep(10) }
    scenario("Scenario 2") { s => SleepHelper.sleep(10) }
    scenario("Scenario 3") { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFixtureFeatureSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFlatSpec extends FlatSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { SleepHelper.sleep(10) }
  it should "do thing 2" in { SleepHelper.sleep(10) }
  it should "do thing 3" in { SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFlatSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFixtureFlatSpec extends fixture.FlatSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { s => SleepHelper.sleep(10) }
  it should "do thing 2" in { s => SleepHelper.sleep(10) }
  it should "do thing 3" in { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFixtureFlatSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFreeSpec extends FreeSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in { SleepHelper.sleep(10) }
    "Test 2" in { SleepHelper.sleep(10) }
    "Test 3" in { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFreeSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFixtureFreeSpec extends fixture.FreeSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in { s => SleepHelper.sleep(10) }
    "Test 2" in { s => SleepHelper.sleep(10) }
    "Test 3" in { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFixtureFreeSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropPropSpec extends PropSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  property("Test 1") { SleepHelper.sleep(10) }
  property("Test 2") { SleepHelper.sleep(10) }
  property("Test 3") { SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropPropSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFixturePropSpec extends fixture.PropSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  property("Test 1") { s => SleepHelper.sleep(10) }
  property("Test 2") { s => SleepHelper.sleep(10) }
  property("Test 3") { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFixturePropSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropWordSpec extends WordSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in { SleepHelper.sleep(10) }
    "Test 2" in { SleepHelper.sleep(10) }
    "Test 3" in { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropWordSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropFixtureWordSpec extends fixture.WordSpec with BeforeAndAfterAll with DeprecatedBeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in { s => SleepHelper.sleep(10) }
    "Test 2" in { s => SleepHelper.sleep(10) }
    "Test 3" in { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleBeforeAndAfterAllPropFixtureWordSpec
}

// Not supported as path.FreeSpec cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropPathFreeSpec extends path.FreeSpec with DeprecatedBeforeAndAfterAllPropFixtureServices {
  override def newInstance: path.FreeSpecLike = new DeprecatedExampleBeforeAndAfterAllPropPathFreeSpec
}

// Not supported as path.FunSpec cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class DeprecatedExampleBeforeAndAfterAllPropPathFunSpec extends path.FunSpec with DeprecatedBeforeAndAfterAllPropFixtureServices {
  override def newInstance: path.FunSpecLike = new DeprecatedExampleBeforeAndAfterAllPropPathFunSpec
}
