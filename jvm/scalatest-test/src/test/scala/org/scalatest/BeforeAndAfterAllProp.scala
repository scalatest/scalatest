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
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class BeforeAndAfterAllProp extends AllSuiteProp {

  type FixtureServices = BeforeAndAfterAllPropFixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  def spec = new ExampleBeforeAndAfterAllPropSpec
  def junit3Suite = new ExampleBeforeAndAfterAllPropJUnit3Suite
  def junitSuite = new ExampleBeforeAndAfterAllPropJUnitSuite
  def testngSuite = new ExampleBeforeAndAfterAllPropTestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
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
        // SKIP-SCALATESTJS-START
        val execService = java.util.concurrent.Executors.newFixedThreadPool(2)
        val dist = new TestConcurrentDistributor(execService)
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY val dist = new TestConcurrentDistributor()
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
          // SKIP-SCALATESTJS-START
          execService.shutdown()
          // SKIP-SCALATESTJS-END
        }
      }
    }
  }
}

trait BeforeAndAfterAllPropFixtureServices {
  def included = this match {
    // SKIP-SCALATESTJS,NATIVE-START
    case _: JUnit3Suite => false
    case _: JUnitSuite => false
    case _: TestNGSuite => false
    // SKIP-SCALATESTJS,NATIVE-END
    case _: freespec.PathAnyFreeSpec => false
    case _: funspec.PathAnyFunSpec => false
    case _ => true
  }
  @volatile var beforeAllTime: Long = 0
  @volatile var afterAllTime: Long = 0
}

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropSpec extends RefSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
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
protected[scalatest] class ExampleBeforeAndAfterAllPropJUnit3Suite extends JUnit3Suite with BeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropJUnitSuite extends JUnitSuite with BeforeAndAfterAllPropFixtureServices { }

// Not supported as JUnitSuite cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropTestNGSuite extends TestNGSuite with BeforeAndAfterAllPropFixtureServices { }
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFunSuite extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  test("Test 1") { SleepHelper.sleep(10) }
  test("Test 2") { SleepHelper.sleep(10) }
  test("Test 3") { SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFunSuite extends funsuite.FixtureAnyFunSuite with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  test("Test 1") { s => SleepHelper.sleep(10) }
  test("Test 2") { s => SleepHelper.sleep(10) }
  test("Test 3") { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFunSpec extends AnyFunSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
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

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFunSpec extends funspec.FixtureAnyFunSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
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

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFeatureSpec extends AnyFeatureSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  Feature("Feature 1") {
    Scenario("Scenario 1") { SleepHelper.sleep(10) }
    Scenario("Scenario 2") { SleepHelper.sleep(10) }
    Scenario("Scenario 3") { SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  Feature("Feature 1") {
    Scenario("Scenario 1") { s => SleepHelper.sleep(10) }
    Scenario("Scenario 2") { s => SleepHelper.sleep(10) }
    Scenario("Scenario 3") { s => SleepHelper.sleep(10) }
  }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFlatSpec extends AnyFlatSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { SleepHelper.sleep(10) }
  it should "do thing 2" in { SleepHelper.sleep(10) }
  it should "do thing 3" in { SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should "do thing 1" in { s => SleepHelper.sleep(10) }
  it should "do thing 2" in { s => SleepHelper.sleep(10) }
  it should "do thing 3" in { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFreeSpec extends AnyFreeSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
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

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
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

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropPropSpec extends AnyPropSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
  property("Test 1") { SleepHelper.sleep(10) }
  property("Test 2") { SleepHelper.sleep(10) }
  property("Test 3") { SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropPropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixturePropSpec extends propspec.FixtureAnyPropSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
  property("Test 1") { s => SleepHelper.sleep(10) }
  property("Test 2") { s => SleepHelper.sleep(10) }
  property("Test 3") { s => SleepHelper.sleep(10) }
  
  override protected def beforeAll(): Unit = {
    beforeAllTime = Platform.currentTime
  }
  override protected def afterAll(): Unit = {
    afterAllTime = Platform.currentTime
  }

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixturePropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropWordSpec extends AnyWordSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with ParallelTestExecution {
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

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropWordSpec
}

@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropFixtureWordSpec extends wordspec.FixtureAnyWordSpec with BeforeAndAfterAll with BeforeAndAfterAllPropFixtureServices with StringFixture with ParallelTestExecution {
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

  override def newInstance: Suite with ParallelTestExecution = new ExampleBeforeAndAfterAllPropFixtureWordSpec
}

// Not supported as path.FreeSpec cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropPathFreeSpec extends freespec.PathAnyFreeSpec with BeforeAndAfterAllPropFixtureServices {
  override def newInstance: freespec.PathAnyFreeSpecLike = new ExampleBeforeAndAfterAllPropPathFreeSpec
}

// Not supported as path.FunSpec cannot use ParallelTestExecution
@DoNotDiscover
protected[scalatest] class ExampleBeforeAndAfterAllPropPathFunSpec extends funspec.PathAnyFunSpec with BeforeAndAfterAllPropFixtureServices {
  override def newInstance: funspec.PathAnyFunSpecLike = new ExampleBeforeAndAfterAllPropPathFunSpec
}
