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

import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.scalatest.testng.TestNGSuite
import org.junit.Test
import org.testng.annotations.{Test => TestNG }
import scala.collection.mutable.ListBuffer
import org.scalatest.tools.SuiteRunner
import SharedHelpers._
import org.scalatest.refspec.RefSpec

import scala.util.{Failure, Success, Try}

class StatusProp extends AllSuiteProp {
  
  type FixtureServices = StatusFixtureServices

  def spec = new ExampleStatusSpec
  def fixtureSpec = new ExampleStatusFixtureSpec
  def junit3Suite = new ExampleStatusJUnit3Suite
  def junitSuite = new ExampleStatusJUnitSuite
  def testngSuite = new ExampleStatusTestNGSuite
  def funSuite = new ExampleStatusFunSuite
  def fixtureFunSuite = new ExampleStatusFixtureFunSuite
  def funSpec = new ExampleStatusFunSpec
  def fixtureFunSpec = new ExampleStatusFixtureFunSpec
  def featureSpec = new ExampleStatusFeatureSpec
  def fixtureFeatureSpec = new ExampleStatusFixtureFeatureSpec
  def flatSpec = new ExampleStatusFlatSpec
  def fixtureFlatSpec = new ExampleStatusFixtureFlatSpec
  def freeSpec = new ExampleStatusFreeSpec
  def fixtureFreeSpec = new ExampleStatusFixtureFreeSpec
  def propSpec = new ExampleStatusPropSpec
  def fixturePropSpec = new ExampleStatusFixturePropSpec
  def wordSpec = new ExampleStatusWordSpec
  def fixtureWordSpec = new ExampleStatusFixtureWordSpec
  def pathFreeSpec = new ExampleStatusPathFreeSpec
  def pathFunSpec = new ExampleStatusPathFunSpec
  
  class DelayExecutionDistributor extends Distributor {
    val buf = ListBuffer.empty[SuiteRunner]
    
    def apply(suite: Suite, tracker: Tracker): Unit = {
      throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
    }

    def apply(suite: Suite, args: Args): Status = {
      val status = new ScalaTestStatefulStatus
      buf += new SuiteRunner(suite, args, status)
      status
    }
    
    def execute(): Int = {
      buf.count { suite => 
        try {
          suite.run()
          false
        }
        catch {
          case vme: VirtualMachineError => true
          case _: Throwable => false
        }
      }
    }
  }
  
  test("Status returned from run should return false in succeeds method when run completes abruptly") {
    forAll(examples) { suite => 
      if (suite.isSupported) {
        val distributor = new DelayExecutionDistributor
        val recordingReporter = new EventRecordingReporter
        val status = suite.run(None, Args(recordingReporter, distributor = Some(distributor)))
        assert(!status.isCompleted, "status.isCompleted should be false before distributor.execute(), but we got true")
        val vmeCount = distributor.execute()
        assert(vmeCount == 1, "should have 1 VirtualMachineError")
        assert(status.isCompleted, "status.isCompleted should be true after distributor.execute(), but we got false")
        assert(!status.succeeds, "status.succeeds should be false after distributor.execute(), but we got false")
      }
      else Succeeded
    }
  }
  
  test("Status returned from runTests should return false in succeeds method when runTests completes abruptly") {
    forAll(examples) { suite => 
      if (suite.isSupported) {
        val distributor = new DelayExecutionDistributor
        val recordingReporter = new EventRecordingReporter
        val status = suite.testRunTests(None, Args(recordingReporter, distributor = Some(distributor)))
        assert(!status.isCompleted, "status.isCompleted should be false before distributor.execute(), but we got true")
        val vmeCount = distributor.execute()
        assert(vmeCount == 1, "should have 1 VirtualMachineError")
        assert(status.isCompleted, "status.isCompleted should be true after distributor.execute(), but we got false")
        assert(!status.succeeds, "status.succeeds should be false after distributor.execute(), but we got false")
      }
      else Succeeded
    }
  }
  
  test("Status returned from runTest should return false in succeeds method when runTest completes abruptly") {
    forAll(examples) { suite => 
      if (suite.isSupported) {
        val distributor = new DelayExecutionDistributor
        val recordingReporter = new EventRecordingReporter
        val status = suite.testRunTest(Args(recordingReporter, distributor = Some(distributor), runTestInNewInstance = true))
        assert(!status.isCompleted, "status.isCompleted should be false before distributor.execute(), but we got true")
        val vmeCount = distributor.execute()
        assert(vmeCount == 1, "should have 1 VirtualMachineError")
        assert(status.isCompleted, "status.isCompleted should be true after distributor.execute(), but we got false")
        assert(!status.succeeds, "status.succeeds should be false after distributor.execute(), but we got false")
      }
      else Succeeded
    }
  }

  val testIllegalArgumentException = new IllegalArgumentException("test")

  def statelessStatus =
    Table(
      ("status", "expectedValue"),
      (SucceededStatus, Some(Success(true))),
      (FailedStatus, Some(Success(false))),
      (AbortedStatus(testIllegalArgumentException), Some(Failure(testIllegalArgumentException)))
    )

  import scala.language.reflectiveCalls

  type StatefulStatusType = {
    def whenCompleted(callback: Try[Boolean] => Unit)
    def setCompleted()
    def isCompleted: Boolean
    // SKIP-SCALATESTJS-START
    def succeeds(): Boolean
    // SKIP-SCALATESTJS-END
    def setFailed()
    // SKIP-SCALATESTJS-START
    def waitUntilCompleted()
    // SKIP-SCALATESTJS-END
  }

  def statefulStatus =
    Table[StatefulStatusType](
      "status",
      new ScalaTestStatefulStatus,
      new StatefulStatus
    )

  test("Stateless status should invoke a function registered with whenCompleted, passing a correct value") {
    forAll(statelessStatus) { case (status, expectedValue) =>
      @volatile var callbackInvoked = false
      @volatile var value: Option[Try[Boolean]] = None

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        value = Some(st)
      }

      // ensure it was executed
      assert(callbackInvoked)
      assert(value == expectedValue)
    }
  }

  test("Stateful status should invoke function registered with whenCompleted, passing over succeeded value if status.setCompleted() is called without status.setFailed()") {
    forAll(statefulStatus) { status =>
      @volatile var callbackInvoked = false
      @volatile var value: Option[Try[Boolean]] = None

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        value = Some(st)
      }

      status.setCompleted()

      // ensure it was executed
      assert(callbackInvoked)
      assert(value == Some(Success(true)))
    }
  }

  test("Stateful status should invoke function registered with whenCompleted, passing over failed value if status.setCompleted() is called after status.setFailed()") {
    forAll(statefulStatus) { status =>
      @volatile var callbackInvoked = false
      @volatile var value: Option[Try[Boolean]] = None

      // register callback
      status.whenCompleted { st =>
        callbackInvoked = true
        value = Some(st)
      }

      status.setFailed()
      status.setCompleted()

      // ensure it was executed
      assert(callbackInvoked)
      assert(value == Some(Success(false)))
    }
  }
}

trait StatusFixtureServices { suite: Suite =>
  val isSupported = true
  def testRunTests(testName: Option[String], args: Args): Status = suite.runTests(testName, args)
  val testNameToRun: String
  def testRunTest(args: Args): Status = 
    suite.runTest(testNameToRun, args)
}

@DoNotDiscover
class ExampleStatusSpec extends RefSpec with StatusFixtureServices with ParallelTestExecution {
  def `test 1`: Unit = {}
  def `test 2`: Unit = { throw new VirtualMachineError {} }
  def `test 3`: Unit = {}
  val testNameToRun = "test 2"
}

@DoNotDiscover
class ExampleStatusFixtureSpec extends fixture.Spec with StatusFixtureServices with StringFixture with ParallelTestExecution {
  def `test 1`(fixture: String): Unit = {}
  def `test 2`(fixture: String): Unit = { throw new VirtualMachineError {} }
  def `test 3`(fixture: String): Unit = {}
  val testNameToRun = "test 2"
}

@DoNotDiscover
class ExampleStatusJUnit3Suite extends JUnit3Suite with StatusFixtureServices {
  def testMethod1(): Unit = {}
  def testMethod2(): Unit = { throw new VirtualMachineError {} }
  def testMethod3(): Unit = {}
  override val isSupported = false
  val testNameToRun = "testMethod2"
}

@DoNotDiscover
class ExampleStatusJUnitSuite extends JUnitSuite with StatusFixtureServices {
  @Test
  def testMethod1(): Unit = {}
  @Test 
  def testMethod2(): Unit = { throw new VirtualMachineError {} }
  @Test 
  def testMethod3(): Unit = {}
  override val isSupported = false
  val testNameToRun = "testMethod2"
}



@DoNotDiscover
class ExampleStatusTestNGSuite extends TestNGSuite with StatusFixtureServices {
  @TestNG
  def testMethod1(): Unit = {}
  @TestNG
  def testMethod2(): Unit = { throw new VirtualMachineError {} }
  @TestNG
  def testMethod3(): Unit = {}
  override val isSupported = false
  val testNameToRun = "testMethod2"
}

@DoNotDiscover
class ExampleStatusFunSuite extends FunSuite with StatusFixtureServices with ParallelTestExecution {
  test("Test 1") {}
  test("Test 2") { throw new VirtualMachineError {} }
  test("Test 3") {}
  val testNameToRun = "Test 2"
}

@DoNotDiscover
class ExampleStatusFixtureFunSuite extends fixture.FunSuite with StatusFixtureServices with StringFixture with ParallelTestExecution {
  test("Test 1") {s =>}
  test("Test 2") {s => throw new VirtualMachineError {} }
  test("Test 3") {s =>}
  val testNameToRun = "Test 2"
}

@DoNotDiscover
class ExampleStatusFunSpec extends FunSpec with StatusFixtureServices with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") { throw new VirtualMachineError {} }
    it("Test 3") {}
  }
  val testNameToRun = "Scope 1 Test 2"
}

@DoNotDiscover
class ExampleStatusFixtureFunSpec extends fixture.FunSpec with StatusFixtureServices with StringFixture with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") {s =>}
    it("Test 2") {s => throw new VirtualMachineError {}}
    it("Test 3") {s =>}
  }
  val testNameToRun = "Scope 1 Test 2"
}

@DoNotDiscover
class ExampleStatusFeatureSpec extends FeatureSpec with StatusFixtureServices with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") {}
    scenario("Scenario 2") { throw new VirtualMachineError {} }
    scenario("Scenario 3") {}
  }
  val testNameToRun = "Feature: Feature 1 Scenario: Scenario 2"
}

@DoNotDiscover
class ExampleStatusFixtureFeatureSpec extends fixture.FeatureSpec with StatusFixtureServices with StringFixture with ParallelTestExecution {
  feature("Feature 1") {
    scenario("Scenario 1") {s =>}
    scenario("Scenario 2") {s => throw new VirtualMachineError {}}
    scenario("Scenario 3") {s =>}
  }
  val testNameToRun = "Feature: Feature 1 Scenario: Scenario 2"
}

@DoNotDiscover
class ExampleStatusFlatSpec extends FlatSpec with StatusFixtureServices with ParallelTestExecution {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in {throw new VirtualMachineError {}}
  it should "do thing 3" in {}
  val testNameToRun = "Scope 1 should do thing 2"
}

@DoNotDiscover
class ExampleStatusFixtureFlatSpec extends fixture.FlatSpec with StatusFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should "do thing 1" in {s =>}
  it should "do thing 2" in {s =>throw new VirtualMachineError {}}
  it should "do thing 3" in {s =>}
  val testNameToRun = "Scope 1 should do thing 2"
}

@DoNotDiscover
class ExampleStatusFreeSpec extends FreeSpec with StatusFixtureServices with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {throw new VirtualMachineError {}}
    "Test 3" in {}
  }
  val testNameToRun = "Scope 1 Test 2"
}

@DoNotDiscover
class ExampleStatusFixtureFreeSpec extends fixture.FreeSpec with StatusFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in {s =>}
    "Test 2" in {s =>throw new VirtualMachineError {}}
    "Test 3" in {s =>}
  }
  val testNameToRun = "Scope 1 Test 2"
}

@DoNotDiscover
class ExampleStatusPropSpec extends PropSpec with StatusFixtureServices with ParallelTestExecution {
  property("Test 1") {}
  property("Test 2") {throw new VirtualMachineError {}}
  property("Test 3") {}
  val testNameToRun = "Test 2"
}

@DoNotDiscover
class ExampleStatusFixturePropSpec extends fixture.PropSpec with StatusFixtureServices with StringFixture with ParallelTestExecution {
  property("Test 1") {s =>}
  property("Test 2") {s =>throw new VirtualMachineError {}}
  property("Test 3") {s =>}
  val testNameToRun = "Test 2"
}

@DoNotDiscover
class ExampleStatusWordSpec extends WordSpec with StatusFixtureServices with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {throw new VirtualMachineError {}}
    "Test 3" in {}
  }
  val testNameToRun = "Scope 1 should Test 2"
}

@DoNotDiscover
class ExampleStatusFixtureWordSpec extends fixture.WordSpec with StatusFixtureServices with StringFixture with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in {s =>}
    "Test 2" in {s =>throw new VirtualMachineError {}}
    "Test 3" in {s =>}
  }
  val testNameToRun = "Scope 1 should Test 2"
}

@DoNotDiscover
class ExampleStatusPathFreeSpec extends path.FreeSpec with StatusFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {throw new VirtualMachineError {}}
    "Test 3" in {}
  }
  override val isSupported = false
  val testNameToRun = "Scope 1 Test 2"
}

@DoNotDiscover
class ExampleStatusPathFunSpec extends path.FunSpec with StatusFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {throw new VirtualMachineError {}}
    it("Test 3") {}
  }
  override val isSupported = false
  val testNameToRun = "Scope 1 Test 2"
}
