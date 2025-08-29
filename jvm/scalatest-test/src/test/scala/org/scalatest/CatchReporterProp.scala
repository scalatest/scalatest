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

import org.scalatest.events._
import org.scalatestplus.junit._
import org.scalatest.time._
import SharedHelpers._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.junit.Test
import org.scalatestplus.testng.TestNGSuite
import org.scalatest.tools.SuiteSortingReporter
import org.scalatest.tools.TestSortingReporter
import org.testng.annotations.{Test => TestNG }
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.refspec.RefSpec
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec
// SKIP-SCALATESTJS,NATIVE-END

class CatchReporterProp extends AllSuiteProp {

  type FixtureServices = CatchReporterFixtureServices
  
  def spec = new ExampleCatchReporterSpec
  def junit3Suite = new ExampleCatchReporterJUnit3Suite
  def junitSuite = new ExampleCatchReporterJUnitSuite
  def testngSuite = new ExampleCatchReporterTestNGSuite
  def funSuite = new ExampleCatchReporterFunSuite
  def fixtureFunSuite = new ExampleCatchReporterFixtureFunSuite
  def funSpec = new ExampleCatchReporterFunSpec
  def fixtureFunSpec = new ExampleCatchReporterFixtureFunSpec
  def featureSpec = new ExampleCatchReporterFeatureSpec
  def fixtureFeatureSpec = new ExampleCatchReporterFixtureFeatureSpec
  def flatSpec = new ExampleCatchReporterFlatSpec
  def fixtureFlatSpec = new ExampleCatchReporterFixtureFlatSpec
  def freeSpec = new ExampleCatchReporterFreeSpec
  def fixtureFreeSpec = new ExampleCatchReporterFixtureFreeSpec
  def propSpec = new ExampleCatchReporterPropSpec
  def fixturePropSpec = new ExampleCatchReporterFixturePropSpec
  def wordSpec = new ExampleCatchReporterWordSpec
  def fixtureWordSpec = new ExampleCatchReporterFixtureWordSpec
  def pathFreeSpec = new ExampleCatchReporterPathFreeSpec
  def pathFunSpec = new ExampleCatchReporterPathFunSpec
  
  class WrapAwareReporter extends EventRecordingReporter {
    @volatile var isWrapped = false
    override def apply(event: Event): Unit = {
      if (!isWrapped) {
        val stackTraceList = Thread.currentThread.getStackTrace.drop(2)
        isWrapped = stackTraceList.find { element => 
          val className = element.getClassName
          try {
            val clazz = getClass.getClassLoader.loadClass(className)
            val t = classOf[Reporter].isAssignableFrom(clazz)
            classOf[Reporter].isAssignableFrom(clazz) && element.getMethodName == "apply"
          }
          catch {
            case _: Throwable => false
          }
        }.isDefined
      }
      super.apply(event)
    }
  }
  
  class WrapperReporter(dispatch: Reporter) extends Reporter {
    def apply(event: Event): Unit = {
      dispatch(event)
    }
  }
  
  class WrapAwareCatchReporter(dispatch: Reporter) extends WrapperCatchReporter(dispatch, new PrintStream(new ByteArrayOutputStream)) {
    @volatile var isWrapped = false
    override def apply(event: Event): Unit = {
      if (!isWrapped) {
        val stackTraceList = Thread.currentThread.getStackTrace.drop(2)
        isWrapped = stackTraceList.find { element => 
          val className = element.getClassName
          try {
            val clazz = getClass.getClassLoader.loadClass(className)
            val t = classOf[Reporter].isAssignableFrom(clazz)
            classOf[Reporter].isAssignableFrom(clazz) && element.getMethodName == "apply"
          }
          catch {
            case _: Throwable => false
          }
        }.isDefined
      }
      super.apply(event)
    }
  }
  
  class BuggyReporter extends WrapAwareReporter {
    private var count = 0
    override def apply(event: Event): Unit = {
      val stackTraceList = Thread.currentThread.getStackTrace.drop(2)
      if (!isWrapped) {
        isWrapped = stackTraceList.exists { element => 
          val className = element.getClassName
          try {
            val clazz = getClass.getClassLoader.loadClass(className)
            classOf[Reporter].isAssignableFrom(clazz) && element.getMethodName == "apply"
          }
          catch {
            case _: Throwable => false
          }
        }
      }
      
      event match {
        case testStarting: TestStarting => 
          if (count == 0) {
            count += 1
            throw new RuntimeException
          }
          else
            super.apply(event)
        case _ => super.apply(event)
      }
    }
  }
  
  class WrapAwareDispatchReporter(reporters: List[Reporter], out: PrintStream) extends DispatchReporter(reporters, out) {
    @volatile var isWrapped = false
    override def apply(event: Event): Unit = {
      if (!isWrapped) {
        val stackTraceList = Thread.currentThread.getStackTrace.drop(2)
        isWrapped = stackTraceList.find { element => 
          val className = element.getClassName
          try {
            val clazz = getClass.getClassLoader.loadClass(className)
            val t = classOf[Reporter].isAssignableFrom(clazz)
            classOf[Reporter].isAssignableFrom(clazz) && element.getMethodName == "apply"
          }
          catch {
            case _: Throwable => false
          }
        }.isDefined
      }
      super.apply(event)
    }
  }
  
  class WrapAwareSuiteSortingReporter(dispatch: Reporter, sortingTimeout: Span) extends SuiteSortingReporter(dispatch, sortingTimeout, new PrintStream(new ByteArrayOutputStream)) {
    @volatile var isWrapped = false
    override def apply(event: Event): Unit = {
      if (!isWrapped) {
        val stackTraceList = Thread.currentThread.getStackTrace.drop(2)
        isWrapped = stackTraceList.find { element => 
          val className = element.getClassName
          try {
            val clazz = getClass.getClassLoader.loadClass(className)
            val t = classOf[Reporter].isAssignableFrom(clazz)
            classOf[Reporter].isAssignableFrom(clazz) && element.getMethodName == "apply"
          }
          catch {
            case _: Throwable => false
          }
        }.isDefined
      }
      super.apply(event)
    }
  }
  
  class WrapAwareTestSortingReporter(suiteId: String, dispatch: Reporter, sortingTimeout: Span, testCount: Int, suiteSorter: Option[DistributedSuiteSorter]) extends TestSortingReporter(suiteId, dispatch, sortingTimeout, testCount, suiteSorter, new PrintStream(new ByteArrayOutputStream)) {
    @volatile var isWrapped = false
    override def apply(event: Event): Unit = {
      if (!isWrapped) {
        val stackTraceList = Thread.currentThread.getStackTrace.drop(2)
        isWrapped = stackTraceList.find { element => 
          val className = element.getClassName
          try {
            val clazz = getClass.getClassLoader.loadClass(className)
            val t = classOf[Reporter].isAssignableFrom(clazz)
            classOf[Reporter].isAssignableFrom(clazz) && element.getMethodName == "apply"
          }
          catch {
            case _: Throwable => false
          }
        }.isDefined
      }
      super.apply(event)
    }
  }
  
  class WrapAwareStopOnFailureReporter(reporter: Reporter) extends StopOnFailureReporter(reporter, Stopper.default, new PrintStream(new ByteArrayOutputStream)) {
    @volatile var isWrapped = false
    override def apply(event: Event): Unit = {
      if (!isWrapped) {
        val stackTraceList = Thread.currentThread.getStackTrace.drop(2)
        isWrapped = stackTraceList.find { element => 
          val className = element.getClassName
          try {
            val clazz = getClass.getClassLoader.loadClass(className)
            val t = classOf[Reporter].isAssignableFrom(clazz)
            classOf[Reporter].isAssignableFrom(clazz) && element.getMethodName == "apply"
          }
          catch {
            case _: Throwable => false
          }
        }.isDefined
      }
      super.apply(event)
    }
  }
  
  test("WrapAwareReporter's isWrapped should return false when it is not wrapped in another reporter") {
    val rep = new WrapAwareReporter
    rep(TestStarting(new Ordinal(99), "suite name", "suite ID", Some("suite.className"), "test name", "test name"))
    assert(!rep.isWrapped, "WrapAwareReporter should return false when it is not wrapped, but it returns true.")
  }
  
  test("WrapAwareReporter's isWrapped should return true when it is wrapped in another reporter") {
    val rep = new WrapAwareReporter
    val wrapper = new WrapperReporter(rep)
    wrapper(TestStarting(new Ordinal(99), "suite name", "suite ID", Some("suite.className"), "test name", "test name"))
    assert(rep.isWrapped, "WrapAwareReporter should return true when it is wrapped, but it returns false.")
  }
  
  test("Well written reporter that does not extend CatchReporter should be wrapped with CatchReporter and execute just fine.") {
    forAll(examples) { s => 
      val rep = new WrapAwareReporter
      s.run(None, Args(reporter = rep))
      assert(rep.testStartingEventsReceived.length === 3)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(rep.isWrapped, "rep should be wrapped with CatchReporter, but it does not.")
    }
  }
  
  test("Well written reporter that extends CatchReporter should not be wrapped with CatchReporter and execute just fine.") {
    forAll(examples) { s => 
      val rep = new EventRecordingReporter
      val wrapper = new WrapAwareCatchReporter(rep)
      s.run(None, Args(reporter = wrapper))
      assert(rep.testStartingEventsReceived.length === 3)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
  
  test("Reporter that does not extend CatchReporter should be wrapped with CatchReporter and handle buggy reporter correctly.") {
    forAll(examples) { s => 
      val rep = new BuggyReporter
      s.run(None, Args(reporter = rep))
      assert(rep.testStartingEventsReceived.length === 2)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(rep.isWrapped, "rep should be wrapped with CatchReporter, but it does not.")
    }
  }
  
  test("Reporter that extends CatchReporter should not be wrapped with CatchReporter and handle buggy reporter correctly.") {
    forAll(examples) { s => 
      val rep = new BuggyReporter
      val wrapper = new WrapAwareCatchReporter(rep)
      s.run(None, Args(reporter = wrapper))
      assert(rep.testStartingEventsReceived.length === 2)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
  
  test("Reporter that extends DispatchReporter should not be wrapped with CatchReporter and execute just fine.") {
    forAll(examples) { s => 
      val rep = new EventRecordingReporter
      val wrapper = new WrapAwareDispatchReporter(List(rep), new PrintStream(new ByteArrayOutputStream))
      s.run(None, Args(reporter = wrapper))
      wrapper.dispatchDisposeAndWaitUntilDone()
      assert(rep.testStartingEventsReceived.length === 3)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
      wrapper.doDispose()
    }
  }
  
  test("Reporter that extends DispatchReporter should not be wrapped with CatchReporter and handle buggy reporter correctly.") {
    forAll(examples) { s => 
      val rep = new BuggyReporter
      val wrapper = new WrapAwareDispatchReporter(List(rep), new PrintStream(new ByteArrayOutputStream))
      s.run(None, Args(reporter = wrapper))
      wrapper.dispatchDisposeAndWaitUntilDone()
      assert(rep.testStartingEventsReceived.length === 2)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
      wrapper.doDispose()
    }
  }
  
  test("Reporter that extends SuiteSortingReporter should not be wrapped with CatchReporter and execute just fine.") {
    forAll(examples) { s => 
      val rep = new EventRecordingReporter
      val wrapper = new WrapAwareSuiteSortingReporter(rep, Span(5, Seconds))
      val tracker = new Tracker()
      wrapper(SuiteStarting(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      s.run(None, Args(reporter = wrapper))
      wrapper(SuiteCompleted(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      assert(rep.testStartingEventsReceived.length === 3)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
  
  test("Reporter that extends SuiteSortingReporter should not be wrapped with CatchReporter and handle buggy reporter correctly.") {
    forAll(examples) { s => 
      val rep = new BuggyReporter
      val wrapper = new WrapAwareSuiteSortingReporter(rep, Span(5, Seconds))
      val tracker = new Tracker()
      wrapper(SuiteStarting(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      s.run(None, Args(reporter = wrapper))
      wrapper(SuiteCompleted(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      assert(rep.testStartingEventsReceived.length === 2)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
  
  test("Reporter that extends TestSortingReporter should not be wrapped with CatchReporter and execute just fine.") {
    forAll(examples) { s => 
      val rep = new EventRecordingReporter
      val wrapper = new WrapAwareTestSortingReporter(s.suiteId, rep, Span(5, Seconds), 3, None)
      val tracker = new Tracker()
      s.run(None, Args(reporter = wrapper))
      assert(rep.testStartingEventsReceived.length === 3)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
  
  test("Reporter that extends TestSortingReporter should not be wrapped with CatchReporter and handle buggy reporter correctly.") {
    forAll(examples) { s => 
      val rep = new BuggyReporter
      val wrapper = new WrapAwareTestSortingReporter(s.suiteId, rep, Span(5, Seconds), 3, None)
      val tracker = new Tracker()
      wrapper(SuiteStarting(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      s.run(None, Args(reporter = wrapper))
      wrapper(SuiteCompleted(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      assert(rep.testStartingEventsReceived.length === 2)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
  
  test("Reporter that extends StopOnFailureReporter should not be wrapped with CatchReporter and execute just fine.") {
    forAll(examples) { s => 
      val rep = new EventRecordingReporter
      val wrapper = new WrapAwareStopOnFailureReporter(rep)
      val tracker = new Tracker()
      s.run(None, Args(reporter = wrapper))
      assert(rep.testStartingEventsReceived.length === 3)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
  
  test("Reporter that extends StopOnFailureReporter should not be wrapped with CatchReporter and handle buggy reporter correctly.") {
    forAll(examples) { s => 
      val rep = new BuggyReporter
      val wrapper = new WrapAwareStopOnFailureReporter(rep)
      val tracker = new Tracker()
      wrapper(SuiteStarting(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      s.run(None, Args(reporter = wrapper))
      wrapper(SuiteCompleted(tracker.nextOrdinal(), s.suiteName, s.suiteId, Some(s.getClass.getName), None))
      assert(rep.testStartingEventsReceived.length === 2)
      assert(rep.testSucceededEventsReceived.length === 3)
      assert(!wrapper.isWrapped, "rep should not be wrapped with CatchReporter, but it does.")
    }
  }
}

trait CatchReporterFixtureServices {}

@DoNotDiscover
class ExampleCatchReporterSpec extends RefSpec with CatchReporterFixtureServices {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterJUnit3Suite extends JUnit3Suite with CatchReporterFixtureServices {
  def testMethod1(): Unit = {}
  def testMethod2(): Unit = {}
  def testMethod3(): Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterJUnitSuite extends JUnitSuite with CatchReporterFixtureServices {
  @Test
  def testMethod1(): Unit = {}
  @Test 
  def testMethod2(): Unit = {}
  @Test 
  def testMethod3(): Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}



@DoNotDiscover
class ExampleCatchReporterTestNGSuite extends TestNGSuite with CatchReporterFixtureServices {
  @TestNG
  def testMethod1(): Unit = {}
  @TestNG
  def testMethod2(): Unit = {}
  @TestNG
  def testMethod3(): Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFunSuite extends AnyFunSuite with CatchReporterFixtureServices {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFixtureFunSuite extends funsuite.FixtureAnyFunSuite with CatchReporterFixtureServices with StringFixture {
  test("Test 1") {s =>}
  test("Test 2") {s =>}
  test("Test 3") {s =>}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFunSpec extends AnyFunSpec with CatchReporterFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFixtureFunSpec extends funspec.FixtureAnyFunSpec with CatchReporterFixtureServices with StringFixture {
  describe("Scope 1") {
    it("Test 1") {s =>}
    it("Test 2") {s =>}
    it("Test 3") {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFeatureSpec extends AnyFeatureSpec with CatchReporterFixtureServices {
  Feature("Feature 1") {
    Scenario("Scenario 1") {}
    Scenario("Scenario 2") {}
    Scenario("Scenario 3") {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with CatchReporterFixtureServices with StringFixture {
  Feature("Feature 1") {
    Scenario("Scenario 1") {s =>}
    Scenario("Scenario 2") {s =>}
    Scenario("Scenario 3") {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFlatSpec extends AnyFlatSpec with CatchReporterFixtureServices {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in {}
  it should "do thing 3" in {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with CatchReporterFixtureServices with StringFixture {
  "Scope 1" should "do thing 1" in {s =>}
  it should "do thing 2" in {s =>}
  it should "do thing 3" in {s =>}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFreeSpec extends AnyFreeSpec with CatchReporterFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with CatchReporterFixtureServices with StringFixture {
  "Scope 1" - {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterPropSpec extends AnyPropSpec with CatchReporterFixtureServices {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFixturePropSpec extends propspec.FixtureAnyPropSpec with CatchReporterFixtureServices with StringFixture {
  property("Test 1") {s =>}
  property("Test 2") {s =>}
  property("Test 3") {s =>}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterWordSpec extends AnyWordSpec with CatchReporterFixtureServices {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterFixtureWordSpec extends wordspec.FixtureAnyWordSpec with CatchReporterFixtureServices with StringFixture {
  "Scope 1" should {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterPathFreeSpec extends freespec.PathAnyFreeSpec with CatchReporterFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class ExampleCatchReporterPathFunSpec extends funspec.PathAnyFunSpec with CatchReporterFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}
