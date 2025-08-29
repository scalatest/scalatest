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

class DeprecatedCatchReporterProp extends AllSuiteProp {

  type FixtureServices = DeprecatedCatchReporterFixtureServices
  
  def spec = new DeprecatedExampleCatchReporterSpec
  def junit3Suite = new DeprecatedExampleCatchReporterJUnit3Suite
  def junitSuite = new DeprecatedExampleCatchReporterJUnitSuite
  def testngSuite = new DeprecatedExampleCatchReporterTestNGSuite
  def funSuite = new DeprecatedExampleCatchReporterFunSuite
  def fixtureFunSuite = new DeprecatedExampleCatchReporterFixtureFunSuite
  def funSpec = new DeprecatedExampleCatchReporterFunSpec
  def fixtureFunSpec = new DeprecatedExampleCatchReporterFixtureFunSpec
  def featureSpec = new DeprecatedExampleCatchReporterFeatureSpec
  def fixtureFeatureSpec = new DeprecatedExampleCatchReporterFixtureFeatureSpec
  def flatSpec = new DeprecatedExampleCatchReporterFlatSpec
  def fixtureFlatSpec = new DeprecatedExampleCatchReporterFixtureFlatSpec
  def freeSpec = new DeprecatedExampleCatchReporterFreeSpec
  def fixtureFreeSpec = new DeprecatedExampleCatchReporterFixtureFreeSpec
  def propSpec = new DeprecatedExampleCatchReporterPropSpec
  def fixturePropSpec = new DeprecatedExampleCatchReporterFixturePropSpec
  def wordSpec = new DeprecatedExampleCatchReporterWordSpec
  def fixtureWordSpec = new DeprecatedExampleCatchReporterFixtureWordSpec
  def pathFreeSpec = new DeprecatedExampleCatchReporterPathFreeSpec
  def pathFunSpec = new DeprecatedExampleCatchReporterPathFunSpec
  
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

trait DeprecatedCatchReporterFixtureServices {}

@DoNotDiscover
class DeprecatedExampleCatchReporterSpec extends RefSpec with DeprecatedCatchReporterFixtureServices {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterJUnit3Suite extends JUnit3Suite with DeprecatedCatchReporterFixtureServices {
  def testMethod1(): Unit = {}
  def testMethod2(): Unit = {}
  def testMethod3(): Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterJUnitSuite extends JUnitSuite with DeprecatedCatchReporterFixtureServices {
  @Test
  def testMethod1(): Unit = {}
  @Test 
  def testMethod2(): Unit = {}
  @Test 
  def testMethod3(): Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}



@DoNotDiscover
class DeprecatedExampleCatchReporterTestNGSuite extends TestNGSuite with DeprecatedCatchReporterFixtureServices {
  @TestNG
  def testMethod1(): Unit = {}
  @TestNG
  def testMethod2(): Unit = {}
  @TestNG
  def testMethod3(): Unit = {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFunSuite extends AnyFunSuite with DeprecatedCatchReporterFixtureServices {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFixtureFunSuite extends funsuite.FixtureAnyFunSuite with DeprecatedCatchReporterFixtureServices with StringFixture {
  test("Test 1") {s =>}
  test("Test 2") {s =>}
  test("Test 3") {s =>}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFunSpec extends AnyFunSpec with DeprecatedCatchReporterFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFixtureFunSpec extends funspec.FixtureAnyFunSpec with DeprecatedCatchReporterFixtureServices with StringFixture {
  describe("Scope 1") {
    it("Test 1") {s =>}
    it("Test 2") {s =>}
    it("Test 3") {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFeatureSpec extends AnyFeatureSpec with DeprecatedCatchReporterFixtureServices {
  Feature("Feature 1") {
    Scenario("Scenario 1") {}
    Scenario("Scenario 2") {}
    Scenario("Scenario 3") {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with DeprecatedCatchReporterFixtureServices with StringFixture {
  Feature("Feature 1") {
    Scenario("Scenario 1") {s =>}
    Scenario("Scenario 2") {s =>}
    Scenario("Scenario 3") {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFlatSpec extends AnyFlatSpec with DeprecatedCatchReporterFixtureServices {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in {}
  it should "do thing 3" in {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with DeprecatedCatchReporterFixtureServices with StringFixture {
  "Scope 1" should "do thing 1" in {s =>}
  it should "do thing 2" in {s =>}
  it should "do thing 3" in {s =>}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFreeSpec extends AnyFreeSpec with DeprecatedCatchReporterFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with DeprecatedCatchReporterFixtureServices with StringFixture {
  "Scope 1" - {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterPropSpec extends AnyPropSpec with DeprecatedCatchReporterFixtureServices {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFixturePropSpec extends propspec.FixtureAnyPropSpec with DeprecatedCatchReporterFixtureServices with StringFixture {
  property("Test 1") {s =>}
  property("Test 2") {s =>}
  property("Test 3") {s =>}
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterWordSpec extends AnyWordSpec with DeprecatedCatchReporterFixtureServices {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterFixtureWordSpec extends wordspec.FixtureAnyWordSpec with DeprecatedCatchReporterFixtureServices with StringFixture {
  "Scope 1" should {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterPathFreeSpec extends freespec.PathAnyFreeSpec with DeprecatedCatchReporterFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}

@DoNotDiscover
class DeprecatedExampleCatchReporterPathFunSpec extends funspec.PathAnyFunSpec with DeprecatedCatchReporterFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
  override private[scalatest] def createCatchReporter(reporter: Reporter) = new WrapperCatchReporter(reporter, new PrintStream(new ByteArrayOutputStream))
}
