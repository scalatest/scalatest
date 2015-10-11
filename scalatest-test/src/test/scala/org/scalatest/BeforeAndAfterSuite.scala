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

import scala.collection.mutable.ListBuffer
import org.scalatest.events.Event
import org.scalatest.events.Ordinal
import org.scalatest.SharedHelpers.SilentReporter
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.events.InfoProvided
import scala.concurrent.Promise

class BeforeAndAfterSuite extends AsyncFunSuite {

  // SKIP-SCALATESTJS-START
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  class TheSuper extends FunSpec {
    var runTestWasCalled = false
    var runWasCalled = false
    protected override def runTest(testName: String, args: Args): Status = {
      runTestWasCalled = true
      super.runTest(testName, args)
    }
    override def run(testName: Option[String], args: Args): Status = {
      runWasCalled = true
      super.run(testName, args)
    }
  }
  
  class MySuite extends TheSuper with BeforeAndAfterEach with BeforeAndAfterAll {
    var beforeEachCalledBeforeRunTest = false
    var afterEachCalledAfterRunTest = false
    var beforeAllCalledBeforeExecute = false
    var afterAllCalledAfterExecute = false

    override def beforeAll() {
      if (!runWasCalled)
        beforeAllCalledBeforeExecute = true
    }
    override def beforeEach() {
      if (!runTestWasCalled)
        beforeEachCalledBeforeRunTest = true
    }
    it("test something") {}
    override def afterEach() {
      if (runTestWasCalled)
        afterEachCalledAfterRunTest = true
    }
    override def afterAll() {
      if (runWasCalled)
        afterAllCalledAfterExecute = true
    }

/* TODO: DELETE THIS ONCE THE TESTS ARE PASSING. THE
   DEPRECATION PERIOD FOR THESE METHODS HAS EXPIRED.
    override def beforeAll(config: ConfigMap) {
      if (!runWasCalled)
        beforeAllConfigCalledBeforeExecute = true
      if (config.contains("hi") && config("hi") == "there")
        beforeAllConfigGotTheGreeting = true
      super.beforeAll(config)
    }
    override def beforeEach(config: ConfigMap) {
      if (!runTestWasCalled)
        beforeEachConfigCalledBeforeRunTest = true
      if (config.contains("hi") && config("hi") == "there")
        beforeEachConfigGotTheGreeting = true
      super.beforeEach(config)
    }
    override def afterEach(config: ConfigMap) {
      if (runTestWasCalled)
        afterEachConfigCalledAfterRunTest = true
      if (config.contains("hi") && config("hi") == "there")
        afterEachConfigGotTheGreeting = true
      super.afterEach(config)
    }
    override def afterAll(config: ConfigMap) {
      if (runWasCalled)
        afterAllConfigCalledAfterExecute = true
      if (config.contains("hi") && config("hi") == "there")
        afterAllConfigGotTheGreeting = true
      super.afterAll(config)
    }
*/
  }

  test("super's runTest must be called") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    assert(a.runTestWasCalled)
  }
  
  test("super's run must be called") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    assert(a.runWasCalled)
  }

  test("beforeEach gets called before runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    assert(a.beforeEachCalledBeforeRunTest)
  }
  
  test("afterEach gets called after runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    assert(a.afterEachCalledAfterRunTest)
  }

  test("beforeAll gets called before run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    assert(a.beforeAllCalledBeforeExecute)
  }
  
  test("afterAll gets called after run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    assert(a.afterAllCalledAfterExecute)
  }
  
  // test exceptions with runTest
  test("If any invocation of beforeEach completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends FunSpec with BeforeAndAfterEach with BeforeAndAfterAll {
      override def beforeEach() { throw new NumberFormatException } 
    }
    assertThrows[NumberFormatException] {
      val a = new MySuite
      a.run(Some("july"), Args(StubReporter))
    }
  }
  
  test("If any call to super.runTest completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception, however, before doing so, it will invoke afterEach") {
    trait FunkySuite extends FunSpec {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEach with BeforeAndAfterAll {
      var afterEachCalled = false
      override def afterEach() {
        afterEachCalled = true
      }
    }
    val a = new MySuite
    assertThrows[NumberFormatException] {
      a.run(Some("july"), Args(StubReporter))
    }
    assert(a.afterEachCalled)
  }
  
  test("If both super.runTest and afterEach complete abruptly with an exception, runTest " + 
    "will complete abruptly with the exception thrown by super.runTest.") {
    trait FunkySuite extends FunSpec {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEach with BeforeAndAfterAll {
      var afterEachCalled = false
      override def afterEach() {
        afterEachCalled = true
        throw new IllegalArgumentException
      }
    }
    val a = new MySuite
    assertThrows[NumberFormatException] {
      a.run(Some("july"), Args(StubReporter))
    }
    assert(a.afterEachCalled)
  }
  
  test("If super.runTest returns normally, but afterEach completes abruptly with an " +
    "exception, runTest will complete abruptly with the same exception.") {
       
    class MySuite extends FunSpec with BeforeAndAfterEach with BeforeAndAfterAll {
      override def afterEach() { throw new NumberFormatException }
      it("test July") {}
    }
    val a = new MySuite
    val status = a.run(Some("test July"), Args(StubReporter))
    val promise = Promise[Option[Throwable]]
    status whenCompleted { _ => promise.success(status.unreportedException) }
    promise.future.map { unrepEx =>
      import OptionValues._
      assert(unrepEx.value.isInstanceOf[NumberFormatException] )
    }
  }
 
  // test exceptions with run
  test("If any invocation of beforeAll completes abruptly with an exception, run " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends FunSpec with BeforeAndAfterEach with BeforeAndAfterAll {
      override def beforeAll() { throw new NumberFormatException }
      it("test July") {}
    }
    assertThrows[NumberFormatException] {
      val a = new MySuite
      a.run(None, Args(StubReporter))
    }
  }
 
  test("If any call to super.run completes abruptly with an exception, run " +
    "will complete abruptly with the same exception, however, before doing so, it will invoke afterAll") {
    trait FunkySuite extends FunSpec {
      override def run(testName: Option[String], args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEach with BeforeAndAfterAll {
      var afterAllCalled = false
      it("test 1") {}
      it("test 2") {}
      it("test 3") {}
      override def afterAll() {
        afterAllCalled = true
      }
    }
    val a = new MySuite
    assertThrows[NumberFormatException] {
      a.run(None, Args(StubReporter))
    }
    assert(a.afterAllCalled)
  }
   
  test("If both super.run and afterAll complete abruptly with an exception, run " + 
    "will complete abruptly with the exception thrown by super.run.") {
    trait FunkySuite extends FunSpec {
      override def run(testName: Option[String], args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEach with BeforeAndAfterAll {
      var afterAllCalled = false
      it("test 1") {}
      it("test 2") {}
      it("test 3") {}
      override def afterAll() {
        afterAllCalled = true
        throw new IllegalArgumentException
      }
    }
    val a = new MySuite
    assertThrows[NumberFormatException] {
      a.run(None, Args(StubReporter))
    }
    assert(a.afterAllCalled)
  }
  
  test("If super.run returns normally, but afterAll completes abruptly with an " +
    "exception, run will complete abruptly with the same exception.") {
       
    class MySuite extends FunSpec with BeforeAndAfterEach with BeforeAndAfterAll {
      override def afterAll() { throw new NumberFormatException }
      it("test July") {}
    }
    assertThrows[NumberFormatException] {
      val a = new MySuite
      a.run(None, Args(StubReporter))
    }
  }
}

class BeforeAndAfterExtendingSuite extends FunSpec with BeforeAndAfterEach with BeforeAndAfterAll {

  var sb: StringBuilder = _
  val lb = new ListBuffer[String]

  override def beforeEach() {
    sb = new StringBuilder("ScalaTest is ")
    lb.clear()
  }

  it("test easy") {
    sb.append("easy!")
    assert(sb.toString === "ScalaTest is easy!")
    assert(lb.isEmpty)
    lb += "sweet"
  }

  it("test fun") {
    sb.append("fun!")
    assert(sb.toString === "ScalaTest is fun!")
    assert(lb.isEmpty)
  }
}

class BeforeAndAfterExtendingFunSuite extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  var sb: StringBuilder = _
  val lb = new ListBuffer[String]

  override def beforeEach() {
    sb = new StringBuilder("ScalaTest is ")
    lb.clear()
  }

  test("easy") {
    sb.append("easy!")
    assert(sb.toString === "ScalaTest is easy!")
    assert(lb.isEmpty)
    lb += "sweet"
  }

  test("fun") {
    sb.append("fun!")
    assert(sb.toString === "ScalaTest is fun!")
    assert(lb.isEmpty)
  }
  
  // This now fails to compile, as I want
  // class IWantThisToFailToCompile extends Examples with BeforeAndAfter
}

class BeforeAndAfterInfoSuite extends FunSuite {
  
  test("InfoProvided in the before should be fired") {
    class ExampleSpec extends FunSuite with BeforeAndAfter {
      before {
        info("In Before")
      }
  
      test("test 1") {
        info("info 1")
      }
    }
    
    val exampleSpec = new ExampleSpec()
    val rep = new EventRecordingReporter
    exampleSpec.run(None, Args(rep))
  
    assert(rep.infoProvidedEventsReceived.size === 1)
    val infoProvided = rep.infoProvidedEventsReceived(0)
    assert(infoProvided.message === "In Before")
    
    assert(rep.testSucceededEventsReceived.size === 1)
    val testSucceeded = rep.testSucceededEventsReceived(0)
    assert(testSucceeded.testName === "test 1")
    assert(testSucceeded.recordedEvents.size === 1)
    val testInfoProvided = testSucceeded.recordedEvents(0).asInstanceOf[InfoProvided]
    assert(testInfoProvided.message === "info 1")
  }
  
  test("InfoProvided in the after should be fired") {
    class ExampleSpec extends FunSuite with BeforeAndAfter {
      test("test 1") {
        info("info 1")
      }
      after {
        info("In After")
      }
    }
    
    val exampleSpec = new ExampleSpec()
    val rep = new EventRecordingReporter
    exampleSpec.run(None, Args(rep))
  
    assert(rep.infoProvidedEventsReceived.size === 1)
    val infoProvided = rep.infoProvidedEventsReceived(0)
    assert(infoProvided.message === "In After")
    
    assert(rep.testSucceededEventsReceived.size === 1)
    val testSucceeded = rep.testSucceededEventsReceived(0)
    assert(testSucceeded.testName === "test 1")
    assert(testSucceeded.recordedEvents.size === 1)
    val testInfoProvided = testSucceeded.recordedEvents(0).asInstanceOf[InfoProvided]
    assert(testInfoProvided.message === "info 1")
  }
}


