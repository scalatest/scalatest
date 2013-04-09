/*
 * Copyright 2001-2008 Artima, Inc.
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

class BeforeAndAfterEachTestDataSuite extends FunSuite {

  class TheSuper extends Suite {
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
  
  class MySuite extends TheSuper with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
    var beforeEachTestDataCalledBeforeRunTest = false
    var afterEachTestDataCalledAfterRunTest = false
    var beforeAllConfigCalledBeforeExecute = false
    var afterAllConfigCalledAfterExecute = false

    var beforeEachTestDataGotTheGreeting = false
    var afterEachTestDataGotTheGreeting = false
    var beforeAllConfigGotTheGreeting = false
    var afterAllConfigGotTheGreeting = false

    def testSomething() = ()

    override def beforeAll(config: ConfigMap) {
      if (!runWasCalled)
        beforeAllConfigCalledBeforeExecute = true
      if (config.contains("hi") && config("hi") == "there")
        beforeAllConfigGotTheGreeting = true
      super.beforeAll(config)
    }
    override def beforeEach(td: TestData) {
      if (!runTestWasCalled)
        beforeEachTestDataCalledBeforeRunTest = true
      if (td.configMap.contains("hi") && td.configMap("hi") == "there")
        beforeEachTestDataGotTheGreeting = true
      super.beforeEach(td)
    }
    override def afterEach(td: TestData) {
      if (runTestWasCalled)
        afterEachTestDataCalledAfterRunTest = true
      if (td.configMap.contains("hi") && td.configMap("hi") == "there")
        afterEachTestDataGotTheGreeting = true
      super.afterEach(td)
    }
    override def afterAll(config: ConfigMap) {
      if (runWasCalled)
        afterAllConfigCalledAfterExecute = true
      if (config.contains("hi") && config("hi") == "there")
        afterAllConfigGotTheGreeting = true
      super.afterAll(config)
    }
  }

  test("super's runTest must be called") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.runTestWasCalled)
  }
  
  test("super's run must be called") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.runWasCalled)
  }

  test("beforeEach gets called before runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.beforeEachTestDataCalledBeforeRunTest)
  }
  
  test("afterEach gets called after runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.afterEachTestDataCalledAfterRunTest)
  }

  test("beforeAll gets called before run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.beforeAllConfigCalledBeforeExecute)
  }
  
  test("afterAll gets called after run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.afterAllConfigCalledAfterExecute)
  }
  
  test("beforeEach(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.beforeEachTestDataGotTheGreeting)
  }

  test("afterEach(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.afterEachTestDataGotTheGreeting)
  }

  test("beforeAll(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.beforeAllConfigGotTheGreeting)
  }

  test("afterAll(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.afterAllConfigGotTheGreeting)
  }

  // test exceptions with runTest
  test("If any invocation of beforeEach completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends Suite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def beforeEach(td: TestData) { throw new NumberFormatException } 
    }
    intercept[NumberFormatException] {
      val a = new MySuite
      a.run(Some("july"), Args(StubReporter))
    }
  }
  
  test("If any call to super.runTest completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception, however, before doing so, it will invoke afterEach") {
    trait FunkySuite extends Suite {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      var afterEachCalled = false
      override def afterEach(td: TestData) {
        afterEachCalled = true
      }
    }
    val a = new MySuite
    intercept[NumberFormatException] {
      a.run(Some("july"), Args(StubReporter))
    }
    assert(a.afterEachCalled)
  }
  
  test("If both super.runTest and afterEach complete abruptly with an exception, runTest " + 
    "will complete abruptly with the exception thrown by super.runTest.") {
    trait FunkySuite extends Suite {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      var afterEachCalled = false
      override def afterEach(td: TestData) {
        afterEachCalled = true
        throw new IllegalArgumentException
      }
    }
    val a = new MySuite
    intercept[NumberFormatException] {
      a.run(Some("july"), Args(StubReporter))
    }
    assert(a.afterEachCalled)
  }
  
  test("If super.runTest returns normally, but afterEach completes abruptly with an " +
    "exception, runTest will complete abruptly with the same exception.") {
       
    class MySuite extends Suite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def afterEach(td: TestData) { throw new NumberFormatException }
      def testJuly() = ()
    }
    intercept[NumberFormatException] {
      val a = new MySuite
      a.run(Some("testJuly"), Args(StubReporter))
    }
  }
 
  // test exceptions with run
  test("If any invocation of beforeAll completes abruptly with an exception, run " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends Suite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def beforeAll(cm: ConfigMap) { throw new NumberFormatException }
      def testJuly() = ()
    }
    intercept[NumberFormatException] {
      val a = new MySuite
      a.run(None, Args(StubReporter))
    }
  }
 
  test("If any call to super.run completes abruptly with an exception, run " +
    "will complete abruptly with the same exception, however, before doing so, it will invoke afterAll") {
    trait FunkySuite extends Suite {
      override def run(testName: Option[String], args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      var afterAllCalled = false
      override def afterAll(cm: ConfigMap) {
        afterAllCalled = true
      }
    }
    val a = new MySuite
    intercept[NumberFormatException] {
      a.run(None, Args(StubReporter))
    }
    assert(a.afterAllCalled)
  }
   
  test("If both super.run and afterAll complete abruptly with an exception, run " + 
    "will complete abruptly with the exception thrown by super.run.") {
    trait FunkySuite extends Suite {
      override def run(testName: Option[String], args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      var afterAllCalled = false
      override def afterAll(cm: ConfigMap) {
        afterAllCalled = true
        throw new IllegalArgumentException
      }
    }
    val a = new MySuite
    intercept[NumberFormatException] {
      a.run(None, Args(StubReporter))
    }
    assert(a.afterAllCalled)
  }
  
  test("If super.run returns normally, but afterAll completes abruptly with an " +
    "exception, run will complete abruptly with the same exception.") {
       
    class MySuite extends Suite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def afterAll(cm: ConfigMap) { throw new NumberFormatException }
      def testJuly() = ()
    }
    intercept[NumberFormatException] {
      val a = new MySuite
      a.run(None, Args(StubReporter))
    }
  }
}

