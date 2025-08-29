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

import org.scalatest.SharedHelpers.SilentReporter
import org.scalatest.events.Event
import org.scalatest.events.InfoProvided
import org.scalatest.events.Ordinal
import scala.collection.mutable.ListBuffer
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class BeforeAndAfterEachTestDataSuite extends AnyFunSuite {

  class TheSuper extends AnyFunSuite {
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

    test("test something") {}

    override def beforeAll(config: ConfigMap): Unit = {
      if (!runWasCalled)
        beforeAllConfigCalledBeforeExecute = true
      if (config.contains("hi") && config("hi") == "there")
        beforeAllConfigGotTheGreeting = true
      super.beforeAll(config)
    }
    override def beforeEach(td: TestData): Unit = {
      if (!runTestWasCalled)
        beforeEachTestDataCalledBeforeRunTest = true
      if (td.configMap.contains("hi") && td.configMap("hi") == "there")
        beforeEachTestDataGotTheGreeting = true
      super.beforeEach(td)
    }
    override def afterEach(td: TestData): Unit = {
      if (runTestWasCalled)
        afterEachTestDataCalledAfterRunTest = true
      if (td.configMap.contains("hi") && td.configMap("hi") == "there")
        afterEachTestDataGotTheGreeting = true
      super.afterEach(td)
    }
    override def afterAll(config: ConfigMap): Unit = {
      if (runWasCalled)
        afterAllConfigCalledAfterExecute = true
      if (config.contains("hi") && config("hi") == "there")
        afterAllConfigGotTheGreeting = true
      super.afterAll(config)
    }
  }

  test("super's runTest must be called") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.runTestWasCalled)
  }
  
  test("super's run must be called") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.runWasCalled)
  }

  test("beforeEach gets called before runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.beforeEachTestDataCalledBeforeRunTest)
  }
  
  test("afterEach gets called after runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.afterEachTestDataCalledAfterRunTest)
  }

  test("beforeAll gets called before run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.beforeAllConfigCalledBeforeExecute)
  }
  
  test("afterAll gets called after run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.afterAllConfigCalledAfterExecute)
  }
  
  test("beforeEach(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.beforeEachTestDataGotTheGreeting)
  }

  test("afterEach(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.afterEachTestDataGotTheGreeting)
  }

  test("beforeAll(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.beforeAllConfigGotTheGreeting)
  }

  test("afterAll(config) gets the config passed to run") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.afterAllConfigGotTheGreeting)
  }

  // test exceptions with runTest
  test("If any invocation of beforeEach completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends Suite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def beforeEach(td: TestData): Unit = { throw new NumberFormatException } 
    }
    assertThrows[NumberFormatException] {
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
      override def afterEach(td: TestData): Unit = {
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
    trait FunkySuite extends Suite {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      var afterEachCalled = false
      override def afterEach(td: TestData): Unit = {
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
    "exception, runTest will return a status that is already completed and contains the exception as an unreportedException.") {
       
    class MySuite extends AnyFunSuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def afterEach(td: TestData): Unit = { throw new NumberFormatException }
      test("test July") {}
    }
    val a = new MySuite
    val status = a.run(Some("test July"), Args(StubReporter))
    assert(status.isCompleted())
    import OptionValues._
    assert(status.unreportedException.value.isInstanceOf[NumberFormatException])
  }
 
  // test exceptions with run
  test("If any invocation of beforeAll completes abruptly with an exception, run " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends AnyFunSuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def beforeAll(cm: ConfigMap): Unit = { throw new NumberFormatException }
      test("test July") {}
    }
    assertThrows[NumberFormatException] {
      val a = new MySuite
      a.run(None, Args(StubReporter))
    }
  }
 
  test("If any call to super.run completes abruptly with an exception, run " +
    "will complete abruptly with the same exception, however, before doing so, it will invoke afterAll") {
    trait FunkySuite extends AnyFunSuite {
      override def run(testName: Option[String], args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      var afterAllCalled = false
      test("test 1") {}
      test("test 2") {}
      test("test 3") {}
      override def afterAll(cm: ConfigMap): Unit = {
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
    trait FunkySuite extends AnyFunSuite {
      override def run(testName: Option[String], args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      var afterAllCalled = false
      test("test 1") {}
      test("test 2") {}
      test("test 3") {}
      override def afterAll(cm: ConfigMap): Unit = {
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
    "exception, runTest will return a status that contains that exception as an unreportedException (using BeforeAndAfterAllConfigMap).") {
    class MySuite extends AnyFunSuite with BeforeAndAfterEachTestData with BeforeAndAfterAllConfigMap {
      override def afterAll(cm: ConfigMap): Unit = { throw new NumberFormatException }
      test("test July") {}
    }
    val a = new MySuite
    val status = a.run(Some("test July"), Args(StubReporter))
    assert(status.isCompleted())
    import OptionValues._
    assert(status.unreportedException.value.isInstanceOf[NumberFormatException])
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("Should propagate and not run afterEach if super.runTest throw java.lang.annotation.AnnotationFormatError") {

    class ExampleSpec extends AnyFunSuite with BeforeAndAfterEachTestData {
      var afterAllCalled = false
      test("test 1") {
        throw new java.lang.annotation.AnnotationFormatError("test")
      }
      override def afterEach(testData: TestData): Unit = {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[java.lang.annotation.AnnotationFormatError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }

  test("Should propagate and not run afterEach if super.runTest throw java.nio.charset.CoderMalfunctionError") {

    class ExampleSpec extends AnyFunSuite with BeforeAndAfterEachTestData {
      var afterAllCalled = false
      test("test 1") {
        throw new java.nio.charset.CoderMalfunctionError(new RuntimeException("test"))
      }
      override def afterEach(testData: TestData): Unit = {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[java.nio.charset.CoderMalfunctionError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("Should propagate and not run afterEach if super.runTest throw javax.xml.parsers.FactoryConfigurationError") {

    class ExampleSpec extends AnyFunSuite with BeforeAndAfterEachTestData {
      var afterAllCalled = false
      test("test 1") {
        throw new javax.xml.parsers.FactoryConfigurationError()
      }
      override def afterEach(testData: TestData): Unit = {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[javax.xml.parsers.FactoryConfigurationError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }

  test("Should propagate and not run afterEach if super.runTest throw java.lang.LinkageError") {

    class ExampleSpec extends AnyFunSuite with BeforeAndAfterEachTestData {
      var afterAllCalled = false
      test("test 1") {
        throw new java.lang.LinkageError()
      }
      override def afterEach(testData: TestData): Unit = {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[java.lang.LinkageError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }

  test("Should propagate and not run afterEach if super.runTest throw javax.xml.transform.TransformerFactoryConfigurationError") {

    class ExampleSpec extends AnyFunSuite with BeforeAndAfterEachTestData {
      var afterAllCalled = false
      test("test 1") {
        throw new javax.xml.transform.TransformerFactoryConfigurationError()
      }
      override def afterEach(testData: TestData): Unit = {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[javax.xml.transform.TransformerFactoryConfigurationError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }

  test("Should propagate and not run afterEach if super.runTest throw java.lang.VirtualMachineError") {

    class ExampleSpec extends AnyFunSuite with BeforeAndAfterEachTestData {
      var afterAllCalled = false
      test("test 1") {
        throw new java.lang.VirtualMachineError() {}
      }
      override def afterEach(testData: TestData): Unit = {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[java.lang.VirtualMachineError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("Should run afterEach, but not tests if beforeEach completes abruptly") {

    class MySuite extends AnyFunSpec with BeforeAndAfterEachTestData {
      var afterIsCalled = false
      var testIsCalled = false
      override def beforeEach(testData: TestData): Unit = { throw new NumberFormatException }
      override def afterEach(testData: TestData): Unit = {
        afterIsCalled = true
      }
      it("test July") {
        testIsCalled = true
      }
    }
    val a = new MySuite
    assertThrows[NumberFormatException] {
      a.run(Some("test July"), Args(StubReporter))
    }
    assert(a.afterIsCalled)
    assert(!a.testIsCalled)
  }
}

