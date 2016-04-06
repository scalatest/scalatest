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

class BeforeAndAfterEachSuite extends FunSuite {

  class TheSuper extends FunSuite {
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
  
  class MySuite extends TheSuper with BeforeAndAfterEach {
    var beforeEachCalledBeforeRunTest = false
    var afterEachCalledAfterRunTest = false

    test("test something") {}

    override def beforeEach() {
      if (!runTestWasCalled)
        beforeEachCalledBeforeRunTest = true
      super.beforeEach()
    }
    override def afterEach() {
      if (runTestWasCalled)
        afterEachCalledAfterRunTest = true
      super.afterEach()
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
    assert(a.beforeEachCalledBeforeRunTest)
  }
  
  test("afterEach gets called after runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker, Set.empty))
    assert(a.afterEachCalledAfterRunTest)
  }

  // test exceptions with runTest
  test("If any invocation of beforeEach completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends Suite with BeforeAndAfterEach {
      override def beforeEach() { throw new NumberFormatException } 
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
    class MySuite extends FunkySuite with BeforeAndAfterEach {
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
    trait FunkySuite extends Suite {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfterEach {
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
    "exception, the status returned by runTest will contain that exception as its unreportedException.") {
       
    class MySuite extends FunSuite with BeforeAndAfterEach {
      override def afterEach() { throw new NumberFormatException }
      test("test July") {}
    }
    val a = new MySuite
    val status = a.run(Some("test July"), Args(StubReporter))
    assert(status.isCompleted)
    import OptionValues._
    assert(status.unreportedException.value.isInstanceOf[NumberFormatException])
  }
 
  // SKIP-SCALATESTJS-START
  test("Should propagate and not run afterEach if super.runTest throw java.lang.annotation.AnnotationFormatError") {

    class ExampleSpec extends FunSuite with BeforeAndAfterEach {
      var afterAllCalled = false
      test("test 1") {
        throw new java.lang.annotation.AnnotationFormatError("test")
      }
      override def afterEach() {
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

    class ExampleSpec extends FunSuite with BeforeAndAfterEach {
      var afterAllCalled = false
      test("test 1") {
        throw new java.nio.charset.CoderMalfunctionError(new RuntimeException("test"))
      }
      override def afterEach() {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[java.nio.charset.CoderMalfunctionError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }

  // SKIP-SCALATESTJS-START
  test("Should propagate and not run afterEach if super.runTest throw javax.xml.parsers.FactoryConfigurationError") {

    class ExampleSpec extends FunSuite with BeforeAndAfterEach {
      var afterAllCalled = false
      test("test 1") {
        throw new javax.xml.parsers.FactoryConfigurationError()
      }
      override def afterEach() {
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

    class ExampleSpec extends FunSuite with BeforeAndAfterEach {
      var afterAllCalled = false
      test("test 1") {
        throw new java.lang.LinkageError()
      }
      override def afterEach() {
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

    class ExampleSpec extends FunSuite with BeforeAndAfterEach {
      var afterAllCalled = false
      test("test 1") {
        throw new javax.xml.transform.TransformerFactoryConfigurationError()
      }
      override def afterEach() {
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

    class ExampleSpec extends FunSuite with BeforeAndAfterEach {
      var afterAllCalled = false
      test("test 1") {
        throw new java.lang.VirtualMachineError() {}
      }
      override def afterEach() {
        afterAllCalled = true
      }
    }

    val a = new ExampleSpec
    assertThrows[java.lang.VirtualMachineError] {
      a.run(None, Args(StubReporter))
    }
    assert(!a.afterAllCalled)
  }
  // SKIP-SCALATESTJS-END

  test("Should run afterEach, but not tests if beforeEach completes abruptly") {

    class MySuite extends FunSpec with BeforeAndAfterEach {
      var afterIsCalled = false
      var testIsCalled = false
      override def beforeEach() { throw new NumberFormatException }
      override def afterEach() {
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

