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

import scala.collection.mutable.ListBuffer
import org.scalatest.SharedHelpers.SilentReporter
import org.scalatest.events.Event
import org.scalatest.events.Ordinal
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class BeforeAndAfterSuite extends AnyFunSuite {

  class TheSuper extends AnyFunSpec {
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
  
  class MySuite extends TheSuper with BeforeAndAfter {

    var beforeCalledBeforeRunTest = false
    var afterCalledAfterRunTest = false

    before {
      if (!runTestWasCalled)
        beforeCalledBeforeRunTest = true
    }
    it("test something") {}
    after {
      if (runTestWasCalled)
        afterCalledAfterRunTest = true
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

  test("before gets called before runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.beforeCalledBeforeRunTest)
  }

  test("after gets called after runTest") {
    val a = new MySuite
    a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> "there"), None, new Tracker))
    assert(a.afterCalledAfterRunTest)
  }

  // test exceptions with runTest
  test("If any invocation of before completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception.") {
    
    class MySuite extends Suite with BeforeAndAfter {
      before { throw new NumberFormatException } 
    }
    intercept[NumberFormatException] {
      val a = new MySuite
      a.run(Some("july"), Args(StubReporter))
    }
  }
  
  test("If any call to super.runTest completes abruptly with an exception, runTest " +
    "will complete abruptly with the same exception, however, before doing so, it will invoke after") {
    trait FunkySuite extends Suite {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfter {
      var afterCalled = false
      after {
        afterCalled = true
      }
    }
    val a = new MySuite
    intercept[NumberFormatException] {
      a.run(Some("july"), Args(StubReporter))
    }
    assert(a.afterCalled)
  }
  
  test("If both super.runTest and after complete abruptly with an exception, runTest " + 
    "will complete abruptly with the exception thrown by super.runTest.") {
    trait FunkySuite extends Suite {
      protected override def runTest(testName: String, args: Args): Status = {
        throw new NumberFormatException
      }
    }
    class MySuite extends FunkySuite with BeforeAndAfter {
      var afterCalled = false
      after {
        afterCalled = true
        throw new IllegalArgumentException
      }
    }
    val a = new MySuite
    intercept[NumberFormatException] {
      a.run(Some("july"), Args(StubReporter))
    }
    assert(a.afterCalled)
  }

  test("If super.runTest returns normally, but after completes abruptly with an " +
    "exception, runTest will return a status that contains that exception as an unreportedException.") {

    class MySuite extends AnyFunSpec with BeforeAndAfter {
      after { throw new NumberFormatException }
      it("test July") {}
    }
    val a = new MySuite
    val status = a.run(Some("test July"), Args(StubReporter))
    assert(status.isCompleted())
    import OptionValues._
    assert(status.unreportedException.value.isInstanceOf[NumberFormatException])
  }

  test("If before completes abruptly super.runTest returns will not be executed, but after will be executed") {

    class MySuite extends AnyFunSpec with BeforeAndAfter {
      var afterIsCalled = false
      var testIsCalled = false
      before { throw new NumberFormatException }
      after {
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
 
  // test exceptions with run
  test("If before is called twice, the second invocation should produce NotAllowedException") {
    var beforeRegisteredFirstTime = false
    var beforeRegisteredSecondTime = false
    class MySuite extends Suite with BeforeAndAfter {
      var s = "zero"
      before {
        s = "one"
      }
      beforeRegisteredFirstTime = true
      before {
        s = "two"
      }
      beforeRegisteredSecondTime = true
    }
    intercept[NotAllowedException] {
      new MySuite
    }
    assert(beforeRegisteredFirstTime)
    assert(!beforeRegisteredSecondTime)
  }

  test("If before is called after run is invoked, the test should fail with NotAllowedException") {
    var beforeRegisteredFirstTime = false
    var beforeRegisteredSecondTime = false
    class MySuite extends AnyFunSuite with BeforeAndAfter {
      var s = "zero"
      var notAllowedExceptionThrown = false
      test("this one should fail") {
        try {
          before {
            s = "one"
          }
        }
        catch {
          case _: NotAllowedException => notAllowedExceptionThrown = true
          case e: Throwable => throw e
        }
      }
    }
    val a = new MySuite
    a.run(None, Args(StubReporter))
    assert(a.notAllowedExceptionThrown)
  }

  test("If after is called twice, the second invocation should produce NotAllowedException") {
    var afterRegisteredFirstTime = false
    var afterRegisteredSecondTime = false
    class MySuite extends Suite with BeforeAndAfter {
      var s = "zero"
      after {
        s = "one"
      }
      afterRegisteredFirstTime = true
      after {
        s = "two"
      }
      afterRegisteredSecondTime = true
    }
    intercept[NotAllowedException] {
      new MySuite
    }
    assert(afterRegisteredFirstTime)
    assert(!afterRegisteredSecondTime)
  }

  test("If after is called after run is invoked, the test should fail with NotAllowedException") {
    var afterRegisteredFirstTime = false
    var afterRegisteredSecondTime = false
    class MySuite extends AnyFunSuite with BeforeAndAfter {
      var s = "zero"
      var notAllowedExceptionThrown = false
      test("this one should fail") {
        try {
          after {
            s = "one"
          }
        }
        catch {
          case _: NotAllowedException => notAllowedExceptionThrown = true
          case e: Throwable => throw e
        }
      }
    }
    val a = new MySuite
    a.run(None, Args(StubReporter))
    assert(a.notAllowedExceptionThrown)
  }
}

class BeforeAndAfterExtendingSuite extends AnyFunSpec with BeforeAndAfter {

  var sb: StringBuilder = _
  val lb = new ListBuffer[String]

  before {
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

class BeforeAndAfterExtendingFunSuite extends AnyFunSuite with BeforeAndAfter {

  var sb: StringBuilder = _
  val lb = new ListBuffer[String]

  before {
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
}


