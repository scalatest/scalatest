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
package org.scalatest.concurrent

import org.scalatest._
import SharedHelpers._
import Thread.State._
import java.util.concurrent.atomic.AtomicBoolean
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.SharedHelpers.thisLineNumber
import time.{Millis, Span}

class ConductorSuite extends FunSuite with Matchers with Conductors with SeveredStackTraces {

  test("if conduct is called twice, the second time it throws an NotAllowedException") {
    val conductor = new Conductor
    conductor.conduct()
    val caught = intercept[NotAllowedException] { conductor.conduct() }
    caught.getMessage should be ("A Conductor's conduct method can only be invoked once.")
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 3))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
  }

  test("if conduct has not been called, conductingHasBegun should return false"){
    val conductor = new Conductor
    conductor.conductingHasBegun should be (false)
  }

  test("if conduct has been called, conductingHasBegun should return true") {
    val conductor = new Conductor
    conductor.conduct
    conductor.conductingHasBegun should be (true)
  }

  test("if thread {} is called after the test has been conducted, it throws an NotAllowedException" +
           "with a detail message that explains the problem") {
    val conductor = new Conductor
    conductor.conduct
    val caught =
      intercept[NotAllowedException] {
        conductor.thread("name") { 1 should be (1) }
      }
    caught.getMessage should be ("Cannot invoke the thread method on Conductor after its multi-threaded test has completed.")
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 4))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
  }

  test("if thread(String) {} is called after the test has been conducted, it throws NotAllowedException" +
          "with a detail message that explains the problem"){
    val conductor = new Conductor    
    conductor.conduct
    val caught =
      intercept[NotAllowedException] {
        conductor.thread("name") { 1 should be (1) }
      }
    caught.getMessage should be ("Cannot invoke the thread method on Conductor after its multi-threaded test has completed.")
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 4))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
  }

  test("if whenFinished is called twice on the same conductor, a NotAllowedException is thrown that explains it " +
          "can only be called once") {
    val conductor = new Conductor    
    conductor.whenFinished { 1 should be (1) }
    val caught =
      intercept[NotAllowedException] {
        conductor.whenFinished { 1 should be (1) }
      }
    caught.getMessage should be ("Cannot invoke whenFinished after conduct (which is called by whenFinished) has been invoked.")
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 4))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
  }

  test("if thread(String) is called twice with the same String name, the second invocation results " +
          "in an IllegalArgumentException that explains each thread in a multi-threaded test " +
          "must have a unique name") {

    val conductor = new Conductor
    conductor.thread("Fiesta del Mar") { 1 should be (1) }
    val caught =
      intercept[NotAllowedException] {
        conductor.thread("Fiesta del Mar") { 2 should be (2) }
      }
    caught.getMessage should be ("Cannot register two threads with the same name. Duplicate name: Fiesta del Mar.")
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 4))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
  }

  test("waitForBeat throws NotAllowedException if is called with zero or a negative number") {
    val conductor = new Conductor
    val caught =
      intercept[NotAllowedException] {
        conductor.waitForBeat(0)
      }
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 3))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
    caught.getMessage should be ("A Conductor starts at beat zero, so you can't wait for beat zero.")
    val caught2 =
      intercept[NotAllowedException] {
        conductor.waitForBeat(-1)
      }
    caught2.getMessage should be ("A Conductor starts at beat zero, so you can only wait for a beat greater than zero.")
    caught2.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 4))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
  }

  test("If a non-positive number is passed to conduct for clockPeriod, it will throw NotAllowedException") {
    val conductor = new Conductor
    val caught =
      intercept[NotAllowedException] {
        conductor.conduct(0, 100)
      }
    caught.getMessage should be ("The clockPeriod passed to conduct must be greater than zero. Value passed was: 0.")
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 4))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
    val caught2 =
      intercept[NotAllowedException] {
        conductor.conduct(-1, 100)
      }
    caught2.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 3))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
    caught2.getMessage should be ("The clockPeriod passed to conduct must be greater than zero. Value passed was: -1.")
  }

  test("If a non-positive number is passed to conduct for runLimit, it will throw NotAllowedException") {
    val conductor = new Conductor
    val caught =
      intercept[NotAllowedException] {
        conductor.conduct(100, 0)
      }
    caught.getMessage should be ("The timeout passed to conduct must be greater than zero. Value passed was: 0.")
    caught.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 4))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
    val caught2 =
      intercept[NotAllowedException] {
        conductor.conduct(100, -1)
      }
    caught2.failedCodeFileNameAndLineNumberString match {
      case Some(s) => s should equal ("ConductorSuite.scala:" + (thisLineNumber - 3))
      case None => fail("Didn't produce a file name and line number string: ", caught)
    }
    caught2.getMessage should be ("The timeout passed to conduct must be greater than zero. Value passed was: -1.")
  }

  test("withConductorFrozen executes the passed function once") {
    val conductor = new Conductor
    var functionExecutionCount = 0
    conductor.withConductorFrozen { // Function will be executed by the calling thread
      functionExecutionCount += 1
    }
    functionExecutionCount should be (1)
  }

  test("first exception thrown is reported") {
    val e = new RuntimeException("howdy")
    class MySuite extends FunSuite {
      test("this will fail") {
        val conductor = new Conductor
        import conductor._
        thread {
          waitForBeat(1)
        }
        thread {
          throw e
          ()
        }
        conductor.conduct()
      }
    }
    val a = new MySuite
    val rep = new EventRecordingReporter
    a.run(None, Args(rep))
    val tf = rep.testFailedEventsReceived
    tf.size should === (1)
    tf.head.throwable should be ('defined)
    tf.head.throwable.get should be theSameInstanceAs e
  }

  test("whenFinished can only be called by thread that created Conductor.") {
    val conductor = new Conductor
    import conductor._
    thread {
      intercept[NotAllowedException] {
        whenFinished { 1 should be (1) }
      }.getMessage should be ("whenFinished can only be called by the thread that created Conductor.")
    }
    whenFinished { 1 should be (1) }
  }

  test("isConductorFrozen returns true if the conductor is frozen, false otherwise") {
    val conductor = new Conductor
    import conductor._
    conductor.isConductorFrozen should be (false)
    withConductorFrozen {
      conductor.isConductorFrozen should be (true)
    }
  }

  test("the beat method returns the correct value") {
    val conductor = new Conductor
    import conductor._
    beat should equal (0)
    thread {
      beat should equal (0)
      waitForBeat(1)
      beat should equal (1)
      waitForBeat(2)
      beat should equal (2)
    }
    whenFinished {
      beat should equal (2)
    }
  }

  test("if I wait for a beat that's lower than the current beat, I just keep going") {
    val conductor = new Conductor
    import conductor._
    beat should equal (0)
    thread {
      beat should equal (0)
      waitForBeat(1)
      beat should equal (1)
      waitForBeat(1) // This should also work
      beat should equal (1)
      waitForBeat(2)
      beat should equal (2)
      waitForBeat(1) // This should also work
      beat should equal (2)
    }
    whenFinished {
      beat should equal (2)
    }
  }

  class Forevermore {
    def waitForever() {
      synchronized {
        wait()
      }
    }
  }

  test("deadlock is detected") {
    val conductor = new Conductor
    import conductor._
    val monitor = new Forevermore
    thread {
      monitor.waitForever()
    }
    thread {
      monitor.waitForever()
    }
    val caught =
      intercept[RuntimeException] {
        conduct()
      }
    caught.getMessage should be ("Test aborted because of suspected deadlock. No progress has been made (the beat did not advance) for 50 clock periods (750 milliseconds).")
  }

  test("other threads are killed when one thread throws an exception") {
    val conductor = new Conductor
    import conductor._
    val monitor = new Forevermore
    val threadWasKilled = new AtomicBoolean()
    thread {
      try {
        monitor.waitForever()
      }
      catch {
        case t: ThreadDeath =>
          threadWasKilled.set(true)
          throw t
      }
    }
    thread {
      waitForBeat(1)
      fail()
      ()
    }
    intercept[RuntimeException] {
      conduct()
    }
    threadWasKilled.get should be (true)
  }

  test("runaway threads will cause a test to be timed out") {
    val conductor = new Conductor
    import conductor._
    class Counter {
      @volatile var count = 0
    }
    val counter = new Counter
    thread {
      while (true)
        counter.count += 1
    }
    thread {
      while (true)
        counter.count -= 1
    }
    val caught =
      intercept[RuntimeException] {
        conduct(10, 1)
      }
    caught.getMessage should be ("Test timed out because threads existed that were runnable while no progress was made (the beat did not advance) for 1 second.")
  }

  test("ConductorFixture is a stackable trait that delegates test function execution to withFixture(NoArgTest)") {
    var calledSuperWithFixtureNoArgTest = false
    class MySpec extends fixture.FunSuite with ConductorFixture {
      override def withFixture(test: NoArgTest): Outcome = {
        calledSuperWithFixtureNoArgTest = true
        super.withFixture(test)
      }
      test("one") { c => }
    }

    val a = new MySpec
    a.run(None, Args(SilentReporter))
    calledSuperWithFixtureNoArgTest should be (true)
  }

  test("ConductorMethods is a stackable trait that delegates test function execution to withFixture(NoArgTest)") {
    var calledSuperWithFixtureNoArgTest = false
    trait SuperTrait extends SuiteMixin { this: Suite =>
      abstract override def withFixture(test: NoArgTest): Outcome = {
        calledSuperWithFixtureNoArgTest = true
        super.withFixture(test)
      }
    }
    class MySpec extends FunSuite with SuperTrait with ConductorMethods {
      test("one") {}
    }

    val a = new MySpec
    a.run(None, Args(SilentReporter))
    calledSuperWithFixtureNoArgTest should be (true)
  }

  // The next 3 tests just make sure things work when calling the other overloaded conduct methods
  test("first exception thrown is reported when calling conduct(timeout, interval)") {
    val e = new RuntimeException("howdy")
    class MySuite extends FunSuite {
      test("this will fail") {
        val conductor = new Conductor
        import conductor._
        thread {
          waitForBeat(1)
        }
        thread {
          throw e
          ()
        }
        conductor.conduct(timeout(Span(300, Millis)), interval(Span(10, Millis)))
      }
    }
    val a = new MySuite
    val rep = new EventRecordingReporter
    a.run(None, Args(rep))
    val tf = rep.testFailedEventsReceived
    tf.size should === (1)
    tf.head.throwable should be ('defined)
    tf.head.throwable.get should be theSameInstanceAs e
  }

  test("first exception thrown is reported when calling conduct(interval)") {
    val e = new RuntimeException("howdy")
    class MySuite extends FunSuite {
      test("this will fail") {
        val conductor = new Conductor
        import conductor._
        thread {
          waitForBeat(1)
        }
        thread {
          throw e
          ()
        }
        conductor.conduct(interval(Span(10, Millis)))
      }
    }
    val a = new MySuite
    val rep = new EventRecordingReporter
    a.run(None, Args(rep))
    val tf = rep.testFailedEventsReceived
    tf.size should === (1)
    tf.head.throwable should be ('defined)
    tf.head.throwable.get should be theSameInstanceAs e
  }

  test("first exception thrown is reported when calling conduct(timeout)") {
    val e = new RuntimeException("howdy")
    class MySuite extends FunSuite {
      test("this will fail") {
        val conductor = new Conductor
        import conductor._
        thread {
          waitForBeat(1)
        }
        thread {
          throw e
          ()
        }
        conductor.conduct(timeout(Span(300, Millis)))
      }
    }
    val a = new MySuite
    val rep = new EventRecordingReporter
    a.run(None, Args(rep))
    val tf = rep.testFailedEventsReceived
    tf.size should === (1)
    tf.head.throwable should be ('defined)
    tf.head.throwable.get should be theSameInstanceAs e
  }
}
