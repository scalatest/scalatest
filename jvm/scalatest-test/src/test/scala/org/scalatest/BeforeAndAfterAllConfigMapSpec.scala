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

import SharedHelpers._
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.concurrent.SleepHelper
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class BeforeAndAfterAllConfigMapSpec extends AnyFunSpec {
  
  class ExampleSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap with ParallelTestExecution {
    
    @volatile var beforeAllTime: Long = 0
    @volatile var afterAllTime: Long = 0
    
    override protected def beforeAll(configMap: ConfigMap): Unit = {
      beforeAllTime = System.currentTimeMillis
    }
    
    test("test 1") { SleepHelper.sleep(100) }
    test("test 2") { SleepHelper.sleep(100) }
    test("test 3") { SleepHelper.sleep(100) }
    
    override def newInstance: Suite with ParallelTestExecution = new ExampleSuite
    
    override protected def afterAll(configMap: ConfigMap): Unit = {
      afterAllTime = System.currentTimeMillis
    }
  }
  
  class ExampleNestedSuite extends AnyFunSuite with ParallelTestExecution {
    test("test 1") { SleepHelper.sleep(100) }
    test("test 2") { SleepHelper.sleep(100) }
    test("test 3") { SleepHelper.sleep(100) }
    override def newInstance: Suite with ParallelTestExecution = new ExampleNestedSuite
  }
  
  @Ignore
  class ExampleIgnoreNestedSuite extends AnyFunSuite with ParallelTestExecution {
    test("test 1") { SleepHelper.sleep(100) }
    test("test 2") { SleepHelper.sleep(100) }
    test("test 3") { SleepHelper.sleep(100) }
    override def newInstance: Suite with ParallelTestExecution = new ExampleNestedSuite
  }
  
  class ExampleSuites extends Suites(
    new ExampleNestedSuite
  ) with BeforeAndAfterAllConfigMap { 
    @volatile var beforeAllTime: Long = 0
    @volatile var afterAllTime: Long = 0
    override protected def beforeAll(configMap: ConfigMap): Unit = {
      beforeAllTime = System.currentTimeMillis
    } 
    override protected def afterAll(configMap: ConfigMap): Unit = {
      afterAllTime = System.currentTimeMillis
    }
  }
  
  class BeforeAfterAllCounter {
    
    @volatile var beforeAll = new AtomicInteger
    @volatile var afterAll = new AtomicInteger
    
    def incrementBeforeAllCount(): Unit = {
      beforeAll.incrementAndGet()
    }
    
    def incrementAfterAllCount(): Unit = {
      afterAll.incrementAndGet()
    }
    
    def beforeAllCount = beforeAll.get
    def afterAllCount = afterAll.get
  }
  
  class ExampleBeforeAndAfterAllWithParallelTestExecutionSuite(counter: BeforeAfterAllCounter) extends AnyFunSuite with BeforeAndAfterAllConfigMap 
    with OneInstancePerTest {
    
    override protected def beforeAll(configMap: ConfigMap): Unit = {
      counter.incrementBeforeAllCount()
    } 
    override protected def afterAll(configMap: ConfigMap): Unit = {
      counter.incrementAfterAllCount()
    }
    
    test("test 1") { SleepHelper.sleep(100) }
    test("test 2") { SleepHelper.sleep(100) }
    test("test 3") { SleepHelper.sleep(100) }
    
    override def newInstance: Suite with OneInstancePerTest = new ExampleBeforeAndAfterAllWithParallelTestExecutionSuite(counter)
  }

  describe("BeforeAndAfterAll") {
    it ("should call beforeAll before any test starts, and call afterAll after all tests completed") {
      // SKIP-SCALATESTJS,NATIVE-START
      val execService = java.util.concurrent.Executors.newFixedThreadPool(2)
      val dist = new TestConcurrentDistributor(execService)
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val dist = new TestConcurrentDistributor()
      try {
        val suite = new ExampleSuite()
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
        // SKIP-SCALATESTJS,NATIVE-START
        execService.shutdown()
        // SKIP-SCALATESTJS,NATIVE-END
      }
    }
    it ("should call beforeAll before any test starts in nested suite, and call afterAll after all tests in nested suites completed") {
      // SKIP-SCALATESTJS,NATIVE-START
      val execService = java.util.concurrent.Executors.newFixedThreadPool(2)
      val dist = new TestConcurrentDistributor(execService)
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val dist = new TestConcurrentDistributor()
      try {
        val suite = new ExampleSuites
        val rep = new EventRecordingReporter
        suite.run(None, Args(reporter = rep, distributor = Some(dist)))
        dist.waitUntilDone()
      
        // should call beforeAll before any test in nested suite starts
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
        // SKIP-SCALATESTJS,NATIVE-START
        execService.shutdown()
        // SKIP-SCALATESTJS,NATIVE-END
      }
    }
    it ("should be called once for beforeAll and afterAll when used with OneInstancePerTest") {
      val counter = new BeforeAfterAllCounter
      val suite = new ExampleBeforeAndAfterAllWithParallelTestExecutionSuite(counter)
      val rep = new EventRecordingReporter
      suite.run(None, Args(reporter = rep))
      
      counter.beforeAllCount should be (1)
      counter.afterAllCount should be (1)
    }
    it ("should have default invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected value false") {
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {}
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (false)
    }
    it ("should invoke beforeAll and afterAll in Suite with no nested suites and some tests, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is true") {
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        override val invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected = true
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        it("test 1") {}
        it("test 2") {}
        it("test 3") {}
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (true)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (1)
      spec.afterAllCount.get should be (1)
    }
    it ("should invoke beforeAll and afterAll in Suite with no nested suites and some tests, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is false") {
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        it("test 1") {}
        it("test 2") {}
        it("test 3") {}
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (false)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (1)
      spec.afterAllCount.get should be (1)
    }
    it ("should invoke beforeAll and afterAll in Suite with nested suites and no test, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is true") {
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        override val invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected = true
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        override def nestedSuites: collection.immutable.IndexedSeq[Suite] = 
          Vector(
            new ExampleNestedSuite, 
            new ExampleNestedSuite, 
            new ExampleNestedSuite
          )
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (true)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (1)
      spec.afterAllCount.get should be (1)
    }
    it ("should invoke beforeAll and afterAll in Suite with nested suites and no tests, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is false") {
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        override def nestedSuites: collection.immutable.IndexedSeq[Suite] = 
          Vector(
            new ExampleNestedSuite, 
            new ExampleNestedSuite, 
            new ExampleNestedSuite
          )
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (false)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (1)
      spec.afterAllCount.get should be (1)
    }
    it("should invoke beforeAll and afterAll in Suite annotated with Ignore, has no nested suites and has tests, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is true") {
      @Ignore
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        override val invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected = true
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        it("test 1") {}
        it("test 2") {}
        it("test 3") {}
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (true)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (1)
      spec.afterAllCount.get should be (1)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should not invoke beforeAll and afterAll in Suite annotated with Ignore, has no nested suites and has tests, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is false") {
      @Ignore
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        it("test 1") {}
        it("test 2") {}
        it("test 3") {}
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (false)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (0)
      spec.afterAllCount.get should be (0)
    }
    // SKIP-SCALATESTJS,NATIVE-END
    it("should not invoke beforeAll and afterAll in Suite that has no test but has nested suites annotated with Ignore, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is true") {
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        override val invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected = true
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        override def nestedSuites: collection.immutable.IndexedSeq[Suite] = 
          Vector(
            new ExampleIgnoreNestedSuite, 
            new ExampleIgnoreNestedSuite, 
            new ExampleIgnoreNestedSuite
          )
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (true)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (1)
      spec.afterAllCount.get should be (1)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should not invoke beforeAll and afterAll in Suite that has no test but has nested suites annotated with Ignore, when invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected is false") {
      class ExampleSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        val beforeAllCount = new AtomicInteger
        val afterAllCount = new AtomicInteger
        override protected def beforeAll(configMap: ConfigMap): Unit = {
          beforeAllCount.incrementAndGet()
        }
        override def nestedSuites: collection.immutable.IndexedSeq[Suite] = 
          Vector(
            new ExampleIgnoreNestedSuite, 
            new ExampleIgnoreNestedSuite, 
            new ExampleIgnoreNestedSuite
          )
        override protected def afterAll(configMap: ConfigMap): Unit = {
          afterAllCount.incrementAndGet()
        }
      }
      val spec = new ExampleSpec
      spec.invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected should be (false)
      spec.run(None, Args(reporter = SilentReporter))
      spec.beforeAllCount.get should be (0)
      spec.afterAllCount.get should be (0)
    }
    // SKIP-SCALATESTJS,NATIVE-END

    // Test exceptions
    it("should, if any invocation of beforeAll completes abruptly with an exception, run " +
      "will complete abruptly with the same exception.") {
      var testIsCalled = false
      class MySuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
        override def beforeAll(configMap: ConfigMap): Unit = {
          throw new NumberFormatException
        }
        test("test 1") { testIsCalled = true }
      }
      val a = new MySuite
      intercept[NumberFormatException] {
        a.run(None, Args(StubReporter))
      }
      assert(!testIsCalled)
    }

    it("should, if any call to super.run completes abruptly with an exception, run " +
      "will complete abruptly with the same exception, however, before doing so, it will invoke afterAll") {
      var afterAllIsCalled = false
      class MySuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
        override def afterAll(configMap: ConfigMap): Unit = {
          afterAllIsCalled = true
        }
        override def run(testName: Option[String], args: Args): Status = {
          super.run(testName, args)
          throw new IllegalArgumentException
        }
        test("test 1") {}
      }
      val a = new MySuite
      intercept[IllegalArgumentException] {
        a.run(None, Args(StubReporter))
      }
      assert(afterAllIsCalled)
    }

    it("should, if both super.run and afterAll complete abruptly with an exception, run " +
      "will complete abruptly with the exception thrown by super.run.") {
      class MySuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
        override def afterAll(configMap: ConfigMap): Unit = { throw new NumberFormatException }
        override def run(testName: Option[String], args: Args): Status = {
          throw new IllegalArgumentException
        }
      }
      val a = new MySuite
      intercept[IllegalArgumentException] {
        a.run(None, Args(StubReporter))
      }
    }

    it("should, if super.run returns normally, but afterEach completes abruptly with an " +
      "exception, the status returned by run will contain that exception as its unreportedException.") {
      class MySuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
        override def afterAll(configMap: ConfigMap): Unit = { throw new NumberFormatException }
        test("test July") {}
      }
      val a = new MySuite
      val status = a.run(Some("test July"), Args(StubReporter))
      assert(status.isCompleted())
      import OptionValues._
      assert(status.unreportedException.value.isInstanceOf[NumberFormatException])
    }

    it("should, ff beforeAll completes abruptly, no test will be executed, but afterAll will be executed") {

      class MySuite extends AnyFunSpec with BeforeAndAfterAllConfigMap {
        var afterAllIsCalled = false
        var test1IsCalled = false
        var test2IsCalled = false
        var test3IsCalled = false

        override def beforeAll(configMap: ConfigMap): Unit = { throw new NumberFormatException }
        override def afterAll(configMap: ConfigMap): Unit = { afterAllIsCalled = true }

        it("test 1") { test1IsCalled = true }
        it("test 2") { test2IsCalled = true }
        it("test 3") { test3IsCalled = true }
      }
      val a = new MySuite
      assertThrows[NumberFormatException] {
        a.run(Some("test July"), Args(StubReporter))
      }
      assert(a.afterAllIsCalled)
      assert(!a.test1IsCalled)
      assert(!a.test2IsCalled)
      assert(!a.test3IsCalled)
    }
  }
}