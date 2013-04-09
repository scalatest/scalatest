package org.scalatest

import SharedHelpers._

class BeforeAndAfterAllSpec extends FunSpec with ShouldMatchers {
  
  class ExampleSuite extends FunSuite with BeforeAndAfterAll with ParallelTestExecution {
    
    @volatile var beforeAllTime: Long = 0
    @volatile var afterAllTime: Long = 0
    
    override protected def beforeAll(configMap: ConfigMap) {
      beforeAllTime = System.currentTimeMillis
    }
    
    test("test 1") { Thread.sleep(100) }
    test("test 2") { Thread.sleep(100) }
    test("test 3") { Thread.sleep(100) }
    
    override def newInstance: Suite with ParallelTestExecution = new ExampleSuite
    
    override protected def afterAll(configMap: ConfigMap) {
      afterAllTime = System.currentTimeMillis
    }
  }
  
  class ExampleNestedSuite extends FunSuite with ParallelTestExecution {
    test("test 1") { Thread.sleep(100) }
    test("test 2") { Thread.sleep(100) }
    test("test 3") { Thread.sleep(100) }
    override def newInstance: Suite with ParallelTestExecution = new ExampleNestedSuite
  }
  
  class ExampleSuites extends Suites(
    new ExampleNestedSuite
  ) with BeforeAndAfterAll { 
    @volatile var beforeAllTime: Long = 0
    @volatile var afterAllTime: Long = 0
    override protected def beforeAll(configMap: ConfigMap) {
      beforeAllTime = System.currentTimeMillis
    } 
    override protected def afterAll(configMap: ConfigMap) {
      afterAllTime = System.currentTimeMillis
    }
  }
  
  class BeforeAfterAllCounter {
    
    import java.util.concurrent.atomic.AtomicInteger
    
    @volatile var beforeAll = new AtomicInteger
    @volatile var afterAll = new AtomicInteger
    
    def incrementBeforeAllCount() {
      beforeAll.incrementAndGet()
    }
    
    def incrementAfterAllCount() {
      afterAll.incrementAndGet()
    }
    
    def beforeAllCount = beforeAll.get
    def afterAllCount = afterAll.get
  }
  
  class ExampleBeforeAndAfterAllWithParallelTestExecutionSuite(counter: BeforeAfterAllCounter) extends FunSuite with BeforeAndAfterAll 
    with OneInstancePerTest {
    
    override protected def beforeAll(configMap: ConfigMap) {
      counter.incrementBeforeAllCount()
    } 
    override protected def afterAll(configMap: ConfigMap) {
      counter.incrementAfterAllCount()
    }
    
    test("test 1") { Thread.sleep(100) }
    test("test 2") { Thread.sleep(100) }
    test("test 3") { Thread.sleep(100) }
    
    override def newInstance: Suite with OneInstancePerTest = new ExampleBeforeAndAfterAllWithParallelTestExecutionSuite(counter)
  }

  describe("BeforeAndAfterAll") {
    it ("should call beforeAll before any test starts, and call afterAll after all tests completed") {
      val suite = new ExampleSuite()
      val rep = new EventRecordingReporter()
      val dist = new TestConcurrentDistributor(2)
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
    it ("should call beforeAll before any test starts in nested suite, and call afterAll after all tests in nested suites completed") {
      val suite = new ExampleSuites
      val rep = new EventRecordingReporter
      val dist = new TestConcurrentDistributor(2)
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
    it ("should be called once for beforeAll and afterAll when used with OneInstancePerTest") {
      val counter = new BeforeAfterAllCounter
      val suite = new ExampleBeforeAndAfterAllWithParallelTestExecutionSuite(counter)
      val rep = new EventRecordingReporter
      suite.run(None, Args(reporter = rep))
      
      counter.beforeAllCount should be (1)
      counter.afterAllCount should be (1)
    }
  }
}
