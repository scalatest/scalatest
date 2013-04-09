package org.scalatest.events

import org.scalatest._

class LocationMethodSuiteProp extends MethodSuiteProp {
  
  test("Method suites should have correct TopOfMethod location in test events.") {
    forAll(examples) { suite =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      val eventList = reporter.eventsReceived
      eventList.foreach { event => suite.checkFun(event) }
      suite.allChecked
    }
  }
  
  type FixtureServices = TestLocationMethodServices
  
  def suite = new TestLocationSuite
  class TestLocationSuite extends Suite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite"
    val expectedStartingList = List(TestStartingPair("testSucceed", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite", "testSucceed()"), 
                                TestStartingPair("testPending", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite", "testPending()"), 
                                TestStartingPair("testCancel", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite", "testCancel()"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite", "testSucceed()"), 
                              TestResultPair(classOf[TestPending], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite", "testPending()"),
                              TestResultPair(classOf[TestCanceled], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite", "testCancel()"),
                              TestResultPair(classOf[TestIgnored], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite", "testIgnore()"))
    val expectedScopeOpenedList = Nil
    val expectedScopeClosedList = Nil
    
    def testSucceed() {
      
    }
    def testPending() {
      pending
    }
    def testCancel() {
      cancel
    }
    @Ignore
    def testIgnore() {
      
    }
  }
  
  def fixtureSuite = new TestLocationFixtureSuite
  class TestLocationFixtureSuite extends fixture.Suite with FixtureServices with StringFixture {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite"
    val expectedStartingList = List(TestStartingPair("testSucceed", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite", "testSucceed()"), 
                                TestStartingPair("testPending", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite", "testPending()"), 
                                TestStartingPair("testCancel", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite", "testCancel()"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite", "testSucceed()"), 
                              TestResultPair(classOf[TestPending], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite", "testPending()"),
                              TestResultPair(classOf[TestCanceled], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite", "testCancel()"),
                              TestResultPair(classOf[TestIgnored], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite", "testIgnore()"))
    val expectedScopeOpenedList = Nil
    val expectedScopeClosedList = Nil
    
    def testSucceed() {
      
    }
    def testPending() {
      pending
    }
    def testCancel() {
      cancel
    }
    @Ignore
    def testIgnore() {
      
    }
  }
  
  def spec = new TestLocationSpec
  class TestLocationSpec extends Spec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec"
    val expectedStartingList = List(TestStartingPair("A Spec test succeed", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020succeed()"), 
                                TestStartingPair("A Spec test pending", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020pending()"), 
                                TestStartingPair("A Spec test cancel", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020cancel()"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020succeed()"), 
                              TestResultPair(classOf[TestPending], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020pending()"),
                              TestResultPair(classOf[TestCanceled], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020cancel()"),
                              TestResultPair(classOf[TestIgnored], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020ignore()"))
    val expectedScopeOpenedList = List(ScopeOpenedPair("A Spec", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$"))
    val expectedScopeClosedList = List(ScopeClosedPair("A Spec", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$"))
    
    object `A Spec` {
      def `test succeed` {
        
      }
      def `test pending` {
        pending
      }
      def `test cancel` {
        cancel
      }
      @Ignore
      def `test ignore` {
        
      }
    }
  }
  
  def fixtureSpec = new TestLocationFixtureSpec
  class TestLocationFixtureSpec extends fixture.Spec with FixtureServices with StringFixture {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec"
    val expectedStartingList = List(TestStartingPair("A Spec test succeed", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$", "test$u0020succeed(java.lang.String)"), 
                                TestStartingPair("A Spec test pending", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$", "test$u0020pending(java.lang.String)"), 
                                TestStartingPair("A Spec test cancel", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$", "test$u0020cancel(java.lang.String)"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$", "test$u0020succeed(java.lang.String)"), 
                              TestResultPair(classOf[TestPending], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$", "test$u0020pending(java.lang.String)"),
                              TestResultPair(classOf[TestCanceled], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$", "test$u0020cancel(java.lang.String)"),
                              TestResultPair(classOf[TestIgnored], "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$", "test$u0020ignore(java.lang.String)"))
    val expectedScopeOpenedList = List(ScopeOpenedPair("A Spec", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$"))
    val expectedScopeClosedList = List(ScopeClosedPair("A Spec", "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSpec$A$u0020Spec$"))
    
    object `A Spec` {
      def `test succeed`(fixture: String) {
        
      }
      def `test pending`(fixture: String) {
        pending
      }
      def `test cancel`(fixture: String) {
        cancel
      }
      @Ignore
      def `test ignore`(fixture: String) {
        
      }
    }
  }
  
  def junit3Suite = new TestLocationMethodJUnit3Suite
  
  def junitSuite = new TestLocationMethodJUnitSuite
  
  def testngSuite = new TestLocationMethodTestNGSuite
}
