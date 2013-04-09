package org.scalatest.events
import org.scalatest.prop.Checkers
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class LocationSpec extends FunSpec with Checkers {
  
  class TestLocationFunSuite extends FunSuite {
    test("succeed") {
      
    }
    test("fail") {
      fail
    }
    test("pending") {
      pending
    }
    ignore("ignore") {
      
    }
  }
  
  describe("FunSuite's events") {
    it("should have LineInFile and SeeStackDepthException location with correct line number and source file name") {
      val testLocationSuite = new TestLocationFunSuite
      val testLocationReporter = new EventRecordingReporter
      testLocationSuite.run(None, Args(testLocationReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      val testLocationEventList = testLocationReporter.eventsReceived
      testLocationEventList.foreach {event => 
        event match {
          case testSucceed:TestSucceeded => 
            assertResult(thisLineNumber - 23) { testSucceed.location.get.asInstanceOf[LineInFile].lineNumber }
            assertResult("LocationSpec.scala") { testSucceed.location.get.asInstanceOf[LineInFile].fileName }
          case testFail:TestFailed => 
            assertResult(SeeStackDepthException.getClass) { testFail.location.get.getClass }
          case testPending:TestPending => 
            assertResult(thisLineNumber - 22) { testPending.location.get.asInstanceOf[LineInFile].lineNumber }
            assertResult("LocationSpec.scala") { testPending.location.get.asInstanceOf[LineInFile].fileName }
          case testIgnore:TestIgnored => 
            assertResult(thisLineNumber - 22) { testIgnore.location.get.asInstanceOf[LineInFile].lineNumber }
            assertResult("LocationSpec.scala") { testIgnore.location.get.asInstanceOf[LineInFile].fileName }
          case _ =>
        }
      }
    }
  }
  
  class TestLocationSuite extends Suite {
    def testSucceed() = {
      
    }
    
    def testFail() = {
      fail
    }
    
    def testPending():Unit = {
      pending
    }
    
    @Ignore
    def testIgnore() = {
      
    }
  }
  
  describe("Suite's events") {
    it("should have TopOfMethod and SeeStackDepthException location with correct line number and source file name") {
      val testLocationSuite = new TestLocationSuite
      val testLocationReporter = new EventRecordingReporter
      testLocationSuite.run(None, Args(testLocationReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      val testLocationEventList = testLocationReporter.eventsReceived
      testLocationEventList.foreach {event => 
        event match {
          case testSucceed:TestSucceeded => 
            assertResult("org.scalatest.events.LocationSpec$TestLocationSuite") { testSucceed.location.get.asInstanceOf[TopOfMethod].className }
            assertResult("public void org.scalatest.events.LocationSpec$TestLocationSuite.testSucceed()") { testSucceed.location.get.asInstanceOf[TopOfMethod].methodId }
          case testFail:TestFailed => 
            assertResult(SeeStackDepthException.getClass) { testFail.location.get.getClass }
          case testPending:TestPending => 
            assertResult("org.scalatest.events.LocationSpec$TestLocationSuite") { testPending.location.get.asInstanceOf[TopOfMethod].className }
            assertResult("public void org.scalatest.events.LocationSpec$TestLocationSuite.testPending()") { testPending.location.get.asInstanceOf[TopOfMethod].methodId }
          case testIgnore:TestIgnored => 
            assertResult("org.scalatest.events.LocationSpec$TestLocationSuite") { testIgnore.location.get.asInstanceOf[TopOfMethod].className }
            assertResult("public void org.scalatest.events.LocationSpec$TestLocationSuite.testIgnore()") { testIgnore.location.get.asInstanceOf[TopOfMethod].methodId }
          case _ =>
        }
      }
    }
  }
  
  //
  // Returns the line number from which this method was called.
  //
  // Found that on some machines it was in the third element in the stack
  // trace, and on others it was the fourth, so here we check the method
  // name of the third element to decide which of the two to return.
  //
  private def thisLineNumber = {
    val st = Thread.currentThread.getStackTrace

    if (!st(2).getMethodName.contains("thisLineNum"))
      st(2).getLineNumber
    else
      st(3).getLineNumber
  }
}
