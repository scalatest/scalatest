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
package org.scalatest.events
import org.scalatest.prop.Checkers
import org.scalatest.SharedHelpers.{ EventRecordingReporter, thisLineNumber }
// SKIP-SCALATESTJS-START
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
// SKIP-SCALATESTJS-END
import org.scalatest._

// SKIP-SCALATESTJS-START
@RunWith(classOf[JUnitRunner])
// SKIP-SCALATESTJS-END
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

  // SKIP-SCALATESTJS-START
  class TestLocationSpec extends RefSpec {
    def `test succeed` {
    }

    def `test fail` {
      fail
    }

    def `test pending` {
      pending
    }

    @Ignore
    def `test ignore` {
    }
  }
  
  describe("Suite's events") {
    it("should have TopOfMethod and SeeStackDepthException location with correct line number and source file name") {
      val testLocationSuite = new TestLocationSpec
      val testLocationReporter = new EventRecordingReporter
      testLocationSuite.run(None, Args(testLocationReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      val testLocationEventList = testLocationReporter.eventsReceived
      testLocationEventList.foreach {event => 
        event match {
          case testSucceed:TestSucceeded => 
            assertResult("org.scalatest.events.LocationSpec$TestLocationSpec$") { testSucceed.location.get.asInstanceOf[TopOfMethod].className }
            assertResult("public void org.scalatest.events.LocationSpec$TestLocationSpec.test$u0020succeed()") { testSucceed.location.get.asInstanceOf[TopOfMethod].methodId }
          case testFail:TestFailed =>
            assertResult(SeeStackDepthException.getClass) { testFail.location.get.getClass }
          case testPending:TestPending =>
            assertResult("org.scalatest.events.LocationSpec$TestLocationSpec$") { testPending.location.get.asInstanceOf[TopOfMethod].className }
            assertResult("public void org.scalatest.events.LocationSpec$TestLocationSpec.test$u0020pending()") { testPending.location.get.asInstanceOf[TopOfMethod].methodId }
          case testIgnore:TestIgnored =>
            assertResult("org.scalatest.events.LocationSpec$TestLocationSpec$") { testIgnore.location.get.asInstanceOf[TopOfMethod].className }
            assertResult("public void org.scalatest.events.LocationSpec$TestLocationSpec.test$u0020ignore()") { testIgnore.location.get.asInstanceOf[TopOfMethod].methodId }
          case _ =>
        }
      }
    }
  }
  // SKIP-SCALATESTJS-END
}
