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
package org.scalatest.events

import org.scalatest.SharedHelpers.{ EventRecordingReporter, thisLineNumber }

// SKIP-SCALATESTJS,NATIVE-START
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest._
import Inside._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

// SKIP-SCALATESTJS,NATIVE-START
@RunWith(classOf[JUnitRunner])
// SKIP-SCALATESTJS,NATIVE-END
class LocationSpec extends AnyFunSpec {
  
  class TestLocationFunSuite extends AnyFunSuite {
    test("succeed") {
      
    }
    test("fail") {
      fail()
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
      testLocationSuite.run(None, Args(testLocationReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      val testLocationEventList = testLocationReporter.eventsReceived
      testLocationEventList.foreach {event => 
        event match {
          case testSucceed:TestSucceeded => 
            assertResult(thisLineNumber - 23) { testSucceed.location.get.asInstanceOf[LineInFile].lineNumber }
            assertResult("LocationSpec.scala") { testSucceed.location.get.asInstanceOf[LineInFile].fileName }
            val sep = System.getProperty("file.separator") // Not using File.separator because it is not scala-js friendly
            inside (testSucceed.location) { case Some(location) =>
              inside (location) { case lineInFile: LineInFile =>
                inside (lineInFile.filePathname) { case Some(filePathname) =>
                  if (System.getenv("SCALACTIC_FILL_FILE_PATHNAMES") != null && System.getenv("SCALACTIC_FILL_FILE_PATHNAMES") == "yes")
                    assert(filePathname.endsWith(s"org${sep}scalatest${sep}events${sep}LocationSpec.scala"))
                  else
                    assert(filePathname == "Please set the environment variable SCALACTIC_FILL_FILE_PATHNAMES to yes at compile time to enable this feature.")
                }
              }
            }
          case testFail:TestFailed =>
            assertResult(SeeStackDepthException.getClass) { testFail.location.get.getClass }
          case testPending:TestPending => 
            assertResult(thisLineNumber - 33) { testPending.location.get.asInstanceOf[LineInFile].lineNumber }
            assertResult("LocationSpec.scala") { testPending.location.get.asInstanceOf[LineInFile].fileName }
          case testIgnore:TestIgnored => 
            assertResult(thisLineNumber - 33) { testIgnore.location.get.asInstanceOf[LineInFile].lineNumber }
            assertResult("LocationSpec.scala") { testIgnore.location.get.asInstanceOf[LineInFile].fileName }
          case _ =>
        }
      }
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  class TestLocationSpec extends RefSpec {
    def `test succeed`: Unit = {
    }

    def `test fail`: Unit = {
      fail()
    }

    def `test pending`: Unit = {
      pending
    }

    @Ignore
    def `test ignore`: Unit = {
    }
  }
  
  describe("Suite's events") {
    it("should have TopOfMethod and SeeStackDepthException location with correct line number and source file name") {
      val testLocationSuite = new TestLocationSpec
      val testLocationReporter = new EventRecordingReporter
      testLocationSuite.run(None, Args(testLocationReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
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
  // SKIP-SCALATESTJS,NATIVE-END
}
