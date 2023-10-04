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
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatestplus.junit.JUnit3Suite
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest._
import SharedHelpers._
import org.scalatest.{ featurespec, flatspec, freespec, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class DeprecatedLocationSuiteProp extends SuiteProp
{
  test("All suite types should have correct location in SuiteStarting, SuiteCompleted, SuiteAborted and TestFailed event.") {
    forAll(examples) { suite =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      val eventList = reporter.eventsReceived
      eventList.foreach { event => suite.checkFun(event) }
      suite.allChecked
    }
  }
  
  type FixtureServices = TestLocationServices

  // SKIP-SCALATESTJS,NATIVE-START
  def junit3Suite = new TestLocationJUnit3Suite
  
  def junitSuite = new TestLocationJUnitSuite
  
  def testngSuite = new TestLocationTestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
  
  def funSuite = new TestLocationFunSuite
  class TestLocationFunSuite extends AnyFunSuite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFunSuite"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends AnyFunSuite {
      test("info") {
        info("test info")
      }
    }
    class AbortNestedSuite extends AnyFunSuite {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyFunSuite {
      test("fail") { fail() }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFunSuite = new TestLocationFixtureFunSuite
  class TestLocationFixtureFunSuite extends StringFixtureFunSuite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFixtureFunSuite"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends StringFixtureFunSuite {
      test("info") { param =>
        info("test info")
      }
    }
    class AbortNestedSuite extends StringFixtureFunSuite {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends StringFixtureFunSuite {
      test("fail") { () => fail() }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  } 
  
  def funSpec = new LocationTestSpec
  class LocationTestSpec extends AnyFunSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$LocationTestSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends AnyFunSpec {
      it("info") {
        info("test info")
      }
    }
    class AbortNestedSuite extends AnyFunSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyFunSpec {
      it("fail") { fail() }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFunSpec = new TestLocationFixtureFunSpec
  class TestLocationFixtureFunSpec extends StringFixtureFunSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFixtureFunSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends StringFixtureFunSpec {
      it("info") { param =>
        info("test info")
      }
    }
    class AbortNestedSuite extends StringFixtureFunSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyFunSpec {
      it("fail") { fail() }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def featureSpec = new TestLocationFeatureSpec
  class TestLocationFeatureSpec extends AnyFeatureSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFeatureSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Feature: feature Scenario: info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Feature: feature Scenario: fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends AnyFeatureSpec {
      Feature("feature") {
        Scenario("info") {
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends AnyFeatureSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyFeatureSpec {
      Feature("feature") { 
        Scenario("fail") {
          fail()
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFeatureSpec = new TestLocationFixtureFeatureSpec
  class StringFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with StringFixture
  class TestLocationFixtureFeatureSpec extends StringFixtureFeatureSpec with FixtureServices { 
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFixtureFeatureSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Feature: feature Scenario: info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Feature: feature Scenario: fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends StringFixtureFeatureSpec {
      Feature("feature") { 
        Scenario("info") { param =>
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends StringFixtureFeatureSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends StringFixtureFeatureSpec {
      Feature("feature") { 
        Scenario("fail") {
          () => fail()
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def flatSpec = new TestLocationFlatSpec
  class TestLocationFlatSpec extends AnyFlatSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFlatSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends AnyFlatSpec {
      "Test" should "info" in {
        info("test info")
      }
    }
    class AbortNestedSuite extends AnyFlatSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyFlatSpec {
      "Test" should "fail" in {
        fail()
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFlatSpec = new TestLocationFixtureFlatSpec
  class StringFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with StringFixture
  class TestLocationFixtureFlatSpec extends StringFixtureFlatSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFixtureFlatSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends StringFixtureFlatSpec {
      "Test" should "info" in { param =>
        info("test info")
      }
    }
    class AbortNestedSuite extends StringFixtureFlatSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends StringFixtureFlatSpec { 
      "Test" should "fail" in { param =>
        fail()
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def freeSpec = new TestLocationFreeSpec
  class TestLocationFreeSpec extends AnyFreeSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFreeSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends AnyFreeSpec {
      "Test" - {
        "should info" in {
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends AnyFreeSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyFreeSpec { 
      "Test" - {
        "should fail" in {
          fail()
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFreeSpec = new TestLocationFixtureFreeSpec
  class StringFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with StringFixture
  class TestLocationFixtureFreeSpec extends StringFixtureFreeSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFixtureFreeSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends StringFixtureFreeSpec {
      "Test" - {
        "should info" in { param =>
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends StringFixtureFreeSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends StringFixtureFreeSpec { 
      "Test" - {
        "should fail" in { param =>
          fail()
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def propSpec = new TestLocationPropSpec
  class TestLocationPropSpec extends AnyPropSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationPropSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends AnyPropSpec {
      property("Test should info") {
        info("test info")
      }
    }
    class AbortNestedSuite extends AnyPropSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyPropSpec { 
      property("Test should fail") {
        fail()
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixturePropSpec = new TestLocationFixturePropSpec
  class StringFixturePropSpec extends propspec.FixtureAnyPropSpec with StringFixture
  class TestLocationFixturePropSpec extends StringFixturePropSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFixturePropSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends StringFixturePropSpec {
      property("Test should info") { param =>
        info("test info")
      }
    }
    class AbortNestedSuite extends StringFixturePropSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends StringFixturePropSpec { 
      property("Test should fail") { param =>
        fail()
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def wordSpec = new TestLocationWordSpec
  class TestLocationWordSpec extends AnyWordSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationWordSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends AnyWordSpec {
      "Test" should {
        "info" in {
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends AnyWordSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends AnyWordSpec { 
      "Test" should {
        "fail" in {
          fail()
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureWordSpec = new TestLocationFixtureWordSpec
  class StringFixtureWordSpec extends wordspec.FixtureAnyWordSpec with StringFixture
  class TestLocationFixtureWordSpec extends StringFixtureWordSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationFixtureWordSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends StringFixtureWordSpec {
      "Test" should {
        "info" in { param =>
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends StringFixtureWordSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends StringFixtureWordSpec { 
      "Test" should {
        "fail" in { param =>
          fail()
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  def spec = new TestLocationSpec
  class TestLocationSpec extends RefSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.DeprecatedLocationSuiteProp$TestLocationSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSpec"),
      TopOfClassPair(suiteTypeName + "$AbortNestedSpec"),
      TopOfClassPair(suiteTypeName + "$FailNestedSpec"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSpec"),
      TopOfClassPair(suiteTypeName + "$FailNestedSpec"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSpec"))
    val expectedTestSucceededList = List(TopOfMethodPair(suiteTypeName + "$NestedSpec$", "public void " + suiteTypeName + "$NestedSpec.test$u0020info()"))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("test fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "DeprecatedLocationSuiteProp.scala", thisLineNumber + 4))

    class NestedSpec extends RefSpec {
      def `test info`: Unit = {
        info("test info")
      }
    }
    class AbortNestedSpec extends RefSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSpec extends RefSpec {
      def `test fail`: Unit = { fail() }
    }
    override def nestedSuites = Vector(new NestedSpec, new AbortNestedSpec, new FailNestedSpec)
  }
  // SKIP-SCALATESTJS,NATIVE-END
}
