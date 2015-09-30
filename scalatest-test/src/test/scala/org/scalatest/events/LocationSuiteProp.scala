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
// SKIP-SCALATESTJS-START
import org.scalatest.junit.JUnit3Suite
// SKIP-SCALATESTJS-END
import org.scalatest._
import SharedHelpers._

class LocationSuiteProp extends SuiteProp
{
  test("All suite types should have correct location in SuiteStarting, SuiteCompleted, SuiteAborted and TestFailed event.") {
    forAll(examples) { suite =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      val eventList = reporter.eventsReceived
      eventList.foreach { event => suite.checkFun(event) }
      suite.allChecked
    }
  }
  
  type FixtureServices = TestLocationServices

  // SKIP-SCALATESTJS-START
  def junit3Suite = new TestLocationJUnit3Suite
  
  def junitSuite = new TestLocationJUnitSuite
  
  def testngSuite = new TestLocationTestNGSuite
  // SKIP-SCALATESTJS-END
  
  def funSuite = new TestLocationFunSuite
  class TestLocationFunSuite extends FunSuite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFunSuite"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends FunSuite {
      test("info") {
        info("test info")
      }
    }
    class AbortNestedSuite extends FunSuite {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends FunSuite {
      test("fail") { fail }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFunSuite = new TestLocationFixtureFunSuite
  class TestLocationFixtureFunSuite extends StringFixtureFunSuite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureFunSuite"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
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
      test("fail") { () => fail }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  } 
  
  def funSpec = new LocationTestSpec
  class LocationTestSpec extends FunSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$LocationTestSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends FunSpec {
      it("info") {
        info("test info")
      }
    }
    class AbortNestedSuite extends FunSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends FunSpec {
      it("fail") { fail }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFunSpec = new TestLocationFixtureFunSpec
  class TestLocationFixtureFunSpec extends StringFixtureFunSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureFunSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
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
    class FailNestedSuite extends FunSpec {
      it("fail") { fail }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def featureSpec = new TestLocationFeatureSpec
  class TestLocationFeatureSpec extends FeatureSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFeatureSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Feature: feature Scenario: info", "LocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Feature: feature Scenario: fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends FeatureSpec {
      feature("feature") {
        scenario("info") {
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends FeatureSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends FeatureSpec {
      feature("feature") { 
        scenario("fail") {
          fail
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFeatureSpec = new TestLocationFixtureFeatureSpec
  class StringFixtureFeatureSpec extends fixture.FeatureSpec with StringFixture
  class TestLocationFixtureFeatureSpec extends StringFixtureFeatureSpec with FixtureServices { 
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureFeatureSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Feature: feature Scenario: info", "LocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Feature: feature Scenario: fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends StringFixtureFeatureSpec {
      feature("feature") { 
        scenario("info") { param =>
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
      feature("feature") { 
        scenario("fail") {
          () => fail
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def flatSpec = new TestLocationFlatSpec
  class TestLocationFlatSpec extends FlatSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFlatSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends FlatSpec {
      "Test" should "info" in {
        info("test info")
      }
    }
    class AbortNestedSuite extends FlatSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends FlatSpec {
      "Test" should "fail" in {
        fail
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFlatSpec = new TestLocationFixtureFlatSpec
  class StringFixtureFlatSpec extends fixture.FlatSpec with StringFixture
  class TestLocationFixtureFlatSpec extends StringFixtureFlatSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureFlatSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
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
        fail
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def freeSpec = new TestLocationFreeSpec
  class TestLocationFreeSpec extends FreeSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFreeSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends FreeSpec {
      "Test" - {
        "should info" in {
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends FreeSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends FreeSpec { 
      "Test" - {
        "should fail" in {
          fail
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureFreeSpec = new TestLocationFixtureFreeSpec
  class StringFixtureFreeSpec extends fixture.FreeSpec with StringFixture
  class TestLocationFixtureFreeSpec extends StringFixtureFreeSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureFreeSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 5))
    
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
          fail
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def propSpec = new TestLocationPropSpec
  class TestLocationPropSpec extends PropSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationPropSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends PropSpec {
      property("Test should info") {
        info("test info")
      }
    }
    class AbortNestedSuite extends PropSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends PropSpec { 
      property("Test should fail") {
        fail
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixturePropSpec = new TestLocationFixturePropSpec
  class StringFixturePropSpec extends fixture.PropSpec with StringFixture
  class TestLocationFixturePropSpec extends StringFixturePropSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixturePropSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 5))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))
    
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
        fail
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def wordSpec = new TestLocationWordSpec
  class TestLocationWordSpec extends WordSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationWordSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 5))
    
    class NestedSuite extends WordSpec {
      "Test" should {
        "info" in {
          info("test info")
        }
      }
    }
    class AbortNestedSuite extends WordSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends WordSpec { 
      "Test" should {
        "fail" in {
          fail
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureWordSpec = new TestLocationFixtureWordSpec
  class StringFixtureWordSpec extends fixture.WordSpec with StringFixture
  class TestLocationFixtureWordSpec extends StringFixtureWordSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureWordSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(LineInFilePair("Test should info", "LocationSuiteProp.scala", thisLineNumber + 6))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("Test should fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 5))
    
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
          fail
        }
      }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }

  // SKIP-SCALATESTJS-START
  def spec = new TestLocationSpec
  class TestLocationSpec extends Spec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSpec"),
      TopOfClassPair(suiteTypeName + "$AbortNestedSpec"),
      TopOfClassPair(suiteTypeName + "$FailNestedSpec"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSpec"),
      TopOfClassPair(suiteTypeName + "$FailNestedSpec"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSpec"))
    val expectedTestSucceededList = List(TopOfMethodPair(suiteTypeName + "$NestedSpec$", "public void " + suiteTypeName + "$NestedSpec.test$u0020info()"))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("test fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))

    class NestedSpec extends Spec {
      def `test info` {
        info("test info")
      }
    }
    class AbortNestedSpec extends Spec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSpec extends Spec {
      def `test fail` { fail }
    }
    override def nestedSuites = Vector(new NestedSpec, new AbortNestedSpec, new FailNestedSpec)
  }

  def fixtureSpec = new TestLocationFixtureSpec
  class StringFixtureSpec extends fixture.Spec with StringFixture
  class TestLocationFixtureSpec extends StringFixtureSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureSpec"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSpec"),
      TopOfClassPair(suiteTypeName + "$AbortNestedSpec"),
      TopOfClassPair(suiteTypeName + "$FailNestedSpec"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSpec"),
      TopOfClassPair(suiteTypeName + "$FailNestedSpec"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSpec"))
    val expectedTestSucceededList = List(TopOfMethodPair(suiteTypeName + "$NestedSpec$", "public void " + suiteTypeName + "$NestedSpec.test$u0020info()"))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("test fail"))
    val expectedInfoProvidedList = List(LineInFilePair("test info", "LocationSuiteProp.scala", thisLineNumber + 4))

    class NestedSpec extends StringFixtureSpec {
      def `test info` {
        info("test info")
      }
    }
    class AbortNestedSpec extends StringFixtureSpec {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSpec extends StringFixtureSpec {
      def `test fail` { fail }
    }
    override def nestedSuites = Vector(new NestedSpec, new AbortNestedSpec, new FailNestedSpec)
  }
  // SKIP-SCALATESTJS-END
}
