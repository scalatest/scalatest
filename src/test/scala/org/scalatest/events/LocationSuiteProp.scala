package org.scalatest.events
import org.scalatest.junit.JUnit3Suite
import org.scalatest._

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
  
  def suite = new TestLocationSuite
  class TestLocationSuite extends Suite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationSuite"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(TopOfMethodPair(suiteTypeName + "$NestedSuite", "public void " + suiteTypeName + "$NestedSuite.testInfo(org.scalatest.Informer)"))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("testFail"))
    val expectedInfoProvidedList = List(LineInFilePair("testInfo", "LocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends Suite {
      def testInfo(info: Informer) {
        info("testInfo")
      }
    }
    class AbortNestedSuite extends Suite {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends Suite {
      def testFail() { fail }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def fixtureSuite = new TestLocationFixtureSuite
  class StringFixtureSuite extends fixture.Suite with StringFixture
  class TestLocationFixtureSuite extends StringFixtureSuite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureSuite"
    val expectedSuiteStartingList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"), 
                                         TopOfClassPair(suiteTypeName + "$AbortNestedSuite"),
                                         TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteCompletedList = List(TopOfClassPair(suiteTypeName + "$NestedSuite"),
                                          TopOfClassPair(suiteTypeName + "$FailNestedSuite"))
    val expectedSuiteAbortedList = List(SeeStackDepthExceptionPair(suiteTypeName + "$AbortNestedSuite"))
    val expectedTestSucceededList = List(TopOfMethodPair(suiteTypeName + "$NestedSuite", "public void " + suiteTypeName + "$NestedSuite.testInfo(org.scalatest.Informer)"))
    val expectedTestFailedList = List(SeeStackDepthExceptionPair("testFail"))
    val expectedInfoProvidedList = List(LineInFilePair("testInfo", "LocationSuiteProp.scala", thisLineNumber + 4))
    
    class NestedSuite extends StringFixtureSuite {
      def testInfo(info: Informer) { 
        info("testInfo")
      }
    }
    class AbortNestedSuite extends StringFixtureSuite {
      override protected def runNestedSuites(args: Args): Status = {
        throw new RuntimeException
      }
    }
    class FailNestedSuite extends StringFixtureSuite {
      def testFail() { fail }
    }
    override def nestedSuites = Vector(new NestedSuite, new AbortNestedSuite, new FailNestedSuite)
  }
  
  def junit3Suite = new TestLocationJUnit3Suite
  
  def junitSuite = new TestLocationJUnitSuite
  
  def testngSuite = new TestLocationTestNGSuite
  
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
      test("fail") { fail }
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
  
  def fixtureSpec = new TestLocationFixtureSpec
  class TestLocationFixtureSpec extends StringFixtureFunSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationSuiteProp$TestLocationFixtureSpec"
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
          fail
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
}
