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
package org.scalatest

import scala.collection.immutable.TreeSet
import org.scalatest.events._
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import java.awt.AWTError
import SharedHelpers._
import tools.Runner.CHOSEN_STYLES
import org.scalatest.Suite.formatterForSuiteStarting
import PrivateMethodTester._
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestFailedException

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.NotAllowedException
*/

class SuiteSuite extends Spec with SeveredStackTraces {

  def `test prettify Array` = {

    // non arrays print just a toString
    assert(FailureMessages.decorateToStringValue(1) === "1")
    assert(FailureMessages.decorateToStringValue("hi") === "\"hi\"")
    assert(FailureMessages.decorateToStringValue(List(1, 2, 3)) === "List(1, 2, 3)")
    assert(FailureMessages.decorateToStringValue(Map("one" -> 1)) === "Map(\"one\" -> 1)")

    // arrays print pretty
    assert(FailureMessages.decorateToStringValue(Array(1, 2)) === "Array(1, 2)")

    // arrays of arrays print pretty
    assert(FailureMessages.decorateToStringValue(Array(Array(1, 2), Array(3, 4))) === "Array(Array(1, 2), Array(3, 4))")
  }

  def `test: execute should use dynamic tagging to enable Doenitz wildcards for encoded test names` {

    class TestWasCalledSpec extends Spec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      var theTestTheOtherCalled = false
      var theTestThisConfigMapWasEmpty = true
      var theTestThatConfigMapWasEmpty = true
      var theTestTheOtherConfigMapWasEmpty = true
      override def withFixture(test: NoArgTest): Outcome = {
        if (test.configMap.size > 0)
          test.name match {
            case "test this" => theTestThisConfigMapWasEmpty = false
            case "test that" => theTestThatConfigMapWasEmpty = false
            case "test the other" => theTestTheOtherConfigMapWasEmpty = false
            case _ => throw new Exception("Should never happen")
          }
        test()
      }
      def `test this` = { theTestThisCalled = true }
      def `test that` = { theTestThatCalled = true }
      def `test the other` = { theTestTheOtherCalled = true }
    }

    val s1 = new TestWasCalledSpec
    s1.execute(" th")
    assert(s1.theTestThisCalled)
    assert(s1.theTestThatCalled)
    assert(s1.theTestTheOtherCalled)
    assert(s1.theTestThisConfigMapWasEmpty)
    assert(s1.theTestThatConfigMapWasEmpty)
    assert(s1.theTestTheOtherConfigMapWasEmpty)

    val s2 = new TestWasCalledSpec
    s2.execute(" this")
    assert(s2.theTestThisCalled)
    assert(!s2.theTestThatCalled)
    assert(!s2.theTestTheOtherCalled)
    assert(s2.theTestThisConfigMapWasEmpty)
    assert(s2.theTestThatConfigMapWasEmpty)
    assert(s2.theTestTheOtherConfigMapWasEmpty)

    val s3 = new TestWasCalledSpec
    s3.execute(" th", configMap = ConfigMap("s" -> "s"))
    assert(s3.theTestThisCalled)
    assert(s3.theTestThatCalled)
    assert(s3.theTestTheOtherCalled)
    assert(!s3.theTestThisConfigMapWasEmpty)
    assert(!s3.theTestThatConfigMapWasEmpty)
    assert(!s3.theTestTheOtherConfigMapWasEmpty)

    val s4 = new TestWasCalledSpec
    s4.execute(" th", ConfigMap("s" -> "s"))
    assert(s4.theTestThisCalled)
    assert(s4.theTestThatCalled)
    assert(s4.theTestTheOtherCalled)
    assert(!s4.theTestThisConfigMapWasEmpty)
    assert(!s4.theTestThatConfigMapWasEmpty)
    assert(!s4.theTestTheOtherConfigMapWasEmpty)

    val s5 = new TestWasCalledSpec
    s5.execute(testName = " th")
    assert(s5.theTestThisCalled)
    assert(s5.theTestThatCalled)
    assert(s5.theTestTheOtherCalled)
    assert(s5.theTestThisConfigMapWasEmpty)
    assert(s5.theTestThatConfigMapWasEmpty)
    assert(s5.theTestTheOtherConfigMapWasEmpty)

    val s6 = new TestWasCalledSpec
    s6.execute(testName = " this", configMap = ConfigMap("s" -> "s"))
    assert(s6.theTestThisCalled)
    assert(!s6.theTestThatCalled)
    assert(!s6.theTestTheOtherCalled)
    assert(!s6.theTestThisConfigMapWasEmpty)
    assert(s6.theTestThatConfigMapWasEmpty)
    assert(s6.theTestTheOtherConfigMapWasEmpty)
  }

  def `test test tags` = {
    class TagSpec extends Spec {  
      def `test no tag method` = {}
      @SlowAsMolasses
      def `test tag method` = {}
    }
    val testTags = new TagSpec().tags
    assert(testTags.size === 1)
    val tagSet = testTags.getOrElse("test tag method", null)
    assert(tagSet != null)
    assert(tagSet.size === 1)
    assert(tagSet.toList(0) === classOf[SlowAsMolasses].getName)
  }
  
  def `test runNestedSuites` = {
    
    class NoTagSpec extends Spec
    @Ignore
    class IgnoreSpec extends Spec {
      def `test method 1` = {}
      def `test method 2` = {}
      def `test method 3` = {}
    }
    @SlowAsMolasses
    class SlowAsMolassesSpec extends Spec
    @FastAsLight
    class FastAsLightSpec extends Spec
    
    class MasterSpec extends Spec {
      override def nestedSuites = Vector(new NoTagSpec(), new IgnoreSpec(), new SlowAsMolassesSpec(), new FastAsLightSpec())
      override def runNestedSuites(args: Args): Status = {
        super.runNestedSuites(args)
      }
    }
    
    class CounterDistributor extends Distributor {
      var count = 0
      def apply(suite: Suite, args: Args): Status = {
        count += 1
        SucceededStatus
      }
      def apply(suite: Suite, tracker: Tracker) {
        count += 1
      }
    }
    
    val masterSuite = new MasterSpec()
    
    val defaultFilter = Filter(None, Set.empty)
    val defaultReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(Args(defaultReporter, Stopper.default, defaultFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(defaultReporter.suiteStartingEventsReceived.size === 4)
    assert(defaultReporter.testIgnoredEventsReceived.size === 3)
    val defaultReporterDist = new EventRecordingReporter
    val defaultDistributor = new CounterDistributor
    masterSuite.runNestedSuites(Args(defaultReporterDist, Stopper.default, defaultFilter, ConfigMap.empty, Some(defaultDistributor), new Tracker(new Ordinal(99)), Set.empty))
    assert(defaultDistributor.count === 4)
    
    val includeFilter = Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)
    val includeReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(Args(includeReporter, Stopper.default, includeFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(includeReporter.suiteStartingEventsReceived.size === 4) 
    assert(includeReporter.testIgnoredEventsReceived.size === 0) 
    val includeReporterDist = new EventRecordingReporter
    val includeDistributor = new CounterDistributor
    masterSuite.runNestedSuites(Args(includeReporterDist, Stopper.default, includeFilter, ConfigMap.empty, Some(includeDistributor), new Tracker(new Ordinal(99)), Set.empty))
    assert(includeDistributor.count === 4) 
    
    val excludeFilter = Filter(None, Set("org.scalatest.SlowAsMolasses"))
    val excludeReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(Args(excludeReporter, Stopper.default, excludeFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(excludeReporter.suiteStartingEventsReceived.size === 4)
    assert(excludeReporter.testIgnoredEventsReceived.size === 3)
    val excludeReporterDist = new EventRecordingReporter
    val excludeDistributor = new CounterDistributor
    masterSuite.runNestedSuites(Args(excludeReporterDist, Stopper.default, excludeFilter, ConfigMap.empty, Some(excludeDistributor), new Tracker(new Ordinal(99)), Set.empty))
    assert(excludeDistributor.count === 4)
  }
  
  def `test expectedTestCount` = {
    class NoTagSpec extends Spec {
      def `test method 1` = {}
      def `test method 2` = {}
      def `test method 3` = {}
    }
    @Ignore
    class IgnoreSpec extends Spec {
      def `test method 1` = {}
      def `test method 2` = {}
      def `test method 3` = {}
    }
    @SlowAsMolasses
    class SlowAsMolassesSpec extends Spec {
      def `test method 1` = {}
      def `test method 2` = {}
      def `test method 3` = {}
    }
    @FastAsLight
    class FastAsLightSpec extends Spec {
      def `test method 1` = {}
      def `test method 2` = {}
      def `test method 3` = {}
    }
    
    class MasterSpec extends Spec {
      override def nestedSuites = Vector(new NoTagSpec(), new IgnoreSpec(), new SlowAsMolassesSpec(), new FastAsLightSpec())
      override def runNestedSuites(args: Args): Status = {
        super.runNestedSuites(args)
      }
    }
    
    val masterSuite = new MasterSpec()
    assert(masterSuite.expectedTestCount(Filter(None, Set.empty)) === 9)
    assert(masterSuite.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)) === 3)
    assert(masterSuite.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 6)
    assert(masterSuite.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set.empty)) === 3)
    assert(masterSuite.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 6)
  }
  
  def `test SuiteRunner` = {
    assert(new NormalSuite().rerunner.get === classOf[NormalSuite].getName)
    assert(new WrappedSuite(Map.empty).rerunner.get === classOf[WrappedSuite].getName)
    assert(new NotAccessibleSuite("test").rerunner === None)
  }
  
  def `test check chosenStyles` = {
    class SimpleSpec extends Spec {
      def `test method 1` = {}
      def `test method 2` = {}
      def `test method 3` = {}
    }
    
    val simpleSuite = new SimpleSpec()
    simpleSuite.run(None, Args(SilentReporter))
    simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.Spec")), None, new Tracker, Set.empty))
    val caught =
      intercept[NotAllowedException] {
        simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSpec")), None, new Tracker, Set.empty))
      }
    import OptionValues._
    assert(caught.message.value === Resources("notTheChosenStyle", "org.scalatest.Spec", "org.scalatest.FunSpec"))
    val caught2 =
      intercept[NotAllowedException] {
        simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSpec", "org.scalatest.FreeSpec")), None, new Tracker, Set.empty))
      }
    assert(caught2.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.Spec", Suite.makeListForHumans(Vector("org.scalatest.FunSpec", "org.scalatest.FreeSpec"))))
    val caught3 =
      intercept[NotAllowedException] {
        simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSpec", "org.scalatest.FreeSpec", "org.scalatest.FlatSpec")), None, new Tracker, Set.empty))
      }
    assert(caught3.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.Spec", Suite.makeListForHumans(Vector("org.scalatest.FunSpec", "org.scalatest.FreeSpec", "org.scalatest.FlatSpec"))))
  }

  def `test make list for humans` = {
    assert(Suite.makeListForHumans(Vector.empty) === "<empty list>")
    assert(Suite.makeListForHumans(Vector("")) === "\"\"")
    assert(Suite.makeListForHumans(Vector("   ")) === "\"   \"")
    assert(Suite.makeListForHumans(Vector("FunSuite FunSpec")) === "\"FunSuite FunSpec\"")
    assert(Suite.makeListForHumans(Vector("hi")) === "hi")
    assert(Suite.makeListForHumans(Vector("ho")) === "ho")
    assert(Suite.makeListForHumans(Vector("hi", "ho")) === Resources("leftAndRight", "hi", "ho"))
    assert(Suite.makeListForHumans(Vector("fee", "fie", "foe", "fum")) === "fee, fie, " + Resources("leftCommaAndRight", "foe", "fum"))
    assert(Suite.makeListForHumans(Vector("A", "stitch", "in", "time", "saves", "nine")) === "A, stitch, in, time, " + Resources("leftCommaAndRight", "saves", "nine"))
    assert(Suite.makeListForHumans(Vector("fee ", "fie", " foe", "fum")) === "\"fee \", fie, " + Resources("leftCommaAndRight", "\" foe\"", "fum"))
  }

  def `test stack depth` = {
    class TestSpec extends Spec {
      def `test failure` = {
        assert(1 === 2)
      }
    }
    val rep = new EventRecordingReporter
    val s1 = new TestSpec
    s1.run(None, Args(rep))
    assert(rep.testFailedEventsReceived.size === 1)
    assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "SuiteSuite.scala")
    assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
  }

  def `test an error that should cause an abort` = {
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new AnnotationFormatError("oops")) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new AWTError("ouch")) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new CoderMalfunctionError(new Exception)) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new FactoryConfigurationError) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new LinkageError) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new ThreadDeath) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new TransformerFactoryConfigurationError) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new VirtualMachineError {}) }
    assertResult(true) { Suite.anExceptionThatShouldCauseAnAbort(new OutOfMemoryError) }
    assertResult(false) { Suite.anExceptionThatShouldCauseAnAbort(new AssertionError) }
    assertResult(false) { Suite.anExceptionThatShouldCauseAnAbort(new RuntimeException) }
  }

  //
  // Verify that Suites that don't contain any tests but do
  // contain nested Suites get a MotionToSuppress formatter for
  // SuiteStarting events.
  //
  def `test formatter for SuiteStarting` = {
    val emptySuite = new Spec {}

    val emptySuiteContainingNestedSuites =
      new Suites(emptySuite, new NormalSuite)

    val nonEmptySuite = new Spec { def `test foo` = {} }

    val nonEmptySuiteContainingNestedSuites =
      new Suites(emptySuite, new NormalSuite) with SpecLike
      {
        def `test foo` = {}
      }

    assert(
      formatterForSuiteStarting(emptySuite) !== 
        Some(MotionToSuppress))

    assert(
      formatterForSuiteStarting(emptySuiteContainingNestedSuites) ===
        Some(MotionToSuppress))

    assert(
      formatterForSuiteStarting(nonEmptySuite) !== 
        Some(MotionToSuppress))

    assert(
      formatterForSuiteStarting(nonEmptySuiteContainingNestedSuites) !==
        Some(MotionToSuppress))
  }
}

@DoNotDiscover
class `My Test` extends Spec {}
@DoNotDiscover
class NormalSuite extends Spec
@DoNotDiscover
@WrapWith(classOf[ConfigMapWrapperSuite]) 
class WrappedSuite(configMap: Map[_, _]) extends Spec
@DoNotDiscover
class NotAccessibleSuite(name: String) extends Spec
