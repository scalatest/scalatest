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
package org.scalatest.events.deprecated
import org.scalatest.events._

import org.scalatest._
import SharedHelpers._
import org.scalatest.AllSuiteProp
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END

class DeprecatedScopePendingProp extends AllSuiteProp {

  type FixtureServices = ScopePendingFixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  def suite = new ExampleScopePendingSuite
  def fixtureSuite = new ExampleScopePendingFixtureSuite
  def spec = new ExampleScopePendingSpec
  def junit3Suite = new ExampleScopePendingJUnit3Suite
  def junitSuite = new ExampleScopePendingJUnitSuite
  def testngSuite = new ExampleScopePendingTestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
  def funSuite = new ExampleScopePendingFunSuite
  def fixtureFunSuite = new ExampleScopePendingFixtureFunSuite
  def funSpec = new ExampleScopePendingFunSpec
  def fixtureFunSpec = new ExampleScopePendingFixtureFunSpec
  def featureSpec = new ExampleScopePendingFeatureSpec
  def fixtureFeatureSpec = new ExampleScopePendingFixtureFeatureSpec
  def flatSpec = new ExampleScopePendingFlatSpec
  def fixtureFlatSpec = new ExampleScopePendingFixtureFlatSpec
  def freeSpec = new ExampleScopePendingFreeSpec
  def fixtureFreeSpec = new ExampleScopePendingFixtureFreeSpec
  def propSpec = new ExampleScopePendingPropSpec
  def fixturePropSpec = new ExampleScopePendingFixturePropSpec
  def wordSpec = new ExampleScopePendingWordSpec
  def fixtureWordSpec = new ExampleScopePendingFixtureWordSpec
  def pathFreeSpec = new ExampleScopePendingPathFreeSpec
  def pathFunSpec = new ExampleScopePendingPathFunSpec
  
  test("suites should fire ScopePending event when pending is called at scope level") {
    try {
      forAll(examples.filter(_.supportScope)) { suite =>
        val rep = new EventRecordingReporter
        try {
          suite.run(None, Args(reporter = rep))
        }
        catch {
          case e: exceptions.TestPendingException => 
            fail("TestPendingException should not be thrown here.")
        }
        rep.scopePendingEventsReceived.length should be (1)
        suite.testNames.toSet should be (suite.expectedTestNames)
        suite.expectedTestCount(Filter.default) should be (suite.testNames.size)
      }
    }
    catch {
      case e: exceptions.TestPendingException => 
        fail("TestPendingException should not be thrown here.")
    }
  }
  
}

trait ScopePendingFixtureServices {
  val supportScope = true
  val expectedTestNames: Set[String] = Set.empty
}

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
class ExampleScopePendingSuite extends Suite with ScopePendingFixtureServices {
  override val supportScope = false
}

@DoNotDiscover
class ExampleScopePendingFixtureSuite extends fixture.TestSuite with ScopePendingFixtureServices with StringFixture {
  override val supportScope = false
}

@DoNotDiscover
class ExampleScopePendingSpec extends RefSpec with ScopePendingFixtureServices {
  object `scope 1` {
    def `test 1`: Unit = {}
    def `test 2`: Unit = {}
    def `test 3`: Unit = {}
  }
  
  object `scope 2` {
    def `test 1`: Unit = {}
    pending
    def `test 2`: Unit = {}
    def `test 3`: Unit = {}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 test 1", 
                                                     "scope 1 test 2", 
                                                     "scope 1 test 3")
}

@DoNotDiscover
class ExampleScopePendingJUnit3Suite extends org.scalatestplus.junit.JUnit3Suite with ScopePendingFixtureServices {
  override val supportScope = false
}

@DoNotDiscover
class ExampleScopePendingJUnitSuite extends org.scalatestplus.junit.JUnitSuite with ScopePendingFixtureServices {
  override val supportScope = false
}

@DoNotDiscover
class ExampleScopePendingTestNGSuite extends org.scalatestplus.testng.TestNGSuite with ScopePendingFixtureServices {
  override val supportScope = false
}
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[events] class ExampleScopePendingFunSuite extends FunSuite with ScopePendingFixtureServices {
  override val supportScope = false
}

@DoNotDiscover
protected[events] class ExampleScopePendingFixtureFunSuite extends fixture.FunSuite with ScopePendingFixtureServices with StringFixture {
  override val supportScope = false
}

@DoNotDiscover
protected[events] class ExampleScopePendingFunSpec extends FunSpec with ScopePendingFixtureServices {
  describe("scope 1") {
    it("test 1") {}
    it("test 2") {}
    it("test 3") {}
  }
  describe("scope 2") {
    it("test 1") {}
    pending
    it("test 2") {}
    it("test 3") {}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 test 1", 
                                                     "scope 1 test 2", 
                                                     "scope 1 test 3", 
                                                     "scope 2 test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingFixtureFunSpec extends fixture.FunSpec with ScopePendingFixtureServices with StringFixture {
  describe("scope 1") {
    it("test 1") {s =>}
    it("test 2") {s =>}
    it("test 3") {s =>}
  }
  describe("scope 2") {
    it("test 1") {s =>}
    pending
    it("test 2") {s =>}
    it("test 3") {s =>}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 test 1", 
                                                     "scope 1 test 2", 
                                                     "scope 1 test 3", 
                                                     "scope 2 test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingFeatureSpec extends FeatureSpec with ScopePendingFixtureServices {
  feature("scope 1") {
    scenario("test 1") {}
    scenario("test 2") {}
    scenario("test 3") {}
  }
  feature("scope 2") {
    scenario("test 1") {}
    pending
    scenario("test 2") {}
    scenario("test 3") {}
  }
  
  override val expectedTestNames: Set[String] = Set("Feature: scope 1 Scenario: test 1", 
                                                     "Feature: scope 1 Scenario: test 2", 
                                                     "Feature: scope 1 Scenario: test 3", 
                                                     "Feature: scope 2 Scenario: test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingFixtureFeatureSpec extends fixture.FeatureSpec with ScopePendingFixtureServices with StringFixture {
  feature("scope 1") {
    scenario("test 1") {s =>}
    scenario("test 2") {s =>}
    scenario("test 3") {s =>}
  }
  feature("scope 2") {
    scenario("test 1") {s =>}
    pending
    scenario("test 2") {s =>}
    scenario("test 3") {s =>}
  }
  
  override val expectedTestNames: Set[String] = Set("Feature: scope 1 Scenario: test 1", 
                                                     "Feature: scope 1 Scenario: test 2", 
                                                     "Feature: scope 1 Scenario: test 3", 
                                                     "Feature: scope 2 Scenario: test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingFlatSpec extends FlatSpec with ScopePendingFixtureServices {
  override val supportScope = false
} 

@DoNotDiscover
protected[events] class ExampleScopePendingFixtureFlatSpec extends fixture.FlatSpec with ScopePendingFixtureServices with StringFixture {
  override val supportScope = false
}

@DoNotDiscover
protected[events] class ExampleScopePendingFreeSpec extends FreeSpec with ScopePendingFixtureServices {
  "scope 1" - {
    "test 1" in {}
    "test 2" in {}
    "test 3" in {}
  }
  "scope 2" - {
    "test 1" in {}
    pending
    "test 2" in {}
    "test 3" in {}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 test 1", 
                                                     "scope 1 test 2", 
                                                     "scope 1 test 3", 
                                                     "scope 2 test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingFixtureFreeSpec extends fixture.FreeSpec with ScopePendingFixtureServices with StringFixture {
  "scope 1" - {
    "test 1" in {s =>}
    "test 2" in {s =>}
    "test 3" in {s =>}
  }
  "scope 2" - {
    "test 1" in {s =>}
    pending
    "test 2" in {s =>}
    "test 3" in {s =>}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 test 1", 
                                                     "scope 1 test 2", 
                                                     "scope 1 test 3", 
                                                     "scope 2 test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingPropSpec extends PropSpec with ScopePendingFixtureServices {
  override val supportScope = false
}

@DoNotDiscover
protected[events] class ExampleScopePendingFixturePropSpec extends fixture.PropSpec with ScopePendingFixtureServices with StringFixture {
  override val supportScope = false
} 

@DoNotDiscover
protected[events] class ExampleScopePendingWordSpec extends WordSpec with ScopePendingFixtureServices {
  "scope 1" should {
    "test 1" in {}
    "test 2" in {}
    "test 3" in {}
  }
  "scope 2" should {
    "test 1" in {}
    pending
    "test 2" in {}
    "test 3" in {}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 should test 1", 
                                                     "scope 1 should test 2", 
                                                     "scope 1 should test 3", 
                                                     "scope 2 should test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingFixtureWordSpec extends fixture.WordSpec with ScopePendingFixtureServices with StringFixture {
  "scope 1" should {
    "test 1" in {s =>}
    "test 2" in {s =>}
    "test 3" in {s =>}
  }
  "scope 2" should {
    "test 1" in {s =>}
    pending
    "test 2" in {s =>}
    "test 3" in {s =>}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 should test 1", 
                                                     "scope 1 should test 2", 
                                                     "scope 1 should test 3", 
                                                     "scope 2 should test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingPathFreeSpec extends path.FreeSpec with ScopePendingFixtureServices {
  //SCALATESTJS,NATIVE-ONLY override def newInstance = new ExampleScopePendingPathFreeSpec
  "scope 1" - {
    "test 1" in {}
    "test 2" in {}
    "test 3" in {}
  }
  "scope 2" - {
    "test 1" in {}
    pending
    "test 2" in {}
    "test 3" in {}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 test 1", 
                                                     "scope 1 test 2", 
                                                     "scope 1 test 3", 
                                                     "scope 2 test 1")
}

@DoNotDiscover
protected[events] class ExampleScopePendingPathFunSpec extends path.FunSpec with ScopePendingFixtureServices {
  //SCALATESTJS,NATIVE-ONLY override def newInstance = new ExampleScopePendingPathFunSpec
  describe("scope 1") {
    it("test 1") {}
    it("test 2") {}
    it("test 3") {}
  }
  describe("scope 2") {
    it("test 1") {}
    pending
    it("test 2") {}
    it("test 3") {}
  }
  
  override val expectedTestNames: Set[String] = Set("scope 1 test 1", 
                                                     "scope 1 test 2", 
                                                     "scope 1 test 3", 
                                                     "scope 2 test 1")
}
