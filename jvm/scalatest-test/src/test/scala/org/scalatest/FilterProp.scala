/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalatest.events.Ordinal
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatestplus.junit.JUnit3Suite
import org.scalatestplus.junit.JUnitSuite
import org.scalatest.refspec.RefSpec
import org.scalatestplus.testng.TestNGSuite
// SKIP-SCALATESTJS,NATIVE-END
import SharedHelpers._
import org.scalatest
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class FilterProp extends SuiteProp {
  
  test("All suite types should not run nested suite when Filters's excludeNestedSuites contains the suiteId.") {
    forAll(examples) { suite =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(None, Set[String](), true), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      // SKIP-SCALATESTJS,NATIVE-START
      if (!suite.isInstanceOf[TestNGSuite])
      // SKIP-SCALATESTJS,NATIVE-END
        reporter.suiteStartingEventsReceived.length should be (0)
      // SKIP-SCALATESTJS,NATIVE-START
      else Succeeded
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
  
  type FixtureServices = AnyRef

  // SKIP-SCALATESTJS,NATIVE-START
  def suite = new Suite {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixtureSuite = new scalatest.FixtureTestSuite with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
  def junit3Suite = new JUnit3Suite {
    override def nestedSuites = Vector(new Suite {})
  }
  def junitSuite = new JUnitSuite {
    override def nestedSuites = Vector(new Suite {})
  }
  def testngSuite = new TestNGSuite {
    override def nestedSuites = Vector(new Suite {})
  }
  def spec = new RefSpec {
    override def nestedSuites = Vector(new Suite {})
  }
  // SKIP-SCALATESTJS,NATIVE-END
  def funSuite = new AnyFunSuite {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixtureFunSuite = new funsuite.FixtureAnyFunSuite with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
  def funSpec = new AnyFunSpec {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixtureFunSpec = new funspec.FixtureAnyFunSpec with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
  def featureSpec = new AnyFeatureSpec {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixtureFeatureSpec = new featurespec.FixtureAnyFeatureSpec with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
  def flatSpec = new AnyFlatSpec {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixtureFlatSpec = new flatspec.FixtureAnyFlatSpec with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
  def freeSpec = new AnyFreeSpec {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixtureFreeSpec = new freespec.FixtureAnyFreeSpec with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
  def propSpec = new AnyPropSpec {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixturePropSpec = new propspec.FixtureAnyPropSpec with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
  def wordSpec = new AnyWordSpec {
    override def nestedSuites = Vector(new Suite {})
  }
  def fixtureWordSpec = new wordspec.FixtureAnyWordSpec with StringFixture {
    override def nestedSuites = Vector(new Suite {})
  }
}
