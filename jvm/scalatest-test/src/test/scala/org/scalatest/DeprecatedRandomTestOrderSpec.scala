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
package org.scalatest

import Retries._
import prop.TableDrivenPropertyChecks._
import org.scalatest.tagobjects.Retryable
import scala.annotation.tailrec
import SharedHelpers.EventRecordingReporter
import collection.mutable.ListBuffer

// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class DeprecatedRandomTestOrderSpec extends AnyFunSpec {

  // SKIP-SCALATESTJS,NATIVE-START
  class ExampleSpec(listBuffer: ListBuffer[Int]) extends RefSpec with RandomTestOrder {
    def `test 1`: Unit = {
      listBuffer += 0
    }
    def `test 2`: Unit = {
      listBuffer += 1
    }
    def `test 3`: Unit = {
      listBuffer += 2
    }

    override def newInstance = new ExampleSpec(listBuffer)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  class ExampleFunSuite(listBuffer: ListBuffer[Int]) extends AnyFunSuite with RandomTestOrder {
    test("test 1") {
      listBuffer += 0
    }
    test("test 2") {
      listBuffer += 1
    }
    test("test 3") {
      listBuffer += 2
    }
    override def newInstance = new ExampleFunSuite(listBuffer)
  }

  class ExampleFixtureFunSuite(listBuffer: ListBuffer[Int]) extends funsuite.FixtureAnyFunSuite with StringFixture with RandomTestOrder {
    test("test 1") { fixture =>
      listBuffer += 0
    }
    test("test 2") { fixture =>
      listBuffer += 1
    }
    test("test 3") { fixture =>
      listBuffer += 2
    }
    override def newInstance = new ExampleFixtureFunSuite(listBuffer)
  }

  class ExampleFunSpec(listBuffer: ListBuffer[Int]) extends AnyFunSpec with RandomTestOrder {

    it("test 1") {
      listBuffer += 0
    }

    it("test 2") {
      listBuffer += 1
    }

    it("test 3") {
      listBuffer += 2
    }

    override def newInstance = new ExampleFunSpec(listBuffer)

  }

  class ExampleFixtureFunSpec(listBuffer: ListBuffer[Int]) extends funspec.FixtureAnyFunSpec with StringFixture with RandomTestOrder {

    it("test 1") { fixture =>
      listBuffer += 0
    }

    it("test 2") { fixture =>
      listBuffer += 1
    }

    it("test 3") { fixture =>
      listBuffer += 2
    }

    override def newInstance = new ExampleFixtureFunSpec(listBuffer)

  }

  class ExampleFeatureSpec(listBuffer: ListBuffer[Int]) extends AnyFeatureSpec with RandomTestOrder {
    Feature("Scope 1") {
      Scenario("test 1") {
        listBuffer += 0
      }
      Scenario("test 2") {
        listBuffer += 1
      }
      Scenario("test 3") {
        listBuffer += 2
      }
    }

    override def newInstance = new ExampleFeatureSpec(listBuffer)
  }

  class ExampleFixtureFeatureSpec(listBuffer: ListBuffer[Int]) extends featurespec.FixtureAnyFeatureSpec with StringFixture with RandomTestOrder {
    Feature("Scope 1") {
      Scenario("test 1") { fixture =>
        listBuffer += 0
      }
      Scenario("test 2") { fixture =>
        listBuffer += 1
      }
      Scenario("test 3") { fixture =>
        listBuffer += 2
      }
    }

    override def newInstance = new ExampleFixtureFeatureSpec(listBuffer)
  }

  class ExampleFlatSpec(listBuffer: ListBuffer[Int]) extends AnyFlatSpec with RandomTestOrder  {
    behavior of "Scope 1"
    it should "test 1" in {
      listBuffer += 0
    }
    it should "test 2" in {
      listBuffer += 1
    }
    it should "test 3" in {
      listBuffer += 2
    }

    override def newInstance = new ExampleFlatSpec(listBuffer)
  }

  class ExampleFixtureFlatSpec(listBuffer: ListBuffer[Int]) extends flatspec.FixtureAnyFlatSpec with StringFixture with RandomTestOrder  {
    behavior of "Scope 1"
    it should "test 1" in { fixture =>
      listBuffer += 0
    }
    it should "test 2" in { fixture =>
      listBuffer += 1
    }
    it should "test 3" in { fixture =>
      listBuffer += 2
    }

    override def newInstance = new ExampleFixtureFlatSpec(listBuffer)
  }

  class ExampleFreeSpec(listBuffer: ListBuffer[Int]) extends AnyFreeSpec with RandomTestOrder {
    "Scope 1" - {
      "test 1" in {
        listBuffer += 0
      }
      "test 2" in {
        listBuffer += 1
      }
      "test 3" in {
        listBuffer += 2
      }
    }

    override def newInstance = new ExampleFreeSpec(listBuffer)
  }

  class ExampleFixtureFreeSpec(listBuffer: ListBuffer[Int]) extends freespec.FixtureAnyFreeSpec with StringFixture with RandomTestOrder {
    "Scope 1" - {
      "test 1" in { fixture =>
        listBuffer += 0
      }
      "test 2" in { fixture =>
        listBuffer += 1
      }
      "test 3" in { fixture =>
        listBuffer += 2
      }
    }

    override def newInstance = new ExampleFixtureFreeSpec(listBuffer)
  }

  class ExamplePropSpec(listBuffer: ListBuffer[Int]) extends AnyPropSpec with RandomTestOrder {
    property("test 1") {
      listBuffer += 0
    }
    property("test 2") {
      listBuffer += 1
    }
    property("test 3") {
      listBuffer += 2
    }
    override def newInstance = new ExamplePropSpec(listBuffer)
  }

  class ExampleFixturePropSpec(listBuffer: ListBuffer[Int]) extends propspec.FixtureAnyPropSpec with StringFixture with RandomTestOrder {
    property("test 1") { fixture =>
      listBuffer += 0
    }
    property("test 2") { fixture =>
      listBuffer += 1
    }
    property("test 3") { fixture =>
      listBuffer += 2
    }
    override def newInstance = new ExampleFixturePropSpec(listBuffer)
  }

  class ExampleWordSpec(listBuffer: ListBuffer[Int]) extends AnyWordSpec with RandomTestOrder {
    "Scope 1" should {
      "test 1" in {
        listBuffer += 0
      }
      "test 2" in {
        listBuffer += 1
      }
      "test 3" in {
        listBuffer += 2
      }
    }
    override def newInstance = new ExampleWordSpec(listBuffer)
  }

  class ExampleFixtureWordSpec(listBuffer: ListBuffer[Int]) extends wordspec.FixtureAnyWordSpec with StringFixture with RandomTestOrder {
    "Scope 1" should {
      "test 1" in { fixture =>
        listBuffer += 0
      }
      "test 2" in { fixture =>
        listBuffer += 1
      }
      "test 3" in { fixture =>
        listBuffer += 2
      }
    }
    override def newInstance = new ExampleFixtureWordSpec(listBuffer)
  }

  def examples =
    Table(
      ("suite", "test1Name", "test2Name", "test3Name"),
      // SKIP-SCALATESTJS,NATIVE-START
      ((buffer: ListBuffer[Int]) => new ExampleSpec(buffer), "test 1", "test 2", "test 3"),
      // SKIP-SCALATESTJS,NATIVE-END
      ((buffer: ListBuffer[Int]) => new ExampleFunSuite(buffer), "test 1", "test 2", "test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixtureFunSuite(buffer), "test 1", "test 2", "test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFunSpec(buffer), "test 1", "test 2", "test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixtureFunSpec(buffer), "test 1", "test 2", "test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFeatureSpec(buffer), "Feature: Scope 1 Scenario: test 1", "Feature: Scope 1 Scenario: test 2", "Feature: Scope 1 Scenario: test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixtureFeatureSpec(buffer), "Feature: Scope 1 Scenario: test 1", "Feature: Scope 1 Scenario: test 2", "Feature: Scope 1 Scenario: test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFlatSpec(buffer), "Scope 1 should test 1", "Scope 1 should test 2", "Scope 1 should test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixtureFlatSpec(buffer), "Scope 1 should test 1", "Scope 1 should test 2", "Scope 1 should test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFlatSpec(buffer), "Scope 1 should test 1", "Scope 1 should test 2", "Scope 1 should test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixtureFlatSpec(buffer), "Scope 1 should test 1", "Scope 1 should test 2", "Scope 1 should test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFreeSpec(buffer), "Scope 1 test 1", "Scope 1 test 2", "Scope 1 test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixtureFreeSpec(buffer), "Scope 1 test 1", "Scope 1 test 2", "Scope 1 test 3"),
      ((buffer: ListBuffer[Int]) => new ExamplePropSpec(buffer), "test 1", "test 2", "test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixturePropSpec(buffer), "test 1", "test 2", "test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleWordSpec(buffer), "Scope 1 should test 1", "Scope 1 should test 2", "Scope 1 should test 3"),
      ((buffer: ListBuffer[Int]) => new ExampleFixtureWordSpec(buffer), "Scope 1 should test 1", "Scope 1 should test 2", "Scope 1 should test 3")
    )

  override def withFixture(test: NoArgTest) = {
    if (isRetryable(test))
      withRetry { super.withFixture(test) }
    else
      super.withFixture(test)
  }

  describe("RandomTestOrder ") {

    it("execute tests in random order, but fire events in original order", Retryable) {
      forAll(examples) { case (specFun, test1Name, test2Name, test3Name) =>

        @tailrec
        def doUntilOutOfOrder(count: Int = 0): EventRecordingReporter = {
          val buffer = new ListBuffer[Int]
          val spec = specFun(buffer)
          val rep = new EventRecordingReporter
          spec.run(None, Args(reporter = rep))
          if (buffer(0) != 0 || buffer(1) != 1 || buffer(2) != 2)
            rep
          else {
            if (count < 100)
              doUntilOutOfOrder(count + 1)
            else
              fail("Tried 100 times but the order is still not shuffled, it probably never will.")
          }
        }

        val rep = doUntilOutOfOrder()

        val testStartingList = rep.testStartingEventsReceived
        assert(testStartingList.size == 3)
        assert(testStartingList(0).testName == test1Name)
        assert(testStartingList(1).testName == test2Name)
        assert(testStartingList(2).testName == test3Name)

        val testSucceededList = rep.testSucceededEventsReceived
        assert(testSucceededList.size == 3)
        assert(testSucceededList(0).testName == test1Name)
        assert(testSucceededList(1).testName == test2Name)
        assert(testSucceededList(2).testName == test3Name)
      }
    }

  }

}
