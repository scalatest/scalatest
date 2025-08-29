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
package org.scalatest.suiteprop

import org.scalatest._
import prop.TableDrivenPropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.{ freespec, funspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

class DeprecatedInfoInsideTestFiredAfterTestExamples extends org.scalatest.suiteprop.SuiteExamples {

  trait Services {
    val msg = "hi there, dude"
    val theTestName = "test name"
  }

  trait NestedTestName extends Services {
    override val theTestName = "A subject should test name"
  }

  trait DeeplyNestedTestName extends Services {
    override val theTestName = "A subject when created should test name"
  }

  trait NestedTestNameWithMust extends Services {
    override val theTestName = "A subject must test name"
  }

  trait DeeplyNestedTestNameWithMust extends Services {
    override val theTestName = "A subject when created must test name"
  }

  trait NestedTestNameWithCan extends Services {
    override val theTestName = "A subject can test name"
  }

  trait DeeplyNestedTestNameWithCan extends Services {
    override val theTestName = "A subject when created can test name"
  }

  type FixtureServices = Services

  // SKIP-SCALATESTJS,NATIVE-START
  class SpecExample extends RefSpec with Services  {
    def `test method` = {
      info(msg)
    }
    override val theTestName = "testMethod(Informer)"
  }
  // SKIP-SCALATESTJS,NATIVE-END

  class FunSuiteExample extends AnyFunSuite with Services {
    test(theTestName) {
      info(msg)
    }
  }

  class FixtureFunSuiteExample extends StringFixtureFunSuite with Services {
    test(theTestName) { s =>
      info(msg)
    }
  }

  class FunSpecExample extends AnyFunSpec with Services {
    it(theTestName) {
      info(msg)
    }
  }

  class NestedFunSpecExample extends AnyFunSpec with NestedTestName {
    describe("A subject") {
      it("should test name") {
        info(msg)
      }
    }
  }

  class DeeplyNestedFunSpecExample extends AnyFunSpec with DeeplyNestedTestName {
    describe("A subject") {
      describe("when created") {
        it("should test name") {
          info(msg)
        }
      }
    }
  }

  class FixtureFunSpecExample extends StringFixtureFunSpec with Services {
    it(theTestName) { s =>
      info(msg)
    }
  }
  
  class NestedFixtureFunSpecExample extends StringFixtureFunSpec with NestedTestName {
    describe("A subject") {
      it("should test name") { s =>
        info(msg)
      }
    }
  }

  class DeeplyNestedFixtureFunSpecExample extends StringFixtureFunSpec with DeeplyNestedTestName {
    describe("A subject") {
      describe("when created") {
        it("should test name") { s =>
          info(msg)
        }
      }
    }
  }

  class PathFunSpecExample extends funspec.PathAnyFunSpec with Services {
    it(theTestName) {
      info(msg)
    }
    //SCALATESTJS,NATIVE-ONLY override def newInstance = new PathFunSpecExample
  }
    
  class NestedPathFunSpecExample extends funspec.PathAnyFunSpec with NestedTestName {
    describe("A subject") {
      it("should test name") {
        info(msg)
      }
    }
    //SCALATESTJS,NATIVE-ONLY override def newInstance = new NestedPathFunSpecExample
  }

  class DeeplyNestedPathFunSpecExample extends funspec.PathAnyFunSpec with DeeplyNestedTestName {
    describe("A subject") {
      describe("when created") {
        it("should test name") {
          info(msg)
        }
      }
    }
    //SCALATESTJS,NATIVE-ONLY override def newInstance = new DeeplyNestedPathFunSpecExample
  }

  class WordSpecExample extends AnyWordSpec with Services {
    theTestName in {
      info(msg)
    }
  }

  class NestedWordSpecExample extends AnyWordSpec with NestedTestName {
    "A subject" should {
      "test name" in {
        info(msg)
      }
    }
  }

  class DeeplyNestedWordSpecExample extends AnyWordSpec with DeeplyNestedTestName {
    "A subject" when {
      "created" should {
        "test name" in {
          info(msg)
        }
      }
    }
  }

  class FixtureWordSpecExample extends StringFixtureWordSpec with Services {
    theTestName in { s =>
      info(msg)
    }
  }

  class NestedFixtureWordSpecExample extends StringFixtureWordSpec with NestedTestName {
    "A subject" should {
      "test name" in { s =>
        info(msg)
      }
    }
  }

  class DeeplyNestedFixtureWordSpecExample extends StringFixtureWordSpec with DeeplyNestedTestName {
    "A subject" when {
      "created" should {
        "test name" in { s =>
          info(msg)
        }
      }
    }
  }

  class NestedWordSpecWithMustExample extends AnyWordSpec with NestedTestNameWithMust {
    "A subject" must {
      "test name" in {
        info(msg)
      }
    }
  }

  class DeeplyNestedWordSpecWithMustExample extends AnyWordSpec with DeeplyNestedTestNameWithMust {
    "A subject" when {
      "created" must {
        "test name" in {
          info(msg)
        }
      }
    }
  }

  class NestedFixtureWordSpecWithMustExample extends StringFixtureWordSpec with NestedTestNameWithMust {
    "A subject" must {
      "test name" in { s =>
        info(msg)
      }
    }
  }

  class DeeplyNestedFixtureWordSpecWithMustExample extends StringFixtureWordSpec with DeeplyNestedTestNameWithMust {
    "A subject" when {
      "created" must {
        "test name" in { s =>
          info(msg)
        }
      }
    }
  }

  class NestedWordSpecWithCanExample extends AnyWordSpec with NestedTestNameWithCan {
    "A subject" can {
      "test name" in {
        info(msg)
      }
    }
  }

  class DeeplyNestedWordSpecWithCanExample extends AnyWordSpec with DeeplyNestedTestNameWithCan {
    "A subject" when {
      "created" can {
        "test name" in {
          info(msg)
        }
      }
    }
  }

  class NestedFixtureWordSpecWithCanExample extends StringFixtureWordSpec with NestedTestNameWithCan {
    "A subject" can {
      "test name" in { s =>
        info(msg)
      }
    }
  }

  class DeeplyNestedFixtureWordSpecWithCanExample extends StringFixtureWordSpec with DeeplyNestedTestNameWithCan {
    "A subject" when {
      "created" can {
        "test name" in { s =>
          info(msg)
        }
      }
    }
  }

  class FlatSpecExample extends AnyFlatSpec with Services {
    it should "test name" in {
      info(msg)
    }
    override val theTestName = "should test name"
  }

  class SubjectFlatSpecExample extends AnyFlatSpec with NestedTestName {
    behavior of "A subject"
    it should "test name" in {
      info(msg)
    }
  }
  class ShorthandSubjectFlatSpecExample extends AnyFlatSpec with NestedTestName {
    "A subject" should "test name" in {
      info(msg)
    }
  }

  class FixtureFlatSpecExample extends StringFixtureFlatSpec with Services {
    it should "test name" in { s =>
      info(msg)
    }
    override val theTestName = "should test name"
  }

  class SubjectFixtureFlatSpecExample extends StringFixtureFlatSpec with NestedTestName {
    behavior of "A subject"
    it should "test name" in { s =>
      info(msg)
    }
  }
  
  class ShorthandSubjectFixtureFlatSpecExample extends StringFixtureFlatSpec with NestedTestName {
    "A subject" should "test name" in { s =>
      info(msg)
    }
  }

  class FlatSpecWithMustExample extends AnyFlatSpec with Services {
    it must "test name" in {
      info(msg)
    }
    override val theTestName = "must test name"
  }

  class SubjectFlatSpecWithMustExample extends AnyFlatSpec with NestedTestNameWithMust {
    behavior of "A subject"
    it must "test name" in {
      info(msg)
    }
  }
  class ShorthandSubjectFlatSpecWithMustExample extends AnyFlatSpec with NestedTestNameWithMust {
    "A subject" must "test name" in {
      info(msg)
    }
  }

  class FixtureFlatSpecWithMustExample extends StringFixtureFlatSpec with Services {
    it must "test name" in { s =>
      info(msg)
    }
    override val theTestName = "must test name"
  }

  class SubjectFixtureFlatSpecWithMustExample extends StringFixtureFlatSpec with NestedTestNameWithMust {
    behavior of "A subject"
    it must "test name" in { s =>
      info(msg)
    }
  }
  
  class ShorthandSubjectFixtureFlatSpecWithMustExample extends StringFixtureFlatSpec with NestedTestNameWithMust {
    "A subject" must "test name" in { s =>
      info(msg)
    }
  }

  class FlatSpecWithCanExample extends AnyFlatSpec with Services {
    it can "test name" in {
      info(msg)
    }
    override val theTestName = "can test name"
  }

  class SubjectFlatSpecWithCanExample extends AnyFlatSpec with NestedTestNameWithCan {
    behavior of "A subject"
    it can "test name" in {
      info(msg)
    }
  }
  class ShorthandSubjectFlatSpecWithCanExample extends AnyFlatSpec with NestedTestNameWithCan {
    "A subject" can "test name" in {
      info(msg)
    }
  }

  class FixtureFlatSpecWithCanExample extends StringFixtureFlatSpec with Services {
    it can "test name" in { s =>
      info(msg)
    }
    override val theTestName = "can test name"
  }

  class SubjectFixtureFlatSpecWithCanExample extends StringFixtureFlatSpec with NestedTestNameWithCan {
    behavior of "A subject"
    it can "test name" in { s =>
      info(msg)
    }
  }
  
  class ShorthandSubjectFixtureFlatSpecWithCanExample extends StringFixtureFlatSpec with NestedTestNameWithCan {
    "A subject" can "test name" in { s =>
      info(msg)
    }
  }

  class FreeSpecExample extends AnyFreeSpec with Services {
    "test name" in {
      info(msg)
    }
  }

  class NestedFreeSpecExample extends AnyFreeSpec with NestedTestName {
    "A subject" - {
      "should test name" in {
        info(msg)
      }
    }
  }

  class DeeplyNestedFreeSpecExample extends AnyFreeSpec with DeeplyNestedTestName {
    "A subject" - {
      "when created" - {
        "should test name" in {
          info(msg)
        }
      }
    }
  }

  class FixtureFreeSpecExample extends StringFixtureFreeSpec with Services {
    "test name" in { s =>
      info(msg)
    }
  }

  class NestedFixtureFreeSpecExample extends StringFixtureFreeSpec with NestedTestName {
    "A subject" - {
      "should test name" in { s =>
        info(msg)
      }
    }
  }

  class DeeplyNestedFixtureFreeSpecExample extends StringFixtureFreeSpec with DeeplyNestedTestName {
    "A subject" - {
      "when created" - {
        "should test name" in { s =>
          info(msg)
        }
      }
    }
  }

  class PathFreeSpecExample extends freespec.PathAnyFreeSpec with Services {
    "test name" in {
      info(msg)
    }
    //SCALATESTJS,NATIVE-ONLY override def newInstance = new PathFreeSpecExample
  }
    
  class NestedPathFreeSpecExample extends freespec.PathAnyFreeSpec with NestedTestName {
    "A subject" - {
      "should test name" in {
        info(msg)
      }
    }
    //SCALATESTJS,NATIVE-ONLY override def newInstance = new NestedPathFreeSpecExample
  }

  class DeeplyNestedPathFreeSpecExample extends freespec.PathAnyFreeSpec with DeeplyNestedTestName {
    "A subject" - {
      "when created" - {
        "should test name" in {
          info(msg)
        }
      }
    }
    //SCALATESTJS,NATIVE-ONLY override def newInstance = new DeeplyNestedPathFreeSpecExample
  }

  class FeatureSpecExample extends AnyFeatureSpec with Services {
    Scenario("test name") {
      info(msg)
    }
    override val theTestName = "Scenario: test name"
  }

  class NestedFeatureSpecExample extends AnyFeatureSpec with Services {
    Feature("A feature") {
      Scenario("test name") {
        info(msg)
      }
    }
    override val theTestName = "Feature: A feature Scenario: test name"
  }

  class FixtureFeatureSpecExample extends StringFixtureFeatureSpec with Services {
    Scenario("test name") { s =>
      info(msg)
    }
    override val theTestName = "Scenario: test name"
  }

  class NestedFixtureFeatureSpecExample extends StringFixtureFeatureSpec with Services {
    Feature("A feature") {
      Scenario("test name") { s =>
        info(msg)
      }
    }
    override val theTestName = "Feature: A feature Scenario: test name"
  }

  class PropSpecExample extends AnyPropSpec with Services {
    property(theTestName) {
      info(msg)
    }
  }

  class FixturePropSpecExample extends StringFixturePropSpec with Services {
    property(theTestName) { s =>
      info(msg)
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  lazy val spec = new SpecExample
  // SKIP-SCALATESTJS,NATIVE-END
  lazy val funSuite = new FunSuiteExample
  lazy val fixtureFunSuite = new FixtureFunSuiteExample
  lazy val funSpec = new FunSpecExample
  lazy val nestedFunSpec = new NestedFunSpecExample
  lazy val deeplyNestedFunSpec = new DeeplyNestedFunSpecExample
  lazy val fixtureFunSpec = new FixtureFunSpecExample
  lazy val nestedFixtureFunSpec = new NestedFixtureFunSpecExample
  lazy val deeplyNestedFixtureFunSpec = new DeeplyNestedFixtureFunSpecExample
  lazy val pathFunSpec = new PathFunSpecExample
  lazy val nestedPathFunSpec = new NestedPathFunSpecExample
  lazy val deeplyNestedPathFunSpec = new DeeplyNestedPathFunSpecExample

  lazy val wordSpec = new WordSpecExample
  lazy val nestedWordSpec = new NestedWordSpecExample
  lazy val deeplyNestedWordSpec = new DeeplyNestedWordSpecExample
  lazy val fixtureWordSpec = new FixtureWordSpecExample
  lazy val nestedFixtureWordSpec = new NestedFixtureWordSpecExample
  lazy val deeplyNestedFixtureWordSpec = new DeeplyNestedFixtureWordSpecExample

  lazy val nestedWordSpecWithMust = new NestedWordSpecWithMustExample
  lazy val deeplyNestedWordSpecWithMust = new DeeplyNestedWordSpecWithMustExample
  lazy val nestedFixtureWordSpecWithMust = new NestedFixtureWordSpecWithMustExample
  lazy val deeplyNestedFixtureWordSpecWithMust = new DeeplyNestedFixtureWordSpecWithMustExample
  
  lazy val nestedWordSpecWithCan = new NestedWordSpecWithCanExample
  lazy val deeplyNestedWordSpecWithCan = new DeeplyNestedWordSpecWithCanExample
  lazy val nestedFixtureWordSpecWithCan = new NestedFixtureWordSpecWithCanExample
  lazy val deeplyNestedFixtureWordSpecWithCan = new DeeplyNestedFixtureWordSpecWithCanExample

  lazy val flatSpec = new FlatSpecExample
  lazy val subjectFlatSpec = new SubjectFlatSpecExample
  lazy val shorthandSubjectFlatSpec = new ShorthandSubjectFlatSpecExample
  lazy val fixtureFlatSpec = new FixtureFlatSpecExample
  lazy val subjectFixtureFlatSpec = new SubjectFixtureFlatSpecExample
  lazy val shorthandSubjectFixtureFlatSpec = new ShorthandSubjectFixtureFlatSpecExample

  lazy val flatSpecWithMust = new FlatSpecWithMustExample
  lazy val subjectFlatSpecWithMust = new SubjectFlatSpecWithMustExample
  lazy val shorthandSubjectFlatSpecWithMust = new ShorthandSubjectFlatSpecWithMustExample
  lazy val fixtureFlatSpecWithMust = new FixtureFlatSpecWithMustExample
  lazy val subjectFixtureFlatSpecWithMust = new SubjectFixtureFlatSpecWithMustExample
  lazy val shorthandSubjectFixtureFlatSpecWithMust = new ShorthandSubjectFixtureFlatSpecWithMustExample

  lazy val flatSpecWithCan = new FlatSpecWithCanExample
  lazy val subjectFlatSpecWithCan = new SubjectFlatSpecWithCanExample
  lazy val shorthandSubjectFlatSpecWithCan = new ShorthandSubjectFlatSpecWithCanExample
  lazy val fixtureFlatSpecWithCan = new FixtureFlatSpecWithCanExample
  lazy val subjectFixtureFlatSpecWithCan = new SubjectFixtureFlatSpecWithCanExample
  lazy val shorthandSubjectFixtureFlatSpecWithCan = new ShorthandSubjectFixtureFlatSpecWithCanExample

  lazy val freeSpec = new FreeSpecExample
  lazy val nestedFreeSpec = new NestedFreeSpecExample
  lazy val deeplyNestedFreeSpec = new DeeplyNestedFreeSpecExample
  lazy val fixtureFreeSpec = new FixtureFreeSpecExample
  lazy val nestedFixtureFreeSpec = new NestedFixtureFreeSpecExample
  lazy val deeplyNestedFixtureFreeSpec = new DeeplyNestedFixtureFreeSpecExample
  lazy val pathFreeSpec = new PathFreeSpecExample
  lazy val nestedPathFreeSpec = new NestedPathFreeSpecExample
  lazy val deeplyNestedPathFreeSpec = new DeeplyNestedPathFreeSpecExample
  lazy val featureSpec = new FeatureSpecExample
  lazy val nestedFeatureSpec = new NestedFeatureSpecExample
  lazy val fixtureFeatureSpec = new FixtureFeatureSpecExample
  lazy val nestedFixtureFeatureSpec = new NestedFixtureFeatureSpecExample
  lazy val propSpec = new PropSpecExample
  lazy val fixturePropSpec = new FixturePropSpecExample
}

/* TODO: This gave me a "null" in the output. string passed in to should was null for some reason
   class FlatSpecExample extends FlatSpec with Services {
    it should theTestName in {
      info(msg)
    }
    override val theTestName = "should test name"
  }

Got the same thing here:
  class NestedWordSpecExample extends WordSpec with Services {
    "A subject" should {
      theTestName in {
        info(msg)
      }
    }
    override val theTestName = "A subject should test name"
  }
Has to do with initialization order of an overridden val I think. Curious.
 */

