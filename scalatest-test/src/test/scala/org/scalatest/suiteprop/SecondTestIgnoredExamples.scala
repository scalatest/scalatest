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
package org.scalatest.suiteprop

import org.scalatest._

class SecondTestIgnoredExamples extends SuiteExamples {

  trait Services {
    val theTestNames = Vector("first test", "second test")
  }

  trait NestedTestNames extends Services {
    override val theTestNames = Vector("A subject should first test", "A subject should second test")
  }

  trait DeeplyNestedTestNames extends Services {
    override val theTestNames = Vector("A subject when created should first test", "A subject when created should second test")
  }

  trait NestedTestNamesWithMust extends Services {
    override val theTestNames = Vector("A subject must first test", "A subject must second test")
  }

  trait DeeplyNestedTestNamesWithMust extends Services {
    override val theTestNames = Vector("A subject when created must first test", "A subject when created must second test")
  }

  trait NestedTestNamesWithCan extends Services {
    override val theTestNames = Vector("A subject can first test", "A subject can second test")
  }

  trait DeeplyNestedTestNamesWithCan extends Services {
    override val theTestNames = Vector("A subject when created can first test", "A subject when created can second test")
  }

  type FixtureServices = Services

  // SKIP-SCALATESTJS-START
  class SpecExample extends Spec with Services {
    def `test first` = {}
    @Ignore def `test second` = {}
    override val theTestNames = Vector("test first", "test second")
  }

  class FixtureSpecExample extends StringFixtureSpec with Services {
    def `test first`(s: String) = {}
    @Ignore def `test second`(s: String) = {}
    override val theTestNames = Vector("test first", "test second")
  }
  // SKIP-SCALATESTJS-END

  class FunSuiteExample extends FunSuite with Services {
    test("first test") { succeed }
    ignore("second test") { succeed }
  }

  class FixtureFunSuiteExample extends StringFixtureFunSuite with Services {
    test("first test") { s => succeed }
    ignore("second test") { s => succeed }
  }

  class FunSpecExample extends FunSpec with Services {
    it("first test") { succeed }
    ignore("second test") { succeed }
  }

  class NestedFunSpecExample extends FunSpec with NestedTestNames {
    describe("A subject") {
      it("should first test") { succeed }
      ignore("should second test") { succeed }
    }
  }

  class DeeplyNestedFunSpecExample extends FunSpec with DeeplyNestedTestNames {
    describe("A subject") {
      describe("when created") {
        it("should first test") { succeed }
        ignore("should second test") { succeed }
      }
    }
  }

  class FixtureFunSpecExample extends StringFixtureFunSpec with Services {
      it("first test") { s => succeed }
      ignore("second test") { s => succeed }
  }
  
  class NestedFixtureFunSpecExample extends StringFixtureFunSpec with NestedTestNames {
    describe("A subject") {
      it("should first test") { s => succeed }
      ignore("should second test") { s => succeed }
    }
  }

  class DeeplyNestedFixtureFunSpecExample extends StringFixtureFunSpec with DeeplyNestedTestNames {
    describe("A subject") {
      describe("when created") {
        it("should first test") { s => succeed }
        ignore("should second test") { s => succeed }
      }
    }
  }

  class PathFunSpecExample extends path.FunSpec with Services {
    it("first test") { succeed }
    ignore("second test") { succeed }
    override def newInstance = new PathFunSpecExample
  }

  class NestedPathFunSpecExample extends path.FunSpec with NestedTestNames {
    describe("A subject") {
      it("should first test") { succeed }
      ignore("should second test") { succeed }
    }
    override def newInstance = new NestedPathFunSpecExample
  }

  class DeeplyNestedPathFunSpecExample extends path.FunSpec with DeeplyNestedTestNames {
    describe("A subject") {
      describe("when created") {
        it("should first test") { succeed }
        ignore("should second test") { succeed }
      }
    }
    override def newInstance = new DeeplyNestedPathFunSpecExample
  }

  class WordSpecExample extends WordSpec with Services {
    "first test" in { succeed }
    "second test" ignore { succeed }
  }

  class NestedWordSpecExample extends WordSpec with NestedTestNames {
    "A subject" should {
      "first test" in { succeed }
      "second test" ignore { succeed }
    }
  }

  class DeeplyNestedWordSpecExample extends WordSpec with DeeplyNestedTestNames {
    "A subject" when {
      "created" should {
        "first test" in { succeed }
        "second test" ignore { succeed }
      }
    }
  }

  class FixtureWordSpecExample extends StringFixtureWordSpec with Services {
    "first test" in { s => succeed }
    "second test" ignore { s => succeed }
  }

  class NestedFixtureWordSpecExample extends StringFixtureWordSpec with NestedTestNames {
    "A subject" should {
      "first test" in { s => succeed }
      "second test" ignore { s => succeed }
    }
  }

  class DeeplyNestedFixtureWordSpecExample extends StringFixtureWordSpec with DeeplyNestedTestNames {
    "A subject" when {
      "created" should {
        "first test" in { s => succeed }
        "second test" ignore { s => succeed }
      }
    }
  }

  class NestedWordSpecWithMustExample extends WordSpec with NestedTestNamesWithMust {
    "A subject" must {
      "first test" in { succeed }
      "second test" ignore { succeed }
    }
  }

  class DeeplyNestedWordSpecWithMustExample extends WordSpec with DeeplyNestedTestNamesWithMust {
    "A subject" when {
      "created" must {
        "first test" in { succeed }
        "second test" ignore { succeed }
      }
    }
  }

  class NestedFixtureWordSpecWithMustExample extends StringFixtureWordSpec with NestedTestNamesWithMust {
    "A subject" must {
      "first test" in { s => succeed }
      "second test" ignore { s => succeed }
    }
  }

  class DeeplyNestedFixtureWordSpecWithMustExample extends StringFixtureWordSpec with DeeplyNestedTestNamesWithMust {
    "A subject" when {
      "created" must {
        "first test" in { s => succeed }
        "second test" ignore { s => succeed }
      }
    }
  }

  class NestedWordSpecWithCanExample extends WordSpec with NestedTestNamesWithCan {
    "A subject" can {
      "first test" in { succeed }
      "second test" ignore { succeed }
    }
  }

  class DeeplyNestedWordSpecWithCanExample extends WordSpec with DeeplyNestedTestNamesWithCan {
    "A subject" when {
      "created" can {
        "first test" in { succeed }
        "second test" ignore { succeed }
      }
    }
  }

  class NestedFixtureWordSpecWithCanExample extends StringFixtureWordSpec with NestedTestNamesWithCan {
    "A subject" can {
      "first test" in { s => succeed }
      "second test" ignore { s => succeed }
    }
  }

  class DeeplyNestedFixtureWordSpecWithCanExample extends StringFixtureWordSpec with DeeplyNestedTestNamesWithCan {
    "A subject" when {
      "created" can {
        "first test" in { s => succeed }
        "second test" ignore { s => succeed }
      }
    }
  }

  class FlatSpecExample extends FlatSpec with Services {
    it should "first test" in { succeed }
    it should "second test" ignore { succeed }
    override val theTestNames = Vector("should first test", "should second test")
   }

  class SubjectFlatSpecExample extends FlatSpec with NestedTestNames {
    behavior of "A subject"
    it should "first test" in { succeed }
    it should "second test" ignore { succeed }
   }

  class ShorthandSubjectFlatSpecExample extends FlatSpec with NestedTestNames {
    "A subject" should "first test" in { succeed }
    it should "second test" ignore { succeed }
   }

  class FixtureFlatSpecExample extends StringFixtureFlatSpec with Services {
    it should "first test" in { s => succeed }
    it should "second test" ignore { s => succeed }
    override val theTestNames = Vector("should first test", "should second test")
  }

  class SubjectFixtureFlatSpecExample extends StringFixtureFlatSpec with NestedTestNames {
    behavior of "A subject"
    it should "first test" in { s => succeed }
    it should "second test" ignore { s => succeed }
   }

  class ShorthandSubjectFixtureFlatSpecExample extends StringFixtureFlatSpec with NestedTestNames {
    "A subject" should "first test" in { s => succeed }
    it should "second test" ignore { s => succeed }
   }

  class FlatSpecWithMustExample extends FlatSpec with Services {
    it must "first test" in { succeed }
    it must "second test" ignore { succeed }
    override val theTestNames = Vector("must first test", "must second test")
   }

  class SubjectFlatSpecWithMustExample extends FlatSpec with NestedTestNamesWithMust {
    behavior of "A subject"
    it must "first test" in { succeed }
    it must "second test" ignore { succeed }
   }

  class ShorthandSubjectFlatSpecWithMustExample extends FlatSpec with NestedTestNamesWithMust {
    "A subject" must "first test" in { succeed }
    it must "second test" ignore { succeed }
   }

  class FixtureFlatSpecWithMustExample extends StringFixtureFlatSpec with Services {
    it must "first test" in { s => succeed }
    it must "second test" ignore { s => succeed }
    override val theTestNames = Vector("must first test", "must second test")
  }

  class SubjectFixtureFlatSpecWithMustExample extends StringFixtureFlatSpec with NestedTestNamesWithMust {
    behavior of "A subject"
    it must "first test" in { s => succeed }
    it must "second test" ignore { s => succeed }
   }

  class ShorthandSubjectFixtureFlatSpecWithMustExample extends StringFixtureFlatSpec with NestedTestNamesWithMust {
    "A subject" must "first test" in { s => succeed }
    it must "second test" ignore { s => succeed }
   }

  class FlatSpecWithCanExample extends FlatSpec with Services {
    it can "first test" in { succeed }
    it can "second test" ignore { succeed }
    override val theTestNames = Vector("can first test", "can second test")
   }

  class SubjectFlatSpecWithCanExample extends FlatSpec with NestedTestNamesWithCan {
    behavior of "A subject"
    it can "first test" in { succeed }
    it can "second test" ignore { succeed }
   }

  class ShorthandSubjectFlatSpecWithCanExample extends FlatSpec with NestedTestNamesWithCan {
    "A subject" can "first test" in { succeed }
    it can "second test" ignore { succeed }
   }

  class FixtureFlatSpecWithCanExample extends StringFixtureFlatSpec with Services {
    it can "first test" in { s => succeed }
    it can "second test" ignore { s => succeed }
    override val theTestNames = Vector("can first test", "can second test")
  }

  class SubjectFixtureFlatSpecWithCanExample extends StringFixtureFlatSpec with NestedTestNamesWithCan {
    behavior of "A subject"
    it can "first test" in { s => succeed }
    it can "second test" ignore { s => succeed }
   }

  class ShorthandSubjectFixtureFlatSpecWithCanExample extends StringFixtureFlatSpec with NestedTestNamesWithCan {
    "A subject" can "first test" in { s => succeed }
    it can "second test" ignore { s => succeed }
   }

  class FreeSpecExample extends FreeSpec with Services {
    "first test" in { succeed }
    "second test" ignore { succeed }
  }

  class NestedFreeSpecExample extends FreeSpec with NestedTestNames {
    "A subject" - {
      "should first test" in { succeed }
      "should second test" ignore { succeed }
    }
  }

  class DeeplyNestedFreeSpecExample extends FreeSpec with DeeplyNestedTestNames {
    "A subject" - {
      "when created" - {
        "should first test" in { succeed }
        "should second test" ignore { succeed }
      }
    }
  }

  class FixtureFreeSpecExample extends StringFixtureFreeSpec with Services {
    "first test" in { s => succeed }
    "second test" ignore { s => succeed }
  }

  class NestedFixtureFreeSpecExample extends StringFixtureFreeSpec with NestedTestNames {
    "A subject" - {
      "should first test" in { s => succeed }
      "should second test" ignore { s => succeed }
    }
  }

  class DeeplyNestedFixtureFreeSpecExample extends StringFixtureFreeSpec with DeeplyNestedTestNames {
    "A subject" - {
      "when created" - {
        "should first test" in { s => succeed }
        "should second test" ignore { s => succeed }
      }
    }
  }

  class PathFreeSpecExample extends path.FreeSpec with Services {
    "first test" in { succeed }
    "second test" ignore { succeed }
    override def newInstance = new PathFreeSpecExample
  }

  class NestedPathFreeSpecExample extends path.FreeSpec with NestedTestNames {
    "A subject" - {
      "should first test" in { succeed }
      "should second test" ignore { succeed }
    }
    override def newInstance = new NestedPathFreeSpecExample
  }

  class DeeplyNestedPathFreeSpecExample extends path.FreeSpec with DeeplyNestedTestNames {
    "A subject" - {
      "when created" - {
        "should first test" in { succeed }
        "should second test" ignore { succeed }
      }
    }
    override def newInstance = new DeeplyNestedPathFreeSpecExample
  }

  class FeatureSpecExample extends FeatureSpec with Services {
    scenario("first test") { succeed }
    ignore("second test") { succeed }
    override val theTestNames = Vector("Scenario: first test", "Scenario: second test")
  }

  class NestedFeatureSpecExample extends FeatureSpec with Services {
    feature("A feature") {
      scenario("first test") { succeed }
      ignore("second test") { succeed }
    }
    override val theTestNames = Vector("Feature: A feature Scenario: first test", "Feature: A feature Scenario: second test")
  }

  class FixtureFeatureSpecExample extends StringFixtureFeatureSpec with Services {
    scenario("first test") { s => succeed }
    ignore("second test") { s => succeed }
    override val theTestNames = Vector("Scenario: first test", "Scenario: second test")
  }

  class NestedFixtureFeatureSpecExample extends StringFixtureFeatureSpec with Services {
    feature("A feature") {
      scenario("first test") { s => succeed }
      ignore("second test") { s => succeed }
    }
    override val theTestNames = Vector("Feature: A feature Scenario: first test", "Feature: A feature Scenario: second test")
  }

  class PropSpecExample extends PropSpec with Services {
    property("first test") { succeed }
    ignore("second test") { succeed }
  }

  class FixturePropSpecExample extends StringFixturePropSpec with Services {
    property("first test") { s => succeed }
    ignore("second test") { s => succeed }
  }

  // SKIP-SCALATESTJS-START
  lazy val spec = new SpecExample
  lazy val fixtureSpec = new FixtureSpecExample
  // SKIP-SCALATESTJS-END
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
   
  // Two ways to ignore in a flat spec, so add two more examples
  override def examples = super.examples ++
    List(
      new FlatSpecExample2,
      new FixtureFlatSpecExample2,
      new FlatSpecWithMustExample2,
      new FixtureFlatSpecWithMustExample2,
      new FlatSpecWithCanExample2,
      new FixtureFlatSpecWithCanExample2
    )

  class FlatSpecExample2 extends FlatSpec with Services {
    it should "first test" in { succeed }
    ignore should "second test" in { succeed }
    override val theTestNames = Vector("should first test", "should second test")
   }

  class FixtureFlatSpecExample2 extends StringFixtureFlatSpec with Services {
    it should "first test" in { s => succeed }
    ignore should "second test" in { s => succeed }
    override val theTestNames = Vector("should first test", "should second test")
  }

  class FlatSpecWithMustExample2 extends FlatSpec with Services {
    it must "first test" in { succeed }
    ignore must "second test" in { succeed }
    override val theTestNames = Vector("must first test", "must second test")
   }

  class FixtureFlatSpecWithMustExample2 extends StringFixtureFlatSpec with Services {
    it must "first test" in { s => succeed }
    ignore must "second test" in { s => succeed }
    override val theTestNames = Vector("must first test", "must second test")
  }

  class FlatSpecWithCanExample2 extends FlatSpec with Services {
    it can "first test" in { succeed }
    ignore can "second test" in { succeed }
    override val theTestNames = Vector("can first test", "can second test")
   }

  class FixtureFlatSpecWithCanExample2 extends StringFixtureFlatSpec with Services {
    it can "first test" in { s => succeed }
    ignore can "second test" in { s => succeed }
    override val theTestNames = Vector("can first test", "can second test")
  }
}
