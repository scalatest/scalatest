/*
 * Copyright 2001-2011 Artima, Inc.
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
import prop.Tables

trait SuiteExamples extends Tables {

  type FixtureServices

  val suite: Suite with FixtureServices
  val fixtureSuite: fixture.Suite with FixtureServices

  val funSuite: FunSuite with FixtureServices
  val fixtureFunSuite: fixture.FunSuite with FixtureServices
  
  val funSpec: FunSpec with FixtureServices
  val nestedFunSpec: FunSpec with FixtureServices
  val deeplyNestedFunSpec: FunSpec with FixtureServices
  val fixtureFunSpec: fixture.FunSpec with FixtureServices
  val nestedFixtureFunSpec: fixture.FunSpec with FixtureServices
  val deeplyNestedFixtureFunSpec: fixture.FunSpec with FixtureServices

  val pathFunSpec: path.FunSpec with FixtureServices
  val nestedPathFunSpec: path.FunSpec with FixtureServices
  val deeplyNestedPathFunSpec: path.FunSpec with FixtureServices

  val wordSpec: WordSpec with FixtureServices
  val nestedWordSpec: WordSpec with FixtureServices
  val deeplyNestedWordSpec: WordSpec with FixtureServices
  val fixtureWordSpec: fixture.WordSpec with FixtureServices
  val nestedFixtureWordSpec: fixture.WordSpec with FixtureServices
  val deeplyNestedFixtureWordSpec: fixture.WordSpec with FixtureServices

  val nestedWordSpecWithMust: WordSpec with FixtureServices
  val deeplyNestedWordSpecWithMust: WordSpec with FixtureServices
  val nestedFixtureWordSpecWithMust: fixture.WordSpec with FixtureServices
  val deeplyNestedFixtureWordSpecWithMust: fixture.WordSpec with FixtureServices

  val nestedWordSpecWithCan: WordSpec with FixtureServices
  val deeplyNestedWordSpecWithCan: WordSpec with FixtureServices
  val nestedFixtureWordSpecWithCan: fixture.WordSpec with FixtureServices
  val deeplyNestedFixtureWordSpecWithCan: fixture.WordSpec with FixtureServices

  val flatSpec: FlatSpec with FixtureServices
  val subjectFlatSpec: FlatSpec with FixtureServices
  val shorthandSubjectFlatSpec: FlatSpec with FixtureServices
  val fixtureFlatSpec: fixture.FlatSpec with FixtureServices
  val subjectFixtureFlatSpec: fixture.FlatSpec with FixtureServices
  val shorthandSubjectFixtureFlatSpec: fixture.FlatSpec with FixtureServices
  
  val flatSpecWithMust: FlatSpec with FixtureServices
  val subjectFlatSpecWithMust: FlatSpec with FixtureServices
  val shorthandSubjectFlatSpecWithMust: FlatSpec with FixtureServices
  val fixtureFlatSpecWithMust: fixture.FlatSpec with FixtureServices
  val subjectFixtureFlatSpecWithMust: fixture.FlatSpec with FixtureServices
  val shorthandSubjectFixtureFlatSpecWithMust: fixture.FlatSpec with FixtureServices

  val flatSpecWithCan: FlatSpec with FixtureServices
  val subjectFlatSpecWithCan: FlatSpec with FixtureServices
  val shorthandSubjectFlatSpecWithCan: FlatSpec with FixtureServices
  val fixtureFlatSpecWithCan: fixture.FlatSpec with FixtureServices
  val subjectFixtureFlatSpecWithCan: fixture.FlatSpec with FixtureServices
  val shorthandSubjectFixtureFlatSpecWithCan: fixture.FlatSpec with FixtureServices

  val freeSpec: FreeSpec with FixtureServices
  val nestedFreeSpec: FreeSpec with FixtureServices
  val deeplyNestedFreeSpec: FreeSpec with FixtureServices
  val fixtureFreeSpec: fixture.FreeSpec with FixtureServices
  val nestedFixtureFreeSpec: fixture.FreeSpec with FixtureServices
  val deeplyNestedFixtureFreeSpec: fixture.FreeSpec with FixtureServices

  val pathFreeSpec: path.FreeSpec with FixtureServices
  val nestedPathFreeSpec: path.FreeSpec with FixtureServices
  val deeplyNestedPathFreeSpec: path.FreeSpec with FixtureServices

  val featureSpec: FeatureSpec with FixtureServices
  val nestedFeatureSpec: FeatureSpec with FixtureServices
  val fixtureFeatureSpec: fixture.FeatureSpec with FixtureServices
  val nestedFixtureFeatureSpec: fixture.FeatureSpec with FixtureServices

  val propSpec: PropSpec with FixtureServices
  val fixturePropSpec: fixture.PropSpec with FixtureServices

  def examples =
    Table(
      "suite",
      suite,
      fixtureSuite,
      
      funSuite,
      fixtureFunSuite,

      funSpec,
      nestedFunSpec,
      deeplyNestedFunSpec,
      fixtureFunSpec,
      nestedFixtureFunSpec,
      deeplyNestedFixtureFunSpec,

      pathFunSpec,
      nestedPathFunSpec,
      deeplyNestedPathFunSpec,

      wordSpec,
      nestedWordSpec,
      deeplyNestedWordSpec,
      fixtureWordSpec,
      nestedFixtureWordSpec,
      deeplyNestedFixtureWordSpec,

      nestedWordSpecWithMust,
      deeplyNestedWordSpecWithMust,
      nestedFixtureWordSpecWithMust,
      deeplyNestedFixtureWordSpecWithMust,

      nestedWordSpecWithCan,
      deeplyNestedWordSpecWithCan,
      nestedFixtureWordSpecWithCan,
      deeplyNestedFixtureWordSpecWithCan,

      flatSpec,
      subjectFlatSpec,
      shorthandSubjectFlatSpec,
      fixtureFlatSpec,
      subjectFixtureFlatSpec,
      shorthandSubjectFixtureFlatSpec,

      flatSpecWithMust,
      subjectFlatSpecWithMust,
      shorthandSubjectFlatSpecWithMust,
      fixtureFlatSpecWithMust,
      subjectFixtureFlatSpecWithMust,
      shorthandSubjectFixtureFlatSpecWithMust,

      flatSpecWithCan,
      subjectFlatSpecWithCan,
      shorthandSubjectFlatSpecWithCan,
      fixtureFlatSpecWithCan,
      subjectFixtureFlatSpecWithCan,
      shorthandSubjectFixtureFlatSpecWithCan,

      freeSpec,
      nestedFreeSpec,
      deeplyNestedFreeSpec,
      fixtureFreeSpec,
      nestedFixtureFreeSpec,
      deeplyNestedFixtureFreeSpec,
      pathFreeSpec,
      nestedPathFreeSpec,
      deeplyNestedPathFreeSpec,

      featureSpec,
      nestedFeatureSpec,
      fixtureFeatureSpec,
      nestedFixtureFeatureSpec,

      propSpec,
      fixturePropSpec
    )
}
