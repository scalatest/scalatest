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
import prop.Tables
// SKIP-SCALATESTJS,NATIVE-START
import refspec.RefSpec
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec
// SKIP-SCALATESTJS,NATIVE-END

trait SuiteExamples extends Tables {

  type FixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  val spec: RefSpec with FixtureServices
  // SKIP-SCALATESTJS,NATIVE-END

  val funSuite: AnyFunSuite with FixtureServices
  val fixtureFunSuite: funsuite.FixtureAnyFunSuite with FixtureServices

  val funSpec: AnyFunSpec with FixtureServices
  val nestedFunSpec: AnyFunSpec with FixtureServices
  val deeplyNestedFunSpec: AnyFunSpec with FixtureServices
  val fixtureFunSpec: funspec.FixtureAnyFunSpec with FixtureServices
  val nestedFixtureFunSpec: funspec.FixtureAnyFunSpec with FixtureServices
  val deeplyNestedFixtureFunSpec: funspec.FixtureAnyFunSpec with FixtureServices

  val pathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val nestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val deeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices

  val wordSpec: AnyWordSpec with FixtureServices
  val nestedWordSpec: AnyWordSpec with FixtureServices
  val deeplyNestedWordSpec: AnyWordSpec with FixtureServices
  val fixtureWordSpec: wordspec.FixtureAnyWordSpec with FixtureServices
  val nestedFixtureWordSpec: wordspec.FixtureAnyWordSpec with FixtureServices
  val deeplyNestedFixtureWordSpec: wordspec.FixtureAnyWordSpec with FixtureServices

  val nestedWordSpecWithMust: AnyWordSpec with FixtureServices
  val deeplyNestedWordSpecWithMust: AnyWordSpec with FixtureServices
  val nestedFixtureWordSpecWithMust: wordspec.FixtureAnyWordSpec with FixtureServices
  val deeplyNestedFixtureWordSpecWithMust: wordspec.FixtureAnyWordSpec with FixtureServices

  val nestedWordSpecWithCan: AnyWordSpec with FixtureServices
  val deeplyNestedWordSpecWithCan: AnyWordSpec with FixtureServices
  val nestedFixtureWordSpecWithCan: wordspec.FixtureAnyWordSpec with FixtureServices
  val deeplyNestedFixtureWordSpecWithCan: wordspec.FixtureAnyWordSpec with FixtureServices

  val flatSpec: AnyFlatSpec with FixtureServices
  val subjectFlatSpec: AnyFlatSpec with FixtureServices
  val shorthandSubjectFlatSpec: AnyFlatSpec with FixtureServices
  val fixtureFlatSpec: flatspec.FixtureAnyFlatSpec with FixtureServices
  val subjectFixtureFlatSpec: flatspec.FixtureAnyFlatSpec with FixtureServices
  val shorthandSubjectFixtureFlatSpec: flatspec.FixtureAnyFlatSpec with FixtureServices
  
  val flatSpecWithMust: AnyFlatSpec with FixtureServices
  val subjectFlatSpecWithMust: AnyFlatSpec with FixtureServices
  val shorthandSubjectFlatSpecWithMust: AnyFlatSpec with FixtureServices
  val fixtureFlatSpecWithMust: flatspec.FixtureAnyFlatSpec with FixtureServices
  val subjectFixtureFlatSpecWithMust: flatspec.FixtureAnyFlatSpec with FixtureServices
  val shorthandSubjectFixtureFlatSpecWithMust: flatspec.FixtureAnyFlatSpec with FixtureServices

  val flatSpecWithCan: AnyFlatSpec with FixtureServices
  val subjectFlatSpecWithCan: AnyFlatSpec with FixtureServices
  val shorthandSubjectFlatSpecWithCan: AnyFlatSpec with FixtureServices
  val fixtureFlatSpecWithCan: flatspec.FixtureAnyFlatSpec with FixtureServices
  val subjectFixtureFlatSpecWithCan: flatspec.FixtureAnyFlatSpec with FixtureServices
  val shorthandSubjectFixtureFlatSpecWithCan: flatspec.FixtureAnyFlatSpec with FixtureServices

  val freeSpec: AnyFreeSpec with FixtureServices
  val nestedFreeSpec: AnyFreeSpec with FixtureServices
  val deeplyNestedFreeSpec: AnyFreeSpec with FixtureServices
  val fixtureFreeSpec: freespec.FixtureAnyFreeSpec with FixtureServices
  val nestedFixtureFreeSpec: freespec.FixtureAnyFreeSpec with FixtureServices
  val deeplyNestedFixtureFreeSpec: freespec.FixtureAnyFreeSpec with FixtureServices

  val pathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val nestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val deeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices

  val featureSpec: AnyFeatureSpec with FixtureServices
  val nestedFeatureSpec: AnyFeatureSpec with FixtureServices
  val fixtureFeatureSpec: featurespec.FixtureAnyFeatureSpec with FixtureServices
  val nestedFixtureFeatureSpec: featurespec.FixtureAnyFeatureSpec with FixtureServices

  val propSpec: AnyPropSpec with FixtureServices
  val fixturePropSpec: propspec.FixtureAnyPropSpec with FixtureServices

  def examples =
    Table(
      "suite",
      // SKIP-SCALATESTJS,NATIVE-START
      spec,
      // SKIP-SCALATESTJS,NATIVE-END
      
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
