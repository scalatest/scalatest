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
import prop.Tables
// SKIP-SCALATESTJS,NATIVE-START
import refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

trait SuiteExamples extends Tables {

  type FixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  def spec: RefSpec with FixtureServices
  // SKIP-SCALATESTJS,NATIVE-END

  def funSuite: AnyFunSuite with FixtureServices
  def fixtureFunSuite: funsuite.FixtureAnyFunSuite with FixtureServices

  def funSpec: AnyFunSpec with FixtureServices
  def nestedFunSpec: AnyFunSpec with FixtureServices
  def deeplyNestedFunSpec: AnyFunSpec with FixtureServices
  def fixtureFunSpec: funspec.FixtureAnyFunSpec with FixtureServices
  def nestedFixtureFunSpec: funspec.FixtureAnyFunSpec with FixtureServices
  def deeplyNestedFixtureFunSpec: funspec.FixtureAnyFunSpec with FixtureServices

  def pathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def nestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def deeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices

  def wordSpec: AnyWordSpec with FixtureServices
  def nestedWordSpec: AnyWordSpec with FixtureServices
  def deeplyNestedWordSpec: AnyWordSpec with FixtureServices
  def fixtureWordSpec: wordspec.FixtureAnyWordSpec with FixtureServices
  def nestedFixtureWordSpec: wordspec.FixtureAnyWordSpec with FixtureServices
  def deeplyNestedFixtureWordSpec: wordspec.FixtureAnyWordSpec with FixtureServices

  def nestedWordSpecWithMust: AnyWordSpec with FixtureServices
  def deeplyNestedWordSpecWithMust: AnyWordSpec with FixtureServices
  def nestedFixtureWordSpecWithMust: wordspec.FixtureAnyWordSpec with FixtureServices
  def deeplyNestedFixtureWordSpecWithMust: wordspec.FixtureAnyWordSpec with FixtureServices

  def nestedWordSpecWithCan: AnyWordSpec with FixtureServices
  def deeplyNestedWordSpecWithCan: AnyWordSpec with FixtureServices
  def nestedFixtureWordSpecWithCan: wordspec.FixtureAnyWordSpec with FixtureServices
  def deeplyNestedFixtureWordSpecWithCan: wordspec.FixtureAnyWordSpec with FixtureServices

  def flatSpec: AnyFlatSpec with FixtureServices
  def subjectFlatSpec: AnyFlatSpec with FixtureServices
  def shorthandSubjectFlatSpec: AnyFlatSpec with FixtureServices
  def fixtureFlatSpec: flatspec.FixtureAnyFlatSpec with FixtureServices
  def subjectFixtureFlatSpec: flatspec.FixtureAnyFlatSpec with FixtureServices
  def shorthandSubjectFixtureFlatSpec: flatspec.FixtureAnyFlatSpec with FixtureServices
  
  def flatSpecWithMust: AnyFlatSpec with FixtureServices
  def subjectFlatSpecWithMust: AnyFlatSpec with FixtureServices
  def shorthandSubjectFlatSpecWithMust: AnyFlatSpec with FixtureServices
  def fixtureFlatSpecWithMust: flatspec.FixtureAnyFlatSpec with FixtureServices
  def subjectFixtureFlatSpecWithMust: flatspec.FixtureAnyFlatSpec with FixtureServices
  def shorthandSubjectFixtureFlatSpecWithMust: flatspec.FixtureAnyFlatSpec with FixtureServices

  def flatSpecWithCan: AnyFlatSpec with FixtureServices
  def subjectFlatSpecWithCan: AnyFlatSpec with FixtureServices
  def shorthandSubjectFlatSpecWithCan: AnyFlatSpec with FixtureServices
  def fixtureFlatSpecWithCan: flatspec.FixtureAnyFlatSpec with FixtureServices
  def subjectFixtureFlatSpecWithCan: flatspec.FixtureAnyFlatSpec with FixtureServices
  def shorthandSubjectFixtureFlatSpecWithCan: flatspec.FixtureAnyFlatSpec with FixtureServices

  def freeSpec: AnyFreeSpec with FixtureServices
  def nestedFreeSpec: AnyFreeSpec with FixtureServices
  def deeplyNestedFreeSpec: AnyFreeSpec with FixtureServices
  def fixtureFreeSpec: freespec.FixtureAnyFreeSpec with FixtureServices
  def nestedFixtureFreeSpec: freespec.FixtureAnyFreeSpec with FixtureServices
  def deeplyNestedFixtureFreeSpec: freespec.FixtureAnyFreeSpec with FixtureServices

  def pathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def nestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def deeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices

  def featureSpec: AnyFeatureSpec with FixtureServices
  def nestedFeatureSpec: AnyFeatureSpec with FixtureServices
  def fixtureFeatureSpec: featurespec.FixtureAnyFeatureSpec with FixtureServices
  def nestedFixtureFeatureSpec: featurespec.FixtureAnyFeatureSpec with FixtureServices

  def propSpec: AnyPropSpec with FixtureServices
  def fixturePropSpec: propspec.FixtureAnyPropSpec with FixtureServices

  def examples: org.scalatest.prop.TableFor1[Suite with FixtureServices] =
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
