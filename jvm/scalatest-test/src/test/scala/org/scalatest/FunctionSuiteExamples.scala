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

import org.scalatest.prop.Tables
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

trait FunctionSuiteExamples extends Tables {

  type FixtureServices
  
  def funSuite: AnyFunSuite with FixtureServices
  def fixtureFunSuite: funsuite.FixtureAnyFunSuite with FixtureServices
  def funSpec: AnyFunSpec with FixtureServices
  def fixtureFunSpec: funspec.FixtureAnyFunSpec with FixtureServices
  def featureSpec: AnyFeatureSpec with FixtureServices
  def fixtureFeatureSpec: featurespec.FixtureAnyFeatureSpec with FixtureServices
  def flatSpec: AnyFlatSpec with FixtureServices
  def fixtureFlatSpec: flatspec.FixtureAnyFlatSpec with FixtureServices
  def freeSpec: AnyFreeSpec with FixtureServices
  def fixtureFreeSpec: freespec.FixtureAnyFreeSpec with FixtureServices
  def propSpec: AnyPropSpec with FixtureServices
  def fixturePropSpec: propspec.FixtureAnyPropSpec with FixtureServices
  def wordSpec: AnyWordSpec with FixtureServices
  def fixtureWordSpec: wordspec.FixtureAnyWordSpec with FixtureServices
  def pathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def pathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  
  def examples =
    Table[Suite with FixtureServices](
      "suite",
      funSuite,
      fixtureFunSuite,
      funSpec,
      fixtureFunSpec,
      featureSpec,
      fixtureFeatureSpec,
      flatSpec,
      fixtureFlatSpec,
      freeSpec,
      fixtureFreeSpec,
      propSpec,
      fixturePropSpec,
      wordSpec,
      fixtureWordSpec, 
      pathFreeSpec, 
      pathFunSpec
    )
  
}
