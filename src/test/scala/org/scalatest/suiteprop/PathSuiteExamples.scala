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

abstract class PathSuiteExamples extends Tables {

  type FixtureServices

  val emptyPathFunSpec: path.FunSpec with FixtureServices
  val emptyNestedPathFunSpec: path.FunSpec with FixtureServices
  val siblingEmptyNestedPathFunSpec: path.FunSpec with FixtureServices
  val oneTestSiblingEmptyNestedPathFunSpec: path.FunSpec with FixtureServices
  val oneTestSiblingEmptyDeeplyNestedPathFunSpec: path.FunSpec with FixtureServices
  val pathFunSpec: path.FunSpec with FixtureServices
  val nestedPathFunSpec: path.FunSpec with FixtureServices
  val siblingNestedPathFunSpec: path.FunSpec with FixtureServices
  val deeplyNestedPathFunSpec: path.FunSpec with FixtureServices
  val siblingDeeplyNestedPathFunSpec: path.FunSpec with FixtureServices
  val asymetricalDeeplyNestedPathFunSpec: path.FunSpec with FixtureServices
  val emptyPathFreeSpec: path.FreeSpec with FixtureServices
  val emptyNestedPathFreeSpec: path.FreeSpec with FixtureServices
  val siblingEmptyNestedPathFreeSpec: path.FreeSpec with FixtureServices
  val oneTestSiblingEmptyNestedPathFreeSpec: path.FreeSpec with FixtureServices
  val oneTestSiblingEmptyDeeplyNestedPathFreeSpec: path.FreeSpec with FixtureServices
  val pathFreeSpec: path.FreeSpec with FixtureServices
  val nestedPathFreeSpec: path.FreeSpec with FixtureServices
  val siblingNestedPathFreeSpec: path.FreeSpec with FixtureServices
  val deeplyNestedPathFreeSpec: path.FreeSpec with FixtureServices
  val siblingDeeplyNestedPathFreeSpec: path.FreeSpec with FixtureServices
  val asymetricalDeeplyNestedPathFreeSpec: path.FreeSpec with FixtureServices
 
  val examples =
    Table(
    "path suite",
    emptyPathFunSpec,
    emptyNestedPathFunSpec,
    siblingEmptyNestedPathFunSpec,
    oneTestSiblingEmptyNestedPathFunSpec,
    oneTestSiblingEmptyDeeplyNestedPathFunSpec,
    pathFunSpec,
    nestedPathFunSpec,
    siblingNestedPathFunSpec,
    deeplyNestedPathFunSpec,
    siblingDeeplyNestedPathFunSpec,
    asymetricalDeeplyNestedPathFunSpec,
    emptyPathFreeSpec,
    emptyNestedPathFreeSpec,
    siblingEmptyNestedPathFreeSpec,
    oneTestSiblingEmptyNestedPathFreeSpec,
    oneTestSiblingEmptyDeeplyNestedPathFreeSpec,
    pathFreeSpec,
    nestedPathFreeSpec,
    siblingNestedPathFreeSpec,
    deeplyNestedPathFreeSpec,
    siblingDeeplyNestedPathFreeSpec,
    asymetricalDeeplyNestedPathFreeSpec
  )
}
