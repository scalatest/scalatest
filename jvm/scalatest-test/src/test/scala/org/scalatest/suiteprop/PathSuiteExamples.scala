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
import org.scalatest.{ freespec, funspec }

abstract class PathSuiteExamples extends Tables {

  type FixtureServices

  val emptyPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val emptyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val siblingEmptyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val oneTestSiblingEmptyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val oneTestSiblingEmptyDeeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val pathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val nestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val siblingNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val deeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val siblingDeeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val asymetricalDeeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  val emptyPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val emptyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val siblingEmptyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val oneTestSiblingEmptyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val oneTestSiblingEmptyDeeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val pathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val nestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val siblingNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val deeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val siblingDeeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  val asymetricalDeeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
 
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
