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
import org.scalatest.{ freespec, funspec }

abstract class PathSuiteExamples extends Tables {

  type FixtureServices

  def emptyPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def emptyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def siblingEmptyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def oneTestSiblingEmptyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def oneTestSiblingEmptyDeeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def pathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def nestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def siblingNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def deeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def siblingDeeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def asymetricalDeeplyNestedPathFunSpec: funspec.PathAnyFunSpec with FixtureServices
  def emptyPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def emptyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def siblingEmptyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def oneTestSiblingEmptyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def oneTestSiblingEmptyDeeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def pathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def nestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def siblingNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def deeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def siblingDeeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
  def asymetricalDeeplyNestedPathFreeSpec: freespec.PathAnyFreeSpec with FixtureServices
 
  def examples: org.scalatest.prop.TableFor1[Suite with FixtureServices] =
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
