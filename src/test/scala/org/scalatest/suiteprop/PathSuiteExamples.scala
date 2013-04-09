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
