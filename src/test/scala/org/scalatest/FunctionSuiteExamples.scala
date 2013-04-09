package org.scalatest

import org.scalatest.prop.Tables

trait FunctionSuiteExamples extends Tables {

  type FixtureServices
  
  def funSuite: FunSuite with FixtureServices
  def fixtureFunSuite: fixture.FunSuite with FixtureServices
  def funSpec: FunSpec with FixtureServices
  def fixtureFunSpec: fixture.FunSpec with FixtureServices
  def featureSpec: FeatureSpec with FixtureServices
  def fixtureFeatureSpec: fixture.FeatureSpec with FixtureServices
  def flatSpec: FlatSpec with FixtureServices
  def fixtureFlatSpec: fixture.FlatSpec with FixtureServices
  def freeSpec: FreeSpec with FixtureServices
  def fixtureFreeSpec: fixture.FreeSpec with FixtureServices
  def propSpec: PropSpec with FixtureServices
  def fixturePropSpec: fixture.PropSpec with FixtureServices
  def wordSpec: WordSpec with FixtureServices
  def fixtureWordSpec: fixture.WordSpec with FixtureServices
  def pathFreeSpec: path.FreeSpec with FixtureServices
  def pathFunSpec: path.FunSpec with FixtureServices
  
  def examples =
    Table(
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