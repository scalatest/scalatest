package org.scalatest

import org.scalatest.prop.Tables
import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.scalatest.testng.TestNGSuite

trait MethodSuiteExamples extends Tables {
  type FixtureServices
  
  def suite: Suite with FixtureServices
  def fixtureSuite: fixture.Suite with FixtureServices
  def spec: Spec with FixtureServices
  def fixtureSpec: fixture.Spec with FixtureServices
  def junit3Suite: JUnit3Suite with FixtureServices
  def junitSuite: JUnitSuite with FixtureServices
  def testngSuite: TestNGSuite with FixtureServices
  
  def examples =
    Table(
      "suite",
      suite,
      fixtureSuite,
      spec,
      fixtureSpec, 
      junit3Suite, 
      junitSuite,
      testngSuite
    )
}