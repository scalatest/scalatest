package org.scalatest

import org.scalatest.junit._
import org.scalatest.testng.TestNGSuite
import org.junit.Test

class ClassTaggingProp extends AllSuiteProp {

  type FixtureServices = ClassTaggingFixtureServices
  
  def suite = new ExampleClassTaggingSuite
  def fixtureSuite = new ExampleClassTaggingFixtureSuite
  def spec = new ExampleClassTaggingSpec
  def fixtureSpec = new ExampleClassTaggingFixtureSpec
  def junit3Suite = new ExampleClassTaggingJUnit3Suite
  def junitSuite = new ExampleClassTaggingJUnitSuite
  def testngSuite = new ExampleClassTaggingTestNGSuite
  def funSuite = new ExampleClassTaggingFunSuite
  def fixtureFunSuite = new ExampleClassTaggingFixtureFunSuite
  def funSpec = new ExampleClassTaggingFunSpec
  def fixtureFunSpec = new ExampleClassTaggingFixtureFunSpec
  def featureSpec = new ExampleClassTaggingFeatureSpec
  def fixtureFeatureSpec = new ExampleClassTaggingFixtureFeatureSpec
  def flatSpec = new ExampleClassTaggingFlatSpec
  def fixtureFlatSpec = new ExampleClassTaggingFixtureFlatSpec
  def freeSpec = new ExampleClassTaggingFreeSpec
  def fixtureFreeSpec = new ExampleClassTaggingFixtureFreeSpec
  def propSpec = new ExampleClassTaggingPropSpec
  def fixturePropSpec = new ExampleClassTaggingFixturePropSpec
  def wordSpec = new ExampleClassTaggingWordSpec
  def fixtureWordSpec = new ExampleClassTaggingFixtureWordSpec
  def pathFreeSpec = new ExampleClassTaggingPathFreeSpec
  def pathFunSpec = new ExampleClassTaggingPathFunSpec
  
  test("@Ignore marked at the class level must auto-mark all tests in the suite") {
    forAll(examples.filter(_.included)) { s =>
      val testNames = s.testNames
      val tags = s.tags
      testNames.foreach { tn => 
        val tagSet = tags.get(tn)
        tagSet.isDefined should be (true)
        tagSet.get should contain ("org.scalatest.Ignore")
      }
    }
  }
}

trait ClassTaggingFixtureServices {
  def included = this match {
    case _: JUnit3Suite => false
    case _ => true
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingSuite extends Suite with ClassTaggingFixtureServices {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureSuite extends fixture.Suite with ClassTaggingFixtureServices with StringFixture {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingSpec extends Spec with ClassTaggingFixtureServices {
  def `test 1` {}
  def `test 2` {}
  def `test 3` {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureSpec extends fixture.Spec with ClassTaggingFixtureServices with StringFixture {
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) {}
  def `test 3`(fixture: String) {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingJUnit3Suite extends JUnit3Suite with ClassTaggingFixtureServices  // JUnit3Suite does not support tag, it always return empty map in tags method.

@DoNotDiscover
@Ignore
class ExampleClassTaggingJUnitSuite extends JUnitSuite with ClassTaggingFixtureServices {
  @Test
  def testMethod1() {}
  @Test 
  def testMethod2() {}
  @Test 
  def testMethod3() {}
}

import org.testng.annotations.{Test => TestNG }

@DoNotDiscover
@Ignore
class ExampleClassTaggingTestNGSuite extends TestNGSuite with ClassTaggingFixtureServices {
  @TestNG
  def testMethod1() {}
  @TestNG
  def testMethod2() {}
  @TestNG
  def testMethod3() {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFunSuite extends FunSuite with ClassTaggingFixtureServices {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFunSuite extends fixture.FunSuite with ClassTaggingFixtureServices with StringFixture {
  test("Test 1") {s =>}
  test("Test 2") {s =>}
  test("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFunSpec extends FunSpec with ClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFunSpec extends fixture.FunSpec with ClassTaggingFixtureServices with StringFixture {
  describe("Scope 1") {
    it("Test 1") {s =>}
    it("Test 2") {s =>}
    it("Test 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFeatureSpec extends FeatureSpec with ClassTaggingFixtureServices {
  feature("Feature 1") {
    scenario("Scenario 1") {}
    scenario("Scenario 2") {}
    scenario("Scenario 3") {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFeatureSpec extends fixture.FeatureSpec with ClassTaggingFixtureServices with StringFixture {
  feature("Feature 1") {
    scenario("Scenario 1") {s =>}
    scenario("Scenario 2") {s =>}
    scenario("Scenario 3") {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFlatSpec extends FlatSpec with ClassTaggingFixtureServices {
  "Scope 1" should "do thing 1" in {}
  it should "do thing 2" in {}
  it should "do thing 3" in {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFlatSpec extends fixture.FlatSpec with ClassTaggingFixtureServices with StringFixture {
  "Scope 1" should "do thing 1" in {s =>}
  it should "do thing 2" in {s =>}
  it should "do thing 3" in {s =>}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFreeSpec extends FreeSpec with ClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureFreeSpec extends fixture.FreeSpec with ClassTaggingFixtureServices with StringFixture {
  "Scope 1" - {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingPropSpec extends PropSpec with ClassTaggingFixtureServices {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixturePropSpec extends fixture.PropSpec with ClassTaggingFixtureServices with StringFixture {
  property("Test 1") {s =>}
  property("Test 2") {s =>}
  property("Test 3") {s =>}
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingWordSpec extends WordSpec with ClassTaggingFixtureServices {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingFixtureWordSpec extends fixture.WordSpec with ClassTaggingFixtureServices with StringFixture {
  "Scope 1" should {
    "Test 1" in {s =>}
    "Test 2" in {s =>}
    "Test 3" in {s =>}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingPathFreeSpec extends path.FreeSpec with ClassTaggingFixtureServices {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
    "Test 3" in {}
  }
}

@DoNotDiscover
@Ignore
class ExampleClassTaggingPathFunSpec extends path.FunSpec with ClassTaggingFixtureServices {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
    it("Test 3") {}
  }
}