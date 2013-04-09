package org.scalatest.examples.featurespec.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSpec extends fixture.FeatureSpec {

  case class FixtureParam(file: File, writer: FileWriter)

  def withFixture(test: OneArgTest) = {

    // create the fixture
    val file = File.createTempFile("hello", "world")
    val writer = new FileWriter(file)
    val theFixture = FixtureParam(file, writer)

    try {
      writer.write("ScalaTest is designed to be ") // set up the fixture
      withFixture(test.toNoArgTest(theFixture)) // "loan" the fixture to the test
    }
    finally writer.close() // clean up the fixture
  }

  feature("Simplicity") {
    scenario("User needs to read test code written by others") { f =>
      f.writer.write("encourage clear code!")
      f.writer.flush()
      assert(f.file.length === 49)
    }

    scenario("User needs to understand what the tests are doing") { f =>
      f.writer.write("be easy to reason about!")
      f.writer.flush()
      assert(f.file.length === 52)
    }
  } 
}
