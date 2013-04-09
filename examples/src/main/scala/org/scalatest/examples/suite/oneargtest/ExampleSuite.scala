package org.scalatest.examples.suite.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSuite extends fixture.Suite {

  case class FixtureParam(file: File, writer: FileWriter)

  def withFixture(test: OneArgTest) = {

    // create the fixture
    val file = File.createTempFile("hello", "world")
    val writer = new FileWriter(file)
    val theFixture = FixtureParam(file, writer)

    try {
      writer.write("ScalaTest is ") // set up the fixture
      withFixture(test.toNoArgTest(theFixture)) // "loan" the fixture to the test
    }
    finally writer.close() // clean up the fixture
  }

  def `test: testing should be easy` (f: FixtureParam) {
    f.writer.write("easy!")
    f.writer.flush()
    assert(f.file.length === 18)
  }

  def `test: testing should be fun` (f: FixtureParam) {
    f.writer.write("fun!")
    f.writer.flush()
    assert(f.file.length === 17)
  }
}
