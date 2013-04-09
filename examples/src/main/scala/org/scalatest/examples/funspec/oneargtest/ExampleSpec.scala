package org.scalatest.examples.funspec.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSpec extends fixture.FunSpec {

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

  describe("Testing") {
    it("should be easy") { f =>
      f.writer.write("easy!")
      f.writer.flush()
      assert(f.file.length === 18)
    }

    it("should be fun") { f =>
      f.writer.write("fun!")
      f.writer.flush()
      assert(f.file.length === 17)
    }
  } 
}
