package org.scalatest.examples.wordspec.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSpec extends fixture.WordSpec {

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

  "Testing" should {
    "be easy" in { f =>
      f.writer.write("easy!")
      f.writer.flush()
      assert(f.file.length === 18)
    }

    "be fun" in { f =>
      f.writer.write("fun!")
      f.writer.flush()
      assert(f.file.length === 17)
    }
  } 
}
