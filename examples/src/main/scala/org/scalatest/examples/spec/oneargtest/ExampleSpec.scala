package org.scalatest.examples.spec.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSpec extends fixture.Spec {

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

  object `Testing ` {
    def `should be easy` { f: FixtureParam =>
      f.writer.write("easy!")
      f.writer.flush()
      assert(f.file.length === 18)
    }
 
    def `should be fun` { f: FixtureParam =>
      f.writer.write("fun!")
      f.writer.flush()
      assert(f.file.length === 17)
    }
  } 
}
