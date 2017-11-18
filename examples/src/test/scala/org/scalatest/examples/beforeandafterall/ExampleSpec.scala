package org.scalatest.examples.beforeandafterall

import org.scalatest._
import java.io._

trait TempFileExistsSpec extends fixture.FlatSpecLike {

  protected val tempFileName = "tmp.txt"

  type FixtureParam = File
  override def withFixture(test: OneArgTest) = {
    val file = new File(tempFileName)
    withFixture(test.toNoArgTest(file)) // loan the fixture to the test
  }

  "The temp file" should ("exist in " + suiteName) in { file =>
    assert(file.exists)
  }
}

class OneSpec extends TempFileExistsSpec
class TwoSpec extends TempFileExistsSpec
class RedSpec extends TempFileExistsSpec
class BlueSpec extends TempFileExistsSpec

class ExampleSpec extends Suites(
  new OneSpec,
  new TwoSpec,
  new RedSpec,
  new BlueSpec
) with TempFileExistsSpec with BeforeAndAfterAll {

  // Set up the temp file needed by the test, taking
  // a file name from the config map
  override def beforeAll() {
    val writer = new FileWriter(tempFileName)
    try writer.write("Hello, suite of tests!")
    finally writer.close()
  }

  // Delete the temp file
  override def afterAll() {
    val file = new File(tempFileName)
    file.delete()
  }
}
