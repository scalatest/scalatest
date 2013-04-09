package org.scalatest.examples.funsuite.noargtest

import java.io.File
import org.scalatest._

class ExampleSuite extends FunSuite {

  override def withFixture(test: NoArgTest) = {

    try super.withFixture(test)
    catch {
      case e: Exception =>
        val currDir = new File(".")
        val fileNames = currDir.list()
        info("Dir snapshot: " + fileNames.mkString(", "))
        throw e
    }
  }

  test("This test should succeed") {
    assert(1 + 1 === 2)
  }

  test("This test should fail") {
    assert(1 + 1 === 3)
  }
}
