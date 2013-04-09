package org.scalatest.examples.funspec.noargtest

import java.io.File
import org.scalatest._

class ExampleSpec extends FunSpec {

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

  describe("This test") {
    it("should succeed") {
      assert(1 + 1 === 2)
    }

    it("should fail") {
      assert(1 + 1 === 3)
    }
  }
}
