package org.scalatest.examples.spec.noargtest

import java.io.File
import org.scalatest._

class ExampleSpec extends Spec {

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

  object `This test` {
    def `should succeed` {
      assert(1 + 1 === 2)
    }

    def `should fail` {
      assert(1 + 1 === 3)
    }
  }
}
