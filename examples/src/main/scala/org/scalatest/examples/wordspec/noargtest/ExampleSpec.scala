package org.scalatest.examples.wordspec.noargtest

import java.io.File
import org.scalatest._

class ExampleSpec extends WordSpec {

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

  "This test" should {
    "succeed" in {
      assert(1 + 1 === 2)
    }

    "fail" in {
      assert(1 + 1 === 3)
    }
  }
}
