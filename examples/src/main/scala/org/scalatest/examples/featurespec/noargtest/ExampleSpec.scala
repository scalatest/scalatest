package org.scalatest.examples.featurespec.noargtest

import java.io.File
import org.scalatest._

class ExampleSpec extends FeatureSpec {

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

  scenario("This scenario should succeed") {
    assert(1 + 1 === 2)
  }

  scenario("This scenario should fail") {
    assert(1 + 1 === 3)
  }
}
