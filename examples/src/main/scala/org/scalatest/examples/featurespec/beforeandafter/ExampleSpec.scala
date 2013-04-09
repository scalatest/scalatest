package org.scalatest.examples.featurespec.beforeandafter

import org.scalatest._
import collection.mutable.ListBuffer

class ExampleSpec extends FeatureSpec with BeforeAndAfter {

  val builder = new StringBuilder
  val buffer = new ListBuffer[String]

  before {
    builder.append("ScalaTest is designed to ")
  }

  after {
    builder.clear()
    buffer.clear()
  }

  feature("Simplicity") {
    scenario("User needs to read test code written by others") {
      builder.append("encourage clear code!")
      assert(builder.toString === "ScalaTest is designed to encourage clear code!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    scenario("User needs to understand what the tests are doing") {
      builder.append("be easy to reason about!")
      assert(builder.toString === "ScalaTest is designed to be easy to reason about!")
      assert(buffer.isEmpty)
    }
  }
}