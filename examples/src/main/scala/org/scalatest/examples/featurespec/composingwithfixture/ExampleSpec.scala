package org.scalatest.examples.featurespec.composingwithfixture

import org.scalatest._
import collection.mutable.ListBuffer

trait Builder extends AbstractSuite { this: Suite =>

  val builder = new StringBuilder

  abstract override def withFixture(test: NoArgTest) = {
    builder.append("ScalaTest is designed to ")
    try super.withFixture(test) // To be stackable, must call super.withFixture
    finally builder.clear()
  }
}

trait Buffer extends AbstractSuite { this: Suite =>

  val buffer = new ListBuffer[String]

  abstract override def withFixture(test: NoArgTest) = {
    try super.withFixture(test) // To be stackable, must call super.withFixture
    finally buffer.clear()
  }
}

class ExampleSpec extends FeatureSpec with Builder with Buffer {

  feature("Simplicity") {
    scenario("User needs to read test code written by others") {
      builder.append("encourage clear code!")
      assert(builder.toString === "ScalaTest is designed to encourage clear code!")
      assert(buffer.isEmpty)
      buffer += "clear"
    }

    scenario("User needs to understand what the tests are doing") {
      builder.append("be easy to reason about!")
      assert(builder.toString === "ScalaTest is designed to be easy to reason about!")
      assert(buffer.isEmpty)
      buffer += "easy"
    }
  }
}
