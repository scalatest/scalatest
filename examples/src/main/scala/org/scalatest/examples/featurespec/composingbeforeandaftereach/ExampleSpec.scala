package org.scalatest.examples.featurespec.composingbeforeandaftereach

import org.scalatest._
import collection.mutable.ListBuffer

trait Builder extends BeforeAndAfterEach { this: Suite =>

  val builder = new StringBuilder

  override def beforeEach() {
    builder.append("ScalaTest is designed to ")
    super.beforeEach() // To be stackable, must call super.beforeEach
  }

  override def afterEach() {
    try super.afterEach() // To be stackable, must call super.afterEach
    finally builder.clear()
  }
}

trait Buffer extends BeforeAndAfterEach { this: Suite =>

  val buffer = new ListBuffer[String]

  override def afterEach() {
    try super.afterEach() // To be stackable, must call super.afterEach
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
