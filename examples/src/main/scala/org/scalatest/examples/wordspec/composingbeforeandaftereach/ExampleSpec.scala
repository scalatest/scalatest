package org.scalatest.examples.wordspec.composingbeforeandaftereach

import org.scalatest._
import org.scalatest.BeforeAndAfterEach
import collection.mutable.ListBuffer

trait Builder extends BeforeAndAfterEach { this: Suite =>

  val builder = new StringBuilder

  override def beforeEach() {
    builder.append("ScalaTest is ")
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

class ExampleSpec extends WordSpec with Builder with Buffer {

  "Testing" should {
    "be easy" in {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    "be fun" in {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
      buffer += "clear"
    }
  }
}
