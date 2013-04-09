package org.scalatest.examples.funspec.beforeandafter

import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import collection.mutable.ListBuffer

class ExampleSpec extends FunSpec with BeforeAndAfter {

  val builder = new StringBuilder
  val buffer = new ListBuffer[String]

  before {
    builder.append("ScalaTest is ")
  }

  after {
    builder.clear()
    buffer.clear()
  }

  describe("Testing") {
    it("should be easy") {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    it("should be fun") {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
    }
  }
}