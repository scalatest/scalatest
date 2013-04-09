package org.scalatest.examples.freespec.beforeandafter

import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter
import collection.mutable.ListBuffer

class ExampleSpec extends FreeSpec with BeforeAndAfter {

  val builder = new StringBuilder
  val buffer = new ListBuffer[String]

  before {
    builder.append("ScalaTest is ")
  }

  after {
    builder.clear()
    buffer.clear()
  }

  "Testing" - {
    "should be easy" in {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    "should be fun" in {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
    }
  }
}