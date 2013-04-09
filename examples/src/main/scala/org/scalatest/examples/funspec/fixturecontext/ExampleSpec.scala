package org.scalatest.examples.funspec.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.FunSpec

class ExampleSpec extends FunSpec {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is")
  }

  describe("Testing") {
    // This test needs the StringBuilder fixture
    it("should be productive") {
      new Builder {
        builder.append("productive!")
        assert(builder.toString === "ScalaTest is productive!")
      }
    }
  }

  describe("Test code") {
    // This test needs the ListBuffer[String] fixture
    it("should be readable") {
      new Buffer {
        buffer += ("readable!")
        assert(buffer === List("ScalaTest", "is", "readable!"))
      }
    }

    // This test needs both the StringBuilder and ListBuffer
    it("should be clear and concise") {
      new Builder with Buffer {
        builder.append("clear!")
        buffer += ("concise!")
        assert(builder.toString === "ScalaTest is clear!")
        assert(buffer === List("ScalaTest", "is", "concise!"))
      }
    }
  }
}