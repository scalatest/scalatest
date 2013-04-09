package org.scalatest.examples.funsuite.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is")
  }

  // This test needs the StringBuilder fixture
  test("Testing should be productive") {
    new Builder {
      builder.append("productive!")
      assert(builder.toString === "ScalaTest is productive!")
    }
  }

  // This test needs the ListBuffer[String] fixture
  test("Test code should be readable") {
    new Buffer {
      buffer += ("readable!")
      assert(buffer === List("ScalaTest", "is", "readable!"))
    }
  }

  // This test needs both the StringBuilder and ListBuffer
  test("Test code should be clear and concise") {
    new Builder with Buffer {
      builder.append("clear!")
      buffer += ("concise!")
      assert(builder.toString === "ScalaTest is clear!")
      assert(buffer === List("ScalaTest", "is", "concise!"))
    }
  }
}