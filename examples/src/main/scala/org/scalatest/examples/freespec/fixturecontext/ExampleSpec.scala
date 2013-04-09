package org.scalatest.examples.freespec.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.FreeSpec

class ExampleSpec extends FreeSpec {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is")
  }

  "Testing" - {
    // This test needs the StringBuilder fixture
    "should be productive" in new Builder {
      builder.append("productive!")
      assert(builder.toString === "ScalaTest is productive!")
    }
  }

  "Test code" - {
    // This test needs the ListBuffer[String] fixture
    "should be readable" in new Buffer {
      buffer += ("readable!")
      assert(buffer === List("ScalaTest", "is", "readable!"))
    }

    // This test needs both the StringBuilder and ListBuffer
    "should be clear and concise" in new Builder with Buffer {
      builder.append("clear!")
      buffer += ("concise!")
      assert(builder.toString === "ScalaTest is clear!")
      assert(buffer === List("ScalaTest", "is", "concise!"))
    }
  }
}