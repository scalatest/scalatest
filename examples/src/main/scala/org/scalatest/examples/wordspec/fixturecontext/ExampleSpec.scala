package org.scalatest.examples.wordspec.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.WordSpec

class ExampleSpec extends WordSpec {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is")
  }

  "Testing" should {
    // This test needs the StringBuilder fixture
    "be productive" in new Builder {
      builder.append("productive!")
      assert(builder.toString === "ScalaTest is productive!")
    }
  }

  "Test code" should {
    // This test needs the ListBuffer[String] fixture
    "be readable" in new Buffer {
      buffer += ("readable!")
      assert(buffer === List("ScalaTest", "is", "readable!"))
    }

    // This test needs both the StringBuilder and ListBuffer
    "be clear and concise" in new Builder with Buffer {
      builder.append("clear!")
      buffer += ("concise!")
      assert(builder.toString === "ScalaTest is clear!")
      assert(buffer === List("ScalaTest", "is", "concise!"))
    }
  }
}