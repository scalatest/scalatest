package org.scalatest.examples.featurespec.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.FeatureSpec

class ExampleSpec extends FeatureSpec {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is designed to ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is", "designed", "to")
  }

  feature("Simplicity") {
    // This test needs the StringBuilder fixture
    scenario("User needs to read test code written by others") {
      new Builder {
        builder.append("encourage clear code!")
        assert(builder.toString === "ScalaTest is designed to encourage clear code!")
      }
    }
    
    // This test needs the ListBuffer[String] fixture
    scenario("User needs to understand what the tests are doing") {
      new Buffer {
        buffer += ("be", "easy", "to", "reason", "about!")
        assert(buffer === List("ScalaTest", "is", "designed", "to", "be", "easy", "to", "reason", "about!"))
      }
    }

    // This test needs both the StringBuilder and ListBuffer
    scenario("User needs to write tests") {
      new Builder with Buffer {
        builder.append("be easy to learn!")
        buffer += ("be", "easy", "to", "remember", "how", "to", "write!")
        assert(builder.toString === "ScalaTest is designed to be easy to learn!")
        assert(buffer === List("ScalaTest", "is", "designed", "to", "be", "easy",
          "to", "remember", "how", "to", "write!"))
      }
    }
  }
}