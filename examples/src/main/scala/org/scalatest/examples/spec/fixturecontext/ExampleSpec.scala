package org.scalatest.examples.spec.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.Spec

class ExampleSpec extends Spec {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is")
  }

  object `Testing ` {
    // This test needs the StringBuilder fixture
    def `should be productive` {
      new Builder {
        builder.append("productive!")
        assert(builder.toString === "ScalaTest is productive!")
      }
    }
  }

  object `Test code` {
    // This test needs the ListBuffer[String] fixture
    def `should be readable` {
      new Buffer {
        buffer += ("readable!")
        assert(buffer === List("ScalaTest", "is", "readable!"))
      }
    }

    // This test needs both the StringBuilder and ListBuffer
    def `should be clear and concise` {
      new Builder with Buffer {
        builder.append("clear!")
        buffer += ("concise!")
        assert(builder.toString === "ScalaTest is clear!")
        assert(buffer === List("ScalaTest", "is", "concise!"))
      }
    }
  }
}
