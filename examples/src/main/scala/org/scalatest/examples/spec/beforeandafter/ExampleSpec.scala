package org.scalatest.examples.spec.beforeandafter

import org.scalatest.Spec
import org.scalatest.BeforeAndAfter
import collection.mutable.ListBuffer

class ExampleSpec extends Spec with BeforeAndAfter {
 
  val builder = new StringBuilder
  val buffer = new ListBuffer[String]
 
  before {
    builder.append("ScalaTest is ")
  }
 
  after {
    builder.clear()
    buffer.clear()
  }
 
  object `Testing ` {
    def `should be easy` {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }
 
    def `should be fun` {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
    }
  }
}
