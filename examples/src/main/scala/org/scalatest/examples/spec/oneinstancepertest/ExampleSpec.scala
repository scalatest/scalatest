package org.scalatest.examples.spec.oneinstancepertest

import org.scalatest._
import collection.mutable.ListBuffer

class ExampleSpec extends Spec with OneInstancePerTest {

  val builder = new StringBuilder("ScalaTest is ")
  val buffer = new ListBuffer[String]

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
