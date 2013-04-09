package org.scalatest.examples.freespec.oneinstancepertest

import org.scalatest._
import collection.mutable.ListBuffer

class ExampleSuite extends FreeSpec with OneInstancePerTest {

  val builder = new StringBuilder("ScalaTest is ")
  val buffer = new ListBuffer[String]

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