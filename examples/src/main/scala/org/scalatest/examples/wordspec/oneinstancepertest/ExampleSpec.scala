package org.scalatest.examples.wordspec.oneinstancepertest

import org.scalatest._
import collection.mutable.ListBuffer

class ExampleSuite extends WordSpec with OneInstancePerTest {

  val builder = new StringBuilder("ScalaTest is ")
  val buffer = new ListBuffer[String]

  "Testing" should {
    "be easy" in {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    "be fun" in {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
    } 
  }
}