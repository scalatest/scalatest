package org.scalatest.examples.suite.oneinstancepertest

import org.scalatest._
import collection.mutable.ListBuffer

class ExampleSuite extends Suite with OneInstancePerTest {

  val builder = new StringBuilder("ScalaTest is ")
  val buffer = new ListBuffer[String]

  def `test: testing should be easy` {
    builder.append("easy!")
    assert(builder.toString === "ScalaTest is easy!")
    assert(buffer.isEmpty)
    buffer += "sweet"
  }

  def `test: testing should be fun` {
    builder.append("fun!")
    assert(builder.toString === "ScalaTest is fun!")
    assert(buffer.isEmpty)
  }
}
