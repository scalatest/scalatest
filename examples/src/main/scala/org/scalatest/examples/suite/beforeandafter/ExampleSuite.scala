package org.scalatest.examples.suite.beforeandafter

import org.scalatest.Suite
import org.scalatest.BeforeAndAfter
import collection.mutable.ListBuffer

class ExampleSuite extends Suite with BeforeAndAfter {

  val builder = new StringBuilder
  val buffer = new ListBuffer[String]

  before {
    builder.append("ScalaTest is ")
  }

  after {
    builder.clear()
    buffer.clear()
  }

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
