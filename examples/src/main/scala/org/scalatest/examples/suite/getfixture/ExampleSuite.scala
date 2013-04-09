package org.scalatest.examples.suite.getfixture

import org.scalatest.Suite
import collection.mutable.ListBuffer

class ExampleSuite extends Suite {

  def fixture =
    new {
      val builder = new StringBuilder("ScalaTest is ")
      val buffer = new ListBuffer[String]
    }

  def `test: testing should be easy` {
    val f = fixture
    f.builder.append("easy!")
    assert(f.builder.toString === "ScalaTest is easy!")
    assert(f.buffer.isEmpty)
    f.buffer += "sweet"
  }

  def `test: testing should be fun` {
    val f = fixture
    f.builder.append("fun!")
    assert(f.builder.toString === "ScalaTest is fun!")
    assert(f.buffer.isEmpty)
  }
}
