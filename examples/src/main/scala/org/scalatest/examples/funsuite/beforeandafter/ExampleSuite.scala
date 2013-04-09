package org.scalatest.examples.funsuite.beforeandafter

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import collection.mutable.ListBuffer

class ExampleSuite extends FunSuite with BeforeAndAfter {

  val builder = new StringBuilder
  val buffer = new ListBuffer[String]

  before {
    builder.append("ScalaTest is ")
  }

  after {
    builder.clear()
    buffer.clear()
  }

  test("testing should be easy") {
    builder.append("easy!")
    assert(builder.toString === "ScalaTest is easy!")
    assert(buffer.isEmpty)
    buffer += "sweet"
  }

  test("testing should be fun") {
    builder.append("fun!")
    assert(builder.toString === "ScalaTest is fun!")
    assert(buffer.isEmpty)
  }
}