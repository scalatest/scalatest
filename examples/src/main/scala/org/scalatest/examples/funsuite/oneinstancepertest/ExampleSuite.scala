package org.scalatest.examples.funsuite.oneinstancepertest

import org.scalatest._
import collection.mutable.ListBuffer

class ExampleSuite extends FunSuite with OneInstancePerTest {

  val builder = new StringBuilder("ScalaTest is ")
  val buffer = new ListBuffer[String]

  test("Testing should be easy") {
    builder.append("easy!")
    assert(builder.toString === "ScalaTest is easy!")
    assert(buffer.isEmpty)
    buffer += "sweet"
  }

  test("Testing should be fun") {
    builder.append("fun!")
    assert(builder.toString === "ScalaTest is fun!")
    assert(buffer.isEmpty)
  }
}