package org.scalatest.examples.funsuite.getfixture

import org.scalatest.FunSuite
import collection.mutable.ListBuffer

class ExampleSuite extends FunSuite {

  def fixture = 
    new {
      val builder = new StringBuilder("ScalaTest is ")
      val buffer = new ListBuffer[String]
    }
  
  test("Testing should be easy") {
    val f = fixture
    f.builder.append("easy!")
    assert(f.builder.toString === "ScalaTest is easy!")
    assert(f.buffer.isEmpty)
    f.buffer += "sweet"
  }
  
  test("Testing should be fun") {
    val f = fixture
    f.builder.append("fun!")
    assert(f.builder.toString === "ScalaTest is fun!")
    assert(f.buffer.isEmpty)
  }
}