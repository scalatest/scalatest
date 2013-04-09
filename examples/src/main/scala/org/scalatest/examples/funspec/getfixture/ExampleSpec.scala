package org.scalatest.examples.funspec.getfixture

import org.scalatest.FunSpec
import collection.mutable.ListBuffer

class ExampleSpec extends FunSpec {

  def fixture = 
    new {
      val builder = new StringBuilder("ScalaTest is ")
      val buffer = new ListBuffer[String]
    }
  
  describe("Testing") {
    it("should be easy") {
      val f = fixture
      f.builder.append("easy!")
      assert(f.builder.toString === "ScalaTest is easy!")
      assert(f.buffer.isEmpty)
      f.buffer += "sweet"
    }
  
    it("should be fun") {
      val f = fixture
      f.builder.append("fun!")
      assert(f.builder.toString === "ScalaTest is fun!")
      assert(f.buffer.isEmpty)
    }
  }
}