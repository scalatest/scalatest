package org.scalatest.examples.freespec.getfixture

import org.scalatest.FreeSpec
import collection.mutable.ListBuffer

class ExampleSpec extends FreeSpec {

  def fixture = 
    new {
      val builder = new StringBuilder("ScalaTest is ")
      val buffer = new ListBuffer[String]
    }
  
  "Testing" - {
    "should be easy" in {
      val f = fixture
      f.builder.append("easy!")
      assert(f.builder.toString === "ScalaTest is easy!")
      assert(f.buffer.isEmpty)
      f.buffer += "sweet"
    }
  
    "should be fun" in {
      val f = fixture
      f.builder.append("fun!")
      assert(f.builder.toString === "ScalaTest is fun!")
      assert(f.buffer.isEmpty)
    }
  }
}