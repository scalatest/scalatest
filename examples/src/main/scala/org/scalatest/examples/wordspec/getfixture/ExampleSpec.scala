package org.scalatest.examples.wordspec.getfixture

import org.scalatest.WordSpec
import collection.mutable.ListBuffer

class ExampleSpec extends WordSpec {

  def fixture = 
    new {
      val builder = new StringBuilder("ScalaTest is ")
      val buffer = new ListBuffer[String]
    }
  
  "Testing" should {
    "be easy" in {
      val f = fixture
      f.builder.append("easy!")
      assert(f.builder.toString === "ScalaTest is easy!")
      assert(f.buffer.isEmpty)
      f.buffer += "sweet"
    }
  
    "be fun" in {
      val f = fixture
      f.builder.append("fun!")
      assert(f.builder.toString === "ScalaTest is fun!")
      assert(f.buffer.isEmpty)
    }
  }
}