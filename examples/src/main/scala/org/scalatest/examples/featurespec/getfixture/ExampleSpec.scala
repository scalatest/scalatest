package org.scalatest.examples.featurespec.getfixture

import org.scalatest.FeatureSpec
import collection.mutable.ListBuffer

class ExampleSpec extends FeatureSpec {

  def fixture = 
    new {
      val builder = new StringBuilder("ScalaTest is designed to ")
      val buffer = new ListBuffer[String]
    }
  
  feature("Simplicity") {
    scenario("User needs to read test code written by others") {
      val f = fixture
      f.builder.append("encourage clear code!")
      assert(f.builder.toString === "ScalaTest is designed to encourage clear code!")
      assert(f.buffer.isEmpty)
      f.buffer += "sweet"
    }
  
    scenario("User needs to understand what the tests are doing") {
      val f = fixture
      f.builder.append("be easy to reason about!")
      assert(f.builder.toString === "ScalaTest is designed to be easy to reason about!")
      assert(f.buffer.isEmpty)
    }
  }
}