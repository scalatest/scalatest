package org.scalatest.examples.spec.composingwithfixture

import org.scalatest._
import collection.mutable.ListBuffer
 
trait Builder extends SuiteMixin { this: Suite =>
 
  val builder = new StringBuilder
 
  abstract override def withFixture(test: NoArgTest) = {
    builder.append("ScalaTest is ")
    try super.withFixture(test) // To be stackable, must call super.withFixture
    finally builder.clear()
  }
}
 
trait Buffer extends SuiteMixin { this: Suite =>
 
  val buffer = new ListBuffer[String]
 
  abstract override def withFixture(test: NoArgTest) = {
    try super.withFixture(test) // To be stackable, must call super.withFixture
    finally buffer.clear()
  }
}
 
class ExampleSpec extends Spec with Builder with Buffer {
 
  object `Testing ` {
    def `should be easy` {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }
 
    def `should be fun` {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
      buffer += "clear"
    }
  }
}
