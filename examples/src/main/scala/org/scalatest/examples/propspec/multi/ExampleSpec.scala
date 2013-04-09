package org.scalatest.examples.fixture.propspec.multi

import org.scalatest._
import prop.PropertyChecks
import scala.collection.mutable.ListBuffer

class ExampleSpec extends fixture.PropSpec with PropertyChecks with ShouldMatchers {

  case class FixtureParam(builder: StringBuilder, buffer: ListBuffer[String])

  def withFixture(test: OneArgTest) = {

    // Create needed mutable objects
    val stringBuilder = new StringBuilder("ScalaTest is ")
    val listBuffer = new ListBuffer[String]
    val theFixture = FixtureParam(stringBuilder, listBuffer)

    // Invoke the test function, passing in the mutable objects
    withFixture(test.toNoArgTest(theFixture))
  }

  property("testing should be easy") { f =>
    f.builder.append("easy!")
    assert(f.builder.toString === "ScalaTest is easy!")
    assert(f.buffer.isEmpty)
    val firstChar = f.builder(0)
    forAll { (c: Char) =>
      whenever (c != 'S') {
        c should not equal firstChar
      }
    }
    f.buffer += "sweet"
  }

  property("testing should be fun") { f =>
    f.builder.append("fun!")
    assert(f.builder.toString === "ScalaTest is fun!")
    assert(f.buffer.isEmpty)
    val firstChar = f.builder(0)
    forAll { (c: Char) =>
      whenever (c != 'S') {
        c should not equal firstChar
      }
    }
  }
}
