package test

import org.scalatest._
import Matchers._

class ExampleMatcherSpec extends FunSpec {

  describe("Equal matcher") {

    it("should do nothing when LHS is equal to RHS") {
      val a = 1
      val b = 1
      a should equal (b)
    }

  }

}