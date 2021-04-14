package org.scalactic

import org.scalatest.funsuite.AnyFunSuite

class TestSpec extends AnyFunSuite {

  test("this is a test") {
    val a = 1
    val b = 2
    assert(a != b)  
  }

}
