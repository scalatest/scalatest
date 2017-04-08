package org.scalatest

class TestTestFlow extends TestFlow[Int] {

  val test0 = Test0("test 1") { 1 }
  val test1 = Test1("test 2") { i: Int =>
    cancel
    i + 1
  }
  val test2 = Test1("test 3") { i: Int =>
    assert(i == 2)
    i + 1
  }

  def flow = test0 andThen test1 andThen test2

}