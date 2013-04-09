package org.scalatest.tools.scalasbt

import org.scalatest._

class SuiteWithNestedSuites extends FunSuite {
  
  test("test 1") {}
  test("test 2") {}
  test("test 3") {}
  
  @DoNotDiscover
  class NestedDoNotDiscoverSuite extends FunSuite {
    override def suiteId = "nested 2"
          
    test("nested 2 test 1") {}
    test("nested 2 test 2") {}
    test("nested 2 test 3") {}
  }
  
  override def nestedSuites = 
    collection.immutable.IndexedSeq(
      new FunSuite() {
        override def suiteId = "nested 1"
          
        test("nested 1 test 1") {}
        test("nested 1 test 2") {}
        test("nested 1 test 3") {}
      }, 
      new NestedDoNotDiscoverSuite
    )
}