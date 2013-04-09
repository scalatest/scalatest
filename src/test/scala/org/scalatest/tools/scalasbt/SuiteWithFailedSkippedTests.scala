package org.scalatest.tools.scalasbt

import org.scalatest._

class SuiteWithFailedSkippedTests extends FunSuite {
  test("success") {}
  ignore("ignored") {}
  test("pending") { pending }
  test("failed") { fail }
  test("canceled") { cancel }
  
  override def nestedSuites = 
    collection.immutable.IndexedSeq(
      new FunSuite() {
        override def suiteId = "nested 1"
          
        test("nested 1 success") {}
        ignore("nested 1 ignored") {}
        test("nested 1 pending") { pending }
        test("nested 1 failed") { fail }
        test("nested 1 canceled") { cancel }
      }, 
      new FunSuite() {
        override def suiteId = "nested 2"
          
        test("nested 2 success") {}
        ignore("nested 2 ignored") {}
        test("nested 2 pending") { pending }
        test("nested 2 failed") { fail }
        test("nested 2 canceled") { cancel }
      },
      new FunSuite() {
        override def suiteId = "nested 3"
        
        override def run(testName: Option[String], args: Args): Status = 
          throw new RuntimeException("Intended to abort suite")
      }
    )
}