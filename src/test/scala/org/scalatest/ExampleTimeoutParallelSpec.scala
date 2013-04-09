package org.scalatest

import org.scalatest.time.Span
import org.scalatest.time.Second

@DoNotDiscover
class ExampleTimeoutParallelSpec extends FunSpec with ParallelTestExecution {

  describe("Thing 1") {
    it ("do thing 1a") {}
    it ("do thing 1b") { Thread.sleep(2000) }
    it ("do thing 1c") {}
  }
  
  describe("Thing 2") {
    it ("do thing 2a") {}
    it ("do thing 2b") {}
    it ("do thing 2c") {}
  }
  
  override protected def sortingTimeout: Span = Span(1, Second)
}