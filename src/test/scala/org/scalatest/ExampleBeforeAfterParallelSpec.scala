package org.scalatest

@DoNotDiscover
class ExampleBeforeAfterParallelSpec extends FunSpec with BeforeAndAfter with ParallelTestExecution {

  before {
   info("In Before")
  }
  
  describe("Thing 1") {
    it ("do thing 1a") {}
    it ("do thing 1b") {}
    it ("do thing 1c") {}
  }
  
  describe("Thing 2") {
    it ("do thing 2a") {}
    it ("do thing 2b") {}
    it ("do thing 2c") {}
  }
  
  after {
    info("In After")
  }
}