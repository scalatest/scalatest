package org.scalatest

@DoNotDiscover
class RunInSpurtsSpec2 extends FunSpec with BeforeAndAfter {

  before {
    info("In Before")
  }

  describe("Thing 1") {
    it("do thing 1a") {}
    it("do thing 1b") {}
    it("do thing 1c") {}
  }

  describe("Thing 2") {
    it("do thing 2a") {}
    it("do thing 2b") {}
    it("do thing 2c") {}
  }

  after {
    info("In After")
  }

  var batch = 0

  override def testNames = {
    if (batch == 0) {
      super.testNames.take(3)
    }
    else {
      super.testNames.drop(3)
    }
  }
}