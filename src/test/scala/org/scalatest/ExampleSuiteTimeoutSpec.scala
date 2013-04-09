package org.scalatest

@DoNotDiscover
class ExampleSuiteTimeoutSpec extends FunSpec with ParallelTestExecution {

  describe("Thing 1") {
    it ("do thing 1a") {}
    it ("do thing 1b") {}
    it ("do thing 1c") {}
  }
  
  describe("Thing 2") {
    it ("do thing 2a") {}
    it ("do thing 2b") { Thread.sleep(1300) }
  }
  
}

@DoNotDiscover
class ExampleSuiteTimeoutSpec2 extends FunSpec with ParallelTestExecution {
  describe("Subject 1") {
    it ("content 1a") {}
    it ("content 1b") {}
    it ("content 1c") {}
  }
  
  describe("Subject 2") {
    it ("content 2a") {}
    it ("content 2b") {}
    it ("content 2c") {}
  }
}