/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

@DoNotDiscover
protected[scalatest] class ExampleSuiteTimeoutSpec extends FunSpec with ParallelTestExecution {

  describe("Thing 1") {
    it ("do thing 1a") {}
    it ("do thing 1b") {}
    it ("do thing 1c") {}
  }
  
  describe("Thing 2") {
    it ("do thing 2a") {}
    it ("do thing 2b") { Thread.sleep(1300) }
  }

  //SCALATESTJS-ONLY override def newInstance = new ExampleSuiteTimeoutSpec
}

@DoNotDiscover
protected[scalatest] class ExampleSuiteTimeoutSpec2 extends FunSpec with ParallelTestExecution {
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

  //SCALATESTJS-ONLY override def newInstance = new ExampleSuiteTimeoutSpec2
}
