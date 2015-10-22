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

import org.scalatest.concurrent.SleepHelper

@DoNotDiscover
protected[scalatest] class ExampleSuiteTimeoutSpec extends FunSpec with ParallelTestExecution {

  describe("Thing 1") {
    it("do thing 1a") { succeed }
    it("do thing 1b") { succeed }
    it("do thing 1c") { succeed }
  }
  
  describe("Thing 2") {
    it("do thing 2a") { succeed }
    it("do thing 2b") { SleepHelper.sleep(1300); succeed }
  }

  //SCALATESTJS-ONLY override def newInstance = new ExampleSuiteTimeoutSpec
}

@DoNotDiscover
protected[scalatest] class ExampleSuiteTimeoutSpec2 extends FunSpec with ParallelTestExecution {
  describe("Subject 1") {
    it("content 1a") { succeed }
    it("content 1b") { succeed }
    it("content 1c") { succeed }
  }
  
  describe("Subject 2") {
    it("content 2a") { succeed }
    it("content 2b") { succeed }
    it("content 2c") { succeed }
  }

  //SCALATESTJS-ONLY override def newInstance = new ExampleSuiteTimeoutSpec2
}
