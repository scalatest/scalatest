/*
 * Copyright 2001-2024 Artima, Inc.
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
import org.scalatest.time.Span
import org.scalatest.time.Second
import org.scalatest.funspec.AnyFunSpec

@DoNotDiscover
protected[scalatest] class ExampleTimeoutParallelSpec extends AnyFunSpec with ParallelTestExecution {

  describe("Thing 1") {
    it ("do thing 1a") {}
    it ("do thing 1b") { SleepHelper.sleep(2000) }
    it ("do thing 1c") {}
  }
  
  describe("Thing 2") {
    it ("do thing 2a") {}
    it ("do thing 2b") {}
    it ("do thing 2c") {}
  }
  
  override protected def sortingTimeout: Span = Span(1, Second)

  //SCALATESTJS,NATIVE-ONLY override def newInstance = new ExampleTimeoutParallelSpec
}
