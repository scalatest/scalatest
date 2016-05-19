/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest.examples.timelimitedtests

import org.scalatest.FunSpec
import org.scalatest.concurrent.{ThreadSignaler, TimeLimitedTests}
import org.scalatest.time.SpanSugar._

class ExampleSignalerSpec extends FunSpec with TimeLimitedTests {

  val timeLimit = 200 millis

  override val defaultTestSignaler = ThreadSignaler

  describe("A time-limited test") {
    it("should succeed if it completes within the time limit") {
      Thread.sleep(100)
    }
    it("should fail if it is taking too darn long") {
      Thread.sleep(300)
    }
  }
}