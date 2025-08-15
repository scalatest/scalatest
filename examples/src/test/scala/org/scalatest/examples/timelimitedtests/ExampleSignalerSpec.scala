/*
 * Copyright 2001-2025 Artima, Inc.
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
      // Your code should run here, the following is just an example
      // code of filling a buffer and assert its content that should
      // not take more than 200 millis
      val buffer = new StringBuilder
      buffer.append("test")
      assert(buffer.toString == "test")
    }
    it("should fail if it is taking too darn long") {
      // Your code should run here, the following is just an example
      // code of filling buffer in loop that will take more than 200 millis
      val startTime = scala.compat.Platform.currentTime
      var stop = false
      val buffer = new StringBuilder
      while (!stop) {
        for (i <- 1 to 100)
          buffer.append(i.toString)
        if ((scala.compat.Platform.currentTime - startTime) > 200)
          stop = true
        else
          buffer.clear()
      }
      assert(buffer.length == 5050)
    }
  }
}