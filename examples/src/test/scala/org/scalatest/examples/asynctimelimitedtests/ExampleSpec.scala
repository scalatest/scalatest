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
package org.scalatest.examples.asynctimelimitedtests

import org.scalatest.AsyncFunSpec
import org.scalatest.concurrent.AsyncTimeLimitedTests
import org.scalatest.time.SpanSugar._

import scala.concurrent.Future

class ExampleSpec extends AsyncFunSpec with AsyncTimeLimitedTests {

  // Note: You may need to either write 200.millis or (200 millis), or
  // place a semicolon or blank line after plain old 200 millis, to
  // avoid the semicolon inference problems of postfix operator notation.
  val timeLimit = 200 millis

  describe("An asynchronous time-limited test") {
    it("should succeed if it completes within the time limit") {
      Future {
        // Your code should run here, the following is just an example
        // code of filling a buffer and assert its content that should
        // not take more than 200 millis
        val buffer = new StringBuilder
        buffer.append("test")
        assert(buffer.toString == "test")
      }
    }
    it("should fail if it is taking too darn long") {
      Future {
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
}