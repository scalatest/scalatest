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
package org.scalatest.concurrent

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.time.{ Span, Millis }
 
class WaiterSpec extends WordSpec with ShouldMatchers with AsyncAssertions {
  "A Waiter instance" should {
    "not break when beaten on" in {
      val n = 100000
      val w = new Waiter
      new Thread(
        new Runnable {
          override def run(): Unit = {
            var i = 0
            while (i < n) { w.dismiss(); i += 1 }
              println(s"i=$i")
            }
          }
        ).start
      w.await(timeout(Span(2000, Millis)), dismissals(n))
    }
  }
}
