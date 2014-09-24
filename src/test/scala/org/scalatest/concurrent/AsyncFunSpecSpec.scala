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

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import Eventually._

class AsyncFunSpecSpec extends FunSpec {

  describe("AsyncFunSpec") {

    it("can be used for tests that return Future") {

      class ExampleSpec extends AsyncFunSpec {

        it("test 1") {
          Future {}
        }

        it("test 2") {
          Future {}
        }

        it("test 3") {
          Future {}
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(rep.testSucceededEventsReceived.length == 3)
    }

  }

}