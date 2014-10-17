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
package org.scalatest.fixture

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import scala.concurrent.Future

class AsyncFixturesSpec extends org.scalatest.FunSpec {

  describe("AsyncFixtures") {

    it("should fail tests with NotAllowedException when mixed in classic style traits") {
      val spec = new FunSpec with AsyncFixtures {

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        it("a test") { fixture => }
      }
      val rep = new EventRecordingReporter
      spec.run(None, Args(reporter = rep))
      assert(rep.testFailedEventsReceived.size == 1)
      val tfe = rep.testFailedEventsReceived(0)
      assert(tfe.throwable.isDefined)
      assert(tfe.throwable.get.isInstanceOf[exceptions.NotAllowedException])
    }

  }

}