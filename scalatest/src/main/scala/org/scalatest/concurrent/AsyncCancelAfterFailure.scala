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
package org.scalatest.concurrent

import org.scalatest.time.Span
import org.scalatest._

import scala.concurrent.Future

trait AsyncCancelAfterFailure extends AsyncSuiteMixin { this: AsyncSuite =>

  @volatile private var cancelRemaining = false

  abstract override def withFixture(test: NoArgAsyncTest): Future[Outcome] = {

    if (cancelRemaining)
      Future.successful(Canceled("Canceled by CancelOnFailure because a test failed previously"))
    else
      super.withFixture(test).map { o =>
        o match {
          case failed: Failed =>
            cancelRemaining = true
            failed
          case outcome => outcome
        }
      }
  }

}
