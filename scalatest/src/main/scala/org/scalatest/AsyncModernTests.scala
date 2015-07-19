/*
 * Copyright 2001-2014 Artima, Inc.
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

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait AsyncModernTests extends SuiteMixin with AsyncFixtures with TestRegistration { this: Suite =>

  type Registration = Future[Assertion]

  import scala.language.implicitConversions

  implicit def executionContext: ExecutionContext

  override private[scalatest] def transformToOutcome(testFun: => Future[Assertion]): () => AsyncOutcome =
    () => {
      val futureUnit = testFun
      FutureOutcome(
        futureUnit.map(u => Succeeded).recover {
          case ex: exceptions.TestCanceledException => Canceled(ex)
          case _: exceptions.TestPendingException => Pending
          case tfe: exceptions.TestFailedException => Failed(tfe)
          case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
        }
      )
    }

  implicit def convertToFuture(o: Assertion): Future[Assertion] = Future.successful(o)
}
